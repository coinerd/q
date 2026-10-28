#lang racket/base

;; tools/builtins/find.rkt — File/directory search tool
;;
;; v0.33.2 W0: Converted to define-tool macro.
;; v0.84.4: BUG-1 early termination, BUG-2 root guard, BUG-3 scan budget.
;;
;; R97 (BUG-0097) — absolute, openable find -> read roundtrip:
;; - Every content result string is an ABSOLUTE path in the executing
;;   filesystem namespace, directly passable as read's path argument there.
;;   Output compatibility change: results were root-relative display
;;   strings before R97 (documented in the schema description).
;; - Results are built from traversal paths (build-path descent from the
;;   search root), never by concatenating user-supplied strings.
;; - details carry 'path-base 'absolute; 'search-root stays absolute.
;; - Symlinks: directory symlinks are NEVER recursed into (cycle/escape
;;   defense). Every symlink entry's resolved target is checked before
;;   emission: dangling targets and targets escaping the search root are
;;   denied and recorded as structured records in details 'denied. When
;;   safe mode is active, the existing safety policy (allowed-path? via
;;   require-safe-path!) is additionally applied to each emitted path and
;;   its resolved target BEFORE emission; policy-denied entries are omitted
;;   and recorded.
;; - max-results / max-depth / scan budget / early termination / protected
;;   root guards are unchanged.
;; - Absolute paths are NOT permission: confinement to the search root and
;;   the safe-mode policy still apply to every emitted absolute path.

(require "../../util/error/error-helpers.rkt"
         racket/file
         racket/string
         racket/path
         (only-in "../tool.rkt" make-success-result make-error-result)
         (only-in "../define-tool.rkt" define-tool)
         (only-in "builtin-helpers.rkt" require-safe-path! expand-tool-path)
         (only-in "../../util/path/glob.rkt" glob->regexp)
         (only-in "../../util/path/path-filters.rkt"
                  hidden-name?
                  vcs-dir?
                  skip-dirs
                  path-component-hidden?)
         (only-in "../../util/path/path-helpers.rkt" expand-home-path))

;; --------------------------------------------------
;; BUG-2: Protected system paths
;; --------------------------------------------------

(define SYSTEM-ROOTS '("/proc" "/sys" "/dev" "/run" "/snap"))

(define (filesystem-root-path? abs-path)
  ;; Returns #t if abs-path is the filesystem root or a protected system directory.
  (define s (path->string abs-path))
  (or (string=? s "/")
      (ormap (lambda (root) (or (string=? s root) (string-prefix? s (string-append root "/"))))
             SYSTEM-ROOTS)))

;; --------------------------------------------------
;; BUG-3: Scan budget hard cap
;; --------------------------------------------------

(define MAX-SCAN-BUDGET 50000)

;; --------------------------------------------------
;; R97: path helpers
;; --------------------------------------------------

;; #t when P is BASE or one of its descendants (lexically, no '..' escape).
(define (path-within? base p)
  (not (ormap (lambda (e) (eq? e 'up)) (explode-path (find-relative-path base p)))))

;; Fully-resolved absolute path for P, or #f when the path cannot be
;; resolved (e.g. missing parent). Note: resolve-path resolves the final
;; symlink component WITHOUT checking target existence, so dangling links
;; are detected separately via file-exists?/directory-exists? in walk-dir.
(define (resolved-absolute-path p)
  (with-handlers ([exn:fail:filesystem? (lambda (_) #f)])
    (simple-form-path (resolve-path p))))

;; --------------------------------------------------
;; Core recursive walk
;; --------------------------------------------------

(define (walk-dir root-path
                  resolved-root
                  name-re
                  type-filter
                  max-depth
                  max-results
                  root-is-hidden
                  #:max-scanned [max-scanned MAX-SCAN-BUDGET])
  ;; Returns (values results total-scanned entries-scanned denied-records)
  ;; BUG-1: Stops recursing into subdirectories once max-results are found.
  ;; BUG-3: Hard cap on entries scanned via max-scanned budget.
  ;; R97: results are ABSOLUTE traversal paths; symlink/policy denials are
  ;; recorded as structured records (path + reason) in denied-records.
  (define results (box '()))
  (define total (box 0))
  (define scanned (box 0))
  (define denied (box '()))

  (define (add-result! abs-str)
    (set-box! total (add1 (unbox total)))
    (when (< (length (unbox results)) max-results)
      (set-box! results (append (unbox results) (list abs-str)))))

  (define (deny! full-path reason)
    (set-box! denied
              (append (unbox denied) (list (hasheq 'path (path->string full-path) 'reason reason)))))

  (define (results-full?)
    (>= (length (unbox results)) max-results))

  (define (budget-remaining?)
    (< (unbox scanned) max-scanned))

  (define (matches-name? filename)
    (or (not name-re) (regexp-match? name-re filename)))

  (define (matches-type? full-path)
    (case type-filter
      [("file") (file-exists? full-path)]
      [("dir") (directory-exists? full-path)]
      [else #t]))

  (define (should-skip-entry? name)
    (cond
      [(vcs-dir? name) #t]
      [(and (hidden-name? name) (not root-is-hidden)) #t]
      [else #f]))

  ;; Existing safety policy applied BEFORE emission (no-op unless safe mode
  ;; is active): the emitted traversal path AND the resolved symlink target
  ;; must both be allowed.
  (define (emission-allowed? full-path resolved-target)
    (and (not (require-safe-path! (path->string full-path) "find"))
         (or (not resolved-target) (not (require-safe-path! (path->string resolved-target) "find")))))

  ;; depth-first walk
  (define (walk current-dir depth)
    (when (budget-remaining?)
      (define entries (with-safe-fallback '() (directory-list current-dir #:build? #f)))
      (for ([entry (in-list entries)]
            #:break (not (budget-remaining?)))
        (set-box! scanned (add1 (unbox scanned)))
        (define entry-str (path->string entry))
        (define full-path (build-path current-dir entry))

        ;; Skip VCS dirs and hidden entries (unless root is hidden)
        (unless (should-skip-entry? entry-str)
          (define entry-is-link? (link-exists? full-path))
          ;; Dangling detection: file-exists?/directory-exists? follow the link;
          ;; #f for a broken link. (resolve-path alone does NOT raise for a
          ;; dangling final component, so it cannot detect dangling here.)
          (define resolved-target
            (and entry-is-link?
                 (or (file-exists? full-path) (directory-exists? full-path))
                 (resolved-absolute-path full-path)))
          (define candidate? (and (matches-type? full-path) (matches-name? entry-str)))
          (cond
            ;; dangling symlink: never emitted, structured denial record
            [(and entry-is-link? (not resolved-target))
             (when candidate?
               (deny! full-path "dangling-symlink"))]
            ;; symlink whose resolved target escapes the search root:
            ;; denied (confinement applies to resolved targets, not just
            ;; path strings)
            [(and entry-is-link? (not (path-within? resolved-root resolved-target)))
             (when candidate?
               (deny! full-path "symlink-target-outside-root"))]
            ;; existing safety policy denied this entry
            [(not (emission-allowed? full-path resolved-target))
             (when candidate?
               (deny! full-path "safe-mode-path-denied"))]
            [else
             ;; Check type + name match — emit the ABSOLUTE traversal path
             (when candidate?
               (add-result! (path->string full-path)))
             ;; BUG-1: Recurse into subdirectories only if results NOT full,
             ;; depth allows, and budget remains. R97: NEVER recurse into
             ;; directory symlinks (cycle/escape defense).
             (when (and (not entry-is-link?)
                        (directory-exists? full-path)
                        (< depth max-depth)
                        (not (results-full?))
                        (budget-remaining?))
               (walk full-path (add1 depth)))])))))

  (walk root-path 0)
  (values (unbox results) (unbox total) (unbox scanned) (unbox denied)))

;; --------------------------------------------------
;; Handler function
;; --------------------------------------------------

(define (find-handler args [exec-ctx #f])
  ;; (safe-mode path check is done by scheduler, not here)
  (cond
    [(not (hash-has-key? args 'path)) (make-error-result "Missing required argument: path")]
    [else
     (define raw-path (hash-ref args 'path))
     (define path-str (expand-home-path raw-path))
     (cond
       [(not (string? path-str)) (make-error-result "Argument 'path' must be a string")]

       ;; 2. Path must exist
       [(not (or (directory-exists? path-str) (file-exists? path-str)))
        (make-error-result (format "Path not found: ~a" path-str))]

       ;; 3. Path must be a directory
       [(not (directory-exists? path-str))
        (make-error-result (format "Path is not a directory: ~a" path-str))]

       [else
        ;; Resolve to an absolute path for checks and traversal (W2: shared
        ;; canonical resolution helper; results stay absolute traversal
        ;; paths, never user-string concatenation).
        (define root-path (expand-tool-path path-str))

        ;; BUG-2: Reject filesystem root and system directories
        (cond
          [(filesystem-root-path? root-path)
           (make-error-result
            (format
             "Scanning from filesystem root or protected system directory (~a) is not allowed. Use a project-relative path instead."
             root-path))]
          [else
           ;; 4. Parse optional arguments
           (define name-pattern (hash-ref args 'name #f))
           (define type-filter (hash-ref args 'type "any"))
           (define max-depth (max 0 (hash-ref args 'max-depth 10)))
           (define max-results (max 1 (hash-ref args 'max-results 100)))

           ;; Compile glob pattern if provided (empty string → match all)
           (define name-re
             (if (and name-pattern (positive? (string-length name-pattern)))
                 (glob->regexp name-pattern #:allow-slash? #t)
                 #f))

           ;; Check if root path itself is under a hidden directory
           (define root-is-hidden (path-component-hidden? root-path))

           ;; R97: resolved root — used to confine symlink targets when the
           ;; search root itself is reached through a symlink
           (define resolved-root (or (resolved-absolute-path root-path) root-path))

           ;; 5. Walk the tree (BUG-3: with scan budget)
           (define-values (results total-found entries-scanned denied-records)
             (walk-dir root-path
                       resolved-root
                       name-re
                       type-filter
                       max-depth
                       max-results
                       root-is-hidden))

           (define budget-exceeded? (>= entries-scanned MAX-SCAN-BUDGET))
           (define truncated? (or (> total-found max-results) budget-exceeded?))

           ;; R97: results are absolute filesystem paths, openable by read
           ;; in the executing filesystem namespace regardless of CWD.
           (make-success-result results
                                (hasheq 'total-found
                                        total-found
                                        'truncated?
                                        truncated?
                                        'search-root
                                        (path->string root-path)
                                        'path-base
                                        'absolute
                                        'scanned
                                        entries-scanned
                                        'denied
                                        denied-records))])])]))

;; --------------------------------------------------
;; Tool definition via define-tool macro
;; --------------------------------------------------

(define-tool
 find
 #:description
 "List matching files/directories. Results are ABSOLUTE paths passable as read.path; details carry path-base 'absolute and a structured 'denied list."
 #:required ("path")
 #:properties
 [(path
   "string"
   "Root directory to search (relative paths resolve against the executing process CWD; results are absolute)")
  (name "string" "Name pattern (glob, optional)")
  (type "string" "Type filter: 'file', 'dir', or 'any' (default)")
  (max-depth "integer" "Maximum directory depth to search (default 10)")
  (max-results "integer" "Maximum results to return (default 100)")]
 find-handler)

(provide find)
