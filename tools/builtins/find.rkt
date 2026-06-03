#lang racket/base

;; tools/builtins/find.rkt — File/directory search tool
;;
;; v0.33.2 W0: Converted to define-tool macro.
;; v0.84.4: BUG-1 early termination, BUG-2 root guard, BUG-3 scan budget.

(require "../../util/error/error-helpers.rkt"
         racket/file
         racket/string
         racket/path
         (only-in "../tool.rkt" make-success-result make-error-result)
         (only-in "../define-tool.rkt" define-tool)
         (only-in "builtin-helpers.rkt" require-safe-path!)
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
;; Core recursive walk
;; --------------------------------------------------

(define (walk-dir root-path
                  name-re
                  type-filter
                  max-depth
                  max-results
                  root-is-hidden
                  #:max-scanned [max-scanned MAX-SCAN-BUDGET])
  ;; Returns (values results total-scanned entries-scanned)
  ;; BUG-1: Stops recursing into subdirectories once max-results are found.
  ;; BUG-3: Hard cap on entries scanned via max-scanned budget.
  (define results (box '()))
  (define total (box 0))
  (define scanned (box 0))

  (define (add-result! rel)
    (set-box! total (add1 (unbox total)))
    (when (< (length (unbox results)) max-results)
      (set-box! results (append (unbox results) (list rel)))))

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

  ;; depth-first walk
  (define (walk current-dir depth)
    (when (budget-remaining?)
      (define entries (with-safe-fallback '() (directory-list current-dir #:build? #f)))
      (for ([entry (in-list entries)]
            #:break (not (budget-remaining?)))
        (set-box! scanned (add1 (unbox scanned)))
        (define entry-str (path->string entry))
        (define full-path (build-path current-dir entry))
        (define rel-path (find-relative-path root-path full-path))

        ;; Skip VCS dirs and hidden entries (unless root is hidden)
        (unless (should-skip-entry? entry-str)
          ;; Check type match
          (when (matches-type? full-path)
            ;; Check name match — always count, only store if not full
            (when (matches-name? entry-str)
              (add-result! (path->string rel-path))))
          ;; BUG-1: Recurse into subdirectories only if results NOT full,
          ;; depth allows, and budget remains.
          (when (and (directory-exists? full-path)
                     (< depth max-depth)
                     (not (results-full?))
                     (budget-remaining?))
            (walk full-path (add1 depth)))))))

  (walk root-path 0)
  (values (unbox results) (unbox total) (unbox scanned)))

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
        ;; Resolve to absolute path for checks
        (define root-path (simple-form-path path-str))

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

           ;; 5. Walk the tree (BUG-3: with scan budget)
           (define-values (results total-found entries-scanned)
             (walk-dir root-path name-re type-filter max-depth max-results root-is-hidden))

           (define budget-exceeded? (>= entries-scanned MAX-SCAN-BUDGET))
           (define truncated? (or (> total-found max-results) budget-exceeded?))

           (make-success-result results
                                (hasheq 'total-found
                                        total-found
                                        'truncated?
                                        truncated?
                                        'search-root
                                        (path->string root-path)
                                        'scanned
                                        entries-scanned))])])]))

;; --------------------------------------------------
;; Tool definition via define-tool macro
;; --------------------------------------------------

(define-tool
 find
 #:description
 "Recursively list files and directories matching a name pattern. Supports glob patterns and type filtering."
 #:required ("path")
 #:properties [(path "string" "Root directory to search")
               (name "string" "Name pattern (glob, optional)")
               (type "string" "Type filter: 'file', 'dir', or 'any' (default)")
               (max-depth "integer" "Maximum directory depth to search (default 10)")
               (max-results "integer" "Maximum results to return (default 100)")]
 find-handler)

(provide find)
