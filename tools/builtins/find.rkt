#lang racket/base

;; tools/builtins/find.rkt — File/directory search tool
;;
;; v0.33.2 W0: Converted to define-tool macro.

(require "../../util/error-helpers.rkt"
         racket/file
         racket/string
         racket/path
         (only-in "../tool.rkt" make-success-result make-error-result)
         (only-in "../define-tool.rkt" define-tool)
         (only-in "builtin-helpers.rkt" require-safe-path!)
         (only-in "../../util/glob.rkt" glob->regexp)
         (only-in "../../util/path-filters.rkt"
                  hidden-name?
                  vcs-dir?
                  skip-dirs
                  path-component-hidden?)
         (only-in "../../util/path-helpers.rkt" expand-home-path))

;; --------------------------------------------------
;; Core recursive walk
;; --------------------------------------------------

(define (walk-dir root-path name-re type-filter max-depth max-results root-is-hidden)
  ;; Returns (values results total-scanned)
  (define results (box '()))
  (define total (box 0))

  (define (add-result! rel)
    (set-box! total (add1 (unbox total)))
    (when (< (length (unbox results)) max-results)
      (set-box! results (append (unbox results) (list rel)))))

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
    (define entries (with-safe-fallback '() (directory-list current-dir #:build? #f)))
    (for ([entry (in-list entries)])
      (define entry-str (path->string entry))
      (define full-path (build-path current-dir entry))
      (define rel-path (find-relative-path root-path full-path))

      ;; Skip VCS dirs and hidden entries (unless root is hidden)
      (unless (should-skip-entry? entry-str)
        ;; Check type match
        (when (matches-type? full-path)
          ;; Check name match
          (when (matches-name? entry-str)
            (add-result! (path->string rel-path))))
        ;; Recurse into subdirectories if depth allows
        (when (and (directory-exists? full-path) (< depth max-depth))
          (walk full-path (add1 depth))))))

  (walk root-path 0)
  (values (unbox results) (unbox total)))

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
        ;; 4. Parse optional arguments
        (define name-pattern (hash-ref args 'name #f))
        (define type-filter (hash-ref args 'type "any"))
        (define max-depth (hash-ref args 'max-depth 10))
        (define max-results (hash-ref args 'max-results 100))

        ;; Compile glob pattern if provided
        (define name-re
          (if name-pattern
              (glob->regexp name-pattern #:allow-slash? #t)
              #f))

        ;; Check if root path itself is under a hidden directory
        (define root-path (simple-form-path path-str))
        (define root-is-hidden (path-component-hidden? root-path))

        ;; 5. Walk the tree
        (define-values (results total-found)
          (walk-dir root-path name-re type-filter max-depth max-results root-is-hidden))

        (define truncated? (> total-found max-results))

        (make-success-result
         results
         (hasheq 'total-found total-found 'truncated? truncated? 'search-root path-str))])]))

;; --------------------------------------------------
;; Tool definition via define-tool macro
;; --------------------------------------------------

(define-tool find
  #:description "Recursively list files and directories matching a name pattern. Supports glob patterns and type filtering."
  #:required ("path")
  #:properties
    [(path "string" "Root directory to search")
     (name "string" "Name pattern (glob, optional)")
     (type "string" "Type filter: 'file', 'dir', or 'any' (default)")
     (max-depth "integer" "Maximum directory depth to search (default 10)")
     (max-results "integer" "Maximum results to return (default 100)")]
  find-handler)

(provide find)
