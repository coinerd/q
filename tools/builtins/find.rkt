#lang racket/base

(require racket/file
         racket/string
         racket/path
         (only-in "../tool.rkt" make-success-result make-error-result))

(provide tool-find)

;; --------------------------------------------------
;; Result helpers - return tool-result structs
;; --------------------------------------------------

(define (ok content details)
  (make-success-result content details))

(define (err msg)
  (make-error-result msg))

;; --------------------------------------------------
;; Glob pattern → regexp
;; --------------------------------------------------

(define (glob->regexp pattern)
  (define escaped
    (for/list ([ch (in-string pattern)])
      (case ch
        [(#\*) ".*"]
        [(#\?) "."]
        [(#\. #\+ #\( #\) #\[ #\] #\{ #\} #\\ #\^ #\$ #\|) (string #\\ ch)]
        [else (string ch)])))
  (regexp (string-append "^" (string-join escaped "") "$")))

;; --------------------------------------------------
;; VCS / skip predicates
;; --------------------------------------------------

(define VCS-DIRS '(".git" ".hg" ".svn" "node_modules"))

(define (vcs-dir? name)
  (member name VCS-DIRS))

(define (hidden-name? name)
  (and (> (string-length name) 0) (char=? (string-ref name 0) #\.)))

(define (path-component-hidden? abs-path)
  ;; Check if any component of the path is hidden
  (for/or ([part (in-list (explode-path abs-path))])
    (define s (path->string part))
    (hidden-name? s)))

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
    (define entries
      (with-handlers ([exn:fail? (lambda (e) '())])
        (directory-list current-dir #:build? #f)))
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
;; Main tool function
;; --------------------------------------------------

(define (tool-find args [exec-ctx #f])
  ;; (safe-mode path check is done by scheduler, not here)
  (cond
    [(not (hash-has-key? args 'path)) (err "Missing required argument: path")]
    [else
     (define path-str (hash-ref args 'path))
     (cond
       [(not (string? path-str)) (err "Argument 'path' must be a string")]

       ;; 2. Path must exist
       [(not (or (directory-exists? path-str) (file-exists? path-str)))
        (err (format "Path not found: ~a" path-str))]

       ;; 3. Path must be a directory
       [(not (directory-exists? path-str)) (err (format "Path is not a directory: ~a" path-str))]

       [else
        ;; 4. Parse optional arguments
        (define name-pattern (hash-ref args 'name #f))
        (define type-filter (hash-ref args 'type "any"))
        (define max-depth (hash-ref args 'max-depth 10))
        (define max-results (hash-ref args 'max-results 100))

        ;; Compile glob pattern if provided
        (define name-re
          (if name-pattern
              (glob->regexp name-pattern)
              #f))

        ;; Check if root path itself is under a hidden directory
        (define root-path (simple-form-path path-str))
        (define root-is-hidden (path-component-hidden? root-path))

        ;; 5. Walk the tree
        (define-values (results total-found)
          (walk-dir root-path name-re type-filter max-depth max-results root-is-hidden))

        (define truncated? (> total-found max-results))

        (ok results
            (hasheq 'total-found total-found 'truncated? truncated? 'search-root path-str))])]))
