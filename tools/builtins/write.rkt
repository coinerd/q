#lang racket/base

(require racket/file
         (only-in "../tool.rkt" make-success-result make-error-result)
         (only-in "../../util/path-helpers.rkt" path-only)
         (only-in "../../util/errors.rkt" raise-tool-error tool-error?)
         (only-in "../../util/safe-mode-predicates.rkt"
                  safe-mode? allowed-path? safe-mode-project-root))

(provide tool-write
         current-max-write-bytes)

;; Maximum write size in bytes (default 1MB) — SEC-03
(define current-max-write-bytes (make-parameter 1048576))

;; Main tool function
(define (tool-write args [exec-ctx #f])
  (define path-str (hash-ref args 'path #f))
  (cond
    [(not path-str) (make-error-result "Missing required argument: path")]
    [(and (safe-mode?) (not (allowed-path? path-str)))
     ;; Defense-in-depth: verify path even if scheduler already checked (SEC-09)
     (make-error-result
      (format "write: path '~a' outside project root (~a)"
              path-str (safe-mode-project-root)))]
    [else
     (define content-str (hash-ref args 'content ""))
     (with-handlers ([exn:fail:filesystem?
                      (lambda (e) (make-error-result (format "Write error: ~a" (exn-message e))))]
                     [tool-error?
                      (lambda (e) (make-error-result (exn-message e)))])
       ;; SEC-03: Enforce max-write-bytes limit
       (define content-bytes (string->bytes/utf-8 content-str))
       (define bytes-count (bytes-length content-bytes))
       (when (> bytes-count (current-max-write-bytes))
         (raise-tool-error (format "write rejected: ~a bytes exceeds limit of ~a"
                                   bytes-count (current-max-write-bytes))
                           'write))

       ;; Create parent directories if needed
       (define dir (path-only path-str))
       (when (and dir (not (directory-exists? dir)))
         (make-directory* dir))

       ;; Write content
       (call-with-output-file path-str (lambda (out) (display content-str out)) #:exists 'replace)

       (make-success-result (list (format "Wrote ~a bytes to ~a" bytes-count path-str))
                            (hasheq 'path path-str 'bytes-written bytes-count)))]))

;; path-only imported from util/path-helpers.rkt
