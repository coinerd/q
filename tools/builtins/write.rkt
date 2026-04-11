#lang racket/base

(require racket/file
         (only-in "../tool.rkt"
                  make-success-result make-error-result)
         (only-in "../../runtime/safe-mode.rkt"
                  allowed-path?))

(provide tool-write)

;; Main tool function
(define (tool-write args [exec-ctx #f])
  (define path-str    (hash-ref args 'path #f))
  (cond
    [(not path-str)
     (make-error-result "Missing required argument: path")]
    [else
     (define content-str (hash-ref args 'content ""))
     (cond
       [(not (allowed-path? path-str))
        (make-error-result
         (format "Access denied: path outside project root: ~a" path-str))]
       [else
        (with-handlers
         ([exn:fail:filesystem?
           (lambda (e)
             (make-error-result
              (format "Write error: ~a" (exn-message e))))])
          ;; Create parent directories if needed
          (define dir (path-only path-str))
          (when (and dir (not (directory-exists? dir)))
            (make-directory* dir))

          ;; Write content
          (call-with-output-file path-str
            (lambda (out)
              (display content-str out))
            #:exists 'replace)

          ;; Measure bytes written (UTF-8)
          (define bytes-count
            (bytes-length (string->bytes/utf-8 content-str)))

          (make-success-result
           (list (format "Wrote ~a bytes to ~a" bytes-count path-str))
           (hasheq 'path path-str
                   'bytes-written bytes-count)))])]))

;; Extract directory portion of a path string
(define (path-only p)
  (define-values (dir _base _must-be-dir?) (split-path p))
  (if (eq? dir 'relative)
      #f
      dir))
