#lang racket/base

;; tests/helpers/fixtures.rkt — Test cleanup and isolation macros
;;
;; Provides `with-temp-dir` and `with-env-var` macros for safe,
;; deterministic test cleanup via dynamic-wind.

(require racket/file)

(provide with-temp-dir
         with-env-var)

;; Create a temporary directory, pass it to body, guarantee cleanup.
;; Uses dynamic-wind so cleanup runs even if the test throws.
(define-syntax-rule (with-temp-dir dir-id body ...)
  (let ([dir-id (make-temporary-file "q-test-~a" 'directory)])
    (dynamic-wind
      void
      (lambda () body ...)
      (lambda ()
        (when (directory-exists? dir-id)
          (delete-directory/files dir-id #:must-exist? #f))))))

;; Save current env var value, set new value, restore on exit.
;; Uses dynamic-wind so restoration happens even if the test throws.
(define-syntax-rule (with-env-var var-name new-value body ...)
  (let ([old-value (getenv var-name)])
    (dynamic-wind
      void
      (lambda ()
        (putenv var-name new-value)
        body ...)
      (lambda ()
        (if old-value
            (putenv var-name old-value)
            (putenv var-name ""))))))
