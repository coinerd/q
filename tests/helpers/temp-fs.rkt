#lang racket

;; tests/helpers/temp-fs.rkt — T05: Temporary filesystem test helpers
;;
;; Provides with-temp-file and with-temp-dir macros that ensure
;; cleanup regardless of test success or failure.

(require racket/file
         rackunit)

(provide with-temp-file
         with-temp-dir)

;; ============================================================
;; with-temp-file
;; ============================================================

;; (with-temp-file (path) body ...)
;; Creates a temporary file, binds path, runs body, deletes file on exit.
(define-syntax-rule (with-temp-file (path-id) body ...)
  (let* ([tmp-dir (find-system-path 'temp-dir)]
         [path-id (make-temporary-file "q-test-~a" tmp-dir)])
    (dynamic-wind
      (lambda () (void))
      (lambda () body ...)
      (lambda ()
        (when (file-exists? path-id)
          (delete-file path-id))))))

;; ============================================================
;; with-temp-dir
;; ============================================================

;; (with-temp-dir (dir) body ...)
;; Creates a temporary directory, binds dir, runs body, deletes recursively on exit.
(define-syntax-rule (with-temp-dir (dir-id) body ...)
  (let ([dir-id (make-temporary-file "q-testdir-~a" 'directory)])
    (dynamic-wind
      (lambda () (void))
      (lambda () body ...)
      (lambda ()
        (when (directory-exists? dir-id)
          (delete-directory/files dir-id))))))
