#lang racket/base

;; @speed fast
;; @suite default

;; Deterministic regressions for edit write integrity.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         (only-in "../tools/tool.rkt" tool-result-is-error?)
         (only-in "../tools/builtins/edit.rkt"
                  current-edit-before-final-replace-hook
                  current-edit-before-replace-hook
                  tool-edit))

(define (with-temp-edit-file content proc)
  (define path (make-temporary-file "q-edit-integrity-~a"))
  (dynamic-wind (lambda () (display-to-file content path #:exists 'replace))
                (lambda () (proc path))
                (lambda ()
                  (when (file-exists? path)
                    (delete-file path)))))

(define integrity-suite
  (test-suite "Edit data integrity"

    (test-case "successful edit preserves executable permission mode"
      (with-temp-edit-file
       "#!/bin/sh\necho old\n"
       (lambda (path)
         (file-or-directory-permissions path #o751)
         (let ([result (tool-edit (hasheq 'path path 'old-text "echo old" 'new-text "echo new"))])
           (check-false (tool-result-is-error? result))
           (check-equal? (bitwise-and (file-or-directory-permissions path 'bits) #o777) #o751)
           (check-equal? (file->string path) "#!/bin/sh\necho new\n")))))

    (test-case "external content change before the initial guard is rejected"
      (with-temp-edit-file
       "alpha old omega\n"
       (lambda (path)
         (define result
           (parameterize ([current-edit-before-replace-hook
                           (lambda (guarded-path)
                             (display-to-file "external update\n" guarded-path #:exists 'replace))])
             (tool-edit (hasheq 'path path 'old-text "old" 'new-text "new"))))
         (check-true (tool-result-is-error? result))
         (check-equal? (file->string path) "external update\n"))))

    (test-case "concurrent chmod is rejected and its mode is preserved"
      (with-temp-edit-file
       "alpha old omega\n"
       (lambda (path)
         (file-or-directory-permissions path #o640)
         (define result
           (parameterize ([current-edit-before-replace-hook
                           (lambda (guarded-path)
                             (file-or-directory-permissions guarded-path #o600))])
             (tool-edit (hasheq 'path path 'old-text "old" 'new-text "new"))))
         (check-true (tool-result-is-error? result))
         (check-equal? (file->string path) "alpha old omega\n")
         (check-equal? (bitwise-and (file-or-directory-permissions path 'bits) #o777) #o600))))

    (test-case "change at the final pre-rename boundary is rejected"
      (with-temp-edit-file "alpha old omega\n"
                           (lambda (path)
                             (define result
                               (parameterize ([current-edit-before-final-replace-hook
                                               (lambda (guarded-path)
                                                 (display-to-file "final-boundary update\n"
                                                                  guarded-path
                                                                  #:exists 'replace))])
                                 (tool-edit (hasheq 'path path 'old-text "old" 'new-text "new"))))
                             (check-true (tool-result-is-error? result))
                             (check-equal? (file->string path) "final-boundary update\n"))))

    (test-case "identity replacement with identical content is rejected"
      (with-temp-edit-file
       "alpha old omega\n"
       (lambda (path)
         (define original (file->string path))
         (define result
           (parameterize ([current-edit-before-replace-hook
                           (lambda (guarded-path)
                             (define replacement
                               (make-temporary-file "q-edit-identity-~a"
                                                    #:base-dir (path-only guarded-path)))
                             (display-to-file original replacement #:exists 'replace)
                             (rename-file-or-directory replacement guarded-path #t))])
             (tool-edit (hasheq 'path path 'old-text "old" 'new-text "new"))))
         (check-true (tool-result-is-error? result))
         (check-equal? (file->string path) original))))))

(module+ test
  (run-tests integrity-suite))

(module+ main
  (run-tests integrity-suite))
