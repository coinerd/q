#lang racket/base

;; @speed fast
;; @suite default

(require rackunit
         racket/file
         racket/string
         "../tools/builtins/edit.rkt"
         "../tools/tool.rkt"
         (only-in "../sandbox/worker-tools.rkt" execute-edit current-allowed-roots)
         (only-in "../sandbox/ipc-protocol.rkt" ipc-response-status ipc-response-error-message)
         (only-in "../util/racket-source-validation.rkt"
                  validate-proposed-racket-source
                  current-racket-parse-timeout-ms))

(define (result-message result)
  (hash-ref (car (tool-result-content result)) 'text))

(define (with-temp-file suffix content proc)
  (define path (make-temporary-file (string-append "q-edit-parse-~a" suffix)))
  (dynamic-wind (lambda () (display-to-file content path #:exists 'replace))
                (lambda () (proc path))
                (lambda ()
                  (when (file-exists? path)
                    (delete-file path)))))

(test-case "unparseable Racket edit is rejected and leaves original bytes unchanged"
  (define original "#lang racket/base\n(define (increment x) (+ x 1))\n")
  (with-temp-file ".rkt"
                  original
                  (lambda (path)
                    (define result
                      (tool-edit (hasheq 'path path 'old-text "(+ x 1)" 'new-text "(+ x 1")))
                    (check-true (tool-result-is-error? result))
                    (check-true (string-contains? (result-message result) "read-syntax"))
                    (check-true (string-contains? (result-message result) "unparseable"))
                    (check-equal? (file->string path) original))))

(test-case "valid Racket edit still succeeds"
  (with-temp-file ".rkt"
                  "#lang racket/base\n(define answer 41)\n"
                  (lambda (path)
                    (define result (tool-edit (hasheq 'path path 'old-text "41" 'new-text "42")))
                    (check-false (tool-result-is-error? result))
                    (check-equal? (file->string path) "#lang racket/base\n(define answer 42)\n"))))

(test-case "non-Racket text edits are not parse validated"
  (with-temp-file ".txt"
                  "balanced text"
                  (lambda (path)
                    (define result (tool-edit (hasheq 'path path 'old-text "balanced" 'new-text "(")))
                    (check-false (tool-result-is-error? result))
                    (check-equal? (file->string path) "( text"))))

(test-case "worker execution path rejects the same unparseable Racket edit"
  (define directory (make-temporary-file "q-worker-edit-parse-~a" 'directory))
  (define path (build-path directory "target.rkt"))
  (define original "#lang racket/base\n(define (increment x) (+ x 1))\n")
  (dynamic-wind (lambda () (display-to-file original path))
                (lambda ()
                  (define response
                    (parameterize ([current-allowed-roots (list directory)]
                                   [current-directory directory])
                      (execute-edit (hasheq 'path path 'old-text "(+ x 1)" 'new-text "(+ x 1"))))
                  (check-equal? (ipc-response-status response) 'error)
                  (check-true (string-contains? (ipc-response-error-message response) "read-syntax"))
                  (check-equal? (file->string path) original))
                (lambda () (delete-directory/files directory))))

(test-case "reader resolution failures become edit errors"
  (define original "#lang racket/base\n(define answer 41)\n")
  (with-temp-file
   ".rkt"
   original
   (lambda (path)
     (define result
       (tool-edit (hasheq 'path path 'old-text "#lang racket/base" 'new-text "#lang missing/reader")))
     (check-true (tool-result-is-error? result))
     (check-true (string-contains? (result-message result) "unparseable"))
     (check-equal? (file->string path) original))))

(test-case "custom reader cannot write files during parse validation"
  (define directory (make-temporary-file "q-edit-reader-~a" 'directory))
  (define target (build-path directory "target.rkt"))
  (define reader (build-path directory "reader.rkt"))
  (define marker (build-path directory "reader-side-effect"))
  (define original "#lang racket/base\n(define answer 41)\n")
  (dynamic-wind
   (lambda ()
     (display-to-file original target)
     (display-to-file
      (format (string-append "#lang racket/base\n"
                             "(require racket/file)\n"
                             "(provide read read-syntax get-info)\n"
                             "(define (touch) (display-to-file ~s ~s #:exists 'replace))\n"
                             "(define (read in) (touch) '(module m racket/base))\n"
                             "(define (read-syntax src in) (touch) "
                             "(datum->syntax #f '(module m racket/base)))\n"
                             "(define (get-info key default) default)\n")
              "owned"
              (path->string marker))
      reader))
   (lambda ()
     (define directive (format "#reader (file ~s)\nignored\n" (path->string reader)))
     (define result (tool-edit (hasheq 'path target 'old-text original 'new-text directive)))
     (check-true (tool-result-is-error? result))
     (check-true (string-contains? (result-message result)
                                   "custom reader directives are not allowed"))
     (check-false (file-exists? marker) "reader side effects must be blocked")
     (check-equal? (file->string target) original))
   (lambda () (delete-directory/files directory))))

(test-case "all supported Racket-family extensions reject malformed proposed content"
  (for ([suffix (in-list '(".rktl" ".scrbl" ".rktd"))])
    (with-temp-file suffix
                    "(valid)\n"
                    (lambda (path)
                      (define result
                        (tool-edit (hasheq 'path path 'old-text "(valid)" 'new-text "(invalid")))
                      (check-true (tool-result-is-error? result) suffix)
                      (check-equal? (file->string path) "(valid)\n" suffix)))))

(test-case "custom reader exit is converted to an edit error"
  (define directory (make-temporary-file "q-edit-exit-reader-~a" 'directory))
  (define target (build-path directory "target.rkt"))
  (define reader (build-path directory "exit-reader.rkt"))
  (define original "#lang racket/base\n(define answer 41)\n")
  (dynamic-wind (lambda ()
                  (display-to-file original target)
                  (display-to-file (string-append "#lang racket/base\n"
                                                  "(provide read read-syntax get-info)\n"
                                                  "(define (read in) (exit 23))\n"
                                                  "(define (read-syntax src in) (exit 23))\n"
                                                  "(define (get-info key default) default)\n")
                                   reader))
                (lambda ()
                  (define directive (format "#reader (file ~s)\nignored\n" (path->string reader)))
                  (define result
                    (tool-edit (hasheq 'path target 'old-text original 'new-text directive)))
                  (check-true (tool-result-is-error? result))
                  (check-true (string-contains? (result-message result)
                                                "custom reader directives are not allowed"))
                  (check-equal? (file->string target) original))
                (lambda () (delete-directory/files directory))))

(test-case "non-terminating custom reader is bounded and cannot change the target"
  (define directory (make-temporary-file "q-edit-loop-reader-~a" 'directory))
  (define target (build-path directory "target.rkt"))
  (define reader (build-path directory "loop-reader.rkt"))
  (define original "#lang racket/base\n(define answer 41)\n")
  (dynamic-wind (lambda ()
                  (display-to-file original target)
                  (display-to-file (string-append "#lang racket/base\n"
                                                  "(provide read read-syntax get-info)\n"
                                                  "(define (forever) (forever))\n"
                                                  "(define (read in) (forever))\n"
                                                  "(define (read-syntax src in) (forever))\n"
                                                  "(define (get-info key default) default)\n")
                                   reader))
                (lambda ()
                  (define directive (format "#reader (file ~s)\nignored\n" (path->string reader)))
                  (define started (current-inexact-milliseconds))
                  (define result
                    (parameterize ([current-racket-parse-timeout-ms 100])
                      (tool-edit (hasheq 'path target 'old-text original 'new-text directive))))
                  (check-true (tool-result-is-error? result))
                  (check-true (string-contains? (result-message result)
                                                "custom reader directives are not allowed"))
                  (check-true (< (- (current-inexact-milliseconds) started) 1000))
                  (check-equal? (file->string target) original))
                (lambda () (delete-directory/files directory))))

(test-case "reader policy ignores directive text inside strings and comments"
  (define content
    "#lang racket/base\n;; #reader (file \"evil.rkt\")\n(define example \"#reader is text\")\n")
  (check-false (validate-proposed-racket-source "safe.rkt" content)))

(test-case "reader-only validation stays below 50 ms for a typical warm edit"
  (define content "#lang racket/base\n(define (increment x) (+ x 1))\n")
  (check-false (validate-proposed-racket-source "typical.rkt" content))
  (define started (current-inexact-milliseconds))
  (for ([i (in-range 10)])
    (check-false (validate-proposed-racket-source "typical.rkt" content)))
  (check-true (< (/ (- (current-inexact-milliseconds) started) 10.0) 50.0)))
