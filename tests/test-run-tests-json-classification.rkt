#lang racket/base

;; @speed fast
;; @suite testing
;; @isolation subprocess

(require rackunit
         rackunit/text-ui
         json
         racket/file
         racket/path
         racket/runtime-path
         racket/system
         "../scripts/run-tests.rkt")

(define-runtime-path here ".")
(define project-root (simplify-path (build-path here "..")))

(define (result #:path [path "tests/example.rkt"]
                #:exit [exit-code 0]
                #:out [out ""]
                #:err [err ""]
                #:passed [passed 0]
                #:failed [failed 0]
                #:total [total 0])
  (test-file-result path
                    exit-code
                    (string->bytes/utf-8 out)
                    (string->bytes/utf-8 err)
                    17
                    passed
                    failed
                    total))

(define (write-temp-test content)
  (define dir (make-temporary-file "q-json-out-test-~a" 'directory))
  (define file (build-path dir "test-json-out.rkt"))
  (call-with-output-file file #:exists 'replace (lambda (out) (display content out)))
  (values file dir))

(define (delete-dir/safe dir)
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (delete-directory/files dir)))

(define (run/capture cmd)
  (parameterize ([current-directory project-root])
    (define out (open-output-string))
    (define err (open-output-string))
    (define code
      (parameterize ([current-output-port out]
                     [current-error-port err])
        (system/exit-code cmd)))
    (values code (get-output-string out) (get-output-string err))))

(define runner-module `(file ,(path->string (build-path project-root "scripts/run-tests.rkt"))))

(define classify-test-result*
  (dynamic-require runner-module
                   'classify-test-result
                   (lambda () (lambda (_) (error 'classify-test-result "missing export")))))

(define test-result->jsexpr*
  (dynamic-require runner-module
                   'test-result->jsexpr
                   (lambda () (lambda (_) (error 'test-result->jsexpr "missing export")))))

(define write-json-results!*
  (dynamic-require runner-module
                   'write-json-results!
                   (lambda () (lambda args (error 'write-json-results! "missing export")))))

(define suite
  (test-suite "run-tests structured classification + json"

    (test-case "parse-args accepts --json-out"
      (define-values (_jobs
                      _seq?
                      _timeout
                      _strict?
                      _suite
                      _extra
                      _repeat
                      _record?
                      _inventory?
                      _diagnose?
                      _mode
                      json-out)
        (parse-args '("--json-out" "/tmp/q-results.json")))
      (check-equal? json-out "/tmp/q-results.json"))

    (test-case "classify-test-result distinguishes core categories"
      (check-equal? (classify-test-result* (result #:exit 0 #:passed 1 #:total 1)) 'PASS)
      (check-equal? (classify-test-result* (result #:exit 0 #:total 0)) 'ZERO_PARSED)
      (check-equal? (classify-test-result* (result #:exit 2 #:err "timeout after 1s")) 'TIMEOUT)
      (check-equal? (classify-test-result* (result #:exit 1 #:failed 1 #:total 1 #:out "FAILURE"))
                    'ASSERTION_FAILURE)
      (check-equal? (classify-test-result* (result #:exit 1 #:err "read-syntax: expected a )"))
                    'COMPILE_FAILURE)
      (check-equal? (classify-test-result*
                     (result #:exit 1 #:err "standard-module-name-resolver: collection not found"))
                    'MODULE_LOAD_FAILURE)
      (check-equal? (classify-test-result* (result #:exit 1
                                                   #:err "missing environment variable API_KEY"))
                    'ENVIRONMENT_MISSING)
      (check-equal? (classify-test-result* (result #:exit 1 #:err "user break")) 'USER_BREAK))

    (test-case "test-result->jsexpr includes category and counts"
      (define js
        (test-result->jsexpr*
         (result #:path "tests/failing.rkt" #:exit 1 #:failed 1 #:total 1 #:out "FAILURE")))
      (check-equal? (hash-ref js 'path) "tests/failing.rkt")
      (check-equal? (hash-ref js 'category) "ASSERTION_FAILURE")
      (check-equal? (hash-ref js 'exit_code) 1)
      (check-equal? (hash-ref js 'total) 1))

    (test-case "write-json-results! writes per-file categories"
      (define out (make-temporary-file "q-results-~a.json"))
      (dynamic-wind
       void
       (lambda ()
         (write-json-results!* out
                               (list (result #:path "tests/pass.rkt" #:passed 1 #:total 1)
                                     (result #:path "tests/zero.rkt" #:total 0))
                               #:suite 'unit-fast
                               #:mode 'in-process
                               #:elapsed-ms 25)
         (define js (call-with-input-file out read-json))
         (check-equal? (hash-ref js 'suite) "unit-fast")
         (check-equal? (hash-ref js 'mode) "in-process")
         (define files (hash-ref js 'files))
         (check-equal? (length files) 2)
         (check-equal? (hash-ref (car files) 'category) "PASS")
         (check-equal? (hash-ref (cadr files) 'category) "ZERO_PARSED"))
       (lambda ()
         (when (file-exists? out)
           (delete-file out)))))

    (test-case "CLI writes --json-out for explicit passing file"
      (define-values (file dir)
        (write-temp-test
         (string-append "#lang racket/base\n"
                        "(require rackunit rackunit/text-ui)\n"
                        "(define s (test-suite \"json\" (test-case \"ok\" (check-true #t))))\n"
                        "(run-tests s)\n")))
      (define out (build-path dir "results.json"))
      (dynamic-wind
       void
       (lambda ()
         (define-values (code stdout stderr)
           (run/capture
            (format "racket scripts/run-tests.rkt --mode subprocess --json-out ~a ~a" out file)))
         (check-equal? code 0 stderr)
         (check-true (regexp-match? #rx"Category" stdout))
         (define js (call-with-input-file out read-json))
         (check-equal? (hash-ref js 'verdict) "pass")
         (define file-js (car (hash-ref js 'files)))
         (check-equal? (hash-ref file-js 'category) "PASS")
         (check-equal? (hash-ref file-js 'total) 1))
       (lambda () (delete-dir/safe dir))))))

(run-tests suite)
