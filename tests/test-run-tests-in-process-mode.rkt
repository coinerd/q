#lang racket/base

;; @speed fast
;; @suite testing
;; @isolation subprocess

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         racket/runtime-path
         racket/system
         "../scripts/run-tests.rkt")

(define-runtime-path here ".")
(define project-root (simplify-path (build-path here "..")))

(define (write-temp-test content)
  (define dir (make-temporary-file "q-in-process-test-~a" 'directory))
  (define file (build-path dir "test-pure.rkt"))
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

(define suite
  (test-suite "run-tests in-process/grouped modes"

    (test-case "parse-args accepts --mode and defaults to auto"
      (define-values (jobs
                      seq?
                      timeout
                      strict?
                      suite
                      extra
                      repeat
                      record?
                      inventory?
                      diagnose?
                      mode
                      json-out)
        (parse-args '("--mode" "in-process" "--suite" "unit-fast")))
      (check-equal? mode 'in-process)
      (check-equal? suite 'unit-fast)
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
                      default-mode
                      _json-out)
        (parse-args '()))
      (check-equal? default-mode 'auto))

    (test-case "in-process and subprocess modes produce equivalent pure test results"
      (define-values (file dir)
        (write-temp-test (string-append "#lang racket/base\n"
                                        "(require rackunit rackunit/text-ui)\n"
                                        "(define s (test-suite \"pure\"\n"
                                        "  (test-case \"one\" (check-equal? (+ 1 1) 2))\n"
                                        "  (test-case \"two\" (check-true (string? \"ok\")))))\n"
                                        "(run-tests s)\n")))
      (dynamic-wind void
                    (lambda ()
                      (define path (path->string file))
                      (define in-process (run-single-file path #:timeout 120000 #:mode 'in-process))
                      (define subprocess (run-single-file path #:timeout 120000 #:mode 'subprocess))
                      (check-equal? (test-file-result-exit-code in-process) 0)
                      (check-equal? (test-file-result-exit-code subprocess) 0)
                      (check-equal? (test-file-result-total in-process) 2)
                      (check-equal? (test-file-result-total subprocess) 2)
                      (check-equal? (list (test-file-result-passed in-process)
                                          (test-file-result-failed in-process)
                                          (test-file-result-total in-process))
                                    (list (test-file-result-passed subprocess)
                                          (test-file-result-failed subprocess)
                                          (test-file-result-total subprocess))))
                    (lambda () (delete-dir/safe dir))))

    (test-case "in-process mode runs module+ test submodules"
      (define-values (file dir)
        (write-temp-test
         (string-append "#lang racket/base\n"
                        "(module+ test\n"
                        "  (require rackunit rackunit/text-ui)\n"
                        "  (define s (test-suite \"module+\" (test-case \"ok\" (check-true #t))))\n"
                        "  (run-tests s))\n")))
      (dynamic-wind void
                    (lambda ()
                      (define result
                        (run-single-file (path->string file) #:timeout 120000 #:mode 'in-process))
                      (check-equal? (test-file-result-exit-code result) 0)
                      (check-equal? (test-file-result-total result) 1))
                    (lambda () (delete-dir/safe dir))))

    (test-case "CLI accepts grouped mode on explicit pure file"
      (define-values (file dir)
        (write-temp-test
         (string-append "#lang racket/base\n"
                        "(require rackunit rackunit/text-ui)\n"
                        "(define s (test-suite \"cli\" (test-case \"ok\" (check-true #t))))\n"
                        "(run-tests s)\n")))
      (dynamic-wind void
                    (lambda ()
                      (define-values (code out err)
                        (run/capture (format "racket scripts/run-tests.rkt --mode grouped ~a" file)))
                      (check-equal? code 0 err)
                      (check-true (regexp-match? #rx"1 total, 1 passed" out)))
                    (lambda () (delete-dir/safe dir))))))

(run-tests suite)
