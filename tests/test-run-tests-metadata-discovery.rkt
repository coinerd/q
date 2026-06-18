#lang racket

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         racket/runtime-path
         "../scripts/run-tests.rkt")

(define-runtime-path here ".")

(define (write-lines path lines)
  (call-with-output-file path
                         #:exists 'replace
                         (lambda (out)
                           (for ([line (in-list lines)])
                             (displayln line out)))))

(define metadata-discovery-tests
  (test-suite "run-tests metadata discovery"

    (test-case "metadata parser recognizes v0.99.30 schema tags"
      (define tmp (make-temporary-file "q-run-tests-metadata-~a.rkt"))
      (dynamic-wind void
                    (lambda ()
                      (write-lines tmp
                                   '("#lang racket" ";; @suite smoke tui"
                                                    ";; @speed fast"
                                                    ";; @requires network browser"
                                                    ";; @isolation subprocess"
                                                    ";; @not-test false"
                                                    "(require rackunit)"
                                                    "(check-true #t)"))
                      (clear-metadata-cache!)
                      (define meta (get-file-metadata tmp))
                      (check-equal? (hash-ref meta 'speed #f) 'fast)
                      (check-equal? (hash-ref meta 'suite #f) "smoke")
                      (check-equal? (hash-ref meta 'suites #f) '("smoke" "tui"))
                      (check-equal? (hash-ref meta 'requires #f) '("network" "browser"))
                      (check-equal? (hash-ref meta 'isolation #f) "subprocess")
                      (check-false (hash-ref meta 'not-test? #t)))
                    (lambda ()
                      (when (file-exists? tmp)
                        (delete-file tmp))
                      (clear-metadata-cache!))))

    (test-case "bare @not-test metadata excludes helper files from discovery"
      (define fixture (build-path here "test-w1-not-test-fixture.rkt"))
      (dynamic-wind (lambda ()
                      (write-lines fixture
                                   '("#lang racket" ";; @not-test"
                                                    ";; helper module intentionally named test-*"
                                                    "(provide helper-value)"
                                                    "(define helper-value 42)"))
                      (clear-metadata-cache!))
                    (lambda ()
                      (define rel "tests/test-w1-not-test-fixture.rkt")
                      (check-true (hash-ref (get-file-metadata rel) 'not-test? #f))
                      (check-false (member rel (collect-test-files 'all)))
                      (check-equal? (classify-exclusion-reason rel) 'metadata-not-test))
                    (lambda ()
                      (when (file-exists? fixture)
                        (delete-file fixture))
                      (clear-metadata-cache!))))))

(run-tests metadata-discovery-tests)
