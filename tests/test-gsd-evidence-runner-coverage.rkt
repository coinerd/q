#lang racket
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @timeout 120
(require rackunit
         racket/runtime-path
         "../extensions/gsd/evidence-runner-coverage.rkt")
(define-runtime-path owner "..")
(module+ test
  (test-case "installed production runner closure qualifies; worker replacement does not"
    (define tmp (make-temporary-file "runner-coverage-~a" 'directory))
    (define git (find-executable-path "git"))
    (define (run . args)
      (define out (open-output-string))
      (unless (parameterize ([current-output-port out]
                             [current-error-port (open-output-nowhere)])
                (apply system* git "-C" tmp args))
        (error 'fixture "git failed"))
      (string-trim (get-output-string out)))
    (define command "racket scripts/run-tests.rkt tests/probe.rkt")
    (dynamic-wind
     void
     (lambda ()
       (check-true
        (parameterize ([current-output-port (open-output-nowhere)]
                       [current-error-port (open-output-nowhere)])
          (system* git "clone" "--shared" "--no-checkout" (path->string owner) (path->string tmp))))
       (define head (run "rev-parse" "HEAD"))
       (check-true (regression-command-covered? tmp head command "tests/probe.rkt"))
       (run "read-tree" "HEAD")
       (define replacement (build-path tmp "fake-runner.rkt"))
       (display-to-file "#lang racket/base\n(void)\n" replacement)
       (define blob (run "hash-object" "-w" (path->string replacement)))
       (run "update-index" "--cacheinfo" (string-append "100644," blob ",scripts/run-tests/cli.rkt"))
       (define tree (run "write-tree"))
       (define altered
         (run "-c"
              "user.name=Fixture"
              "-c"
              "user.email=fixture@example.invalid"
              "commit-tree"
              tree
              "-p"
              head
              "-m"
              "replace trusted dependency"))
       (check-false (regression-command-covered? tmp altered command "tests/probe.rkt"))
       (check-false (regression-command-covered? tmp
                                                 head
                                                 "racket untrusted.rkt tests/probe.rkt"
                                                 "tests/probe.rkt")))
     (lambda () (delete-directory/files tmp)))))
