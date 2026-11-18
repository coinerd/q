#lang racket
;; @speed fast
;; @suite testing
;; @boundary integration
(require rackunit
         racket/runtime-path
         "helpers/supplemental-evidence-fixture.rkt"
         "../scripts/testing/full-plan.rkt")

(define-runtime-path qt "../qt")
(define-runtime-path root "..")

;; Launch the checkout's actual executable, even from a fixture cwd. Drain
;; both pipes concurrently and bound each canary independently.
(define (invoke cwd . args)
  (define cust (make-custodian))
  (dynamic-wind void
                (lambda ()
                  (parameterize ([current-custodian cust]
                                 [current-directory cwd]
                                 [subprocess-group-enabled #t])
                    (define-values (child out in err) (apply subprocess #f #f #f qt args))
                    (dynamic-wind void
                                  (lambda ()
                                    (close-output-port in)
                                    (define stdout
                                      (delay/thread
                                        (port->string out)))
                                    (define stderr
                                      (delay/thread
                                        (port->string err)))
                                    (unless (sync/timeout 30 child)
                                      (error 'qt-canary "timed out: ~s" args))
                                    (values (subprocess-status child) (force stdout) (force stderr)))
                                  (lambda ()
                                    (when (eq? (subprocess-status child) 'running)
                                      (subprocess-kill child #t))
                                    (subprocess-wait child)))))
                (lambda () (custodian-shutdown-all cust))))

(module+ test
  (test-case "help advertises the real interface surfaces"
    (define-values (status out err) (invoke root "--help"))
    (check-equal? status 0)
    (check-equal? err "")
    (for ([phrase '("quick --changed BASE..HEAD" "default --file FILE"
                                                 "release --candidate SHA"
                                                 "--lane NAME"
                                                 "Same as `default`")])
      (check-true (string-contains? out phrase) phrase)))

  (test-case "bare default executes its offline lane; explicit default is only a reproduction"
    (define dir (make-temporary-file "b4-qt-default-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (fixture-put dir
                    "tests/tiny.rkt"
                    (string-append "#lang racket\n;; @speed fast\n(require rackunit)\n"
                                   "(module+ test (test-case \"miniature\" (check-true #t)))\n"))
       (define-values (status out err) (invoke dir))
       (check-equal? status 0 (string-append out err))
       (check-regexp-match #rx"offline PR verification only" out)
       (check-regexp-match #rx"default passed: 1 files" out)
       (define-values (repro-status repro repro-err)
         (invoke dir "default" "--file" "tests/tiny.rkt" "--explain"))
       (check-equal? repro-status 0 (string-append repro repro-err))
       (check-regexp-match #rx"explicit reproduction only, not a PR pass" repro)
       (check-regexp-match #rx"reproduction files: 1" repro)
       (check-false (regexp-match? #rx"default passed:" repro))
       (define-values (refused _out why)
         (invoke dir "default" "--file" "tests/tiny.rkt" "--record-gate-evidence"))
       (check-not-equal? refused 0)
       (check-regexp-match #rx"default does not accept CI-surface option --record-gate-evidence" why))
     (lambda () (delete-directory/files dir))))

  (test-case "quick impact entrypoint fails closed on an empty Git range"
    (define-values (status out err) (invoke root "quick" "--changed" "HEAD..HEAD" "--explain"))
    (check-not-equal? status 0)
    (check-regexp-match #rx"empty \\(zero selection is a failure, not a pass\\)" err)
    (check-false (regexp-match? #rx"changed-file impact selection|quick passed:" out)))

  (test-case "release binds evidence to a candidate and never claims the hosted obligation locally"
    (define-values (unbound _out why) (invoke root "release" "--evidence" "unused" "--explain"))
    (check-not-equal? unbound 0)
    (check-regexp-match #rx"--evidence requires --candidate SHA" why)
    (define dir (make-temporary-file "b4-qt-release-~a" 'directory))
    (dynamic-wind void
                  (lambda ()
                    (fixture-git dir "init" "-q")
                    (fixture-put dir "info.rkt" "#lang info\n(define version \"0.0.0\")\n")
                    (fixture-put dir "tests/tiny.rkt" "#lang racket\n;; @speed fast\n")
                    (fixture-git dir "add" ".")
                    (fixture-git dir
                                 "-c"
                                 "user.name=Fixture"
                                 "-c"
                                 "user.email=fixture@invalid"
                                 "-c"
                                 "commit.gpgsign=false"
                                 "commit"
                                 "-qm"
                                 "candidate")
                    (define sha (fixture-git dir "rev-parse" "HEAD"))
                    (define-values (status out err) (invoke dir "release" "--explain"))
                    (check-equal? status 0 (string-append out err))
                    (check-regexp-match #rx"RELEASE OBLIGATION ci-verification \\[UNAVAILABLE\\]" out)
                    (check-false (regexp-match? #rx"release passed:" out))
                    (fixture-put dir "scratch/note.txt" "unrelated\n")
                    (check-equal? (release-candidate-check dir sha) sha)
                    (fixture-put dir "tests/new-input.rkt" "#lang racket\n")
                    (check-exn #rx"relevant untracked file"
                               (lambda () (release-candidate-check dir sha)))
                    (delete-file (build-path dir "tests/new-input.rkt"))
                    (fixture-put dir "info.rkt" "#lang info\n(define version \"0.0.1\")\n")
                    (check-exn #rx"dirty checkout" (lambda () (release-candidate-check dir sha))))
                  (lambda () (delete-directory/files dir))))

  (test-case "policy lane dispatch keeps sharding and rejects conflicting selection"
    (define dir (make-temporary-file "b4-qt-policy-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (for ([name '("test-a.rkt" "test-b.rkt")])
         (fixture-put dir
                      (build-path "tests" "workflows" name)
                      "#lang racket\n;; @suite workflows\n"))
       (define-values (status out err) (invoke dir "--lane" "workflows" "--explain"))
       (check-equal? status 0 (string-append out err))
       (check-regexp-match #rx"lane: workflows \\(suite workflows; timeout 90; shard-plan #f" out)
       (check-regexp-match #rx"shard: 0 of 2" out)
       (check-false (regexp-match? #rx"passed:" out)))
     (lambda () (delete-directory/files dir)))
    (define-values (refused _out why) (invoke root "--lane" "fast" "--suite" "tui" "--explain"))
    (check-not-equal? refused 0)
    (check-regexp-match #rx"--lane cannot be combined with --suite" why)))
