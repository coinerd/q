#lang racket
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 90
;; @covers extensions/gsd/go-orchestrator.rkt extensions/gsd/red-checkpoint.rkt
(require rackunit
         "helpers/delivery-fixtures.rkt"
         "helpers/private-fixture-templates.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         (only-in "../extensions/gsd/go-orchestrator.rkt" run-campaign-wave)
         (only-in "../extensions/gsd/campaign-result.rkt" campaign-result-status)
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/review-proof.rkt"
         "../extensions/gsd/composition-root.rkt"
         "../extensions/gsd/verification-job.rkt"
         "../extensions/gsd-planning/execution-policy.rkt")

(module+ test
  (test-case "outer timeout reaps owned structured RED without cancelling ambient work"
    (define root (make-temporary-file "checkpoint-cancel-~a" 'directory))
    (define repo (build-path root "q"))
    (define marker (build-path root "adapter-entered"))
    (define ambient (make-verification-registry))
    (define owned #f)
    (define unrelated #f)
    (define phases '())
    (define verified? #f)
    (define (put path text)
      (make-directory* (path-only path))
      (display-to-file text path #:exists 'truncate))
    (dynamic-wind
     void
     (lambda ()
       (make-directory repo)
       (git-quiet! repo "init" "-q")
       (hermetic-identity! repo)
       (define remote (build-path root "origin.git"))
       (make-directory remote)
       (git-quiet! remote "init" "--bare" "-q")
       (git-quiet! repo "remote" "add" "origin" "https://github.com/example/q.git")
       (git-quiet! repo
                   "config"
                   (string-append "url.file://" (path->string remote) ".insteadOf")
                   "https://github.com/example/q.git")
       (put (build-path repo ".gitignore") "compiled/\n")
       (put (build-path repo "source.rkt")
            (format (string-append "#lang racket/base\n(provide answer)\n"
                                   "(define (answer) (call-with-output-file ~s "
                                   "(lambda (out) (display \"entered\" out)) #:exists 'truncate) "
                                   "(sleep 120) 0)\n")
                    (path->string marker)))
       (git-quiet! repo "add" ".")
       (git-quiet! repo "commit" "-qm" "blocking baseline")
       (git-quiet! repo "push" "-q" "origin" "HEAD:refs/heads/main")
       (git-quiet! repo "update-ref" "refs/remotes/origin/main" "HEAD")
       (make-directory* (build-path root ".planning" "waves"))
       (write-plan! root 0 "Checkpoint" "checkpoint")
       (write-wave-doc! root
                        0
                        "checkpoint"
                        '("q/source.rkt" "q/tests/probe.rkt")
                        "raco test tests/probe.rkt")
       (define record (migrate-campaign! root))
       (persist-campaign! root record)
       (set! unrelated
             (start-result-job-id
              (verification-start!
               ambient
               (verification-identity "unrelated" "0" "sleep" (path->string root) "test")
               (path->string (find-executable-path "sleep"))
               '("120")
               #:log-path (build-path root "unrelated.log"))))
       (parameterize ([current-owned-proof-root (build-path root "owned-proofs")]
                      [current-gsd-verification-registry ambient])
         (define outcome
           (run-campaign-wave
            root
            record
            0
            #:fence-token 1
            ;; Fixture budget within production limits: the outer deadline
            ;; must stay BELOW the structured adapter child's own 60 s cap so
            ;; the timeout reaps a LIVE child (its regression module sleeps
            ;; 120 s). 25 s was measured too tight under a loaded 4-CPU
            ;; runner: the adapter child never reached the module body before
            ;; reaping, so the reach-adapter proof fired spuriously. 45 s
            ;; keeps the contract identical (outer cancel of a live child,
            ;; ambient registry untouched) while absorbing cold-start/load
            ;; jitter; owned lanes never consume #:timeout-retries, so the
            ;; window runs once and the file stays inside its 90 s budget.
            #:timeout-sec 45
            #:timeout-retries 1
            #:no-change-retries 1
            #:isolate? #t
            #:stall-soft-limit #f
            #:stall-hard-limit #f
            #:runner
            (lambda (_)
              (set! owned (current-gsd-verification-registry))
              (define phase (current-gsd-author-phase))
              (set! phases (append phases (list (hash-ref phase 'phase))))
              (when (eq? (hash-ref phase 'phase) 'test-author)
                (put
                 (build-path (current-directory) "tests" "probe.rkt")
                 (string-append
                  "#lang racket/base\n(require rackunit \"../source.rkt\")\n(provide evidence-suite)\n"
                  "(define evidence-suite (test-suite \"suite\" (test-case \"target\" (check-equal? (answer) 1))))\n"
                  "(module+ test (require rackunit/text-ui) (exit (run-tests evidence-suite)))\n")))
              'ok)
            #:verifier (lambda (_)
                         (set! verified? #t)
                         (error 'verify "unexpected full Verify"))))
         (check-true (file-exists? marker)
                     (format "must reach actual structured adapter: ~s" outcome))
         (check-equal? phases '(test-author))
         (check-eq? (campaign-result-status outcome)
                    'wave-cancelled
                    "owned timeout stops honestly even with generic retries configured")
         (check-false verified?)
         (check-false (load-delivery-journal root (campaign-plan-id record) 0))
         (check-eq? (verification-job-state (verification-status ambient unrelated)) 'running)
         (check-false (verification-job-reaped? (verification-status ambient unrelated)))
         ;; The only permitted active job is the caller's unrelated sleep. If
         ;; the runner created a dedicated registry it must be entirely empty.
         (check-not-eq? owned ambient "never close a shared caller registry")
         (define jobs (verification-close-registry! owned))
         (check-equal? (length jobs) 1 "actual structured child, not an empty private registry")
         (for ([job (in-list jobs)])
           (check-eq? (verification-job-state job) 'cancelled)
           (check-true (verification-job-reaped? job))
           (define proc (build-path "/proc" (number->string (verification-job-pid job))))
           (check-false (directory-exists? proc)))
         (check-equal? (registry-active-count owned)
                       0
                       "outer timeout must cancel and reap the checkpoint child")))
     (lambda ()
       ;; Always reap the real child even when the RED assertion raises.
       (when owned
         (verification-cancel-campaign! owned (path->string (path->complete-path root))))
       (when unrelated
         (verification-cancel! ambient unrelated))
       (delete-directory/files root)))))
