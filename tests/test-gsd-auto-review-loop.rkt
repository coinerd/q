#lang racket/base
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @covers extensions/gsd/go-orchestrator.rkt
;; @covers extensions/gsd/review-session.rkt
;; @covers extensions/gsd/review-dispatch.rkt
;; Actual /go loop, default production coordinator and Python controller.
;; Only the model transport is deterministic; Git remote/publication, fresh
;; sessions, Verify, journal and rejection checkpoint are real.
(require rackunit
         racket/file
         racket/format
         racket/path
         racket/string
         json
         (only-in "helpers/private-fixture-templates.rkt" git-quiet! hermetic-identity!)
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/delivery-coordinator.rkt"
         "../extensions/gsd/review-dispatch.rkt"
         "../extensions/gsd/delivery-receipt.rkt"
         "../extensions/gsd/go-orchestrator.rkt"
         "../extensions/gsd/review-session.rkt"
         (only-in "../extensions/gsd/policy.rkt" current-gsd-wave-failure-context)
         (only-in "../extensions/gsd/wave-executor.rkt" current-gsd-worktree-isolation)
         (only-in "../extensions/gsd/delivery-verifier.rkt" current-gsd-delivery-branch-context)
         "../llm/model.rkt"
         "../llm/provider.rkt"
         "../util/loop-result.rkt")
(module+ test
  (test-case "production /go review rejection -> ordinary fenced retry -> real superset Verify -> r1 fresh review"
    (define root (make-temporary-file "auto-review-loop-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (make-directory* (build-path root ".planning/waves"))
       (display-to-file "# Plan: Review test\n\n## Waves\n\n- [Inbox] W0: Test → waves/W0-test.md\n"
                        (build-path root ".planning/PLAN.md"))
       (display-to-file
        (string-append "# Test\n\nGoal: test\n\nBranch: `campaign/review-test`; "
                       "GitHub issue [#9686](https://github.com/example/q/issues/9686), "
                       "milestone #895.\n\n## Verify\n\nraco test .\n")
        (build-path root ".planning/waves/W0-test.md"))
       (define rec (migrate-campaign! root))
       (define plan (campaign-plan-id rec))
       (define repo (build-path root "q"))
       (make-directory repo)
       (git-quiet! repo "init" "-q")
       (hermetic-identity! repo)
       (git-quiet! repo "checkout" "-b" "main")
       (make-directory* (build-path repo "scripts"))
       (display-to-file "(\"lint\" \"test (0)\" \"workflows (0)\")"
                        (build-path repo "scripts/required-pr-checks.policy"))
       (git-quiet! repo "add" "scripts/required-pr-checks.policy")
       (git-quiet! repo "commit" "-qm" "baseline")
       (define remote (build-path root "remote.git"))
       (git-quiet! root "init" "--bare" "-q" (path->string remote))
       (git-quiet! repo "remote" "add" "origin" "https://github.com/example/q.git")
       (git-quiet! repo
                   "config"
                   (string-append "url." (path->string remote) ".insteadOf")
                   "https://github.com/example/q.git")
       (git-quiet! repo "push" "-q" "origin" "main")
       (git-quiet! repo "fetch" "-q" "origin")
       (define implementations 0)
       (define verifies 0)
       (define reviews 0)
       (define attempts '())
       (define generations '())
       (define runner
         (make-auto-review-runner
          root
          plan
          #:provider-builder
          (lambda (_cwd)
            (set! reviews (add1 reviews))
            (make-mock-provider
             (make-model-response
              (list (hasheq 'type
                            "text"
                            'text
                            (jsexpr->string
                             (if (= reviews 1)
                                 (hasheq 'kind
                                         "request-changes"
                                         'scope
                                         (make-string 70 #\s)
                                         'report
                                         (make-string 70 #\r)
                                         'findings
                                         '("Add the second acceptance check"))
                                 (hasheq 'kind
                                         "review-failed"
                                         'reason
                                         "test ends at fresh r1 review; no approval fabricated")))))
              (hasheq 'prompt_tokens 3 'completion_tokens 4 'total_tokens 7)
              "glm-5.3"
              'stop)))))
       (define (recording-runner request)
         (set! generations (cons (hash-ref request 'generation) generations))
         (define receipt (hash-ref (load-delivery-journal root plan 0) 'receipt))
         (for ([key '(head branch attempt-id attempt-fence)])
           (check-equal? (hash-ref request key) (hash-ref receipt key)))
         (runner request))
       (define crashed? #f)
       (define (crash-once-coordinator dir p wave)
         (define outcome (default-delivery-coordinator dir p wave))
         (cond
           [(and (not crashed?) (eq? (delivery-outcome-kind outcome) 'review-rejected))
            ;; The REAL controller already pushed/read back the rejection.
            ;; Lose only the checkpoint outcome, as on process death.
            (set! crashed? #t)
            (delivery-outcome 'review-infra "simulated publication-to-checkpoint crash")]
           [else outcome]))
       (define (request-for rec)
         (make-campaign-request root
                                rec
                                (lambda (_)
                                  (string-append "Implement the acceptance checks"
                                                 (or (current-gsd-wave-failure-context) "")))
                                (lambda (_)
                                  (set! verifies (add1 verifies))
                                  ;; Genuine Verify against current committed implementation bytes;
                                  ;; the repair must preserve the original acceptance obligation.
                                  (define context (current-gsd-delivery-branch-context))
                                  (define worktree (hash-ref context 'worktree-path))
                                  (define contents (file->string (build-path worktree "acceptance")))
                                  (check-true (string-contains? contents "first acceptance"))
                                  (when (= verifies 2)
                                    (check-true (string-contains? contents "second acceptance")))
                                  #t)
                                #:auto-review-runner recording-runner
                                #:delivery-coordinator crash-once-coordinator))
       (define (execute request)
         (parameterize ([current-gsd-worktree-isolation #t]
                        [current-gsd-remote-published
                         (lambda (r b h)
                           (git-quiet! r "push" "-q" "origin" (string-append h ":refs/heads/" b))
                           #t)]
                        [current-gsd-freshness-check (lambda (_)
                                                       (campaign-freshness "test" "test" #f #f #f))])
           (execute-campaign-request!
            request
            (lambda (prompt)
              ;; Abort before any write/commit if isolation ever regresses.
              (define relative-cwd (find-relative-path root (current-directory)))
              (unless (and (relative-path? relative-cwd) (not (memq 'up (explode-path relative-cwd))))
                (error 'review-fixture "refusing implementation writes outside temporary fixture"))
              (set! implementations (add1 implementations))
              (when (= implementations 2)
                (check-true (string-contains? prompt "Add the second acceptance check"))
                (check-true (file-exists?
                             (build-path (current-directory)
                                         (format "docs/reports/gsd-wave-reviews/~a-w0.rktd" plan)))))
              (define live (load-campaign-record root plan))
              (define a (campaign-wave-current-attempt (car (campaign-record-waves live))))
              (set! attempts
                    (cons (cons (campaign-attempt-id a) (campaign-attempt-fence-token a)) attempts))
              (display-to-file
               (if (= implementations 1) "first acceptance\n" "first acceptance\nsecond acceptance\n")
               (build-path (current-directory) "acceptance")
               #:exists 'truncate)
              (git-quiet! (current-directory) "add" "acceptance")
              (git-quiet! (current-directory) "commit" "-qm" "implementation")
              (make-loop-result '() 'completed (hasheq))))))
       (define first-result (execute (request-for rec)))
       (check-equal? (campaign-result-status first-result) 'wave-blocked)
       (check-equal? implementations 1)
       (check-equal? reviews 1)
       (check-true crashed?)
       (check-false (hash-ref (load-delivery-journal root plan 0) 'review-rejection #f))
       ;; Resume with neither a marker nor local dispatch staging. The real
       ;; fetched artifact must reenter the ordinary retry without another r0
       ;; reviewer; the second reviewer belongs ONLY to the fresh r1 attempt.
       (delete-directory/files (review-dispatch-staging-path root plan 0 0))
       (define result (execute (request-for (load-campaign-record root plan))))
       (check-equal? (campaign-result-status result) 'wave-blocked)
       (check-equal? implementations 2 (campaign-result-message result))
       (check-equal? verifies 2)
       (check-equal? reviews 2)
       (check-equal? (reverse generations) '(0 1))
       (check-not-equal? (caar attempts) (caadr attempts))
       (check-true (> (cdar attempts) (cdadr attempts)))
       (define journal (load-delivery-journal root plan 0))
       (check-equal? (length (hash-ref journal 'receipt-history)) 1)
       (check-equal? (hash-ref journal 'review-rejection-count) 1))
     (lambda () (delete-directory/files root)))))
