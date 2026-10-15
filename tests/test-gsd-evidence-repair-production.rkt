#lang racket
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @timeout 900
;; Two sequential real local-Git/owned-job/controller/publication fixtures.
;; This wrapper allowance does not change the production review's 600s cap.
;; Genuine frozen GREEN -> production repair -> fresh review authority.
(require rackunit
         json
         "helpers/delivery-fixtures.rkt"
         "helpers/private-fixture-templates.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/composition-root.rkt"
         "../extensions/gsd/verification-job.rkt"
         "../extensions/gsd/delivery-verifier.rkt"
         "../extensions/gsd/delivery-receipt.rkt"
         "../extensions/gsd/plan-snapshot.rkt"
         "../extensions/gsd/review-proof.rkt"
         "../extensions/gsd/evidence-readiness.rkt"
         "../extensions/gsd/evidence-probe.rkt"
         "../extensions/gsd/evidence-repair-controller.rkt"
         "../extensions/gsd/evidence-repair-ledger.rkt"
         "../extensions/gsd/delivery-coordinator.rkt"
         "../extensions/gsd/review-dispatch.rkt"
         "../scripts/gsd-binding-data.rkt"
         "../scripts/gsd-gates/owned-audit.rkt"
         "../extensions/gsd/review-audit-format.rkt")
(module+ test
  ;; Sequential, distinct execution repositories catch process-global authority
  ;; closures; workspace paths deliberately differ from context repo strings.
  (for ([campaign '("first" "second")])
    (test-case (string-append "production repair and independent review: " campaign)
      (define root (make-temporary-file "repair-production-~a" 'directory))
      (define repo (build-path root "q"))
      (define manifest (make-campaign-manifest 1 campaign '() '() (path->string root)))
      (define plan (campaign-manifest-hash manifest))
      (define command
        (string-append "raco fmt -i source.rkt && raco make source.rkt && "
                       "racket scripts/run-tests.rkt tests/probe.rkt && "
                       "racket scripts/run-tests.rkt --suite fast && racket scripts/lint-all.rkt"))
      (dynamic-wind
       void
       (lambda ()
         (make-directory repo)
         (git-quiet! repo "init" "-q")
         (hermetic-identity! repo)
         (git-quiet! repo "checkout" "-qb" "campaign/w2")
         (define remote (build-path root "remote.git"))
         (git-quiet! repo "init" "--bare" "-q" (path->string remote))
         (git-quiet! repo
                     "config"
                     (string-append "url." (path->string remote) ".insteadOf")
                     "https://github.com/example/q.git")
         (git-quiet! repo "remote" "add" "origin" "https://github.com/example/q.git")
         (define (put path text)
           (make-directory* (path-only (build-path repo path)))
           (display-to-file text (build-path repo path) #:exists 'truncate))
         (put ".gitignore" "compiled/\n.planning/\nproofs/\npublication/\nclone/\n")
         (put "source.rkt" "#lang racket/base\n(provide answer)\n(define (answer) 0)\n")
         (put "scripts/required-pr-checks.policy" "(\"lint\")\n")
         (git-quiet! repo "add" "source.rkt" ".gitignore" "scripts/required-pr-checks.policy")
         (git-quiet! repo "commit" "-qm" "baseline")
         (git-quiet! repo "push" "-q" "origin" "HEAD:refs/heads/main")
         (put "source.rkt" "#lang racket/base\n(provide answer)\n(define (answer) 1)\n")
         (put
          "tests/probe.rkt"
          (string-append
           "#lang racket/base\n(require rackunit \"../source.rkt\")\n(provide evidence-suite)\n"
           "(define evidence-suite (test-suite \"suite\" (test-case \"target\" (check-equal? (answer) 1))))\n"
           "(module+ test (require rackunit/text-ui) (exit (run-tests evidence-suite)))\n"))
         (put
          "scripts/run-tests.rkt"
          (string-append
           "#lang racket/base\n(require racket/runtime-path)\n"
           "(define-runtime-path test \"../tests/probe.rkt\")\n(dynamic-require `(submod ,test test) #f)\n"))
         (put
          "scripts/lint-all.rkt"
          (string-append
           "#lang racket/base\n(require syntax/modread)\n"
           "(for ([p '(\"source.rkt\" \"tests/probe.rkt\")]) (call-with-input-file p "
           "(lambda (in) (with-module-reading-parameterization (lambda () (read-syntax p in))))))\n"))
         (parameterize ([current-directory repo])
           (check-true (system* (find-executable-path "raco") "fmt" "-i" "source.rkt")))
         (git-quiet! repo
                     "add"
                     "source.rkt"
                     "tests/probe.rkt"
                     "scripts/run-tests.rkt"
                     "scripts/lint-all.rkt")
         (git-quiet! repo "commit" "-qm" "candidate")
         (git-quiet! repo "push" "-q" "origin" "campaign/w2")
         (make-directory* (build-path root ".planning" "waves"))
         (write-plan! root 2 "Proof" "proof")
         (write-wave-doc! root 2 "proof" '("q/source.rkt") command)
         (define wave-path (build-path root ".planning" "waves" "W2-proof.md"))
         (display-to-file
          (string-replace
           (file->string wave-path)
           "## Verify"
           "Branch: `campaign/w2`; GitHub issue https://github.com/example/q/issues/42, milestone #1\n\n## Verify")
          wave-path
          #:exists 'truncate)
         (make-plan-snapshot! root plan (file->string (build-path root ".planning" "PLAN.md")))
         (define rec
           (make-campaign-record
            plan
            manifest
            (list
             (make-campaign-wave 2 "proof" 'awaiting-delivery 1 (campaign-attempt "attempt-1" 7 0)))
            #f
            7
            'test
            0
            0))
         (persist-campaign! root rec)
         (parameterize ([current-gsd-verification-registry (make-verification-registry)]
                        [current-gsd-delivery-verify-timeout-sec 60])
           (define snapshot (committed-delivery-snapshot repo))
           (define verified
             (verify-campaign-delivery
              root
              plan
              2
              repo
              (make-branch-delivery-context #:repo-root (hash-ref snapshot 'repo)
                                            #:branch (hash-ref snapshot 'branch)
                                            #:base-commit (hash-ref snapshot 'head)
                                            #:worktree-path repo)
              (lambda (_)
                (define check (check-verify-command root 2 #f))
                (delivery-verification (cadr check) (list check) "actual miniature chain"))
              (lambda () rec)
              #:remote-published (lambda args #t)))
           (check-true (delivery-verification-approved? verified))
           (define before (current-evidence-context root plan 2))
           (check-true (string? (hash-ref before 'repo)))
           (check-equal? (hash-ref before 'evidence-revision) 0)
           ;; A stale guard must not reserve or publish repair evidence.
           (check-equal? (hash-ref (resume-evidence-repair! root plan 2 #:guard (lambda () #f)) 'code)
                         "stale-live-guard")
           (check-equal? (hash-ref (current-evidence-context root plan 2) 'evidence-revision) 0)
           (define calls 0)
           (define (runner request)
             (set! calls (add1 calls))
             (check-equal? (hash-ref request 'head) (hash-ref before 'head))
             (check-equal? (hash-ref (hash-ref request 'owned-authority) 'evidence-context)
                           (hash-ref (evidence-readiness root plan 2) 'context))
             (check-equal? (hash-ref request 'evidence-revision) 1)
             (check-true (hash-ref request 'fresh-session-required))
             (check-true (hash-ref request 'non-author-review-required))
             (define authority (hash-ref request 'owned-authority))
             (define clone (build-path root "clone"))
             (git-quiet! repo "clone" "--quiet" "--no-local" (path->string repo) (path->string clone))
             (copy-directory/files (build-path (hash-ref request 'staging) "docs")
                                   (build-path clone "docs"))
             (define audited (validate-owned-authority clone authority))
             (check-equal? (hash-ref (hash-ref audited 'red-facts) 'origin) "posthoc-reproduction")
             ;; Exercise the same repeat export used by production finalize.
             (define exported (owned-review-authority root plan 2 (hash-ref request 'staging)))
             ;; Publication writes this derived envelope before its last guard.
             ;; It must never become a new input to the authority it contains.
             (call-with-output-file
              (build-path (hash-ref request 'staging) (hash-ref exported 'bundle) "authority.json")
              (lambda (out) (write-json authority out))
              #:exists 'truncate)
             (check-equal? (owned-review-authority root plan 2 (hash-ref request 'staging)) exported)
             (define binding (audit-binding request))
             (hasheq
              'kind
              "approved"
              'reviewer
              (string-append "independent-" campaign)
              'scope
              "Reviewed the miniature source, the baseline counterexample and the complete owned verification chain."
              'report
              (string-append
               "The implementation returns one; the real baseline returns zero and fails "
               "the same focused assertion. All owned GREEN gates passed.")
              'audit-ack
              (hasheq 'mode
                      owned-audit-mode
                      'verification-origin
                      "audited-owned"
                      'source-read
                      #t
                      'raw-output-read
                      #t
                      'binding
                      binding
                      'red-ack
                      (hasheq 'proof-sha256
                              (hash-ref (hash-ref (hash-ref authority 'red) 'reference) 'sha256)
                              'raw-output-read
                              #t))))
           (define outcome
             (parameterize ([current-gsd-auto-review-runner runner])
               (default-delivery-controller root plan 2 "implementation-review")))
           (check-equal? calls 1 (format "~s" outcome))
           ;; The direct controller's successful advancement is 'ok; the
           ;; coordinator (not called here) owns journal stage transitions.
           (check-equal? (delivery-effect-result-kind outcome) 'ok (format "~s" outcome))
           (check-equal? (hash-ref (delivery-effect-result-data outcome) 'status) "reviewed")
           (define after (current-evidence-context root plan 2))
           (check-equal? (hash-ref after 'evidence-revision) 0)
           (check-equal? after before)
           (check-equal? (hash-ref (evidence-readiness root plan 2) 'evidence-revision) 1)
           (check-equal? (hash-ref after 'green-proof-sha256) (hash-ref before 'green-proof-sha256))
           (define ledger (load-evidence-repair root plan 2))
           (check-equal? (length (hash-ref ledger 'registrations)) 1)
           (check-equal? (hash-ref (hash-ref ledger 'operation) 'state) "registered")))
       (lambda ()
         (define proof-dir (build-path (current-owned-proof-root) plan))
         (when (directory-exists? proof-dir)
           (delete-directory/files proof-dir))
         (delete-directory/files root))))))
