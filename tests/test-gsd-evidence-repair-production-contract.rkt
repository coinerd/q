#lang racket
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @timeout 210
;; One real verifier/controller/audit chain; leave the second campaign to full.
(require rackunit
         json
         "helpers/delivery-fixtures.rkt"
         "helpers/private-fixture-templates.rkt"
         "helpers/supplemental-evidence-fixture.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/composition-root.rkt"
         "../extensions/gsd/verification-job.rkt"
         "../extensions/gsd/delivery-verifier.rkt"
         "../extensions/gsd/delivery-receipt.rkt"
         "../extensions/gsd/plan-snapshot.rkt"
         "../extensions/gsd/review-proof.rkt"
         "../extensions/gsd/evidence-readiness.rkt"
         "../extensions/gsd/evidence-repair-controller.rkt"
         "../extensions/gsd/evidence-repair-ledger.rkt"
         "../extensions/gsd/delivery-coordinator.rkt"
         "../extensions/gsd/review-dispatch.rkt"
         "../scripts/gsd-binding-data.rkt"
         "../scripts/gsd-gates/owned-audit.rkt"
         "../extensions/gsd/review-audit-format.rkt")

(module+ test
  (test-case "repair publishes once with fresh audited authority and leaves frozen GREEN unchanged"
    (define root (make-temporary-file "b4-repair-~a" 'directory))
    (define repo (build-path root "q"))
    (define manifest (make-campaign-manifest 1 "b4" '() '() (path->string root)))
    (define plan (campaign-manifest-hash manifest))
    (define command
      (string-append "raco fmt -i source.rkt && raco make source.rkt && "
                     "racket scripts/run-tests.rkt tests/probe.rkt && "
                     "racket scripts/run-tests.rkt --suite fast && racket scripts/lint-all.rkt"))
    (dynamic-wind
     void
     (lambda ()
       (parameterize ([current-gsd-verification-registry (make-verification-registry)]
                      [current-gsd-delivery-verify-timeout-sec 60])
         (make-directory repo)
         (define (git . args)
           (apply fixture-git repo args))
         (define (put path text)
           (fixture-put repo path text))
         (git "init" "-qb" "campaign/w2")
         (hermetic-identity! repo)
         (define remote (path->string (build-path root "remote.git")))
         (define origin "https://github.com/example/q.git")
         (git "init" "--bare" "-q" remote)
         (git "config" (string-append "url." remote ".insteadOf") origin)
         (git "remote" "add" "origin" origin)
         (put ".gitignore" "compiled/\n.planning/\n")
         (put "source.rkt" "#lang racket/base\n(provide answer)\n(define answer 0)\n")
         (put "scripts/required-pr-checks.policy" "(\"lint\")\n")
         (git "add" ".")
         (git "commit" "-qm" "baseline")
         (git "push" "-q" "origin" "HEAD:refs/heads/main")
         (put "source.rkt" "#lang racket/base\n(provide answer)\n(define answer 1)\n")
         (put
          "tests/probe.rkt"
          (string-append
           "#lang racket/base\n(require rackunit \"../source.rkt\")\n(provide evidence-suite)\n"
           "(define evidence-suite (test-suite \"suite\" (test-case \"target\" (check-equal? answer 1))))\n"
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
         (git "add" ".")
         (git "commit" "-qm" "candidate")
         (git "push" "-q" "origin" "campaign/w2")
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
              (delivery-verification (cadr check) (list check) "miniature owned chain"))
            (lambda () rec)
            #:remote-published (lambda args #t)))
         (check-true (delivery-verification-approved? verified))
         (define before (current-evidence-context root plan 2))
         (check-equal? (hash-ref (resume-evidence-repair! root plan 2 #:guard (lambda () #f)) 'code)
                       "stale-live-guard")
         (check-equal? (hash-ref (current-evidence-context root plan 2) 'evidence-revision) 0)
         (define calls 0)
         (define (review request)
           (set! calls (add1 calls))
           (define authority (hash-ref request 'owned-authority))
           (define staging (hash-ref request 'staging))
           (check-true (hash-ref request 'fresh-session-required))
           (check-true (hash-ref request 'non-author-review-required))
           (check-equal? (hash-ref request 'head) (hash-ref before 'head))
           (check-equal? (hash-ref request 'evidence-revision) 1)
           (check-equal? (hash-ref authority 'evidence-context)
                         (hash-ref (evidence-readiness root plan 2) 'context))
           (define clone (build-path root "clone"))
           (git "clone" "--quiet" "--no-local" (path->string repo) (path->string clone))
           (copy-directory/files (build-path staging "docs") (build-path clone "docs"))
           (check-equal? (hash-ref (hash-ref (validate-owned-authority clone authority) 'red-facts)
                                   'origin)
                         "posthoc-reproduction")
           (define exported (owned-review-authority root plan 2 staging))
           (call-with-output-file (build-path staging (hash-ref exported 'bundle) "authority.json")
                                  (lambda (out) (write-json authority out))
                                  #:exists 'truncate)
           (check-equal? (owned-review-authority root plan 2 staging) exported)
           (hasheq
            'kind
            "approved"
            'reviewer
            "independent-b4"
            'scope
            "Reviewed the source, baseline counterexample and complete owned verification chain."
            'report
            "The candidate returns one; the ancestor returns zero and fails the same focused assertion."
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
                    (audit-binding request)
                    'red-ack
                    (hasheq 'proof-sha256
                            (hash-ref (hash-ref (hash-ref authority 'red) 'reference) 'sha256)
                            'raw-output-read
                            #t))))
         (define outcome
           (parameterize ([current-gsd-auto-review-runner review])
             (default-delivery-controller root plan 2 "implementation-review")))
         (check-equal? calls 1 (format "~s" outcome))
         (check-equal? (delivery-effect-result-kind outcome) 'ok (format "~s" outcome))
         (check-equal? (hash-ref (delivery-effect-result-data outcome) 'status) "reviewed")
         (check-equal? (current-evidence-context root plan 2) before)
         (check-equal? (hash-ref (evidence-readiness root plan 2) 'evidence-revision) 1)
         (define ledger (load-evidence-repair root plan 2))
         (check-equal? (length (hash-ref ledger 'registrations)) 1)
         (check-equal? (hash-ref (hash-ref ledger 'operation) 'state) "registered")))
     (lambda ()
       ;; The audit process resolves the default store independently. Isolate
       ;; this fixture by its unique campaign key and remove only its proofs.
       (define proof-dir (build-path (current-owned-proof-root) plan))
       (when (directory-exists? proof-dir)
         (delete-directory/files proof-dir))
       (delete-directory/files root)))))
