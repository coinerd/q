#lang racket/base
;; Real owned-registry producer for temporary Git integration fixtures only.
;; Every command executed here is a REAL repository-spelled command (raco
;; fmt -i, raco make, the fixture's honest scripts/run-tests.rkt and
;; scripts/lint-all.rkt runners); there is no shimmed or fictional CLI.
(require json
         racket/file
         racket/list
         racket/path
         racket/string
         "../../extensions/gsd/review-proof.rkt"
         "../../extensions/gsd/verification-job.rkt"
         "../../extensions/gsd/composition-root.rkt"
         "../../extensions/gsd/campaign-state.rkt"
         "../../extensions/gsd/plan-snapshot.rkt"
         "../../extensions/gsd/delivery-verifier.rkt"
         "../../extensions/gsd/delivery-receipt.rkt"
         "../../scripts/gsd-binding-data.rkt")
(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (define action (first args))
  (define base (string->path (second args)))
  (define plan (third args))
  (define cwd (build-path base "q"))
  (define (ensure-snapshot!)
    (unless (load-snapshot-manifest base plan)
      (make-plan-snapshot! base plan (file->string (build-path base ".planning" "PLAN.md")))))
  (define (fixture-record)
    (make-campaign-record
     plan
     (make-campaign-manifest 1 "fixture" '() '() "constraints")
     (list (make-campaign-wave 1 "fixture" 'awaiting-delivery 1 (campaign-attempt "attempt-5" 2 0)))
     #f
     2
     'test
     0
     0))
  (define (live-guard rec)
    (lambda ()
      (and (not (campaign-record-cancellation rec))
           (not (campaign-record-budget-pause rec))
           (= (campaign-fence-token rec) 2)
           (for/or ([w (in-list (campaign-record-waves rec))])
             (define a (campaign-wave-current-attempt w))
             (and (= 1 (campaign-wave-index w))
                  a
                  (equal? (campaign-attempt-id a) "attempt-5")
                  (= (campaign-attempt-fence-token a) 2))))))
  (parameterize ([current-owned-proof-root (build-path base "proof-store")])
    (cond
      [(equal? action "red")
       ;; Authentic behavioral RED: a fresh registered execution of the
       ;; declared focused stage at the committed pre-fix head, retained
       ;; with its exact source identity, job facts and raw log.
       (ensure-snapshot!)
       (define rec (fixture-record))
       (define snapshot (committed-delivery-snapshot cwd))
       (define context
         (make-branch-delivery-context #:repo-root (hash-ref snapshot 'repo)
                                       #:branch (hash-ref snapshot 'branch)
                                       #:base-commit (hash-ref snapshot 'head)
                                       #:worktree-path cwd))
       (parameterize ([current-gsd-verification-registry (make-verification-registry)]
                      [current-gsd-delivery-verify-timeout-sec 60]
                      [current-gsd-delivery-branch-context context])
         (define-values (job red-ref)
           (run-owned-red-verification base
                                       plan
                                       1
                                       "attempt-5"
                                       2
                                       cwd
                                       "racket scripts/run-tests.rkt tests/w1-fixture.rkt"
                                       #:guard (live-guard rec)))
         (unless red-ref
           (error 'fixture
                  "actual owned fixture RED execution was not retained: state=~a exit=~a log=~a"
                  (verification-job-state job)
                  (verification-job-exit-code job)
                  (verification-job-log-path job)))
         (write-json red-ref)
         (newline))]
      [(equal? action "produce")
       (ensure-snapshot!)
       (define rec (fixture-record))
       (define snapshot (committed-delivery-snapshot cwd))
       (define context
         (make-branch-delivery-context #:repo-root (hash-ref snapshot 'repo)
                                       #:branch (hash-ref snapshot 'branch)
                                       #:base-commit (hash-ref snapshot 'head)
                                       #:worktree-path cwd))
       (parameterize ([current-gsd-verification-registry (make-verification-registry)]
                      [current-gsd-delivery-verify-timeout-sec 120])
         (define result
           (verify-campaign-delivery
            base
            plan
            1
            cwd
            context
            (lambda (_)
              (define check (check-verify-command base 1 #f))
              (delivery-verification (cadr check) (list check) "actual fixture owned Verify"))
            (lambda () rec)
            #:remote-published (lambda (_repo _branch _head) #t)))
         (unless (delivery-verification-approved? result)
           (error 'fixture "actual owned fixture execution failed: ~a" result))
         (displayln result))]
      [(equal? action "authority")
       (write-json (owned-review-authority base plan 1 (fourth args)))
       (newline)]
      [else (error 'fixture "unknown fixture action")])))
