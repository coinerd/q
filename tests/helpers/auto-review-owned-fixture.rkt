#lang racket/base
;; Real owned-registry producer for the automatic-review integration
;; fixtures ONLY (tests/test-gsd-auto-review-production.rkt). Every command
;; executed through this helper is the campaign's REAL frozen declared
;; Verify chain, run as a REAL registered job with a live attempt-bound
;; campaign record, so verify-with-delivery-receipt captures and retains a
;; genuine owned proof under the DEFAULT owned-proof root (the caller
;; controls the subprocess HOME; there is no command override and no
;; shimmed CLI anywhere in this helper).
(require racket/file
         racket/format
         racket/list
         racket/path
         racket/string
         "../../extensions/gsd/review-proof.rkt"
         "../../extensions/gsd/verification-job.rkt"
         "../../extensions/gsd/composition-root.rkt"
         "../../extensions/gsd/campaign-state.rkt"
         "../../extensions/gsd/plan-snapshot.rkt"
         "../../extensions/gsd/delivery-verifier.rkt"
         "../../extensions/gsd/delivery-receipt.rkt")
(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (unless (and (= 5 (length args)) (equal? (first args) "verify"))
    (error 'fixture "usage: verify <campaign-root> <plan> <wave> <verify-cwd>"))
  (define base (string->path (second args)))
  (define plan (third args))
  (define wave (string->number (fourth args)))
  (define cwd (string->path (fifth args)))
  ;; The immutable plan snapshot is created through the REAL module (full
  ;; per-file hashing); a hand-authored manifest is never fabricated here.
  (unless (load-snapshot-manifest base plan)
    (make-plan-snapshot! base plan (file->string (build-path base ".planning" "PLAN.md"))))
  (define rec
    (make-campaign-record
     plan
     (make-campaign-manifest 1 "fixture" '() '() "constraints")
     (list (make-campaign-wave wave "fixture" 'awaiting-delivery 1 (campaign-attempt "attempt-5" 2 0)))
     #f
     2
     'test
     0
     0))
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
       wave
       cwd
       context
       (lambda (_)
         (define check (check-verify-command base wave #f))
         (delivery-verification (cadr check) (list check) "actual fixture owned Verify"))
       (lambda () rec)))
    (unless (delivery-verification-approved? result)
      (error 'fixture "actual owned fixture execution failed: ~a" result))
    (displayln result)))
