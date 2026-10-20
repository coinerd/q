#lang racket/base
;; owned-lane.rkt — construction of the owned-phase runner port.
;;
;; Extracted from go-orchestrator.rkt (BUG-0042 maintenance contract:
;; extract a module instead of growing the orchestrator). This module
;; owns the owned-red-v1 / evidence-author execution lane's port wiring:
;;   - the live guard (fresh record, same attempt/fence, no cancellation,
;;     no budget pause) checked around every phase dispatch;
;;   - STRICT between-phase usage accounting (a stamp or ceiling-check
;;     failure STOPS the next model dispatch rather than silently
;;     allowing it; unknown cost coverage is preserved, never zeroed);
;;   - a DEDICATED verification registry isolating this runner's
;;     structured RED, declared RED and candidate jobs from unrelated
;;     ambient verification, with atomic close-before-cancel admission
;;     so a launch racing the timeout can never orphan a subprocess.
;; The orchestrator keeps only the lane decision and the thin call site.
(require (only-in "campaign-result.rkt" campaign-result)
         "wave-runner-port.rkt"
         "campaign-state.rkt"
         (only-in "campaign-repository.rkt" load-campaign-record)
         (only-in "campaign-budgets.rkt"
                  current-campaign-usage-observation
                  stamp-observed-usage!
                  pause-campaign-if-over-budget!)
         (only-in "delivery-receipt.rkt" current-wave-for-attempt)
         (only-in "author-execution.rkt" run-proactive-author-phases! run-retained-author-phases!)
         (only-in "author-checkpoint.rkt" author-repair-pending? author-repair-running?)
         (only-in "supplemental-evidence-controller.rkt"
                  run-supplemental-author-phases!
                  supplemental-author-pending?
                  supplemental-entry-outcome)
         (only-in "composition-root.rkt" current-gsd-verification-registry)
         (only-in "verification-job.rkt" make-verification-registry verification-close-registry!))
(provide make-owned-runner-port
         owned-lane?
         owned-entry-block)

;; Lane decision: a pending evidence-author permission or the fresh
;; owned-red-v1 execution policy selects the owned phase lane.
(define (owned-lane? author-pending? active)
  (or author-pending? (eq? (campaign-record-execution-policy active) 'owned-red-v1)))

;; Honest stop when the author repair already launched: ambiguous or lost
;; model responses are never redispatched. A supplemental operation past its
;; launch boundary owns the same guarantee: the entry resumes its protected
;; flow WITHOUT beginning a new attempt (never re-reserves) and returns the
;; typed controller outcome instead of a dead-end block.
(define (owned-entry-block base-dir active wave-idx)
  (or (and (author-repair-running? base-dir (campaign-plan-id active) wave-idx)
           (campaign-result
            'wave-blocked
            '()
            "author repair already launched; ambiguous/lost model response is not redispatched"))
      (supplemental-entry-outcome base-dir (campaign-plan-id active) wave-idx)))

;; observe : -> fresh campaign record (or #f). base-dir/plan-id/wave-idx
;; identify the campaign; expected-id/fence the live attempt. The returned
;; port dispatches the retained-author or proactive phase runner with the
;; strict guards above; success, exception and cancellation all close the
;; dedicated registry.
(define (make-owned-runner-port base-dir
                                plan-id
                                wave-idx
                                expected-id
                                fence
                                observe
                                source-runner-port)
  (define (phase-live?)
    (define now (observe))
    (and (current-wave-for-attempt now wave-idx fence expected-id)
         (not (campaign-record-cancellation now))
         (not (campaign-record-budget-pause now))))
  (define (account-phase!)
    (define observation (current-campaign-usage-observation))
    (stamp-observed-usage! base-dir
                           plan-id
                           wave-idx
                           (and (box? observation) (unbox observation))
                           #:strict? #t)
    (pause-campaign-if-over-budget! base-dir plan-id #:strict? #t))
  (define phase-registry (make-verification-registry))
  (define (close-phase-jobs!)
    (verification-close-registry! phase-registry))
  (make-wave-runner-port
   (lambda (idx)
     (dynamic-wind
      void
      (lambda ()
        (parameterize ([current-gsd-verification-registry phase-registry])
          ((cond
             [(supplemental-author-pending? base-dir plan-id idx) run-supplemental-author-phases!]
             [(author-repair-pending? base-dir plan-id idx) run-retained-author-phases!]
             [else run-proactive-author-phases!])
           base-dir
           plan-id
           idx
           expected-id
           fence
           (lambda () ((gsd-wave-runner-port-run source-runner-port) idx))
           #:guard phase-live?
           #:after-phase account-phase!)))
      close-phase-jobs!))
   #:cancel! (lambda ()
               (close-phase-jobs!)
               ((gsd-wave-runner-port-cancel! source-runner-port)))
   #:cancel-requested?
   (lambda ()
     (or (not (phase-live?)) ((gsd-wave-runner-port-cancel-requested? source-runner-port))))))
