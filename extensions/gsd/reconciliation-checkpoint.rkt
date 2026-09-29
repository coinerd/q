#lang racket/base
;; extensions/gsd/reconciliation-checkpoint.rkt — post-DONE tracker
;; reconciliation policy (v1.00.33 W0, BUG-0074 wiring).
;;
;;   tracker-reconciliation.rkt decides WHETHER a delivered wave may be
;;   reconciled. This module decides WHEN the pass is attempted, WHAT happens
;;   when it is not configured, and WHAT happens when it fails — and it makes
;;   the one guarantee the whole design rests on:
;;
;;     Tracker state is an OUTPUT of delivery, never an INPUT to it.
;;
;; Concretely, this checkpoint runs AFTER the durable wave is already `done`
;; and it can never move a wave out of `done`:
;;
;;   - no reconciler configured      -> 'not-configured, zero effects
;;   - reconciler refuses             -> 'blocked,        zero effects
;;   - reconciler raises              -> 'blocked,        zero effects (never
;;                                      an exception escapes into the campaign)
;;   - reconciler reconciles          -> 'reconciled,     its own effects
;;
;; A tracker failure is therefore bookkeeping trouble, never a wave outcome:
;; a genuinely delivered wave stays `done` and the refusal is reported, which
;; is the opposite of BUG-0074 (a delivered wave stranded open) and of
;; BUG-0077 (an undelivered wave recorded as delivered).
;;
;; The orchestrator is NOT wired here directly: `delivery-finalize.rkt`'s
;; run-delivery-finalization calls this module in its `done` branch, so the
;; post-DONE path is covered without growing go-orchestrator.rkt (which sits
;; 4 lines under its 1700-line architecture pin).

(require racket/base
         racket/format
         "delivery-handoff.rkt"
         "tracker-reconciliation.rkt")

(provide run-tracker-reconciliation!
         reconciliation-checkpoint-result
         reconciliation-checkpoint-result?
         reconciliation-checkpoint-result-status
         reconciliation-checkpoint-result-reason
         reconciliation-checkpoint-result-actions
         reconciler-status
         reconciliation-checkpoint-not-configured
         reconciliation-checkpoint-blocked
         reconciliation-checkpoint-reconciled
         reconciliation-blocked-status?)

;; ============================================================
;; Result
;; ============================================================

;; status   — one of 'not-configured | 'reconciled | 'blocked
;; reason   — string or #f (the refusal/blocked explanation)
;; actions  — the reconciler's command list, or '() when nothing ran
(struct reconciliation-checkpoint-result (status reason actions) #:transparent)

(define (reconciliation-checkpoint-not-configured)
  (reconciliation-checkpoint-result 'not-configured "no tracker reconciler configured" '()))

(define (reconciliation-checkpoint-blocked reason)
  (reconciliation-checkpoint-result 'blocked reason '()))

(define (reconciliation-checkpoint-reconciled merge-sha actions)
  (reconciliation-checkpoint-result 'reconciled
                                    #f
                                    (if (list? actions)
                                        actions
                                        '())))

;; Any status other than 'reconciled means the pass performed NO effects.
(define (reconciliation-blocked-status? result)
  (and (reconciliation-checkpoint-result? result)
       (not (eq? (reconciliation-checkpoint-result-status result) 'reconciled))))

;; ============================================================
;; Reconciler contract
;; ============================================================

;; A reconciler is any (or/c #f procedure) taking (base-dir plan-id wave-index)
;; and returning a tracker-reconciliation-result. It is injected, never
;; constructed here: this module deliberately has no GitHub credentials, no
;; adapter and no subprocess, so a tracker effect can only ever happen through
;; a caller that explicitly supplied a live port.
(define reconciler-status (lambda (r) (and (procedure? r) 'ok)))

;; ============================================================
;; The checkpoint
;; ============================================================

;; run-tracker-reconciliation! : base-dir plan-id wave-index reconciler
;;                             [#:delivery-reader reader] -> result
;;
;; PURE with respect to campaign state: this function never writes campaign
;; records, wave statuses, journals or handoffs. Its only possible effect is
;; the tracker action the injected reconciler performs, and only when that
;; reconciler's own fail-closed checks pass.
;;
;; Fails soft by design. An exception from the reconciler is converted to a
;; typed 'blocked result: a broken tracker must never abort a campaign whose
;; wave is already, provably, delivered.
(define (run-tracker-reconciliation! base-dir
                                     plan-id
                                     wave-index
                                     reconciler
                                     #:delivery-reader [delivery-reader delivery-readback])
  (cond
    [(not (string? plan-id)) (reconciliation-checkpoint-blocked "missing plan id")]
    [(not (exact-nonnegative-integer? wave-index))
     (reconciliation-checkpoint-blocked "missing wave index")]
    [(not (procedure? reconciler)) (reconciliation-checkpoint-not-configured)]
    [else
     (with-handlers ([exn:fail? (lambda (e)
                                  (reconciliation-checkpoint-blocked
                                   (format "tracker reconciler raised: ~a" (exn-message e))))])
       (define outcome (reconciler base-dir plan-id wave-index #:delivery-reader delivery-reader))
       (cond
         [(not (tracker-reconciliation-result? outcome))
          (reconciliation-checkpoint-blocked "tracker reconciler returned a non-result")]
         [else
          (define status (tracker-reconciliation-result-status outcome))
          (cond
            [(eq? status 'reconciled)
             (reconciliation-checkpoint-reconciled (tracker-reconciliation-result-merge-sha outcome)
                                                   (tracker-reconciliation-result-actions outcome))]
            [else
             (reconciliation-checkpoint-blocked (or (tracker-reconciliation-result-reason outcome)
                                                    "tracker reconciliation refused"))])]))]))
