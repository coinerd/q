#lang racket/base
;; Retry the receipt-authoritative tracker output for previously delivered
;; waves on /go resume. This closes the crash window between durable DONE and
;; the in-memory post-DONE checkpoint. A retry reauthenticates delivery in
;; tracker-reconciliation.rkt before any GitHub effect. Status/close updates
;; are externally idempotent; this module NEVER treats tracker state as proof.
(require "campaign-state.rkt"
         (only-in "campaign-repository.rkt" load-campaign-record)
         "delivery-handoff.rkt"
         "reconciliation-checkpoint.rkt"
         "tracker-production-wiring.rkt"
         "tracker-reconciliation.rkt")
(provide resume-tracker-reconciliation!)

;; The config is explicitly bound to ONE plan and wave. A historical DONE wave
;; without this opt-in is invisible to this sweep; it causes no network read,
;; no journal change and no tracker write.
(define (resume-tracker-reconciliation! base-dir
                                        rec
                                        #:delivery-reader [reader delivery-readback]
                                        #:target? [target? live-tracker-target?]
                                        #:resolver [resolver resolve-live-tracker-reconciler])
  (define plan-id (campaign-plan-id rec))
  ;; Fresh durable readback, not the mutable record supplied by a caller.
  (define fresh
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (load-campaign-record base-dir plan-id)))
  (if (not fresh)
      '()
      (for/list ([wave (in-list (campaign-record-waves fresh))]
                 #:when (and (eq? (campaign-wave-status wave) 'done)
                             (target? base-dir plan-id (campaign-wave-index wave))))
        (define idx (campaign-wave-index wave))
        (define result
          (run-tracker-reconciliation! base-dir
                                       plan-id
                                       idx
                                       (resolver base-dir plan-id idx)
                                       #:delivery-reader reader))
        (unless (eq? (reconciliation-checkpoint-result-status result) 'reconciled)
          (log-warning
           "tracker resume ~a for plan ~a wave ~a (~a completed action(s)); delivered wave remains DONE"
           (reconciliation-checkpoint-result-status result)
           plan-id
           idx
           (length (reconciliation-checkpoint-result-actions result))))
        result)))
