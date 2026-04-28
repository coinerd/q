#lang racket/base

;; extensions/gsd-planning-state.rkt — Thin shim over gsd/ modules
;;
;; v0.21.6: steering.rkt import removed (dead code).
;; v0.22.1 QUAL-03: Migrated from global mutable boxes to per-session
;; parameters via session-state.rkt.

(require "gsd/state-machine.rkt"
         "gsd/session-state.rkt"
         "gsd/wave-docs.rkt")

(provide gsd-mode
         gsd-mode?
         set-gsd-mode!
         pinned-planning-dir
         set-pinned-planning-dir!
         current-max-old-text-len
         set-current-max-old-text-len!
         ;; Wave tracking
         completed-waves
         total-waves
         set-total-waves!
         mark-wave-complete!
         wave-complete?
         next-pending-wave
         current-wave-index
         set-current-wave-index!
         ;; Observability
         gsd-snapshot
         reset-all-gsd-state!
         ;; Event bus
         gsd-event-bus
         set-gsd-event-bus!)

;; ============================================================
;; Mode delegation to state machine
;; ============================================================

(define (legacy-map-new->old s)
  (cond
    [(eq? s 'idle) #f]
    [(eq? s 'exploring) 'planning]
    [else s]))

(define (gsd-mode)
  (legacy-map-new->old (gsm-current)))

(define (gsd-mode? v)
  (eq? (gsd-mode) v))

(define (set-gsd-mode! v)
  (cond
    [(not v) (gsm-reset!)]
    [(eq? v 'planning) (gsm-transition! 'exploring)]
    [(eq? v 'plan-written)
     (when (eq? (gsm-current) 'idle)
       (gsm-transition! 'exploring))
     (gsm-transition! 'plan-written)]
    [(eq? v 'executing)
     (when (eq? (gsm-current) 'exploring)
       (gsm-transition! 'plan-written))
     (when (eq? (gsm-current) 'idle)
       (gsm-transition! 'exploring)
       (gsm-transition! 'plan-written))
     (gsm-transition! 'executing)]
    [else (gsm-transition! v)]))

;; ============================================================
;; Accessors — now backed by session parameters
;; ============================================================

(define (pinned-planning-dir)
  (current-pinned-dir))

(define (set-pinned-planning-dir! v)
  (current-pinned-dir v))

(define (current-max-old-text-len)
  (current-edit-limit))

(define (set-current-max-old-text-len! v)
  (current-edit-limit v))

;; ============================================================
;; Wave tracking (delegated to state machine — F4 consolidation)
;; ============================================================

(define (completed-waves)
  (gsm-completed-waves))
(define (total-waves)
  (gsm-total-waves))
(define (set-total-waves! n)
  (gsm-set-total-waves! n))
(define (mark-wave-complete! idx)
  (gsm-mark-wave-complete! idx)
  ;; Also update PLAN.md status marker (#2157)
  (define base-dir (pinned-planning-dir))
  (when base-dir
    (with-handlers ([exn:fail? (lambda (e) (void))])
      (mark-wave-status! base-dir idx "DONE"))))
(define (wave-complete? idx)
  (gsm-wave-complete? idx))
(define (current-wave-index)
  (gsm-current-wave))
(define (set-current-wave-index! n)
  (gsm-set-current-wave! n))
(define (next-pending-wave)
  (gsm-next-pending-wave))

;; ============================================================
;; Event bus — now backed by session parameter
;; ============================================================

(define (gsd-event-bus)
  (current-gsd-event-bus))

(define (set-gsd-event-bus! v)
  (current-gsd-event-bus v))

;; ============================================================
;; Observability + reset — now backed by session parameters
;; ============================================================

(define (gsd-snapshot)
  (hasheq 'mode
          (gsm-current)
          'pinned-dir
          (current-pinned-dir)
          'edit-limit
          (current-edit-limit)
          'total-waves
          (gsm-total-waves)
          'current-wave
          (gsm-current-wave)
          'completed-waves
          (gsm-completed-waves)
          'wave-executor
          (and (gsm-wave-executor) #t)))

(define (reset-all-gsd-state!)
  (reset-gsm!)
  (current-pinned-dir #f)
  (current-edit-limit 500)
  (current-gsd-event-bus #f)
  (current-plan-data #f))
