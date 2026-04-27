#lang racket/base

;; extensions/gsd-planning-state.rkt — Thin shim over new gsd/ modules
;;
;; v0.21.6: steering.rkt import removed (dead code).
;; Only wave tracking
;; and mode delegation remain.

(require racket/set
         "gsd/state-machine.rkt")

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
;; State boxes (semaphore-protected)
;; ============================================================

(define state-sem (make-semaphore 1))

(define pinned-dir-box (box #f))
(define edit-limit-box (box 500))
(define completed-waves-box (box (set)))
(define total-waves-box (box 0))
(define current-wave-box (box 0))
(define gsd-event-bus-box (box #f))

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
;; Accessors (semaphore-protected)
;; ============================================================

(define (pinned-planning-dir)
  (call-with-semaphore state-sem (lambda () (unbox pinned-dir-box))))

(define (set-pinned-planning-dir! v)
  (call-with-semaphore state-sem (lambda () (set-box! pinned-dir-box v))))

(define (current-max-old-text-len)
  (call-with-semaphore state-sem (lambda () (unbox edit-limit-box))))

(define (set-current-max-old-text-len! v)
  (call-with-semaphore state-sem (lambda () (set-box! edit-limit-box v))))

;; ============================================================
;; Wave tracking
;; ============================================================

(define (completed-waves)
  (call-with-semaphore state-sem (lambda () (set-copy (unbox completed-waves-box)))))

(define (total-waves)
  (call-with-semaphore state-sem (lambda () (unbox total-waves-box))))

(define (set-total-waves! n)
  (call-with-semaphore state-sem (lambda () (set-box! total-waves-box n))))

(define (mark-wave-complete! idx)
  (call-with-semaphore state-sem
                       (lambda ()
                         (set-box! completed-waves-box (set-add (unbox completed-waves-box) idx)))))

(define (wave-complete? idx)
  (call-with-semaphore state-sem (lambda () (set-member? (unbox completed-waves-box) idx))))

(define (current-wave-index)
  (call-with-semaphore state-sem (lambda () (unbox current-wave-box))))

(define (set-current-wave-index! n)
  (call-with-semaphore state-sem (lambda () (set-box! current-wave-box n))))

(define (next-pending-wave)
  (call-with-semaphore state-sem
                       (lambda ()
                         (define tw (unbox total-waves-box))
                         (define cw (unbox completed-waves-box))
                         (for/first ([i (in-range tw)]
                                     #:when (not (set-member? cw i)))
                           i))))

;; ============================================================
;; Event bus
;; ============================================================

(define (gsd-event-bus)
  (call-with-semaphore state-sem (lambda () (unbox gsd-event-bus-box))))

(define (set-gsd-event-bus! v)
  (call-with-semaphore state-sem (lambda () (set-box! gsd-event-bus-box v))))

;; ============================================================
;; Observability + reset
;; ============================================================

(define (gsd-snapshot)
  (call-with-semaphore state-sem
                       (lambda ()
                         (hasheq 'mode
                                 (gsm-current)
                                 'pinned-dir
                                 (unbox pinned-dir-box)
                                 'edit-limit
                                 (unbox edit-limit-box)
                                 'completed-waves
                                 (set-copy (unbox completed-waves-box))
                                 'total-waves
                                 (unbox total-waves-box)
                                 'current-wave
                                 (unbox current-wave-box)))))

(define (reset-all-gsd-state!)
  (call-with-semaphore state-sem
                       (lambda ()
                         (gsm-reset!)
                         (set-box! pinned-dir-box #f)
                         (set-box! edit-limit-box 500)
                         (set-box! completed-waves-box (set))
                         (set-box! total-waves-box 0)
                         (set-box! current-wave-box 0)
                         (set-box! gsd-event-bus-box #f))))
