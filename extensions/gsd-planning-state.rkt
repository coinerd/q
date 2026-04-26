#lang racket/base

;; extensions/gsd-planning-state.rkt — Thin shim over new gsd/ modules
;;
;; Wave 4a of v0.21.0: Backward-compatible re-export layer.
;; All state management now lives in extensions/gsd/state-machine.rkt.
;; This module provides the OLD public API by delegating to the new modules.
;;
;; Migration map:
;;   gsd-mode              → gsm-current (from state-machine)
;;   set-gsd-mode!         → gsm-transition! (from state-machine)
;;   gsd-mode?             → (eq? (gsm-current) v)
;;   reset-all-gsd-state!  → gsm-reset! + reset-steering-state!
;;   gsd-snapshot          → gsm-snapshot + budget/counts from here
;;
;; Budget-related functions are kept here for now (DD-2 removes budgets
;; in a later pass). Wave tracking delegates to state-machine transitions.

(require racket/set
         "gsd/state-machine.rkt"
         "gsd/steering.rkt")

(provide gsd-mode
         gsd-mode?
         set-gsd-mode!
         pinned-planning-dir
         set-pinned-planning-dir!
         go-read-budget
         set-go-read-budget!
         decrement-budget!
         reset-go-budget!
         GO-READ-BUDGET
         GO-READ-WARN-THRESHOLD
         GO-READ-BLOCK-THRESHOLD
         read-counts
         get-read-count
         increment-read-count!
         clear-read-counts!
         current-max-old-text-len
         set-current-max-old-text-len!
         ;; Wave tracking
         completed-waves
         total-waves
         set-total-waves!
         mark-wave-complete!
         wave-complete?
         next-pending-wave
         ;; Plan tool budget
         plan-tool-budget
         decrement-plan-budget!
         reset-plan-budget!
         EXPLORATION-BUDGET
         ;; Observability
         gsd-snapshot
         reset-all-gsd-state!
         READ-ONLY-TOOLS
         ;; Event bus
         gsd-event-bus
         set-gsd-event-bus!)

;; ============================================================
;; Legacy state: kept for budget/count features not yet migrated
;; ============================================================

(define state-sem (make-semaphore 1))

(define pinned-dir-box (box #f))
(define budget-box (box #f))
(define edit-limit-box (box 500))
(define read-counts-box (box (make-hash)))
(define completed-waves-box (box (set)))
(define total-waves-box (box 0))
(define EXPLORATION-BUDGET 30)
(define plan-tool-budget-box (box #f))
(define gsd-event-bus-box (box #f))

(define GO-READ-BUDGET 30)
(define GO-READ-WARN-THRESHOLD 5)
(define GO-READ-BLOCK-THRESHOLD -3)

(define READ-ONLY-TOOLS '("read" "grep" "find" "ls" "glob"))

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
     ;; May need to go through exploring first
     (when (eq? (gsm-current) 'idle)
       (gsm-transition! 'exploring))
     (gsm-transition! 'plan-written)]
    [(eq? v 'executing)
     ;; Legacy allows direct planning→executing; state machine requires plan-written intermediate
     (when (eq? (gsm-current) 'exploring)
       (gsm-transition! 'plan-written))
     (when (eq? (gsm-current) 'idle)
       (gsm-transition! 'exploring)
       (gsm-transition! 'plan-written))
     (gsm-transition! 'executing)]
    [else (gsm-transition! v)]))

;; ============================================================
;; Legacy accessors (semaphore-protected)
;; ============================================================

(define (pinned-planning-dir)
  (call-with-semaphore state-sem (lambda () (unbox pinned-dir-box))))

(define (set-pinned-planning-dir! v)
  (call-with-semaphore state-sem (lambda () (set-box! pinned-dir-box v))))

(define (go-read-budget)
  (call-with-semaphore state-sem (lambda () (unbox budget-box))))

(define (set-go-read-budget! v)
  (call-with-semaphore state-sem (lambda () (set-box! budget-box v))))

(define (decrement-budget!)
  (call-with-semaphore state-sem
                       (lambda ()
                         (define new-val (sub1 (unbox budget-box)))
                         (set-box! budget-box new-val)
                         new-val)))

(define (reset-go-budget!)
  (call-with-semaphore state-sem (lambda () (set-box! budget-box GO-READ-BUDGET))))

(define (read-counts)
  (call-with-semaphore state-sem (lambda () (hash-copy (unbox read-counts-box)))))

(define (get-read-count key)
  (call-with-semaphore state-sem (lambda () (hash-ref (unbox read-counts-box) key 0))))

(define (increment-read-count! key)
  (call-with-semaphore state-sem
                       (lambda ()
                         (define h (unbox read-counts-box))
                         (define new-count (add1 (hash-ref h key 0)))
                         (hash-set! h key new-count)
                         new-count)))

(define (clear-read-counts!)
  (call-with-semaphore state-sem (lambda () (set-box! read-counts-box (make-hash)))))

(define (current-max-old-text-len)
  (call-with-semaphore state-sem (lambda () (unbox edit-limit-box))))

(define (set-current-max-old-text-len! v)
  (call-with-semaphore state-sem (lambda () (set-box! edit-limit-box v))))

;; ============================================================
;; Wave tracking (legacy)
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

(define (next-pending-wave)
  (call-with-semaphore state-sem
                       (lambda ()
                         (define tw (unbox total-waves-box))
                         (define cw (unbox completed-waves-box))
                         (for/first ([i (in-range tw)]
                                     #:when (not (set-member? cw i)))
                           i))))

;; ============================================================
;; Plan tool budget
;; ============================================================

(define (plan-tool-budget)
  (call-with-semaphore state-sem (lambda () (unbox plan-tool-budget-box))))

(define (decrement-plan-budget!)
  (call-with-semaphore state-sem
                       (lambda ()
                         (define cur (unbox plan-tool-budget-box))
                         (when cur
                           (set-box! plan-tool-budget-box (sub1 cur)))
                         (unbox plan-tool-budget-box))))

(define (reset-plan-budget!)
  (call-with-semaphore state-sem (lambda () (set-box! plan-tool-budget-box EXPLORATION-BUDGET))))

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
                                 'go-read-budget
                                 (unbox budget-box)
                                 'edit-limit
                                 (unbox edit-limit-box)
                                 'read-counts
                                 (hash-copy (unbox read-counts-box))
                                 'completed-waves
                                 (set-copy (unbox completed-waves-box))
                                 'total-waves
                                 (unbox total-waves-box)
                                 'plan-tool-budget
                                 (unbox plan-tool-budget-box)))))

(define (reset-all-gsd-state!)
  (call-with-semaphore state-sem
                       (lambda ()
                         (gsm-reset!)
                         (reset-steering-state!)
                         (set-box! pinned-dir-box #f)
                         (set-box! budget-box #f)
                         (set-box! edit-limit-box 500)
                         (set-box! read-counts-box (make-hash))
                         (set-box! completed-waves-box (set))
                         (set-box! total-waves-box 0)
                         (set-box! plan-tool-budget-box #f)
                         (set-box! gsd-event-bus-box #f))))
