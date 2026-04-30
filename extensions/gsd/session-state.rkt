#lang racket/base

;; extensions/gsd/session-state.rkt — Per-session GSD state (QUAL-02)
;; STABILITY: evolving
;;
;; Provides shared mutable state for GSD session via boxes.
;; v0.22.2 REV-01: Reverted from make-parameter to box storage because
;; hook handlers run in child threads (hooks.rkt:108) where parameter
;; mutations are invisible to the parent thread.
;;
;; Thread safety: The semaphore gsd-state-sem serializes atomic updates.
;; Boxes are shared across threads — mutations visible to all threads.

(require racket/set
         "runtime-state-types.rkt")

;; ============================================================
;; Shared mutable state (boxes, not parameters)
;; ============================================================

;; Core GSD state: gsd-runtime-state struct (F1 fix)
(define gsd-state-box (box (make-initial-gsd-state)))

;; Plan data
(define gsd-plan-data-box (box #f))

;; Planning directory
(define gsd-pinned-dir-box (box #f))

;; Edit limit
(define gsd-edit-limit-box (box 500))

;; Event bus
(define gsd-event-bus-box (box #f))

;; State machine history
(define gsd-history-box (box '()))

;; Semaphore for atomic state transitions
(define gsd-state-sem (make-semaphore 1))

;; ============================================================
;; Parameter-compatible accessors (readers)
;; ============================================================

(define (current-gsd-state)
  (unbox gsd-state-box))

(define (set-gsd-state! v)
  (set-box! gsd-state-box v))

(define (current-gsd-mode)
  (gsd-runtime-state-mode (unbox gsd-state-box)))

(define (current-wave-number)
  (gsd-runtime-state-current-wave (unbox gsd-state-box)))

(define (current-plan-data)
  (unbox gsd-plan-data-box))

(define (set-plan-data! v)
  (set-box! gsd-plan-data-box v))

(define (current-pinned-dir)
  (unbox gsd-pinned-dir-box))

(define (set-pinned-dir! v)
  (set-box! gsd-pinned-dir-box v))

(define (current-edit-limit)
  (unbox gsd-edit-limit-box))

(define (set-edit-limit! v)
  (set-box! gsd-edit-limit-box v))

(define (current-gsd-event-bus)
  (unbox gsd-event-bus-box))

(define (set-gsd-event-bus! v)
  (set-box! gsd-event-bus-box v))

(define (current-gsd-history)
  (unbox gsd-history-box))

(define (set-gsd-history! v)
  (set-box! gsd-history-box v))

;; ============================================================
;; Atomic state operations
;; ============================================================

(define (gsd-state-snapshot)
  (call-with-semaphore gsd-state-sem (lambda () (unbox gsd-state-box))))

(define (gsd-state-update! update-thunk)
  (call-with-semaphore gsd-state-sem
                       (lambda () (set-box! gsd-state-box (update-thunk (unbox gsd-state-box))))))

(define (gsd-history-snapshot)
  (call-with-semaphore gsd-state-sem (lambda () (reverse (unbox gsd-history-box)))))

(define (gsd-history-update! update-thunk)
  (call-with-semaphore gsd-state-sem
                       (lambda () (set-box! gsd-history-box (update-thunk (unbox gsd-history-box))))))

;; ============================================================
;; Provide
;; ============================================================

(provide current-gsd-state
         set-gsd-state!
         current-gsd-mode
         current-wave-number
         current-plan-data
         set-plan-data!
         current-pinned-dir
         set-pinned-dir!
         current-edit-limit
         set-edit-limit!
         current-gsd-event-bus
         set-gsd-event-bus!
         current-gsd-history
         set-gsd-history!
         ;; Atomic operations
         gsd-state-sem
         gsd-state-snapshot
         gsd-state-update!
         gsd-history-snapshot
         gsd-history-update!)
