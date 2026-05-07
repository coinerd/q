#lang racket/base

;; extensions/gsd/session-state.rkt — Per-session GSD state (QUAL-02)
;; STABILITY: evolving
;;
;; v0.32.5 W1: Replaced closure factory with mutable struct.
;; Thread safety: Each context has its own semaphore for atomic updates.
;;
;; v0.29.4 W1: Originally replaced raw box exports with closure-encapsulated factory.
;; This version replaces the closure with a struct for better introspection.

(require racket/set
         "runtime-state-types.rkt")

;; ============================================================
;; Context struct (replaces closure factory)
;; ============================================================

;; Mutable struct with semaphore for thread-safe access.
;; Each field corresponds to a former closure variable.
(struct gsd-session-ctx
        (state-box ; (boxof gsd-runtime-state)
         plan-box ; (boxof any) — plan data or #f
         pinned-dir-box ; (boxof (or/c string? #f))
         edit-limit-box ; (boxof positive-integer?)
         event-bus-box ; (boxof any) — event bus or #f
         history-box ; (boxof list?)
         busy-box ; (boxof boolean?)
         correlation-id-box ; (boxof (or/c string? #f))
         transaction-box ; (boxof hash?)
         sem ; semaphore for thread safety
         )
  #:transparent)

(define (make-gsd-context)
  "Create an isolated GSD context with thread-safe accessors."
  (gsd-session-ctx (box (make-initial-gsd-state))
                   (box #f)
                   (box #f)
                   (box 500)
                   (box #f)
                   (box '())
                   (box #f)
                   (box #f)
                   (box (hash))
                   (make-semaphore 1)))

;; ============================================================
;; Thread-safe accessors
;; ============================================================

;; Perform a thread-safe read of a context field.
(define (ctx-read ctx box-accessor)
  (call-with-semaphore (gsd-session-ctx-sem ctx) (lambda () (unbox (box-accessor ctx)))))

;; Perform a thread-safe write to a context field.
(define (ctx-write! ctx box-accessor value)
  (call-with-semaphore (gsd-session-ctx-sem ctx) (lambda () (set-box! (box-accessor ctx) value))))

;; Perform a thread-safe update (read-modify-write) on a context field.
(define (ctx-update! ctx box-accessor update-fn)
  (call-with-semaphore (gsd-session-ctx-sem ctx)
                       (lambda ()
                         (set-box! (box-accessor ctx) (update-fn (unbox (box-accessor ctx)))))))

;; ============================================================
;; Default global context (backward compatibility)
;; ============================================================

(define gsd-default-ctx (make-gsd-context))

;; Outer semaphore for compound operations (with-gsd-lock).
;; The struct has its own inner semaphore for individual operations.
;; Two-level locking: outer serializes compound ops, inner serializes individual ops.
;; No deadlock risk since outer is always acquired before inner.
(define gsd-state-sem (make-semaphore 1))

;; ============================================================
;; Convenience accessors for default context
;; ============================================================

(define (current-gsd-state)
  (ctx-read gsd-default-ctx gsd-session-ctx-state-box))

(define (set-gsd-state! v)
  (ctx-write! gsd-default-ctx gsd-session-ctx-state-box v))

(define (current-gsd-mode)
  (gsd-runtime-state-mode (current-gsd-state)))

(define (current-wave-number)
  (gsd-runtime-state-current-wave (current-gsd-state)))

(define (current-plan-data)
  (ctx-read gsd-default-ctx gsd-session-ctx-plan-box))

(define (set-plan-data! v)
  (ctx-write! gsd-default-ctx gsd-session-ctx-plan-box v))

(define (current-pinned-dir)
  (ctx-read gsd-default-ctx gsd-session-ctx-pinned-dir-box))

(define (set-pinned-dir! v)
  (ctx-write! gsd-default-ctx gsd-session-ctx-pinned-dir-box v))

(define (current-edit-limit)
  (ctx-read gsd-default-ctx gsd-session-ctx-edit-limit-box))

(define (set-edit-limit! v)
  (ctx-write! gsd-default-ctx gsd-session-ctx-edit-limit-box v))

(define (current-gsd-event-bus)
  (ctx-read gsd-default-ctx gsd-session-ctx-event-bus-box))

(define (set-gsd-event-bus! v)
  (ctx-write! gsd-default-ctx gsd-session-ctx-event-bus-box v))

(define (current-gsd-history)
  (ctx-read gsd-default-ctx gsd-session-ctx-history-box))

(define (set-gsd-history! v)
  (ctx-write! gsd-default-ctx gsd-session-ctx-history-box v))

;; ============================================================
;; Thread-safe lock helper (outer semaphore for compound ops)
;; ============================================================

(define (with-gsd-lock thunk)
  (call-with-semaphore gsd-state-sem thunk))

;; ============================================================
;; Atomic state operations
;; ============================================================

(define (gsd-state-snapshot)
  (with-gsd-lock (lambda () (current-gsd-state))))

(define (gsd-state-update! update-thunk)
  (with-gsd-lock (lambda () (set-gsd-state! (update-thunk (current-gsd-state))))))

(define (gsd-history-snapshot)
  (with-gsd-lock (lambda () (current-gsd-history))))

(define (gsd-history-update! update-thunk)
  (with-gsd-lock (lambda () (set-gsd-history! (update-thunk (current-gsd-history))))))

;; ============================================================
;; Provide
;; ============================================================

(provide make-gsd-context
         gsd-session-ctx?
         gsd-session-ctx-state-box
         gsd-session-ctx-plan-box
         gsd-session-ctx-pinned-dir-box
         gsd-session-ctx-edit-limit-box
         gsd-session-ctx-event-bus-box
         gsd-session-ctx-history-box
         gsd-session-ctx-busy-box
         gsd-session-ctx-correlation-id-box
         gsd-session-ctx-transaction-box
         gsd-session-ctx-sem
         ctx-read
         ctx-write!
         ctx-update!
         ;; Backward-compatible accessors
         current-gsd-state
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
         gsd-history-update!
         with-gsd-lock)
