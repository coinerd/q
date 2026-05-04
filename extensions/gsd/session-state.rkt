#lang racket/base

;; extensions/gsd/session-state.rkt — Per-session GSD state (QUAL-02)
;; STABILITY: evolving
;;
;; v0.29.4 W1: Replaced raw box exports with closure-encapsulated factory.
;; The default global context is still available via the accessor functions
;; for backward compatibility. New code should use make-gsd-context for
;; isolated contexts.
;;
;; Thread safety: Each context has its own semaphore for atomic updates.

(require racket/set
         "runtime-state-types.rkt")

;; ============================================================
;; Closure factory
;; ============================================================

(define (make-gsd-context)
  "Create an isolated GSD context with thread-safe dispatch.
   Returns a procedure that accepts actions:
     'get-state, 'set-state, 'get-plan, 'set-plan,
     'busy?, 'set-busy!,
     'get-correlation-id, 'set-correlation-id!,
     'transaction-ref, 'transaction-set!,
     'get-history, 'set-history!,
     'get-edit-limit, 'set-edit-limit!,
     'get-pinned-dir, 'set-pinned-dir!,
     'get-event-bus, 'set-event-bus!"
  (let ([state (make-initial-gsd-state)]
        [plan-data #f]
        [pinned-dir #f]
        [edit-limit 500]
        [event-bus #f]
        [history '()]
        [busy #f]
        [correlation-id #f]
        [transaction (hash)]
        [sem (make-semaphore 1)])
    (lambda (action . args)
      (call-with-semaphore
       sem
       (lambda ()
         (case action
           ;; Core state
           [(get-state) state]
           [(set-state) (set! state (car args))]
           ;; Plan data
           [(get-plan) plan-data]
           [(set-plan) (set! plan-data (car args))]
           ;; Busy flag
           [(busy?) busy]
           [(set-busy!) (set! busy (car args))]
           ;; Correlation ID
           [(get-correlation-id) correlation-id]
           [(set-correlation-id!) (set! correlation-id (car args))]
           ;; Transaction store
           [(transaction-ref) (hash-ref transaction (car args))]
           [(transaction-set!) (set! transaction (hash-set transaction (car args) (cadr args)))]
           ;; History
           [(get-history) history]
           [(set-history!) (set! history (car args))]
           ;; Edit limit
           [(get-edit-limit) edit-limit]
           [(set-edit-limit!) (set! edit-limit (car args))]
           ;; Pinned dir
           [(get-pinned-dir) pinned-dir]
           [(set-pinned-dir!) (set! pinned-dir (car args))]
           ;; Event bus
           [(get-event-bus) event-bus]
           [(set-event-bus!) (set! event-bus (car args))]
           [else (error 'gsd-context "unknown action: ~a" action)]))))))

;; ============================================================
;; Default global context (backward compatibility)
;; ============================================================

(define gsd-default-ctx (make-gsd-context))

;; Outer semaphore for compound operations (with-gsd-lock).
;; The closure has its own inner semaphore for individual operations.
;; Two-level locking: outer serializes compound ops, inner serializes individual ops.
;; No deadlock risk since outer is always acquired before inner.
(define gsd-state-sem (make-semaphore 1))

(define (current-gsd-state)
  (gsd-default-ctx 'get-state))

(define (set-gsd-state! v)
  (gsd-default-ctx 'set-state v))

(define (current-gsd-mode)
  (gsd-runtime-state-mode (current-gsd-state)))

(define (current-wave-number)
  (gsd-runtime-state-current-wave (current-gsd-state)))

(define (current-plan-data)
  (gsd-default-ctx 'get-plan))

(define (set-plan-data! v)
  (gsd-default-ctx 'set-plan v))

(define (current-pinned-dir)
  (gsd-default-ctx 'get-pinned-dir))

(define (set-pinned-dir! v)
  (gsd-default-ctx 'set-pinned-dir! v))

(define (current-edit-limit)
  (gsd-default-ctx 'get-edit-limit))

(define (set-edit-limit! v)
  (gsd-default-ctx 'set-edit-limit! v))

(define (current-gsd-event-bus)
  (gsd-default-ctx 'get-event-bus))

(define (set-gsd-event-bus! v)
  (gsd-default-ctx 'set-event-bus! v))

(define (current-gsd-history)
  (gsd-default-ctx 'get-history))

(define (set-gsd-history! v)
  (gsd-default-ctx 'set-history! v))

;; ============================================================
;; Thread-safe lock helper (outer semaphore for compound ops)
;; ============================================================

(define (with-gsd-lock thunk)
  (call-with-semaphore gsd-state-sem thunk))

;; ============================================================
;; Atomic state operations
;; ============================================================

(define (gsd-state-snapshot)
  (with-gsd-lock (lambda () (gsd-default-ctx 'get-state))))

(define (gsd-state-update! update-thunk)
  (with-gsd-lock (lambda ()
                   (gsd-default-ctx 'set-state (update-thunk (gsd-default-ctx 'get-state))))))

(define (gsd-history-snapshot)
  (with-gsd-lock (lambda () (gsd-default-ctx 'get-history))))

(define (gsd-history-update! update-thunk)
  (with-gsd-lock (lambda ()
                   (gsd-default-ctx 'set-history! (update-thunk (gsd-default-ctx 'get-history))))))

;; ============================================================
;; Provide
;; ============================================================

;; Closure factory
(provide make-gsd-context
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
