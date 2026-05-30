#lang racket/base

;; extensions/gsd/session-state.rkt — Per-session GSD state (QUAL-02)
;; STABILITY: evolving
;;
;; v0.32.5 W1: Replaced closure factory with mutable struct.
;; Thread safety: Each context has its own semaphore for atomic updates.
;;
;; v0.29.4 W1: Originally replaced raw box exports with closure-encapsulated factory.
;; This version replaces the closure with a struct for better introspection.

(require racket/contract
         racket/set
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

;; Atomic state+history transaction. Runs thunk under semaphore with
;; read-only state, history, event-bus and write callbacks for state and history.
;; Avoids exposing raw box accessors to consumers.
(define (gsd-ctx-transaction! ctx thunk)
  (call-with-semaphore (gsd-session-ctx-sem ctx)
                       (lambda ()
                         (define state (unbox (gsd-session-ctx-state-box ctx)))
                         (define history (unbox (gsd-session-ctx-history-box ctx)))
                         (define event-bus (unbox (gsd-session-ctx-event-bus-box ctx)))
                         (define (set-state! v)
                           (set-box! (gsd-session-ctx-state-box ctx) v))
                         (define (set-history! v)
                           (set-box! (gsd-session-ctx-history-box ctx) v))
                         (thunk state history event-bus set-state! set-history!))))

;; ============================================================
;; Per-session accessors (explicit ctx argument) — C-01 v0.35.1
;; ============================================================

(define (gsd-ctx-state ctx)
  (ctx-read ctx gsd-session-ctx-state-box))
(define (gsd-ctx-set-state! ctx v)
  (ctx-write! ctx gsd-session-ctx-state-box v))
(define (gsd-ctx-mode ctx)
  (gsd-runtime-state-mode (gsd-ctx-state ctx)))
(define (gsd-ctx-wave-number ctx)
  (gsd-runtime-state-current-wave (gsd-ctx-state ctx)))
(define (gsd-ctx-plan ctx)
  (ctx-read ctx gsd-session-ctx-plan-box))
(define (gsd-ctx-set-plan! ctx v)
  (ctx-write! ctx gsd-session-ctx-plan-box v))
(define (gsd-ctx-pinned-dir ctx)
  (ctx-read ctx gsd-session-ctx-pinned-dir-box))
(define (gsd-ctx-set-pinned-dir! ctx v)
  (ctx-write! ctx gsd-session-ctx-pinned-dir-box v))
(define (gsd-ctx-edit-limit ctx)
  (ctx-read ctx gsd-session-ctx-edit-limit-box))
(define (gsd-ctx-set-edit-limit! ctx v)
  (ctx-write! ctx gsd-session-ctx-edit-limit-box v))
(define (gsd-ctx-event-bus ctx)
  (ctx-read ctx gsd-session-ctx-event-bus-box))
(define (gsd-ctx-set-event-bus! ctx v)
  (ctx-write! ctx gsd-session-ctx-event-bus-box v))
(define (gsd-ctx-history ctx)
  (ctx-read ctx gsd-session-ctx-history-box))
(define (gsd-ctx-set-history! ctx v)
  (ctx-write! ctx gsd-session-ctx-history-box v))
(define (gsd-ctx-correlation-id ctx)
  (ctx-read ctx gsd-session-ctx-correlation-id-box))
(define (gsd-ctx-set-correlation-id! ctx v)
  (ctx-write! ctx gsd-session-ctx-correlation-id-box v))
(define (gsd-ctx-busy ctx)
  (ctx-read ctx gsd-session-ctx-busy-box))
(define (gsd-ctx-set-busy! ctx v)
  (ctx-write! ctx gsd-session-ctx-busy-box v))
(define (gsd-ctx-transaction ctx)
  (ctx-read ctx gsd-session-ctx-transaction-box))
(define (gsd-ctx-set-transaction! ctx v)
  (ctx-write! ctx gsd-session-ctx-transaction-box v))
(define (gsd-ctx-transaction-update! ctx update-fn)
  (ctx-update! ctx gsd-session-ctx-transaction-box update-fn))

;; Thread-safe transaction with explicit ctx
(define (with-gsd-transaction ctx thunk)
  (call-with-semaphore (gsd-session-ctx-sem ctx) thunk))

;; Atomic state operations with explicit ctx
(define (gsd-ctx-state-snapshot ctx)
  (gsd-ctx-state ctx))

(define (gsd-ctx-state-update! ctx update-thunk)
  (call-with-semaphore (gsd-session-ctx-sem ctx)
                       (lambda ()

                         (set-box! (gsd-session-ctx-state-box ctx)
                                   (update-thunk (unbox (gsd-session-ctx-state-box ctx)))))))

(define (gsd-ctx-history-update! ctx update-thunk)
  (call-with-semaphore (gsd-session-ctx-sem ctx)
                       (lambda ()

                         (set-box! (gsd-session-ctx-history-box ctx)
                                   (update-thunk (unbox (gsd-session-ctx-history-box ctx)))))))
;; ============================================================
;; Default global context (backward compatibility)
;; DEPRECATION TIMELINE: Deprecated globals (current-gsd-state, set-gsd-state!,
;; current-gsd-history, set-gsd-history!, gsd-state-snapshot, gsd-state-update!,
;; gsd-history-snapshot, gsd-history-update!) will be removed in v0.38.0.
;; Use per-session gsd-ctx-* accessors and with-gsd-transaction instead.
;; ============================================================

(define gsd-default-ctx (make-gsd-context))

;; v0.57.1 W7: Active context parameter for ctx-aware handler dispatch.
;; Defaults to gsd-default-ctx. Tests can parameterize to isolated contexts.
(define current-gsd-ctx (make-parameter gsd-default-ctx))

;; H-05: gsd-state-sem removed — collapsed into per-ctx semaphore.

;; ============================================================
;; Convenience accessors for default context (DEPRECATED)
;; H-05: These now use DIRECT box access (no semaphore) so they can be
;; safely called inside with-gsd-lock without deadlock.
;; ============================================================

(define (current-gsd-state)
  (unbox (gsd-session-ctx-state-box gsd-default-ctx)))

(define (set-gsd-state! v)
  (set-box! (gsd-session-ctx-state-box gsd-default-ctx) v))

(define (current-gsd-mode)
  (gsd-runtime-state-mode (current-gsd-state)))

(define (current-wave-number)
  (gsd-runtime-state-current-wave (current-gsd-state)))

(define (current-plan-data)
  (unbox (gsd-session-ctx-plan-box gsd-default-ctx)))

(define (set-plan-data! v)
  (set-box! (gsd-session-ctx-plan-box gsd-default-ctx) v))

(define (current-pinned-dir)
  (unbox (gsd-session-ctx-pinned-dir-box gsd-default-ctx)))

(define (set-pinned-dir! v)
  (set-box! (gsd-session-ctx-pinned-dir-box gsd-default-ctx) v))

(define (current-edit-limit)
  (unbox (gsd-session-ctx-edit-limit-box gsd-default-ctx)))

(define (set-edit-limit! v)
  (set-box! (gsd-session-ctx-edit-limit-box gsd-default-ctx) v))

(define (current-gsd-event-bus)
  (unbox (gsd-session-ctx-event-bus-box gsd-default-ctx)))

(define (set-gsd-event-bus! v)
  (set-box! (gsd-session-ctx-event-bus-box gsd-default-ctx) v))

(define (current-gsd-history)
  (unbox (gsd-session-ctx-history-box gsd-default-ctx)))

(define (set-gsd-history! v)
  (set-box! (gsd-session-ctx-history-box gsd-default-ctx) v))

;; H-05: Collapsed two-level locking. The outer gsd-state-sem has been removed.
;; with-gsd-lock now acquires the per-ctx semaphore directly.
;; This eliminates the possibility of nested lock acquisition deadlock.
(define (with-gsd-lock thunk)
  (call-with-semaphore (gsd-session-ctx-sem gsd-default-ctx) thunk))

;; ============================================================
;; Atomic state operations (H-05: now use direct box access inside with-gsd-lock)
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

;; Struct exports (plain)
(provide gsd-session-ctx?
         ;; Global default context (plain)
         gsd-default-ctx
         current-gsd-ctx
         ;; Functions (contracted)
         (contract-out [make-gsd-context (-> gsd-session-ctx?)]
                       [ctx-read (-> gsd-session-ctx? procedure? any)]
                       [ctx-write! (-> gsd-session-ctx? procedure? any/c void?)]
                       [ctx-update! (-> gsd-session-ctx? procedure? procedure? void?)]
                       ;; Per-session accessors (C-01, v0.35.1)
                       [gsd-ctx-state (-> gsd-session-ctx? gsd-runtime-state?)]
                       [gsd-ctx-set-state! (-> gsd-session-ctx? gsd-runtime-state? void?)]
                       [gsd-ctx-mode (-> gsd-session-ctx? symbol?)]
                       [gsd-ctx-wave-number (-> gsd-session-ctx? exact-nonnegative-integer?)]
                       [gsd-ctx-plan (-> gsd-session-ctx? any)]
                       [gsd-ctx-set-plan! (-> gsd-session-ctx? any/c void?)]
                       [gsd-ctx-pinned-dir (-> gsd-session-ctx? (or/c path-string? #f))]
                       [gsd-ctx-set-pinned-dir! (-> gsd-session-ctx? (or/c path-string? #f) void?)]
                       [gsd-ctx-edit-limit (-> gsd-session-ctx? exact-positive-integer?)]
                       [gsd-ctx-set-edit-limit! (-> gsd-session-ctx? exact-positive-integer? void?)]
                       [gsd-ctx-event-bus (-> gsd-session-ctx? any)]
                       [gsd-ctx-set-event-bus! (-> gsd-session-ctx? any/c void?)]
                       [gsd-ctx-history (-> gsd-session-ctx? list?)]
                       [gsd-ctx-set-history! (-> gsd-session-ctx? list? void?)]
                       [gsd-ctx-correlation-id (-> gsd-session-ctx? (or/c string? #f))]
                       [gsd-ctx-set-correlation-id! (-> gsd-session-ctx? (or/c string? #f) void?)]
                       [gsd-ctx-busy (-> gsd-session-ctx? any)]
                       [gsd-ctx-set-busy! (-> gsd-session-ctx? any/c void?)]
                       [gsd-ctx-transaction (-> gsd-session-ctx? hash?)]
                       [gsd-ctx-set-transaction! (-> gsd-session-ctx? hash? void?)]
                       [gsd-ctx-transaction-update! (-> gsd-session-ctx? procedure? void?)]
                       [gsd-ctx-transaction! (-> gsd-session-ctx? procedure? any)]
                       [with-gsd-transaction (-> gsd-session-ctx? procedure? any)]
                       [gsd-ctx-state-snapshot (-> gsd-session-ctx? gsd-runtime-state?)]
                       [gsd-ctx-state-update! (-> gsd-session-ctx? procedure? any)]
                       [gsd-ctx-history-update! (-> gsd-session-ctx? procedure? any)]
                       ;; Backward-compatible accessors (deprecated, removal v0.37.0)
                       [current-gsd-state (-> gsd-runtime-state?)]
                       [set-gsd-state! (-> gsd-runtime-state? void?)]
                       [current-gsd-mode (-> symbol?)]
                       [current-wave-number (-> exact-nonnegative-integer?)]
                       [current-plan-data (-> any)]
                       [set-plan-data! (-> any/c void?)]
                       [current-pinned-dir (-> (or/c path-string? #f))]
                       [set-pinned-dir! (-> (or/c path-string? #f) void?)]
                       [current-edit-limit (-> exact-positive-integer?)]
                       [set-edit-limit! (-> exact-positive-integer? void?)]
                       [current-gsd-event-bus (-> any)]
                       [set-gsd-event-bus! (-> any/c void?)]
                       [current-gsd-history (-> list?)]
                       [set-gsd-history! (-> list? void?)]
                       ;; Atomic operations (deprecated global)
                       [gsd-state-snapshot (-> gsd-runtime-state?)]
                       [gsd-state-update! (-> procedure? any)]
                       [gsd-history-snapshot (-> list?)]
                       [gsd-history-update! (-> procedure? any)]
                       [with-gsd-lock (-> procedure? any)]))
