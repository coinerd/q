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
         ;; Global default context (plain)
         gsd-default-ctx
         ;; Functions (contracted)
         (contract-out [make-gsd-context (-> any/c)]
                       [ctx-read (-> any/c any/c any/c)]
                       [ctx-write! (-> any/c any/c any/c any/c)]
                       [ctx-update! (-> any/c any/c any/c any/c)]
                       ;; Per-session accessors (C-01, v0.35.1)
                       [gsd-ctx-state (-> any/c any/c)]
                       [gsd-ctx-set-state! (-> any/c any/c any/c)]
                       [gsd-ctx-mode (-> any/c any/c)]
                       [gsd-ctx-wave-number (-> any/c any/c)]
                       [gsd-ctx-plan (-> any/c any/c)]
                       [gsd-ctx-set-plan! (-> any/c any/c any/c)]
                       [gsd-ctx-pinned-dir (-> any/c any/c)]
                       [gsd-ctx-set-pinned-dir! (-> any/c any/c any/c)]
                       [gsd-ctx-edit-limit (-> any/c any/c)]
                       [gsd-ctx-set-edit-limit! (-> any/c any/c any/c)]
                       [gsd-ctx-event-bus (-> any/c any/c)]
                       [gsd-ctx-set-event-bus! (-> any/c any/c any/c)]
                       [gsd-ctx-history (-> any/c any/c)]
                       [gsd-ctx-set-history! (-> any/c any/c any/c)]
                       [gsd-ctx-correlation-id (-> any/c any/c)]
                       [gsd-ctx-set-correlation-id! (-> any/c any/c any/c)]
                       [with-gsd-transaction (-> any/c any/c any/c)]
                       [gsd-ctx-state-snapshot (-> any/c any/c)]
                       [gsd-ctx-state-update! (-> any/c any/c any/c)]
                       [gsd-ctx-history-update! (-> any/c any/c any/c)]
                       ;; Backward-compatible accessors (deprecated, removal v0.37.0)
                       [current-gsd-state (-> any/c)]
                       [set-gsd-state! (-> any/c any/c)]
                       [current-gsd-mode (-> any/c)]
                       [current-wave-number (-> any/c)]
                       [current-plan-data (-> any/c)]
                       [set-plan-data! (-> any/c any/c)]
                       [current-pinned-dir (-> any/c)]
                       [set-pinned-dir! (-> any/c any/c)]
                       [current-edit-limit (-> any/c)]
                       [set-edit-limit! (-> any/c any/c)]
                       [current-gsd-event-bus (-> any/c)]
                       [set-gsd-event-bus! (-> any/c any/c)]
                       [current-gsd-history (-> any/c)]
                       [set-gsd-history! (-> any/c any/c)]
                       ;; Atomic operations (deprecated global)
                       [gsd-state-snapshot (-> any/c)]
                       [gsd-state-update! (-> any/c any/c)]
                       [gsd-history-snapshot (-> any/c)]
                       [gsd-history-update! (-> any/c any/c)]
                       [with-gsd-lock (-> any/c any/c)]))
