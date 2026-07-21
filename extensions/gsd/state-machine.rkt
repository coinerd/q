#lang racket/base

;; extensions/gsd/state-machine.rkt — GSD State Machine
;; STABILITY: evolving
;;
;; Wave 0 of v0.21.0: Central state machine with explicit transitions and guards.
;; v0.22.1 QUAL-03: Migrated from global mutable boxes to per-session parameters
;; via session-state.rkt.
;;
;; States: idle → exploring → plan-written → executing → verifying → idle
;;                 ↑              ↓
;;                 └──────────────┘ (re-plan on failure)
;;
;; Thread safety: Uses gsd-state-sem from session-state.rkt for atomic updates.
;;
;; W5 v0.99.35: Pure transition logic (state predicates, transition table,
;; compute-next-gsm-state, result types) extracted to transition-logic.rkt.
;; This module now focuses on session-state interactions, event-bus emission,
;; wave-gate tracking, and rework-loop protection.

(require racket/contract
         racket/set
         "runtime-state-types.rkt"
         ;; W5 v0.99.35: Pure transition logic (re-exported below)
         "transition-logic.rkt"
         (only-in "policy.rkt" blocked-tools-for gsd-decide-action policy-allowed?)
         (only-in "events.rkt"
                  emit-gsd-event!
                  ctx-emit-gsd-event!
                  current-gsd-correlation-id
                  emit-to-bus!)
         (only-in "event-structs.rkt"
                  make-gsd-transition-attempted-event
                  make-gsd-transition-succeeded-event
                  make-gsd-transition-failed-event)
         (only-in "session-state.rkt"
                  current-gsd-ctx
                  current-gsd-history
                  set-gsd-history!
                  gsd-state-snapshot
                  gsd-history-snapshot
                  gsd-state-update!
                  gsd-default-ctx
                  gsd-session-ctx?
                  gsd-ctx-state
                  gsd-ctx-set-state!
                  gsd-ctx-state-snapshot
                  gsd-ctx-state-update!
                  gsd-ctx-history
                  gsd-ctx-set-history!
                  gsd-ctx-history-update!
                  gsd-ctx-transaction!
                  gsd-ctx-rework-count
                  gsd-ctx-reset-rework-count!
                  gsd-session-ctx-rework-count-box
                  gsd-ctx-event-bus))

;; Re-export pure transition logic (public API only, not internal struct accessors)
(provide ok-result
         ok-result?
         err-result
         err-result?
         ok?
         ok-from
         ok-to
         err?
         err-reason
         GSD-STATES
         TRANSITIONS
         TRANSITIONS-FLAT
         gsm-state?
         valid-transition?
         valid-targets
         find-transition-path
         compute-next-gsm-state
         check-state-invariants
         compute-next-pending-wave)

;; Struct exports (plain)
(provide gsd-runtime-state
         gsd-runtime-state?
         gsd-runtime-state-mode
         gsd-runtime-state-total-waves
         gsd-runtime-state-current-wave
         gsd-runtime-state-completed-waves
         gsd-runtime-state-wave-executor
         gsd-runtime-state-plan-path
         gsd-runtime-state-pinned-dir
         gsd-runtime-state-edit-limit
         gsd-runtime-state-transition-history
         ;; Wave gate
         gsd-wave-gate-counter
         gsd-wave-gate-interval
         gsd-wave-gate-increment!
         ;; Rework-loop protection (v0.99.20 W1)
         gsd-max-rework-iterations
         gsd-rework-limit-reached?)

;; Functions (contracted)
(provide (contract-out
          [make-initial-gsd-state (-> gsd-runtime-state?)]
          [gsm-current (-> symbol?)]
          [gsm-transition! (->* (symbol?) (#:event (or/c symbol? #f)) (or/c ok-result? err-result?))]
          [gsm-transition-to! (-> symbol? (or/c ok-result? err-result?))]
          [gsm-reset! (-> (or/c ok-result? err-result?))]
          [gsm-valid-next-states (-> (listof symbol?))]
          [gsm-tool-allowed? (-> string? boolean?)]
          [gsm-snapshot (-> gsd-runtime-state?)]
          [reset-gsm! (-> void?)]
          [gsm-history (-> list?)]
          [gsm-wave-executor (-> (or/c any/c #f))]
          [gsm-set-wave-executor! (-> (or/c any/c #f) void?)]
          [gsm-total-waves (-> exact-nonnegative-integer?)]
          [gsm-set-total-waves! (-> exact-nonnegative-integer? void?)]
          [gsm-current-wave (-> exact-nonnegative-integer?)]
          [gsm-set-current-wave! (-> exact-nonnegative-integer? void?)]
          [gsm-completed-waves (-> (set/c exact-nonnegative-integer?))]
          [gsm-mark-wave-complete! (-> exact-nonnegative-integer? void?)]
          [gsm-wave-complete? (-> exact-nonnegative-integer? boolean?)]
          [gsm-next-pending-wave (-> (or/c exact-nonnegative-integer? #f))]
          [gsm-clear-wave-gate! (-> void?)]
          [gsd-wave-gate-blocked? (-> boolean?)]
          [gsd-invariants-hold? (-> (values boolean? (or/c string? #f)))]
          ;; Context-aware API (v0.57.1 W6)
          [gsm-ctx-current (-> gsd-session-ctx? symbol?)]
          [gsm-ctx-transition!
           (->* (gsd-session-ctx? symbol?) (#:event (or/c symbol? #f)) (or/c ok-result? err-result?))]
          [gsm-ctx-reset! (-> gsd-session-ctx? (or/c ok-result? err-result?))]
          [gsm-ctx-history (-> gsd-session-ctx? list?)]
          [gsm-ctx-valid-next-states (-> gsd-session-ctx? (listof symbol?))]
          ;; Context-aware API extended (v0.57.1 W7)
          [gsm-ctx-transition-to! (-> gsd-session-ctx? symbol? (or/c ok-result? err-result?))]
          [gsm-ctx-snapshot (-> gsd-session-ctx? gsd-runtime-state?)]
          [gsm-ctx-wave-executor (-> gsd-session-ctx? any/c)]
          [gsm-ctx-set-wave-executor! (-> gsd-session-ctx? any/c void?)]
          [gsm-ctx-total-waves (-> gsd-session-ctx? exact-nonnegative-integer?)]
          [gsm-ctx-set-total-waves! (-> gsd-session-ctx? exact-nonnegative-integer? void?)]
          [gsm-ctx-current-wave (-> gsd-session-ctx? exact-nonnegative-integer?)]
          [gsm-ctx-set-current-wave! (-> gsd-session-ctx? exact-nonnegative-integer? void?)]
          [gsm-ctx-completed-waves (-> gsd-session-ctx? set?)]
          [gsm-ctx-mark-wave-complete! (-> gsd-session-ctx? exact-nonnegative-integer? void?)]
          [gsm-ctx-wave-complete? (-> gsd-session-ctx? exact-nonnegative-integer? boolean?)]
          [gsm-ctx-next-pending-wave (-> gsd-session-ctx? (or/c exact-nonnegative-integer? #f))]
          [gsm-ctx-state-restore! (-> gsd-session-ctx? gsd-runtime-state? void?)]))

;; ============================================================
;; Core API — now backed by session-state parameters
;; ============================================================

#| DEPRECATED — use gsm-ctx-* with explicit context instead. Will be removed in v1.0.0. |#
(define (gsm-current)
  (gsm-ctx-current gsd-default-ctx))

(define (gsm-history)
  (gsm-ctx-history gsd-default-ctx))

(define (gsm-transition! target #:event [event #f])
  (gsd-ctx-transaction!
   gsd-default-ctx
   (lambda (state history event-bus set-state! set-history!)
     (define current (gsd-runtime-state-mode state))
     (emit-to-bus!
      event-bus
      'gsd.transition.attempted
      (make-gsd-transition-attempted-event #:session-id "" #:turn-id 0 #:from current #:to target))
     (define-values (result new-state) (compute-next-gsm-state state target #:event event))
     (cond
       [(ok? result)
        (define new-mode (gsd-runtime-state-mode new-state))
        (set-state! new-state)
        (set-history! (cons (list current new-mode (current-seconds)) history))
        (emit-to-bus! event-bus
                      'gsd.transition.succeeded
                      (make-gsd-transition-succeeded-event #:session-id ""
                                                           #:turn-id 0
                                                           #:from current
                                                           #:to new-mode))
        result]
       [else
        (emit-to-bus! event-bus
                      'gsd.transition.failed
                      (make-gsd-transition-failed-event #:session-id ""
                                                        #:turn-id 0
                                                        #:from current
                                                        #:to target
                                                        #:reason
                                                        (format "invalid: ~a -> ~a" current target)))
        result]))))

;; FF-01: Auto-routing transition — finds shortest path via BFS and follows it.
;; additive API; gsm-transition! still works for single-step transitions.
(define (gsm-transition-to! target)
  (define current (gsm-current))
  (cond
    [(eq? current target) (ok-result current target)]
    [else
     (define path (find-transition-path current target))
     (if path
         (for/fold ([result (ok-result current current)]) ([step path])
           (gsm-transition! step))
         (gsm-transition! target))]))

(define (gsm-reset!)
  (gsd-ctx-transaction!
   gsd-default-ctx
   (lambda (state history event-bus set-state! set-history!)
     (define old (gsd-runtime-state-mode state))
     (set-state! (struct-copy gsd-runtime-state state [mode 'idle] [wave-executor #f]))
     (set-history! (cons (list old 'idle (current-seconds)) history))
     (ok-result old 'idle))))

(define (reset-gsm!)
  (gsd-ctx-transaction! gsd-default-ctx
                        (lambda (state history event-bus set-state! set-history!)
                          (set-state! (make-initial-gsd-state))
                          (set-history! '())
                          (void))))

(define (gsm-valid-next-states)
  (gsm-ctx-valid-next-states gsd-default-ctx))

(define (gsm-snapshot)
  (gsm-ctx-snapshot gsd-default-ctx))

;; ============================================================
;; Tool access
;; ============================================================

(define (gsm-tool-allowed? tool-name)
  (define current (gsm-current))
  (policy-allowed? (gsd-decide-action (hasheq 'mode current 'tool tool-name) 'tool-call)))

;; ============================================================
;; Wave state accessors — now backed by session-state parameters
;; ============================================================

#| DEPRECATED — use gsm-ctx-* with explicit context instead. Will be removed in v1.0.0. |#
(define (gsm-wave-executor)
  (gsm-ctx-wave-executor gsd-default-ctx))

(define (gsm-set-wave-executor! exec)
  (gsm-ctx-set-wave-executor! gsd-default-ctx exec))

(define (gsm-total-waves)
  (gsm-ctx-total-waves gsd-default-ctx))

(define (gsm-set-total-waves! n)
  (gsm-ctx-set-total-waves! gsd-default-ctx n))

#| DEPRECATED — use gsm-ctx-* with explicit context instead. Will be removed in v1.0.0. |#
(define (gsm-current-wave)
  (gsm-ctx-current-wave gsd-default-ctx))

(define (gsm-set-current-wave! n)
  (gsm-ctx-set-current-wave! gsd-default-ctx n))

(define (gsm-completed-waves)
  (gsm-ctx-completed-waves gsd-default-ctx))

(define (gsm-mark-wave-complete! idx)
  (gsm-ctx-mark-wave-complete! gsd-default-ctx idx))

#| DEPRECATED — use gsm-ctx-* with explicit context instead. Will be removed in v1.0.0. |#
(define (gsm-wave-complete? idx)
  (set-member? (gsm-completed-waves) idx))

(define (gsm-next-pending-wave)
  (compute-next-pending-wave (gsm-total-waves) (gsm-completed-waves)))

;; ============================================================
;; Wave gate (budget enforcement)
;; ============================================================

;; Tracks consecutive waves since last gate clear.
;; Stored in a parameter for simplicity — single-threaded per session.
(define gsd-wave-gate-counter (make-parameter 0))
(define gsd-wave-gate-interval (make-parameter 5))

(define (gsd-wave-gate-blocked?)
  (>= (gsd-wave-gate-counter) (gsd-wave-gate-interval)))

(define (gsm-clear-wave-gate!)
  (gsd-wave-gate-counter 0))

;; Incremented by wave-start! — called from wave-executor
(define (gsd-wave-gate-increment!)
  (gsd-wave-gate-counter (add1 (gsd-wave-gate-counter))))

;; ============================================================
;; Rework-loop protection (v0.99.20 W1 — §3.3)
;; ============================================================

;; Maximum consecutive verifying→executing (rework) transitions before
;; the state machine blocks and forces 'idle (done).
;; Config: mas.verifier.max-rework-iterations (default 3)
(define gsd-max-rework-iterations (make-parameter 3))

(define (gsd-rework-limit-reached? ctx)
  (>= (unbox (gsd-session-ctx-rework-count-box ctx)) (gsd-max-rework-iterations)))

;; ============================================================
;; State invariants (F3 fix: runtime invariant checks)
;; ============================================================

;; Returns (values ok? error-message-or-#f).
;; W5 v0.99.35: Delegates pure invariant checking to check-state-invariants.
(define (gsd-invariants-hold?)
  (check-state-invariants (gsd-state-snapshot)))

;; ============================================================
;; Context-aware API (v0.57.1 W6)
;; ============================================================

(define (gsm-ctx-current ctx)
  (gsd-runtime-state-mode (gsd-ctx-state-snapshot ctx)))

(define (gsm-ctx-history ctx)
  (gsd-ctx-history ctx))

(define (gsm-ctx-valid-next-states ctx)
  (valid-targets (gsm-ctx-current ctx)))

(define (gsm-ctx-transition! ctx target #:event [event #f])
  ;; v0.99.20 W1: Rework-loop protection.
  ;; Block verifying→executing (rework) when limit is reached.
  ;; Reset counter on fresh execution start (plan-written→executing).
  (define is-rework?
    (and (eq? (gsd-runtime-state-mode (gsd-ctx-state-snapshot ctx)) 'verifying)
         (eq? target 'executing)
         (or (not event) (eq? event 'rework))))
  (cond
    [(and is-rework? (gsd-rework-limit-reached? ctx))
     (emit-to-bus! (gsd-ctx-event-bus ctx)
                   'gsd.transition.failed
                   (make-gsd-transition-failed-event #:session-id ""
                                                     #:turn-id 0
                                                     #:from 'verifying
                                                     #:to target
                                                     #:reason (format "rework limit (~a) reached"
                                                                      (gsd-max-rework-iterations))))
     (err-result (format "rework limit (~a) reached" (gsd-max-rework-iterations)) 'verifying target)]
    [else
     (gsd-ctx-transaction!
      ctx
      (lambda (state history event-bus set-state! set-history!)
        (define current (gsd-runtime-state-mode state))
        (emit-to-bus!
         event-bus
         'gsd.transition.attempted
         (make-gsd-transition-attempted-event #:session-id "" #:turn-id 0 #:from current #:to target))
        (define-values (result new-state) (compute-next-gsm-state state target #:event event))
        (cond
          [(ok? result)
           (define new-mode (gsd-runtime-state-mode new-state))
           ;; v0.99.20 W1: Increment rework counter on verifying→executing transition.
           ;; Reset counter on fresh execution start (plan-written→executing).
           (cond
             [(and (eq? current 'verifying) (eq? new-mode 'executing))
              (set-box! (gsd-session-ctx-rework-count-box ctx)
                        (add1 (unbox (gsd-session-ctx-rework-count-box ctx))))]
             [(and (eq? current 'plan-written) (eq? new-mode 'executing))
              (set-box! (gsd-session-ctx-rework-count-box ctx) 0)])
           (set-state! new-state)
           (set-history! (cons (list current new-mode (current-seconds)) history))
           (emit-to-bus! event-bus
                         'gsd.transition.succeeded
                         (make-gsd-transition-succeeded-event #:session-id ""
                                                              #:turn-id 0
                                                              #:from current
                                                              #:to new-mode))
           result]
          [else
           (emit-to-bus! event-bus
                         'gsd.transition.failed
                         (make-gsd-transition-failed-event
                          #:session-id ""
                          #:turn-id 0
                          #:from current
                          #:to target
                          #:reason (format "invalid: ~a -> ~a" current target)))
           result])))]))

(define (gsm-ctx-reset! ctx)
  (gsd-ctx-transaction!
   ctx
   (lambda (state history event-bus set-state! set-history!)
     (define old (gsd-runtime-state-mode state))
     (set-state! (struct-copy gsd-runtime-state state [mode 'idle] [wave-executor #f]))
     (set-history! (cons (list old 'idle (current-seconds)) history))
     (ok-result old 'idle))))

;; Auto-routing multi-step transition (ctx-aware)
(define (gsm-ctx-transition-to! ctx target)
  (define current (gsm-ctx-current ctx))
  (cond
    [(eq? current target) (ok-result current target)]
    [else
     (define path (find-transition-path current target))
     (if path
         (for/fold ([result (ok-result current current)]) ([step path])
           (gsm-ctx-transition! ctx step))
         (gsm-ctx-transition! ctx target))]))

;; Snapshot (ctx-aware)
(define (gsm-ctx-snapshot ctx)
  (gsd-ctx-state-snapshot ctx))

;; Wave operations (ctx-aware)
(define (gsm-ctx-wave-executor ctx)
  (gsd-runtime-state-wave-executor (gsd-ctx-state-snapshot ctx)))

(define (gsm-ctx-set-wave-executor! ctx exec)
  (gsd-ctx-state-update! ctx
                         (lambda (state) (struct-copy gsd-runtime-state state [wave-executor exec]))))

(define (gsm-ctx-total-waves ctx)
  (gsd-runtime-state-total-waves (gsd-ctx-state-snapshot ctx)))

(define (gsm-ctx-set-total-waves! ctx n)
  (gsd-ctx-state-update! ctx (lambda (state) (struct-copy gsd-runtime-state state [total-waves n]))))

(define (gsm-ctx-current-wave ctx)
  (gsd-runtime-state-current-wave (gsd-ctx-state-snapshot ctx)))

(define (gsm-ctx-set-current-wave! ctx n)
  (gsd-ctx-state-update! ctx (lambda (state) (struct-copy gsd-runtime-state state [current-wave n]))))

(define (gsm-ctx-completed-waves ctx)
  (gsd-runtime-state-completed-waves (gsd-ctx-state-snapshot ctx)))

(define (gsm-ctx-mark-wave-complete! ctx idx)
  (gsd-ctx-state-update! ctx
                         (lambda (state)
                           (struct-copy gsd-runtime-state
                                        state
                                        [completed-waves
                                         (set-add (gsd-runtime-state-completed-waves state) idx)]))))

(define (gsm-ctx-wave-complete? ctx idx)
  (set-member? (gsm-ctx-completed-waves ctx) idx))

(define (gsm-ctx-next-pending-wave ctx)
  (compute-next-pending-wave (gsm-ctx-total-waves ctx) (gsm-ctx-completed-waves ctx)))

;; State restore (ctx-aware)
(define (gsm-ctx-state-restore! ctx snapshot)
  (gsd-ctx-set-state! ctx snapshot))
