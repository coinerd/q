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

(require racket/contract
         racket/match
         racket/set
         "runtime-state-types.rkt"
         (only-in "policy.rkt" blocked-tools-for gsd-decide-action policy-allowed?)
         (only-in "events.rkt" emit-gsd-event! current-gsd-correlation-id)
         (only-in "event-structs.rkt"
                  make-gsd-transition-attempted-event
                  make-gsd-transition-succeeded-event
                  make-gsd-transition-failed-event)
         (only-in "session-state.rkt"
                  current-gsd-history
                  set-gsd-history!
                  gsd-state-snapshot
                  gsd-history-snapshot
                  gsd-state-update!
                  with-gsd-lock
                  gsd-default-ctx
                  gsd-session-ctx-state-box
                  gsd-session-ctx-history-box
                  gsd-session-ctx?
                  gsd-ctx-state
                  gsd-ctx-set-state!
                  gsd-ctx-state-snapshot
                  gsd-ctx-state-update!
                  gsd-ctx-history
                  gsd-ctx-set-history!
                  gsd-ctx-history-update!
                  gsd-session-ctx-sem))

;; States
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
         ok?
         ok-from
         ok-to
         err?
         err-reason
         ;; Wave gate
         gsd-wave-gate-counter
         gsd-wave-gate-interval
         gsd-wave-gate-increment!)

;; Data constants (plain)
(provide GSD-STATES
         TRANSITIONS
         TRANSITIONS-FLAT
         ;; Functions (contracted)
         (contract-out
          [gsm-state? (-> any/c boolean?)]
          [make-initial-gsd-state (-> gsd-runtime-state?)]
          [gsm-current (-> symbol?)]
          [gsm-transition! (->* (symbol?) (#:event (or/c symbol? #f)) (or/c ok-result? err-result?))]
          [gsm-transition-to! (-> symbol? (or/c ok-result? err-result?))]
          [gsm-reset! (-> (or/c ok-result? err-result?))]
          [compute-next-gsm-state
           (->* (gsd-runtime-state? symbol?)
                (#:event (or/c symbol? #f))
                (values (or/c ok-result? err-result?) gsd-runtime-state?))]
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
          [gsm-ctx-valid-next-states (-> gsd-session-ctx? (listof symbol?))]))

;; ============================================================
;; States and transitions
;; ============================================================

(define GSD-STATES '(idle exploring plan-written executing verifying))

(define (gsm-state? v)
  (and (symbol? v) (memq v GSD-STATES) #t))

;; L-09: Transition table design note.
;; This table is intentionally simple: plain (from . to) pairs with no guards,
;; no actions, no conditions. This keeps the FSM easy to reason about and test.
;; If the GSD state machine grows more complex (e.g., conditional transitions,
;; entry/exit actions), the table should be enriched with a proper FSM library.
;; Current design is sufficient for the 5-state GSD lifecycle.
(define TRANSITIONS
  ;; Enriched transition table (F4): ((from . event) . to)
  ;; Events name the trigger for each transition, enabling event-driven dispatch.
  '(((idle . explore) . exploring) ((exploring . plan) . plan-written)
                                   ((exploring . cancel) . idle)
                                   ((plan-written . execute) . executing)
                                   ((plan-written . cancel) . idle)
                                   ((executing . verify) . verifying)
                                   ((executing . cancel) . idle)
                                   ((verifying . done) . idle)
                                   ((verifying . rework) . executing)))

;; Legacy: flat transition pairs for backward compatibility
;; (derived from enriched table)
(define TRANSITIONS-FLAT
  (for/list ([t TRANSITIONS])
    (cons (caar t) (cdr t))))

;; ============================================================
;; Transition result types
;; ============================================================

;; Successful transition
(struct ok-result (from to) #:transparent)
;; Failed transition
(struct err-result (reason from attempted) #:transparent)

(define (ok? r)
  (ok-result? r))
(define (ok-from r)
  (ok-result-from r))
(define (ok-to r)
  (ok-result-to r))
(define (err? r)
  (err-result? r))
(define (err-reason r)
  (err-result-reason r))

;; ============================================================
;; Core API — now backed by session-state parameters
;; ============================================================

(define (gsm-current)
  (gsd-runtime-state-mode (gsd-state-snapshot)))

(define (gsm-history)
  (gsd-history-snapshot))

;; Pure transition kernel (Finding 3.1.3)
;; Compute next state without side effects.
;; Returns (or/c ok-result? err-result?).
(define (compute-next-gsm-state current-state target #:event [event #f])
  (define current (gsd-runtime-state-mode current-state))
  (cond
    [(not (gsm-state? target))
     (values (err-result (format "invalid state: ~a" target) current target) current-state)]
    [(valid-transition? current target event)
     ;; Clear executor when leaving executing mode
     (define state*
       (if (and (eq? current 'executing) (not (eq? target 'executing)))
           (struct-copy gsd-runtime-state current-state [wave-executor #f])
           current-state))
     (define new-state (struct-copy gsd-runtime-state state* [mode target]))
     (values (ok-result current target) new-state)]
    [else
     (values
      (err-result
       (format "invalid transition: ~a → ~a (valid: ~a)" current target (valid-targets current))
       current
       target)
      current-state)]))

(define (gsm-transition! target #:event [event #f])
  ;; R-01: Direct box access inside with-gsd-lock to avoid nested semaphore deadlock.
  ;; gsd-state-snapshot/gsd-state-update! call with-gsd-lock internally,
  ;; causing deadlock on the same non-reentrant semaphore.
  (with-gsd-lock
   (lambda ()
     (define state (unbox (gsd-session-ctx-state-box gsd-default-ctx)))
     (define current (gsd-runtime-state-mode state))
     (emit-gsd-event!
      'gsd.transition.attempted
      (make-gsd-transition-attempted-event #:session-id "" #:turn-id 0 #:from current #:to target))
     (define-values (result new-state) (compute-next-gsm-state state target #:event event))
     (cond
       [(ok? result)
        (define new-mode (gsd-runtime-state-mode new-state))
        (set-box! (gsd-session-ctx-state-box gsd-default-ctx) new-state)
        (set-gsd-history! (cons (list current new-mode (current-seconds)) (current-gsd-history)))
        (emit-gsd-event! 'gsd.transition.succeeded
                         (make-gsd-transition-succeeded-event #:session-id ""
                                                              #:turn-id 0
                                                              #:from current
                                                              #:to new-mode))
        result]
       [else
        (emit-gsd-event! 'gsd.transition.failed
                         (make-gsd-transition-failed-event
                          #:session-id ""
                          #:turn-id 0
                          #:from current
                          #:to target
                          #:reason (format "invalid: ~a -> ~a" current target)))
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
  ;; R-01: Direct box access inside with-gsd-lock to avoid nested semaphore deadlock.
  (with-gsd-lock (lambda ()
                   (define state (unbox (gsd-session-ctx-state-box gsd-default-ctx)))
                   (define old (gsd-runtime-state-mode state))
                   (set-box! (gsd-session-ctx-state-box gsd-default-ctx)
                             (struct-copy gsd-runtime-state state [mode 'idle] [wave-executor #f]))
                   (set-gsd-history! (cons (list old 'idle (current-seconds)) (current-gsd-history)))
                   (ok-result old 'idle))))

(define (reset-gsm!)
  ;; R-01: Use set-gsd-state!/set-gsd-history! directly inside with-gsd-lock.
  (with-gsd-lock (lambda ()
                   (set-box! (gsd-session-ctx-state-box gsd-default-ctx) (make-initial-gsd-state))
                   (set-gsd-history! '())
                   (void))))

(define (gsm-valid-next-states)
  (define current (gsm-current))
  (valid-targets current))

(define (gsm-snapshot)
  (gsd-state-snapshot))

;; ============================================================
;; Tool access
;; ============================================================

(define (gsm-tool-allowed? tool-name)
  (define current (gsm-current))
  (policy-allowed? (gsd-decide-action (hasheq 'mode current 'tool tool-name) 'tool-call)))

;; ============================================================
;; Internal helpers
;; ============================================================

;; BFS path finder for multi-step transitions
(define (find-transition-path from to)
  (define visited (make-hash))
  (define q (list (list from '())))
  (let loop ([q q])
    (cond
      [(null? q) #f]
      [else
       (define node (caar q))
       (define path (cdar q))
       (cond
         [(eq? node to) (reverse path)]
         [(hash-has-key? visited node) (loop (cdr q))]
         [else
          (hash-set! visited node #t)
          (define next-steps
            (for/list ([t TRANSITIONS]
                       #:when (eq? (caar t) node)
                       #:unless (hash-has-key? visited (cdr t)))
              (cdr t)))
          (define new-q
            (append (cdr q)
                    (for/list ([s next-steps])
                      (cons s (cons s path)))))
          (loop new-q)])])))

(define (valid-transition? from to [event #f])
  (or (and (eq? from 'idle) (eq? to 'idle))
      (for/or ([t TRANSITIONS])
        (and (eq? (caar t) from) (eq? (cdr t) to) (or (not event) (eq? (cdar t) event))))))

(define (valid-targets from)
  (for/list ([t TRANSITIONS]
             #:when (eq? (caar t) from))
    (cdr t)))

;; ============================================================
;; Wave state accessors — now backed by session-state parameters
;; ============================================================

(define (gsm-wave-executor)
  (gsd-runtime-state-wave-executor (gsd-state-snapshot)))

(define (gsm-set-wave-executor! exec)
  (gsd-state-update! (lambda (state) (struct-copy gsd-runtime-state state [wave-executor exec]))))

(define (gsm-total-waves)
  (gsd-runtime-state-total-waves (gsd-state-snapshot)))

(define (gsm-set-total-waves! n)
  (gsd-state-update! (lambda (state) (struct-copy gsd-runtime-state state [total-waves n]))))

(define (gsm-current-wave)
  (gsd-runtime-state-current-wave (gsd-state-snapshot)))

(define (gsm-set-current-wave! n)
  (gsd-state-update! (lambda (state) (struct-copy gsd-runtime-state state [current-wave n]))))

(define (gsm-completed-waves)
  (gsd-runtime-state-completed-waves (gsd-state-snapshot)))

(define (gsm-mark-wave-complete! idx)
  (gsd-state-update! (lambda (state)
                       (struct-copy gsd-runtime-state
                                    state
                                    [completed-waves
                                     (set-add (gsd-runtime-state-completed-waves state) idx)]))))

(define (gsm-wave-complete? idx)
  (set-member? (gsd-runtime-state-completed-waves (gsd-state-snapshot)) idx))

(define (gsm-next-pending-wave)
  (define state (gsd-state-snapshot))
  (define tw (gsd-runtime-state-total-waves state))
  (define cw (gsd-runtime-state-completed-waves state))
  (for/first ([i (in-range tw)]
              #:when (not (set-member? cw i)))
    i))

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
;; State invariants (F3 fix: runtime invariant checks)
;; ============================================================

;; Returns (values ok? error-message-or-#f)
;; Checks structural invariants that should hold at all times.
(define (gsd-invariants-hold?)
  (define state (gsd-state-snapshot))
  (define mode (gsd-runtime-state-mode state))
  (define tw (gsd-runtime-state-total-waves state))
  (define cw (gsd-runtime-state-current-wave state))
  (define completed (gsd-runtime-state-completed-waves state))
  (define exec (gsd-runtime-state-wave-executor state))
  (cond
    [(not (gsm-state? mode)) (values #f (format "invalid mode: ~a" mode))]
    [(not (exact-nonnegative-integer? tw)) (values #f (format "total-waves not non-neg-int: ~a" tw))]
    [(not (exact-nonnegative-integer? cw)) (values #f (format "current-wave not non-neg-int: ~a" cw))]
    [(> cw tw) (values #f (format "current-wave (~a) > total-waves (~a)" cw tw))]
    [(not (set? completed)) (values #f (format "completed-waves not a set: ~a" completed))]
    [(not (for/and ([idx (in-set completed)])
            (and (exact-nonnegative-integer? idx) (< idx tw))))
     (values #f (format "completed-waves contains invalid indices: ~a" completed))]
    ;; If in executing/verifying, wave-executor should be set when waves exist
    [(and (memq mode '(executing verifying)) (> tw 0) (not exec))
     (values #f (format "in ~a with ~a waves but no wave-executor" mode tw))]
    [else (values #t #f)]))

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
  (call-with-semaphore
   (gsd-session-ctx-sem ctx)
   (lambda ()
     (define state (unbox (gsd-session-ctx-state-box ctx)))
     (define current (gsd-runtime-state-mode state))
     (emit-gsd-event!
      'gsd.transition.attempted
      (make-gsd-transition-attempted-event #:session-id "" #:turn-id 0 #:from current #:to target))
     (define-values (result new-state) (compute-next-gsm-state state target #:event event))
     (cond
       [(ok? result)
        (define new-mode (gsd-runtime-state-mode new-state))
        (set-box! (gsd-session-ctx-state-box ctx) new-state)
        (set-box! (gsd-session-ctx-history-box ctx)
                  (cons (list current new-mode (current-seconds))
                        (unbox (gsd-session-ctx-history-box ctx))))
        (emit-gsd-event! 'gsd.transition.succeeded
                         (make-gsd-transition-succeeded-event #:session-id ""
                                                              #:turn-id 0
                                                              #:from current
                                                              #:to new-mode))
        result]
       [else
        (emit-gsd-event! 'gsd.transition.failed
                         (make-gsd-transition-failed-event
                          #:session-id ""
                          #:turn-id 0
                          #:from current
                          #:to target
                          #:reason (format "invalid: ~a -> ~a" current target)))
        result]))))

(define (gsm-ctx-reset! ctx)
  (call-with-semaphore
   (gsd-session-ctx-sem ctx)
   (lambda ()
     (define state (unbox (gsd-session-ctx-state-box ctx)))
     (define old (gsd-runtime-state-mode state))
     (set-box! (gsd-session-ctx-state-box ctx)
               (struct-copy gsd-runtime-state state [mode 'idle] [wave-executor #f]))
     (set-box! (gsd-session-ctx-history-box ctx)
               (cons (list old 'idle (current-seconds)) (unbox (gsd-session-ctx-history-box ctx))))
     (ok-result old 'idle))))
