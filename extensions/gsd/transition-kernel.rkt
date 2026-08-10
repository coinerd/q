#lang racket/base

;; extensions/gsd/transition-kernel.rkt — Pure GSD transition kernel (W1 v0.99.89)
;;
;; v0.99.89 W1: Pure transition kernel extracted from transition-logic.rkt.
;; The kernel owns ALL transition computation and operates ONLY on neutral
;; GSD-domain data. It has no knowledge of gsd-runtime-state, filesystem,
;; GitHub, runtime services or the event bus — by construction (enforced by
;; the purity fitness test in tests/test-transition-kernel.rkt).
;;
;; Neutral domain data:
;;   gsd-transition-state (mode total-waves current-wave completed-waves)
;;   — a plain projection of the runtime aggregate containing exactly the
;;   fields the transition logic needs. The runtime facade adapts the
;;   gsd-runtime-state struct to/from this neutral view.
;;
;; Boundary contract:
;;   INPUTS:  gsd-transition-state structs, symbols, integers, sets
;;   OUTPUTS: ok-result/err-result structs, gsd-transition-state structs, lists
;;   EFFECTS: None — pure, deterministic, safe to call from any context.
;;            Repeated application of the same transition to the same input
;;            yields identical results (idempotent; pinned by tests).

(require racket/match
         racket/set)

;; ============================================================
;; States and transitions
;; ============================================================

(provide GSD-STATES
         TRANSITIONS
         TRANSITIONS-FLAT
         GSD-TERMINAL-STATES
         gsm-state?
         terminal-state?
         campaign-complete?)

;; The five GSD lifecycle states (names are frozen — the facade and all
;; consumers rely on them; W1 must not rename any state).
(define GSD-STATES '(idle exploring plan-written executing verifying))

(define (gsm-state? v)
  (and (symbol? v) (memq v GSD-STATES) #t))

;; Terminal states of a GSD lifecycle.
;;   verifying — all waves complete, awaiting the /done archive (the campaign
;;               path ends here; golden-trace finding #4).
;;   idle      — post-close state (archive resets the FSM to idle).
;; A terminal state has no productive outgoing transition.
(define GSD-TERMINAL-STATES '(verifying idle))

(define (terminal-state? s)
  (and (gsm-state? s) (memq s GSD-TERMINAL-STATES) #t))

;; Pure terminal condition: a campaign is complete iff it has at least one
;; wave and every wave index is in the completed set. This is the pure
;; precondition of the /done milestone-close path (archive.rkt's
;; all-waves-complete? is the filesystem-side equivalent).
(define (campaign-complete? total-waves completed-set)
  (and (exact-nonnegative-integer? total-waves)
       (> total-waves 0)
       (set? completed-set)
       (= (set-count completed-set) total-waves)))

;; L-09: Transition table design note.
;; This table is intentionally simple: plain (from . to) pairs with no guards,
;; no actions, no conditions. This keeps the FSM easy to reason about and test.
;; If the GSD state machine grows more complex (e.g., conditional transitions,
;; entry/exit actions), the table should be enriched with a proper FSM library.
;; Current design is sufficient for the 5-state GSD lifecycle.
;;
;; MAS Schritt 1 Integration Point:
;; The executing→verifying transition ((executing . verify) . verifying)
;; is where the verifier agent role (agent/roles/verifier.rkt) will be
;; activated in Schritt 2. The verifier role has '(read-only) capability
;; and will review wave results before transitioning to 'idle.
;; Currently this transition is triggered by the GSD executor;
;; in Schritt 2 it will route through the supervisor dispatch.
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
;; Neutral domain state
;; ============================================================

(provide gsd-transition-state
         gsd-transition-state?
         make-gsd-transition-state
         gts-mode
         gts-total-waves
         gts-current-wave
         gts-completed-waves)

;; Short accessor aliases (the struct's canonical accessors are
;; gsd-transition-state-*; gts-* keeps kernel call sites terse).
(define (gts-mode s)
  (gsd-transition-state-mode s))
(define (gts-total-waves s)
  (gsd-transition-state-total-waves s))
(define (gts-current-wave s)
  (gsd-transition-state-current-wave s))
(define (gts-completed-waves s)
  (gsd-transition-state-completed-waves s))

;; Neutral projection of the runtime GSD state aggregate. Contains exactly
;; the fields the transition logic reads: mode, wave counters, completed set.
;; Deliberately excludes wave-executor, plan-path, pinned-dir, edit-limit
;; and transition-history — those are runtime concerns the facade owns.
(struct gsd-transition-state
        (mode ; symbol ∈ GSD-STATES
         total-waves ; non-negative integer
         current-wave ; non-negative integer
         completed-waves ; set of exact non-negative integers
         )
  #:transparent)

(define (make-gsd-transition-state mode [total-waves 0] [current-wave 0] [completed-waves (set)])
  (gsd-transition-state mode total-waves current-wave completed-waves))

;; ============================================================
;; Transition result types
;; ============================================================

(provide ok-result
         ok-result?
         ok-result-from
         ok-result-to
         err-result
         err-result?
         err-result-reason
         err-result-from
         err-result-attempted
         ok?
         ok-from
         ok-to
         err?
         err-reason)

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
;; Pure transition predicates and functions
;; ============================================================

(provide valid-transition?
         valid-targets
         find-transition-path
         compute-next-state
         transition-idempotent?
         check-transition-invariants
         compute-next-pending-wave)

;; Check if a transition is valid given current state, target, and optional event.
(define (valid-transition? from to [event #f])
  (or (and (eq? from 'idle) (eq? to 'idle))
      (for/or ([t TRANSITIONS])
        (and (eq? (caar t) from) (eq? (cdr t) to) (or (not event) (eq? (cdar t) event))))))

;; List valid target states from a given state.
(define (valid-targets from)
  (for/list ([t TRANSITIONS]
             #:when (eq? (caar t) from))
    (cdr t)))

;; BFS path finder for multi-step transitions.
;; Returns list of states to visit (excluding 'from, including 'to) or #f.
;; Same-state requests are successful zero-hop paths and return '(), which is
;; intentionally distinguishable from #f (no path exists).
(define (find-transition-path from to)
  (define visited (make-hash))
  (define q (list (cons from '())))
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

;; Pure transition kernel (Finding 3.1.3) — neutral-domain version.
;; Computes the next neutral state without side effects.
;; Returns (values ok-result-or-err-result gsd-transition-state?).
;; Note: executor management is NOT part of the kernel — the runtime facade
;; applies its executor-clearing policy after delegating here.
(define (compute-next-state current-state target #:event [event #f])
  (define current (gts-mode current-state))
  (cond
    [(not (gsm-state? target))
     (values (err-result (format "invalid state: ~a" target) current target) current-state)]
    [(valid-transition? current target event)
     (values (ok-result current target)
             (struct-copy gsd-transition-state current-state [mode target]))]
    [else
     (values
      (err-result
       (format "invalid transition: ~a → ~a (valid: ~a)" current target (valid-targets current))
       current
       target)
      current-state)]))

;; Idempotency guard: applying the same transition twice to the same input
;; state yields the same result and the same next state (pure determinism).
;; Returns #t when the second application reproduces the first exactly.
(define (transition-idempotent? current-state target #:event [event #f])
  (define-values (r1 s1) (compute-next-state current-state target #:event event))
  (define-values (r2 s2) (compute-next-state s1 target #:event event))
  ;; Applying a successful transition twice: the second application either
  ;; finds the same transition from the new state (self-loop / re-trigger) or
  ;; is rejected — either way the pair (result, next-state) is reproducible.
  ;; We pin the strong property on the result type and the stable fields:
  ;; an ok result must yield an identical next state when re-applied to the
  ;; original input; an err result must return the input unchanged.
  (if (ok? r1)
      (equal? s2 (struct-copy gsd-transition-state current-state [mode (gts-mode s1)]))
      (and (equal? r2 r1) (equal? s2 s1))))

;; ============================================================
;; Pure invariant checker (preconditions)
;; ============================================================

;; Returns (values ok? error-message-or-#f).
;; Checks structural invariants of the neutral transition state.
;; The runtime-only executor-presence rule lives in the facade.
(define (check-transition-invariants state)
  (define mode (gts-mode state))
  (define tw (gts-total-waves state))
  (define cw (gts-current-wave state))
  (define completed (gts-completed-waves state))
  (cond
    [(not (gsm-state? mode)) (values #f (format "invalid mode: ~a" mode))]
    [(not (exact-nonnegative-integer? tw)) (values #f (format "total-waves not non-neg-int: ~a" tw))]
    [(not (exact-nonnegative-integer? cw)) (values #f (format "current-wave not non-neg-int: ~a" cw))]
    [(> cw tw) (values #f (format "current-wave (~a) > total-waves (~a)" cw tw))]
    [(not (set? completed)) (values #f (format "completed-waves not a set: ~a" completed))]
    [(not (for/and ([idx (in-set completed)])
            (and (exact-nonnegative-integer? idx) (< idx tw))))
     (values #f (format "completed-waves contains invalid indices: ~a" completed))]
    [else (values #t #f)]))

;; ============================================================
;; Pure wave computation
;; ============================================================

;; Compute the next pending (incomplete) wave index.
;; Returns #f if all waves are completed.
(define (compute-next-pending-wave total-waves completed-set)
  (for/first ([i (in-range total-waves)]
              #:when (not (set-member? completed-set i)))
    i))
