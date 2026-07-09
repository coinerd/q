#lang racket/base

;; extensions/gsd/transition-logic.rkt — Pure GSD state machine transition logic
;;
;; W5 v0.99.35: Extracted from state-machine.rkt to separate pure transition
;; computation from mutable session-state interactions and event-bus side-effects.
;;
;; All functions in this module are pure (no I/O, no mutation of external state,
;; no parameters, no event bus). They operate solely on the gsd-runtime-state
;; struct and the static transition table.
;;
;; Boundary contract:
;;   INPUTS:  gsd-runtime-state structs, symbols, integers, sets
;;   OUTPUTS: ok-result/err-result structs, gsd-runtime-state structs, lists
;;   EFFECTS: None — safe to call from any context

(require racket/match
         racket/set
         "runtime-state-types.rkt")

;; Result type structs
;; W3 v0.99.36: Explicit exports replace struct-out for ok-result and err-result.
;; This documents the exact public surface and prevents accidental exposure
;; of internal fields when struct definitions evolve.
(provide ok-result
         ok-result?
         ok-result-from
         ok-result-to
         err-result
         err-result?
         err-result-reason
         err-result-from
         err-result-attempted
         ;; Result type wrapper functions
         ok?
         ok-from
         ok-to
         err?
         err-reason
         ;; State/transition constants
         GSD-STATES
         TRANSITIONS
         TRANSITIONS-FLAT
         ;; Pure predicates
         gsm-state?
         ;; Pure transition logic
         valid-transition?
         valid-targets
         find-transition-path
         compute-next-gsm-state
         ;; Pure invariant checker
         check-state-invariants
         ;; Pure wave computation
         compute-next-pending-wave)

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
;; Pure transition predicates and functions
;; ============================================================

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

;; Pure transition kernel (Finding 3.1.3)
;; Compute next state without side effects.
;; Returns (values (or/c ok-result? err-result?) gsd-runtime-state?).
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

;; ============================================================
;; Pure invariant checker
;; ============================================================

;; Returns (values ok? error-message-or-#f).
;; Checks structural invariants that should hold at all times.
(define (check-state-invariants state)
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
;; Pure wave computation
;; ============================================================

;; Compute the next pending (incomplete) wave index.
;; Returns #f if all waves are completed.
(define (compute-next-pending-wave total-waves completed-set)
  (for/first ([i (in-range total-waves)]
              #:when (not (set-member? completed-set i)))
    i))
