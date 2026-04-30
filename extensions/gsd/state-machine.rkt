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
         (only-in "session-state.rkt"
                  current-gsd-state
                  set-gsd-state!
                  current-gsd-history
                  set-gsd-history!
                  gsd-state-snapshot
                  gsd-state-update!
                  gsd-state-sem
                  gsd-history-snapshot))

;; States
(provide gsm-state?
         GSD-STATES
         ;; Runtime state struct
         (struct-out gsd-runtime-state)
         make-initial-gsd-state
         ;; Current state query
         gsm-current
         ;; Transition
         gsm-transition!
         gsm-reset!
         ;; Transition result
         ok?
         ok-from
         ok-to
         err?
         err-reason
         ;; Valid transitions query
         gsm-valid-next-states
         ;; Tool access
         gsm-tool-allowed?
         ;; Snapshot / reset
         gsm-snapshot
         reset-gsm!
         ;; History
         gsm-history
         ;; Wave state (F4)
         gsm-wave-executor
         gsm-set-wave-executor!
         gsm-total-waves
         gsm-set-total-waves!
         gsm-current-wave
         gsm-set-current-wave!
         gsm-completed-waves
         gsm-mark-wave-complete!
         gsm-wave-complete?
         gsm-next-pending-wave
         ;; Invariants
         gsd-invariants-hold?)

;; ============================================================
;; States and transitions
;; ============================================================

(define GSD-STATES '(idle exploring plan-written executing verifying))

(define (gsm-state? v)
  (and (symbol? v) (memq v GSD-STATES) #t))

;; Transition table: (from . to) pairs that are valid.
(define TRANSITIONS
  '((idle . exploring) (exploring . plan-written)
                       (exploring . idle)
                       (plan-written . executing)
                       (plan-written . idle)
                       (executing . verifying)
                       (executing . idle)
                       (verifying . idle)
                       (verifying . executing)))

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

(define (gsm-transition! target)
  (call-with-semaphore
   gsd-state-sem
   (lambda ()
     (define state (current-gsd-state))
     (define current (gsd-runtime-state-mode state))
     (cond
       [(not (gsm-state? target)) (err-result (format "invalid state: ~a" target) current target)]
       [(valid-transition? current target)
        ;; Clear executor when leaving executing mode
        (define state*
          (if (and (eq? current 'executing) (not (eq? target 'executing)))
              (struct-copy gsd-runtime-state state [wave-executor #f])
              state))
        (set-gsd-state! (struct-copy gsd-runtime-state state* [mode target]))
        (set-gsd-history! (cons (list current target (current-seconds)) (current-gsd-history)))
        (ok-result current target)]
       [else
        (err-result
         (format "invalid transition: ~a → ~a (valid: ~a)" current target (valid-targets current))
         current
         target)]))))

(define (gsm-reset!)
  (call-with-semaphore
   gsd-state-sem
   (lambda ()
     (define old (gsd-runtime-state-mode (current-gsd-state)))
     (set-gsd-state!
      (struct-copy gsd-runtime-state (current-gsd-state) [mode 'idle] [wave-executor #f]))
     (set-gsd-history! (cons (list old 'idle (current-seconds)) (current-gsd-history)))
     (ok-result old 'idle))))

(define (reset-gsm!)
  (call-with-semaphore gsd-state-sem
                       (lambda ()
                         (set-gsd-state! (make-initial-gsd-state))
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

(define (valid-transition? from to)
  (or (and (eq? from 'idle) (eq? to 'idle))
      (for/or ([t TRANSITIONS])
        (and (eq? (car t) from) (eq? (cdr t) to)))))

(define (valid-targets from)
  (for/list ([t TRANSITIONS]
             #:when (eq? (car t) from))
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
