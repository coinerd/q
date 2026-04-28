#lang racket/base

;; extensions/gsd/state-machine.rkt — GSD State Machine
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
         "session-state.rkt")

;; States
(provide gsm-state?
         GSD-STATES
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
         gsm-next-pending-wave)

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

;; Tool permissions per state.
;; States not listed allow all tools.
(define BLOCKED-TOOLS
  '((plan-written . ("edit" "write" "bash")) (executing . ("planning-write"))
                                             (verifying . ("edit" "write" "bash" "planning-write"))))

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
  (hash-ref (gsd-state-snapshot) 'mode))

(define (gsm-history)
  (gsd-history-snapshot))

(define (gsm-transition! target)
  (call-with-semaphore
   gsd-state-sem
   (lambda ()
     (define state (current-gsd-state))
     (define current (hash-ref state 'mode))
     (cond
       [(not (gsm-state? target)) (err-result (format "invalid state: ~a" target) current target)]
       [(valid-transition? current target)
        ;; Clear executor when leaving executing mode
        (define state*
          (if (and (eq? current 'executing) (not (eq? target 'executing)))
              (hash-set state 'wave-executor #f)
              state))
        (current-gsd-state (hash-set state* 'mode target))
        (current-gsd-history (cons (list current target (current-seconds)) (current-gsd-history)))
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
     (define old (hash-ref (current-gsd-state) 'mode))
     (current-gsd-state (hash-set* (current-gsd-state) 'mode 'idle 'wave-executor #f))
     (current-gsd-history (cons (list old 'idle (current-seconds)) (current-gsd-history)))
     (ok-result old 'idle))))

(define (reset-gsm!)
  (call-with-semaphore
   gsd-state-sem
   (lambda ()
     (current-gsd-state
      (hasheq 'mode 'idle 'wave-executor #f 'total-waves 0 'current-wave 0 'completed-waves (set)))
     (current-gsd-history '())
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
  (define blocked (assq current BLOCKED-TOOLS))
  (if blocked
      (not (member tool-name (cdr blocked)))
      #t))

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
  (hash-ref (gsd-state-snapshot) 'wave-executor))

(define (gsm-set-wave-executor! exec)
  (gsd-state-update! (lambda (state) (hash-set state 'wave-executor exec))))

(define (gsm-total-waves)
  (hash-ref (gsd-state-snapshot) 'total-waves))

(define (gsm-set-total-waves! n)
  (gsd-state-update! (lambda (state) (hash-set state 'total-waves n))))

(define (gsm-current-wave)
  (hash-ref (gsd-state-snapshot) 'current-wave))

(define (gsm-set-current-wave! n)
  (gsd-state-update! (lambda (state) (hash-set state 'current-wave n))))

(define (gsm-completed-waves)
  (hash-ref (gsd-state-snapshot) 'completed-waves))

(define (gsm-mark-wave-complete! idx)
  (gsd-state-update! (lambda (state)
                       (define completed (hash-ref state 'completed-waves))
                       (hash-set state 'completed-waves (set-add completed idx)))))

(define (gsm-wave-complete? idx)
  (set-member? (hash-ref (gsd-state-snapshot) 'completed-waves) idx))

(define (gsm-next-pending-wave)
  (define state (gsd-state-snapshot))
  (define tw (hash-ref state 'total-waves))
  (define cw (hash-ref state 'completed-waves))
  (for/first ([i (in-range tw)]
              #:when (not (set-member? cw i)))
    i))
