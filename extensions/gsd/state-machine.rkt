#lang racket/base

;; extensions/gsd/state-machine.rkt — GSD State Machine
;;
;; Wave 0 of v0.21.0: Central state machine with explicit transitions and guards.
;; Replaces scattered mode checks across 4 hook handlers.
;;
;; States: idle → exploring → plan-written → executing → verifying → idle
;;                 ↑              ↓
;;                 └──────────────┘ (re-plan on failure)
;;
;; All state is semaphore-protected for thread safety.

(require racket/contract
         racket/match)

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
         gsm-history)

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
                       (plan-written . exploring)
                       (executing . verifying)
                       (executing . exploring)
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
;; State storage (semaphore-protected)
;; ============================================================

(define gsm-sem (make-semaphore 1))
(define gsm-state-box (box 'idle))
(define gsm-history-box (box '()))

;; ============================================================
;; Core API
;; ============================================================

(define (gsm-current)
  (call-with-semaphore gsm-sem (lambda () (unbox gsm-state-box))))

(define (gsm-history)
  (call-with-semaphore gsm-sem (lambda () (reverse (unbox gsm-history-box)))))

(define (gsm-transition! target)
  (call-with-semaphore
   gsm-sem
   (lambda ()
     (define current (unbox gsm-state-box))
     (cond
       [(not (gsm-state? target)) (err-result (format "invalid state: ~a" target) current target)]
       [(valid-transition? current target)
        (set-box! gsm-state-box target)
        (set-box! gsm-history-box
                  (cons (list current target (current-seconds)) (unbox gsm-history-box)))
        (ok-result current target)]
       [else
        (err-result
         (format "invalid transition: ~a → ~a (valid: ~a)" current target (valid-targets current))
         current
         target)]))))

(define (gsm-reset!)
  (call-with-semaphore gsm-sem
                       (lambda ()
                         (define old (unbox gsm-state-box))
                         (set-box! gsm-state-box 'idle)
                         (set-box! gsm-history-box
                                   (cons (list old 'idle (current-seconds)) (unbox gsm-history-box)))
                         (ok-result old 'idle))))

(define (reset-gsm!)
  (call-with-semaphore gsm-sem
                       (lambda ()
                         (set-box! gsm-state-box 'idle)
                         (set-box! gsm-history-box '())
                         (void))))

(define (gsm-valid-next-states)
  (define current (gsm-current))
  (valid-targets current))

(define (gsm-snapshot)
  (call-with-semaphore
   gsm-sem
   (lambda () (hasheq 'current (unbox gsm-state-box) 'history (reverse (unbox gsm-history-box))))))

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
