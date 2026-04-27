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
         racket/match
         racket/set)

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
;; State storage (semaphore-protected)
;; ============================================================

(define gsm-sem (make-semaphore 1))
;; State is a hash: 'mode, 'wave-executor, 'total-waves, 'current-wave, 'completed-waves
(define gsm-state-box
  (box (hasheq 'mode 'idle 'wave-executor #f 'total-waves 0 'current-wave 0 'completed-waves (set))))
(define gsm-history-box (box '()))

;; ============================================================
;; Core API
;; ============================================================

(define (gsm-current)
  (call-with-semaphore gsm-sem (lambda () (hash-ref (unbox gsm-state-box) 'mode))))

(define (gsm-history)
  (call-with-semaphore gsm-sem (lambda () (reverse (unbox gsm-history-box)))))

(define (gsm-transition! target)
  (call-with-semaphore
   gsm-sem
   (lambda ()
     (define current (hash-ref (unbox gsm-state-box) 'mode))
     (cond
       [(not (gsm-state? target)) (err-result (format "invalid state: ~a" target) current target)]
       [(valid-transition? current target)
        ;; Clear executor when leaving executing mode
        (define state (unbox gsm-state-box))
        (define state*
          (if (and (eq? current 'executing) (not (eq? target 'executing)))
              (hash-set state 'wave-executor #f)
              state))
        (set-box! gsm-state-box (hash-set state* 'mode target))
        (set-box! gsm-history-box
                  (cons (list current target (current-seconds)) (unbox gsm-history-box)))
        (ok-result current target)]
       [else
        (err-result
         (format "invalid transition: ~a → ~a (valid: ~a)" current target (valid-targets current))
         current
         target)]))))

(define (gsm-reset!)
  (call-with-semaphore
   gsm-sem
   (lambda ()
     (define old (hash-ref (unbox gsm-state-box) 'mode))
     (set-box! gsm-state-box (hash-set* (unbox gsm-state-box) 'mode 'idle 'wave-executor #f))
     (set-box! gsm-history-box (cons (list old 'idle (current-seconds)) (unbox gsm-history-box)))
     (ok-result old 'idle))))

(define (reset-gsm!)
  (call-with-semaphore
   gsm-sem
   (lambda ()
     (set-box!
      gsm-state-box
      (hasheq 'mode 'idle 'wave-executor #f 'total-waves 0 'current-wave 0 'completed-waves (set)))
     (set-box! gsm-history-box '())
     (void))))

(define (gsm-valid-next-states)
  (define current (gsm-current))
  (valid-targets current))

(define (gsm-snapshot)
  (call-with-semaphore gsm-sem (lambda () (unbox gsm-state-box))))

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
;; Wave state accessors (F4: consolidated from gsd-planning-state)
;; ============================================================

(define (gsm-wave-executor)
  (call-with-semaphore gsm-sem (lambda () (hash-ref (unbox gsm-state-box) 'wave-executor))))

(define (gsm-set-wave-executor! exec)
  (call-with-semaphore
   gsm-sem
   (lambda () (set-box! gsm-state-box (hash-set (unbox gsm-state-box) 'wave-executor exec)))))

(define (gsm-total-waves)
  (call-with-semaphore gsm-sem (lambda () (hash-ref (unbox gsm-state-box) 'total-waves))))

(define (gsm-set-total-waves! n)
  (call-with-semaphore gsm-sem
                       (lambda ()
                         (set-box! gsm-state-box (hash-set (unbox gsm-state-box) 'total-waves n)))))

(define (gsm-current-wave)
  (call-with-semaphore gsm-sem (lambda () (hash-ref (unbox gsm-state-box) 'current-wave))))

(define (gsm-set-current-wave! n)
  (call-with-semaphore gsm-sem
                       (lambda ()
                         (set-box! gsm-state-box (hash-set (unbox gsm-state-box) 'current-wave n)))))

(define (gsm-completed-waves)
  (call-with-semaphore gsm-sem (lambda () (hash-ref (unbox gsm-state-box) 'completed-waves))))

(define (gsm-mark-wave-complete! idx)
  (call-with-semaphore gsm-sem
                       (lambda ()
                         (define state (unbox gsm-state-box))
                         (define completed (hash-ref state 'completed-waves))
                         (set-box! gsm-state-box
                                   (hash-set state 'completed-waves (set-add completed idx))))))

(define (gsm-wave-complete? idx)
  (call-with-semaphore gsm-sem
                       (lambda ()
                         (set-member? (hash-ref (unbox gsm-state-box) 'completed-waves) idx))))

(define (gsm-next-pending-wave)
  (call-with-semaphore gsm-sem
                       (lambda ()
                         (define tw (hash-ref (unbox gsm-state-box) 'total-waves))
                         (define cw (hash-ref (unbox gsm-state-box) 'completed-waves))
                         (for/first ([i (in-range tw)]
                                     #:when (not (set-member? cw i)))
                           i))))
