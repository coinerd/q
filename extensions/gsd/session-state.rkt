#lang racket/base

;; extensions/gsd/session-state.rkt — Per-session GSD state parameters (QUAL-02)
;;
;; Provides Racket parameters for GSD session state, enabling per-session
;; isolation via parameterize. Replaces global mutable boxes from
;; gsd-planning-state.rkt and state-machine.rkt.
;;
;; Thread safety: Parameters are thread-local by default. The state hash
;; parameter uses a semaphore for atomic multi-field updates (mode + wave
;; state must transition together).

(require racket/set)

;; ============================================================
;; Per-session state parameters
;; ============================================================

;; Core GSD state hash: mode, wave-executor, total-waves, current-wave, completed-waves
;; This replaces the global gsm-state-box in state-machine.rkt.
(define current-gsd-state
  (make-parameter
   (hasheq 'mode 'idle 'wave-executor #f 'total-waves 0 'current-wave 0 'completed-waves (set))))

;; Convenience accessor: current GSD mode symbol
(define (current-gsd-mode)
  (hash-ref (current-gsd-state) 'mode))

;; Convenience accessor: current wave number
(define (current-wave-number)
  (hash-ref (current-gsd-state) 'current-wave))

;; Convenience accessor: current plan data (from planning state)
(define current-plan-data (make-parameter #f))

;; Planning directory (replaces pinned-dir-box)
(define current-pinned-dir (make-parameter #f))

;; Edit limit (replaces edit-limit-box)
(define current-edit-limit (make-parameter 500))

;; Event bus (replaces gsd-event-bus-box)
(define current-gsd-event-bus (make-parameter #f))

;; State machine history (replaces gsm-history-box)
(define current-gsd-history (make-parameter '()))

;; Semaphore for atomic state transitions
(define gsd-state-sem (make-semaphore 1))

;; ============================================================
;; Atomic state operations
;; ============================================================

;; Read current state atomically
(define (gsd-state-snapshot)
  (call-with-semaphore gsd-state-sem (lambda () (current-gsd-state))))

;; Update state atomically (thunk receives current state, returns new state)
(define (gsd-state-update! update-thunk)
  (call-with-semaphore gsd-state-sem
                       (lambda () (current-gsd-state (update-thunk (current-gsd-state))))))

;; Read history atomically
(define (gsd-history-snapshot)
  (call-with-semaphore gsd-state-sem (lambda () (reverse (current-gsd-history)))))

;; Update history atomically
(define (gsd-history-update! update-thunk)
  (call-with-semaphore gsd-state-sem
                       (lambda () (current-gsd-history (update-thunk (current-gsd-history))))))

;; Reset all session state to defaults
(define (reset-session-state!)
  (call-with-semaphore
   gsd-state-sem
   (lambda ()
     (current-gsd-state
      (hasheq 'mode 'idle 'wave-executor #f 'total-waves 0 'current-wave 0 'completed-waves (set)))
     (current-gsd-history '())))
  (current-pinned-dir #f)
  (current-edit-limit 500)
  (current-gsd-event-bus #f)
  (current-plan-data #f))

;; ============================================================
;; Provide
;; ============================================================

(provide current-gsd-state
         current-gsd-mode
         current-wave-number
         current-plan-data
         current-pinned-dir
         current-edit-limit
         current-gsd-event-bus
         current-gsd-history
         ;; Atomic operations
         gsd-state-sem
         gsd-state-snapshot
         gsd-state-update!
         gsd-history-snapshot
         gsd-history-update!
         reset-session-state!)
