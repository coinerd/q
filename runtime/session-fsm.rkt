#lang racket/base

;; runtime/session-fsm.rkt — Session lifecycle FSM overlay (F5)
;; STABILITY: evolving
;;
;; Defines a session lifecycle FSM using define-fsm-machine.
;; Rather than replacing the boolean fields in agent-session (which has
;; 203+ callsites), this provides a *derived* FSM state computed from
;; the existing flags. The FSM defines the legal state space and
;; transitions, while the boolean flags remain the backing store.
;;
;; Session lifecycle:
;;   created → active → streaming → active → compacting → active →
;;   idle → active (new turn) → ... → terminated
;;
;; Usage:
;;   (session-current-state sess) → fsm-state
;;   (session-valid-lifecycle? sess) → boolean

(require (only-in "../util/fsm.rkt"
                  define-fsm-machine
                  fsm-state
                  fsm-state-name
                  fsm-event
                  fsm-event-name
                  fsm-lookup
                  fsm-valid-transition?)
         (only-in "session-types.rkt"
                  agent-session?
                  agent-session-active?
                  agent-session-compacting?
                  agent-session-prompt-running?))

;; ── Session FSM ──

(define-fsm-machine session-lifecycle
  #:states (created active streaming compacting idle terminated)
  #:events (session-started stream-requested stream-complete
            compaction-needed compaction-complete
            turn-complete new-turn session-ended)
  #:transitions
  [(created -> active) session-started]
  [(active -> streaming) stream-requested]
  [(active -> compacting) compaction-needed]
  [(active -> idle) turn-complete]
  [(streaming -> active) stream-complete]
  [(compacting -> active) compaction-complete]
  [(idle -> active) new-turn]
  [(active -> terminated) session-ended]
  [(streaming -> terminated) session-ended]
  [(compacting -> terminated) session-ended]
  [(idle -> terminated) session-ended])

;; ── Derived state computation ──

;; session-current-state : agent-session? -> fsm-state
;; Computes the FSM state from the session's boolean flags.
(define (session-current-state sess)
  (cond
    [(not (agent-session-active? sess))
     session-lifecycle-terminated]
    [(agent-session-compacting? sess)
     session-lifecycle-compacting]
    [(agent-session-prompt-running? sess)
     session-lifecycle-streaming]
    [else
     session-lifecycle-active]))

;; session-current-state-name : agent-session? -> symbol
(define (session-current-state-name sess)
  (fsm-state-name (session-current-state sess)))

;; session-valid-lifecycle? : agent-session? -> boolean
;; Checks whether the session's current state is valid (known FSM state).
(define (session-valid-lifecycle? sess)
  (session-lifecycle-state? (session-current-state sess)))

;; session-can-transition? : agent-session? symbol -> boolean
;; Whether the session can accept the given event from its current state.
(define (session-can-transition? sess event-sym)
  (fsm-valid-transition? session-lifecycle-machine
                         (session-current-state-name sess)
                         event-sym))

(provide session-lifecycle-machine
         session-lifecycle-state?
         session-lifecycle-event?
         session-lifecycle-terminated
         session-lifecycle-active
         session-lifecycle-streaming
         session-lifecycle-compacting
         session-lifecycle-idle
         session-lifecycle-created
         session-current-state
         session-current-state-name
         session-valid-lifecycle?
         session-can-transition?)
