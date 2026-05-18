#lang racket/base

;; agent/loop-fsm.rkt — Agent turn FSM states (R-06, R-07)
;; STABILITY: evolving
;;
;; Defines FSM states for a single agent turn using define-fsm-machine
;; from util/fsm.rkt. Backward-compatible exports maintained.

(require (only-in "../util/fsm.rkt"
                  define-fsm-machine
                  fsm-state
                  fsm-state-name
                  fsm-event
                  fsm-event-name
                  fsm?
                  fsm-states
                  fsm-transitions
                  fsm-lookup
                  fsm-valid-transition?))

;; ── Machine definition ──

(define-fsm-machine turn
  #:states (emit-start build-context pre-hook stream post-hook complete blocked)
  #:events (start context-built hook-pass hook-block stream-complete stream-cancel post-hook-done msg-hook-block)
  #:transitions
  [(emit-start -> build-context) start]
  [(build-context -> pre-hook) context-built]
  [(pre-hook -> stream) hook-pass]
  [(pre-hook -> blocked) hook-block]
  [(stream -> post-hook) stream-complete]
  [(stream -> blocked) msg-hook-block]
  [(stream -> complete) stream-cancel]
  [(post-hook -> complete) post-hook-done]
  [(complete -> complete) start]
  [(blocked -> blocked) start])

;; ── Backward-compatible exports ──

;; State singletons (old names: turn-state-<name>)
(define turn-state-emit-start turn-emit-start)
(define turn-state-build-context turn-build-context)
(define turn-state-pre-hook turn-pre-hook)
(define turn-state-stream turn-stream)
(define turn-state-post-hook turn-post-hook)
(define turn-state-complete turn-complete)
(define turn-state-blocked turn-blocked)

;; Event singletons (old names: turn-event-<name>)
(define turn-event-start turn-start)
(define turn-event-context-built turn-context-built)
(define turn-event-hook-pass turn-hook-pass)
(define turn-event-hook-block turn-hook-block)
(define turn-event-stream-complete turn-stream-complete)
(define turn-event-stream-cancel turn-stream-cancel)
(define turn-event-post-hook-done turn-post-hook-done)
(define turn-event-msg-hook-block turn-msg-hook-block)

;; State predicate: turn-state? now checks fsm-state? with turn state names
;; (this is what the macro generates — compatible with old struct predicate)
;; Event predicate: turn-event? — same

;; Converters
(define (turn-state->symbol s) (fsm-state-name s))
(define (turn-event->symbol e) (fsm-event-name e))

;; Transition table (backward compat — derived from machine)
(define TURN-TRANSITIONS
  (fsm-transitions turn-machine))

;; Next-state with error (backward compat)
(define (next-turn-state state event)
  (define result (turn-next-state state event))
  (unless result
    (error 'next-turn-state
           "invalid turn FSM transition: (~a, ~a)"
           (fsm-state-name state)
           (fsm-event-name event)))
  result)

;; Valid transition check (backward compat)
(define (valid-turn-transition? state event)
  (turn-valid-transition? state event))

;; FSM state parameter
(define current-turn-fsm-state (make-parameter turn-state-emit-start))

;; ── Provides ──
(provide turn-state?
         turn-state-emit-start
         turn-state-build-context
         turn-state-pre-hook
         turn-state-stream
         turn-state-post-hook
         turn-state-complete
         turn-state-blocked
         turn-state->symbol

         turn-event?
         turn-event-start
         turn-event-context-built
         turn-event-hook-pass
         turn-event-hook-block
         turn-event-stream-complete
         turn-event-stream-cancel
         turn-event-post-hook-done
         turn-event-msg-hook-block
         turn-event->symbol

         current-turn-fsm-state

         TURN-TRANSITIONS
         turn-machine
         next-turn-state
         valid-turn-transition?)
