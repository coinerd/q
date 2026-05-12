#lang racket/base

;; agent/loop-fsm.rkt — Agent turn FSM states (R-06, R-07)
;; STABILITY: evolving
;;
;; Defines FSM states for a single agent turn. Each phase of the turn
;; is a named state with explicit transitions. This provides observability
;; and testability for the turn lifecycle.

;; Turn states
(provide turn-state?
         turn-state
         turn-state-emit-start
         turn-state-build-context
         turn-state-pre-hook
         turn-state-stream
         turn-state-post-hook
         turn-state-complete
         turn-state-blocked
         turn-state->symbol

         ;; Turn events
         turn-event?
         turn-event
         turn-event-start
         turn-event-context-built
         turn-event-hook-pass
         turn-event-hook-block
         turn-event-stream-complete
         turn-event-stream-cancel
         turn-event-post-hook-done
         turn-event->symbol

         ;; Transition table + lookup
         TURN-TRANSITIONS
         next-turn-state
         valid-turn-transition?)

(require racket/match)

;; ── States ──

(struct turn-state (name) #:transparent)

(define turn-state-emit-start (turn-state 'emit-start))
(define turn-state-build-context (turn-state 'build-context))
(define turn-state-pre-hook (turn-state 'pre-hook))
(define turn-state-stream (turn-state 'stream))
(define turn-state-post-hook (turn-state 'post-hook))
(define turn-state-complete (turn-state 'complete))
(define turn-state-blocked (turn-state 'blocked))

(define (turn-state->symbol s)
  (turn-state-name s))

;; ── Events ──

(struct turn-event (name) #:transparent)

(define turn-event-start (turn-event 'start))
(define turn-event-context-built (turn-event 'context-built))
(define turn-event-hook-pass (turn-event 'hook-pass))
(define turn-event-hook-block (turn-event 'hook-block))
(define turn-event-stream-complete (turn-event 'stream-complete))
(define turn-event-stream-cancel (turn-event 'stream-cancel))
(define turn-event-post-hook-done (turn-event 'post-hook-done))

(define (turn-event->symbol e)
  (turn-event-name e))

;; ── Transition table ──

(define TURN-TRANSITIONS
  ;; emit-start -> build-context
  ;; build-context -> pre-hook
  '([(emit-start . start) . build-context] [(build-context . context-built) . pre-hook]
                                           ;; pre-hook -> stream (hook passes)
                                           [(pre-hook . hook-pass) . stream]
                                           ;; pre-hook -> blocked (hook blocks)
                                           [(pre-hook . hook-block) . blocked]
                                           ;; stream -> post-hook (normal completion)
                                           [(stream . stream-complete) . post-hook]
                                           ;; stream -> complete (cancelled)
                                           [(stream . stream-cancel) . complete]
                                           ;; post-hook -> complete
                                           [(post-hook . post-hook-done) . complete]
                                           ;; Terminal states
                                           [(complete . start) . complete]
                                           [(blocked . start) . blocked]))

;; ── Lookup ──

(define (find-turn-transition state-sym event-sym)
  (for/or ([entry (in-list TURN-TRANSITIONS)])
    (match entry
      [`((,s . ,e) . ,next) (and (eq? s state-sym) (eq? e event-sym) next)]
      [_ #f])))

(define (next-turn-state state event)
  (define state-sym (turn-state->symbol state))
  (define event-sym (turn-event->symbol event))
  (define next-sym (find-turn-transition state-sym event-sym))
  (unless next-sym
    (error 'next-turn-state "invalid turn FSM transition: (~a, ~a)" state-sym event-sym))
  (case next-sym
    [(emit-start) turn-state-emit-start]
    [(build-context) turn-state-build-context]
    [(pre-hook) turn-state-pre-hook]
    [(stream) turn-state-stream]
    [(post-hook) turn-state-post-hook]
    [(complete) turn-state-complete]
    [(blocked) turn-state-blocked]
    [else (error 'next-turn-state "unknown state: ~a" next-sym)]))

(define (valid-turn-transition? state event)
  (define state-sym (turn-state->symbol state))
  (define event-sym (turn-event->symbol event))
  (and (find-turn-transition state-sym event-sym) #t))
