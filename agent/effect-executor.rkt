#lang racket/base

;; agent/effect-executor.rkt — Executor for agent-level effects (F1, W2-T3)
;; STABILITY: evolving
;;
;; Executes lists of effect descriptors produced by loop-phases.
;; Separates WHAT happens (effect descriptors in effect-types.rkt)
;; from HOW it happens (this executor).
;;
;; Layering: effect-types.rkt defines structs (no infrastructure deps).
;;           effect-executor.rkt imports infrastructure to execute them.
;;
;; v0.99.70 W0: Support for build-result, cancel, log, validate-messages, stream

(require racket/contract
         racket/match
         "event-bus.rkt"
         "event-emitter.rkt"
         "loop-fsm.rkt"
         "effect-types.rkt"
         "state.rkt"
         (only-in "../util/loop-result.rkt" loop-result)
         (only-in "loop-stream.rkt" stream-from-provider handle-cancellation build-stream-result)
         (only-in "stream-runner.rkt" safe-hook-dispatch)
         (only-in "loop-messages.rkt" valid-api-message-sequence?)
         (only-in "turn-model.rkt" make-stream-completion turn-decision-tag)
         (only-in "turn-reducer.rkt" decide-after-stream))

(provide (contract-out
          [execute-effects!
           (->* (list?)
                (#:bus (or/c any/c #f) #:state (or/c any/c #f) #:hook-dispatcher (or/c procedure? #f))
                void?)]
          [execute-effects/return
           (->* (list?)
                (#:bus (or/c any/c #f) #:state (or/c any/c #f) #:hook-dispatcher (or/c procedure? #f))
                any/c)]))

;; ---------------------------------------------------------------------------
;; Executor
;; ---------------------------------------------------------------------------

(define (execute-effects! effects #:bus [bus #f] #:state [st #f] #:hook-dispatcher [hook-disp #f])
  ;; Execute a list of effect descriptors against real infrastructure.
  ;; This is the ONLY place where effects become side effects.
  ;; Returns (void) — use execute-effects/return to capture build-result.
  (execute-effects/return effects #:bus bus #:state st #:hook-dispatcher hook-disp)
  (void))

;; ---------------------------------------------------------------------------
;; Executor with return value
;; ---------------------------------------------------------------------------

(define (execute-effects/return effects
                                #:bus [bus #f]
                                #:state [st #f]
                                #:hook-dispatcher [hook-disp #f])
  ;; Like execute-effects! but returns the accumulated value.
  ;; Supports effect:build-result which captures the loop-result.
  (define result-box (box #f))
  (for ([eff (in-list effects)])
    (match eff
      [(? effect:emit-event?)
       (when (and bus (effect:emit-event-payload eff))
         (emit-typed-event! bus (effect:emit-event-payload eff) #:state st))]
      [(? effect:update-fsm?)
       (current-turn-fsm-state (next-turn-state (effect:update-fsm-from-state eff)
                                                (effect:update-fsm-event eff)))]
      [(? effect:dispatch-hook?)
       ;; Execute hook dispatch and capture the return value
       (define hook-result
         (when hook-disp
           (hook-disp (effect:dispatch-hook-hook-point eff) (effect:dispatch-hook-payload eff))))
       ;; Overwrite result-box so execute-effects/return returns hook result
       (when hook-result
         (set-box! result-box hook-result))]
      [(? effect:build-result?)
       (set-box! result-box
                 (loop-result (loop-state-messages (effect:build-result-state eff))
                              (effect:build-result-result-type eff)
                              (or (effect:build-result-metadata eff) (hasheq))))]
      [(? effect:cancel?)
       (handle-cancellation bus
                            (effect:cancel-session-id eff)
                            (effect:cancel-turn-id eff)
                            st
                            #:hook-dispatcher hook-disp)]
      [(? effect:log?)
       (match (effect:log-level eff)
         ['warning (log-warning (effect:log-message eff))]
         ['info (log-info (effect:log-message eff))]
         ['debug (log-debug (effect:log-message eff))]
         [_ (void)])]
      [(? effect:validate-messages?)
       (unless (valid-api-message-sequence? (effect:validate-messages-messages eff))
         (log-warning "INVALID message sequence detected"))]
      [(? effect:stream?)
       ;; Run the full streaming pipeline: stream → decide → handle/build-result
       (define stream-data
         (stream-from-provider (effect:stream-provider eff)
                               (effect:stream-request eff)
                               (effect:stream-bus eff)
                               (effect:stream-session-id eff)
                               (effect:stream-turn-id eff)
                               (effect:stream-state eff)
                               (effect:stream-hook-dispatcher eff)
                               (effect:stream-cancellation-token eff)))
       (define stream-sc
         (make-stream-completion #:cancelled? (hash-ref stream-data 'cancelled? #f)
                                 #:cancel-reason (hash-ref stream-data 'cancel-reason #f)
                                 #:text (hash-ref stream-data 'text "")
                                 #:tool-calls (hash-ref stream-data 'tool-calls '())))
       (define stream-decision (decide-after-stream stream-sc))
       (define stream-result
         (match (turn-decision-tag stream-decision)
           ['cancelled
            (transition-turn-state! turn-event-stream-cancel)
            (handle-cancellation (effect:stream-bus eff)
                                 (effect:stream-session-id eff)
                                 (effect:stream-turn-id eff)
                                 (effect:stream-state eff)
                                 #:hook-dispatcher (effect:stream-hook-dispatcher eff))]
           [_
            (build-stream-result stream-data
                                 (effect:stream-raw-messages eff)
                                 (effect:stream-bus eff)
                                 (effect:stream-session-id eff)
                                 (effect:stream-turn-id eff)
                                 (effect:stream-state eff)
                                 (effect:stream-tools eff)
                                 (effect:stream-provider eff)
                                 (effect:stream-hook-dispatcher eff))]))
       (set-box! result-box stream-result)]
      [(? effect:none?) (void)]
      [else (void)]))
  (unbox result-box))
