#lang racket/base

;; agent/loop-dispatch.rkt — IMPURE streaming dispatch (extracted from loop-phases)
;; STABILITY: evolving
;;
;; run-streaming-phase is an EFFECTFUL dispatch function that calls
;; emit-typed-event! and uses parameterize. It does NOT belong in
;; the pure loop-phases module.

(require racket/contract
         racket/match
         "../util/ids.rkt"
         (only-in "../llm/model.rkt" model-request-settings)
         (only-in "../llm/provider.rkt" provider-name provider?)
         "effect-types.rkt"
         "loop-messages.rkt"
         "loop-stream.rkt"
         "loop-fsm.rkt"
         "state.rkt"
         (only-in "event-structs.rkt"
                  make-provider-request-event
                  make-message-blocked-event
                  make-turn-end-event)
         (only-in "turn-reducer.rkt" decide-after-msg-hook decide-after-stream)
         (only-in "turn-model.rkt" make-stream-completion turn-decision-tag)
         (only-in "event-emitter.rkt" emit-typed-event!)
         (only-in "../util/tool-types.rkt" tool?)
         (only-in "event-bus.rkt" event-bus?)
         (only-in "loop-stream.rkt" handle-cancellation build-stream-result)
         (only-in "loop-phases.rkt" phase-msg-hook phase-stream)
         (only-in "state.rkt" current-loop-state-for-error-recovery)
         "../util/loop-result.rkt")

(provide (contract-out [run-streaming-phase
                        (-> provider?
                            any/c
                            event-bus?
                            string?
                            string?
                            loop-state?
                            (listof any/c)
                            (or/c (listof any/c) #f)
                            (or/c procedure? #f)
                            (or/c any/c #f)
                            any/c)]))

;; Phase 7: Full post-pre-hook streaming dispatch
;; Returns loop-result directly (this is an effectful dispatch function)
;; Handles: msg-hook dispatch, streaming, cancellation, result building
(define (run-streaming-phase provider
                             req
                             bus
                             session-id
                             turn-id
                             st
                             raw-messages
                             tools
                             hook-dispatcher
                             cancellation-token)
  (parameterize ([current-turn-fsm-state (current-turn-fsm-state)])
    ;; FSM: pre-hook -> stream
    (current-turn-fsm-state (next-turn-state turn-state-pre-hook turn-event-hook-pass))

    ;; DEBUG: validate raw-messages before sending
    (unless (valid-api-message-sequence? raw-messages)
      (log-warning "INVALID message sequence detected! Dumping raw messages:")
      (for ([rm (in-list raw-messages)]
            [i (in-naturals)])
        (log-warning "  msg[~a]: role=~a keys=~a" i (hash-ref rm 'role #f) (hash-keys rm)))
      (log-warning "End of invalid sequence dump"))

    (emit-typed-event! bus
                       (make-provider-request-event
                        #:session-id session-id
                        #:turn-id turn-id
                        #:timestamp (current-inexact-milliseconds)
                        #:model (hash-ref (model-request-settings req)
                                          'model
                                          (lambda () (format "~a" (provider-name provider))))
                        #:provider (format "~a" (provider-name provider)))
                       #:state st)

    ;; Phase 5: Message-start hook
    (define-values (msg-start-result _fx5)
      (phase-msg-hook hook-dispatcher provider req raw-messages session-id turn-id st))

    (define d-msg (decide-after-msg-hook msg-start-result))
    (match (turn-decision-tag d-msg)
      ['blocked
       (current-turn-fsm-state (next-turn-state turn-state-stream turn-event-msg-hook-block))
       (emit-typed-event! bus
                          (make-message-blocked-event #:session-id session-id
                                                      #:turn-id turn-id
                                                      #:timestamp (current-inexact-milliseconds)
                                                      #:hook "message-start"
                                                      #:reason "blocked"))
       (emit-typed-event! bus
                          (make-turn-end-event #:session-id session-id
                                               #:turn-id turn-id
                                               #:timestamp (current-inexact-milliseconds)
                                               #:reason "hook-blocked"
                                               #:duration-ms 0))
       (loop-result raw-messages 'hook-blocked (hasheq 'hook 'message-start))]
      [_
       ;; Phase 6: Stream from provider
       (define-values (stream-data _fx6)
         (phase-stream provider req bus session-id turn-id st hook-dispatcher cancellation-token))

       (define sc
         (make-stream-completion #:cancelled? (hash-ref stream-data 'cancelled? #f)
                                 #:cancel-reason (hash-ref stream-data 'cancel-reason #f)
                                 #:text (hash-ref stream-data 'text "")
                                 #:tool-calls (hash-ref stream-data 'tool-calls '())))
       (define d-stream (decide-after-stream sc))
       (match (turn-decision-tag d-stream)
         ['cancelled
          (current-turn-fsm-state (next-turn-state turn-state-stream turn-event-stream-cancel))
          (handle-cancellation bus session-id turn-id st #:hook-dispatcher hook-dispatcher)]
         [_
          (build-stream-result stream-data
                               raw-messages
                               bus
                               session-id
                               turn-id
                               st
                               tools
                               provider
                               hook-dispatcher)])])))
