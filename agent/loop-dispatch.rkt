#lang racket/base

;; agent/loop-dispatch.rkt — IMPURE streaming dispatch (extracted from loop-phases)
;; STABILITY: evolving
;;
;; run-streaming-phase is an EFFECTFUL dispatch function that calls
;; emit-typed-event! and uses parameterize. It does NOT belong in
;; the pure loop-phases module.

(require racket/contract
         racket/match
         (only-in racket/list take-right)
         "../util/ids.rkt"
         (only-in "../llm/model.rkt" model-request-settings model-request?)
         (only-in "../llm/provider.rkt" provider-name provider?)
         "effect-types.rkt"
         (only-in "effect-executor.rkt" execute-effects! execute-effects/return)
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
         (only-in "../util/cancellation.rkt" cancellation-token?)
         (only-in "event-bus.rkt" event-bus?)
         (only-in "loop-stream.rkt" handle-cancellation build-stream-result)
         (only-in "loop-phases.rkt" phase-msg-hook phase-stream)
         (only-in "state.rkt" current-loop-state-for-error-recovery)
         (only-in "../util/loop-result.rkt" loop-result loop-result?))

(provide (contract-out [run-streaming-phase
                        (-> provider?
                            model-request?
                            event-bus?
                            string?
                            string?
                            loop-state?
                            (listof any/c)
                            (or/c (listof hash?) #f)
                            (or/c procedure? #f)
                            (or/c cancellation-token? #f)
                            loop-result?)]))

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
    ;; v0.99.69 W2: Derive from-state from current-turn-fsm-state, not hardcoded literal
    (transition-turn-state! turn-event-hook-pass)

    ;; Build validation + DIAG + provider-request effect list
    (define pre-stream-effects
      (append
       ;; DEBUG validation via effect:validate-messages
       (list (effect:validate-messages raw-messages))
       ;; DIAG logging via effect:log
       (let ([n (length raw-messages)])
         (if (> n 0)
             (let* ([n3 (min 3 n)]
                    [last-roles (for/list ([rm (in-list (take-right raw-messages n3))])
                                  (hash-ref rm 'role #f))])
               (list (effect:log
                      'warning
                      (format "DIAG: provider request: ~a messages, last roles: ~a" n last-roles)
                      #f)))
             '()))
       ;; Provider-request event
       (list (effect:emit-event 'provider-request
                                (make-provider-request-event
                                 #:session-id session-id
                                 #:turn-id turn-id
                                 #:timestamp (current-inexact-milliseconds)
                                 #:model (hash-ref (model-request-settings req)
                                                   'model
                                                   (lambda () (format "~a" (provider-name provider))))
                                 #:provider (format "~a" (provider-name provider)))))))
    (execute-effects! pre-stream-effects #:bus bus #:state st)

    ;; Phase 5: Message-start hook — route through effects!
    (define-values (msg-payload fx5) (phase-msg-hook provider raw-messages session-id turn-id))
    (define msg-start-result
      (execute-effects/return fx5 #:bus bus #:state st #:hook-dispatcher hook-dispatcher))

    (define d-msg (decide-after-msg-hook msg-start-result))
    (match (turn-decision-tag d-msg)
      ['blocked
       ;; v0.99.70 W1: Route blocked branch through effects!
       (execute-effects/return
        (list (effect:update-fsm (current-turn-fsm-state) turn-event-msg-hook-block)
              (effect:emit-event 'message-blocked
                                 (make-message-blocked-event #:session-id session-id
                                                             #:turn-id turn-id
                                                             #:timestamp
                                                             (current-inexact-milliseconds)
                                                             #:hook "message-start"
                                                             #:reason "blocked"))
              (effect:emit-event 'turn-end
                                 (make-turn-end-event #:session-id session-id
                                                      #:turn-id turn-id
                                                      #:timestamp (current-inexact-milliseconds)
                                                      #:reason "hook-blocked"
                                                      #:duration-ms 0))
              (effect:build-result raw-messages 'hook-blocked (hasheq 'hook 'message-start)))
        #:bus bus
        #:state st)]
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
          ;; v0.99.69 W2: Derive from-state from current-turn-fsm-state
          (transition-turn-state! turn-event-stream-cancel)
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
