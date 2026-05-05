#lang racket/base

;; agent/loop.rkt — pure agent turn execution (facade)
;;
;; Re-exports from loop-messages.rkt and loop-stream.rkt.
;; This module is the public API surface for the agent loop.
;;
;; Architecture (v0.25.0 decomposition):
;;   loop-messages.rkt — message helpers (build-raw-messages, emit!, etc.)
;;   loop-stream.rkt   — streaming, cancellation, result building
;;   loop.rkt          — facade: re-exports + run-agent-turn orchestrator

(require racket/contract
         racket/match
         racket/string
         racket/list
         racket/date
         racket/set
         "../util/ids.rkt"
         "../util/protocol-types.rkt"
         "event-bus.rkt"
         "state.rkt"
         "../llm/model.rkt"
         "../llm/provider.rkt"
         (only-in "../llm/stream.rkt" accumulate-tool-call-deltas)
         (only-in "../llm/token-budget.rkt" estimate-turn-tokens estimate-context-tokens)
         (only-in "../util/cancellation.rkt" cancellation-token? cancellation-token-cancelled?)
         (only-in "../util/hook-types.rkt" hook-result? hook-result-action hook-result-payload)
         (only-in "../util/content-helpers.rkt" result-content->string)
         "streaming-message.rkt"
         "loop-messages.rkt"
         "loop-stream.rkt"
         (only-in "event-emitter.rkt" emit-typed-event!)
         (only-in "event-structs.rkt"
                  make-turn-start-event make-turn-end-event
                  make-provider-request-event
                  make-context-event
                  make-model-request-blocked-event
                  make-message-blocked-event))

(provide (contract-out [run-agent-turn
                        (->i ([ctx (listof message?)] [prov provider?] [bus event-bus?])
                             (#:session-id [session-id string?]
                              #:turn-id [turn-id string?]
                              #:state [state (or/c loop-state? #f)]
                              #:tools [tools (or/c (listof hash?) #f)]
                              #:cancellation-token [cancellation-token (or/c cancellation-token? #f)]
                              #:hook-dispatcher [hook-dispatcher (or/c procedure? #f)]
                              #:provider-settings [provider-settings (or/c hash? #f)])
                             [result loop-result?])]
                       [build-raw-messages (-> (listof message?) (listof hash?))]
                       [stream-from-provider
                        (-> provider?
                            model-request?
                            event-bus?
                            string?
                            string?
                            loop-state?
                            (or/c procedure? #f)
                            (or/c cancellation-token? #f)
                            hash?)]
                       [handle-cancellation (-> event-bus? string? string? loop-state? loop-result?)]
                       [build-stream-result
                        (-> hash?
                            (listof hash?)
                            event-bus?
                            string?
                            string?
                            loop-state?
                            (or/c (listof hash?) #f)
                            provider?
                            (or/c procedure? #f)
                            loop-result?)])
         MAX-STREAM-CHUNKS
         usage-empty?
         parts->text-string
         emit!
         handle-hook-result)

;; ============================================================
;; Main entry point — thin orchestrator
;; ============================================================

;; run-agent-turn : (listof message?) provider? event-bus?
;;                  #:session-id string?
;;                  #:turn-id string?
;;                  #:state (or/c loop-state? #f)
;;                  #:tools (or/c (listof hash?) #f)  -- pre-formatted OpenAI tool schemas
;;                  #:cancellation-token (or/c cancellation-token? #f)
;;                  #:hook-dispatcher (or/c procedure? #f)
;;               -> loop-result?
(define (run-agent-turn context
                        provider
                        bus
                        #:session-id [session-id "session"]
                        #:turn-id [turn-id "turn"]
                        #:state [state #f]
                        #:tools [tools #f]
                        #:cancellation-token [cancellation-token #f]
                        #:hook-dispatcher [hook-dispatcher #f]
                        #:provider-settings [provider-settings #f])
  ;; Ensure we have a state for accumulation
  (define st (or state (make-loop-state session-id turn-id)))

  ;; 1. Emit turn.started
  (emit-typed-event! bus
                       (make-turn-start-event #:session-id session-id #:turn-id turn-id
                                              #:timestamp (current-inexact-milliseconds)
                                              #:model "" #:provider "")
                       #:state st)

  ;; #667: Dispatch 'agent-start hook at LLM call begin
  (when hook-dispatcher
    (hook-dispatcher
     'agent-start
     (hasheq 'session-id session-id 'turn-id turn-id 'message-count (length context))))

  ;; 2. Build normalized model context (pure function)
  (define raw-messages (build-raw-messages context))

  ;; 3. Emit context.built (v0.19.12 W1: added tokenCount)
  (define ctx-built-token-count (estimate-context-tokens raw-messages))
  (emit-typed-event! bus
                       (make-context-event #:session-id session-id #:turn-id turn-id
                                           #:timestamp (current-inexact-milliseconds)
                                           #:token-count ctx-built-token-count
                                           #:window-size (length raw-messages))
                       #:state st)

  ;; 4. Build model-request
  (define req (make-model-request raw-messages tools (or provider-settings (hasheq))))

  ;; R2-7: model-request-pre hook
  (define pre-hook-result
    (and hook-dispatcher
         (hook-dispatcher 'model-request-pre
                          (hasheq 'model-name
                                  (provider-name provider)
                                  'message-count
                                  (length raw-messages)
                                  'messages
                                  raw-messages
                                  'settings
                                  (model-request-settings req)))))

  (handle-hook-result
   pre-hook-result
   (lambda (payload)
     (emit-typed-event! bus (make-model-request-blocked-event #:session-id session-id #:turn-id turn-id
                                                                           #:timestamp (current-inexact-milliseconds)
                                                                           #:reason "hook"))
     (emit-typed-event! bus (make-turn-end-event #:session-id session-id #:turn-id turn-id
                                                                    #:timestamp (current-inexact-milliseconds)
                                                                    #:reason "hook-blocked"
                                                                    #:duration-ms 0))
     (loop-result raw-messages 'hook-blocked (hasheq 'hook 'model-request-pre)))
   (lambda ()
     ;; DEBUG: validate raw-messages before sending
     (unless (valid-api-message-sequence? raw-messages)
       (log-warning "INVALID message sequence detected! Dumping raw messages:")
       (for ([rm (in-list raw-messages)]
             [i (in-naturals)])
         (log-warning "  msg[~a]: role=~a keys=~a" i (hash-ref rm 'role #f) (hash-keys rm)))
       (log-warning "End of invalid sequence dump"))

     (emit-typed-event! bus
                        (make-provider-request-event #:session-id session-id #:turn-id turn-id
                                                     #:timestamp (current-inexact-milliseconds)
                                                     #:model (hash-ref (model-request-settings req)
                                                                       'model
                                                                       (lambda () (format "~a" (provider-name provider))))
                                                     #:provider (format "~a" (provider-name provider)))
                        #:state st)

     (define msg-start-result
       (and hook-dispatcher
            (hook-dispatcher 'message-start
                             (hasheq 'session-id
                                     session-id
                                     'turn-id
                                     turn-id
                                     'model-name
                                     (provider-name provider)
                                     'message-count
                                     (length raw-messages)))))

     (handle-hook-result
      msg-start-result
      (lambda (payload)
        (emit-typed-event! bus (make-message-blocked-event #:session-id session-id #:turn-id turn-id
                                                                         #:timestamp (current-inexact-milliseconds)
                                                                         #:hook "message-start" #:reason "blocked"))
        (emit-typed-event! bus (make-turn-end-event #:session-id session-id #:turn-id turn-id
                                                                    #:timestamp (current-inexact-milliseconds)
                                                                    #:reason "hook-blocked"
                                                                    #:duration-ms 0))
        (loop-result raw-messages 'hook-blocked (hasheq 'hook 'message-start)))
      (lambda ()
        ;; 5-7. Stream from provider
        (define stream-data
          (stream-from-provider provider
                                req
                                bus
                                session-id
                                turn-id
                                st
                                hook-dispatcher
                                cancellation-token))

        (cond
          [(hash-ref stream-data 'cancelled?)
           (handle-cancellation bus session-id turn-id st #:hook-dispatcher hook-dispatcher)]
          [else
           (build-stream-result stream-data
                                raw-messages
                                bus
                                session-id
                                turn-id
                                st
                                tools
                                provider
                                hook-dispatcher)]))))))
