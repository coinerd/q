#lang racket/base

;; runtime/provider-retry.rkt — provider retry execution and PN-6 adaptation
;; STABILITY: internal

(require racket/contract/base
         (only-in "../agent/event-emitter.rkt" emit-session-event! emit-typed-event!)
         (only-in "../agent/event-structs/iteration-events.rkt" make-auto-retry-start-event)
         (only-in "../llm/token-budget.rkt" estimate-context-tokens)
         (only-in "../util/event/event-bus.rkt" event-bus?)
         (only-in "adaptive-retry.rkt" adaptive-network-error-type? adapt-provider-request)
         (only-in "auto-retry.rkt" with-auto-retry))

(provide (contract-out [call-with-provider-retry
                        (-> procedure? list? hash? event-bus? string? string? real? any/c)]))

(define (call-with-provider-retry attempt-proc
                                  initial-context
                                  initial-settings
                                  bus
                                  session-id
                                  turn-id
                                  ceiling-secs)
  (define ctx-for-retry (box initial-context))
  (define settings-for-retry (box initial-settings))

  (define (emit-retry-event! attempt max-retries delay-ms error-msg error-type)
    (emit-typed-event! bus
                       (make-auto-retry-start-event #:session-id session-id
                                                    #:turn-id turn-id
                                                    #:timestamp (current-inexact-milliseconds)
                                                    #:attempt attempt
                                                    #:max-retries max-retries
                                                    #:delay-ms delay-ms
                                                    #:error error-msg
                                                    #:error-type error-type)))

  (define (maybe-adapt-request! attempt error-type)
    (when (and (>= attempt 2) (adaptive-network-error-type? error-type))
      (define original-ctx (unbox ctx-for-retry))
      (define original-settings (unbox settings-for-retry))
      (define-values (reduced-ctx reduced-settings adapted?)
        (adapt-provider-request original-ctx original-settings))
      (set-box! ctx-for-retry reduced-ctx)
      (set-box! settings-for-retry reduced-settings)
      (emit-session-event! bus
                           session-id
                           "provider.adaptive-retry"
                           (hasheq 'attempt
                                   attempt
                                   'errorType
                                   error-type
                                   'originalMessageCount
                                   (length original-ctx)
                                   'reducedMessageCount
                                   (length reduced-ctx)
                                   'originalTokenEstimate
                                   (estimate-context-tokens original-ctx)
                                   'reducedTokenEstimate
                                   (estimate-context-tokens reduced-ctx)
                                   'originalMaxTokens
                                   (hash-ref original-settings 'max-tokens #f)
                                   'reducedMaxTokens
                                   (hash-ref reduced-settings 'max-tokens #f)
                                   'floorReached
                                   (not adapted?)))))

  (with-auto-retry (lambda () (attempt-proc (unbox ctx-for-retry) (unbox settings-for-retry)))
                   #:max-retries 2
                   #:base-delay-ms 1000
                   #:cumulative-ceiling-secs ceiling-secs
                   #:on-retry (lambda (attempt max-retries delay-ms error-msg error-type)
                                (emit-retry-event! attempt max-retries delay-ms error-msg error-type)
                                (maybe-adapt-request! attempt error-type))
                   #:on-circuit-break
                   (lambda (_ original-exn)
                     (emit-retry-event! 0 0 0 (exn-message original-exn) 'circuit-breaker))))
