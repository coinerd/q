#lang racket/base

;; runtime/provider-retry.rkt — provider retry execution and PN-6 adaptation
;; STABILITY: internal

(require racket/contract/base
         (only-in "../agent/event-emitter.rkt" emit-session-event! emit-typed-event!)
         (only-in "../agent/event-structs/iteration-events.rkt" make-auto-retry-start-event)
         (only-in "../llm/token-budget.rkt" estimate-context-tokens)
         (only-in "../util/event/event-bus.rkt" event-bus?)
         (only-in "adaptive-retry.rkt" adaptive-network-error-type? adapt-provider-request)
         (only-in "auto-retry.rkt" with-auto-retry)
         "provider-health.rkt")

(provide (contract-out [call-with-provider-retry
                        (->* (procedure? list? hash? event-bus? string? string? real?)
                             (#:health-tracker (or/c provider-health? #f)
                                               #:health-window-secs exact-positive-integer?
                                               #:health-failure-threshold exact-nonnegative-integer?)
                             any/c)]))

(define (call-with-provider-retry attempt-proc
                                  initial-context
                                  initial-settings
                                  bus
                                  session-id
                                  turn-id
                                  ceiling-secs
                                  #:health-tracker [health #f]
                                  #:health-window-secs [health-window default-health-window-secs]
                                  #:health-failure-threshold
                                  [health-threshold default-health-failure-threshold])
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

  (with-auto-retry
   (lambda () (attempt-proc (unbox ctx-for-retry) (unbox settings-for-retry)))
   #:max-retries 2
   #:base-delay-ms 1000
   #:cumulative-ceiling-secs ceiling-secs
   #:on-retry (lambda (attempt max-retries delay-ms error-msg error-type)
                (emit-retry-event! attempt max-retries delay-ms error-msg error-type)
                (maybe-adapt-request! attempt error-type))
   #:on-circuit-break (lambda (_ original-exn)
                        (emit-retry-event! 0 0 0 (exn-message original-exn) 'circuit-breaker))
   ;; v0.99.82 W2 NR-3: Provider health gate.
   ;; Before each retry, record the failure and check health.
   ;; If the provider is unhealthy (≥ threshold failures in window),
   ;; deny the retry — the provider is consistently failing.
   #:health-check-proc
   (if health
       (lambda (exn attempt)
         (record-failure! health)
         (define healthy?
           (provider-healthy? health #:window-secs health-window #:threshold health-threshold))
         (unless healthy?
           (emit-session-event! bus
                                session-id
                                "provider.health-gate"
                                (hasheq 'failures
                                        (recent-failure-count health #:window-secs health-window)
                                        'window-secs
                                        health-window
                                        'threshold
                                        health-threshold
                                        'decision
                                        'unhealthy)))
         healthy?)
       #f)
   ;; On success, record it to reset the failure window.
   #:on-success (if health
                    (lambda () (record-success! health))
                    #f)))
