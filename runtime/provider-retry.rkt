#lang racket/base

;; runtime/provider-retry.rkt — provider retry execution and PN-6 adaptation
;; STABILITY: internal

(require racket/contract/base
         racket/string
         (only-in "../agent/event-emitter.rkt" emit-session-event! emit-typed-event!)
         (only-in "../agent/event-structs/iteration-events.rkt" make-auto-retry-start-event)
         (only-in "../llm/token-budget.rkt" estimate-context-tokens)
         (only-in "../util/event/event-bus.rkt" event-bus?)
         (only-in "../util/content/content-parts.rkt" make-text-part)
         (only-in "../util/message/message.rkt" make-message)
         (only-in "../util/exn.rkt"
                  exn:fail:stream-error
                  exn:fail:stream-error?
                  exn:fail:stream-error-partial-text
                  exn:fail:stream-error-partial-messages
                  exn:fail:stream-error-original-exn)
         (only-in "../util/ids.rkt" generate-id now-seconds)
         (only-in "adaptive-retry.rkt" adaptive-network-error-type? adapt-provider-request)
         (only-in "auto-retry.rkt" with-auto-retry retry-cancelled? default-stall-max-consecutive)
         (only-in "../util/cancellation.rkt" cancellation-token?)
         "provider-health.rkt")

;; D8 (#9357): campaign-aware provider retry scaling. Interactive turns keep
;; the interactive defaults (2 retries, 2-stall breaker, caller ceiling).
;; The campaign executor parameterizes these to wave-scale values (via
;; extensions/gsd/go-orchestrator.rkt execute-campaign-request!) so a single
;; transient SSE read timeout does not burn an entire implementation wave.
(define current-provider-retry-max-retries (make-parameter 2))
(define current-provider-retry-stall-max-consecutive (make-parameter default-stall-max-consecutive))
(define current-provider-retry-ceiling-secs (make-parameter #f))

(provide (contract-out [call-with-provider-retry
                        (->* (procedure? list? hash? event-bus? string? string? real?)
                             (#:health-tracker (or/c provider-health? #f)
                                               #:health-window-secs exact-positive-integer?
                                               #:health-failure-threshold exact-nonnegative-integer?
                                               #:partial-recovery boolean?
                                               #:partial-recovery-min-chars exact-nonnegative-integer?
                                               #:cancellation-token (or/c cancellation-token? #f))
                             any/c)]
                       [default-partial-recovery-min-chars exact-nonnegative-integer?]))

;; D8 (#9357): provider-retry scaling knobs for campaign-aware retry.
(provide current-provider-retry-max-retries
         current-provider-retry-stall-max-consecutive
         current-provider-retry-ceiling-secs)

;; v0.99.82 W3 NR-4: Minimum partial text length to qualify for recovery.
(define default-partial-recovery-min-chars 200)

(define (call-with-provider-retry
         attempt-proc
         initial-context
         initial-settings
         bus
         session-id
         turn-id
         ceiling-secs
         #:health-tracker [health #f]
         #:health-window-secs [health-window default-health-window-secs]
         #:health-failure-threshold [health-threshold default-health-failure-threshold]
         #:partial-recovery [partial-recovery? #f]
         #:partial-recovery-min-chars [partial-min-chars default-partial-recovery-min-chars]
         #:cancellation-token [cancellation-token #f])
  (define ctx-for-retry (box initial-context))
  (define settings-for-retry (box initial-settings))

  ;; Recovery data extracted from exn:fail:stream-error wrappers.
  ;; These local boxes replace the former current-partial-text parameter.
  (define partial-text-box (box #f))
  (define partial-msgs-box (box '()))

  ;; Wrap attempt-proc to intercept exn:fail:stream-error from stream-from-provider.
  ;; Extracts recovery data into local boxes, then re-raises the original
  ;; exception so with-auto-retry's error classification works correctly.
  (define (wrapped-attempt ctx settings)
    (with-handlers ([exn:fail:stream-error?
                     (lambda (e)
                       (set-box! partial-text-box (exn:fail:stream-error-partial-text e))
                       (set-box! partial-msgs-box (exn:fail:stream-error-partial-messages e))
                       ;; Re-raise original so retry classification (held-request?,
                       ;; minimal-output-stall?, etc.) can inspect the real exception.
                       (raise (exn:fail:stream-error-original-exn e)))])
      (attempt-proc ctx settings)))

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

  ;; Partial result recovery: when partial-recovery is enabled and partial
  ;; text (from exn:fail:stream-error) exceeds the threshold, prepend it as
  ;; a continuation prompt so the retry can resume from where the provider
  ;; left off.
  (define (maybe-inject-partial-recovery!)
    (when partial-recovery?
      (define partial (unbox partial-text-box))
      (when (and partial (>= (string-length partial) partial-min-chars))
        (define continuation-text
          (format
           "[Previous partial response (provider stalled):\n~a\n]\n\nContinue from where you left off."
           partial))
        (define continuation-msg
          (make-message (generate-id)
                        #f
                        'assistant
                        'message
                        (list (make-text-part continuation-text))
                        (now-seconds)
                        (hasheq 'partial-recovery #t)))
        (set-box! ctx-for-retry (cons continuation-msg (unbox ctx-for-retry)))
        (emit-session-event!
         bus
         session-id
         "provider.partial-recovery"
         (hasheq 'partialChars (string-length partial) 'minChars partial-min-chars))))
    ;; Always clear after checking (consumed or not).
    (set-box! partial-text-box #f))

  ;; Execute with retry. After with-auto-retry returns or raises,
  ;; if an exception propagates and we have partial messages,
  ;; re-wrap with exn:fail:stream-error so session-lifecycle can flush them.
  (with-handlers (;; W0-F5: a cancellation during backoff must propagate cleanly,
                  ;; never be re-wrapped as a partial stream error.
                  [retry-cancelled? (lambda (e) (raise e))]
                  [exn:fail?
                   (lambda (e)
                     (define msgs (unbox partial-msgs-box))
                     (if (pair? msgs)
                         ;; Attach recovery data for upstream consumers (session-lifecycle).
                         (raise (exn:fail:stream-error (exn-message e)
                                                       (current-continuation-marks)
                                                       #f ; partial-text already consumed or cleared
                                                       msgs
                                                       e))
                         (raise e)))])
    (with-auto-retry
     (lambda () (wrapped-attempt (unbox ctx-for-retry) (unbox settings-for-retry)))
     #:max-retries (current-provider-retry-max-retries)
     #:base-delay-ms 1000
     #:stall-max-consecutive (current-provider-retry-stall-max-consecutive)
     #:cancellation-token cancellation-token
     #:cumulative-ceiling-secs (or (current-provider-retry-ceiling-secs) ceiling-secs)
     #:on-retry (lambda (attempt max-retries delay-ms error-msg error-type)
                  (emit-retry-event! attempt max-retries delay-ms error-msg error-type)
                  (maybe-adapt-request! attempt error-type)
                  (maybe-inject-partial-recovery!))
     #:on-circuit-break
     (lambda (break-reason original-exn)
       ;; Emit dedicated circuit-break.tripped trace event for post-hoc analysis
       (emit-session-event! bus
                            session-id
                            "circuit-break.tripped"
                            (hasheq 'reason break-reason 'sessionId session-id 'turnId turn-id))
       ;; Emit existing auto-retry.start event for TUI display
       (emit-retry-event! 0 0 0 (exn-message original-exn) 'circuit-breaker))
     ;; v0.99.82 W2 NR-3: Provider health gate.
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
     ;; On success, clear recovery data and record health.
     #:on-success (lambda ()
                    (set-box! partial-text-box #f)
                    (set-box! partial-msgs-box '())
                    (when health
                      (record-success! health))))))
