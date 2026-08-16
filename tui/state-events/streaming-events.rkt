#lang racket/base

;; tui/state-events/streaming-events.rkt -- Streaming/model event handlers
;; STABILITY: internal
;;
;; Extracted from core-handlers.rkt. Handles model streaming deltas,
;; context built, and assistant message completion events.
;; Registration side-effects happen at module load time.

(require racket/string
         racket/match
         racket/list
         (only-in "../../util/event/event.rkt"
                  event
                  event-ev
                  event-payload
                  event-time
                  event-session-id
                  event-turn-id
                  event?)
         (only-in "../../util/message/message.rkt" message)
         "../../util/cost-tracker.rkt"
         "../../ui-core/conversation-artifact.rkt"
         "../../ui-core/conversation-reducer.rkt"
         "../state-types.rkt"
         "handler-helpers.rkt"
         "helpers.rkt"
         "registry.rkt")

;; ============================================================
;; Message / streaming handlers
;; ============================================================

;; The event envelope is the sole identity source.  Payload IDs and empty
;; strings are deliberately not accepted as fallbacks because they can merge
;; unrelated sessions into the same artifact.
(define (event->conversation-fact evt)
  (define payload (event-payload evt))
  (define base
    (hasheq 'event-type
            (event-ev evt)
            'session-id
            (event-session-id evt)
            'turn-id
            (event-turn-id evt)))
  (if (hash? payload)
      (for/fold ([fact base]) ([(key value) (in-hash payload)])
        (if (memq key '(session-id sessionId turn-id turnId event-type))
            fact
            (hash-set fact key value)))
      base))

(define (reduce-conversation state evt)
  (struct-copy ui-state
               state
               [conversation-reducer
                (reduce-event (ui-state-conversation-reducer state) (event->conversation-fact evt))]))

(define (canonical-event-identity? evt)
  (and (string? (event-session-id evt))
       (not (string=? (event-session-id evt) ""))
       (string? (event-turn-id evt))
       (not (string=? (event-turn-id evt) ""))))

(define (event-for-active-stream? state evt)
  (and (canonical-event-identity? evt)
       (string? (ui-state-session-id state))
       (equal? (ui-state-session-id state) (event-session-id evt))
       (or (equal? (ui-state-active-turn-id state) (event-turn-id evt))
           (equal? (ui-state-active-model-turn-id state) (event-turn-id evt)))))

(define (event-can-activate-stream? state evt)
  (and (canonical-event-identity? evt)
       (or (not (ui-state-session-id state))
           (equal? (ui-state-session-id state) (event-session-id evt)))
       (or (and (not (ui-state-active-turn-id state)) (not (ui-state-active-model-turn-id state)))
           (event-for-active-stream? state evt))))

;; Some providers begin streaming without a preceding turn.started event. Adopt
;; the first canonical stream identity only when no unrelated turn is active.
(define (activate-stream-identity state evt)
  (cond
    [(or (not (canonical-event-identity? evt))
         (and (ui-state-session-id state)
              (not (equal? (ui-state-session-id state) (event-session-id evt)))))
     state]
    [(or (ui-state-active-turn-id state) (ui-state-active-model-turn-id state)) state]
    [else
     (set-active-model-turn-id (if (ui-state-session-id state)
                                   state
                                   (struct-copy ui-state state [session-id (event-session-id evt)]))
                               (event-turn-id evt))]))

(define (artifact-entry-meta artifact)
  (hasheq 'artifact
          artifact
          'artifact-id
          (conversation-artifact-id artifact)
          'session-id
          (conversation-artifact-session-id artifact)
          'turn-id
          (conversation-artifact-turn-id artifact)))

(define (entry-for-artifact? entry artifact)
  (equal? (hash-ref (transcript-entry-meta entry) 'artifact-id #f)
          (conversation-artifact-id artifact)))

(define (upsert-artifact-entry state artifact timestamp)
  (define body (conversation-artifact-body artifact))
  (cond
    [(or (and (eq? (conversation-artifact-kind artifact) 'thinking) (string=? (string-trim body) ""))
         ;; Thinking-only turns retain the canonical empty assistant artifact
         ;; internally but do not project a phantom assistant transcript row.
         (and (eq? (conversation-artifact-kind artifact) 'assistant)
              (string=? (string-trim body) "")
              (reducer-thinking-artifact (ui-state-conversation-reducer state)
                                         (conversation-artifact-session-id artifact)
                                         (conversation-artifact-turn-id artifact))))
     state]
    [else
     (define entries (ui-state-transcript state))
     (define existing? (ormap (lambda (entry) (entry-for-artifact? entry artifact)) entries))
     (if existing?
         (struct-copy ui-state
                      state
                      [transcript
                       (for/list ([entry (in-list entries)])
                         (if (entry-for-artifact? entry artifact)
                             (transcript-entry (conversation-artifact-kind artifact)
                                               body
                                               (transcript-entry-timestamp entry)
                                               (artifact-entry-meta artifact)
                                               (transcript-entry-id entry))
                             entry))])
         (append-entry state
                       (make-entry (conversation-artifact-kind artifact)
                                   body
                                   timestamp
                                   (artifact-entry-meta artifact))))]))

(define (sync-turn-artifacts state evt)
  (define session-id (event-session-id evt))
  (define turn-id (event-turn-id evt))
  (cond
    [(not (and (string? session-id)
               (not (string=? session-id ""))
               (string? turn-id)
               (not (string=? turn-id ""))))
     state]
    [else
     (define reducer (ui-state-conversation-reducer state))
     ;; Chronological insertion is thinking then assistant.  Since append-entry
     ;; stores newest first, the resulting transcript order is assistant,
     ;; thinking, matching the existing TUI contract.
     (for/fold ([current state])
               ([artifact
                 (in-list (filter values
                                  (list (reducer-thinking-artifact reducer session-id turn-id)
                                        (reducer-assistant-artifact reducer session-id turn-id))))])
       (upsert-artifact-entry current artifact (event-time evt)))]))

(define (handle-assistant-message-completed state evt)
  (define terminal-for-active? (event-for-active-stream? state evt))
  (define reduced (reduce-conversation state evt))
  (define synced (sync-turn-artifacts reduced evt))
  (if terminal-for-active?
      (clear-streaming (set-pending-tool-name (set-busy synced #f) #f))
      synced))

(define (handle-model-stream-delta state evt)
  (if (not (event-can-activate-stream? state evt))
      state
      (let* ([payload (event-payload evt)]
             [delta (hash-ref payload 'delta "")]
             [reduced (activate-stream-identity (reduce-conversation state evt) evt)]
             [current-streaming (ui-state-streaming-text reduced)]
             [new-streaming (string-append (or current-streaming "") delta)]
             [now (current-inexact-milliseconds)])
        ;; BF1b (v0.99.4): Record delta timestamp for streaming stall watchdog
        (set-last-delta-ms
         (set-streaming-phase (set-streaming-text (set-busy reduced #t) new-streaming) 'streaming)
         now))))

(define (handle-model-stream-thinking state evt)
  (if (not (event-can-activate-stream? state evt))
      state
      (let* ([payload (event-payload evt)]
             [delta (hash-ref payload 'delta "")]
             [reduced (activate-stream-identity (reduce-conversation state evt) evt)]
             [current-thinking (ui-state-streaming-thinking reduced)]
             [new-thinking (string-append (or current-thinking "") delta)]
             [now (current-inexact-milliseconds)])
        ;; BF1b (v0.99.4): Record thinking timestamp for streaming stall watchdog
        (set-last-delta-ms (set-streaming-thinking (set-busy reduced #t) new-thinking) now))))

(define (handle-model-stream-completed state evt)
  (define payload (event-payload evt))
  (define session-id (event-session-id evt))
  (define turn-id (event-turn-id evt))
  (define canonical-identity? (canonical-event-identity? evt))
  (define terminal-for-active? (event-for-active-stream? state evt))
  (define first-model-completion?
    (and
     canonical-identity?
     (not (reducer-turn-completed? (ui-state-conversation-reducer state) session-id turn-id 'model))))
  (define reduced (reduce-conversation state evt))
  (define raw-usage (and (hash? payload) (hash-ref payload 'usage (hasheq))))
  (define usage
    (if (hash? raw-usage)
        raw-usage
        (hasheq)))
  (define (safe-token-count value)
    (if (exact-nonnegative-integer? value) value 0))
  (define in-tok (safe-token-count (hash-ref usage 'prompt_tokens (hash-ref usage 'input_tokens 0))))
  (define out-tok
    (safe-token-count (hash-ref usage 'completion_tokens (hash-ref usage 'output_tokens 0))))
  (define ct (ui-state-cost-tracker state))
  (when (and first-model-completion? ct (or (positive? in-tok) (positive? out-tok)))
    (cost-tracker-update! ct in-tok out-tok (ui-model-label state)))
  (define synced (sync-turn-artifacts reduced evt))
  (if terminal-for-active?
      (clear-streaming synced)
      synced))

(define (handle-model-request-started state evt)
  (if (event-can-activate-stream? state evt)
      (set-busy (activate-stream-identity state evt) #t)
      state))

(define (handle-context-built state evt)
  (define payload (event-payload evt))
  (define tok
    (and (hash? payload) (or (hash-ref payload 'tokenCount #f) (hash-ref payload 'token-count #f))))
  (if tok
      (struct-copy ui-state state [context-tokens tok])
      state))

;; ============================================================
;; Register handlers at module load time
;; ============================================================

(register-event-reducer! "model.stream.delta" handle-model-stream-delta)
(register-event-reducer! "model.stream.thinking" handle-model-stream-thinking)
(register-event-reducer! "model.stream.completed" handle-model-stream-completed)
(register-event-reducer! "model.request.started" handle-model-request-started)
(register-event-reducer! "context.built" handle-context-built)
(register-event-reducer! "assistant.message.completed" handle-assistant-message-completed)
