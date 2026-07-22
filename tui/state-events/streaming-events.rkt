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
         (only-in "../../util/event/event.rkt" event event-payload event-time event?)
         (only-in "../../util/message/message.rkt" message)
         "../../util/cost-tracker.rkt"
         "../state-types.rkt"
         "handler-helpers.rkt"
         "helpers.rkt"
         "registry.rkt")

;; ============================================================
;; Message / streaming handlers
;; ============================================================

(define (handle-assistant-message-completed state evt)
  (define payload (event-payload evt))
  (define streamed (ui-state-streaming-text state))
  (define content (or streamed (hash-ref payload 'content "")))
  (define ts (event-time evt))
  (define thinking (ui-state-streaming-thinking state))
  (define s0
    (if (and thinking
             (> (string-length (string-trim thinking)) 0)
             (string=? (string-trim content) ""))
        (append-entry state (make-entry 'thinking thinking ts (hash)))
        state))
  (clear-streaming (set-pending-tool-name
                    (set-busy (append-entry s0 (make-entry 'assistant content ts (hash))) #f)
                    #f)))

(define (handle-model-stream-delta state evt)
  (define payload (event-payload evt))
  (define delta (hash-ref payload 'delta ""))
  (define current-streaming (ui-state-streaming-text state))
  (define new-streaming (string-append (or current-streaming "") delta))
  ;; BF1b (v0.99.4): Record delta timestamp for streaming stall watchdog
  (define now (current-inexact-milliseconds))
  (set-last-delta-ms (set-streaming-phase (set-streaming-text (set-busy state #t) new-streaming)
                                          'streaming)
                     now))

(define (handle-model-stream-thinking state evt)
  (define payload (event-payload evt))
  (define delta (hash-ref payload 'delta ""))
  (define current-thinking (ui-state-streaming-thinking state))
  (define new-thinking (string-append (or current-thinking "") delta))
  ;; BF1b (v0.99.4): Record thinking timestamp for streaming stall watchdog
  (define now (current-inexact-milliseconds))
  (set-last-delta-ms (set-streaming-thinking (set-busy state #t) new-thinking) now))

(define (handle-model-stream-completed state evt)
  (define payload (event-payload evt))
  (define raw-usage (and (hash? payload) (hash-ref payload 'usage (hasheq))))
  (define usage
    (if (hash? raw-usage)
        raw-usage
        (hasheq)))
  (define in-tok (hash-ref usage 'prompt_tokens (hash-ref usage 'input_tokens 0)))
  (define out-tok (hash-ref usage 'completion_tokens (hash-ref usage 'output_tokens 0)))
  (define ct (ui-state-cost-tracker state))
  (when (and ct (or (positive? in-tok) (positive? out-tok)))
    (cost-tracker-update! ct in-tok out-tok (ui-model-label state)))
  (clear-streaming state))

(define (handle-model-request-started state evt)
  (set-busy state #t))

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
