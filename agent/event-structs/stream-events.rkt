#lang racket/base

;; agent/event-structs/stream-events.rkt — streaming-specific events
;;
;; These events are emitted by loop-stream.rkt during provider streaming.
;; They use typed-event as the base and carry stream-specific payloads.
;;
;; v0.32.3: Created for complete typed event migration (replaces raw emit! calls).

(require "base.rkt"
         "../../util/event-macro.rkt")

;; ============================================================
;; Stream lifecycle events
;; ============================================================

;; model.stream.completed — sent when the LLM stream ends normally or is truncated.
;; Different from provider-events' model-stream-completed-event (which has model+provider).
;; This one carries usage, finish_reason, truncated? from the streaming loop.
(define-typed-event stream-completed-event
                    "model.stream.completed"
                    (usage finish_reason)
                    #:optional ([truncated? #f]))

;; model.stream.delta — text delta during streaming
(define-typed-event stream-delta-event "model.stream.delta" (delta))

;; model.stream.delta with tool-call — emitted for tool-call deltas
(define-typed-event stream-tool-call-delta-event "model.stream.delta" (delta-tool-call))

;; model.stream.thinking — thinking/reasoning delta
(define-typed-event stream-thinking-event "model.stream.thinking" (delta))

;; ============================================================
;; Message lifecycle events (streaming context)
;; ============================================================

;; message.start — emitted when first text delta arrives
(define-typed-event stream-message-start-event "message.start" (message-id))

;; message.delta — text delta with message-id
(define-typed-event stream-message-delta-event "message.delta" (text message-id))

;; message.end — emitted when stream completes or is interrupted
(define-typed-event stream-message-end-event "message.end" (message-id usage))

;; ============================================================
;; Turn lifecycle events (streaming context)
;; ============================================================

;; turn.completed — emitted with termination reason and details
(define-typed-event stream-turn-completed-event
                    "turn.completed"
                    (termination turn-id-str)
                    #:optional ([reason #f]))

;; turn.cancelled — emitted when user cancels
(define-typed-event stream-turn-cancelled-event "turn.cancelled" (reason))

;; ============================================================
;; Tool call events (streaming context)
;; ============================================================

;; tool.call.started — emitted when tool call is detected in stream
(define-typed-event stream-tool-call-started-event "tool.call.started" (id name arguments))

;; assistant.message.completed — emitted when assistant message is fully assembled
(define-typed-event stream-assistant-msg-completed-event
                    "assistant.message.completed"
                    (message-id content))
