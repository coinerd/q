#lang racket/base

;; agent/loop-stream.rkt — Streaming, cancellation, and result building
;;
;; Encapsulates the provider streaming loop, cancellation cleanup,
;; and post-stream result assembly for the agent loop.
;;
;; Extracted from loop.rkt (decomposition step).
;; v0.32.3: Migrated from raw emit! to emit-typed-event! with typed structs.
;; v0.32.4: Replaced CPS handle-hook-result with classify-hook-result + match.

(require racket/contract
         racket/match
         racket/string
         "../llm/provider.rkt"
         "../llm/model.rkt"
         "streaming-message.rkt"
         "state.rkt"
         (only-in "../util/content/content-parts.rkt"
                  make-text-part
                  make-tool-call-part
                  text-part
                  text-part-text
                  tool-call-part-arguments
                  tool-call-part-id
                  tool-call-part-name)
         (only-in "../util/loop-result.rkt" make-loop-result loop-result?)
         (only-in "../util/message/message.rkt" make-message message message-id)
         "../util/ids.rkt"
         (only-in "../util/cancellation.rkt" cancellation-token? cancellation-token-cancelled?)
         (only-in "../util/hook-types.rkt" hook-result? hook-result-action hook-result-payload)
         ;; Re-exported from stream-reducer.rkt (v0.53.2)
         (only-in "stream-reducer.rkt"
                  classify-chunk
                  chunk-has-data?
                  accumulate-stream-chunks
                  MAX-STREAM-CHUNKS)
         ;; Re-exported from stream-runner.rkt (v0.53.2)
         (only-in "stream-runner.rkt" stream-from-provider)
         (only-in "../llm/token-budget.rkt" estimate-turn-tokens)
         (only-in "event-emitter.rkt" emit-typed-event!)
         ;; v0.45.12 M2: emit-session-event! moved to agent layer (was runtime/runtime-helpers.rkt)
         (only-in "event-emitter.rkt" emit-session-event!)
         (only-in "event-bus.rkt" event-bus?)
         ;; Stream event types
         (only-in "event-structs/stream-events.rkt"
                  make-stream-completed-event
                  make-stream-delta-event
                  make-stream-tool-call-delta-event
                  make-stream-thinking-event
                  make-stream-message-start-event
                  make-stream-message-delta-event
                  make-stream-message-end-event
                  make-stream-turn-completed-event
                  make-stream-turn-cancelled-event
                  make-stream-tool-call-started-event
                  make-stream-assistant-msg-completed-event)
         (only-in "loop-messages.rkt" usage-empty? classify-hook-result)
         ;; v0.95.16 W3: Post-turn auto-extraction
         (only-in "../runtime/memory/auto-extraction.rkt" maybe-auto-extract-after-response!))

(provide classify-chunk
         chunk-has-data?
         MAX-STREAM-CHUNKS
         accumulate-stream-chunks
         stream-from-provider
         (contract-out [handle-cancellation
                        (->* ((or/c any/c #f) string? string? any/c)
                             (#:hook-dispatcher (or/c procedure? #f))
                             any/c)]
                       [build-stream-result
                        (-> hash?
                            (listof any/c)
                            event-bus?
                            string?
                            string?
                            loop-state?
                            (or/c (listof hash?) #f)
                            provider?
                            (or/c procedure? #f)
                            loop-result?)]))

;; handle-cancellation
;; ============================================================

;; Emits turn.cancelled and turn.completed events, returns a cancelled loop-result.
(define (handle-cancellation bus session-id turn-id state #:hook-dispatcher [hook-dispatcher #f])
  (when hook-dispatcher
    (hook-dispatcher
     'agent-end
     (hasheq 'session-id (loop-state-session-id state) 'turn-id turn-id 'termination 'cancelled)))
  (emit-typed-event! bus
                     (make-stream-turn-completed-event #:session-id session-id
                                                       #:turn-id turn-id
                                                       #:termination 'cancelled
                                                       #:turn-id-str turn-id
                                                       #:reason "cancellation-token")
                     #:state state)
  (make-loop-result (loop-state-messages state)
                    'cancelled
                    (hasheq 'turnId turn-id 'reason "cancellation-token")))

;; ============================================================
;; build-final-stream-result (v0.32.4: extracted from message-end hook)
;; ============================================================

;; Helper that builds the final loop-result after message-end hook processing.
;; Extracted to avoid code duplication in match branches (amend vs pass).
(define (build-final-stream-result bus
                                   session-id
                                   turn-id
                                   state
                                   tools
                                   provider
                                   hook-dispatcher
                                   tool-call-parts
                                   effective-usage
                                   raw-messages
                                   final-text-box)
  (define final-text (unbox final-text-box))
  (define final-text-part (make-text-part final-text))
  (define final-content-parts (append (list final-text-part) tool-call-parts))

  (define assistant-msg-id (generate-id))
  (define assistant-msg
    (make-message assistant-msg-id
                  #f
                  'assistant
                  'message
                  final-content-parts
                  (now-seconds)
                  (hasheq 'turnId turn-id 'model "streamed")))

  ;; v0.95.17 W1: Post-turn auto-extraction (non-fatal, gated by parameter)
  ;; Must fire for both text-only and tool-call turns.
  (maybe-auto-extract-after-response! final-text #:session-id session-id)
  (cond
    [(null? tool-call-parts)
     (emit-typed-event! bus
                        (make-stream-assistant-msg-completed-event #:session-id session-id
                                                                   #:turn-id turn-id
                                                                   #:message-id assistant-msg-id
                                                                   #:content
                                                                   (text-part-text final-text-part))
                        #:state state)
     (state-add-message! state assistant-msg)
     (emit-typed-event! bus
                        (make-stream-turn-completed-event #:session-id session-id
                                                          #:turn-id turn-id
                                                          #:termination 'completed
                                                          #:turn-id-str turn-id)
                        #:state state)
     (when hook-dispatcher
       (hook-dispatcher 'agent-end
                        (hasheq 'session-id session-id 'turn-id turn-id 'termination 'completed)))
     (make-loop-result
      (loop-state-messages state)
      'completed
      (hasheq 'turnId turn-id 'usage (or effective-usage (hasheq)) 'model "streamed"))]
    [else
     (emit-typed-event! bus
                        (make-stream-assistant-msg-completed-event #:session-id session-id
                                                                   #:turn-id turn-id
                                                                   #:message-id assistant-msg-id
                                                                   #:content
                                                                   (text-part-text final-text-part))
                        #:state state)
     (for ([tc (in-list tool-call-parts)])
       (emit-typed-event! bus
                          (make-stream-tool-call-started-event #:session-id session-id
                                                               #:turn-id turn-id
                                                               #:id (tool-call-part-id tc)
                                                               #:name (tool-call-part-name tc)
                                                               #:arguments
                                                               (tool-call-part-arguments tc))
                          #:state state))
     (state-add-message! state assistant-msg)
     (emit-typed-event! bus
                        (make-stream-turn-completed-event #:session-id session-id
                                                          #:turn-id turn-id
                                                          #:termination 'tool-calls-pending
                                                          #:turn-id-str turn-id)
                        #:state state)
     (when hook-dispatcher
       (hook-dispatcher
        'agent-end
        (hasheq 'session-id session-id 'turn-id turn-id 'termination 'tool-calls-pending)))
     (make-loop-result (loop-state-messages state)
                       'tool-calls-pending
                       (hasheq 'turnId
                               turn-id
                               'usage
                               (or effective-usage (hasheq))
                               'model
                               "streamed"
                               'toolCallCount
                               (length tool-call-parts)))]))

;; ============================================================
;; build-stream-result
;; ============================================================

;; Takes accumulated stream data and builds the final loop-result.
(define (build-stream-result stream-data
                             raw-messages
                             bus
                             session-id
                             turn-id
                             state
                             tools
                             provider
                             hook-dispatcher)
  (define all-chunks (reverse (hash-ref stream-data 'all-chunks)))
  (define acc (accumulate-stream-chunks all-chunks))
  (define accumulated-text (hash-ref acc 'text))

  ;; AF4 (RC1) + v0.45.10 NF3/NF4/NF5: Warn on empty assistant response
  ;; Uses event bus (not log-warning) so TUI/extensions can observe.
  ;; string-trim catches whitespace-only responses (NF4).
  (define trimmed-text (string-trim accumulated-text))
  (when (and (string=? trimmed-text "") (null? (hash-ref acc 'tool-calls)))
    (define thinking-len (string-length (hash-ref acc 'thinking "")))
    (emit-session-event! bus
                         session-id
                         "runtime.warning"
                         (hasheq 'warning
                                 "empty-assistant-response"
                                 'turnId
                                 turn-id
                                 'detail
                                 (format "model returned ~a chars thinking but no text content"
                                         thinking-len)
                                 'thinkingLength
                                 thinking-len)))

  (define text-part (make-text-part accumulated-text))

  (define accumulated-tcs (hash-ref acc 'tool-calls))

  (define tool-call-parts
    (for/list ([tc (in-list accumulated-tcs)])
      (make-tool-call-part (hash-ref tc 'id "")
                           (hash-ref tc 'name "")
                           (hash-ref tc 'arguments "{}"))))

  (define content-parts (append (list text-part) tool-call-parts))

  (define stream-usage (hash-ref acc 'usage))

  (define effective-usage
    (if (usage-empty? stream-usage)
        (let ([est (estimate-turn-tokens raw-messages accumulated-text)])
          (hasheq 'prompt_tokens est 'completion_tokens 0 'total_tokens est 'estimated? #t))
        stream-usage))

  (when hook-dispatcher
    (define post-payload
      (hasheq 'model-name
              (provider-name provider)
              'response-content
              accumulated-text
              'usage
              (or stream-usage (hasheq))
              'tool-call-count
              (length tool-call-parts)))
    (hook-dispatcher 'model-response-post post-payload))

  (define msg-end-payload
    (hasheq 'session-id
            session-id
            'turn-id
            turn-id
            'content
            accumulated-text
            'tool-call-count
            (length tool-call-parts)
            'usage
            (or effective-usage (hasheq))))
  (define msg-end-result (and hook-dispatcher (hook-dispatcher 'message-end msg-end-payload)))

  (define final-text-box (box accumulated-text))

  ;; v0.32.4: Flat match instead of CPS callbacks for message-end hook
  (match (classify-hook-result msg-end-result)
    [(list 'block _)
     (emit-typed-event! bus
                        (make-stream-turn-completed-event #:session-id session-id
                                                          #:turn-id turn-id
                                                          #:termination 'completed
                                                          #:turn-id-str turn-id
                                                          #:reason "message-end-blocked")
                        #:state state)
     (make-loop-result (loop-state-messages state)
                       'hook-blocked
                       (hasheq 'turnId turn-id 'hook 'message-end))]
    [(list 'amend payload)
     (set-box! final-text-box (hash-ref payload 'content accumulated-text))
     (build-final-stream-result bus
                                session-id
                                turn-id
                                state
                                tools
                                provider
                                hook-dispatcher
                                tool-call-parts
                                effective-usage
                                raw-messages
                                final-text-box)]
    [_
     (build-final-stream-result bus
                                session-id
                                turn-id
                                state
                                tools
                                provider
                                hook-dispatcher
                                tool-call-parts
                                effective-usage
                                raw-messages
                                final-text-box)]))
