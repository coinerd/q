#lang racket/base

;; agent/loop-stream.rkt — Streaming, cancellation, and result building
;;
;; Encapsulates the provider streaming loop, cancellation cleanup,
;; and post-stream result assembly for the agent loop.
;;
;; Extracted from loop.rkt (decomposition step).

(require "../llm/provider.rkt"
         "../llm/model.rkt"
         "streaming-message.rkt"
         "state.rkt"
         "../util/protocol-types.rkt"
         "../util/ids.rkt"
         (only-in "../util/cancellation.rkt" cancellation-token? cancellation-token-cancelled?)
         (only-in "../util/hook-types.rkt" hook-result? hook-result-action hook-result-payload)
         (only-in "../llm/stream.rkt" accumulate-tool-call-deltas)
         (only-in "../llm/token-budget.rkt" estimate-turn-tokens)
         "loop-messages.rkt")

;; ============================================================
;; Configurable chunk limit (v0.12.3 Wave 0.1)
;; ============================================================

;; Maximum number of chunks to process from a provider stream.
;; Prevents infinite loops from misbehaving providers.
;; Parameter justified: tests use `parameterize` to set small values;
;; production uses default.
(define MAX-STREAM-CHUNKS (make-parameter 10000))

;; ============================================================
;; Pure stream accumulator (v0.29.5 W1)
;; ============================================================

;; Pure accumulator for stream chunk processing.
;; No I/O, no mutation — just functional state transitions.
(struct stream-accumulator
        (text-parts tool-calls thinking-parts usage chunk-count finish-reason done?)
  #:transparent)

(define (make-empty-accumulator)
  (stream-accumulator '() '() '() #f 0 #f #f))

;; Pure: process a single chunk and return the next accumulator state.
;; Returns a new stream-accumulator; never mutates.
;; Handles: text delta, tool-call delta, thinking delta, done, usage.
;; Ignores malformed chunks gracefully (returns acc unchanged).
(define (process-chunk chunk acc)
  (cond
    [(not chunk) acc] ; EOF / #f
    [(not (stream-chunk? chunk)) acc] ; malformed
    [else
     (define text-parts (stream-accumulator-text-parts acc))
     (define tool-calls (stream-accumulator-tool-calls acc))
     (define thinking-parts (stream-accumulator-thinking-parts acc))
     (define usage (stream-accumulator-usage acc))
     (define chunk-count (stream-accumulator-chunk-count acc))
     (define finish-reason (stream-accumulator-finish-reason acc))
     (define done? (stream-accumulator-done? acc))
     ;; Extract fields
     (define delta-text (stream-chunk-delta-text chunk))
     (define delta-tc (stream-chunk-delta-tool-call chunk))
     (define delta-thinking (stream-chunk-delta-thinking chunk))
     (define chunk-usage (stream-chunk-usage chunk))
     (define chunk-done? (stream-chunk-done? chunk))
     (define chunk-finish (stream-chunk-finish-reason chunk))
     ;; Update fields based on chunk content
     (define new-text-parts
       (if delta-text
           (append text-parts (list delta-text))
           text-parts))
     (define new-tool-calls
       (if delta-tc
           (append tool-calls (list delta-tc))
           tool-calls))
     (define new-thinking-parts
       (if delta-thinking
           (append thinking-parts (list delta-thinking))
           thinking-parts))
     (define new-usage (or chunk-usage usage))
     (define new-chunk-count (add1 chunk-count))
     (define new-finish-reason (or chunk-finish finish-reason))
     (define new-done? (or chunk-done? done?))
     (stream-accumulator new-text-parts
                         new-tool-calls
                         new-thinking-parts
                         new-usage
                         new-chunk-count
                         new-finish-reason
                         new-done?)]))

(provide MAX-STREAM-CHUNKS
         stream-from-provider
         handle-cancellation
         build-stream-result
         ;; Pure stream accumulator (v0.29.5)
         stream-accumulator
         stream-accumulator?
         stream-accumulator-text-parts
         stream-accumulator-tool-calls
         stream-accumulator-thinking-parts
         stream-accumulator-usage
         stream-accumulator-chunk-count
         stream-accumulator-finish-reason
         stream-accumulator-done?
         make-empty-accumulator
         ;; NOTE (v0.29.11): process-chunk and stream-accumulator are provided for
         ;; future use but have 0 production callers. process-chunk is blocked by
         ;; mutable streaming-message vs immutable stream-accumulator gap.
         ;; Deferred to v0.30.x — see AUDIT-v0.29.10 for details.
         process-chunk)

;; ============================================================
;; stream-from-provider
;; ============================================================

;; stream-from-provider : provider model-request event-bus string string loop-state
;;                        (or/c procedure? #f) (or/c cancellation-token? #f) -> hash
;; Wraps the streaming loop. Returns a hash with keys:
;;   'text            — accumulated text string
;;   'tool-calls      — list of accumulated tool-call deltas
;;   'all-chunks      — list of all stream chunks
;;   'cancelled?      — boolean
;;   'stream-blocked? — boolean (from message-update hook block)
(define (stream-from-provider provider
                              req
                              bus
                              session-id
                              turn-id
                              state
                              hook-dispatcher
                              cancellation-token)
  (define sm (make-streaming-message (format "msg-~a-~a" turn-id (current-inexact-milliseconds))))
  (define message-id (streaming-message-message-id sm))

  (define stream-gen (provider-stream provider req))
  (define chunk-count (box 0))
  (define limit (MAX-STREAM-CHUNKS))
  ;; v0.15.2: Track whether a normal done chunk was received.
  ;; Used to detect silent stream EOF (BUG-SILENT-STREAM-EOF).
  (define received-done? (box #f))

  ;; v0.12.3 Wave 0.1: Error boundary — emit cleanup events on provider crash.
  ;; Without this, a mid-stream exception leaves TUI in busy?=#t forever.
  (with-handlers ([exn:fail?
                   (lambda (e)
                     ;; Emit cleanup events so subscribers (TUI) don't hang
                     (when (streaming-message-message-started? sm)
                       (emit! bus
                              session-id
                              turn-id
                              "message.end"
                              (hasheq 'message-id message-id 'usage (hasheq))
                              #:state state))
                     (emit!
                      bus
                      session-id
                      turn-id
                      "turn.completed"
                      (hasheq 'termination 'error 'turnId turn-id 'reason "provider-stream-error")
                      #:state state)
                     (raise e))])
    (let stream-loop ()
      (define chunk (stream-gen))
      (when (and chunk (not (eq? chunk #f)))
        ;; v0.12.3 Wave 0.1: Chunk limit — prevent infinite streams
        (set-box! chunk-count (+ 1 (unbox chunk-count)))
        (when (> (unbox chunk-count) limit)
          (log-warning
           (format "stream-from-provider: exceeded ~a chunks, truncating session=~a turn=~a"
                   limit
                   session-id
                   turn-id))
          ;; Emit stream completion + message.end as if done
          (emit! bus
                 session-id
                 turn-id
                 "model.stream.completed"
                 (hasheq 'usage (hasheq) 'truncated? #t)
                 #:state state)
          (when (streaming-message-message-started? sm)
            (emit! bus
                   session-id
                   turn-id
                   "message.end"
                   (hasheq 'message-id message-id 'usage (hasheq))
                   #:state state)))
        (unless (> (unbox chunk-count) limit)
          (streaming-message-append-chunk! sm chunk)
          (when (stream-chunk-delta-text chunk)
            ;; Emit message.start on first text delta
            (unless (streaming-message-message-started? sm)
              (streaming-message-set-message-started! sm)
              (emit! bus
                     session-id
                     turn-id
                     "message.start"
                     (hasheq 'message-id message-id)
                     #:state state))
            ;; Text delta
            (streaming-message-append-text! sm (stream-chunk-delta-text chunk))
            (emit! bus
                   session-id
                   turn-id
                   "model.stream.delta"
                   (hasheq 'delta (stream-chunk-delta-text chunk))
                   #:state state)
            ;; Emit message.delta with message-id for extension consumption
            (emit! bus
                   session-id
                   turn-id
                   "message.delta"
                   (hasheq 'text (stream-chunk-delta-text chunk) 'message-id message-id)
                   #:state state)
            ;; Dispatch message-update hook for text delta
            (when hook-dispatcher
              (define update-result
                (hook-dispatcher 'message-update
                                 (hasheq 'session-id
                                         session-id
                                         'turn-id
                                         turn-id
                                         'delta-text
                                         (stream-chunk-delta-text chunk)
                                         'delta-tool-call
                                         #f)))
              (handle-hook-result update-result
                                  (lambda (payload) (streaming-message-set-blocked! sm))
                                  void)))
          ;; FEAT-72: Thinking/reasoning delta
          (when (stream-chunk-delta-thinking chunk)
            (streaming-message-append-thinking! sm (stream-chunk-delta-thinking chunk))
            (emit! bus
                   session-id
                   turn-id
                   "model.stream.thinking"
                   (hasheq 'delta (stream-chunk-delta-thinking chunk))
                   #:state state))
          (when (stream-chunk-delta-tool-call chunk)
            ;; Tool call delta
            (define tc-delta (stream-chunk-delta-tool-call chunk))
            (streaming-message-append-tool-call! sm tc-delta)
            (emit! bus
                   session-id
                   turn-id
                   "model.stream.delta"
                   (hasheq 'delta-tool-call tc-delta)
                   #:state state)
            ;; Dispatch message-update hook for tool-call delta
            (when hook-dispatcher
              (define update-result
                (hook-dispatcher 'message-update
                                 (hasheq 'session-id
                                         session-id
                                         'turn-id
                                         turn-id
                                         'delta-text
                                         #f
                                         'delta-tool-call
                                         tc-delta)))
              (handle-hook-result update-result
                                  (lambda (payload) (streaming-message-set-blocked! sm))
                                  void)))
          (when (stream-chunk-done? chunk)
            ;; v0.15.2: Mark that we received a normal done chunk
            (set-box! received-done? #t)
            ;; Stream completed
            ;; v0.15.0 Wave 1: include finish_reason in event
            (emit! bus
                   session-id
                   turn-id
                   "model.stream.completed"
                   (hasheq 'usage
                           (or (stream-chunk-usage chunk) (hasheq))
                           'finish_reason
                           (or (stream-chunk-finish-reason chunk) "unknown"))
                   #:state state)
            ;; Emit message.end if we started a message
            (when (streaming-message-message-started? sm)
              (emit! bus
                     session-id
                     turn-id
                     "message.end"
                     (hasheq 'message-id message-id 'usage (or (stream-chunk-usage chunk) (hasheq)))
                     #:state state)))
          ;; Check cancellation after processing chunk
          (cond
            [(and cancellation-token (cancellation-token-cancelled? cancellation-token))
             (streaming-message-set-cancelled! sm)
             (emit! bus
                    session-id
                    turn-id
                    "turn.cancelled"
                    (hasheq 'reason "cancellation-token")
                    #:state state)]
            [(streaming-message-blocked? sm)
             ;; message-update hook blocked -- emit model.stream.completed then stop streaming
             (emit! bus
                    session-id
                    turn-id
                    "model.stream.completed"
                    (hasheq 'usage (hasheq))
                    #:state state)]
            [else (stream-loop)])))))

  ;; v0.15.2 (BUG-SILENT-STREAM-EOF): If the stream closed without sending
  ;; a done chunk, emit a synthetic model.stream.completed with finish_reason
  ;; "eof" and truncated? #t so downstream consumers are aware.
  (unless (unbox received-done?)
    ;; Only emit if we actually received some data (message was started)
    (when (streaming-message-message-started? sm)
      (emit! bus
             session-id
             turn-id
             "model.stream.completed"
             (hasheq 'usage (hasheq) 'finish_reason "eof" 'truncated? #t)
             #:state state)
      (emit! bus
             session-id
             turn-id
             "message.end"
             (hasheq 'message-id message-id 'usage (hasheq))
             #:state state)))

  (streaming-message->hash sm))

;; ============================================================
;; handle-cancellation
;; ============================================================

;; handle-cancellation : event-bus string string loop-state -> loop-result?
;; Emits turn.cancelled and turn.completed events, returns a cancelled loop-result.
(define (handle-cancellation bus session-id turn-id state #:hook-dispatcher [hook-dispatcher #f])
  ;; #667: Dispatch agent-end hook on cancellation
  (when hook-dispatcher
    (hook-dispatcher
     'agent-end
     (hasheq 'session-id (loop-state-session-id state) 'turn-id turn-id 'termination 'cancelled)))
  (emit! bus
         session-id
         turn-id
         "turn.completed"
         (hasheq 'termination 'cancelled 'turnId turn-id 'reason "cancellation-token")
         #:state state)
  (make-loop-result (loop-state-messages state)
                    'cancelled
                    (hasheq 'turnId turn-id 'reason "cancellation-token")))

;; ============================================================
;; build-stream-result
;; ============================================================

;; build-stream-result : hash (listof hash) event-bus string string loop-state
;;                       (or/c (listof hash?) #f) provider (or/c procedure? #f) -> loop-result?
;; Takes accumulated stream data and builds the final loop-result, handling:
;;   - Tool call accumulation via accumulate-tool-call-deltas
;;   - Usage estimation
;;   - model-response-post hook
;;   - message-end hook (amend/block)
;;   - Building the final assistant message
;;   - Checking for tool calls → completed vs tool-calls-pending
(define (build-stream-result stream-data
                             raw-messages
                             bus
                             session-id
                             turn-id
                             state
                             tools
                             provider
                             hook-dispatcher)
  (define accumulated-text (hash-ref stream-data 'text))
  (define all-chunks (hash-ref stream-data 'all-chunks))

  ;; Build content parts from accumulated stream data
  (define text-part (make-text-part accumulated-text))

  ;; Tool calls from accumulated deltas using the tested accumulation logic
  (define accumulated-tcs (accumulate-tool-call-deltas (reverse all-chunks)))

  (define tool-call-parts
    (for/list ([tc (in-list accumulated-tcs)])
      (make-tool-call-part (hash-ref tc 'id "")
                           (hash-ref tc 'name "")
                           (hash-ref tc 'arguments "{}"))))

  (define content-parts (append (list text-part) tool-call-parts))

  ;; Usage from the last stream chunk that has usage data
  (define stream-usage
    (for/first ([c (in-list all-chunks)]
                #:when (stream-chunk-usage c))
      (stream-chunk-usage c)))

  ;; When provider returns no usage, estimate from message lengths
  (define effective-usage
    (if (usage-empty? stream-usage)
        (let ([est (estimate-turn-tokens raw-messages accumulated-text)])
          (hasheq 'prompt_tokens est 'completion_tokens 0 'total_tokens est 'estimated? #t))
        stream-usage))

  ;; R2-7: model-response-post hook -- dispatch after response received
  ;; Supports pass/amend semantics (block not meaningful for completed response).
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

  ;; Dispatch 'message-end hook -- extensions can amend content or suppress
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

  ;; Mutable text — may be amended by message-end hook
  (define final-text-box (box accumulated-text))

  (handle-hook-result
   msg-end-result
   (lambda (payload)
     ;; message-end blocked -- return completed with empty content
     (emit! bus
            session-id
            turn-id
            "turn.completed"
            (hasheq 'termination 'completed 'turnId turn-id 'reason "message-end-blocked")
            #:state state)
     (make-loop-result (loop-state-messages state)
                       'hook-blocked
                       (hasheq 'turnId turn-id 'hook 'message-end)))
   (lambda ()
     (define final-text (unbox final-text-box))
     ;; Rebuild text-part with potentially amended content
     (define final-text-part (make-text-part final-text))
     (define final-content-parts (append (list final-text-part) tool-call-parts))

     ;; Build assistant message
     (define assistant-msg-id (generate-id))
     (define assistant-msg
       (make-message assistant-msg-id
                     #f
                     'assistant
                     'message
                     final-content-parts
                     (now-seconds)
                     (hasheq 'turnId turn-id 'model "streamed")))

     ;; Check for tool calls
     (cond
       [(null? tool-call-parts)
        ;; No tool calls -- completed turn
        (emit! bus
               session-id
               turn-id
               "assistant.message.completed"
               (hasheq 'messageId assistant-msg-id 'content (text-part-text final-text-part))
               #:state state)
        (state-add-message! state assistant-msg)
        (emit! bus
               session-id
               turn-id
               "turn.completed"
               (hasheq 'termination 'completed 'turnId turn-id)
               #:state state)
        ;; #667: Dispatch agent-end hook on completed turn
        (when hook-dispatcher
          (hook-dispatcher 'agent-end
                           (hasheq 'session-id session-id 'turn-id turn-id 'termination 'completed)))
        (make-loop-result
         (loop-state-messages state)
         'completed
         (hasheq 'turnId turn-id 'usage (or effective-usage (hasheq)) 'model "streamed"))]
       [else
        ;; Tool calls detected -- commit assistant text FIRST, then emit tool.call.started
        ;; Bug B1 fix: emit assistant.message.completed before tool.call.started
        ;; so the TUI commits streaming text to permanent transcript.
        (emit! bus
               session-id
               turn-id
               "assistant.message.completed"
               (hasheq 'messageId assistant-msg-id 'content (text-part-text final-text-part))
               #:state state)
        (for ([tc (in-list tool-call-parts)])
          (emit! bus
                 session-id
                 turn-id
                 "tool.call.started"
                 (hasheq 'id
                         (tool-call-part-id tc)
                         'name
                         (tool-call-part-name tc)
                         'arguments
                         (tool-call-part-arguments tc))
                 #:state state))
        (state-add-message! state assistant-msg)
        (emit! bus
               session-id
               turn-id
               "turn.completed"
               (hasheq 'termination 'tool-calls-pending 'turnId turn-id)
               #:state state)
        ;; #667: Dispatch agent-end hook on tool-calls-pending turn
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
   #:on-amend (lambda (payload)
                (set-box! final-text-box (hash-ref payload 'content accumulated-text)))))
