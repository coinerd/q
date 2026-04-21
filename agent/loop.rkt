#lang racket/base

;; agent/loop.rkt — pure agent turn execution
;;
;; Core loop that:
;;   1. Emits turn.started event
;;   2. Builds normalized model context
;;   3. Emits context.built event
;;   4. Streams from provider, emitting delta events
;;   5. Detects tool calls in response
;;   6. If no tool calls -> emits assistant.message.completed, turn ends
;;   7. If tool calls -> emits tool.call.started for each, returns tool-calls-pending
;;   8. Emits turn.completed event
;;
;; The loop does NOT execute tools, touch files, load resources, or render UI.
;;
;; Decoupled from tools/tool.rkt and extensions/hooks.rkt:
;;   - #:tools receives pre-formatted OpenAI tool schemas (listof hash?)
;;   - #:hook-dispatcher receives a function that returns hook-result?

(require racket/contract
         racket/string
         racket/list
         racket/date
         "../util/ids.rkt"
         "../util/protocol-types.rkt"
         "event-bus.rkt"
         "state.rkt"
         "../llm/model.rkt"
         "../llm/provider.rkt"
         (only-in "../llm/stream.rkt" accumulate-tool-call-deltas)
         (only-in "../llm/token-budget.rkt" estimate-turn-tokens)
         (only-in "../util/cancellation.rkt" cancellation-token? cancellation-token-cancelled?)
         (only-in "../util/hook-types.rkt" hook-result? hook-result-action hook-result-payload)
         (only-in "../util/content-helpers.rkt" result-content->string)
         "streaming-message.rkt")

;; ============================================================
;; Configurable chunk limit (v0.12.3 Wave 0.1)
;; ============================================================

;; Maximum number of chunks to process from a provider stream.
;; Prevents infinite loops from misbehaving providers.
;; Parameterized for testing (set to small value in tests).
(define MAX-STREAM-CHUNKS (make-parameter 10000))

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
         MAX-STREAM-CHUNKS)

;; ============================================================
;; Helpers (shared utilities)
;; ============================================================

;; estimate-tokens replaced by estimate-turn-tokens from llm/token-budget.rkt
;; result-content->string imported from util/content-helpers.rkt

;; Check whether usage hash is empty/zero (provider didn't return real usage).
(define (usage-empty? u)
  (and (hash? u)
       (or (hash-empty? u)
           (and (zero? (hash-ref u 'prompt_tokens 0)) (zero? (hash-ref u 'completion_tokens 0))))))

;; Extract plain text from a list of content parts (text-part only)
(define (parts->text-string parts)
  (cond
    [(string? parts) parts]
    [(list? parts)
     (string-join (for/list ([p (in-list parts)]
                             #:when (text-part? p))
                    (text-part-text p))
                  "")]
    [else (format "~a" parts)]))

;; result-content->string imported from util/content-helpers.rkt

;; Emit an event on the bus and optionally record in state
(define (emit! bus session-id turn-id event-name payload #:state [state #f])
  (define evt (make-event event-name (now-seconds) session-id turn-id payload))
  (publish! bus evt)
  (when state
    (state-add-event! state evt))
  evt)

;; ============================================================
;; Extracted helper: build-raw-messages
;; ============================================================

;; build-raw-messages : (listof message?) -> (listof hash?)
;; PURE function — converts a list of message? structs into raw OpenAI-format
;; hash messages. No side effects, no bus/state needed.
;;; build-raw-messages : (listof message?) -> (listof hash?)
;;;
;;; Converts internal message structs to raw OpenAI-format message hashes.
;;; Handles all roles:
;;;   'user     -> {role: "user", content: <text>}
;;;   'assistant -> {role: "assistant", content: <text>, tool_calls: [...]}
;;;   'tool     -> {role: "tool", tool_call_id: ..., content: ...}
;;; Pure function — no side effects.
(define (build-raw-messages context)
  (append*
   (for/list ([msg (in-list context)])
     (define role (message-role msg))
     (define parts (message-content msg))
     (cond
       ;; user -> simple text message
       [(eq? role 'user) (list (hasheq 'role "user" 'content (parts->text-string parts)))]

       ;; assistant -> text + optional tool_calls
       [(eq? role 'assistant)
        (define text-parts (filter text-part? parts))
        (define tc-parts (filter tool-call-part? parts))
        (define text-content (parts->text-string text-parts))
        (if (null? tc-parts)
            ;; text-only assistant message
            (list (hasheq 'role "assistant" 'content text-content))
            ;; assistant with tool calls -- OpenAI format
            ;; Note: GLM rejects 'null for content, so we omit the field when empty
            (let* ([tool-calls-list (for/list ([tc (in-list tc-parts)])
                                      (hasheq 'id
                                              (tool-call-part-id tc)
                                              'type
                                              "function"
                                              'function
                                              (hasheq 'name
                                                      (tool-call-part-name tc)
                                                      'arguments
                                                      (tool-call-part-arguments tc))))]
                   [assistant-msg (if (string=? text-content "")
                                      ;; No text content: omit content field (GLM compatible)
                                      (hasheq 'role "assistant" 'tool_calls tool-calls-list)
                                      ;; Has text content: include content field
                                      (hasheq 'role
                                              "assistant"
                                              'content
                                              text-content
                                              'tool_calls
                                              tool-calls-list))])
              (list assistant-msg)))]

       ;; tool -> one OpenAI message per tool-result-part
       [(eq? role 'tool)
        (for/list ([p (in-list parts)]
                   #:when (tool-result-part? p))
          (hasheq 'role
                  "tool"
                  'tool_call_id
                  (tool-result-part-tool-call-id p)
                  'content
                  (result-content->string (tool-result-part-content p))))]

       ;; fallback -- unknown role
       [else (list (hasheq 'role (symbol->string role) 'content (parts->text-string parts)))]))))

;; ============================================================
;; Extracted helper: stream-from-provider
;; ============================================================

;; stream-from-provider : provider model-request event-bus string string loop-state
;;                        (or/c procedure? #f) (or/c cancellation-token? #f) -> hash
;; Wraps the streaming loop. Returns a hash with keys:
;;   'text            — accumulated text string
;;   'tool-calls      — list of accumulated tool-call deltas
;;   'all-chunks      — list of all stream chunks
;;   'cancelled?      — boolean
;;   'stream-blocked? — boolean (from message-update hook block)
;;; stream-from-provider : provider? model-request? event-bus? string? string?
;;;                         loop-state? (or/c procedure? #f) (or/c cancellation-token? #f)
;;;                         -> hash?
;;;
;;; Wraps the streaming loop. Pulls chunks from the provider's stream
;;; generator, emits message.start/delta/end events, supports cancellation
;;; via token and blocking via message-update hook. Returns a hash with
;;; keys: 'text, 'tool-calls, 'all-chunks, 'cancelled?, 'stream-blocked?.
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
              (when (and (hook-result? update-result) (eq? (hook-result-action update-result) 'block))
                (streaming-message-set-blocked! sm))))
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
              (when (and (hook-result? update-result) (eq? (hook-result-action update-result) 'block))
                (streaming-message-set-blocked! sm))))
          (when (stream-chunk-done? chunk)
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

  (streaming-message->hash sm))

;; ============================================================
;; Extracted helper: handle-cancellation
;; ============================================================

;; handle-cancellation : event-bus string string loop-state -> loop-result?
;; Emits turn.cancelled and turn.completed events, returns a cancelled loop-result.
;;; handle-cancellation : event-bus? string? string? loop-state? -> loop-result?
;;;
;;; Cancellation cleanup helper. Dispatches agent-end hook with 'cancelled
;;; termination, emits turn.completed with 'cancelled, and returns a
;;; loop-result with status 'cancelled.
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
;; Extracted helper: build-stream-result
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
;;; build-stream-result : hash? (listof hash?) event-bus? string? string?
;;;                       loop-state? (or/c (listof hash?) #f) provider?
;;;                       (or/c procedure? #f) -> loop-result?
;;;
;;; Post-stream result builder. Extracts text and tool calls from stream
;;; data, resolves usage, dispatches model-response-post and message-end
;;; hooks, builds the final assistant message, and emits completion events.
;;; Returns 'completed if no tool calls, or 'tool-calls-pending if tools
;;; need execution.
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
              (object-name provider)
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

  ;; Handle message-end result -- hook-dispatcher returns (list action payload) or #f
  (define final-text
    (if (and (hook-result? msg-end-result) (eq? (hook-result-action msg-end-result) 'amend))
        (hash-ref (hook-result-payload msg-end-result) 'content accumulated-text)
        accumulated-text))

  (cond
    [(and (hook-result? msg-end-result) (eq? (hook-result-action msg-end-result) 'block))
     ;; message-end blocked -- return completed with empty content
     (emit! bus
            session-id
            turn-id
            "turn.completed"
            (hasheq 'termination 'completed 'turnId turn-id 'reason "message-end-blocked")
            #:state state)
     (make-loop-result (loop-state-messages state)
                       'hook-blocked
                       (hasheq 'turnId turn-id 'hook 'message-end))]
    [else
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
                                  (length tool-call-parts)))])]))

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
;;; run-agent-turn : (listof message?) provider? event-bus?
;;;                  #:session-id string?
;;;                  #:turn-id string?
;;;                  #:state (or/c loop-state? #f)
;;;                  #:tools (or/c (listof hash?) #f)
;;;                  #:cancellation-token (or/c cancellation-token? #f)
;;;                  #:hook-dispatcher (or/c procedure? #f)
;;;                  -> loop-result?
;;;
;;; Main entry point for a single agent turn. Orchestrates the pipeline:
;;;   1. Emits turn.started event
;;;   2. Dispatches agent-start hook
;;;   3. Builds normalized model context via build-raw-messages
;;;   4. Emits context.built event
;;;   5. Streams from provider via stream-from-provider
;;;   6. On cancellation: handle-cancellation
;;;   7. Otherwise: build-stream-result for final assembly
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
  (emit! bus
         session-id
         turn-id
         "turn.started"
         (hasheq 'turnId turn-id 'sessionId session-id)
         #:state st)

  ;; #667: Dispatch 'agent-start hook at LLM call begin
  (when hook-dispatcher
    (hook-dispatcher
     'agent-start
     (hasheq 'session-id session-id 'turn-id turn-id 'message-count (length context))))

  ;; 2. Build normalized model context (pure function)
  (define raw-messages (build-raw-messages context))

  ;; 3. Emit context.built
  (emit! bus
         session-id
         turn-id
         "context.built"
         (hasheq 'messageCount (length raw-messages))
         #:state st)

  ;; 4. Build model-request
  ;; v0.14.4 Wave 2: Pass provider settings (max-tokens etc.) from config
  (define req (make-model-request raw-messages tools (or provider-settings (hasheq))))

  ;; R2-7: model-request-pre hook
  (define pre-hook-result
    (and hook-dispatcher
         (hook-dispatcher 'model-request-pre
                          (hasheq 'model-name
                                  (object-name provider)
                                  'message-count
                                  (length raw-messages)
                                  'messages
                                  raw-messages
                                  'settings
                                  (model-request-settings req)))))

  (cond
    [(and (hook-result? pre-hook-result) (eq? (hook-result-action pre-hook-result) 'block))
     ;; Hook blocked the request -- emit turn.completed then return early
     (emit! bus session-id turn-id "model.request.blocked" (hasheq 'reason "hook"))
     (emit! bus
            session-id
            turn-id
            "turn.completed"
            (hasheq 'termination 'hook-blocked 'turnId turn-id 'reason "model-request-pre-blocked"))
     (loop-result raw-messages 'hook-blocked (hasheq 'hook 'model-request-pre))]
    [else
     ;; v0.15.0 Wave 1: enriched llm.request event for trace logging
     ;; v0.15.1: Use model name from request settings instead of
     ;; (object-name provider) to avoid non-jsexpr struct values.
     (emit! bus
            session-id
            turn-id
            "model.request.started"
            (hasheq 'messageCount
                    (length raw-messages)
                    'toolCount
                    (if tools
                        (length tools)
                        0)
                    'model
                    (hash-ref (model-request-settings req)
                              'model
                              (lambda () (format "~a" (object-name provider))))
                    'max_tokens
                    (hash-ref (model-request-settings req) 'max-tokens #f)
                    'settings
                    (model-request-settings req))
            #:state st)

     ;; Dispatch 'message-start hook
     (define msg-start-result
       (and hook-dispatcher
            (hook-dispatcher 'message-start
                             (hasheq 'session-id
                                     session-id
                                     'turn-id
                                     turn-id
                                     'model-name
                                     (object-name provider)
                                     'message-count
                                     (length raw-messages)))))

     (cond
       [(and (hook-result? msg-start-result) (eq? (hook-result-action msg-start-result) 'block))
        (emit! bus session-id turn-id "message.blocked" (hasheq 'hook 'message-start))
        (emit! bus
               session-id
               turn-id
               "turn.completed"
               (hasheq 'termination 'hook-blocked 'turnId turn-id 'reason "message-start-blocked"))
        (loop-result raw-messages 'hook-blocked (hasheq 'hook 'message-start))]
       [else
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

        ;; -- Cancellation fast-path --
        (cond
          [(hash-ref stream-data 'cancelled?)
           (handle-cancellation bus session-id turn-id st #:hook-dispatcher hook-dispatcher)]
          [else
           ;; 8-9. Build final result from stream data
           (build-stream-result stream-data
                                raw-messages
                                bus
                                session-id
                                turn-id
                                st
                                tools
                                provider
                                hook-dispatcher)])])]))
