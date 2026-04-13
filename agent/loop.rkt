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
         (only-in "../util/cancellation.rkt" cancellation-token? cancellation-token-cancelled?)
         (only-in "../util/hook-types.rkt" hook-result? hook-result-action hook-result-payload))

(provide (contract-out [run-agent-turn
                        (->i ([ctx (listof message?)] [prov provider?] [bus event-bus?])
                             (#:session-id [session-id string?]
                              #:turn-id [turn-id string?]
                              #:state [state (or/c loop-state? #f)]
                              #:tools [tools (or/c (listof hash?) #f)]
                              #:cancellation-token [cancellation-token (or/c cancellation-token? #f)]
                              #:hook-dispatcher [hook-dispatcher (or/c procedure? #f)])
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
                            loop-result?)]))

;; ============================================================
;; Helpers (shared utilities)
;; ============================================================

;; Estimate token count from message/response text as fallback.
;; Rough estimate: ~4 chars per token for English text.
(define (estimate-tokens messages response-text)
  (define total-chars
    (+ (for/sum ([m (in-list messages)]) (string-length (format "~a" (hash-ref m 'content ""))))
       (string-length (or response-text ""))))
  (quotient total-chars 4))

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

;; Convert tool-result content to a string for the API.
;; Content may be a list of content-part hashes, plain strings, or nested data.
(define (result-content->string content)
  (cond
    [(string? content) content]
    [(list? content)
     (string-join (for/list ([part (in-list content)])
                    (cond
                      [(string? part) part]
                      [(hash? part) (hash-ref part 'text (format "~a" part))]
                      [else (format "~a" part)]))
                  "\n")]
    [else (format "~a" content)]))

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
(define (stream-from-provider provider
                              req
                              bus
                              session-id
                              turn-id
                              state
                              hook-dispatcher
                              cancellation-token)
  (define accumulated-text (box ""))
  (define accumulated-tool-calls (box '()))
  (define all-chunks (box '()))
  (define cancelled-during-stream (box #f))
  (define stream-blocked (box #f))

  (define stream-gen (provider-stream provider req))

  (let stream-loop ()
    (define chunk (stream-gen))
    (when (and chunk (not (eq? chunk #f)))
      (set-box! all-chunks (cons chunk (unbox all-chunks)))
      (when (stream-chunk-delta-text chunk)
        ;; Text delta
        (set-box! accumulated-text
                  (string-append (unbox accumulated-text) (stream-chunk-delta-text chunk)))
        (emit! bus
               session-id
               turn-id
               "model.stream.delta"
               (hasheq 'delta (stream-chunk-delta-text chunk))
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
            (set-box! stream-blocked #t))))
      (when (stream-chunk-delta-tool-call chunk)
        ;; Tool call delta
        (define tc-delta (stream-chunk-delta-tool-call chunk))
        (set-box! accumulated-tool-calls (append (unbox accumulated-tool-calls) (list tc-delta)))
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
            (set-box! stream-blocked #t))))
      (when (stream-chunk-done? chunk)
        ;; Stream completed
        (emit! bus
               session-id
               turn-id
               "model.stream.completed"
               (hasheq 'usage (or (stream-chunk-usage chunk) (hasheq)))
               #:state state))
      ;; Check cancellation after processing chunk
      (cond
        [(and cancellation-token (cancellation-token-cancelled? cancellation-token))
         (set-box! cancelled-during-stream #t)
         (emit! bus
                session-id
                turn-id
                "turn.cancelled"
                (hasheq 'reason "cancellation-token")
                #:state state)]
        [(unbox stream-blocked) (void)] ;; message-update hook blocked -- stop streaming
        [else (stream-loop)])))

  (hasheq 'text
          (unbox accumulated-text)
          'tool-calls
          (unbox accumulated-tool-calls)
          'all-chunks
          (unbox all-chunks)
          'cancelled?
          (unbox cancelled-during-stream)
          'stream-blocked?
          (unbox stream-blocked)))

;; ============================================================
;; Extracted helper: handle-cancellation
;; ============================================================

;; handle-cancellation : event-bus string string loop-state -> loop-result?
;; Emits turn.cancelled and turn.completed events, returns a cancelled loop-result.
(define (handle-cancellation bus session-id turn-id state)
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
        (let ([est (estimate-tokens raw-messages accumulated-text)])
          (hash 'prompt_tokens est 'completion_tokens 0 'total_tokens est 'estimated? #t))
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
        (make-loop-result
         (loop-state-messages state)
         'completed
         (hasheq 'turnId turn-id 'usage (or effective-usage (hasheq)) 'model "streamed"))]
       [else
        ;; Tool calls detected -- emit tool.call.started for each
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
(define (run-agent-turn context
                        provider
                        bus
                        #:session-id [session-id "session"]
                        #:turn-id [turn-id "turn"]
                        #:state [state #f]
                        #:tools [tools #f]
                        #:cancellation-token [cancellation-token #f]
                        #:hook-dispatcher [hook-dispatcher #f])
  ;; Ensure we have a state for accumulation
  (define st (or state (make-loop-state session-id turn-id)))

  ;; 1. Emit turn.started
  (emit! bus
         session-id
         turn-id
         "turn.started"
         (hasheq 'turnId turn-id 'sessionId session-id)
         #:state st)

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
  (define req (make-model-request raw-messages tools (hasheq)))

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
     ;; Hook blocked the request -- return early
     (emit! bus session-id turn-id "model.request.blocked" (hasheq 'reason "hook"))
     (loop-result raw-messages 'hook-blocked (hasheq 'hook 'model-request-pre))]
    [else
     (emit! bus
            session-id
            turn-id
            "model.request.started"
            (hasheq 'messageCount
                    (length raw-messages)
                    'toolCount
                    (if tools
                        (length tools)
                        0))
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
          [(hash-ref stream-data 'cancelled?) (handle-cancellation bus session-id turn-id st)]
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
