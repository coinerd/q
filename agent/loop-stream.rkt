#lang racket/base

;; agent/loop-stream.rkt — Streaming, cancellation, and result building
;;
;; Encapsulates the provider streaming loop, cancellation cleanup,
;; and post-stream result assembly for the agent loop.
;;
;; Extracted from loop.rkt (decomposition step).
;; v0.32.3: Migrated from raw emit! to emit-typed-event! with typed structs.
;; v0.32.4: Replaced CPS handle-hook-result with classify-hook-result + match.

(require racket/match
         racket/string
         "../llm/provider.rkt"
         "../llm/model.rkt"
         "streaming-message.rkt"
         "state.rkt"
         "../util/protocol-types.rkt"
         "../util/ids.rkt"
         (only-in "../util/cancellation.rkt" cancellation-token? cancellation-token-cancelled?)
         (only-in "../util/hook-types.rkt" hook-result? hook-result-action hook-result-payload)
         (only-in "../llm/stream.rkt" accumulate-tool-call-deltas)
         (only-in "../llm/token-budget.rkt" estimate-turn-tokens)
         (only-in "event-emitter.rkt" emit-typed-event!)
         ;; v0.45.10 NF3: emit-session-event! for empty-response warning
         (only-in "../runtime/runtime-helpers.rkt" emit-session-event!)
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
         (only-in "loop-messages.rkt" usage-empty? classify-hook-result))

;; ============================================================
;; Configurable chunk limit (v0.12.3 Wave 0.1)
;; ============================================================

(define MAX-STREAM-CHUNKS (make-parameter 10000))

(provide MAX-STREAM-CHUNKS
         accumulate-stream-chunks
         stream-from-provider
         handle-cancellation
         build-stream-result)

;; ============================================================
;; accumulate-stream-chunks : pure helper (S11-F1)
;; ============================================================

;; Given a list of stream-chunks, return accumulated:
;;   'text, 'tool-calls, 'thinking, 'usage, 'finish-reason
(define (accumulate-stream-chunks chunks)
  (define text-parts
    (for/list ([c (in-list chunks)]
               #:when (stream-chunk-delta-text c))
      (stream-chunk-delta-text c)))
  (define thinking-parts
    (for/list ([c (in-list chunks)]
               #:when (stream-chunk-delta-thinking c))
      (stream-chunk-delta-thinking c)))
  (define tool-calls (accumulate-tool-call-deltas chunks))
  (define usage
    (for/first ([c (in-list chunks)]
                #:when (stream-chunk-usage c))
      (stream-chunk-usage c)))
  (define finish-reason
    (for/first ([c (in-list chunks)]
                #:when (stream-chunk-finish-reason c))
      (stream-chunk-finish-reason c)))
  (hasheq 'text
          (apply string-append text-parts)
          'thinking
          (apply string-append thinking-parts)
          'tool-calls
          tool-calls
          'usage
          usage
          'finish-reason
          finish-reason))

;; ============================================================
;; stream-from-provider
;; ============================================================

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
  (define received-done? (box #f))

  ;; Error boundary — emit cleanup events on provider crash.
  (with-handlers ([exn:fail?
                   (lambda (e)
                     ;; AF5 (RC2): Persist partial assistant message before re-raising.
                     ;; Prevents data loss when stream times out after receiving content.
                     (define partial-text (streaming-message-text sm))
                     (when (and partial-text (> (string-length partial-text) 0))
                       (define partial-msg
                         (make-message (generate-id)
                                       #f
                                       'assistant
                                       'message
                                       (list (make-text-part partial-text))
                                       (now-seconds)
                                       (hasheq 'turnId turn-id 'partial #t)))
                       (state-add-message! state partial-msg))
                     (when (streaming-message-message-started? sm)
                       (emit-typed-event! bus
                                          (make-stream-message-end-event #:session-id session-id
                                                                         #:turn-id turn-id
                                                                         #:message-id message-id
                                                                         #:usage (hasheq))
                                          #:state state))
                     (emit-typed-event! bus
                                        (make-stream-turn-completed-event #:session-id session-id
                                                                          #:turn-id turn-id
                                                                          #:termination 'error
                                                                          #:turn-id-str turn-id
                                                                          #:reason
                                                                          "provider-stream-error")
                                        #:state state)
                     (raise e))])
    (let stream-loop ()
      (define chunk (stream-gen))
      (when (and chunk (not (eq? chunk #f)))
        (set-box! chunk-count (+ 1 (unbox chunk-count)))
        (when (> (unbox chunk-count) limit)
          (log-warning
           (format "stream-from-provider: exceeded ~a chunks, truncating session=~a turn=~a"
                   limit
                   session-id
                   turn-id))
          (emit-typed-event! bus
                             (make-stream-completed-event #:session-id session-id
                                                          #:turn-id turn-id
                                                          #:usage (hasheq)
                                                          #:finish_reason "unknown"
                                                          #:truncated? #t)
                             #:state state)
          (when (streaming-message-message-started? sm)
            (emit-typed-event! bus
                               (make-stream-message-end-event #:session-id session-id
                                                              #:turn-id turn-id
                                                              #:message-id message-id
                                                              #:usage (hasheq))
                               #:state state)))
        (unless (> (unbox chunk-count) limit)
          (streaming-message-append-chunk! sm chunk)
          (when (stream-chunk-delta-text chunk)
            ;; Emit message.start on first text delta
            (unless (streaming-message-message-started? sm)
              (streaming-message-set-message-started! sm)
              (emit-typed-event! bus
                                 (make-stream-message-start-event #:session-id session-id
                                                                  #:turn-id turn-id
                                                                  #:message-id message-id)
                                 #:state state))
            ;; Text delta
            (streaming-message-append-text! sm (stream-chunk-delta-text chunk))
            (emit-typed-event! bus
                               (make-stream-delta-event #:session-id session-id
                                                        #:turn-id turn-id
                                                        #:delta (stream-chunk-delta-text chunk))
                               #:state state)
            ;; Emit message.delta with message-id for extension consumption
            (emit-typed-event! bus
                               (make-stream-message-delta-event #:session-id session-id
                                                                #:turn-id turn-id
                                                                #:text (stream-chunk-delta-text chunk)
                                                                #:message-id message-id)
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
              (match (classify-hook-result update-result)
                [(list 'block _) (streaming-message-set-blocked! sm)]
                [_ (void)])))
          ;; FEAT-72: Thinking/reasoning delta
          (when (stream-chunk-delta-thinking chunk)
            (streaming-message-append-thinking! sm (stream-chunk-delta-thinking chunk))
            (emit-typed-event! bus
                               (make-stream-thinking-event #:session-id session-id
                                                           #:turn-id turn-id
                                                           #:delta
                                                           (stream-chunk-delta-thinking chunk))
                               #:state state))
          (when (stream-chunk-delta-tool-call chunk)
            (define tc-delta (stream-chunk-delta-tool-call chunk))
            (streaming-message-append-tool-call! sm tc-delta)
            (emit-typed-event! bus
                               (make-stream-tool-call-delta-event #:session-id session-id
                                                                  #:turn-id turn-id
                                                                  #:delta-tool-call tc-delta)
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
              (match (classify-hook-result update-result)
                [(list 'block _) (streaming-message-set-blocked! sm)]
                [_ (void)])))
          (when (stream-chunk-done? chunk)
            (set-box! received-done? #t)
            (emit-typed-event! bus
                               (make-stream-completed-event
                                #:session-id session-id
                                #:turn-id turn-id
                                #:usage (or (stream-chunk-usage chunk) (hasheq))
                                #:finish_reason (or (stream-chunk-finish-reason chunk) "unknown"))
                               #:state state)
            (when (streaming-message-message-started? sm)
              (emit-typed-event! bus
                                 (make-stream-message-end-event #:session-id session-id
                                                                #:turn-id turn-id
                                                                #:message-id message-id
                                                                #:usage (or (stream-chunk-usage chunk)
                                                                            (hasheq)))
                                 #:state state)))
          ;; Check cancellation after processing chunk
          (cond
            [(and cancellation-token (cancellation-token-cancelled? cancellation-token))
             (streaming-message-set-cancelled! sm)
             (emit-typed-event! bus
                                (make-stream-turn-cancelled-event #:session-id session-id
                                                                  #:turn-id turn-id
                                                                  #:reason "cancellation-token")
                                #:state state)]
            [(streaming-message-blocked? sm)
             (emit-typed-event! bus
                                (make-stream-completed-event #:session-id session-id
                                                             #:turn-id turn-id
                                                             #:usage (hasheq)
                                                             #:finish_reason "unknown")
                                #:state state)]
            [else (stream-loop)])))))

  ;; BUG-SILENT-STREAM-EOF: synthetic completion for streams without done chunk
  (unless (unbox received-done?)
    (when (streaming-message-message-started? sm)
      (emit-typed-event! bus
                         (make-stream-completed-event #:session-id session-id
                                                      #:turn-id turn-id
                                                      #:usage (hasheq)
                                                      #:finish_reason "eof"
                                                      #:truncated? #t)
                         #:state state)
      (emit-typed-event! bus
                         (make-stream-message-end-event #:session-id session-id
                                                        #:turn-id turn-id
                                                        #:message-id message-id
                                                        #:usage (hasheq))
                         #:state state)))

  (streaming-message->hash sm))

;; ============================================================
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
