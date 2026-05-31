#lang racket/base

;; agent/stream-runner.rkt — Effectful streaming loop
;;
;; Extracted from loop-stream.rkt (v0.53.2 decomposition).
;; Contains the provider streaming loop with event emission and state mutation.

(require racket/contract
         racket/match
         "../llm/model.rkt"
         "../llm/provider.rkt"
         "streaming-message.rkt"
         "state.rkt"
         "../util/protocol-types.rkt"
         "../util/ids.rkt"
         (only-in "../util/cancellation.rkt" cancellation-token? cancellation-token-cancelled?)
         (only-in "../util/hook-types.rkt" hook-result? hook-result-action hook-result-payload)
         (only-in "event-emitter.rkt" emit-typed-event!)
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
                  make-stream-turn-cancelled-event)
         (only-in "loop-messages.rkt" classify-hook-result)
         ;; Chunk limit parameter from stream-reducer module
         (only-in "stream-reducer.rkt" MAX-STREAM-CHUNKS))

(provide (contract-out [stream-from-provider
                        (-> any/c ; provider
                            any/c ; req
                            (or/c any/c #f) ; bus
                            string? ; session-id
                            string? ; turn-id
                            any/c ; state
                            (or/c procedure? #f) ; hook-dispatcher
                            (or/c any/c #f) ; cancellation-token
                            hash?)]))

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
  (with-handlers
      ([exn:fail?
        (lambda (e)
          ;; AF5 (RC2): Persist partial assistant message before re-raising.
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
          ;; v0.75.9: Emit stream-completed so GUI resets current-response-text
          (emit-typed-event! bus
                             (make-stream-completed-event #:session-id session-id #:turn-id turn-id)
                             #:state state)
          (emit-typed-event! bus
                             (make-stream-turn-completed-event #:session-id session-id
                                                               #:turn-id turn-id
                                                               #:termination 'error
                                                               #:turn-id-str turn-id
                                                               #:reason "provider-stream-error")
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
