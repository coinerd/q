#lang racket

;; tests/test-loop-cancellation.rkt — Wave 8: A1-A3 cancellation tests
;;
;; Tests cancellation edge cases in the agent loop:
;; - A1: Cancellation between message.start and first text delta
;; - A2: Cancellation during tool-call delta (stream-blocked path)
;; - A3: Cancellation after stream completes but before build-stream-result

(require rackunit
         rackunit/text-ui
         "../util/protocol-types.rkt"
         "../agent/event-bus.rkt"
         "../agent/loop.rkt"
         "../llm/model.rkt"
         "../llm/provider.rkt"
         "../extensions/hooks.rkt"
         "../util/hook-types.rkt"
         "../util/cancellation.rkt")

(define cancellation-tests
  (test-suite
   "Agent Loop Cancellation Tests"

   ;; ============================================================
   ;; A1: Cancellation between message.start and first text delta
   ;; ============================================================
   ;; When cancellation fires after the first text delta (which triggers
   ;; message.start), we should see turn.cancelled emitted.
   (test-case
    "A1: cancellation after first text delta emits turn.cancelled"
    (define events (box '()))
    (define bus (make-event-bus))
    (subscribe! bus (lambda (e) (set-box! events (cons e (unbox events)))))
    (define cancel-tok (make-cancellation-token))

    ;; Provider: emit one text delta, then cancellation fires
    (define prov
      (make-provider
       (lambda () "cancel-mock-a1")
       (lambda () (hash 'streaming #t 'token-counting #t))
       (lambda (req) (make-model-response '() (hasheq) "mock" 'stop))
       ;; Stream: first chunk has text, second chunk is done but cancellation was triggered
       (lambda (req)
         (list (make-stream-chunk "Hello" #f #f #f)
               (begin
                 (cancel-token! cancel-tok)
                 (make-stream-chunk #f #f (hasheq 'prompt-tokens 5 'completion-tokens 2 'total-tokens 7) #t))))))

    (define ctx (list (make-message "m-1" #f 'user 'message
                                    (list (make-text-part "hi"))
                                    1000 '#hash())))
    (define result (run-agent-turn ctx prov bus
                                   #:session-id "s1" #:turn-id "t1"
                                   #:cancellation-token cancel-tok))
    (define evts (reverse (unbox events)))
    (define evt-names (map event-event evts))

    ;; Should have turn.cancelled because cancellation fired mid-stream
    (check-not-false (member "turn.cancelled" evt-names)
                "turn.cancelled emitted after cancellation during stream")
    ;; turn.completed should still be emitted
    (check-not-false (member "turn.completed" evt-names)
                "turn.completed emitted")
    ;; Result should be cancelled
    (check-equal? (loop-result-termination-reason result) 'cancelled
                  "result status is cancelled"))

   ;; ============================================================
   ;; A2: Cancellation during tool-call delta (stream-blocked path)
   ;; ============================================================
   ;; When a message-update hook blocks during tool-call delta,
   ;; the stream-blocked path should emit model.stream.completed.
   (test-case
    "A2: message-update hook block during tool-call delta stops stream"
    (define events (box '()))
    (define bus (make-event-bus))
    (subscribe! bus (lambda (e) (set-box! events (cons e (unbox events)))))

    ;; Hook dispatcher that blocks on tool-call updates
    (define (blocking-hook-dispatcher hook-type data)
      (if (and (eq? hook-type 'message-update)
               (hash-ref data 'delta-tool-call #f))
          (hook-block)
          (hook-pass)))

    ;; Provider that sends a tool-call delta
    (define prov
      (make-provider
       (lambda () "tool-block-mock")
       (lambda () (hash 'streaming #t 'token-counting #t))
       (lambda (req) (make-model-response '() (hasheq) "mock" 'stop))
       (lambda (req)
         (list (make-stream-chunk #f
                             (hasheq 'index 0 'id "tc-1"
                                     'function (hasheq 'name "read"
                                                       'arguments "/tmp/file"))
                             #f #f)
               ;; This won't be reached because stream-blocked stops the loop
               (make-stream-chunk "more" #f #f #f)
               (make-stream-chunk #f #f (hasheq) #t)))))

    (define ctx (list (make-message "m-1" #f 'user 'message
                                    (list (make-text-part "hi"))
                                    1000 '#hash())))
    (define result (run-agent-turn ctx prov bus
                                   #:session-id "s1" #:turn-id "t1"
                                   #:hook-dispatcher blocking-hook-dispatcher))
    (define evts (reverse (unbox events)))
    (define evt-names (map event-event evts))

    ;; Should have model.stream.completed (emitted by stream-blocked path)
    (check-not-false (member "model.stream.completed" evt-names)
                "model.stream.completed emitted on stream-block"))

   ;; ============================================================
   ;; A3: Cancellation after stream completes but turn.cancelled still shows
   ;; ============================================================
   ;; If cancellation fires in the very last chunk, the stream data
   ;; will have cancelled?=#t, so handle-cancellation takes over.
   (test-case
    "A3: cancellation at stream end triggers handle-cancellation"
    (define events (box '()))
    (define bus (make-event-bus))
    (subscribe! bus (lambda (e) (set-box! events (cons e (unbox events)))))
    (define cancel-tok (make-cancellation-token))

    ;; Provider: complete normally, but cancellation fires on done chunk
    (define prov
      (make-provider
       (lambda () "cancel-end-mock")
       (lambda () (hash 'streaming #t 'token-counting #t))
       (lambda (req) (make-model-response '() (hasheq) "mock" 'stop))
       (lambda (req)
         (list (make-stream-chunk "Full response" #f #f #f)
               (begin
                 (cancel-token! cancel-tok)
                 (make-stream-chunk #f #f (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15) #t))))))

    (define ctx (list (make-message "m-1" #f 'user 'message
                                    (list (make-text-part "hi"))
                                    1000 '#hash())))
    (define result (run-agent-turn ctx prov bus
                                   #:session-id "s1" #:turn-id "t1"
                                   #:cancellation-token cancel-tok))
    (define evts (reverse (unbox events)))
    (define evt-names (map event-event evts))

    ;; Stream completed, but cancelled?=#t so handle-cancellation runs
    (check-not-false (member "turn.cancelled" evt-names)
                "turn.cancelled emitted when cancellation at stream end")
    (check-equal? (loop-result-termination-reason result) 'cancelled
                  "result status is cancelled"))))

(module+ main
  (run-tests cancellation-tests))

(module+ test
  (run-tests cancellation-tests))
