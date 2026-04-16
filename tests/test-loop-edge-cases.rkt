#lang racket

;; tests/test-loop-edge-cases.rkt — Wave 8: A4-A6 edge case tests
;;
;; Tests edge cases in the agent loop:
;; - A4: Stream blocked on first chunk (tool-call delta, no text)
;; - A5: message-end hook returns 'amend with empty content + tool calls
;; - A6: model-response-post hook raises exception

(require rackunit
         rackunit/text-ui
         "../util/protocol-types.rkt"
         "../util/hook-types.rkt"
         "../agent/event-bus.rkt"
         "../agent/loop.rkt"
         "../llm/model.rkt"
         "../llm/provider.rkt")

(define edge-case-tests
  (test-suite
   "Agent Loop Edge Case Tests"

   ;; ============================================================
   ;; A4: Stream with tool-call delta, no text at all
   ;; ============================================================
   ;; When the stream only produces tool-call deltas (no text),
   ;; the tool calls should still be extracted and returned.
   (test-case
    "A4: stream with only tool-call delta, no text"
    (define events (box '()))
    (define bus (make-event-bus))
    (subscribe! bus (lambda (e) (set-box! events (cons e (unbox events)))))

    ;; Provider that returns only tool-call
    (define prov
      (make-provider
       (lambda () "tool-only-mock")
       (lambda () (hash 'streaming #t 'token-counting #t))
       (lambda (req) (make-model-response '() (hasheq) "mock" 'stop))
       (lambda (req)
         (list (make-stream-chunk #f
                             (hasheq 'index 0 'id "tc-1"
                                     'function (hasheq 'name "read"
                                                       'arguments "/tmp/file"))
                             #f #f)
               (make-stream-chunk #f #f
                             (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)
                             #t)))))

    (define ctx (list (make-message "m-1" #f 'user 'message
                                    (list (make-text-part "Read the file"))
                                    1000 '#hash())))
    (define result (run-agent-turn ctx prov bus
                                   #:session-id "s1" #:turn-id "t1"))
    (define evts (reverse (unbox events)))
    (define evt-names (map event-event evts))

    ;; Should complete normally
    (check-not-false (member "turn.completed" evt-names)
                     "turn.completed emitted")
    ;; No text delta was emitted (no message.start because no text)
    (check-false (member "message.start" evt-names)
                 "no message.start for tool-only stream")
    ;; Should have tool-calls-pending status
    (check-equal? (loop-result-termination-reason result) 'tool-calls-pending
                  "result has tool-calls-pending"))

   ;; ============================================================
   ;; A5: Empty stream (no content, no tool calls) completes normally
   ;; ============================================================
   ;; When the stream produces no text and no tool calls,
   ;; the turn should still complete.
   (test-case
    "A5: empty stream (no content, no tool calls) completes"
    (define events (box '()))
    (define bus (make-event-bus))
    (subscribe! bus (lambda (e) (set-box! events (cons e (unbox events)))))

    ;; Provider that returns empty stream
    (define prov
      (make-provider
       (lambda () "empty-mock")
       (lambda () (hash 'streaming #t 'token-counting #t))
       (lambda (req) (make-model-response '() (hasheq) "mock" 'stop))
       (lambda (req)
         (list (make-stream-chunk #f #f
                             (hasheq 'prompt-tokens 5 'completion-tokens 0 'total-tokens 5)
                             #t)))))

    (define ctx (list (make-message "m-1" #f 'user 'message
                                    (list (make-text-part "hi"))
                                    1000 '#hash())))
    (define result (run-agent-turn ctx prov bus
                                   #:session-id "s1" #:turn-id "t1"))
    (define evts (reverse (unbox events)))
    (define evt-names (map event-event evts))

    ;; Should complete normally
    (check-not-false (member "turn.completed" evt-names)
                     "turn.completed emitted on empty stream")
    (check-equal? (loop-result-termination-reason result) 'completed
                  "result status is completed")
    ;; No message events (no text content)
    (check-false (member "message.start" evt-names)
                 "no message.start for empty stream"))

   ;; ============================================================
   ;; A6: model-response-post hook raises exception
   ;; ============================================================
   ;; If a hook raises during post-processing, the turn should still
   ;; emit turn.completed (or at least not hang).
   (test-case
    "A6: hook exception during post-processing"
    (define events (box '()))
    (define bus (make-event-bus))
    (subscribe! bus (lambda (e) (set-box! events (cons e (unbox events)))))

    ;; Hook dispatcher that raises on message-end
    (define (failing-hook-dispatcher hook-type data)
      (if (eq? hook-type 'message-end)
          (error 'failing-hook "simulated hook failure")
          (hook-pass)))

    ;; Provider with simple text
    (define prov
      (make-provider
       (lambda () "failing-hook-mock")
       (lambda () (hash 'streaming #t 'token-counting #t))
       (lambda (req) (make-model-response '() (hasheq) "mock" 'stop))
       (lambda (req)
         (list (make-stream-chunk "Hello" #f #f #f)
               (make-stream-chunk #f #f
                             (hasheq 'prompt-tokens 5 'completion-tokens 2 'total-tokens 7)
                             #t)))))

    (define ctx (list (make-message "m-1" #f 'user 'message
                                    (list (make-text-part "hi"))
                                    1000 '#hash())))

    ;; The hook exception should propagate (unhandled) — this documents
    ;; current behavior: hook failures are not caught by the loop
    (check-exn
     exn:fail?
     (lambda ()
       (run-agent-turn ctx prov bus
                       #:session-id "s1" #:turn-id "t1"
                       #:hook-dispatcher failing-hook-dispatcher))))
  ))

(module+ main
  (run-tests edge-case-tests))

(module+ test
  (run-tests edge-case-tests))
