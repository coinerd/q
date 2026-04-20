#lang racket

;; tests/test-loop-edge-cases.rkt — Wave 8 + v0.12.3 Wave 0 edge case tests
;;
;; Tests edge cases in the agent loop:
;; - A4: Stream blocked on first chunk (tool-call delta, no text)
;; - A5: message-end hook returns 'amend with empty content + tool calls
;; - A6: model-response-post hook raises exception
;; - 0.1a/b/c: Stream error boundary + chunk limit (v0.12.3 Wave 0)

(require rackunit
         rackunit/text-ui
         racket/generator
         "../util/protocol-types.rkt"
         "../util/hook-types.rkt"
         "../agent/event-bus.rkt"
         "../agent/loop.rkt"
         "../llm/model.rkt"
         "../llm/provider.rkt")

(define edge-case-tests
  (test-suite "Agent Loop Edge Case Tests"

    ;; ============================================================
    ;; A4: Stream with tool-call delta, no text at all
    ;; ============================================================
    (test-case "A4: stream with only tool-call delta, no text"
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
           (list (make-stream-chunk
                  #f
                  (hasheq 'index 0 'id "tc-1" 'function (hasheq 'name "read" 'arguments "/tmp/file"))
                  #f
                  #f)
                 (make-stream-chunk #f
                                    #f
                                    (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)
                                    #t)))))

      (define ctx
        (list (make-message "m-1"
                            #f
                            'user
                            'message
                            (list (make-text-part "Read the file"))
                            1000
                            '#hash())))
      (define result (run-agent-turn ctx prov bus #:session-id "s1" #:turn-id "t1"))
      (define evts (reverse (unbox events)))
      (define evt-names (map event-event evts))

      ;; Should complete normally
      (check-not-false (member "turn.completed" evt-names) "turn.completed emitted")

      ;; No text delta was emitted (no message.start because no text)
      (check-false (member "message.start" evt-names) "no message.start for tool-only stream")
      ;; Should have tool-calls-pending status
      (check-equal? (loop-result-termination-reason result)
                    'tool-calls-pending
                    "result has tool-calls-pending"))

    ;; ============================================================
    ;; A5: Empty stream (no content, no tool calls) completes normally
    ;; ============================================================
    (test-case "A5: empty stream (no content, no tool calls) completes"
      (define events (box '()))
      (define bus (make-event-bus))
      (subscribe! bus (lambda (e) (set-box! events (cons e (unbox events)))))

      ;; Provider that returns empty stream
      (define prov
        (make-provider (lambda () "empty-mock")
                       (lambda () (hash 'streaming #t 'token-counting #t))
                       (lambda (req) (make-model-response '() (hasheq) "mock" 'stop))
                       (lambda (req)
                         (list (make-stream-chunk
                                #f
                                #f
                                (hasheq 'prompt-tokens 5 'completion-tokens 0 'total-tokens 5)
                                #t)))))

      (define ctx
        (list (make-message "m-1" #f 'user 'message (list (make-text-part "hi")) 1000 '#hash())))
      (define result (run-agent-turn ctx prov bus #:session-id "s1" #:turn-id "t1"))
      (define evts (reverse (unbox events)))
      (define evt-names (map event-event evts))

      ;; Should complete normally
      (check-not-false (member "turn.completed" evt-names) "turn.completed emitted on empty stream")
      (check-equal? (loop-result-termination-reason result) 'completed "result status is completed")
      ;; No message events (no text content)
      (check-false (member "message.start" evt-names) "no message.start for empty stream"))

    ;; ============================================================
    ;; A6: model-response-post hook raises exception
    ;; ============================================================
    (test-case "A6: hook exception during post-processing"
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
        (make-provider (lambda () "failing-hook-mock")
                       (lambda () (hash 'streaming #t 'token-counting #t))
                       (lambda (req) (make-model-response '() (hasheq) "mock" 'stop))
                       (lambda (req)
                         (list (make-stream-chunk "Hello" #f #f #f)
                               (make-stream-chunk
                                #f
                                #f
                                (hasheq 'prompt-tokens 5 'completion-tokens 2 'total-tokens 7)
                                #t)))))

      (define ctx
        (list (make-message "m-1" #f 'user 'message (list (make-text-part "hi")) 1000 '#hash())))

      ;; The hook exception should propagate (unhandled) — this documents
      ;; current behavior: hook failures are not caught by the loop
      (check-exn exn:fail?
                 (lambda ()
                   (run-agent-turn ctx
                                   prov
                                   bus
                                   #:session-id "s1"
                                   #:turn-id "t1"
                                   #:hook-dispatcher failing-hook-dispatcher))))))

;; ============================================================
;; v0.12.3 Wave 0.1: Stream error boundary + chunk limit
;; ============================================================
;; These tests verify that:
;; - Provider crashes mid-stream emit cleanup events (message.end, turn.completed)
;; - Infinite streams are broken by the chunk limit
;; - Provider crashes before any text don't emit message.start but still emit turn.completed

(define stream-safety-tests
  (test-suite "Stream Error Boundary + Chunk Limit"

    ;; ============================================================
    ;; 0.1a: Provider throws mid-stream → cleanup events emitted
    ;; ============================================================
    (test-case "0.1a: provider throws mid-stream, cleanup events emitted"
      (define events (box '()))
      (define bus (make-event-bus))
      (subscribe! bus (lambda (e) (set-box! events (cons e (unbox events)))))

      ;; Provider that yields one text chunk then throws on second call
      (define call-count (box 0))
      (define prov
        (make-provider (lambda () "crashing-mock")
                       (lambda () (hash 'streaming #t 'token-counting #t))
                       (lambda (req) (make-model-response '() (hasheq) "mock" 'stop))
                       (lambda (req)
                         ;; Return a generator that crashes on 2nd yield
                         (generator ()
                                    (set-box! call-count (+ 1 (unbox call-count)))
                                    (yield (make-stream-chunk "Hello" #f #f #f))
                                    (set-box! call-count (+ 1 (unbox call-count)))
                                    (yield (raise (exn:fail "simulated crash"
                                                            (current-continuation-marks))))))))

      (define ctx
        (list (make-message "m-1" #f 'user 'message (list (make-text-part "hi")) 1000 '#hash())))

      ;; The provider error should propagate, but cleanup events must be emitted
      (check-exn exn:fail? (lambda () (run-agent-turn ctx prov bus #:session-id "s1" #:turn-id "t1")))

      (define evts (reverse (unbox events)))
      (define evt-names (map event-event evts))

      ;; message.start should have been emitted (first chunk had text)
      (check-not-false (member "message.start" evt-names) "message.start emitted before crash")
      ;; message.end must be emitted as cleanup
      (check-not-false (member "message.end" evt-names) "message.end emitted as cleanup after crash")
      ;; turn.completed must be emitted as cleanup
      (check-not-false (member "turn.completed" evt-names)
                       "turn.completed emitted as cleanup after crash"))

    ;; ============================================================
    ;; 0.1b: Provider that never sends done → chunk limit breaks out
    ;; ============================================================
    (test-case "0.1b: infinite stream hits chunk limit and terminates"
      (define events (box '()))
      (define bus (make-event-bus))
      (subscribe! bus (lambda (e) (set-box! events (cons e (unbox events)))))

      ;; Provider that yields text chunks forever, never sends done?=#t
      (define call-count (box 0))
      (define prov
        (make-provider (lambda () "infinite-mock")
                       (lambda () (hash 'streaming #t 'token-counting #t))
                       (lambda (req) (make-model-response '() (hasheq) "mock" 'stop))
                       (lambda (req)
                         (generator ()
                                    (let loop ()
                                      (set-box! call-count (+ 1 (unbox call-count)))
                                      (yield (make-stream-chunk "x" #f #f #f))
                                      (loop))))))

      ;; Use parameterized small chunk limit for testing
      (parameterize ([MAX-STREAM-CHUNKS 100])
        (define ctx
          (list (make-message "m-1" #f 'user 'message (list (make-text-part "hi")) 1000 '#hash())))
        (define result (run-agent-turn ctx prov bus #:session-id "s1" #:turn-id "t1"))

        ;; Should complete (not hang forever)
        (check-equal? (loop-result-termination-reason result)
                      'completed
                      "turn completes even with infinite stream")

        (define evts (reverse (unbox events)))
        (define evt-names (map event-event evts))
        ;; Should have message.start, message.end, turn.completed
        (check-not-false (member "message.start" evt-names))
        (check-not-false (member "message.end" evt-names))
        (check-not-false (member "turn.completed" evt-names))
        ;; Chunk count should be limited
        (check-true (<= (unbox call-count) 105)
                    (format "chunk count ~a is within limit" (unbox call-count)))))

    ;; ============================================================
    ;; 0.1c: Provider throws before first text → no message.start
    ;; ============================================================
    (test-case "0.1c: provider throws before any text, no message.start"
      (define events (box '()))
      (define bus (make-event-bus))
      (subscribe! bus (lambda (e) (set-box! events (cons e (unbox events)))))

      (define prov
        (make-provider (lambda () "early-crash-mock")
                       (lambda () (hash 'streaming #t 'token-counting #t))
                       (lambda (req) (make-model-response '() (hasheq) "mock" 'stop))
                       (lambda (req)
                         (generator ()
                                    (yield (raise (exn:fail "crash on first chunk"
                                                            (current-continuation-marks))))))))

      (define ctx
        (list (make-message "m-1" #f 'user 'message (list (make-text-part "hi")) 1000 '#hash())))

      (check-exn exn:fail? (lambda () (run-agent-turn ctx prov bus #:session-id "s1" #:turn-id "t1")))

      (define evts (reverse (unbox events)))
      (define evt-names (map event-event evts))

      ;; No message.start was emitted
      (check-false (member "message.start" evt-names) "no message.start when crash before text")
      ;; But turn.completed should still be emitted as cleanup
      (check-not-false (member "turn.completed" evt-names) "turn.completed emitted as cleanup"))))

(module+ main
  (run-tests edge-case-tests)
  (run-tests stream-safety-tests))

(module+ test
  (run-tests edge-case-tests)
  (run-tests stream-safety-tests))
