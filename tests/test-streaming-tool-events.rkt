#lang racket

;; tests/test-streaming-tool-events.rkt — Wave 3: Agent loop integration tests
;;
;; Verifies the B1 fix at the agent-loop level: when a streaming response
;; contains both text and tool calls, assistant.message.completed must be
;; emitted BEFORE tool.call.started, so the TUI can commit streaming text
;; to the permanent transcript before the tool indicator appears.

(require rackunit
         rackunit/text-ui
         "../agent/loop.rkt"
         "../agent/event-bus.rkt"
         "../llm/provider.rkt"
         "../llm/model.rkt"
         "../agent/types.rkt"
         "../util/protocol-types.rkt")

(define streaming-tool-event-tests
  (test-suite
   "Agent Loop Streaming → Tool Event Ordering"

   ;; Test 1: Text+tool response emits assistant.message.completed before tool.call.started
   (test-case
    "text+tool response: assistant.message.completed before tool.call.started"
    (define bus (make-event-bus))
    (define captured (box '()))

    (subscribe! bus (lambda (evt)
                      (set-box! captured
                                (append (unbox captured)
                                        (list (event-ev evt))))))

    ;; Provider that streams text then a tool call
    (define prov
      (make-provider
       (lambda () "mock")
       (lambda () (hasheq 'streaming #t))
       (lambda (req) (error 'send "not used"))
       (lambda (req)
         (list (stream-chunk "Reading file..." #f #f #f)
               (stream-chunk
                #f
                (hasheq 'id "tc-1" 'index 0
                        'function (hasheq 'name "read"
                                          'arguments "{\"path\":\"foo.rkt\"}"))
                #f #f)
               (stream-chunk #f #f #f #t)))))

    (define ctx (list (make-message "m-1" #f 'user 'message
                                    (list (make-text-part "read foo"))
                                    1000 '#hash())))

    (define result (run-agent-turn ctx prov bus
                                   #:session-id "s1" #:turn-id "t1"
                                   #:tools (list (hasheq 'type "function"
                                                         'function (hasheq 'name "read"
                                                                           'parameters (hasheq))))))

    ;; Verify termination
    (check-equal? (loop-result-termination-reason result) 'tool-calls-pending
                  "turn should be tool-calls-pending")

    ;; Verify event sequence
    (define events (unbox captured))
    (check-equal? events
                  '("turn.started"
                    "model.stream.delta"
                    "assistant.message.completed"
                    "tool.call.started"
                    "turn.completed")
                  "events must be: turn.started, delta, assistant.completed, tool.started, turn.completed")

    ;; Specifically verify B1: assistant.message.completed comes before tool.call.started
    (define amc-idx (index-of events "assistant.message.completed"))
    (define tcs-idx (index-of events "tool.call.started"))
    (check-true (and amc-idx tcs-idx (< amc-idx tcs-idx))
                "assistant.message.completed index must be < tool.call.started index"))

   ;; Test 2: Text-only response emits assistant.message.completed but no tool events
   (test-case
    "text-only response: no tool events emitted"
    (define bus (make-event-bus))
    (define captured (box '()))

    (subscribe! bus (lambda (evt)
                      (set-box! captured
                                (append (unbox captured)
                                        (list (event-ev evt))))))

    (define prov
      (make-provider
       (lambda () "mock")
       (lambda () (hasheq 'streaming #t))
       (lambda (req) (error 'send "not used"))
       (lambda (req)
         (list (stream-chunk "Hello" #f #f #f)
               (stream-chunk " world" #f #f #f)
               (stream-chunk #f #f #f #t)))))

    (define ctx (list (make-message "m-1" #f 'user 'message
                                    (list (make-text-part "hi"))
                                    1000 '#hash())))

    (define result (run-agent-turn ctx prov bus
                                   #:session-id "s1" #:turn-id "t1"))

    (check-equal? (loop-result-termination-reason result) 'completed
                  "turn should complete normally")

    (define events (unbox captured))
    (check-false (member "tool.call.started" events)
                 "no tool.call.started for text-only response")
    (check-true (and (member "assistant.message.completed" events) #t)
                "assistant.message.completed must be present"))

   ;; Test 3: Multiple tool calls — all tool.call.started after assistant.message.completed
   (test-case
    "multiple tool calls: all tool.call.started after assistant.message.completed"
    (define bus (make-event-bus))
    (define captured (box '()))

    (subscribe! bus (lambda (evt)
                      (set-box! captured
                                (append (unbox captured)
                                        (list (event-ev evt))))))

    (define prov
      (make-provider
       (lambda () "mock")
       (lambda () (hasheq 'streaming #t))
       (lambda (req) (error 'send "not used"))
       (lambda (req)
         (list (stream-chunk "Need info" #f #f #f)
               (stream-chunk
                #f
                (hasheq 'id "tc-1" 'index 0
                        'function (hasheq 'name "read"
                                          'arguments "{\"path\":\"a\"}"))
                #f #f)
               (stream-chunk
                #f
                (hasheq 'id "tc-2" 'index 1
                        'function (hasheq 'name "bash"
                                          'arguments "{\"command\":\"ls\"}"))
                #f #f)
               (stream-chunk #f #f #f #t)))))

    (define ctx (list (make-message "m-1" #f 'user 'message
                                    (list (make-text-part "go"))
                                    1000 '#hash())))

    (define result (run-agent-turn ctx prov bus
                                   #:session-id "s1" #:turn-id "t1"
                                   #:tools (list (hasheq 'type "function"
                                                         'function (hasheq 'name "read"
                                                                           'parameters (hasheq)))
                                                 (hasheq 'type "function"
                                                         'function (hasheq 'name "bash"
                                                                           'parameters (hasheq))))))

    (check-equal? (loop-result-termination-reason result) 'tool-calls-pending)

    (define events (unbox captured))
    (define amc-idx (index-of events "assistant.message.completed"))
    (define first-tcs (index-of events "tool.call.started"))

    ;; All tool.call.started events come after assistant.message.completed
    (check-true (and amc-idx first-tcs (< amc-idx first-tcs))
                "assistant.message.completed before first tool.call.started")

    ;; Count tool.call.started events
    (define tcs-count (count (lambda (e) (equal? e "tool.call.started")) events))
    (check-equal? tcs-count 2 "two tool.call.started events"))

   ;; Test 4: tool-call-only response (no text) still emits assistant.message.completed
   (test-case
    "tool-call-only response: still emits assistant.message.completed"
    (define bus (make-event-bus))
    (define captured (box '()))

    (subscribe! bus (lambda (evt)
                      (set-box! captured
                                (append (unbox captured)
                                        (list (event-ev evt))))))

    (define prov
      (make-provider
       (lambda () "mock")
       (lambda () (hasheq 'streaming #t))
       (lambda (req) (error 'send "not used"))
       (lambda (req)
         (list (stream-chunk
                #f
                (hasheq 'id "tc-1" 'index 0
                        'function (hasheq 'name "bash"
                                          'arguments "{\"command\":\"ls\"}"))
                #f #f)
               (stream-chunk #f #f #f #t)))))

    (define ctx (list (make-message "m-1" #f 'user 'message
                                    (list (make-text-part "go"))
                                    1000 '#hash())))

    (define result (run-agent-turn ctx prov bus
                                   #:session-id "s1" #:turn-id "t1"
                                   #:tools (list (hasheq 'type "function"
                                                         'function (hasheq 'name "bash"
                                                                           'parameters (hasheq))))))

    (check-equal? (loop-result-termination-reason result) 'tool-calls-pending)

    (define events (unbox captured))
    (check-true (and (member "assistant.message.completed" events) #t)
                "assistant.message.completed present even with no text")
    (define amc-idx (index-of events "assistant.message.completed"))
    (define tcs-idx (index-of events "tool.call.started"))
    (check-true (and amc-idx tcs-idx (< amc-idx tcs-idx))
                "assistant.message.completed before tool.call.started even with no text"))

   ;; Test 5: Event payloads contain correct data
   (test-case
    "event payloads contain correct data"
    (define bus (make-event-bus))
    (define captured-events (box '()))

    (subscribe! bus (lambda (evt)
                      (set-box! captured-events
                                (append (unbox captured-events)
                                        (list evt)))))

    (define prov
      (make-provider
       (lambda () "mock")
       (lambda () (hasheq 'streaming #t))
       (lambda (req) (error 'send "not used"))
       (lambda (req)
         (list (stream-chunk "Answer" #f #f #f)
               (stream-chunk
                #f
                (hasheq 'id "tc-42" 'index 0
                        'function (hasheq 'name "read"
                                          'arguments "{\"path\":\"x\"}"))
                #f #f)
               (stream-chunk #f #f #f #t)))))

    (define ctx (list (make-message "m-1" #f 'user 'message
                                    (list (make-text-part "go"))
                                    1000 '#hash())))

    (define result (run-agent-turn ctx prov bus
                                   #:session-id "s1" #:turn-id "t1"
                                   #:tools (list (hasheq 'type "function"
                                                         'function (hasheq 'name "read"
                                                                           'parameters (hasheq))))))

    (define events (unbox captured-events))

    ;; Find specific events
    (define amc-event
      (for/first ([e events] #:when (equal? (event-ev e) "assistant.message.completed")) e))
    (define tcs-event
      (for/first ([e events] #:when (equal? (event-ev e) "tool.call.started")) e))

    ;; Check assistant.message.completed payload
    (check-not-false amc-event "assistant.message.completed event found")
    (check-equal? (hash-ref (event-payload amc-event) 'content) "Answer"
                  "assistant message content correct")

    ;; Check tool.call.started payload
    (check-not-false tcs-event "tool.call.started event found")
    (check-equal? (hash-ref (event-payload tcs-event) 'id) "tc-42"
                  "tool call id correct")
    (check-equal? (hash-ref (event-payload tcs-event) 'name) "read"
                  "tool call name correct"))))

(module+ main
  (run-tests streaming-tool-event-tests))
