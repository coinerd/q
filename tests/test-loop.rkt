#lang racket

;; tests/test-loop.rkt — tests for agent/state.rkt, agent/queue.rkt, agent/loop.rkt

(require rackunit
         "../agent/types.rkt"
         "../agent/event-bus.rkt"
         "../agent/state.rkt"
         "../agent/queue.rkt"
         "../agent/loop.rkt"
         "../llm/model.rkt"
         "../llm/provider.rkt")

;; ============================================================
;; 1. Loop state tests
;; ============================================================

(test-case
 "make-loop-state returns a loop-state"
 (check-true (loop-state? (make-loop-state "sess-1" "turn-1"))))

(test-case
 "fresh loop-state has correct session and turn ids"
 (define st (make-loop-state "sess-1" "turn-1"))
 (check-equal? (loop-state-session-id st) "sess-1")
 (check-equal? (loop-state-turn-id st) "turn-1")
 (check-equal? (loop-state-messages st) '())
 (check-equal? (loop-state-events st) '()))

(test-case
 "state-add-message! adds messages in order"
 (define st (make-loop-state "sess-1" "turn-1"))
 (define msg-1 (make-message "m-1" #f 'user 'message
                              (list (make-text-part "hi"))
                              1000 '#hash()))
 (define msg-2 (make-message "m-2" "m-1" 'assistant 'message
                              (list (make-text-part "hello"))
                              1001 '#hash()))
 (state-add-message! st msg-1)
 (check-equal? (length (loop-state-messages st)) 1)
 (check-equal? (message-id (car (loop-state-messages st))) "m-1")
 (state-add-message! st msg-2)
 (check-equal? (length (loop-state-messages st)) 2)
 (check-equal? (message-id (cadr (loop-state-messages st))) "m-2"))

(test-case
 "state-add-event! adds events"
 (define st (make-loop-state "sess-1" "turn-1"))
 (define evt-1 (make-event "turn.started" 1000 "sess-1" "turn-1" '#hash()))
 (state-add-event! st evt-1)
 (check-equal? (length (loop-state-events st)) 1)
 (check-equal? (event-event (car (loop-state-events st))) "turn.started"))

(test-case
 "separate loop-state instances don't share state"
 (define st (make-loop-state "sess-1" "turn-1"))
 (define msg-1 (make-message "m-1" #f 'user 'message
                              (list (make-text-part "hi"))
                              1000 '#hash()))
 (state-add-message! st msg-1)
 (state-add-message! st (make-message "m-2" "m-1" 'assistant 'message
                                       (list (make-text-part "hello"))
                                       1001 '#hash()))
 (define st2 (make-loop-state "sess-2" "turn-2"))
 (check-equal? (length (loop-state-messages st2)) 0)
 (check-equal? (length (loop-state-messages st)) 2))

;; ============================================================
;; 2. Queue tests
;; ============================================================

(test-case
 "fresh queue is empty; enqueue/dequeue round-trip"
 (define q (make-queue))
 (check-true (queue-empty? q))
 (define msg-1 (make-message "m-1" #f 'user 'message
                              (list (make-text-part "hi"))
                              1000 '#hash()))
 (enqueue-steering! q msg-1)
 (check-false (queue-empty? q))
 (check-equal? (dequeue-steering! q) msg-1)
 (check-true (queue-empty? q)))

;; ============================================================
;; 3. Event bus integration
;; ============================================================

(test-case
 "subscribe and publish events on event bus"
 (define bus (make-event-bus))
 (define collected-events '())
 (subscribe! bus (lambda (evt)
                   (set! collected-events (cons evt collected-events))))
 (publish! bus (make-event "test.event" 1000 "sess-1" "turn-1" (hasheq 'foo "bar")))
 (check-equal? (length collected-events) 1)
 (check-equal? (event-event (car collected-events)) "test.event"))

;; ============================================================
;; 4. BUG-35: GLM 5.1 Error Code 1214 - Assistant messages with tool calls
;; ============================================================

(test-case
 "BUG-35: assistant with tool calls but NO text content"
 (define assistant-msg-no-text
   (make-message "m-3" "m-2" 'assistant 'message
                 (list (make-tool-call-part "tc-1" "bash" "{}"))
                 1002 '#hash()))
 (define content (message-content assistant-msg-no-text))
 (define text-parts (filter text-part? content))
 (define tc-parts (filter tool-call-part? content))
 (check-equal? (length text-parts) 0)
 (check-equal? (length tc-parts) 1)
 (check-equal? (tool-call-part-name (car tc-parts)) "bash"))

(test-case
 "BUG-35: assistant with BOTH text and tool calls"
 (define assistant-msg-with-text
   (make-message "m-4" "m-3" 'assistant 'message
                 (list (make-text-part "I'll help you")
                       (make-tool-call-part "tc-2" "edit" "{}"))
                 1003 '#hash()))
 (define content (message-content assistant-msg-with-text))
 (define text-parts (filter text-part? content))
 (define tc-parts (filter tool-call-part? content))
 (check-equal? (length text-parts) 1)
 (check-equal? (length tc-parts) 1)
 (check-equal? (text-part-text (car text-parts)) "I'll help you"))
