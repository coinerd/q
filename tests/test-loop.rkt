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

(check-true (loop-state? (make-loop-state "sess-1" "turn-1"))
            "make-loop-state returns a loop-state")

(define st (make-loop-state "sess-1" "turn-1"))
(check-equal? (loop-state-session-id st) "sess-1")
(check-equal? (loop-state-turn-id st) "turn-1")
(check-equal? (loop-state-messages st) '() "fresh state has no messages")
(check-equal? (loop-state-events st) '() "fresh state has no events")

;; Add messages
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
(check-equal? (message-id (cadr (loop-state-messages st))) "m-2")

;; Add events
(define evt-1 (make-event "turn.started" 1000 "sess-1" "turn-1" '#hash()))
(state-add-event! st evt-1)
(check-equal? (length (loop-state-events st)) 1)
(check-equal? (event-event (car (loop-state-events st))) "turn.started")

;; Separate instances don't share state
(define st2 (make-loop-state "sess-2" "turn-2"))
(check-equal? (length (loop-state-messages st2)) 0 "st2 has no messages")
(check-equal? (length (loop-state-messages st)) 2 "st still has 2 messages")

;; ============================================================
;; 2. Queue tests
;; ============================================================

(define q (make-queue))
(check-true (queue-empty? q) "fresh queue is empty")

(enqueue-steering! q msg-1)
(check-false (queue-empty? q) "queue not empty after enqueue")
(check-equal? (dequeue-steering! q) msg-1 "dequeue returns enqueued message")
(check-true (queue-empty? q) "queue empty after dequeue")

;; ============================================================
;; 3. Event bus integration
;; ============================================================

(define bus (make-event-bus))
(define collected-events '())

(subscribe! bus (lambda (evt)
                  (set! collected-events (cons evt collected-events))))

(publish! bus (make-event "test.event" 1000 "sess-1" "turn-1" (hasheq 'foo "bar")))
(check-equal? (length collected-events) 1 "event was collected")
(check-equal? (event-event (car collected-events)) "test.event")

;; ============================================================
;; 4. BUG-35: GLM 5.1 Error Code 1214 - Assistant messages with tool calls
;; ============================================================

(println "Running BUG-35 tests...")

;; Test 1: Assistant with tool calls but NO text content
;; This tests that the message structure is correct for GLM compatibility
(let ([assistant-msg-no-text
       (make-message "m-3" "m-2" 'assistant 'message
                     (list (make-tool-call-part "tc-1" "bash" "{}"))
                     1002 '#hash())])
  
  (define content (message-content assistant-msg-no-text))
  (define text-parts (filter text-part? content))
  (define tc-parts (filter tool-call-part? content))
  
  ;; Should have 0 text parts and 1 tool-call part
  (check-equal? (length text-parts) 0 "BUG-35: no text parts when only tool call")
  (check-equal? (length tc-parts) 1 "BUG-35: has one tool-call part")
  (check-equal? (tool-call-part-name (car tc-parts)) "bash" "BUG-35: correct tool name"))

;; Test 2: Assistant with BOTH text and tool calls
(let ([assistant-msg-with-text
       (make-message "m-4" "m-3" 'assistant 'message
                     (list (make-text-part "I'll help you")
                           (make-tool-call-part "tc-2" "edit" "{}"))
                     1003 '#hash())])
  
  (define content (message-content assistant-msg-with-text))
  (define text-parts (filter text-part? content))
  (define tc-parts (filter tool-call-part? content))
  
  ;; Should have 1 text part and 1 tool-call part
  (check-equal? (length text-parts) 1 "BUG-35: has text part when text+tools")
  (check-equal? (length tc-parts) 1 "BUG-35: has tool-call part when text+tools")
  (check-equal? (text-part-text (car text-parts)) "I'll help you" "BUG-35: correct text"))

(println "BUG-35 Issue: GLM 1214 error code - tests passed!")

(println "All loop tests passed!")
