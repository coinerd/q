#lang racket

;; tests/test-loop.rkt — tests for agent/state.rkt, agent/queue.rkt, agent/loop.rkt

(require rackunit
         racket/generator
         "../util/protocol-types.rkt"
         "../agent/event-bus.rkt"
         "../agent/state.rkt"
         "../agent/queue.rkt"
         "../agent/loop.rkt"
         "../llm/model.rkt"
         "../llm/provider.rkt"
         "../extensions/hooks.rkt"
         "../util/cancellation.rkt")

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
 (check-pred queue-empty? q)
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

;; ============================================================
;; 5. run-agent-turn direct tests
;; ============================================================

(test-case
 "run-agent-turn: 3 text deltas → accumulated text"
 (define bus (make-event-bus))
 (define prov
   (make-provider
    (lambda () "mock")
    (lambda () (hasheq 'streaming #t))
    (lambda (req) (error 'send "not used"))
    (lambda (req)
      (list (stream-chunk "Hel" #f #f #f)
            (stream-chunk "lo " #f #f #f)
            (stream-chunk "world" #f #f #f)
            (stream-chunk #f #f #f #t)))))
 (define ctx (list (make-message "m-1" #f 'user 'message
                                 (list (make-text-part "hi"))
                                 1000 '#hash())))
 (define result (run-agent-turn ctx prov bus
                                #:session-id "s1" #:turn-id "t1"))
 (check-equal? (loop-result-termination-reason result) 'completed)
 (define msgs (loop-result-messages result))
 (check = (length msgs) 1)
 (define assistant (car msgs))
 (check-equal? (message-role assistant) 'assistant)
 (define text-parts (filter text-part? (message-content assistant)))
 (check = (length text-parts) 1)
 (check-equal? (text-part-text (car text-parts)) "Hello world"))

(test-case
 "run-agent-turn: tool-call delta → tool-calls-pending"
 (define bus (make-event-bus))
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
                                 (list (make-text-part "run bash"))
                                 1000 '#hash())))
 (define result (run-agent-turn ctx prov bus
                                #:session-id "s1" #:turn-id "t1"
                                #:tools (list (hasheq 'type "function"
                                                      'function (hasheq 'name "bash"
                                                                        'parameters (hasheq))))))
 (check-equal? (loop-result-termination-reason result) 'tool-calls-pending)
 (define msgs (loop-result-messages result))
 (check = (length msgs) 1)
 (define assistant (car msgs))
 (define tc-parts (filter tool-call-part? (message-content assistant)))
 (check = (length tc-parts) 1)
 (check-equal? (tool-call-part-name (car tc-parts)) "bash")
 (check-equal? (tool-call-part-id (car tc-parts)) "tc-1"))

(test-case
 "run-agent-turn: mixed text + tool call → both captured"
 (define bus (make-event-bus))
 (define prov
   (make-provider
    (lambda () "mock")
    (lambda () (hasheq 'streaming #t))
    (lambda (req) (error 'send "not used"))
    (lambda (req)
      (list (stream-chunk "Thinking..." #f #f #f)
            (stream-chunk
             #f
             (hasheq 'id "tc-2" 'index 0
                     'function (hasheq 'name "read"
                                       'arguments "{\"path\":\"foo.rkt\"}"))
             #f #f)
            (stream-chunk #f #f #f #t)))))
 (define ctx (list (make-message "m-1" #f 'user 'message
                                 (list (make-text-part "do stuff"))
                                 1000 '#hash())))
 (define result (run-agent-turn ctx prov bus
                                #:session-id "s1" #:turn-id "t1"
                                #:tools (list (hasheq 'type "function"
                                                      'function (hasheq 'name "read"
                                                                        'parameters (hasheq))))))
 (check-equal? (loop-result-termination-reason result) 'tool-calls-pending)
 (define msgs (loop-result-messages result))
 (check = (length msgs) 1)
 (define assistant (car msgs))
 (define text-parts (filter text-part? (message-content assistant)))
 (define tc-parts (filter tool-call-part? (message-content assistant)))
 (check = (length text-parts) 1)
 (check-equal? (text-part-text (car text-parts)) "Thinking...")
 (check = (length tc-parts) 1)
 (check-equal? (tool-call-part-name (car tc-parts)) "read"))

(test-case
 "run-agent-turn: pre-cancelled token → 'cancelled termination"
 (define bus (make-event-bus))
 (define tok (make-cancellation-token))
 (cancel-token! tok)
 (define prov
   (make-provider
    (lambda () "mock")
    (lambda () (hasheq 'streaming #t))
    (lambda (req) (error 'send "not used"))
    (lambda (req)
      (list (stream-chunk "text" #f #f #f)
            (stream-chunk #f #f #f #t)))))
 (define ctx (list (make-message "m-1" #f 'user 'message
                                 (list (make-text-part "hi"))
                                 1000 '#hash())))
 (define result (run-agent-turn ctx prov bus
                                #:session-id "s1" #:turn-id "t1"
                                #:cancellation-token tok))
 (check-equal? (loop-result-termination-reason result) 'cancelled))

(test-case
 "run-agent-turn: empty stream → completed with empty text"
 (define bus (make-event-bus))
 (define prov
   (make-provider
    (lambda () "mock")
    (lambda () (hasheq 'streaming #t))
    (lambda (req) (error 'send "not used"))
    (lambda (req)
      (list (stream-chunk #f #f #f #t)))))
 (define ctx (list (make-message "m-1" #f 'user 'message
                                 (list (make-text-part "hi"))
                                 1000 '#hash())))
 (define result (run-agent-turn ctx prov bus
                                #:session-id "s1" #:turn-id "t1"))
 (check-equal? (loop-result-termination-reason result) 'completed)
 (define msgs (loop-result-messages result))
 (check = (length msgs) 1)
 (define assistant (car msgs))
 (define text-parts (filter text-part? (message-content assistant)))
 (check = (length text-parts) 1)
 (check-equal? (text-part-text (car text-parts)) ""))

(test-case
 "run-agent-turn: hook dispatcher receives message-start/update/end"
 (define bus (make-event-bus))
 (define received-hooks (box '()))
 (define (recording-hook-dispatcher hook-point payload)
   (set-box! received-hooks (append (unbox received-hooks) (list hook-point)))
   #f)
 (define prov
   (make-provider
    (lambda () "mock")
    (lambda () (hasheq 'streaming #t))
    (lambda (req) (error 'send "not used"))
    (lambda (req)
      (list (stream-chunk "hi" #f #f #f)
            (stream-chunk #f #f #f #t)))))
 (define ctx (list (make-message "m-1" #f 'user 'message
                                 (list (make-text-part "hello"))
                                 1000 '#hash())))
 (define result (run-agent-turn ctx prov bus
                                #:session-id "s1" #:turn-id "t1"
                                #:hook-dispatcher recording-hook-dispatcher))
 (check-equal? (loop-result-termination-reason result) 'completed)
 (define hooks (unbox received-hooks))
 (check-not-false (member 'model-request-pre hooks))
 (check-not-false (member 'message-start hooks))
 (check-not-false (member 'message-update hooks))
 (check-not-false (member 'model-response-post hooks))
 (check-not-false (member 'message-end hooks)))

;; ============================================================
;; 6. BUG #633: turn.started/turn.completed pairing on hook-blocked paths
;; ============================================================

(test-case
 "BUG #633: model-request-pre hook block emits turn.completed"
 (define bus (make-event-bus))
 (define events '())
 (subscribe! bus (lambda (evt) (set! events (cons evt events))))
 (define (blocking-pre-hook hook-point payload)
   (if (eq? hook-point 'model-request-pre)
       (hook-block "test-block")
       #f))
 (define prov
   (make-provider
    (lambda () "mock")
    (lambda () (hasheq 'streaming #t))
    (lambda (req) (error 'send "not used"))
    (lambda (req) (list (stream-chunk #f #f #f #t)))))
 (define ctx (list (make-message "m-1" #f 'user 'message
                                 (list (make-text-part "hi"))
                                 1000 '#hash())))
 (define result (run-agent-turn ctx prov bus
                                #:session-id "s1" #:turn-id "t1"
                                #:hook-dispatcher blocking-pre-hook))
 ;; Must be hook-blocked
 (check-equal? (loop-result-termination-reason result) 'hook-blocked)
 ;; Both turn.started AND turn.completed must be in events
 (define event-names (map event-event (reverse events)))
 (check-not-false (member "turn.started" event-names)
                  "turn.started must be emitted")
 (check-not-false (member "turn.completed" event-names)
                  "turn.completed must be emitted after hook-blocked early return"))

(test-case
 "BUG #633: message-start hook block emits turn.completed"
 (define bus (make-event-bus))
 (define events '())
 (subscribe! bus (lambda (evt) (set! events (cons evt events))))
 (define (blocking-msg-start-hook hook-point payload)
   (if (eq? hook-point 'message-start)
       (hook-block "test-block")
       #f))
 (define prov
   (make-provider
    (lambda () "mock")
    (lambda () (hasheq 'streaming #t))
    (lambda (req) (error 'send "not used"))
    (lambda (req) (list (stream-chunk #f #f #f #t)))))
 (define ctx (list (make-message "m-1" #f 'user 'message
                                 (list (make-text-part "hi"))
                                 1000 '#hash())))
 (define result (run-agent-turn ctx prov bus
                                #:session-id "s1" #:turn-id "t1"
                                #:hook-dispatcher blocking-msg-start-hook))
 ;; Must be hook-blocked
 (check-equal? (loop-result-termination-reason result) 'hook-blocked)
 ;; Both turn.started AND turn.completed must be in events
 (define event-names (map event-event (reverse events)))
 (check-not-false (member "turn.started" event-names)
                  "turn.started must be emitted")
 (check-not-false (member "turn.completed" event-names)
                  "turn.completed must be emitted after message-start hook block"))

;; ============================================================
;; 7. BUG #634: stream-blocked emits model.stream.completed
;; ============================================================

(test-case
 "BUG #634: stream-blocked by message-update hook emits model.stream.completed"
 (define bus (make-event-bus))
 (define events '())
 (subscribe! bus (lambda (evt) (set! events (cons evt events))))
 (define (blocking-update-hook hook-point payload)
   (if (eq? hook-point 'message-update)
       (hook-block "test-stream-block")
       #f))
 (define prov
   (make-provider
    (lambda () "mock")
    (lambda () (hasheq 'streaming #t))
    (lambda (req) (error 'send "not used"))
    (lambda (req)
      ;; Emit one text delta, then done
      (list (stream-chunk "hello" #f #f #f)
            (stream-chunk #f #f #f #t)))))
 (define ctx (list (make-message "m-1" #f 'user 'message
                                 (list (make-text-part "hi"))
                                 1000 '#hash())))
 (define result (run-agent-turn ctx prov bus
                                #:session-id "s1" #:turn-id "t1"
                                #:hook-dispatcher blocking-update-hook))
 (define event-names (map event-event (reverse events)))
 ;; model.stream.completed must still be emitted even when stream is blocked
 (check-not-false (member "model.stream.completed" event-names)
                  "model.stream.completed must be emitted on stream-blocked path"))
