#lang racket

;; tests/test-event-ordering.rkt — Wave 4: Regression guard
;;
;; CI-level invariant: for any agent turn with streaming text,
;; assistant.message.completed MUST appear before any tool.call.started
;; in the event sequence. This test would FAIL without the B1 fix.

(require rackunit
         rackunit/text-ui
         "../agent/loop.rkt"
         "../agent/event-bus.rkt"
         "../llm/provider.rkt"
         "../llm/model.rkt"
         "../util/protocol-types.rkt"
         "../util/protocol-types.rkt")

(define event-ordering-tests
  (test-suite
   "Event Ordering Invariant Regression Guard"

   ;; Invariant test: assistant.message.completed before tool.call.started
   ;; This would fail if the B1 fix (Wave 1) were reverted.
   (test-case
    "regression: assistant.message.completed before tool.call.started (invariant)"
    ;; Test with multiple turn scenarios
    (define scenarios
      (list
       ;; Scenario 1: Text then tool call
       (cons "text+tool"
             (lambda ()
               (list (make-stream-chunk "Answer" #f #f #f)
                     (make-stream-chunk #f
                                   (hasheq 'id "tc-1" 'index 0
                                           'function (hasheq 'name "read"
                                                             'arguments "{\"path\":\"x\"}"))
                                   #f #f)
                     (make-stream-chunk #f #f #f #t))))
       ;; Scenario 2: Long text then multiple tool calls
       (cons "long-text+2-tools"
             (lambda ()
               (list (make-stream-chunk "Let me " #f #f #f)
                     (make-stream-chunk "check " #f #f #f)
                     (make-stream-chunk "that." #f #f #f)
                     (make-stream-chunk #f
                                   (hasheq 'id "tc-1" 'index 0
                                           'function (hasheq 'name "read"
                                                             'arguments "{\"path\":\"a\"}"))
                                   #f #f)
                     (make-stream-chunk #f
                                   (hasheq 'id "tc-2" 'index 1
                                           'function (hasheq 'name "bash"
                                                             'arguments "{\"command\":\"ls\"}"))
                                   #f #f)
                     (make-stream-chunk #f #f #f #t))))
       ;; Scenario 3: No text, only tool call
       (cons "tool-only"
             (lambda ()
               (list (make-stream-chunk #f
                                   (hasheq 'id "tc-1" 'index 0
                                           'function (hasheq 'name "bash"
                                                             'arguments "{\"command\":\"ls\"}"))
                                   #f #f)
                     (make-stream-chunk #f #f #f #t))))))

    (define tools
      (list (hasheq 'type "function"
                    'function (hasheq 'name "read" 'parameters (hasheq)))
            (hasheq 'type "function"
                    'function (hasheq 'name "bash" 'parameters (hasheq)))))

    (for ([scenario scenarios])
      (define name (car scenario))
      (define make-chunks (cdr scenario))
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
         (lambda (req) (make-chunks))))

      (define ctx (list (make-message "m-1" #f 'user 'message
                                      (list (make-text-part "go"))
                                      1000 '#hash())))

      (run-agent-turn ctx prov bus
                      #:session-id "s1" #:turn-id "t1"
                      #:tools tools)

      (define events (unbox captured))
      (define amc-idx (index-of events "assistant.message.completed"))
      (define tcs-idx (index-of events "tool.call.started"))

      (check-not-false amc-idx
                       (format "~a: assistant.message.completed found" name))
      (check-not-false tcs-idx
                       (format "~a: tool.call.started found" name))
      (check-true (< amc-idx tcs-idx)
                  (format "~a: assistant.message.completed (~a) before tool.call.started (~a)"
                          name amc-idx tcs-idx))))

   ;; Additional invariant: turn.started is always first event
   (test-case
    "regression: turn.started is always the first event"
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
         (list (make-stream-chunk "Hi" #f #f #f)
               (make-stream-chunk #f #f #f #t)))))

    (define ctx (list (make-message "m-1" #f 'user 'message
                                    (list (make-text-part "hello"))
                                    1000 '#hash())))

    (run-agent-turn ctx prov bus #:session-id "s1" #:turn-id "t1")

    (define events (unbox captured))
    (check-equal? (car events) "turn.started"
                  "turn.started must be the first event in the sequence"))

   ;; Invariant: turn.completed is always last event
   (test-case
    "regression: turn.completed is always the last event"
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
         (list (make-stream-chunk "Go" #f #f #f)
               (make-stream-chunk #f
                             (hasheq 'id "tc-1" 'index 0
                                     'function (hasheq 'name "read"
                                                       'arguments "{\"path\":\"x\"}"))
                             #f #f)
               (make-stream-chunk #f #f #f #t)))))

    (define ctx (list (make-message "m-1" #f 'user 'message
                                    (list (make-text-part "go"))
                                    1000 '#hash())))

    (run-agent-turn ctx prov bus
                    #:session-id "s1" #:turn-id "t1"
                    #:tools (list (hasheq 'type "function"
                                          'function (hasheq 'name "read"
                                                            'parameters (hasheq)))))

    (define events (unbox captured))
    (check-equal? (last events) "turn.completed"
                  "turn.completed must be the last event in the sequence"))))

(module+ main
  (run-tests event-ordering-tests))
