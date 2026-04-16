#lang racket

;; tests/test-sdk-steer-followup.rkt — FEAT-76: steer!/follow-up! in SDK

(require rackunit
         rackunit/text-ui
         "../interfaces/sdk.rkt"
         "../agent/queue.rkt"
         (only-in "../runtime/agent-session.rkt" agent-session-queue)
         "../tools/tool.rkt"
         "../agent/event-bus.rkt")

;; Mock provider that returns a simple response
(define (make-mock-provider)
  (lambda (messages tools config)
    (values (list (hasheq 'role "assistant" 'content (hasheq 'type "text" 'text "mock response")))
            (hasheq 'prompt-tokens 10 'completion-tokens 5))))

(define steer-tests
  (test-suite "SDK steer! and follow-up!"

    (test-case "steer! enqueues steering message"
      (define rt (make-runtime #:provider (make-mock-provider)))
      (define rt2 (steer! rt "change direction"))
      ;; No session open — should return rt unchanged
      (check-equal? rt rt2))

    (test-case "steer! with open session enqueues to queue"
      (define rt (make-runtime #:provider (make-mock-provider)))
      (define rt-open (open-session rt))
      (define rt-steered (steer! rt-open "redirect now"))
      (define sess (runtime-rt-session rt-steered))
      (define q (agent-session-queue sess))
      ;; Should be able to dequeue the steering message
      (define msg (dequeue-steering! q))
      (check-not-false msg)
      (check-equal? msg "redirect now"))

    (test-case "follow-up! enqueues follow-up message"
      (define rt (make-runtime #:provider (make-mock-provider)))
      (define rt2 (follow-up! rt "then do this"))
      ;; No session open — should return rt unchanged
      (check-equal? rt rt2))

    (test-case "follow-up! with open session enqueues to queue"
      (define rt (make-runtime #:provider (make-mock-provider)))
      (define rt-open (open-session rt))
      (define rt-followed (follow-up! rt-open "next step"))
      (define sess (runtime-rt-session rt-followed))
      (define q (agent-session-queue sess))
      (define msg (dequeue-followup! q))
      (check-not-false msg)
      (check-equal? msg "next step"))

    (test-case "multiple steer! calls enqueue multiple messages"
      (define rt (make-runtime #:provider (make-mock-provider)))
      (define rt-open (open-session rt))
      (define rt1 (steer! rt-open "first"))
      (define rt2 (steer! rt1 "second"))
      (define sess (runtime-rt-session rt2))
      (define q (agent-session-queue sess))
      (check-equal? (dequeue-steering! q) "first")
      (check-equal? (dequeue-steering! q) "second")
      (check-false (dequeue-steering! q)))

    (test-case "multiple follow-up! calls enqueue multiple messages"
      (define rt (make-runtime #:provider (make-mock-provider)))
      (define rt-open (open-session rt))
      (define rt1 (follow-up! rt-open "step 1"))
      (define rt2 (follow-up! rt1 "step 2"))
      (define sess (runtime-rt-session rt2))
      (define q (agent-session-queue sess))
      (define all (dequeue-all-followups! q))
      (check-equal? (length all) 2)
      (check-equal? (car all) "step 1")
      (check-equal? (cadr all) "step 2"))

    ;; ============================================================
    ;; Edge cases
    ;; ============================================================
    (test-case "steer! with empty string message"
      (define rt (make-runtime #:provider (make-mock-provider)))
      (define rt-open (open-session rt))
      (define rt-steered (steer! rt-open ""))
      (define sess (runtime-rt-session rt-steered))
      (define q (agent-session-queue sess))
      (define msg (dequeue-steering! q))
      (check-not-false msg)
      (check-equal? msg ""))

    (test-case "follow-up! with empty string"
      (define rt (make-runtime #:provider (make-mock-provider)))
      (define rt-open (open-session rt))
      (define rt-followed (follow-up! rt-open ""))
      (define sess (runtime-rt-session rt-followed))
      (define q (agent-session-queue sess))
      (define msg (dequeue-followup! q))
      (check-not-false msg)
      (check-equal? msg ""))))

(run-tests steer-tests)
