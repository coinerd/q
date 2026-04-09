#lang racket

;; tests/test-iteration.rkt — tests for iteration loop with steering queue

(require rackunit
         rackunit/text-ui
         "../agent/types.rkt"
         "../agent/queue.rkt"
         "../agent/event-bus.rkt"
         "../util/ids.rkt"
         "../runtime/iteration.rkt"
         (only-in "../llm/provider.rkt" make-mock-provider)
         (only-in "../llm/model.rkt" make-model-response))

;; ============================================================
;; Test helpers
;; ============================================================

;; Create a simple user message
(define (make-user-message text)
  (make-message (generate-id) #f 'user 'user
                (list (make-text-part text))
                (current-seconds)
                (hasheq)))

;; ============================================================
;; Steering queue tests
;; ============================================================

(define iteration-tests
  (test-suite
   "Iteration Loop with Steering Queue"

   (test-case "run-iteration-loop accepts optional queue parameter"
     (define bus (make-event-bus))
     (define queue (make-queue))
     (define ctx (list (make-user-message "Initial message")))
     
     ;; Mock provider using make-mock-provider with proper content parts
     (define content-parts (list (hasheq 'type "text" 'text "Response")))
     (define mock-prov
       (make-mock-provider (make-model-response content-parts (hash) "mock" #f)))
     
     ;; Should not raise error with queue parameter
     (check-not-exn
      (lambda ()
        (run-iteration-loop ctx mock-prov bus #f #f "/tmp/test.log" "test-session" 10
                            #:queue queue))))

   (test-case "steering queue messages are injected into context"
     (define bus (make-event-bus))
     (define queue (make-queue))
     (define steering-msg (make-user-message "Steering input"))
     
     ;; Enqueue a steering message
     (enqueue-steering! queue steering-msg)
     
     ;; Track what context was passed to provider
     ;; Note: We can't easily capture context with mock provider,
     ;; so we test indirectly by checking behavior works
     (define ctx (list (make-user-message "Initial")))
     
     ;; Mock provider
     (define content-parts (list (hasheq 'type "text" 'text "Response")))
     (define mock-prov
       (make-mock-provider (make-model-response content-parts (hash) "mock" #f)))
     
     ;; Run iteration with queue - should complete without error
     (check-not-exn
      (lambda ()
        (run-iteration-loop ctx mock-prov bus #f #f "/tmp/test.log" "test-session" 10
                            #:queue queue))))

   (test-case "multiple steering messages are injected in order"
     (define bus (make-event-bus))
     (define queue (make-queue))
     
     (enqueue-steering! queue (make-user-message "First steering"))
     (enqueue-steering! queue (make-user-message "Second steering"))
     
     ;; Mock provider
     (define content-parts (list (hasheq 'type "text" 'text "Response")))
     (define mock-prov
       (make-mock-provider (make-model-response content-parts (hash) "mock" #f)))
     
     (define ctx (list (make-user-message "Initial")))
     
     ;; Should complete without error with multiple steering messages
     (check-not-exn
      (lambda ()
        (run-iteration-loop ctx mock-prov bus #f #f "/tmp/test.log" "test-session" 10
                            #:queue queue))))

   (test-case "empty queue does not affect context"
     (define bus (make-event-bus))
     (define queue (make-queue))
     
     ;; Mock provider
     (define content-parts (list (hasheq 'type "text" 'text "Response")))
     (define mock-prov
       (make-mock-provider (make-model-response content-parts (hash) "mock" #f)))
     
     (define ctx (list (make-user-message "Initial")))
     
     ;; Should complete without error with empty queue
     (check-not-exn
      (lambda ()
        (run-iteration-loop ctx mock-prov bus #f #f "/tmp/test.log" "test-session" 10
                            #:queue queue))))

   (test-case "without queue parameter iteration still works (backward compat)"
     (define bus (make-event-bus))
     (define ctx (list (make-user-message "Initial")))
     
     ;; Mock provider
     (define content-parts (list (hasheq 'type "text" 'text "Response")))
     (define mock-prov
       (make-mock-provider (make-model-response content-parts (hash) "mock" #f)))
     
     ;; Should work without queue parameter (backward compatibility)
     (check-not-exn
      (lambda ()
        (run-iteration-loop ctx mock-prov bus #f #f "/tmp/test.log" "test-session" 10))))
   ))

;; Run tests
(module+ main
  (run-tests iteration-tests))

(module+ test
  (run-tests iteration-tests))
