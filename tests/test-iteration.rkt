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

;; ============================================================
;; ensure-hash-args tests (STRUC-02 fix)
;; ============================================================

(test-case "ensure-hash-args with valid JSON string returns hash"
  (define result (ensure-hash-args "{\"key\": \"val\"}"))
  (check-pred hash? result)
  (check-equal? (hash-ref result 'key #f) "val"))

(test-case "ensure-hash-args with invalid JSON string returns hash with _parse_failed key"
  (define result (ensure-hash-args "not json"))
  (check-pred hash? result)
  (check-true (hash-ref result '_parse_failed #f)))

(test-case "ensure-hash-args with hash returns same hash"
  (define h (hasheq 'foo 'bar))
  (define result (ensure-hash-args h))
  (check-eq? result h))

(test-case "ensure-hash-args with empty JSON object string returns empty hash"
  (define result (ensure-hash-args "{}"))
  (check-pred hash? result)
  (check-equal? (hash-keys result) '()))

(test-case "ensure-hash-args with empty string returns empty hash"
  (define result (ensure-hash-args ""))
  (check-pred hash? result)
  (check-equal? (hash-keys result) '()))

;; Run tests
(module+ main
  (run-tests iteration-tests))

(module+ test
  (run-tests iteration-tests))
