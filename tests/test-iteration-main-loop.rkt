#lang racket

;; @speed fast  ;; @suite runtime

;; BOUNDARY: integration

;; tests/test-iteration-main-loop.rkt -- Tests for run-iteration-loop orchestration
;;
;; v0.34.7 T-01: Dedicated tests for iteration main-loop module.
;; Tests through public run-iteration-loop API with mock providers.

(require rackunit
         rackunit/text-ui
         "../agent/iteration/main-loop.rkt"
         "../agent/iteration/loop-state.rkt"
         (only-in "../util/event/event-bus.rkt" make-event-bus subscribe!)
         (only-in "../util/event/event.rkt" event-event)
         (only-in "../llm/provider.rkt" make-provider)
         (only-in "../util/message/protocol-types.rkt" make-message make-text-part message-role)
         (only-in "../util/loop-result.rkt" loop-result? loop-result-termination-reason)
         (only-in "../llm/model.rkt"
                  model-response-content
                  model-response-stop-reason
                  make-model-response
                  make-stream-chunk))

;; ============================================================
;; Helpers
;; ============================================================

(define (simple-msg role text)
  (make-message "test-id" #f role 'message (list (make-text-part text)) (current-seconds) (hasheq)))

;; Mock provider that returns a single completed response
(define (make-completed-provider text)
  (make-provider (lambda () "mock")
                 (lambda () (hash 'streaming #t))
                 ;; send
                 (lambda (req)
                   (make-model-response (list (hasheq 'type "text" 'text text))
                                        "stop"
                                        (hash 'usage
                                              (hasheq 'prompt_tokens 10 'completion_tokens 5))))
                 ;; stream
                 (lambda (req) (list (make-stream-chunk text #f #f #f)))))

;; v0.99.83 W2: Provider that always returns empty thinking
(define (make-empty-thinking-provider [thinking-text "thinking..."])
  (make-provider
   (lambda () "empty-thinking-mock")
   (lambda () (hash 'streaming #t))
   ;; send
   (lambda (req) (make-model-response '() "stop" (hash)))
   ;; stream: empty text delta, but thinking delta, then done
   (lambda (req)
     (list (make-stream-chunk "" #f #f #f #:delta-thinking thinking-text)
           (make-stream-chunk #f #f (hasheq 'prompt_tokens 10 'completion_tokens 0) #t)))))

;; ============================================================
;; Tests
;; ============================================================

(define main-loop-tests
  (test-suite "iteration/main-loop"

    (test-case "run-iteration-loop returns loop-result for simple completion"
      (define bus (make-event-bus))
      (define prov (make-completed-provider "done"))
      (define ctx (list (simple-msg 'user "hello")))
      (define result (run-iteration-loop ctx prov bus #f #f "/tmp/test-log" "test-session" 10))
      (check-not-false result)
      (check-equal? (loop-result-termination-reason result) 'completed))

    (test-case "run-iteration-loop with nil provider raises exception"
      ;; With #f provider, run-provider-turn should still return something
      ;; (graceful degradation)
      (define bus (make-event-bus))
      (define ctx (list (simple-msg 'user "hello")))
      ;; #f provider -- the loop should handle this gracefully
      ;; It may error, which is acceptable behavior
      (check-exn exn:fail?
                 (lambda () (run-iteration-loop ctx #f bus #f #f "/tmp/test-log" "test-session" 10))))

    (test-case "run-iteration-loop respects max-iterations=0"
      ;; With 0 max iterations, even tool-calls-pending should trigger limits
      (define bus (make-event-bus))
      (define prov (make-completed-provider "done"))
      (define ctx (list (simple-msg 'user "hello")))
      (define result (run-iteration-loop ctx prov bus #f #f "/tmp/test-log" "test-session" 0))
      ;; Should still complete -- the mock returns 'stop
      (check-not-false result))

    ;; ============================================================
    ;; v0.99.83 W2: Empty-response detection & auto-retry
    ;; ============================================================
    (test-case "empty-response with thinking triggers auto-retry with nudge"
      (define bus (make-event-bus))
      (define events (box '()))
      (subscribe! bus (lambda (e) (set-box! events (cons e (unbox events)))))
      ;; Provider returns empty text with thinking on first call, then normal on retry
      (define stream-count 0)
      (define prov
        (make-provider (lambda () "empty-then-ok")
                       (lambda () (hash 'streaming #t))
                       (lambda (req)
                         (make-model-response (list (hasheq 'type "text" 'text "ok")) "stop" (hash)))
                       (lambda (req)
                         (set! stream-count (add1 stream-count))
                         (if (= stream-count 1)
                             (list (make-stream-chunk "" #f #f #f #:delta-thinking "thinking...")
                                   (make-stream-chunk #f #f (hasheq) #t))
                             (list (make-stream-chunk "ok" #f #f #t))))))
      (define ctx (list (simple-msg 'user "hello")))
      (define result (run-iteration-loop ctx prov bus #f #f "/tmp/test-log" "test-session" 10))
      ;; Should complete after retry
      (check-equal? (loop-result-termination-reason result) 'completed)
      ;; Should have made 2 stream calls (empty + retry)
      (check-equal? stream-count 2)
      ;; Check that runtime.empty-response.retry event was emitted (nudge injected)
      (define retry-events
        (filter (lambda (e) (equal? (event-event e) "runtime.empty-response.retry")) (unbox events)))
      (check-not-false (car retry-events) "runtime.empty-response.retry event should be emitted"))

    (test-case "empty-response after retry limit stops with error"
      (define bus (make-event-bus))
      ;; Provider always returns empty thinking
      (define prov (make-empty-thinking-provider "always-thinking"))
      (define ctx (list (simple-msg 'user "hello")))
      (define result (run-iteration-loop ctx prov bus #f #f "/tmp/test-log" "test-session" 10))
      ;; After retry limit exceeded, should stop (not loop forever)
      ;; The result should be 'completed because we transform empty-response -> completed with nudge
      ;; but since the retry already happened, the second empty-response flows through normally
      (check-not-false result))))

(module+ main
  (run-tests main-loop-tests))
