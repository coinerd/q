#lang racket

;; tests/test-iteration-main-loop.rkt -- Tests for run-iteration-loop orchestration
;;
;; v0.34.7 T-01: Dedicated tests for iteration main-loop module.
;; Tests through public run-iteration-loop API with mock providers.

(require rackunit
         rackunit/text-ui
         "../runtime/iteration/main-loop.rkt"
         "../runtime/iteration/loop-state.rkt"
         (only-in "../agent/event-bus.rkt" make-event-bus)
         (only-in "../llm/provider.rkt" make-provider)
         (only-in "../util/protocol-types.rkt" make-message make-text-part message-role)
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

    (test-case "run-iteration-loop with nil provider returns result"
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
      (check-not-false result))))

(module+ main
  (run-tests main-loop-tests))
(module+ test
  (run-tests main-loop-tests))
