#lang racket/base

;; BOUNDARY: integration

;; tests/test-tool-coordinator-duration.rkt — duration-ms accuracy tests
;; v0.29.17 W0: Verify tool-execution-end-event duration-ms is non-zero and
;; correlates with wall-clock time.

(require rackunit
         racket/list
         (only-in "../runtime/tool-coordinator.rkt" handle-tool-calls-pending)
         (only-in "../tools/tool.rkt" make-tool make-tool-registry register-tool! make-success-result)
         (only-in "../util/protocol-types.rkt" make-message make-tool-call-part)
         (only-in "../util/ids.rkt" generate-id)
         (only-in "../util/event.rkt" event-event event-payload)
         "../agent/event-bus.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-sleep-tool ms)
  (make-tool
   "sleep"
   "Sleep for testing"
   (hasheq 'type "object" 'properties (hasheq 'ms (hasheq 'type "integer")) 'required '("ms"))
   (lambda (args _ctx)
     (define delay-ms (hash-ref args 'ms 0))
     (sleep (/ delay-ms 1000.0))
     (make-success-result (format "slept ~a ms" delay-ms)))))

(define (make-assistant-msg-with-sleep ms)
  (make-message (generate-id)
                #f
                'assistant
                'text
                (list (make-tool-call-part (generate-id) "sleep" (hasheq 'ms ms)))
                1000
                (hasheq)))

;; ============================================================
;; duration-ms tests
;; ============================================================

(test-case "duration-ms is non-zero after tool execution"
  (define bus (make-event-bus))
  (define reg (make-tool-registry))
  (register-tool! reg (make-sleep-tool 50))
  (define collected-durations (box '()))
  (subscribe! bus
              (lambda (evt)
                (when (equal? (event-event evt) "tool.execution.completed")
                  (define payload (event-payload evt))
                  (set-box! collected-durations
                            (cons (hash-ref payload 'duration-ms 0) (unbox collected-durations))))))
  (define new-msgs (list (make-assistant-msg-with-sleep 50)))
  (handle-tool-calls-pending new-msgs '() #f reg bus "test-session" "/tmp/test.log" #f (hash))
  (define durations (unbox collected-durations))
  (check-equal? (length durations) 1)
  (check-true (> (car durations) 0)))

(test-case "duration-ms correlates with wall time (±200ms tolerance)"
  (define bus (make-event-bus))
  (define reg (make-tool-registry))
  (register-tool! reg (make-sleep-tool 80))
  (define collected-durations (box '()))
  (subscribe! bus
              (lambda (evt)
                (when (equal? (event-event evt) "tool.execution.completed")
                  (define payload (event-payload evt))
                  (set-box! collected-durations
                            (cons (hash-ref payload 'duration-ms 0) (unbox collected-durations))))))
  (define new-msgs (list (make-assistant-msg-with-sleep 80)))
  (handle-tool-calls-pending new-msgs '() #f reg bus "test-session" "/tmp/test.log" #f (hash))
  (define durations (unbox collected-durations))
  (check-equal? (length durations) 1)
  (define dur (car durations))
  (check-true (>= dur 40))
  (check-true (<= dur 300)))
