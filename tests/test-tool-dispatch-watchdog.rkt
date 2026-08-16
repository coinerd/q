#lang racket/base

;; @speed fast  ;; @suite runtime

;; BOUNDARY: contract

;; tests/test-tool-dispatch-watchdog.rkt — D1 remediation (issue #9351)
;;
;; Incident 01M05MCKP: tool dispatch lost between iteration.decision and
;; tool.execution.started with no deadline — the turn stayed open for 92
;; minutes while the TUI remained responsive. Trace evidence: 35×
;; tool.call.started vs 34× tool.execution.started.
;;
;; The watchdog runs coordinator phases 1+2 on a worker thread with a
;; bounded pre-start deadline. If execution never starts (publish! is
;; synchronous fan-out, so a blocked subscriber stalls the first
;; tool.execution.updated emit exactly like the incident), the coordinator
;; emits tool.dispatch.timeout and synthesizes error tool-results for every
;; pending call so the turn recovers instead of hanging forever.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/list
         (only-in "../util/event/event-bus.rkt" make-event-bus subscribe!)
         (only-in "../util/event/event.rkt" event-event event-payload)
         (only-in "../tools/tool.rkt" make-tool make-tool-registry register-tool! make-success-result)
         (only-in "../runtime/session/session-config.rkt" hash->session-config)
         (only-in "../util/message/protocol-types.rkt"
                  make-message
                  make-tool-call-part
                  message-content
                  tool-result-part?
                  tool-result-part-is-error?
                  tool-result-part-content)
         (only-in "../util/ids.rkt" generate-id)
         (only-in "../tools/permission-gate.rkt"
                  make-default-permission-config
                  make-permissive-permission-config)
         (only-in "../runtime/tool-coordinator.rkt"
                  handle-tool-calls-pending/outcome
                  tool-batch-outcome?
                  tool-batch-outcome-current-result-messages
                  tool-batch-outcome-effective-current-calls
                  current-tool-dispatch-timeout-ms))

;; ── helpers (mirrors tests/test-arch-01-regression.rkt) ──

(define (make-echo-tool)
  (make-tool "echo"
             "Echo tool for testing"
             (hasheq 'type "object" 'properties (hasheq) 'required '())
             (lambda (args _ctx) (make-success-result "ok"))))

(define (make-assistant-msg-with-call [tool-name "echo"])
  (make-message (generate-id)
                #f
                'assistant
                'text
                (list (make-tool-call-part (generate-id) tool-name (hasheq)))
                1000
                (hasheq)))

(define (first-error-text msg)
  (define part
    (findf (lambda (p) (and (tool-result-part? p) (tool-result-part-is-error? p)))
           (message-content msg)))
  (and part (tool-result-part-content part)))

;; ── D1: pre-start dispatch deadline ──

(define watchdog-suite
  (test-suite "tool dispatch watchdog (D1, issue #9351)"

    (test-case "coordinator recovers when execution never starts"
      ;; Simulate the incident: a bus subscriber blocks the first
      ;; tool.execution.updated publish (synchronous fan-out), so the batch
      ;; never starts executing. With a 100ms dispatch deadline the
      ;; coordinator must return synthetic error results and emit
      ;; tool.dispatch.timeout instead of blocking forever.
      (define bus (make-event-bus))
      (define release-sem (make-semaphore))
      (define captured '())
      (subscribe! bus
                  (lambda (evt)
                    (cond
                      ;; Block the worker far beyond the deadline. The semaphore is
                      ;; posted at the end of the test so no thread lingers.
                      [(equal? (event-event evt) "tool.execution.updated") (sync release-sem)]
                      [else (set! captured (cons (format "~a" (event-event evt)) captured))])))
      (define reg (make-tool-registry))
      (register-tool! reg (make-echo-tool))
      (define log-path (make-temporary-file "q-watchdog-~a.log"))
      (define start-ms (current-inexact-milliseconds))
      (define outcome
        (parameterize ([current-tool-dispatch-timeout-ms 100])
          (handle-tool-calls-pending/outcome (list (make-assistant-msg-with-call))
                                             '()
                                             #f
                                             reg
                                             bus
                                             "watchdog-test"
                                             log-path
                                             #f
                                             (hash->session-config (hash))
                                             #:permission-config
                                             (make-permissive-permission-config))))
      (define elapsed-ms (- (current-inexact-milliseconds) start-ms))
      (semaphore-post release-sem)
      ;; Must return an outcome quickly, not hang.
      (check-true (< elapsed-ms 5000) (format "watchdog recovery too slow: ~ams" elapsed-ms))
      (check-true (tool-batch-outcome? outcome))
      ;; The pending call is answered with a synthetic error tool-result so
      ;; the model can react and the turn continues (no orphaned tool_calls).
      (define result-msgs (tool-batch-outcome-current-result-messages outcome))
      (check-equal? (length result-msgs) 1)
      (define err-text (first-error-text (car result-msgs)))
      (check-true (and err-text (regexp-match? #rx"dispatch" (format "~a" err-text)))
                  (format "expected dispatch-timeout error text, got: ~a" err-text))
      ;; Diagnostic event must be observable on the bus.
      (check-not-false (member "tool.dispatch.timeout" captured)
                       (format "tool.dispatch.timeout not emitted; captured: ~a" captured))
      ;; Synthetic results must be persisted to the log as well.
      (check-true (regexp-match? #rx"dispatch" (file->string log-path)))
      (delete-file log-path))

    (test-case "normal execution is unaffected by the watchdog"
      ;; With a generous deadline and no blocked subscriber the batch runs
      ;; normally: real (non-error) results, no timeout event.
      (define bus (make-event-bus))
      (define captured '())
      (subscribe! bus (lambda (evt) (set! captured (cons (event-event evt) captured))))
      (define reg (make-tool-registry))
      (register-tool! reg (make-echo-tool))
      (define log-path (make-temporary-file "q-watchdog-ok-~a.log"))
      (define outcome
        (parameterize ([current-tool-dispatch-timeout-ms 5000])
          (handle-tool-calls-pending/outcome (list (make-assistant-msg-with-call))
                                             '()
                                             #f
                                             reg
                                             bus
                                             "watchdog-ok"
                                             log-path
                                             #f
                                             (hash->session-config (hash))
                                             #:permission-config
                                             (make-permissive-permission-config))))
      (check-true (tool-batch-outcome? outcome))
      (define result-msgs (tool-batch-outcome-current-result-messages outcome))
      (check-equal? (length result-msgs) 1)
      (check-false (first-error-text (car result-msgs)) "echo result must not be an error")
      (check-false (member "tool.dispatch.timeout" captured))
      (delete-file log-path))

    (test-case "dispatch deadline of 0 disables the watchdog"
      ;; Legacy behavior: no deadline, no synthetic results.
      (define bus (make-event-bus))
      (define reg (make-tool-registry))
      (register-tool! reg (make-echo-tool))
      (define log-path (make-temporary-file "q-watchdog-off-~a.log"))
      (define outcome
        (parameterize ([current-tool-dispatch-timeout-ms 0])
          (handle-tool-calls-pending/outcome (list (make-assistant-msg-with-call))
                                             '()
                                             #f
                                             reg
                                             bus
                                             "watchdog-off"
                                             log-path
                                             #f
                                             (hash->session-config (hash))
                                             #:permission-config
                                             (make-permissive-permission-config))))
      (check-true (tool-batch-outcome? outcome))
      (check-false (first-error-text (car (tool-batch-outcome-current-result-messages outcome))))
      (delete-file log-path))))

(module+ test
  (void (run-tests watchdog-suite)))
