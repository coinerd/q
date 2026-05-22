#lang racket/base

;; tests/test-arch-01-regression.rkt — ARCH-01 regression test (v0.54.7 W0)
;;
;; Regression test for execute-tool-batch-phase arity mismatch.
;; Verifies that perm-cfg is correctly threaded through
;; handle-tool-calls-pending → execute-tool-batch-phase.

(require rackunit
         rackunit/text-ui
         (only-in "../runtime/tool-coordinator.rkt" handle-tool-calls-pending)
         (only-in "../tools/tool.rkt" make-tool make-tool-registry register-tool! make-success-result)
         (only-in "../util/protocol-types.rkt" make-message make-tool-call-part)
         (only-in "../util/ids.rkt" generate-id)
         (only-in "../util/event.rkt" event-event event-payload)
         (only-in "../tools/permission-gate.rkt" make-default-permission-config
                  permission-config?)
         "../agent/event-bus.rkt")

(define (make-echo-tool)
  (make-tool
   "echo"
   "Echo tool for testing"
   (hasheq 'type "object" 'properties (hasheq) 'required '())
   (lambda (args _ctx)
     (make-success-result "ok"))))

(define (make-assistant-msg-with-call tool-name)
  (make-message (generate-id)
                #f
                'assistant
                'text
                (list (make-tool-call-part (generate-id) tool-name (hasheq)))
                1000
                (hasheq)))

;; Regression: handle-tool-calls-pending must accept #:permission-config
;; and pass it through to execute-tool-batch-phase without arity error
(define arch-01-suite
  (test-suite "ARCH-01: perm-cfg arity regression"

    (test-case "handle-tool-calls-pending threads perm-cfg to execute-tool-batch-phase"
      ;; This would have crashed before the ARCH-01 fix with
      ;; "execute-tool-batch-phase: arity mismatch: actual: 10, expected: 11"
      (define bus (make-event-bus))
      (define reg (make-tool-registry))
      (register-tool! reg (make-echo-tool))
      (define perm-cfg (make-default-permission-config))
      (check-pred permission-config? perm-cfg)
      (define completed? (box #f))
      (subscribe! bus
                  (lambda (evt)
                    (when (equal? (event-event evt) "tool.execution.completed")
                      (set-box! completed? #t))))
      (define new-msgs (list (make-assistant-msg-with-call "echo")))
      ;; This call would previously crash
      (define result
        (handle-tool-calls-pending new-msgs '() #f reg bus
                                    "test-session" "/tmp/test.log" #f (hash)
                                    #:permission-config perm-cfg))
      ;; Verify the tool actually executed
      (check-pred list? result)
      ;; Give event bus a moment to deliver
      (check-true (unbox completed?) "tool.execution.completed event should fire"))

    (test-case "handle-tool-calls-pending without perm-cfg still works (default #f)"
      (define bus (make-event-bus))
      (define reg (make-tool-registry))
      (register-tool! reg (make-echo-tool))
      (define completed? (box #f))
      (subscribe! bus
                  (lambda (evt)
                    (when (equal? (event-event evt) "tool.execution.completed")
                      (set-box! completed? #t))))
      (define new-msgs (list (make-assistant-msg-with-call "echo")))
      ;; Default path — no #:permission-config supplied
      (define result
        (handle-tool-calls-pending new-msgs '() #f reg bus
                                    "test-session" "/tmp/test.log" #f (hash)))
      (check-pred list? result)
      (check-true (unbox completed?) "default perm-cfg path should work"))))

(run-tests arch-01-suite)
