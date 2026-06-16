#lang racket/base

;; @speed fast  ;; @suite core

;; tests/test-execution-plane-characterization.rkt
;; v0.99.17 W0: Characterization tests for execution plane + gateway IPC.
;;
;; These tests lock in the CURRENT behavior of the execution plane pipeline
;; under `racket` direct execution. They serve as a baseline that W1/W2 fixes
;; must not regress.
;;
;; IMPORTANT CONTEXT (raco test vs racket discrepancy):
;; All execution plane tests pass under `racket tests/<file>.rkt` but many
;; FAIL under `raco test tests/<file>.rkt`. This characterization suite
;; uses `racket`-compatible patterns to establish the baseline.

(require rackunit
         rackunit/text-ui
         racket/port
         racket/string
         json
         "../sandbox/ipc-protocol.rkt"
         "../sandbox/gateway-ipc.rkt")

;; Path to the mock worker script
(define mock-worker-path (build-path (current-directory) "tests" "mock-worker.rkt"))
(define racket-bin (find-executable-path "racket"))

(define (start-mock-worker [mode "echo"])
  (start-worker! racket-bin (list (path->string mock-worker-path) mode)))

(define characterization-suite
  (test-suite "Execution Plane Characterization (v0.99.17 W0)"

    ;; ── Test 1: Worker subprocess starts and responds to ping ──
    (test-case "worker subprocess starts and responds to ping"
      (define gw (start-mock-worker))
      (check-true (gateway-alive? gw) "worker should be alive after start")
      (sleep 0.2) ; let worker initialize
      (define req (ipc-request "ping-1" "ping" (hasheq) 5000 #f 'read-only 1))
      (define resp (send-request! gw req 5000))
      (check-equal? (ipc-response-status resp) 'ok)
      (gateway-shutdown! gw))

    ;; ── Test 2: send-request! round-trip returns 'ok status ──
    (test-case "send-request! round-trip returns ok status"
      (define gw (start-mock-worker))
      (sleep 0.1)
      (define req (ipc-request "rt-1" "bash" (hasheq 'command "echo test") 5000 #f 'shell-exec 1))
      (define resp (send-request! gw req 5000))
      (check-equal? (ipc-response-status resp) 'ok)
      (check-equal? (ipc-response-content resp) "bash")
      (gateway-shutdown! gw))

    ;; ── Test 3: Gateway IPC worker lifecycle ──
    (test-case "worker lifecycle: start → alive → shutdown → dead"
      (define gw (start-mock-worker))
      (check-true (gateway-alive? gw) "alive after start")
      (sleep 0.1)
      ;; Verify it can handle a request while alive
      (define req (ipc-request "life-1" "bash" (hasheq) 5000 #f 'shell-exec 1))
      (define resp (send-request! gw req 5000))
      (check-equal? (ipc-response-status resp) 'ok)
      ;; Shutdown
      (gateway-shutdown! gw)
      (sleep 0.1)
      (check-false (gateway-alive? gw) "not alive after shutdown"))

    ;; ── Test 4: Worker response preserves request-id ──
    (test-case "worker response preserves request-id"
      (define gw (start-mock-worker))
      (sleep 0.1)
      (define req (ipc-request "my-unique-id-123" "git" (hasheq) 5000 #f 'git-write 1))
      (define resp (send-request! gw req 5000))
      (check-equal? (ipc-response-request-id resp) "my-unique-id-123")
      (check-equal? (ipc-response-status resp) 'ok)
      (gateway-shutdown! gw))

    ;; ── Test 5: Worker auto-restart after crash ──
    (test-case "worker restart after crash creates new live worker"
      (define gw (start-mock-worker))
      (sleep 0.1)
      (check-true (gateway-alive? gw))
      ;; Restart
      (define gw2 (gateway-restart! gw racket-bin (list (path->string mock-worker-path) "echo")))
      (sleep 0.2)
      (check-true (gateway-alive? gw2) "new worker is alive after restart")
      ;; New worker should respond
      (define req (ipc-request "restart-1" "bash" (hasheq) 5000 #f 'shell-exec 1))
      (define resp (send-request! gw2 req 5000))
      (check-equal? (ipc-response-status resp) 'ok)
      (gateway-shutdown! gw2))

    ;; ── Test 6: Sequential requests work correctly ──
    (test-case "sequential requests each get correct response"
      (define gw (start-mock-worker))
      (sleep 0.1)
      (for ([i (in-range 5)])
        (define req
          (ipc-request (format "seq-~a" i) (format "tool-~a" i) (hasheq) 5000 #f 'read-only 1))
        (define resp (send-request! gw req 5000))
        (check-equal? (ipc-response-status resp) 'ok)
        (check-equal? (ipc-response-content resp) (format "tool-~a" i)))
      (gateway-shutdown! gw))

    ;; ── Test 7: Real dispatch mode returns actual bash output ──
    (test-case "real dispatch mode returns actual bash output"
      (define gw (start-mock-worker "real"))
      (sleep 0.1)
      (define req
        (ipc-request "real-1" "bash" (hasheq 'command "echo chartest") 5000 #f 'shell-exec 1))
      (define resp (send-request! gw req 5000))
      (check-equal? (ipc-response-status resp) 'ok)
      (check-true (string-contains? (ipc-response-content resp) "chartest"))
      (gateway-shutdown! gw))

    ;; ── Test 8: gateway-stderr collects worker stderr ──
    (test-case "gateway-stderr collects worker stderr output"
      (define gw (start-mock-worker "stderr"))
      (sleep 0.2)
      (define req (ipc-request "stderr-1" "bash" (hasheq) 5000 #f 'shell-exec 1))
      (define resp (send-request! gw req 5000))
      (check-equal? (ipc-response-status resp) 'ok)
      (sleep 0.1)
      (define err-text (gateway-stderr gw))
      (check-true (string-contains? err-text "something went wrong"))
      (gateway-shutdown! gw))

    ;; ── Test 9: Timeout for slow worker ──
    (test-case "timeout response for slow worker"
      (define gw (start-mock-worker "slow"))
      (sleep 0.2)
      (define req (ipc-request "timeout-1" "bash" (hasheq) 100 #f 'shell-exec 1))
      (define resp (send-request! gw req 100))
      (check-equal? (ipc-response-status resp) 'timeout)
      (gateway-shutdown! gw))))

(run-tests characterization-suite)
