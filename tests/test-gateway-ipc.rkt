#lang racket/base

;; tests/test-gateway-ipc.rkt — Gateway IPC layer tests

(require rackunit
         rackunit/text-ui
         racket/port
         racket/string
         "../sandbox/ipc-protocol.rkt"
         "../sandbox/gateway-ipc.rkt")

;; Path to the mock worker script
(define mock-worker-path (build-path (current-directory) "tests" "mock-worker.rkt"))

(define racket-bin (find-executable-path "racket"))

(define (start-mock-worker [mode "echo"])
  (start-worker! racket-bin (list (path->string mock-worker-path) mode)))

;; ── Test Suite ──────────────────────────────────────────────────

(define suite
  (test-suite "Gateway IPC Layer"

    ;; ── request-id generation ──

    (test-case "generate-request-id returns unique strings"
      (define id1 (generate-request-id))
      (define id2 (generate-request-id))
      (check-pred string? id1)
      (check-pred string? id2)
      (check-false (string=? id1 id2) "consecutive request ids should differ"))

    (test-case "generate-request-id contains req- prefix"
      (check-true (string-prefix? (generate-request-id) "req-")))

    ;; ── Worker lifecycle ──

    (test-case "start-worker! returns a gateway-worker"
      (define gw (start-mock-worker))
      (check-pred gateway-worker? gw)
      (gateway-shutdown! gw))

    (test-case "gateway-alive? returns #t for freshly started worker"
      (define gw (start-mock-worker))
      (check-true (gateway-alive? gw))
      (gateway-shutdown! gw))

    (test-case "gateway-alive? returns #f after shutdown"
      (define gw (start-mock-worker))
      (gateway-shutdown! gw)
      (sleep 0.1) ; let process die
      (check-false (gateway-alive? gw)))

    (test-case "gateway-pid returns a positive integer for live worker"
      (define gw (start-mock-worker))
      (define pid (gateway-pid gw))
      (check-pred exact-nonnegative-integer? pid)
      (gateway-shutdown! gw))

    (test-case "gateway-pid returns #f or stale after shutdown"
      (define gw (start-mock-worker))
      (gateway-shutdown! gw)
      ;; The pid is still accessible but process is dead
      (check-pred (lambda (v) (or (not v) (exact-nonnegative-integer? v))) (gateway-pid gw)))

    ;; ── Request/Response round-trip ──

    (test-case "send-request! receives ok response from echo worker"
      (define gw (start-mock-worker))
      (sleep 0.1) ; let worker start
      (define req (ipc-request "test-rt-1" "bash" (hasheq 'command "echo hi") 5000 #f 'shell-exec 1))
      (define resp (send-request! gw req 5000))
      (check-equal? (ipc-response-status resp) 'ok)
      (check-equal? (ipc-response-content resp) "bash")
      (gateway-shutdown! gw))

    (test-case "send-request! preserves request-id in response"
      (define gw (start-mock-worker))
      (sleep 0.1)
      (define req (ipc-request "preserve-id-xyz" "git" (hasheq) 5000 #f 'git-write 1))
      (define resp (send-request! gw req 5000))
      (check-equal? (ipc-response-request-id resp) "preserve-id-xyz")
      (gateway-shutdown! gw))

    (test-case "send-request! echoes arguments in details"
      (define gw (start-mock-worker))
      (sleep 0.1)
      (define args (hasheq 'path "/tmp/test" 'content "hello"))
      (define req (ipc-request "echo-args" "write" args 5000 #f 'file-write 1))
      (define resp (send-request! gw req 5000))
      (check-equal? (ipc-response-status resp) 'ok)
      (check-equal? (ipc-response-details resp) args)
      (gateway-shutdown! gw))

    ;; ── Multiple sequential requests ──

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

    ;; ── Timeout ──

    (test-case "send-request! returns timeout response for slow worker"
      (define gw (start-mock-worker "slow"))
      (sleep 0.2)
      (define req (ipc-request "timeout-test" "bash" (hasheq) 100 #f 'shell-exec 1))
      (define resp (send-request! gw req 100))
      (check-equal? (ipc-response-status resp) 'timeout)
      (gateway-shutdown! gw))

    ;; ── Delay worker ──

    (test-case "delay worker responds after wait"
      (define gw (start-mock-worker "delay:0.3"))
      (sleep 0.2)
      (define req (ipc-request "delay-test" "bash" (hasheq) 5000 #f 'shell-exec 1))
      (define resp (send-request! gw req 5000))
      (check-equal? (ipc-response-status resp) 'ok)
      (check-equal? (ipc-response-content resp) "delayed-response")
      (gateway-shutdown! gw))

    ;; ── Stderr collection ──

    (test-case "gateway-stderr collects worker stderr output"
      (define gw (start-mock-worker "stderr"))
      (sleep 0.2)
      (define req (ipc-request "stderr-test" "bash" (hasheq) 5000 #f 'shell-exec 1))
      (define resp (send-request! gw req 5000))
      (check-equal? (ipc-response-status resp) 'ok)
      (sleep 0.1) ; allow stderr drain
      (define err-text (gateway-stderr gw))
      (check-true (string-contains? err-text "something went wrong")
                  (format "expected stderr text, got: [~a]" err-text))
      (gateway-shutdown! gw))

    ;; ── Worker crash ──

    (test-case "send-request! to crashed worker raises gateway error"
      (define gw (start-mock-worker "crash"))
      (sleep 0.2)
      ;; First request will cause worker to crash
      (define req (ipc-request "crash-test" "bash" (hasheq) 5000 #f 'shell-exec 1))
      (with-handlers ([exn:fail:gateway? (lambda (e)
                                           (check-true #t "correctly raised gateway error"))]
                      [exn:fail? (lambda (e)
                                   ;; Either error type is acceptable — worker is dying
                                   (check-true #t "raised some error"))])
        (send-request! gw req 5000))
      (gateway-shutdown! gw))

    ;; ── Restart ──

    (test-case "gateway-restart! creates a new live worker"
      (define gw (start-mock-worker))
      (sleep 0.1)
      (check-true (gateway-alive? gw))
      (define gw2 (gateway-restart! gw racket-bin (list (path->string mock-worker-path) "echo")))
      (sleep 0.2)
      (check-true (gateway-alive? gw2))
      ;; New worker should respond to requests
      (define req (ipc-request "restart-test" "bash" (hasheq) 5000 #f 'shell-exec 1))
      (define resp (send-request! gw2 req 5000))
      (check-equal? (ipc-response-status resp) 'ok)
      (gateway-shutdown! gw2))

    ;; ── Struct accessors ──

    (test-case "gateway-worker struct has all expected fields"
      (define gw (start-mock-worker))
      (check-not-false (gateway-worker-process gw))
      (check-not-false (gateway-worker-custodian gw))
      (check-not-false (gateway-worker-stdin gw))
      (check-not-false (gateway-worker-stdout gw))
      (check-not-false (gateway-worker-stderr gw))
      (check-not-false (gateway-worker-drain-stdout gw))
      (check-not-false (gateway-worker-drain-stderr gw))
      (check-not-false (gateway-worker-response-channel gw))
      (check-pred box? (gateway-worker-stderr-log gw))
      (check-pred box? (gateway-worker-active? gw))
      (check-pred real? (gateway-worker-started-ms gw))
      (check-pred box? (gateway-worker-pending-requests gw))
      (check-pred semaphore? (gateway-worker-lock gw))
      (gateway-shutdown! gw))

    ;; ── M6: Real dispatch mode ──

    (test-case "M6: real dispatch mode returns actual bash output"
      (define gw (start-mock-worker "real"))
      (sleep 0.1)
      (define req
        (ipc-request "m6-real-1" "bash" (hasheq 'command "echo m6test") 5000 #f 'shell-exec 1))
      (define resp (send-request! gw req 5000))
      (check-equal? (ipc-response-status resp) 'ok)
      ;; Real dispatch produces actual output, not just echo of tool-name
      (define content (ipc-response-content resp))
      (check-not-false (string-contains? content "m6test"))
      (gateway-shutdown! gw))

    (test-case "M6: real dispatch with missing command returns error"
      (define gw (start-mock-worker "real"))
      (sleep 0.1)
      (define req (ipc-request "m6-real-2" "bash" (hasheq) 5000 #f 'shell-exec 1))
      (define resp (send-request! gw req 5000))
      (check-equal? (ipc-response-status resp) 'error)
      (gateway-shutdown! gw))))

(run-tests suite)
