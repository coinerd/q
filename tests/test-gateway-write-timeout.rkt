#lang racket/base

;; tests/test-gateway-write-timeout.rkt — B3: write deadlock detection
;; @speed fast

(require rackunit
         rackunit/text-ui
         racket/runtime-path
         "../sandbox/gateway-ipc.rkt"
         "../sandbox/ipc-protocol.rkt")

(define-runtime-path mock-worker-path "mock-worker.rkt")
(define racket-bin (find-executable-path "racket"))

(define suite
  (test-suite "Gateway Write Timeout (B3)"

    (test-case "send-request! succeeds normally within write timeout"
      (define gw (start-worker! racket-bin (list (path->string mock-worker-path) "echo")))
      (define req (ipc-request "test-req-1" "echo" (hasheq) 30000 #f 'any 1))
      (define resp (send-request! gw req 30000))
      (check-equal? (ipc-response-status resp) 'ok)
      (gateway-shutdown! gw))

    (test-case "send-request! returns timeout when worker is slow"
      (define gw (start-worker! racket-bin (list (path->string mock-worker-path) "delay:60")))
      (define req (ipc-request "test-req-2" "echo" (hasheq) 5000 #f 'any 1))
      (define resp (send-request! gw req 2000))
      (check-equal? (ipc-response-status resp) 'timeout)
      (gateway-shutdown! gw))

    (test-case "gateway-alive? is #f after shutdown"
      (define gw (start-worker! racket-bin (list (path->string mock-worker-path) "echo")))
      (check-true (gateway-alive? gw))
      (gateway-shutdown! gw)
      (sleep 0.1)
      (check-false (gateway-alive? gw)))))

(run-tests suite)
