#lang racket

;; tests/test-rpc-ui-adapter.rkt — FEAT-80: Extension UI over RPC

(require rackunit
         rackunit/text-ui
         "../wiring/rpc-ui-adapter.rkt"
         "../extensions/ui-channel.rkt")

(define rpc-ui-tests
  (test-suite
   "extension UI over RPC"

   (test-case "make-rpc-ui-response-handler returns ok for valid response"
     (define ui-ch (make-ui-channel))
     (define handler (make-rpc-ui-response-handler ui-ch))
     (define result (handler (hasheq 'requestId "ui-123" 'value #t)))
     (check-equal? (hash-ref result 'status) "ok")
     (check-equal? (hash-ref result 'requestId) "ui-123"))

   (test-case "make-rpc-ui-response-handler returns error without requestId"
     (define ui-ch (make-ui-channel))
     (define handler (make-rpc-ui-response-handler ui-ch))
     (define result (handler (hasheq 'value #t)))
     (check-equal? (hash-ref result 'status) "error")
     (check-not-false (hash-ref result 'message #f)))

   (test-case "ui-channel creation works"
     (define ch (make-ui-channel))
     (check-true (ui-channel? ch)))

   (test-case "start-rpc-ui-bridge! starts without error"
     (define ui-ch (make-ui-channel))
     (define out (open-output-string))
     (start-rpc-ui-bridge! ui-ch out)
     ;; Give the thread a moment to start
     (sleep 0.05)
     (check-true #t))

   (test-case "response handler accepts various value types"
     (define ui-ch (make-ui-channel))
     (define handler (make-rpc-ui-response-handler ui-ch))
     ;; Boolean
     (check-equal? (hash-ref (handler (hasheq 'requestId "r1" 'value #t)) 'status) "ok")
     ;; String
     (check-equal? (hash-ref (handler (hasheq 'requestId "r2" 'value "text")) 'status) "ok")
     ;; Number
     (check-equal? (hash-ref (handler (hasheq 'requestId "r3" 'value 42)) 'status) "ok"))))

(run-tests rpc-ui-tests)
