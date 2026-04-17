#lang racket

;; tests/test-rpc-ui-adapter.rkt — FEAT-80: Extension UI over RPC (e2e)

(require rackunit
         rackunit/text-ui
         json
         "../wiring/rpc-ui-adapter.rkt"
         "../extensions/ui-channel.rkt")

(define rpc-ui-tests
  (test-suite "extension UI over RPC"

    ;; --------------------------------------------------------
    ;; Unit: handler returns error without requestId
    ;; --------------------------------------------------------
    (test-case "make-rpc-ui-response-handler returns error without requestId"
      (define ui-ch (make-ui-channel))
      (define handler (make-rpc-ui-response-handler ui-ch))
      (define result (handler (hasheq 'value #t)))
      (check-equal? (hash-ref result 'status) "error")
      (check-not-false (hash-ref result 'message #f)))

    ;; --------------------------------------------------------
    ;; Unit: handler returns error for unknown requestId
    ;; --------------------------------------------------------
    (test-case "make-rpc-ui-response-handler returns error for unknown requestId"
      (define ui-ch (make-ui-channel))
      (define handler (make-rpc-ui-response-handler ui-ch))
      (define result (handler (hasheq 'requestId "nonexistent-999" 'value #t)))
      (check-equal? (hash-ref result 'status) "error")
      (check-true (string-contains? (hash-ref result 'message "") "unknown requestId")))

    ;; --------------------------------------------------------
    ;; Unit: ui-channel creation works
    ;; --------------------------------------------------------
    (test-case "ui-channel creation works"
      (define ch (make-ui-channel))
      (check-true (ui-channel? ch)))

    ;; --------------------------------------------------------
    ;; E2E: bridge sends notification, handler delivers response
    ;; --------------------------------------------------------
    (test-case "e2e: bridge → notification → handler → response channel"
      ;; 1. Set up channel and output port
      (define ui-ch (make-ui-channel))
      (define out (open-output-string))

      ;; 2. Start the bridge
      (start-rpc-ui-bridge! ui-ch out)
      ;; Yield to let bridge thread start (replaces sleep 0.05)
      (sync/timeout 0.05 never-evt)

      ;; 3. Create a ui-request with a response channel
      (define resp-ch (make-channel))
      (define req (ui-request 'confirm resp-ch "Continue?"))

      ;; 4. Send request onto the ui-channel
      (thread (lambda () (channel-put ui-ch req)))
      ;; Yield to let bridge thread process (replaces sleep 0.1)
      (sync/timeout 0.1 never-evt)

      ;; 5. Read from the output port and verify JSON notification
      (define output (get-output-string out))
      (check-not-equal? output "")
      (define parsed (read-json (open-input-string output)))
      (check-true (hash? parsed))
      (check-equal? (hash-ref parsed 'jsonrpc #f) "2.0")
      (check-equal? (hash-ref parsed 'method #f) "ui.confirm")
      (define params (hash-ref parsed 'params #f))
      (check-true (hash? params))
      (define request-id (hash-ref params 'requestId #f))
      (check-true (string? request-id))

      ;; 6. Create response handler
      (define handler (make-rpc-ui-response-handler ui-ch))

      ;; 7. Call handler with a response (in a thread, since channel-put
      ;;    blocks until someone reads)
      (thread (lambda () (handler (hasheq 'requestId request-id 'value #t))))

      ;; 8. Read from response channel and verify
      (define resp (channel-get resp-ch))
      (check-true (ui-response? resp))
      (check-equal? (ui-response-value resp) #t)
      (check-false (ui-response-cancelled? resp)))

    ;; --------------------------------------------------------
    ;; E2E: bridge sends notification for select request
    ;; --------------------------------------------------------
    (test-case "e2e: bridge handles select request type"
      (define ui-ch (make-ui-channel))
      (define out (open-output-string))
      (start-rpc-ui-bridge! ui-ch out)
      (sync/timeout 0.05 never-evt)

      (define resp-ch (make-channel))
      (define req
        (ui-select-request 'select resp-ch "Pick one:" '(("a" . "Option A") ("b" . "Option B")) #f))
      (thread (lambda () (channel-put ui-ch req)))
      (sync/timeout 0.1 never-evt)

      (define output (get-output-string out))
      (define parsed (read-json (open-input-string output)))
      (check-equal? (hash-ref parsed 'method #f) "ui.select"))))

(run-tests rpc-ui-tests)
