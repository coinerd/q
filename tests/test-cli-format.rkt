#lang racket

;; tests/test-cli-format.rkt — v0.70.8 W0
;; Extracted from test-cli.rkt: format-event-for-terminal tests

(require rackunit
         rackunit/text-ui
         json
         "../util/protocol-types.rkt"
         "../interfaces/cli.rkt")

(define-test-suite
 test-cli-format
 (test-suite "format-event-for-terminal"

   (test-case "assistant.message.completed → suppressed (empty string)"
     (define evt
       (make-event "assistant.message.completed"
                   1000
                   "sess1"
                   "turn1"
                   (hasheq 'content "Hello, world!")))
     (check-equal? (format-event-for-terminal evt) ""))

   (test-case "tool.call.started → [tool: name]"
     (define evt (make-event "tool.call.started" 1000 "sess1" "turn1" (hasheq 'name "read")))
     (check-equal? (format-event-for-terminal evt) "[tool: read]"))

   (test-case "tool.call.completed → [tool result]"
     (define evt
       (make-event "tool.execution.completed"
                   1000
                   "sess1"
                   "turn1"
                   (hasheq 'name "bash" 'summary "exit 0")))
     (check-equal? (format-event-for-terminal evt) "[tool result: bash]"))

   (test-case "tool.call.completed with result content → shows truncated content"
     (define evt
       (make-event "tool.execution.completed"
                   1000
                   "sess1"
                   "turn1"
                   (hasheq 'name "bash" 'result (list (hasheq 'type "text" 'text "hello world")))))
     (check-equal? (format-event-for-terminal evt) "[tool result: hello world]"))

   (test-case "tool.call.completed with multi-part result → joins with newline"
     (define evt
       (make-event "tool.execution.completed"
                   1000
                   "sess1"
                   "turn1"
                   (hasheq 'name
                           "ls"
                           'result
                           (list (hasheq 'type "text" 'text "file1.rkt")
                                 (hasheq 'type "text" 'text "file2.rkt")))))
     (check-equal? (format-event-for-terminal evt) "[tool result: file1.rkt\nfile2.rkt]"))

   (test-case "BUG-18: tool.call.completed with long result → truncated"
     (define long-text (make-string 500 #\A))
     (define evt
       (make-event "tool.execution.completed"
                   1000
                   "sess1"
                   "turn1"
                   (hasheq 'name "bash" 'result (list (hasheq 'type "text" 'text long-text)))))
     (define output (format-event-for-terminal evt))
     (check-true (string-contains? output "..."))
     (check-true (<= (string-length output) 330)))

   (test-case "BUG-18: tool.call.started with command argument → shows command"
     (define evt
       (make-event "tool.call.started"
                   1000
                   "sess1"
                   "turn1"
                   (hasheq 'name "bash" 'arguments (hasheq 'command "curl -s https://example.com"))))
     (define output (format-event-for-terminal evt))
     (check-true (string-contains? output "curl")))

   (test-case "BUG-18: tool.call.started with path argument → shows path"
     (define evt
       (make-event "tool.call.started"
                   1000
                   "sess1"
                   "turn1"
                   (hasheq 'name "read" 'arguments (hasheq 'path "/tmp/test.txt"))))
     (check-equal? (format-event-for-terminal evt) "[tool: read: /tmp/test.txt]"))

   (test-case "BUG-18: model.stream.delta → shows delta text"
     (define evt (make-event "model.stream.delta" 1000 "sess1" "turn1" (hasheq 'delta "Hello ")))
     (check-equal? (format-event-for-terminal evt) "Hello "))

   (test-case "BUG-18: tool.call.started with JSON string arguments → parses and shows command"
     (define args-json (jsexpr->string (hasheq 'command "curl -s https://example.com")))
     (define evt
       (make-event "tool.call.started"
                   1000
                   "sess1"
                   "turn1"
                   (hasheq 'name "bash" 'arguments args-json)))
     (define output (format-event-for-terminal evt))
     (check-true (string-contains? output "curl")))

   (test-case "tool.call.failed → [tool failed: name]"
     (define evt
       (make-event "tool.execution.completed"
                   1000
                   "sess1"
                   "turn1"
                   (hasheq 'name "write" 'error "permission denied")))
     (check-equal? (format-event-for-terminal evt) "[tool failed: write — permission denied]"))

   (test-case "turn.started → empty string"
     (define evt (make-event "turn.started" 1000 "sess1" "turn1" (hasheq)))
     (check-equal? (format-event-for-terminal evt) ""))

   (test-case "turn.completed → empty string"
     (define evt (make-event "turn.completed" 1000 "sess1" "turn1" (hasheq)))
     (check-equal? (format-event-for-terminal evt) ""))

   (test-case "runtime.error → Error: message"
     (define evt (make-event "runtime.error" 1000 "sess1" #f (hasheq 'error "something went wrong")))
     (check-equal? (format-event-for-terminal evt) "Error: something went wrong"))

   (test-case "session.started → session info"
     (define evt (make-event "session.started" 1000 "sess1" #f (hasheq 'sessionId "sess1")))
     (check-equal? (format-event-for-terminal evt) "[session started: sess1]"))

   (test-case "unknown event → empty string"
     (define evt (make-event "something.else" 1000 "sess1" #f (hasheq)))
     (check-equal? (format-event-for-terminal evt) ""))

   ;; Additional tests
   (test-case "assistant message completed → suppressed"
     (check-equal? (format-event-for-terminal
                    (make-event "assistant.message.completed" 0 #f #f (hasheq 'content "Hello")))
                   ""))

   (test-case "tool call started"
     (check-equal?
      (format-event-for-terminal (make-event "tool.call.started" 0 #f #f (hasheq 'name "read")))
      "[tool: read]"))

   (test-case "tool call failed"
     (check-equal?
      (format-event-for-terminal
       (make-event "tool.execution.completed" 0 #f #f (hasheq 'name "bash" 'error "exit 1")))
      "[tool failed: bash — exit 1]"))

   (test-case "runtime error"
     (check-equal?
      (format-event-for-terminal (make-event "runtime.error" 0 #f #f (hasheq 'error "boom")))
      "Error: boom"))

   (test-case "unknown event returns empty string"
     (check-equal? (format-event-for-terminal (make-event "unknown.event" 0 #f #f (hasheq))) ""))

   (test-case "turn started returns empty"
     (check-equal? (format-event-for-terminal (make-event "turn.started" 0 #f #f (hasheq))) ""))))

(run-tests test-cli-format)
