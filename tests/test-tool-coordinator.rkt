#lang racket

;; tests/test-tool-coordinator.rkt — tests for runtime/tool-coordinator.rkt
;;
;; Tests for the extracted tool coordinator (A-02, v0.16.1 Wave 4).
;; Covers: extract-tool-calls-from-messages, make-tool-result-messages.

(require rackunit
         rackunit/text-ui
         json
         (only-in "../util/protocol-types.rkt"
                  make-message
                  make-text-part
                  make-tool-call-part
                  message-role
                  message-content
                  tool-call-part?
                  tool-call-part-id
                  tool-call-part-name
                  tool-call-part-arguments
                  make-tool-result-part
                  tool-result-content
                  tool-result-is-error?
                  tool-result-part-is-error?
                  make-tool-call
                  tool-call-name)
         (only-in "../tools/tool.rkt" make-success-result make-error-result)
         "../runtime/tool-coordinator.rkt")

(define-test-suite
 test-tool-coordinator-suite
 (test-case "extract-tool-calls-from-messages returns empty for text-only messages"
   (define msg (make-message "m1" #f 'user 'message (list (make-text-part "hello")) 1000 (hasheq)))
   (check-equal? (extract-tool-calls-from-messages (list msg)) '()))
 (test-case "extract-tool-calls-from-messages finds tool calls in assistant messages"
   (define msg
     (make-message "m1"
                   #f
                   'assistant
                   'message
                   (list (make-tool-call-part "tc1" "read_file" "{\"path\":\"/tmp/x\"}"))
                   1000
                   (hasheq)))
   (define result (extract-tool-calls-from-messages (list msg)))
   (check-equal? (length result) 1)
   (check-equal? (tool-call-name (first result)) "read_file"))
 (test-case "extract-tool-calls-from-messages skips user messages"
   (define user-msg
     (make-message "m1" #f 'user 'message (list (make-text-part "hello")) 1000 (hasheq)))
   (define asst-msg
     (make-message "m2"
                   "m1"
                   'assistant
                   'message
                   (list (make-tool-call-part "tc1" "bash" "{\"cmd\":\"ls\"}"))
                   1000
                   (hasheq)))
   (define result (extract-tool-calls-from-messages (list user-msg asst-msg)))
   (check-equal? (length result) 1)
   (check-equal? (tool-call-name (first result)) "bash"))
 (test-case "extract-tool-calls-from-messages returns empty for empty list"
   (check-equal? (extract-tool-calls-from-messages '()) '()))
 (test-case "make-tool-result-messages creates correct number of messages"
   (define tc (list (make-tool-call "tc1" "read" (hasheq 'path "/tmp/x"))))
   (define results (list (make-success-result "file contents")))
   (define msgs (make-tool-result-messages tc results "parent-1"))
   (check-equal? (length msgs) 1)
   (check-equal? (message-role (first msgs)) 'tool))
 (test-case "make-tool-result-messages preserves error flag"
   (define tc (list (make-tool-call "tc1" "bash" (hasheq 'cmd "bad"))))
   (define results (list (make-error-result "command failed")))
   (define msgs (make-tool-result-messages tc results "parent-1"))
   (check-equal? (length msgs) 1)
   ;; The tool-result-part has is-error? field directly
   (define msg (first msgs))
   (define part (first (message-content msg)))
   (check-true (tool-result-part-is-error? part)))
 (test-case "make-tool-result-messages pairs calls with results"
   (define tcs (list (make-tool-call "tc1" "read" (hasheq)) (make-tool-call "tc2" "write" (hasheq))))
   (define results (list (make-success-result "ok") (make-success-result "done")))
   (define msgs (make-tool-result-messages tcs results "p1"))
   (check-equal? (length msgs) 2)))

(run-tests test-tool-coordinator-suite)
