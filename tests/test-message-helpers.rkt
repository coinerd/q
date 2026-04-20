#lang racket

;; tests/test-message-helpers.rkt — shared utility consolidation tests

(require rackunit
         rackunit/text-ui
         "../util/message-helpers.rkt"
         "../util/protocol-types.rkt")

(define message-helper-tests
  (test-suite
   "util/message-helpers"

   ;; ============================================================
   ;; has-tool-calls?
   ;; ============================================================
   (test-case "has-tool-calls? returns #t for message with tool-call part"
     (define msg (make-message "m1" #f 'assistant 'message
                               (list (make-text-part "hello")
                                     (make-tool-call-part "tc1" "bash" "{}"))
                               1000 (hasheq)))
     (check-true (has-tool-calls? msg)))

   (test-case "has-tool-calls? returns #f for text-only message"
     (define msg (make-message "m2" #f 'assistant 'message
                               (list (make-text-part "hello"))
                               1000 (hasheq)))
     (check-false (has-tool-calls? msg)))

   (test-case "has-tool-calls? returns #f for user message"
     (define msg (make-message "m3" #f 'user 'message
                               (list (make-text-part "hi"))
                               1000 (hasheq)))
     (check-false (has-tool-calls? msg)))

   ;; ============================================================
   ;; tool-result-message?
   ;; ============================================================
   (test-case "tool-result-message? returns #t for tool-result kind"
     (define msg (make-message "m4" #f 'tool 'tool-result
                               '() 1000 (hasheq)))
     (check-not-false (tool-result-message? msg)))

   (test-case "tool-result-message? returns #t for bash-execution kind"
     (define msg (make-message "m5" #f 'tool 'bash-execution
                               '() 1000 (hasheq)))
     (check-not-false (tool-result-message? msg)))

   (test-case "tool-result-message? returns #f for regular message"
     (define msg (make-message "m6" #f 'user 'message
                               (list (make-text-part "hi"))
                               1000 (hasheq)))
     (check-false (tool-result-message? msg)))

   ;; ============================================================
   ;; get-tool-call-ids
   ;; ============================================================
   (test-case "get-tool-call-ids extracts IDs from tool-call parts"
     (define msg (make-message "m7" #f 'assistant 'message
                               (list (make-tool-call-part "tc-1" "bash" "{}")
                                     (make-text-part "text")
                                     (make-tool-call-part "tc-2" "read" "{}"))
                               1000 (hasheq)))
     (check-equal? (get-tool-call-ids msg) '("tc-1" "tc-2")))

   (test-case "get-tool-call-ids returns empty for no tool calls"
     (define msg (make-message "m8" #f 'assistant 'message
                               (list (make-text-part "hi"))
                               1000 (hasheq)))
     (check-equal? (get-tool-call-ids msg) '()))
   ))

(module+ main
  (run-tests message-helper-tests))

(module+ test
  (run-tests message-helper-tests))
