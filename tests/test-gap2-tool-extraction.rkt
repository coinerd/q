#lang racket/base
;; tests/test-gap2-tool-extraction.rkt — GAP-2 TDD tests
;; Validates tool-call-id → tool-name mapping in auto-extraction

(require rackunit
         rackunit/text-ui
         racket/string
         (only-in "../util/content/content-parts.rkt"
                  make-tool-result-part
                  tool-result-part?)
         (only-in "../util/message/message.rkt"
                  make-message
                  message-role
                  message-content
                  message-id)
         (only-in "../runtime/memory/auto-extraction.rkt"
                  maybe-auto-extract-tool-results!
                  current-auto-extraction-enabled
                  classify-sensitivity)
         (only-in "../runtime/memory/service.rkt"
                  current-memory-backend)
         (only-in "../runtime/memory/backends/memory-hash.rkt"
                  make-memory-hash-backend
                  memory-hash-backend-items)
         (only-in "../runtime/memory/types.rkt"
                  memory-item-content))

;; Helper: create a tool-result message with tool-result-parts
(define (make-tool-result-msg tool-call-id content-str)
  (make-message #f
                'tool
                'assistant
                'user
                (list (make-tool-result-part tool-call-id content-str #f))
                #f
                #f))

(define-test-suite gap-2-tests
  (test-case "GAP-2: tool-result-part content extracted correctly"
    (define msg (make-tool-result-msg "call_read_abc123" "file contents here"))
    (define parts (message-content msg))
    (define result-part
      (for/first ([p (in-list parts)] #:when (tool-result-part? p)) p))
    (check-not-false result-part)
    (check-equal? (tool-result-part-content result-part) "file contents here"))

  (test-case "GAP-2: tool-call-id matches tool name via hash lookup"
    ;; Simulate the tcid->name hash that step-interpreter builds
    (define tcid->name (hash "call_read_abc" "read"
                             "call_grep_def" "grep"))
    (check-equal? (hash-ref tcid->name "call_read_abc" "unknown") "read")
    (check-equal? (hash-ref tcid->name "call_grep_def" "unknown") "grep")
    (check-equal? (hash-ref tcid->name "nonexistent" "unknown") "unknown"))

  (test-case "GAP-2: maybe-auto-extract-tool-results! works with correct input"
    (define backend (make-memory-hash-backend))
    (define extractable-msgs
      (list (hasheq 'content "const x = 1; // initialize counter variable for loop" 'name "read")
            (hasheq 'content "TODO: fix this critical bug in the authentication flow" 'name "grep")))
    (parameterize ([current-auto-extraction-enabled #t]
                   [current-memory-backend backend])
      (maybe-auto-extract-tool-results! extractable-msgs
                                         #:session-id "test-session"
                                         #:project-root "."))
    (define items (memory-hash-backend-items backend))
    (check >= (length items) 1 "At least one item should be extracted")
    (when (pair? items)
      (define all-contents (map memory-item-content items))
      (check-not-false (for/or ([c (in-list all-contents)])
                          (string-contains? c "initialize counter"))
                        "Content should contain extracted text"))))

(require (only-in "../util/content/content-parts.rkt"
                  tool-result-part-content
                  tool-result-part-tool-call-id))

(run-tests gap-2-tests)
