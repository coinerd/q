#lang racket/base

;; BOUNDARY: contract

;; tests/test-tool-coordinator-contracts.rkt — Contract enforcement tests for tool-coordinator
;;
;; W0 scaffolding for v0.29.0 milestone: Verify that tool coordinator functions
;; validate input types correctly.

(require rackunit
         racket/hash
         (only-in "../util/protocol-types.rkt"
                  message?
                  message-role
                  message-content
                  make-message
                  tool-call-part?
                  make-tool-call-part
                  tool-call?
                  make-tool-result
                  tool-result?)
         (only-in "../runtime/tool-coordinator.rkt"
                  extract-tool-calls-from-messages
                  make-tool-result-messages))

;; ── extract-tool-calls-from-messages ──

(test-case "extract-returns-empty-for-no-tool-calls"
  (define msgs (list (make-message "m1" #f 'user 'text '("hello") 1000 (hasheq))))
  (define calls (extract-tool-calls-from-messages msgs))
  (check-equal? calls '()))

(test-case "extract-returns-calls-from-assistant-tool-parts"
  (define msgs
    (list (make-message "m2"
                        #f
                        'assistant
                        'text
                        (list (make-tool-call-part "tc1" "read" (hasheq 'path "/tmp/x")))
                        1000
                        (hasheq))))
  (define calls (extract-tool-calls-from-messages msgs))
  (check-equal? (length calls) 1)
  (check-true (tool-call? (car calls))))

(test-case "extract-ignores-user-messages"
  (define msgs
    (list (make-message "m3"
                        #f
                        'user
                        'text
                        (list (make-tool-call-part "tc2" "bash" (hasheq)))
                        1000
                        (hasheq))))
  (define calls (extract-tool-calls-from-messages msgs))
  ;; tool-call-parts in user messages should be ignored
  (check-equal? calls '()))

(test-case "extract-handles-empty-list"
  (check-equal? (extract-tool-calls-from-messages '()) '()))

(test-case "extract-handles-multiple-assistant-messages"
  (define msgs
    (list (make-message "m4"
                        #f
                        'assistant
                        'text
                        (list (make-tool-call-part "tc3" "read" (hasheq 'path "/a")))
                        1000
                        (hasheq))
          (make-message "m5"
                        #f
                        'assistant
                        'text
                        (list (make-tool-call-part "tc4" "bash" (hasheq 'command "ls")))
                        1000
                        (hasheq))))
  (define calls (extract-tool-calls-from-messages msgs))
  (check-equal? (length calls) 2))

;; ── make-tool-result-messages ──

(test-case "make-tool-result-messages-basic"
  (define tc
    (car (extract-tool-calls-from-messages
          (list (make-message "m6"
                              #f
                              'assistant
                              'text
                              (list (make-tool-call-part "tc5" "read" (hasheq 'path "/x")))
                              1000
                              (hasheq))))))
  (define results
    (list (make-tool-result (list (hasheq 'type "text" 'text "file contents")) (hasheq) #f)))
  (define msgs (make-tool-result-messages (list tc) results "m6"))
  (check-equal? (length msgs) 1)
  (check-true (message? (car msgs))))

(test-case "make-tool-result-messages-empty-calls"
  (define msgs (make-tool-result-messages '() '() "parent"))
  (check-equal? msgs '()))
