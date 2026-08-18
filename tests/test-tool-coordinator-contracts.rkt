#lang racket/base

;; @speed fast  ;; @suite runtime
;; @boundary unit

;; BOUNDARY: contract

;; tests/test-tool-coordinator-contracts.rkt — Contract enforcement tests for tool-coordinator
;;
;; W0 scaffolding for v0.29.0 milestone: Verify that tool coordinator functions
;; validate input types correctly.

(require rackunit
         racket/hash
         (only-in "../util/message/protocol-types.rkt"
                  message?
                  message-role
                  message-content
                  make-message
                  tool-call-part?
                  make-tool-call-part
                  tool-call?
                  tool-call-id
                  make-tool-result
                  tool-result?)
         (only-in "../runtime/tool-coordinator.rkt"
                  extract-tool-calls-from-messages
                  make-tool-result-messages
                  find-malformed-tool-calls))

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

;; ── v0.99.78 Bug A: malformed tool-call arguments ──

(test-case "malformed-args-do-not-raise-and-are-excluded"
  ;; The exact malformed payload from the live KZQK7B52 session:
  ;; {"path": "/home/user/src/q-agent/tests", "all?: false} — missing closing
  ;; quote after all? and missing closing brace.
  (define msgs
    (list (make-message
           "m6"
           #f
           'assistant
           'text
           (list (make-tool-call-part "tc-bad"
                                      "ls"
                                      "{\"path\": \"/home/user/src/q-agent/tests\", \"all?: false}"))
           1000
           (hasheq))))
  ;; Must NOT raise; the malformed call is excluded from executable calls.
  (check-equal? (extract-tool-calls-from-messages msgs) '())
  (define malformed (find-malformed-tool-calls msgs))
  (check-equal? (length malformed) 1)
  (check-equal? (hash-ref (car malformed) 'id) "tc-bad")
  (check-equal? (hash-ref (car malformed) 'name) "ls")
  (check-equal? (hash-ref (car malformed) 'raw)
                "{\"path\": \"/home/user/src/q-agent/tests\", \"all?: false}"))

(test-case "mixed-valid-and-malformed-keeps-only-valid"
  (define msgs
    (list (make-message "m7"
                        #f
                        'assistant
                        'text
                        (list (make-tool-call-part "tc-ok" "read" (hasheq 'path "/a"))
                              (make-tool-call-part "tc-bad2" "bash" "{not json}"))
                        1000
                        (hasheq))))
  (define calls (extract-tool-calls-from-messages msgs))
  (check-equal? (length calls) 1)
  (check-equal? (tool-call-id (car calls)) "tc-ok")
  (define malformed (find-malformed-tool-calls msgs))
  (check-equal? (length malformed) 1)
  (check-equal? (hash-ref (car malformed) 'id) "tc-bad2"))

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
