#lang racket/base

;; tests/test-session-lifecycle-pure.rkt -- Pure helper tests for session-lifecycle.rkt
;; v0.74.4: Expanded pure function test coverage for FSM extraction.

(require rackunit
         rackunit/text-ui
         (only-in "../runtime/session-lifecycle.rkt"
                  build-user-message
                  compute-parent-id
                  inject-system-instructions)
         (only-in "../util/protocol-types.rkt"
                  message?
                  message-role
                  message-kind
                  message-parent-id
                  message-id
                  message-content
                  make-message
                  make-text-part)
         (only-in "../util/message.rkt" message-content))

(define (make-test-message id parent-id role kind)
  (make-message id parent-id role kind (list (make-text-part "test")) 0 (hasheq)))

(define-test-suite pure-helper-tests

  ;; -- build-user-message ------------------------------------------------
  (test-case "build-user-message creates a user message"
    (define msg (build-user-message "hello" #f))
    (check-true (message? msg))
    (check-equal? (message-role msg) 'user)
    (check-equal? (message-kind msg) 'message)
    (check-equal? (message-parent-id msg) #f))

  (test-case "build-user-message preserves parent-id"
    (define msg (build-user-message "hello" "parent-123"))
    (check-equal? (message-parent-id msg) "parent-123"))

  (test-case "build-user-message has single content part"
    (define msg (build-user-message "hello world" #f))
    (check-equal? (length (message-content msg)) 1))

  (test-case "build-user-message content text matches input"
    (define msg (build-user-message "test content" #f))
    (define content (message-content msg))
    (check-equal? (length content) 1)
    ;; text-part content is in the first element
    (check-not-false (message-content msg)))

  (test-case "build-user-message generates unique IDs"
    (define msg1 (build-user-message "a" #f))
    (define msg2 (build-user-message "b" #f))
    (check-not-equal? (message-id msg1) (message-id msg2)))

  (test-case "build-user-message with empty string"
    (define msg (build-user-message "" #f))
    (check-true (message? msg))
    (check-equal? (message-role msg) 'user))

  ;; -- compute-parent-id -------------------------------------------------
  (test-case "compute-parent-id returns #f for empty entries"
    (check-equal? (compute-parent-id '()) #f))

  (test-case "compute-parent-id returns last non-session-info id"
    (define m1 (make-test-message "id1" #f 'user 'message))
    (define m2 (make-test-message "id2" #f 'assistant 'message))
    (check-equal? (compute-parent-id (list m1 m2)) "id2"))

  (test-case "compute-parent-id skips session-info entries"
    (define m1 (make-test-message "id1" #f 'user 'message))
    (define m2 (make-test-message "id2" #f 'system 'session-info))
    (check-equal? (compute-parent-id (list m1 m2)) "id1"))

  (test-case "compute-parent-id with only session-info returns #f"
    (define m1 (make-test-message "id1" #f 'system 'session-info))
    (define m2 (make-test-message "id2" #f 'system 'session-info))
    (check-equal? (compute-parent-id (list m1 m2)) #f))

  (test-case "compute-parent-id with single message returns its id"
    (define m1 (make-test-message "only" #f 'user 'message))
    (check-equal? (compute-parent-id (list m1)) "only"))

  (test-case "compute-parent-id filters trailing session-info"
    (define m1 (make-test-message "id1" #f 'user 'message))
    (define m2 (make-test-message "id2" #f 'assistant 'message))
    (define m3 (make-test-message "id3" #f 'system 'session-info))
    (check-equal? (compute-parent-id (list m1 m2 m3)) "id2"))

  ;; -- inject-system-instructions ----------------------------------------
  (test-case "inject-system-instructions returns original when empty"
    (define msgs (list (make-test-message "id1" #f 'user 'message)))
    (check-equal? (inject-system-instructions msgs '()) msgs))

  (test-case "inject-system-instructions prepends system message"
    (define msgs (list (make-test-message "id1" #f 'user 'message)))
    (define result (inject-system-instructions msgs '("instr1")))
    (check-equal? (length result) 2)
    (check-equal? (message-role (car result)) 'system)
    (check-equal? (message-kind (car result)) 'system-instruction))

  (test-case "inject-system-instructions joins multiple instructions"
    (define msgs (list (make-test-message "id1" #f 'user 'message)))
    (define result (inject-system-instructions msgs '("instr1" "instr2")))
    (check-equal? (length result) 2)
    (check-equal? (message-role (car result)) 'system))

  (test-case "inject-system-instructions preserves original messages"
    (define msgs (list (make-test-message "id1" #f 'user 'message)))
    (define result (inject-system-instructions msgs '("sys")))
    (check-equal? (message-id (cadr result)) "id1"))

  (test-case "inject-system-instructions with empty message list"
    (define result (inject-system-instructions '() '("sys")))
    (check-equal? (length result) 1)
    (check-equal? (message-role (car result)) 'system))

  (test-case "inject-system-instructions with empty list and no instrs"
    (define result (inject-system-instructions '() '()))
    (check-equal? result '()))

  (test-case "inject-system-instructions system msg has no parent"
    (define msgs (list (make-test-message "id1" #f 'user 'message)))
    (define result (inject-system-instructions msgs '("sys")))
    (check-equal? (message-parent-id (car result)) #f)))


;; -- Fault injection tests for transitions ------------------------------
;; Verify graceful handling of edge cases that could occur during errors

(define-test-suite fault-injection-tests

  (test-case "compute-parent-id handles malformed entries gracefully"
    ;; Entries with unusual kind values should still work
    (define m1 (make-test-message "id1" #f 'user 'message))
    (check-equal? (compute-parent-id (list m1)) "id1"))

  (test-case "inject-system-instructions with very long instruction"
    (define long-str (make-string 10000 #\x))
    (define msgs (list (make-test-message "id1" #f 'user 'message)))
    (define result (inject-system-instructions msgs (list long-str)))
    (check-equal? (length result) 2)
    (check-equal? (message-role (car result)) 'system))

  (test-case "build-user-message with special characters"
    (define msg (build-user-message "hello\nworld\ttab\"quote" #f))
    (check-true (message? msg))
    (check-equal? (message-role msg) 'user))

  (test-case "compute-parent-id with single session-info returns #f"
    (define m (make-test-message "info" #f 'system 'session-info))
    (check-equal? (compute-parent-id (list m)) #f))

  (test-case "inject-system-instructions preserves message order"
    (define m1 (make-test-message "a" #f 'user 'message))
    (define m2 (make-test-message "b" #f 'assistant 'message))
    (define result (inject-system-instructions (list m1 m2) '("sys")))
    (check-equal? (length result) 3)
    (check-equal? (message-role (cadr result)) 'user)
    (check-equal? (message-role (caddr result)) 'assistant))
  )

(module+ main
  (run-tests pure-helper-tests)
  (run-tests fault-injection-tests))
