#lang racket/base

;; tests/test-session-lifecycle-pure.rkt — Pure helper tests for session-lifecycle.rkt
;; v0.49.5: Tests for extracted pure functions.

(require rackunit
         rackunit/text-ui
         (only-in "../../runtime/session-lifecycle.rkt"
                  build-user-message
                  compute-parent-id
                  inject-system-instructions)
         (only-in "../../util/protocol-types.rkt"
                  message?
                  message-role
                  message-kind
                  message-parent-id
                  make-message
                  make-text-part)
         (only-in "../../util/message.rkt" message-content))

(define (make-test-message id parent-id role kind)
  (make-message id parent-id role kind (list (make-text-part "test")) 0 (hasheq)))

(define-test-suite pure-helper-tests
                   ;; ── build-user-message ─────────────────────────────────────
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
                   ;; ── compute-parent-id ──────────────────────────────────────
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
                   ;; ── inject-system-instructions ─────────────────────────────
                   (test-case "inject-system-instructions returns original when empty"
                     (define msgs (list (make-test-message "id1" #f 'user 'message)))
                     (check-equal? (inject-system-instructions msgs '()) msgs))
                   (test-case "inject-system-instructions prepends system message"
                     (define msgs (list (make-test-message "id1" #f 'user 'message)))
                     (define result (inject-system-instructions msgs '("instr1")))
                     (check-equal? (length result) 2)
                     (check-equal? (message-role (car result)) 'system)
                     (check-equal? (message-kind (car result)) 'system-instruction)))

(module+ main
  (run-tests pure-helper-tests))
