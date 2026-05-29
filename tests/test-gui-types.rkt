#lang racket/base

(require rackunit
         rackunit/text-ui
         "../gui/gui-types.rkt")

(define-test-suite test-gui-types
  (test-case "gui-message struct"
    (define msg (make-gui-message "user" "hello"))
    (check-equal? (gui-message-role msg) "user")
    (check-equal? (gui-message-text msg) "hello"))

  (test-case "gui-state defaults"
    (define gs (make-gui-state))
    (check-equal? (gui-state-messages gs) '())
    (check-equal? (gui-state-status gs) 'idle)
    (check-false (gui-state-model gs)))

  (test-case "gui-state-add-message"
    (define gs (make-gui-state))
    (define gs2 (gui-state-add-message gs (make-gui-message "user" "hi")))
    (check-equal? (length (gui-state-messages gs2)) 1)
    (check-equal? (gui-state-messages gs) '())) ; immutable original

  (test-case "gui-state-update-last-message"
    (define gs (make-gui-state))
    (define gs2 (gui-state-add-message gs (make-gui-message "assistant" "hello")))
    (define gs3 (gui-state-update-last-message gs2
                  (lambda (m) (gui-message "assistant" (string-append (gui-message-text m) " world")))))
    (check-equal? (gui-message-text (car (gui-state-messages gs3))) "hello world"))

  (test-case "gui-state-set-status"
    (define gs (gui-state-set-status (make-gui-state) 'processing))
    (check-equal? (gui-state-status gs) 'processing))

  (test-case "gui-message->hash round-trip"
    (define msg (make-gui-message "tool" "[bash: ls]"))
    (define h (gui-message->hash msg))
    (check-equal? (hash-ref h 'role) "tool")
    (define msg2 (hash->gui-message h))
    (check-equal? (gui-message-role msg2) "tool")
    (check-equal? (gui-message-text msg2) "[bash: ls]"))

  (test-case "gui-state->hash round-trip"
    (define gs (gui-state-add-message (make-gui-state) (make-gui-message "user" "test")))
    (define h (gui-state->hash gs))
    (check-equal? (length (hash-ref h 'messages)) 1)
    (define gs2 (hash->gui-state h))
    (check-equal? (length (gui-state-messages gs2)) 1)
    (check-equal? (gui-message-role (car (gui-state-messages gs2))) "user"))

  (test-case "gui-state-update-last-message on empty"
    (define gs (make-gui-state))
    (define gs2 (gui-state-update-last-message gs (lambda (m) m)))
    (check-equal? (gui-state-messages gs2) '()))
  )

(run-tests test-gui-types)
