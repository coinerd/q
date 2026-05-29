#lang racket/base

(require rackunit
         rackunit/text-ui
         racket/string
         "../gui/gui-types.rkt"
         "../gui/slash-commands.rkt")

(define test-add-system-msg
  (test-suite "add-system-msg!"
    (test-case "adds a system message"
      (define state-box (box (make-gui-state)))
      (define lock (make-semaphore 1))
      (add-system-msg! "hello" state-box lock)
      (define msgs (gui-state-messages (unbox state-box)))
      (check-equal? (length msgs) 1)
      (check-equal? (gui-message-role (car msgs)) "system")
      (check-equal? (gui-message-text (car msgs)) "hello"))

    (test-case "appends to existing messages"
      (define state-box (box (make-gui-state #:messages (list (make-gui-message "user" "hi")))))
      (define lock (make-semaphore 1))
      (add-system-msg! "sys" state-box lock)
      (define msgs (gui-state-messages (unbox state-box)))
      (check-equal? (length msgs) 2)
      (check-equal? (gui-message-role (cadr msgs)) "system"))))

(define test-make-slash-command-handler
  (test-suite "make-slash-command-handler"
    (test-case "returns a procedure"
      (define handler (make-slash-command-handler #f (box (make-gui-state)) (make-semaphore 1)))
      (check-true (procedure? handler)))

    (test-case "returns #f for empty input"
      (define handler (make-slash-command-handler #f (box (make-gui-state)) (make-semaphore 1)))
      (check-equal? (handler "") #f))

    (test-case "handler accepts string input"
      (define state-box (box (make-gui-state)))
      (define lock (make-semaphore 1))
      (define handler (make-slash-command-handler #f state-box lock))
      (check-false (handler "")))))

(define test-known-commands
  (test-suite "known commands"
    (test-case "/clear clears messages"
      (define state-box (box (make-gui-state #:messages (list (make-gui-message "user" "x")))))
      (define lock (make-semaphore 1))
      (define handler (make-slash-command-handler #f state-box lock))
      (check-true (handler "/clear"))
      (check-equal? (gui-state-messages (unbox state-box)) '()))

    (test-case "/help adds help system message"
      (define state-box (box (make-gui-state)))
      (define lock (make-semaphore 1))
      (define handler (make-slash-command-handler #f state-box lock))
      (check-true (handler "/help"))
      (define msgs (gui-state-messages (unbox state-box)))
      (check-equal? (length msgs) 1)
      (check-equal? (gui-message-role (car msgs)) "system")
      (check-true (string-contains? (gui-message-text (car msgs)) "Available commands")))

    (test-case "/compact adds system message"
      (define state-box (box (make-gui-state)))
      (define lock (make-semaphore 1))
      (define handler (make-slash-command-handler #f state-box lock))
      (check-true (handler "/compact"))
      (define msgs (gui-state-messages (unbox state-box)))
      (check-equal? (length msgs) 1)
      (check-true (string-contains? (gui-message-text (car msgs)) "compaction")))))

(define test-extension-dispatch
  (test-suite "extension dispatch"
    (test-case "try-extension-dispatch with #f session returns #f"
      (define state-box (box (make-gui-state)))
      (define lock (make-semaphore 1))
      (check-false (try-extension-dispatch #f state-box lock "/foo")))

    (test-case "unknown command adds 'Unknown command' system message"
      (define state-box (box (make-gui-state)))
      (define lock (make-semaphore 1))
      (define handler (make-slash-command-handler #f state-box lock))
      (check-true (handler "/unknowncommand"))
      (define msgs (gui-state-messages (unbox state-box)))
      (check-equal? (length msgs) 1)
      (check-equal? (gui-message-role (car msgs)) "system")
      (check-true (string-contains? (gui-message-text (car msgs)) "Unknown command")))

    (test-case "handler returns #t even for unknown commands"
      (define state-box (box (make-gui-state)))
      (define lock (make-semaphore 1))
      (define handler (make-slash-command-handler #f state-box lock))
      (check-true (handler "/nope")))))

(run-tests (test-suite "gui-slash-commands"
             test-add-system-msg
             test-make-slash-command-handler
             test-known-commands
             test-extension-dispatch))
