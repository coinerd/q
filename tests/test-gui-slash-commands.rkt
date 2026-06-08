#lang racket/base

(require rackunit
         rackunit/text-ui
         racket/string
         racket/class
         "../gui/gui-types.rkt"
         "../gui/slash-commands.rkt"
         "../runtime/session/session-types.rkt"
         (only-in "../runtime/session/lifecycle-state.rkt" make-lifecycle-state)
         "../extensions/api.rkt"
         "../extensions/hooks.rkt"
         "../util/hook-types.rkt"
         "../agent/queue.rkt")

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

    (test-case "gui-slash-commands: appends to existing messages"
      (define state-box (box (make-gui-state #:messages (list (make-gui-message "user" "hi")))))
      (define lock (make-semaphore 1))
      (add-system-msg! "sys" state-box lock)
      (define msgs (gui-state-messages (unbox state-box)))
      (check-equal? (length msgs) 2)
      (check-equal? (gui-message-role (cadr msgs)) "system"))

    (test-case "add-system-msg! calls notify callback"
      (define state-box (box (make-gui-state)))
      (define lock (make-semaphore 1))
      (define notify-called? (box #f))
      (add-system-msg! "hello" state-box lock (lambda () (set-box! notify-called? #t)))
      (check-true (unbox notify-called?))
      (check-equal? (length (gui-state-messages (unbox state-box))) 1))))

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
      (check-false (handler "")))

    (test-case "handler /help calls notify"
      (define state-box (box (make-gui-state)))
      (define lock (make-semaphore 1))
      (define notify-called? (box #f))
      (define handler
        (make-slash-command-handler #f state-box lock (lambda () (set-box! notify-called? #t))))
      (handler "/help")
      (check-true (unbox notify-called?))
      (check-equal? (length (gui-state-messages (unbox state-box))) 1))))

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

(define test-new-session-dispatch
  (test-suite "new-session dispatch"
    (test-case "try-extension-dispatch handles new-session hook result"
      (define ext-reg (make-extension-registry))
      (define test-ext
        (extension "test-new-session"
                   "1.0"
                   "1"
                   (hasheq 'execute-command
                           (lambda (payload) (hook-amend (hasheq 'new-session "test prompt"))))))
      (register-extension! ext-reg test-ext)
      (define mock-sess
        (agent-session "test-session"
                       (string->path "/tmp/test")
                       #f
                       #f
                       #f
                       ext-reg
                       "test-model"
                       '()
                       #f
                       (make-queue)
                       (hash)
                       #t
                       0
                       #f
                       #f
                       (make-lifecycle-state)))
      (define state-box (box (make-gui-state)))
      (define lock (make-semaphore 1))
      (check-true (try-extension-dispatch mock-sess state-box lock "/test-new-session"))
      (define msgs (gui-state-messages (unbox state-box)))
      (check-equal? (length msgs) 0))

    (test-case "try-extension-dispatch handles submit hook result"
      (define ext-reg (make-extension-registry))
      (define test-ext
        (extension "test-submit"
                   "1.0"
                   "1"
                   (hasheq 'execute-command
                           (lambda (payload) (hook-amend (hasheq 'submit "test submit text"))))))
      (register-extension! ext-reg test-ext)
      (define mock-sess
        (agent-session "test-session"
                       (string->path "/tmp/test")
                       #f
                       #f
                       #f
                       ext-reg
                       "test-model"
                       '()
                       #f
                       (make-queue)
                       (hash)
                       #t
                       0
                       #f
                       #f
                       (make-lifecycle-state)))
      (define state-box (box (make-gui-state)))
      (define lock (make-semaphore 1))
      (check-true (try-extension-dispatch mock-sess state-box lock "/test-submit")))))

(run-tests (test-suite "gui-slash-commands"
             test-add-system-msg
             test-make-slash-command-handler
             test-known-commands
             test-extension-dispatch
             test-new-session-dispatch))
