#lang racket/base

(require rackunit
         rackunit/text-ui
         racket/string
         "../gui/slash-commands.rkt")

(define test-add-system-msg
  (test-suite
   "add-system-msg!"
   (test-case "adds a system message"
     (define state-box (box (hash 'messages '())))
     (define lock (make-semaphore 1))
     (add-system-msg! "hello" state-box lock)
     (define msgs (hash-ref (unbox state-box) 'messages))
     (check-equal? (length msgs) 1)
     (check-equal? (hash-ref (car msgs) 'role) "system")
     (check-equal? (hash-ref (car msgs) 'text) "hello"))

   (test-case "appends to existing messages"
     (define state-box (box (hash 'messages (list (hash 'role "user" 'text "hi")))))
     (define lock (make-semaphore 1))
     (add-system-msg! "sys" state-box lock)
     (define msgs (hash-ref (unbox state-box) 'messages))
     (check-equal? (length msgs) 2)
     (check-equal? (hash-ref (cadr msgs) 'role) "system"))))

(define test-make-slash-command-handler
  (test-suite
   "make-slash-command-handler"
   (test-case "returns a procedure"
     (define handler (make-slash-command-handler #f (box (hash 'messages '())) (make-semaphore 1)))
     (check-true (procedure? handler)))

   (test-case "returns #f for empty input"
     (define handler (make-slash-command-handler #f (box (hash 'messages '())) (make-semaphore 1)))
     (check-equal? (handler "") #f))

   (test-case "handler accepts string input"
     (define state-box (box (hash 'messages '())))
     (define lock (make-semaphore 1))
     (define handler (make-slash-command-handler #f state-box lock))
     (check-false (handler "")))))

(define test-known-commands
  (test-suite
   "known commands"
   (test-case "/clear clears messages"
     (define state-box (box (hash 'messages (list (hash 'role "user" 'text "x")))))
     (define lock (make-semaphore 1))
     (define handler (make-slash-command-handler #f state-box lock))
     (check-true (handler "/clear"))
     (check-equal? (hash-ref (unbox state-box) 'messages) '()))

   (test-case "/help adds help system message"
     (define state-box (box (hash 'messages '())))
     (define lock (make-semaphore 1))
     (define handler (make-slash-command-handler #f state-box lock))
     (check-true (handler "/help"))
     (define msgs (hash-ref (unbox state-box) 'messages))
     (check-equal? (length msgs) 1)
     (check-equal? (hash-ref (car msgs) 'role) "system")
     (check-true (string-contains? (hash-ref (car msgs) 'text) "Available commands")))

   (test-case "/compact adds system message"
     (define state-box (box (hash 'messages '())))
     (define lock (make-semaphore 1))
     (define handler (make-slash-command-handler #f state-box lock))
     (check-true (handler "/compact"))
     (define msgs (hash-ref (unbox state-box) 'messages))
     (check-equal? (length msgs) 1)
     (check-true (string-contains? (hash-ref (car msgs) 'text) "compaction")))))

(define test-extension-dispatch
  (test-suite
   "extension dispatch"
   (test-case "try-extension-dispatch with #f session returns #f"
     (define state-box (box (hash 'messages '())))
     (define lock (make-semaphore 1))
     (check-false (try-extension-dispatch #f state-box lock "/foo")))

   (test-case "unknown command adds 'Unknown command' system message"
     (define state-box (box (hash 'messages '())))
     (define lock (make-semaphore 1))
     (define handler (make-slash-command-handler #f state-box lock))
     (check-true (handler "/unknowncommand"))
     (define msgs (hash-ref (unbox state-box) 'messages))
     (check-equal? (length msgs) 1)
     (check-equal? (hash-ref (car msgs) 'role) "system")
     (check-true (string-contains? (hash-ref (car msgs) 'text) "Unknown command")))

   (test-case "handler returns #t even for unknown commands"
     (define state-box (box (hash 'messages '())))
     (define lock (make-semaphore 1))
     (define handler (make-slash-command-handler #f state-box lock))
     (check-true (handler "/nope")))))

(run-tests (test-suite "gui-slash-commands"
                        test-add-system-msg
                        test-make-slash-command-handler
                        test-known-commands
                        test-extension-dispatch))
