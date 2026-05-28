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
     ;; Empty string returns #f (not a command)
     (check-false (handler "")))))

(run-tests (test-suite "gui-slash-commands" test-add-system-msg test-make-slash-command-handler))
