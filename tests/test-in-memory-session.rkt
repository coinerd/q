#lang racket

;; tests/test-in-memory-session.rkt — GC-18: In-memory session manager
;;
;; Tests the in-memory session manager that provides the same interface
;; as file-backed storage but without disk I/O. Useful for SDK testing.

(require rackunit
         rackunit/text-ui
         "../runtime/session-store.rkt"
         "../util/protocol-types.rkt")

(define in-memory-tests
  (test-suite "in-memory session manager"

    (test-case "make-in-memory-session-manager creates empty manager"
      (define mgr (make-in-memory-session-manager))
      (check-true (in-memory-session-manager? mgr))
      (check-equal? (in-memory-list-sessions mgr) '()))

    (test-case "append and load single message"
      (define mgr (make-in-memory-session-manager))
      (define msg (make-message "m1" #f "user" "text" (list (make-text-part "hello")) 1000 (hash)))
      (in-memory-append! mgr "s1" msg)
      (define loaded (in-memory-load mgr "s1"))
      (check-equal? (length loaded) 1)
      (check-equal? (message-id (car loaded)) "m1"))

    (test-case "append-entries! appends multiple messages atomically"
      (define mgr (make-in-memory-session-manager))
      (define msgs
        (for/list ([i (in-range 5)])
          (make-message (format "m~a" i)
                        #f
                        "user"
                        "text"
                        (list (make-text-part (format "msg ~a" i)))
                        (+ 1000 i)
                        (hash))))
      (in-memory-append-entries! mgr "s1" msgs)
      (define loaded (in-memory-load mgr "s1"))
      (check-equal? (length loaded) 5)
      (check-equal? (message-id (car loaded)) "m0")
      (check-equal? (message-id (list-ref loaded 4)) "m4"))

    (test-case "load returns empty list for unknown session"
      (define mgr (make-in-memory-session-manager))
      (check-equal? (in-memory-load mgr "nonexistent") '()))

    (test-case "list-sessions returns all session IDs"
      (define mgr (make-in-memory-session-manager))
      (define msg (make-message "m1" #f "user" "text" (list (make-text-part "x")) 1000 (hash)))
      (in-memory-append! mgr "s1" msg)
      (in-memory-append! mgr "s2" msg)
      (in-memory-append! mgr "s3" msg)
      (define sessions (in-memory-list-sessions mgr))
      (check-equal? (length sessions) 3)
      (check-not-false (member "s1" sessions))
      (check-not-false (member "s2" sessions))
      (check-not-false (member "s3" sessions)))

    (test-case "fork copies entries to new session"
      (define mgr (make-in-memory-session-manager))
      (for ([i (in-range 5)])
        (define msg
          (make-message (format "m~a" i)
                        #f
                        "user"
                        "text"
                        (list (make-text-part (format "msg ~a" i)))
                        (+ 1000 i)
                        (hash)))
        (in-memory-append! mgr "src" msg))
      (in-memory-fork! mgr "src" "dest" "m2")
      (define forked (in-memory-load mgr "dest"))
      (check-equal? (length forked) 3)
      (check-equal? (message-id (list-ref forked 2)) "m2"))

    (test-case "fork without entry-id copies all"
      (define mgr (make-in-memory-session-manager))
      (for ([i (in-range 3)])
        (define msg
          (make-message (format "m~a" i)
                        #f
                        "user"
                        "text"
                        (list (make-text-part (format "msg ~a" i)))
                        (+ 1000 i)
                        (hash)))
        (in-memory-append! mgr "src" msg))
      (in-memory-fork! mgr "src" "full-copy")
      (define forked (in-memory-load mgr "full-copy"))
      (check-equal? (length forked) 3))

    (test-case "append preserves order across multiple calls"
      (define mgr (make-in-memory-session-manager))
      (for ([i (in-range 3)])
        (define msg
          (make-message (format "m~a" i)
                        #f
                        "user"
                        "text"
                        (list (make-text-part (format "msg ~a" i)))
                        (+ 1000 i)
                        (hash)))
        (in-memory-append! mgr "s1" msg))
      (define loaded (in-memory-load mgr "s1"))
      (check-equal? (map message-id loaded) '("m0" "m1" "m2")))

    (test-case "multiple sessions are independent"
      (define mgr (make-in-memory-session-manager))
      (define msg1 (make-message "m1" #f "user" "text" (list (make-text-part "a")) 1000 (hash)))
      (define msg2 (make-message "m2" #f "user" "text" (list (make-text-part "b")) 2000 (hash)))
      (in-memory-append! mgr "s1" msg1)
      (in-memory-append! mgr "s2" msg2)
      (check-equal? (length (in-memory-load mgr "s1")) 1)
      (check-equal? (length (in-memory-load mgr "s2")) 1)
      (check-equal? (message-id (car (in-memory-load mgr "s1"))) "m1")
      (check-equal? (message-id (car (in-memory-load mgr "s2"))) "m2"))))

(run-tests in-memory-tests)
