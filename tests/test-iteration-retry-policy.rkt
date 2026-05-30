#lang racket
;; BOUNDARY: pure
;; BOUNDARY: unit
;; tests/test-iteration-retry-policy.rkt -- Pure retry-policy tests (T-1a)

(require rackunit
         rackunit/text-ui
         racket/string
         "../runtime/iteration/retry-policy.rkt"
         (only-in "../util/message.rkt" make-message))

(define estimate-suite
  (test-suite "compute-mid-turn-estimate"

    (test-case "empty context returns zero estimated tokens"
      (define-values (estimated threshold max-toks)
        (compute-mid-turn-estimate '()
                                   (hasheq 'max-context-tokens 1000)
                                   (lambda (msgs) 100)))
      (check-equal? estimated 0)
      (check-equal? max-toks 1000)
      (check-equal? threshold (exact-floor (* 1000 9/10))))

    (test-case "default max-context-tokens when not in config"
      (define-values (estimated threshold max-toks)
        (compute-mid-turn-estimate '() (hasheq) (lambda (msgs) 100)))
      (check-equal? max-toks 128000))

    (test-case "estimate with messages returns sum"
      (define msgs (list (make-message "1" #f (quote user) (quote text)
                                        (list (hasheq 'type "text" 'text "hello world"))
                                        0 (hasheq))))
      (define-values (estimated threshold max-toks)
        (compute-mid-turn-estimate msgs
                                   (hasheq 'max-context-tokens 4000)
                                   (lambda (msgs) (length msgs))))
      (check-equal? estimated 1))
    ))

(define exploration-suite
  (test-suite "detect-exploration-loop"

    (test-case "repeating 2-tool pattern is detected"
      (define tools '("read" "grep" "read" "grep" "read" "grep"))
      (define result (detect-exploration-loop tools))
      (check-not-false result)
      (check-true (string-contains? result "exploration loop detected")))

    (test-case "non-repeating pattern is not detected"
      (define tools '("read" "grep" "bash" "edit" "write" "find"))
      (check-false (detect-exploration-loop tools)))

    (test-case "empty list is not detected"
      (check-false (detect-exploration-loop '())))

    (test-case "short list is not detected"
      (check-false (detect-exploration-loop '("read" "grep"))))

    (test-case "single tool repeated is not a pair loop"
      (check-false (detect-exploration-loop '("read" "read" "read" "read"))))
    ))

(define count-suite
  (test-suite "count-occurrences"

    (test-case "known list produces correct hash"
      (define h (count-occurrences '(a b a c b a)))
      (check-equal? (hash-ref h 'a) 3)
      (check-equal? (hash-ref h 'b) 2)
      (check-equal? (hash-ref h 'c) 1))

    (test-case "empty list produces empty hash"
      (define h (count-occurrences '()))
      (check-equal? (hash-keys h) '()))

    (test-case "list of lists works as keys"
      (define h (count-occurrences '(("a" "b") ("c" "d") ("a" "b"))))
      (check-equal? (hash-ref h '("a" "b")) 2))
    ))

(define overflow-suite
  (test-suite "call-with-overflow-recovery"

    (test-case "normal thunk returns result"
      (define result (call-with-overflow-recovery
                       (lambda () 42) '() "test-session"))
      (check-equal? result 42))

    (test-case "error thunk re-raises"
      (check-exn exn:fail?
        (lambda ()
          (call-with-overflow-recovery
            (lambda () (error "test-error"))
            '() "test-session"))))

    (test-case "emit-event callback not called for success"
      (define called? (box #f))
      (call-with-overflow-recovery
        (lambda () 'ok) '() "test-session"
        #:emit-event (lambda (type data) (set-box! called? #t)))
      (check-false (unbox called?)))
    ))

(define mid-turn-suite
  (test-suite "estimate-mid-turn-tokens"

    (test-case "returns estimate for empty context"
      (define result (estimate-mid-turn-tokens '() #f (hasheq 'max-context-tokens 1000)))
      (check-true (exact-nonnegative-integer? result)))

    (test-case "with custom estimate-tokens"
      (define msgs (list (make-message "1" #f (quote user) (quote text)
                                        (list (hasheq 'type "text" 'text "test"))
                                        0 (hasheq))))
      (define result (estimate-mid-turn-tokens
                       msgs
                       "s1"
                       (hasheq 'max-context-tokens 1000)
                       #:estimate-tokens (lambda (msgs) 42)))
      (check-equal? result 42))
    ))

(run-tests estimate-suite 'verbose)
(run-tests exploration-suite 'verbose)
(run-tests count-suite 'verbose)
(run-tests overflow-suite 'verbose)
(run-tests mid-turn-suite 'verbose)
