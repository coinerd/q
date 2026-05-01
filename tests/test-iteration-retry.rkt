#lang racket

;; tests/test-iteration-retry.rkt — tests for iteration/retry-policy

(require rackunit
         rackunit/text-ui
         "../runtime/iteration/retry-policy.rkt")

(define retry-tests
  (test-suite "Iteration Retry Policy"

    (test-case "check-mid-turn-budget! returns estimated count"
      (define config (hash 'max-context-tokens 128000))
      (define estimated
        (check-mid-turn-budget! '() #f "test" config
                                #:estimate-tokens (lambda (texts) 100)))
      (check-equal? estimated 0))

    (test-case "call-with-overflow-recovery: passthrough on success"
      (define result
        (call-with-overflow-recovery
         (lambda () 42)
         '()
         #f
         "test"
         #:compact-proc (lambda (ctx) (error "should not be called"))))
      (check-equal? result 42))))

(module+ main
  (run-tests retry-tests))
(module+ test
  (run-tests retry-tests))
