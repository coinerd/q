#lang racket/base
;; BOUNDARY: pure

;; tests/test-effect-executor.rkt — Step effect executor tests (F2)

(require rackunit
         rackunit/text-ui
         racket/contract
         "../runtime/iteration/effect-executor.rkt")

(define effect-executor-tests
  (test-suite "effect-executor"

    (test-case "step-effect:append-entries construction"
      (define e (step-effect:append-entries '(msg1 msg2)))
      (check-true (step-effect:append-entries? e))
      (check-equal? (step-effect:append-entries-entries e) '(msg1 msg2)))

    (test-case "step-effect:emit-event construction"
      (define e (step-effect:emit-event "test.event" (hasheq 'key 'val)))
      (check-true (step-effect:emit-event? e))
      (check-equal? (step-effect:emit-event-name e) "test.event"))

    (test-case "step-effect? predicate accepts all variants"
      (for ([e (list (step-effect:append-entries '()) (step-effect:emit-event "x" #f))])
        (check-true (step-effect? e))))))

(run-tests effect-executor-tests)
