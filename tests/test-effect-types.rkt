#lang racket/base
;; BOUNDARY: pure

;; tests/test-effect-types.rkt — Effect descriptor type tests (F1)

(require rackunit
         rackunit/text-ui
         "../agent/effect-types.rkt")

(define effect-types-tests
  (test-suite "effect-types"

    (test-case "effect:none construction"
      (define e (effect:none))
      (check-true (effect:none? e)))

    (test-case "effect:emit-event construction"
      (define e (effect:emit-event 'turn-start (hasheq 'test #t)))
      (check-equal? (effect:emit-event-type e) 'turn-start)
      (check-true (hash? (effect:emit-event-payload e))))

    (test-case "effect:update-fsm construction"
      (define e (effect:update-fsm 'emit-start 'turn-start))
      (check-equal? (effect:update-fsm-from-state e) 'emit-start)
      (check-equal? (effect:update-fsm-event e) 'turn-start))

    (test-case "effect:dispatch-hook construction"
      (define e (effect:dispatch-hook 'agent-start (hasheq)))
      (check-equal? (effect:dispatch-hook-hook-point e) 'agent-start))

    (test-case "effect predicates"
      (check-true (effect:none? (effect:none)))
      (check-true (effect:emit-event? (effect:emit-event 'test #f)))
      (check-true (effect:update-fsm? (effect:update-fsm 'a 'b)))
      (check-true (effect:dispatch-hook? (effect:dispatch-hook 'test #f))))

    (test-case "effect? contract accepts all variants"
      (for ([e (list (effect:none)
                     (effect:emit-event 'x #f)
                     (effect:update-fsm 'a 'b)
                     (effect:dispatch-hook 'c #f))])
        (check-true (effect? e))))))

(run-tests effect-types-tests)
