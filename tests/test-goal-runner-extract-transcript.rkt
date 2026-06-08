#lang racket/base

;; @speed fast
;; @suite default

(require rackunit
         (only-in "../runtime/goal/goal-runner.rkt"
                  extract-transcript-from-result)
         (only-in "../util/loop-result.rkt"
                  make-loop-result)
         (only-in "../runtime/session/session-config.rkt"
                  current-goal-loop-enabled?))

(test-case "extract-transcript-from-result handles loop-result? struct"
  (define msgs (list 'msg1 'msg2))
  (define lr (make-loop-result msgs 'achieved (hasheq)))
  (check-equal? (extract-transcript-from-result lr) msgs))

(test-case "extract-transcript-from-result handles hash?"
  (define h (hasheq 'messages '(a b c)))
  (check-equal? (extract-transcript-from-result h) '(a b c)))

(test-case "extract-transcript-from-result handles list?"
  (check-equal? (extract-transcript-from-result '(x y)) '(x y)))

(test-case "extract-transcript-from-result handles unknown type"
  (check-equal? (extract-transcript-from-result 42) '()))

(test-case "current-goal-loop-enabled? default is #t"
  (check-true (current-goal-loop-enabled?)))

(test-case "current-goal-loop-enabled? can be set to #f"
  (parameterize ([current-goal-loop-enabled? #f])
    (check-false (current-goal-loop-enabled?))))
