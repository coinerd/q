#lang racket/base

;; test-termination-reasons.rkt — Exhaustiveness: every known termination reason handled
;; v0.29.1 W0: Test scaffolding (function does not exist yet — tests should FAIL)

(require rackunit
         racket/base
         (only-in "../util/loop-result.rkt"
                  make-loop-result)
         (only-in "../runtime/iteration.rkt"
                  iteration-ctx
                  decide-next-action
                  known-termination-reasons))

;; Helper: build an iteration-ctx
(define (make-ictx #:iteration [iteration 0]
                   #:max-iter [max-iter 50]
                   #:max-iter-hard [max-iter-hard 100])
  (iteration-ctx iteration 0 0 max-iter max-iter-hard))

(test-case "known-termination-reasons: all reasons handled by decide-next-action"
  (define ctx (make-ictx #:iteration 10))
  (for ([reason (in-list (known-termination-reasons))])
    (define result (make-loop-result '() reason (hasheq)))
    (define action (decide-next-action ctx result))
    (check (λ (v) (member v '(stop stop-hard-limit stop-soft-limit continue)))
           action
           (format "Termination reason ~a returned unexpected action ~a" reason action))))

(test-case "known-termination-reasons: list is non-empty"
  (check-true (and (list? (known-termination-reasons))
                   (> (length (known-termination-reasons)) 0))))

(test-case "known-termination-reasons: includes core reasons"
  (define reasons (known-termination-reasons))
  (for ([required '(completed cancelled tool-calls-pending error)])
    (check-true (member required reasons)
                (format "Missing required termination reason: ~a" required))))
