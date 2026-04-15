#lang racket

;; q/tests/test-regressions.rkt — Regression tests for fixed bugs
;;
;; Prevents re-introduction of previously fixed bugs.
;; Each test is named after the bug it prevents.

(require rackunit
         rackunit/text-ui)

;; ============================================================
;; BUG #515: filter-map double-evaluation
;;
;; Old code used `(for/list ([x lst] #:when (f x)) (f x))` which
;; evaluates f twice per element. The fix uses a manual loop that
;; calls f exactly once.
;; ============================================================

(define (filter-map f lst)
  (let loop ([lst lst]
             [acc '()])
    (cond
      [(null? lst) (reverse acc)]
      [else
       (let ([r (f (car lst))])
         (if r
             (loop (cdr lst) (cons r acc))
             (loop (cdr lst) acc)))])))

(test-case "regression: filter-map calls f exactly once per element (BUG #515)"
  (define call-count (box 0))
  (define (counting-f x)
    (set-box! call-count (add1 (unbox call-count)))
    (and (odd? x) (* x 10)))
  (define result (filter-map counting-f '(1 2 3 4 5)))
  ;; Only odd values should be in the result, transformed
  (check-equal? result '(10 30 50))
  ;; f should be called exactly 5 times (once per element), NOT 10
  (check-equal? (unbox call-count) 5
                "filter-map should call f exactly once per element"))

(test-case "regression: filter-map preserves order (BUG #515)"
  (define result (filter-map (lambda (x) (and (positive? x) (* x 2)))
                             '(3 -1 0 5 -2 7)))
  (check-equal? result '(6 10 14)))

(test-case "regression: filter-map with all-false returns empty (BUG #515)"
  (define call-count (box 0))
  (define (always-false x)
    (set-box! call-count (add1 (unbox call-count)))
    #f)
  (define result (filter-map always-false '(1 2 3)))
  (check-equal? result '())
  (check-equal? (unbox call-count) 3
                "filter-map should still call f once even when all false"))

(test-case "regression: filter-map with all-true returns all (BUG #515)"
  (define call-count (box 0))
  (define (always-true x)
    (set-box! call-count (add1 (unbox call-count)))
    (* x 100))
  (define result (filter-map always-true '(1 2 3 4)))
  (check-equal? result '(100 200 300 400))
  (check-equal? (unbox call-count) 4
                "filter-map should call f exactly once per element"))

(test-case "regression: filter-map with empty list (BUG #515)"
  (define call-count (box 0))
  (define (counter x)
    (set-box! call-count (add1 (unbox call-count)))
    x)
  (define result (filter-map counter '()))
  (check-equal? result '())
  (check-equal? (unbox call-count) 0
                "filter-map should not call f on empty list"))
