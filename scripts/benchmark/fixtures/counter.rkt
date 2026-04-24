#lang racket

(provide count-up
         sum-to
         count-evens)

;; Counts from 1 to n inclusive.
;; BUG: off-by-one — uses (in-range 1 n) which excludes n.
;;      Should be (in-range 1 (add1 n)).
(define (count-up n)
  (for/list ([i (in-range 1 n)])
    i))

;; Returns the sum 1 + 2 + ... + n.
;; Relies on count-up, so inherits the off-by-one.
(define (sum-to n)
  (apply + (count-up n)))

;; Counts even numbers in 1..n inclusive.
(define (count-evens n)
  (length (filter even? (count-up n))))
