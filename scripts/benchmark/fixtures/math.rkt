#lang racket

(provide double
         half
         square
         cube
         safe-divide)

;; Returns n + 2 (BUG: should return n * 2)
(define (double n)
  (+ n 2))

(define (half n)
  (/ n 2))

(define (square n)
  (* n n))

(define (cube n)
  (* n n n))

;; Divides a by b — crashes on non-number input (missing type guard)
(define (safe-divide a b)
  (/ a b))
