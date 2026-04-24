#lang racket

(provide add
         subtract
         multiply
         divide
         modulo
         power)

;; Basic calculator — no error handling at all.
;; Each function should validate inputs and raise informative errors.

(define (add a b)
  (+ a b))

(define (subtract a b)
  (- a b))

(define (multiply a b)
  (* a b))

(define (divide a b)
  (/ a b))

(define (modulo a b)
  (remainder a b))

(define (power base exponent)
  (expt base exponent))
