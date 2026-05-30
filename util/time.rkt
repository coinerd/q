#lang racket/base

;; util/time.rkt -- Time utility functions
;; Extracts the repeated (inexact->exact (round (current-inexact-milliseconds))) pattern.

(require racket/contract)

(provide (contract-out [now-epoch-ms (-> exact-nonnegative-integer?)]
                       [now-epoch-secs (-> exact-nonnegative-integer?)]))

;; Returns current time as epoch milliseconds (exact integer)
(define (now-epoch-ms)
  (inexact->exact (round (current-inexact-milliseconds))))

;; Returns current time as epoch seconds (exact integer)
(define (now-epoch-secs)
  (inexact->exact (round (/ (current-inexact-milliseconds) 1000))))
