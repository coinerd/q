#lang racket/base

;; runtime/iteration/transition-logic.rkt — iteration loop termination and transition checks
;;
;; Pure decision functions for loop transitions: cancellation, shutdown,
;; soft/hard limits, turn blocking.

(require (only-in "../../util/cancellation.rkt" cancellation-token? cancellation-token-cancelled?))

(provide check-cancellation
         check-soft-limit
         check-hard-limit)

;; Check if the loop should terminate due to cancellation or shutdown.
;; Returns (values should-stop? reason-symbol) or (values #f #f).
(define (check-cancellation token shutdown-check force-shutdown-check)
  (cond
    [(and force-shutdown-check (force-shutdown-check)) (values #t 'force-shutdown)]
    [(and token (cancellation-token-cancelled? token)) (values #t 'cancellation-token)]
    [(and shutdown-check (shutdown-check)) (values #t 'graceful-shutdown)]
    [else (values #f #f)]))

;; Check if soft iteration limit is reached.
(define (check-soft-limit iteration max-iterations max-iterations-hard)
  (and (>= (add1 iteration) max-iterations) (< (add1 iteration) max-iterations-hard)))

;; Check if hard iteration limit is reached.
(define (check-hard-limit iteration max-iterations-hard)
  (>= (add1 iteration) max-iterations-hard))
