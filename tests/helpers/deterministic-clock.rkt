#lang racket/base

;; @speed fast
;; @suite default
;; @boundary unit

;; tests/helpers/deterministic-clock.rkt — shared deterministic clock/sleeper seam
;;
;; W1 of v1.00.28 (test workload reduction): logical timing semantics in tests
;; must run on deterministic clock/sleeper seams instead of real wall-clock
;; waits. This helper is the canonical seam for remediated families:
;;   - a fake clock records every requested logical delay (labelled, ordered)
;;   - advancing logical time costs zero wall time
;;   - a sleeper procedure is a drop-in replacement wherever a family injects
;;     a (delay-ms) -> any callable
;; Existing per-family seams (fast-fixtures with-deterministic-retries,
;; auto-retry #:now-proc, sync/timeout-on-event patterns) remain valid; this
;; helper unifies new remediations behind one auditable API.

(provide make-fake-clock
         fake-clock?
         fake-clock-now
         fake-clock-advance!
         fake-clock-sleep!
         fake-clock-events
         fake-clock-total-delay-ms
         fake-clock-sleeper
         fake-clock-reset!)

(struct fake-clock (now-box events-box))

(define (make-fake-clock #:start-ms [start-ms 0])
  (fake-clock (box start-ms) (box '())))

(define (fake-clock-now c)
  (unbox (fake-clock-now-box c)))

(define (fake-clock-advance! c ms)
  (set-box! (fake-clock-now-box c) (+ ms (unbox (fake-clock-now-box c))))
  (void))

;; Record a logical delay: appends a labelled event and advances logical time
;; by exactly delay-ms. Never blocks.
(define (fake-clock-sleep! c delay-ms [label 'unlabeled])
  (set-box! (fake-clock-events-box c)
            (append (unbox (fake-clock-events-box c))
                    (list (vector label delay-ms (fake-clock-now c)))))
  (fake-clock-advance! c delay-ms))

(define (fake-clock-events c)
  (unbox (fake-clock-events-box c)))

(define (fake-clock-total-delay-ms c)
  (for/sum ([e (in-list (fake-clock-events c))]) (vector-ref e 1)))

;; Drop-in sleeper: (lambda (delay-ms) ...) suitable wherever a family
;; injects its delay callable; each call is recorded under `label`.
(define (fake-clock-sleeper c [label 'unlabeled])
  (lambda (delay-ms) (fake-clock-sleep! c delay-ms label)))

(define (fake-clock-reset! c [start-ms 0])
  (set-box! (fake-clock-now-box c) start-ms)
  (set-box! (fake-clock-events-box c) '())
  (void))
