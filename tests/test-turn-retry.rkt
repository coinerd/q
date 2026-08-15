#lang racket/base

;; @speed fast
;; @suite default

;; test-turn-retry.rkt — W6 (BUG-0011): bounded turn-level auto-retry
;; Tests the turn-retry policy shape and with-turn-retry semantics:
;; transient failures retried with exponential backoff, non-transient
;; failures surfaced immediately, bound respected when exhausted.

(require rackunit
         racket/list
         "../llm/provider-errors.rkt"
         "../agent/loop-stream.rkt")

(define instant-policy (turn-retry-policy 3 100 400))

(define (transient-exn)
  (raise-provider-error "502 bad gateway" 'server 502))

(define (non-transient-exn)
  (raise-provider-error "unauthorized" 'auth 401))

;; ============================================================
;; Policy shape
;; ============================================================

(test-case "default-turn-retry-policy: bounded max-attempts with backoff schedule"
  (define p (default-turn-retry-policy))
  (check-true (turn-retry-policy? p))
  (check-true (exact-positive-integer? (turn-retry-policy-max-attempts p)))
  (check-true (exact-positive-integer? (turn-retry-policy-base-delay-ms p)))
  (check-true (exact-positive-integer? (turn-retry-policy-max-delay-ms p))))

(test-case "turn-retry-delay-ms: exponential backoff capped at max"
  (check-equal? (turn-retry-delay-ms instant-policy 1) 100)
  (check-equal? (turn-retry-delay-ms instant-policy 2) 200)
  (check-equal? (turn-retry-delay-ms instant-policy 3) 400)
  ;; capped: doubling beyond max stays at max
  (check-equal? (turn-retry-delay-ms instant-policy 10) 400))

;; ============================================================
;; Transient classification
;; ============================================================

(test-case "turn-error-transient?: transient provider error qualifies"
  (check-true (turn-error-transient?
               (with-handlers ([values values]) (transient-exn)))))

(test-case "turn-error-transient?: auth error does not qualify"
  (check-false (turn-error-transient?
                (with-handlers ([values values]) (non-transient-exn)))))

;; ============================================================
;; with-turn-retry semantics
;; ============================================================

(test-case "with-turn-retry: success on first attempt, no retries"
  (define retries '())
  (define result (with-turn-retry
                   (lambda () 'ok)
                   #:policy instant-policy
                   #:sleep (lambda (ms) (set! retries (cons ms retries)))))
  (check-equal? result 'ok)
  (check-equal? retries '()))

(test-case "with-turn-retry: transient failure retried, then succeeds"
  (define attempts 0)
  (define slept '())
  (define retry-events '())
  (define result (with-turn-retry
                   (lambda ()
                     (set! attempts (add1 attempts))
                     (when (< attempts 2) (transient-exn))
                     'recovered)
                   #:policy instant-policy
                   #:sleep (lambda (ms) (set! slept (cons ms slept)))
                   #:on-retry (lambda (e attempt delay-ms)
                                (set! retry-events
                                      (cons (list attempt delay-ms) retry-events)))))
  (check-equal? result 'recovered)
  (check-equal? attempts 2)
  (check-equal? slept '(100))
  (check-equal? retry-events '((1 100))))

(test-case "with-turn-retry: backoff doubles across attempts"
  (define slept '())
  (define result (with-turn-retry
                   (lambda ()
                     (when (< (length slept) 2) (transient-exn))
                     'third-time)
                   #:policy instant-policy
                   #:sleep (lambda (ms) (set! slept (cons ms slept)))))
  (check-equal? result 'third-time)
  (check-equal? (reverse slept) '(100 200)))

(test-case "with-turn-retry: exhausted transient errors are surfaced"
  (define attempts 0)
  (check-exn
   provider-error?
   (lambda ()
     (with-turn-retry
      (lambda ()
        (set! attempts (add1 attempts))
        (transient-exn))
      #:policy instant-policy
      #:sleep void)))
  ;; max-attempts 3 → exactly 3 executions, not more
  (check-equal? attempts 3))

(test-case "with-turn-retry: non-transient error fails immediately, no retry"
  (define attempts 0)
  (define slept '())
  (check-exn
   provider-error?
   (lambda ()
     (with-turn-retry
      (lambda ()
        (set! attempts (add1 attempts))
        (non-transient-exn))
      #:policy instant-policy
      #:sleep (lambda (ms) (set! slept (cons ms slept))))))
  (check-equal? attempts 1)
  (check-equal? slept '()))

(test-case "with-turn-retry: returns thunk value unchanged on success"
  (check-equal? (with-turn-retry (lambda () 42) #:sleep void) 42))
