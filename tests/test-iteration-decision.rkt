#lang racket/base

;; test-iteration-decision.rkt — Tests for pure decide-next-action function
;; v0.29.1 W0: Test scaffolding (function does not exist yet — tests should FAIL)

(require rackunit
         racket/base
         (only-in "../util/loop-result.rkt" make-loop-result loop-result-termination-reason)
         (only-in "../runtime/iteration/decision.rkt" decide-next-action iteration-ctx))

;; Helper: build an iteration-ctx
(define (make-ictx #:iteration [iteration 0]
                   #:max-iter [max-iter 50]
                   #:max-iter-hard [max-iter-hard 100])
  (iteration-ctx iteration 0 0 max-iter max-iter-hard))

;; ── Completed termination → 'stop ──

(test-case "decide-next-action: completed → stop"
  (define ctx (make-ictx))
  (define result (make-loop-result '() 'completed (hasheq)))
  (check-equal? (decide-next-action ctx result) 'stop))

;; ── Cancelled → 'stop ──

(test-case "decide-next-action: cancelled → stop"
  (define ctx (make-ictx))
  (define result (make-loop-result '() 'cancelled (hasheq)))
  (check-equal? (decide-next-action ctx result) 'stop))

;; ── Force-shutdown → 'stop ──

(test-case "decide-next-action: force-shutdown → stop"
  (define ctx (make-ictx))
  (define result (make-loop-result '() 'force-shutdown (hasheq)))
  (check-equal? (decide-next-action ctx result) 'stop))

;; ── Shutdown → 'stop ──

(test-case "decide-next-action: shutdown → stop"
  (define ctx (make-ictx))
  (define result (make-loop-result '() 'shutdown (hasheq)))
  (check-equal? (decide-next-action ctx result) 'stop))

;; ── Tool calls pending at hard limit → 'stop-hard-limit ──

(test-case "decide-next-action: tool-calls-pending at hard limit → stop-hard-limit"
  (define ctx (make-ictx #:iteration 99 #:max-iter 50 #:max-iter-hard 100))
  (define result (make-loop-result '() 'tool-calls-pending (hasheq)))
  (check-equal? (decide-next-action ctx result) 'stop-hard-limit))

;; ── Tool calls pending at soft limit → 'stop-soft-limit ──

(test-case "decide-next-action: tool-calls-pending at soft limit → stop-soft-limit"
  (define ctx (make-ictx #:iteration 49 #:max-iter 50 #:max-iter-hard 100))
  (define result (make-loop-result '() 'tool-calls-pending (hasheq)))
  (check-equal? (decide-next-action ctx result) 'stop-soft-limit))

;; ── Tool calls pending under limits → 'continue ──

(test-case "decide-next-action: tool-calls-pending under limits → continue"
  (define ctx (make-ictx #:iteration 10 #:max-iter 50 #:max-iter-hard 100))
  (define result (make-loop-result '() 'tool-calls-pending (hasheq)))
  (check-equal? (decide-next-action ctx result) 'continue))

;; ── Error → 'stop ──

(test-case "decide-next-action: error → stop"
  (define ctx (make-ictx))
  (define result (make-loop-result '() 'error (hasheq)))
  (check-equal? (decide-next-action ctx result) 'stop))

;; ── Unknown termination → 'stop ──

(test-case "decide-next-action: unknown termination → stop"
  (define ctx (make-ictx))
  (define result (make-loop-result '() 'some-unknown-reason (hasheq)))
  (check-equal? (decide-next-action ctx result) 'stop))

;; ── Boundary: iteration exactly at hard limit ──

(test-case "decide-next-action: iteration at hard limit boundary"
  (define ctx (make-ictx #:iteration 100 #:max-iter 50 #:max-iter-hard 100))
  (define result (make-loop-result '() 'tool-calls-pending (hasheq)))
  ;; (add1 100) = 101 >= 100 → hard limit
  (check-equal? (decide-next-action ctx result) 'stop-hard-limit))

;; ── Boundary: iteration at soft limit exactly ──

(test-case "decide-next-action: iteration at soft limit boundary"
  (define ctx (make-ictx #:iteration 50 #:max-iter 50 #:max-iter-hard 100))
  (define result (make-loop-result '() 'tool-calls-pending (hasheq)))
  ;; (add1 50) = 51 >= 50 but < 100 → soft limit
  (check-equal? (decide-next-action ctx result) 'stop-soft-limit))
