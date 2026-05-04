#lang racket/base

;; test-hook-match-dispatch.rkt — Tests for match-based hook result handling
;; v0.29.1 W0: Test scaffolding (function does not exist yet — tests should FAIL)

(require rackunit
         racket/base
         (only-in "../util/hook-types.rkt"
                  hook-result
                  hook-result?
                  hook-result-action
                  hook-result-payload)
         (only-in "../agent/loop.rkt"
                  handle-hook-result))

;; ── Block action → returns 'blocked with message ──

(test-case "handle-hook-result: block → calls on-block with message"
  (define hr (hook-result 'block "Access denied"))
  (define result
    (handle-hook-result hr
                        (λ (msg) (list 'blocked msg))
                        (λ () 'continued)))
  (check-equal? result '(blocked "Access denied")))

;; ── Allow action → calls on-continue ──

(test-case "handle-hook-result: allow → calls on-continue"
  (define hr (hook-result 'allow #f))
  (define result
    (handle-hook-result hr
                        (λ (msg) (list 'blocked msg))
                        (λ () 'continued)))
  (check-equal? result 'continued))

;; ── #f (no hook installed) → calls on-continue ──

(test-case "handle-hook-result: #f → calls on-continue"
  (define result
    (handle-hook-result #f
                        (λ (msg) (list 'blocked msg))
                        (λ () 'continued)))
  (check-equal? result 'continued))

;; ── Pass action → calls on-continue ──

(test-case "handle-hook-result: pass → calls on-continue"
  (define hr (hook-result 'pass #f))
  (define result
    (handle-hook-result hr
                        (λ (msg) (list 'blocked msg))
                        (λ () 'continued)))
  (check-equal? result 'continued))

;; ── Amend action → calls on-continue (amend handled elsewhere) ──

(test-case "handle-hook-result: amend → calls on-continue"
  (define hr (hook-result 'amend (hasheq 'text "modified")))
  (define result
    (handle-hook-result hr
                        (λ (msg) (list 'blocked msg))
                        (λ () 'continued)))
  (check-equal? result 'continued))

;; ── Unknown action → calls on-continue (safe default) ──

(test-case "handle-hook-result: unknown action → calls on-continue"
  (define hr (hook-result 'unknown-thing #f))
  (define result
    (handle-hook-result hr
                        (λ (msg) (list 'blocked msg))
                        (λ () 'continued)))
  (check-equal? result 'continued))
