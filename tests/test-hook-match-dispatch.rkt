#lang racket/base

;; test-hook-match-dispatch.rkt — Tests for classify-hook-result (data-return)
;; v0.32.4: Migrated from CPS handle-hook-result to data-return classify-hook-result.

(require rackunit
         racket/match
         racket/base
         (only-in "../util/hook-types.rkt"
                  hook-result
                  hook-result?
                  hook-result-action
                  hook-result-payload)
         (only-in "../agent/loop-messages.rkt" classify-hook-result))

;; ── Block action → returns (list 'block payload) ──

(test-case "classify-hook-result: block → (list 'block payload)"
  (define hr (hook-result 'block "Access denied"))
  (check-equal? (classify-hook-result hr) '(block "Access denied")))

;; ── Allow action → returns 'pass ──

(test-case "classify-hook-result: allow → 'pass"
  (define hr (hook-result 'allow #f))
  (check-equal? (classify-hook-result hr) 'pass))

;; ── #f (no hook installed) → returns 'pass ──

(test-case "classify-hook-result: #f → 'pass"
  (check-equal? (classify-hook-result #f) 'pass))

;; ── Pass action → returns 'pass ──

(test-case "classify-hook-result: pass → 'pass"
  (define hr (hook-result 'pass #f))
  (check-equal? (classify-hook-result hr) 'pass))

;; ── Amend action → returns (list 'amend payload) ──

(test-case "classify-hook-result: amend → (list 'amend payload)"
  (define hr (hook-result 'amend (hasheq 'text "modified")))
  (check-equal? (classify-hook-result hr) (list 'amend (hasheq 'text "modified"))))

;; ── Unknown action → returns 'pass (safe default) ──

(test-case "classify-hook-result: unknown action → 'pass"
  (define hr (hook-result 'unknown-thing #f))
  (check-equal? (classify-hook-result hr) 'pass))

;; ── Match dispatch works correctly ──

(test-case "classify-hook-result: match dispatch for block"
  (define hr (hook-result 'block "stop"))
  (define result
    (match (classify-hook-result hr)
      [(list 'block payload) (list 'caught payload)]
      [(list 'amend payload) (list 'amended payload)]
      [_ 'pass]))
  (check-equal? result '(caught "stop")))

(test-case "classify-hook-result: match dispatch for amend"
  (define hr (hook-result 'amend "new-text"))
  (define result
    (match (classify-hook-result hr)
      [(list 'block payload) (list 'caught payload)]
      [(list 'amend payload) (list 'amended payload)]
      [_ 'pass]))
  (check-equal? result '(amended "new-text")))

(test-case "classify-hook-result: match dispatch for pass"
  (define result
    (match (classify-hook-result #f)
      [(list 'block payload) (list 'caught payload)]
      [(list 'amend payload) (list 'amended payload)]
      [_ 'pass]))
  (check-equal? result 'pass))
