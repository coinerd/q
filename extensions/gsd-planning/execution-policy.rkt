#lang racket/base

;; extensions/gsd-planning/execution-policy.rkt — phase transitions, tool blocking rules
;;
;; Mode-based tool access control and state machine guard logic.

(require "../gsd/state-machine.rkt"
         "../hooks.rkt")

(provide gsd-tool-guard)

(define (gsd-tool-guard payload)
  (define mode (gsm-current))
  (define tool-name (hash-ref payload 'tool-name #f))
  (define allowed (gsm-tool-allowed? tool-name))
  (cond
    [(and (eq? mode 'executing) (equal? tool-name "planning-write"))
     (hook-block "Cannot update plan during /go. Focus on executing the existing plan.")]
    [(and (not allowed) (eq? mode 'plan-written))
     (hook-block "Plan written to PLAN.md. Use /go to start implementing.")]
    [(not allowed) (hook-block (format "Tool '~a' blocked in ~a mode." tool-name mode))]
    [else (hook-pass payload)]))
