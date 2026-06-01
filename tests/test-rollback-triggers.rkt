#lang racket/base

;; tests/test-rollback-triggers.rkt — Tests for M4 W2 rollback triggers
;; v0.76.3 W2: Observational triggers that emit warnings when context is too aggressive.

(require rackunit
         "../runtime/context-assembly/state-aware-builder.rkt"
         "../runtime/context-assembly/context-floor.rkt"
         "../runtime/context-assembly/task-conclusion.rkt"
         (only-in "../util/protocol-types.rkt" make-message make-text-part message-content))

;; Helpers
(define (make-test-msg text)
  (make-message "id" #f 'user 'text (list (make-text-part text)) (current-seconds) (hasheq)))

(define (make-conclusion text)
  (task-conclusion "c1" text 'fact 'exploration '() (current-seconds) '() '()))

;; ── Trigger 1: Excessive savings warning (savings > 50%) ──

(test-case "trigger-1: excessive savings returns warning when >50% tokens cut"
  (define warnings
    (check-rollback-triggers #:before-messages 1000
                             #:after-messages 400
                             #:conclusion-coverage 0.5
                             #:repeat-tool-count 0))
  (define excessive (filter (λ (w) (eq? (car w) 'excessive-savings)) warnings))
  (check-true (pair? excessive)))

(test-case "trigger-1: no warning when savings < 50%"
  (define warnings
    (check-rollback-triggers #:before-messages 1000
                             #:after-messages 600
                             #:conclusion-coverage 0.5
                             #:repeat-tool-count 0))
  (define excessive (filter (λ (w) (eq? (car w) 'excessive-savings)) warnings))
  (check-false (pair? excessive)))

(test-case "trigger-1: no warning when after-tokens = 0 (no context at all)"
  (define warnings
    (check-rollback-triggers #:before-messages 1000
                             #:after-messages 0
                             #:conclusion-coverage 0.5
                             #:repeat-tool-count 0))
  (define excessive (filter (λ (w) (eq? (car w) 'excessive-savings)) warnings))
  (check-false (pair? excessive)))

;; ── Trigger 2: Low conclusion coverage (amnesia risk) ──

(test-case "trigger-2: low coverage returns warning when < 0.20"
  (define warnings
    (check-rollback-triggers #:before-messages 1000
                             #:after-messages 800
                             #:conclusion-coverage 0.10
                             #:repeat-tool-count 0))
  (define amnesia (filter (λ (w) (eq? (car w) 'amnesia-risk)) warnings))
  (check-true (pair? amnesia)))

(test-case "trigger-2: no warning when coverage >= 0.20"
  (define warnings
    (check-rollback-triggers #:before-messages 1000
                             #:after-messages 800
                             #:conclusion-coverage 0.30
                             #:repeat-tool-count 0))
  (define amnesia (filter (λ (w) (eq? (car w) 'amnesia-risk)) warnings))
  (check-false (pair? amnesia)))

;; ── Trigger 3: Repeated tool calls (same file re-read) ──

(test-case "trigger-3: repeated tool calls returns warning when > 2"
  (define warnings
    (check-rollback-triggers #:before-messages 1000
                             #:after-messages 800
                             #:conclusion-coverage 0.5
                             #:repeat-tool-count 3))
  (define repeated (filter (λ (w) (eq? (car w) 'task-amnesia-detected)) warnings))
  (check-true (pair? repeated)))

(test-case "trigger-3: no warning when repeat count <= 2"
  (define warnings
    (check-rollback-triggers #:before-messages 1000
                             #:after-messages 800
                             #:conclusion-coverage 0.5
                             #:repeat-tool-count 2))
  (define repeated (filter (λ (w) (eq? (car w) 'task-amnesia-detected)) warnings))
  (check-false (pair? repeated)))

;; ── No triggers case ──

(test-case "no triggers when all metrics healthy"
  (define warnings
    (check-rollback-triggers #:before-messages 1000
                             #:after-messages 800
                             #:conclusion-coverage 0.5
                             #:repeat-tool-count 0))
  (check-equal? warnings '()))

;; ── Multiple triggers fire simultaneously ──

(test-case "multiple triggers fire simultaneously"
  (define warnings
    (check-rollback-triggers #:before-messages 1000
                             #:after-messages 400
                             #:conclusion-coverage 0.10
                             #:repeat-tool-count 3))
  (check-equal? (length warnings) 3))
