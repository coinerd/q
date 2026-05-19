#lang racket/base

;; tools/scheduler-strategy.rkt — Pluggable scheduler strategy (R-15)
;; STABILITY: evolving
;;
;; Defines scheduler-strategy struct with function fields for customizing
;; tool batch execution behavior. Default strategy preserves current behavior.

(require "tool-struct.rkt")

(provide scheduler-strategy
         scheduler-strategy?
         scheduler-strategy-preflight-filter
         scheduler-strategy-execution-order
         tool-invocation-result
         tool-invocation-result?
         tool-invocation-result-tool-name
         tool-success
         tool-success?
         tool-success-content
         tool-success-details
         tool-structured-failure
         tool-structured-failure?
         tool-structured-failure-message
         tool-structured-failure-details
         tool-retryable-failure
         tool-retryable-failure?
         tool-retryable-failure-message
         tool-retryable-failure-error
         tool-policy-denied
         tool-policy-denied?
         tool-policy-denied-reason
         default-scheduler-strategy
         tool-result->invocation-result)

;; ── Tool invocation result tagged union ──

(struct tool-invocation-result (tool-name) #:transparent)
(struct tool-success tool-invocation-result (content details) #:transparent)
(struct tool-structured-failure tool-invocation-result (message details) #:transparent)
(struct tool-retryable-failure tool-invocation-result (message error) #:transparent)
(struct tool-policy-denied tool-invocation-result (reason) #:transparent)

;; ── Scheduler strategy ──

(struct scheduler-strategy
        (preflight-filter ; (listof tool-call) -> (listof tool-call)
         execution-order) ; (listof tool-call) -> (listof tool-call)
  #:transparent)

;; Default strategy: all tools pass preflight, maintain original order, serial execution
(define (default-scheduler-strategy)
  (scheduler-strategy (lambda (calls) calls) ; preflight-filter: pass all
                      (lambda (calls) calls))) ; execution-order: keep order

;; Convert tool-result to tool-invocation-result
(define (tool-result->invocation-result tool-name result)
  (if (tool-result-is-error? result)
      (tool-structured-failure tool-name
                               (format "~a" (tool-result-content result))
                               (tool-result-details result))
      (tool-success tool-name (tool-result-content result) (tool-result-details result))))

;; Need these from tool.rkt (re-import to avoid circular dependency)
(require (only-in "tool.rkt"
                  tool-result?
                  tool-result-content
                  tool-result-details
                  tool-result-is-error?))
