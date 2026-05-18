#lang racket/base

;; tools/scheduler-strategy.rkt — Pluggable scheduler strategy (R-15)
;; STABILITY: evolving
;;
;; Defines scheduler-strategy struct with function fields for customizing
;; tool batch execution behavior. Default strategy preserves current behavior.

(require racket/contract)
(require "tool-struct.rkt")

(provide (contract-out [scheduler-strategy (-> procedure? procedure? scheduler-strategy?)]
                       [scheduler-strategy? (-> any/c boolean?)]
                       [scheduler-strategy-preflight-filter (-> scheduler-strategy? procedure?)]
                       [scheduler-strategy-execution-order (-> scheduler-strategy? procedure?)]
                       [tool-invocation-result (-> string? tool-invocation-result?)]
                       [tool-invocation-result? (-> any/c boolean?)]
                       [tool-invocation-result-tool-name (-> tool-invocation-result? string?)]
                       [tool-success (-> string? any/c any/c tool-success?)]
                       [tool-success? (-> any/c boolean?)]
                       [tool-success-content (-> tool-success? any/c)]
                       [tool-success-details (-> tool-success? any/c)]
                       [tool-structured-failure (-> string? string? any/c tool-structured-failure?)]
                       [tool-structured-failure? (-> any/c boolean?)]
                       [tool-structured-failure-message (-> tool-structured-failure? string?)]
                       [tool-structured-failure-details (-> tool-structured-failure? any/c)]
                       [tool-retryable-failure (-> string? string? any/c tool-retryable-failure?)]
                       [tool-retryable-failure? (-> any/c boolean?)]
                       [tool-retryable-failure-message (-> tool-retryable-failure? string?)]
                       [tool-retryable-failure-error (-> tool-retryable-failure? any/c)]
                       [tool-policy-denied (-> string? string? tool-policy-denied?)]
                       [tool-policy-denied? (-> any/c boolean?)]
                       [tool-policy-denied-reason (-> tool-policy-denied? string?)])
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
