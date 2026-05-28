#lang racket/base

;; tools/scheduler-strategy.rkt — Pluggable scheduler strategy (R-15)
;; STABILITY: evolving
;;
;; Defines scheduler-strategy struct with function fields for customizing
;; tool batch execution behavior. Default strategy preserves current behavior.

(require racket/contract)

(provide (contract-out [scheduler-strategy (-> procedure? procedure? scheduler-strategy?)]
                       [scheduler-strategy? (-> any/c boolean?)]
                       [scheduler-strategy-preflight-filter (-> scheduler-strategy? procedure?)]
                       [scheduler-strategy-execution-order (-> scheduler-strategy? procedure?)]
                       [default-scheduler-strategy (-> scheduler-strategy?)]))

;; ── Scheduler strategy ──

(struct scheduler-strategy
        (preflight-filter ; (listof tool-call) -> (listof tool-call)
         execution-order) ; (listof tool-call) -> (listof tool-call)
  #:transparent)

;; Default strategy: all tools pass preflight, maintain original order, serial execution
(define (default-scheduler-strategy)
  (scheduler-strategy (lambda (calls) calls) ; preflight-filter: pass all
                      (lambda (calls) calls))) ; execution-order: keep order
