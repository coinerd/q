#lang racket/base

;; benchmarks/metrics.rkt — benchmark result metrics and reporting types
;;
;; Defines the data structures for capturing benchmark results:
;;   - metrics: status, timing, turn count, tool calls, tokens
;;   - task-result: task + metrics + raw output

(provide (struct-out metrics)
         (struct-out task-result)
         make-metrics)

;; ============================================================
;; Structs
;; ============================================================

;; Per-task execution metrics
(struct metrics (status elapsed-ms turns tool-calls tokens) #:transparent)

;; Task execution result wrapper
(struct task-result (task metrics output) #:transparent)

;; ============================================================
;; Constructor helper
;; ============================================================

(define (make-metrics #:status [status 'unknown]
                       #:elapsed-ms [elapsed-ms 0]
                       #:turns [turns 0]
                       #:tool-calls [tool-calls 0]
                       #:tokens [tokens 0])
  (metrics status elapsed-ms turns tool-calls tokens))
