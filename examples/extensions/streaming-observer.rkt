#lang racket/base

;; examples/extensions/streaming-observer.rkt — streaming hook observation example
;;
;; Demonstrates the fine-grained streaming hooks added in #1208:
;;   - tool.execution.start: observe before each tool executes
;;   - tool.execution.end: observe after each tool completes
;;   - tool.execution.update: amend streaming tool output
;;
;; Usage:
;;   (load-extension! registry "examples/extensions/streaming-observer.rkt")

(require "../../extensions/api.rkt"
         "../../extensions/hooks.rkt")

(provide the-extension)

(define the-extension
  (extension "streaming-observer"
             "1.0.0"
             "1"
             (hasheq 'tool.execution.start
                     (lambda (payload)
                       ;; payload has: tools (list of tool names), count
                       (log-info (format "streaming-observer: ~a tools starting execution"
                                         (hash-ref payload 'count 0)))
                       (hook-pass payload))

                     'tool.execution.end
                     (lambda (payload)
                       ;; payload has: tools (list of tool results), count
                       (log-info (format "streaming-observer: ~a tools completed"
                                         (hash-ref payload 'count 0)))
                       (hook-pass payload)))))

;; Key concepts:
;;   1. tool.execution.start/end are OBSERVATION hooks — only 'pass is valid
;;   2. They fire once per tool batch, not per individual tool
;;   3. Use these for timing metrics, progress reporting, audit trails
;;   4. tool.execution.update (not shown) allows 'amend for modifying streaming output
