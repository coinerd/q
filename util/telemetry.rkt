#lang racket/base

;; util/telemetry.rkt — Timing/telemetry utilities
;;
;; Extracted from error-helpers.rkt to separate cross-cutting concerns:
;; error handling vs. observability.

(require racket/logging)

(provide with-telemetry)

;; with-telemetry — Execute body, logging operation name and elapsed time (ms).
;; Returns the body result unchanged. Errors propagate to caller.
;;
;; (with-telemetry "compaction"
;;   (expensive-operation ...))
;; => logs "[telemetry] compaction completed in 42.3 ms"
(define-syntax-rule (with-telemetry op-name body ...)
  (let ([t0 (current-inexact-milliseconds)])
    (begin0 (begin
              body ...)
      (log-info (format "[telemetry] ~a completed in ~a ms"
                        op-name
                        (real->decimal-string (- (current-inexact-milliseconds) t0) 1))))))
