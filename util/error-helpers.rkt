#lang racket/base

;;; error-helpers.rkt — Error handling macros for common fallback patterns.
;;;
;;; Finding A7: 73+ `(with-handlers ([exn:fail? (lambda (_) #f)]) ...)` occurrences.
;;; Finding A8: 28+ `(with-handlers ([exn:fail? (lambda (e) (log-warning ...) #f)]) ...)` occurrences.
;;; Gate criterion: 3+ repetitions (73, 28) + domain concept naming.

(require racket/logging)
(provide with-safe-fallback
         with-logged-error
         with-telemetry)

;; with-safe-fallback — Execute body, returning DEFAULT on any exn:fail.
;;
;; (with-safe-fallback #f
;;   (dangerous-operation ...))
(define-syntax-rule (with-safe-fallback default body ...)
  (with-handlers ([exn:fail? (lambda (_) default)])
    body ...))

;; with-logged-error — Execute body, logging exception message and returning #f.
;; For a custom default, wrap the call and use with-safe-fallback instead.
;;
;; (with-logged-error "operation failed"
;;   (dangerous-operation ...))
(define-syntax-rule (with-logged-error msg body ...)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning (format "~a: ~a" msg (exn-message e)))
                               #f)])
    body ...))

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
