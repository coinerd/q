#lang racket/base

;; llm/timing.rkt -- Shared timing utility for LLM streaming providers (Q-2)
;;
;; Extracts the repeated _stream-t0 + log-info telemetry pattern.

(require racket/contract
         racket/format)

(provide (contract-out [log-stream-setup-timing (-> string? real? void?)]))

;; Log the elapsed time since stream setup started.
;; Usage: (define t0 (current-inexact-milliseconds)) ... (log-stream-setup-timing "provider" t0)
(define (log-stream-setup-timing provider-name start-ms)
  (log-info (format "[telemetry] ~a-stream setup completed in ~a ms"
                    provider-name
                    (real->decimal-string (- (current-inexact-milliseconds) start-ms) 1))))
