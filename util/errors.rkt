#lang racket/base

;; util/errors.rkt — Domain-specific exception types for q
;;
;; Provides structured error types for different error domains:
;;   - q-error: base error with message and optional context hash
;;   - provider-error: LLM provider failures (provider name, status-code)
;;   - tool-error: tool execution failures (tool-name)
;;   - session-error: session lifecycle errors (session-id)

(require racket/contract)

(provide
 (struct-out q-error)
 (struct-out provider-error)
 (struct-out tool-error)
 (struct-out session-error)
 raise-q-error
 raise-provider-error
 raise-tool-error
 raise-session-error)

;; Base error type for all q domain errors
(struct q-error exn:fail (context) #:transparent)

;; LLM provider errors
(struct provider-error q-error (provider status-code) #:transparent)

;; Tool execution errors
(struct tool-error q-error (tool-name) #:transparent)

;; Session lifecycle errors
(struct session-error q-error (session-id) #:transparent)

;; Convenience constructors
(define (raise-q-error message [context (hash)])
  (raise (q-error message (current-continuation-marks) context)))

(define (raise-provider-error message provider [status-code #f] [context (hash)])
  (raise (provider-error message (current-continuation-marks) context provider status-code)))

(define (raise-tool-error message tool-name [context (hash)])
  (raise (tool-error message (current-continuation-marks) context tool-name)))

(define (raise-session-error message session-id [context (hash)])
  (raise (session-error message (current-continuation-marks) context session-id)))
