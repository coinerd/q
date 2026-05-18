#lang racket/base

;; util/errors.rkt — Domain-specific exception types for q
;;
;; Provides structured error types for different error domains:
;;   - q-error: base error with message and optional context hash
;;   - provider-error: → canonical definition in llm/provider-errors.rkt
;;   - tool-error: tool execution failures (tool-name)
;;   - session-error: session lifecycle errors (session-id)

(require racket/contract)

;; Deprecation warning
(define (warn-deprecated! symbol-name removal-version [extra-notes #f])
  (log-warning (format "DEPRECATED: ~a (will be removed in ~a).~a"
                       symbol-name
                       removal-version
                       (if extra-notes
                           (format " ~a" extra-notes)
                           ""))))

;; Error structs and constructors
(provide warn-deprecated!
         (struct-out q-error)
         (struct-out tool-error)
         (struct-out session-error)
         (struct-out ui-error)
         (struct-out extension-error)
         (struct-out policy-error)
         (struct-out credential-error)
         (contract-out [raise-q-error (->* (string?) (hash?) exn:fail?)]
                       [raise-tool-error (->* (string? string?) (hash?) exn:fail?)]
                       [raise-session-error (->* (string? string?) (hash?) exn:fail?)]
                       [raise-ui-error (->* (string? string?) (hash?) exn:fail?)]
                       [raise-extension-error (->* (string? string? string?) (hash?) exn:fail?)]
                       [raise-policy-error (->* (string? string? string?) (hash?) exn:fail?)]
                       [raise-credential-error (->* (string? string? string?) (hash?) exn:fail?)])
         ;; Quality macros (Q06/Q07)
         with-cleanup
         with-logged-catch)

;; Base error type for all q domain errors
(struct q-error exn:fail (context) #:transparent)

;; Tool execution errors
(struct tool-error q-error (tool-name) #:transparent)

;; Session lifecycle errors
(struct session-error q-error (session-id) #:transparent)

;; TUI rendering/input errors (recoverable, don't crash session)
(struct ui-error q-error (component) #:transparent)

;; Extension lifecycle/hook violation errors
(struct extension-error q-error (extension-name hook-point) #:transparent)

;; GSD policy violation errors (blocked tools, budget exceeded)
(struct policy-error q-error (policy-name violation) #:transparent)

;; Credential/authentication errors (OAuth, API keys, backend auth)
(struct credential-error q-error (backend details) #:transparent)

;; Convenience constructors
(define (raise-q-error message [context (hash)])
  (raise (q-error message (current-continuation-marks) context)))

(define (raise-tool-error message tool-name [context (hash)])
  (raise (tool-error message (current-continuation-marks) context tool-name)))

(define (raise-session-error message session-id [context (hash)])
  (raise (session-error message (current-continuation-marks) context session-id)))

(define (raise-ui-error message component [context (hash)])
  (raise (ui-error message (current-continuation-marks) context component)))

(define (raise-extension-error message extension-name hook-point [context (hash)])
  (raise (extension-error message (current-continuation-marks) context extension-name hook-point)))

(define (raise-policy-error message policy-name violation [context (hash)])
  (raise (policy-error message (current-continuation-marks) context policy-name violation)))

(define (raise-credential-error message backend [details #f] [context (hash)])
  (raise (credential-error message (current-continuation-marks) context backend details)))

;; ============================================================
;; Quality macros (Q06/Q07)
;; ============================================================

;; with-cleanup: Ensure cleanup runs regardless of success/failure.
;; (with-cleanup cleanup-expr body-expr ...)
(define-syntax-rule (with-cleanup cleanup body ...)
  (dynamic-wind (lambda () (void))
                (lambda ()
                  body ...)
                (lambda () cleanup)))

;; with-logged-catch: Like with-handlers but logs the exception before
;; returning the fallback value. Prevents silent failures.
;; Usage: (with-logged-catch fallback thunk)
;; where thunk is a (lambda () ...)
(define (with-logged-catch fallback thunk)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning "with-logged-catch: caught ~a" (exn-message e))
                               fallback)])
    (thunk)))
