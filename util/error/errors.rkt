#lang racket/base
;; STABILITY: public

;; util/errors.rkt — Domain-specific exception types for q
;;
;; Provides structured error types for different error domains:
;;   - q-error: base error with message and optional context hash
;;   - provider-error: → canonical definition in llm/provider-errors.rkt
;;   - tool-error: tool execution failures (tool-name)
;;   - session-error: session lifecycle errors (session-id)

(require racket/contract
         (only-in "deprecation.rkt" warn-deprecated!))

;; Error structs and constructors
(provide warn-deprecated!
         ;; Base error
         q-error
         q-error?
         q-error-context
         ;; Branch 1: LLM
         q-llm-error
         q-llm-error?
         q-llm-error-category
         ;; Branch 2: Tool
         q-tool-error
         q-tool-error?
         q-tool-error-category
         tool-error
         tool-error?
         tool-error-tool-name
         ;; Branch 3: Extension
         q-extension-error
         q-extension-error?
         q-extension-error-category
         extension-error
         extension-error?
         extension-error-extension-name
         extension-error-hook-point
         ;; Branch 4: Session
         q-session-error
         q-session-error?
         q-session-error-category
         session-error
         session-error?
         session-error-session-id
         ;; Other error types
         ui-error
         ui-error?
         ui-error-component
         policy-error
         policy-error?
         policy-error-policy-name
         policy-error-violation
         credential-error
         credential-error?
         credential-error-backend
         credential-error-details
         ;; Branch 5: Browser
         q-browser-error
         q-browser-error?
         q-browser-error-category
         browser-adapter-unavailable?
         browser-page-load-failed?
         browser-action-failed?
         browser-navigation-blocked?
         browser-screenshot-failed?
         browser-sidecar-crashed?
         browser-session-expired?
         browser-url-blocked-error?
         browser-adapter-error?
         ;; Category-based predicates
         llm-timeout?
         llm-rate-limit?
         llm-auth-error?
         tool-permission-denied?
         tool-execution-timeout?
         session-corrupted?
         session-migration-failed?
         (contract-out
          [raise-q-error (->* (string?) (hash?) exn:fail?)]
          [raise-tool-error (->* (string? (or/c string? symbol?)) (hash?) exn:fail?)]
          [raise-session-error (->* (string? string?) (hash?) exn:fail?)]
          [raise-ui-error (->* (string? string?) (hash?) exn:fail?)]
          [raise-extension-error
           (->* (string? (or/c string? symbol?) (or/c string? symbol?)) (hash?) exn:fail?)]
          [raise-policy-error (->* (string? string? string?) (hash?) exn:fail?)]
          [raise-credential-error (->* (string? string?) (string? hash?) exn:fail?)]
          [raise-browser-error (->* (string? symbol?) (hash?) exn:fail?)])
         ;; Quality macros (Q06/Q07)
         with-logged-catch)

;; Base error type for all q domain errors.
;; All q errors carry a context hash for structured metadata.
(struct q-error exn:fail (context) #:transparent)

;; ── Branch 1: LLM errors ────────────────────────────────────
;; Intermediate branch for all LLM/provider errors.
;; provider-error (in llm/provider-errors.rkt) inherits from this.
(struct q-llm-error q-error (category) #:transparent)

;; ── Branch 2: Tool errors ───────────────────────────────────
;; Tool execution failures (permission-denied, execution-timeout, invalid-args).
(struct q-tool-error q-error (category) #:transparent)

;; Legacy tool-error (backward-compat) — inherits from q-tool-error.
(struct tool-error q-tool-error (tool-name) #:transparent)

;; ── Branch 3: Extension errors ──────────────────────────────
;; Extension lifecycle/hook violation errors (load-failure, hook-error, timeout).
(struct q-extension-error q-error (category) #:transparent)

;; Legacy extension-error (backward-compat).
(struct extension-error q-extension-error (extension-name hook-point) #:transparent)

;; ── Branch 4: Session errors ────────────────────────────────
;; Session lifecycle errors (corrupted-journal, migration-failure, write-failure).
(struct q-session-error q-error (category) #:transparent)

;; Legacy session-error (backward-compat).
(struct session-error q-session-error (session-id) #:transparent)

;; ── Branch 5: Browser errors ────────────────────────────────
;; Browser subsystem errors (adapter-unavailable, page-load-failed,
;;   action-failed, navigation-blocked, screenshot-failed,
;;   sidecar-crashed, session-expired).
(struct q-browser-error q-error (category) #:transparent)

;; ── Other error types (flat under q-error) ──────────────────
;; TUI rendering/input errors (recoverable, don't crash session)
(struct ui-error q-error (component) #:transparent)

;; GSD policy violation errors (blocked tools, budget exceeded)
(struct policy-error q-error (policy-name violation) #:transparent)

;; Credential/authentication errors (OAuth, API keys, backend auth)
(struct credential-error q-error (backend details) #:transparent)

;; Browser category predicates
(define (browser-adapter-unavailable? e)
  (and (q-browser-error? e) (eq? (q-browser-error-category e) 'adapter-unavailable)))

(define (browser-page-load-failed? e)
  (and (q-browser-error? e) (eq? (q-browser-error-category e) 'page-load-failed)))

(define (browser-action-failed? e)
  (and (q-browser-error? e) (eq? (q-browser-error-category e) 'action-failed)))

(define (browser-navigation-blocked? e)
  (and (q-browser-error? e) (eq? (q-browser-error-category e) 'navigation-blocked)))

(define (browser-screenshot-failed? e)
  (and (q-browser-error? e) (eq? (q-browser-error-category e) 'screenshot-failed)))

(define (browser-sidecar-crashed? e)
  (and (q-browser-error? e) (eq? (q-browser-error-category e) 'sidecar-crashed)))

(define (browser-session-expired? e)
  (and (q-browser-error? e) (eq? (q-browser-error-category e) 'session-expired)))

;; Category naming note: Plan specified url-blocked, adapter-error, timeout,
;; policy-violation, safe-mode. Implementation uses more specific names.
;; Mapping:
;;   plan 'url-blocked       → policy.rkt raises 'url-blocked
;;   plan 'adapter-error     → service.rkt raises 'adapter-error
;;   plan 'timeout           → sidecar-crashed predicate covers this
;;   plan 'policy-violation  → navigation-blocked predicate covers this
;;   plan 'safe-mode         → session-expired predicate covers safe-mode block
(define (browser-url-blocked-error? e)
  (and (q-browser-error? e) (eq? (q-browser-error-category e) 'url-blocked)))

(define (browser-adapter-error? e)
  (and (q-browser-error? e) (eq? (q-browser-error-category e) 'adapter-error)))

;; Convenience constructors
(define (raise-q-error message [context (hash)])
  (raise (q-error message (current-continuation-marks) context)))

(define (raise-tool-error message tool-name [context (hash)])
  (raise (tool-error message (current-continuation-marks) context 'tool-error tool-name)))

(define (raise-session-error message session-id [context (hash)])
  (raise (session-error message (current-continuation-marks) context 'session-error session-id)))

(define (raise-ui-error message component [context (hash)])
  (raise (ui-error message (current-continuation-marks) context component)))

(define (raise-extension-error message extension-name hook-point [context (hash)])
  (raise (extension-error message
                          (current-continuation-marks)
                          context
                          'extension-error
                          extension-name
                          hook-point)))

(define (raise-policy-error message policy-name violation [context (hash)])
  (raise (policy-error message (current-continuation-marks) context policy-name violation)))

(define (raise-credential-error message backend [details #f] [context (hash)])
  (raise (credential-error message (current-continuation-marks) context backend details)))

(define (raise-browser-error message category [context (hash)])
  (raise (q-browser-error message (current-continuation-marks) context category)))

;; ============================================================
;; Category-based predicates for convenient error matching
;; ============================================================

;; LLM category predicates
(define (llm-timeout? e)
  (and (q-llm-error? e) (eq? (q-llm-error-category e) 'timeout)))

(define (llm-rate-limit? e)
  (and (q-llm-error? e) (eq? (q-llm-error-category e) 'rate-limit)))

(define (llm-auth-error? e)
  (and (q-llm-error? e) (memq (q-llm-error-category e) '(auth context-overflow))))

;; Tool category predicates
(define (tool-permission-denied? e)
  (and (q-tool-error? e) (eq? (q-tool-error-category e) 'permission-denied)))

(define (tool-execution-timeout? e)
  (and (q-tool-error? e) (eq? (q-tool-error-category e) 'execution-timeout)))

;; Session category predicates
(define (session-corrupted? e)
  (and (q-session-error? e) (eq? (q-session-error-category e) 'corrupted-journal)))

(define (session-migration-failed? e)
  (and (q-session-error? e) (eq? (q-session-error-category e) 'migration-failure)))

;; ============================================================
;; Quality macros (Q06/Q07)
;; ============================================================

;; with-logged-catch: Like with-handlers but logs the exception before
;; returning the fallback value. Prevents silent failures.
;; Usage: (with-logged-catch fallback thunk)
;; where thunk is a (lambda () ...)
(define (with-logged-catch fallback thunk)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning "with-logged-catch: caught ~a" (exn-message e))
                               fallback)])
    (thunk)))
