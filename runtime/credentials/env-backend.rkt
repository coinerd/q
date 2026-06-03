#lang racket/base

;; runtime/credentials/env-backend.rkt — Environment variable credential backend
;; Extracted from credential-backend.rkt (v0.73.5 A-4)

(require racket/contract
         racket/string
         "../../util/error/error-helpers.rkt"
         "../../util/error/errors.rkt"
         (only-in "protocol.rkt"
                  credential-backend
                  credential-backend?))

(provide make-env-credential-backend)

(define (provider->env-var provider-name)
  (define normalized (string-upcase (string-replace provider-name "-" "_")))
  (format "Q_~a_API_KEY" normalized))

(define (make-env-credential-backend)
  (credential-backend
   "env"
   ;; store! — read-only, raises error
   (λ (be provider-name api-key)
     (raise-credential-error "Environment backend is read-only" "env" "store-disabled"))
   ;; load
   (λ (be provider-name env-var)
     (define var (or env-var (provider->env-var provider-name)))
     (define val (getenv var))
     (if (and val (non-empty-string? val))
         (hasheq 'api-key val 'source "environment" 'provider provider-name)
         #f))
   ;; delete! — no-op
   (λ (be provider-name) (void))
   ;; list — not possible to enumerate env vars meaningfully
   (λ (be) '())
   ;; available?
   (λ (be) #t)))
