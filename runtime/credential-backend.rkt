#lang racket/base

;; runtime/credential-backend.rkt — Credential backend abstraction layer (facade)
;;
;; Originally a 650-line god module (A-4 warning). Decomposed in v0.73.5 into:
;;   credentials/protocol.rkt      — struct + generic operations + shared parameters
;;   credentials/file-backend.rkt  — JSON file backend
;;   credentials/env-backend.rkt   — Environment variable backend
;;   credentials/memory-backend.rkt — In-memory backend (for testing)
;;   credentials/keychain-backend.rkt — Linux secret-tool backend
;;   credentials/platform-backends.rkt — macOS security + Windows cmdkey
;;   credentials/chained-backend.rkt — Chained backend + policy-aware wrapper
;;
;; This file re-exports everything for backward compatibility.

(require racket/contract
         "credentials/protocol.rkt"
         "credentials/file-backend.rkt"
         "credentials/env-backend.rkt"
         "credentials/memory-backend.rkt"
         "credentials/keychain-backend.rkt"
         "credentials/platform-backends.rkt"
         "credentials/chained-backend.rkt")

(provide
 ;; From protocol.rkt — struct + parameters (not in contract-out)
 current-external-command-runner
 current-shell-command-runner
 credential-backend
 credential-backend?
 credential-backend-name
 credential-backend-store-fn
 credential-backend-load-fn
 credential-backend-delete-fn
 credential-backend-list-fn
 credential-backend-available?-fn
 credential-policy?
 valid-credential-policies
 shell-escape
 ;; From platform-backends.rkt — not in contract-out
 credential-backend-capabilities
 ;; Contracted operations + constructors
 (contract-out
  [backend-name (-> credential-backend? string?)]
  [backend-store! (-> credential-backend? string? string? void?)]
  [backend-load
   (->* (credential-backend? string?) (#:env-var (or/c string? #f)) (or/c hash? #f))]
  [backend-delete! (-> credential-backend? string? void?)]
  [backend-list-providers (-> credential-backend? (listof string?))]
  [backend-available? (-> credential-backend? boolean?)]
  [make-policy-aware-backend
   (->* (credential-backend?)
        (#:policy (or/c 'auto 'keychain-preferred 'keychain-required 'env-only)
                  #:warn-port (or/c output-port? #f))
        credential-backend?)]
  [make-chained-credential-backend (-> (listof credential-backend?) credential-backend?)]
  [make-file-credential-backend (->* () ((or/c path-string? #f)) credential-backend?)]
  [make-env-credential-backend (-> credential-backend?)]
  [make-memory-credential-backend (-> credential-backend?)]
  [make-keychain-credential-backend (-> credential-backend?)]
  [make-macos-keychain-credential-backend (-> credential-backend?)]
  [make-windows-credential-backend (-> credential-backend?)]))
