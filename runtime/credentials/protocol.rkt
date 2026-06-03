#lang racket/base

;; runtime/credentials/protocol.rkt — Credential backend protocol + shared infrastructure
;; Extracted from credential-backend.rkt (v0.73.5 A-4)

(require racket/contract
         racket/function
         racket/port
         racket/string)
(require "../../util/error/error-helpers.rkt"
         "../../util/error/errors.rkt")

(provide
 ;; Backend struct
 credential-backend
 credential-backend?
 credential-backend-name
 credential-backend-store-fn
 credential-backend-load-fn
 credential-backend-delete-fn
 credential-backend-list-fn
 credential-backend-available?-fn
 ;; Generic operations
 backend-name
 backend-store!
 backend-load
 backend-delete!
 backend-list-providers
 backend-available?
 ;; Command runner seams
 current-external-command-runner
 current-shell-command-runner
 ;; Credential policy
 credential-policy?
 valid-credential-policies
 ;; Helpers
 shell-escape)

;; ═══════════════════════════════════════════════════════════════════
;; Backend struct
;; ═══════════════════════════════════════════════════════════════════

(struct credential-backend
        (name ; string — human-readable name
         store-fn ; (backend provider-name api-key) → void?
         load-fn ; (backend provider-name env-var) → (or/c #f hash?)
         delete-fn ; (backend provider-name) → void?
         list-fn ; (backend) → (listof string?)
         available?-fn ; (backend) → boolean?
         )
  #:transparent)

;; ═══════════════════════════════════════════════════════════════════
;; Generic operations
;; ═══════════════════════════════════════════════════════════════════

(define (backend-name be)
  (credential-backend-name be))

(define (backend-store! be provider-name api-key)
  ((credential-backend-store-fn be) be provider-name api-key))

(define (backend-load be provider-name #:env-var [env-var #f])
  ((credential-backend-load-fn be) be provider-name env-var))

(define (backend-delete! be provider-name)
  ((credential-backend-delete-fn be) be provider-name))

(define (backend-list-providers be)
  ((credential-backend-list-fn be) be))

(define (backend-available? be)
  ((credential-backend-available?-fn be) be))

;; ═══════════════════════════════════════════════════════════════════
;; Command runner seams — injectable for testing
;; ═══════════════════════════════════════════════════════════════════

(define current-external-command-runner
  (make-parameter (λ (executable-path args #:stdin [stdin-str #f])
                    (with-safe-fallback (values 1 "")
                                        (define-values (sp out-port in-port err-port)
                                          (subprocess #f #f #f executable-path args))
                                        (when stdin-str
                                          (display stdin-str in-port)
                                          (close-output-port in-port))
                                        (define out (open-output-string))
                                        (copy-port out-port out)
                                        (close-input-port out-port)
                                        (close-input-port err-port)
                                        (values (subprocess-status sp) (get-output-string out))))))

(define current-shell-command-runner
  (make-parameter (λ (cmd-string output-port)
                    (with-safe-fallback
                     #f
                     (define-values (sp out-port in-port err-port)
                       (subprocess #f #f #f (find-executable-path "/bin/sh") "-c" cmd-string))
                     (copy-port out-port output-port)
                     (close-input-port out-port)
                     (close-input-port err-port)
                     (close-input-port in-port)
                     (= (subprocess-status sp) 0)))))

;; ═══════════════════════════════════════════════════════════════════
;; Credential policy
;; ═══════════════════════════════════════════════════════════════════

(define valid-credential-policies '(auto keychain-preferred keychain-required env-only))

(define (credential-policy? v)
  (and (symbol? v) (member v valid-credential-policies) #t))

;; ═══════════════════════════════════════════════════════════════════
;; Helpers
;; ═══════════════════════════════════════════════════════════════════

(define (shell-escape s)
  (string-replace s "'" "'\\''"))
