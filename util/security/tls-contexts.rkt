#lang racket/base

;; util/security/tls-contexts.rkt — mTLS context creation wrappers
;; STABILITY: evolving
;;
;; W0 (v0.99.12): Provides safe, fail-loud wrappers around Racket's openssl
;; module for creating mutual-TLS (mTLS) server and client contexts.
;;
;; Both server and client contexts require:
;;   - A certificate chain PEM file (cert-path)
;;   - A private key PEM file (key-path)
;;   - A CA certificate PEM file (ca-path)
;;
;; Both contexts enable mandatory certificate verification (ssl-set-verify! #t),
;; which means:
;;   - Server context: client must present a valid cert signed by the CA
;;   - Client context: server must present a valid cert signed by the CA
;;
;; All paths are validated at context-creation time. Missing files raise
;; exn:fail with a clear, actionable error message.

(require racket/contract
         racket/file
         openssl)

;; ============================================================
;; Validation Helpers
;; ============================================================

(define (file-exists-or-fail! path description)
  (unless (file-exists? path)
    (raise (exn:fail (format "mTLS context error: ~a not found at '~a'" description path)
                     (current-continuation-marks))))
  path)

;; ============================================================
;; Server SSL Context
;; ============================================================

;; Create an mTLS server context with mandatory client-cert verification.
;; All three paths must point to valid PEM files.
(define (make-server-ssl-context #:cert-path cert-path #:key-path key-path #:ca-path ca-path)
  (file-exists-or-fail! cert-path "server certificate chain")
  (file-exists-or-fail! key-path "server private key")
  (file-exists-or-fail! ca-path "CA certificate")
  (define ctx (ssl-make-server-context 'tls12))
  (ssl-load-certificate-chain! ctx cert-path)
  (ssl-load-private-key! ctx key-path #f #f)
  (ssl-load-verify-root-certificates! ctx ca-path)
  (ssl-set-verify! ctx #t)
  ctx)

;; ============================================================
;; Client SSL Context
;; ============================================================

;; Create an mTLS client context with mandatory server-cert verification.
;; All three paths must point to valid PEM files.
(define (make-client-ssl-context #:cert-path cert-path #:key-path key-path #:ca-path ca-path)
  (file-exists-or-fail! cert-path "client certificate chain")
  (file-exists-or-fail! key-path "client private key")
  (file-exists-or-fail! ca-path "CA certificate")
  (define ctx (ssl-make-client-context 'tls12))
  (ssl-load-certificate-chain! ctx cert-path)
  (ssl-load-private-key! ctx key-path #f #f)
  (ssl-load-verify-root-certificates! ctx ca-path)
  (ssl-set-verify! ctx #t)
  ctx)

;; ============================================================
;; Provides
;; ============================================================

(provide (contract-out [make-server-ssl-context
                        (-> #:cert-path path-string?
                            #:key-path path-string?
                            #:ca-path path-string?
                            ssl-server-context?)]
                       [make-client-ssl-context
                        (-> #:cert-path path-string?
                            #:key-path path-string?
                            #:ca-path path-string?
                            ssl-client-context?)]))
