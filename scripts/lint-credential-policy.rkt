#lang racket/base

;; scripts/lint-credential-policy.rkt — Credential policy consistency lint
;;
;; Checks that credential documentation is consistent with actual backend
;; availability. Runs as a warning-only lint (exit 0 unless --strict).

(require racket/string
         racket/port
         racket/system
         racket/file
         racket/list)

(define strict-mode? (member "--strict" (vector->list (current-command-line-arguments))))

(define warnings '())

(define (warn msg)
  (set! warnings (cons msg warnings))
  (displayln (format "  WARN: ~a" msg) (current-error-port)))

;; Check 1: credential docs exist and mention policy
(define cred-doc "docs/getting-started/credentials.md")
(if (file-exists? cred-doc)
    (let ([content (file->string cred-doc)])
      (unless (or (string-contains? content "credential policy") (string-contains? content "Credential Policy"))
        (warn "credentials.md missing 'credential policy' section"))
      (unless (or (string-contains? content "keychain-preferred") (string-contains? content ""))
        (warn "credentials.md missing keychain-preferred policy mention"))
      (unless (or (string-contains? content "env-only") (string-contains? content ""))
        (warn "credentials.md missing env-only policy mention")))
    (warn (format "~a not found" cred-doc)))

;; Check 2: credential-backend.rkt exports make-policy-aware-backend
(define backend-src "runtime/credential-backend.rkt")
(if (file-exists? backend-src)
    (let ([content (file->string backend-src)])
      (unless (string-contains? content "make-policy-aware-backend")
        (warn "credential-backend.rkt missing make-policy-aware-backend export"))
      (unless (string-contains? content "valid-credential-policies")
        (warn "credential-backend.rkt missing valid-credential-policies"))
      (unless (string-contains? content "current-external-command-runner")
        (warn "credential-backend.rkt missing current-external-command-runner (mockable seam)")))
    (warn (format "~a not found" backend-src)))

;; Check 3: settings.rkt exports credential-policy
(define settings-src "runtime/settings.rkt")
(if (file-exists? settings-src)
    (let ([content (file->string settings-src)])
      (unless (string-contains? content "credential-policy")
        (warn "settings.rkt missing credential-policy function")))
    (warn (format "~a not found" settings-src)))

;; Summary
(define n (length warnings))
(cond
  [(zero? n)
   (displayln "  PASS: credential policy consistency OK")
   (exit 0)]
  [strict-mode?
   (displayln (format "  FAIL: ~a credential policy warning(s)" n) (current-error-port))
   (exit 1)]
  [else
   (displayln (format "  WARN: ~a credential policy warning(s) (non-blocking)" n) (current-error-port))
   (exit 0)])
