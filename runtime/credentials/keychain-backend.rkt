#lang racket/base

;; runtime/credentials/keychain-backend.rkt — Linux secret-tool keychain backend
;; Extracted from credential-backend.rkt (v0.73.5 A-4)

(require racket/contract
         racket/string
         racket/list
         "../../util/error/error-helpers.rkt"
         "../../util/error/errors.rkt"
         (only-in "protocol.rkt"
                  credential-backend
                  credential-backend?
                  current-external-command-runner))

(provide make-keychain-credential-backend)

(define (keychain-attrs provider-name)
  (list (cons "application" "q-agent") (cons "provider" provider-name)))

(define (keychain-label provider-name)
  (format "q-agent: ~a" provider-name))

(define (run-secret-tool args #:stdin [stdin-str #f])
  ((current-external-command-runner) (find-executable-path "secret-tool") args #:stdin stdin-str))

(define (secret-tool-available?)
  (with-safe-fallback #f (define-values (status _) (run-secret-tool '("--version"))) (= status 0)))

(define (make-keychain-credential-backend)
  (credential-backend "keychain"
                      ;; store!
                      (λ (be provider-name api-key) (keychain-store! provider-name api-key))
                      ;; load
                      (λ (be provider-name env-var) (keychain-load provider-name))
                      ;; delete!
                      (λ (be provider-name) (keychain-delete! provider-name))
                      ;; list
                      (λ (be) (keychain-list-providers))
                      ;; available?
                      (λ (be) (secret-tool-available?))))

(define (keychain-store! provider-name api-key)
  (unless (secret-tool-available?)
    (raise-credential-error "secret-tool not available" "keychain" "missing-secret-tool"))
  (define attrs (keychain-attrs provider-name))
  (define args
    (append* (list "store" "--label" (keychain-label provider-name))
             (for/list ([a (in-list attrs)])
               (list (format "--~a" (car a)) (cdr a)))))
  (define-values (status _) (run-secret-tool args #:stdin (string-append api-key "\n")))
  (unless (= status 0)
    (raise-credential-error (format "secret-tool store failed for ~a" provider-name)
                            "keychain"
                            "store-failed")))

(define (keychain-load provider-name)
  (unless (secret-tool-available?)
    #f)
  (define attrs (keychain-attrs provider-name))
  (define args
    (cons "lookup"
          (append* (for/list ([a (in-list attrs)])
                     (list (format "--~a" (car a)) (cdr a))))))
  (define-values (status out) (run-secret-tool args))
  (define result (string-trim out))
  (if (and (= status 0) (non-empty-string? result))
      (hasheq 'api-key result 'source "keychain" 'provider provider-name)
      #f))

(define (keychain-delete! provider-name)
  (unless (secret-tool-available?)
    (void))
  (define attrs (keychain-attrs provider-name))
  (define args
    (cons "clear"
          (append* (for/list ([a (in-list attrs)])
                     (list (format "--~a" (car a)) (cdr a))))))
  (run-secret-tool args)
  (void))

(define (keychain-list-providers)
  (unless (secret-tool-available?)
    '())
  (define-values (status out) (run-secret-tool '("search" "--all" "application" "q-agent")))
  (for/list ([line (in-list (string-split out "\n"))]
             #:when (string-contains? line "provider"))
    (define m (regexp-match #rx"provider *= *(.+)" line))
    (if m
        (string-trim (cadr m))
        "unknown")))
