#lang racket/base

;; runtime/credentials/memory-backend.rkt — In-memory credential backend (for testing)
;; Extracted from credential-backend.rkt (v0.73.5 A-4)

(require racket/contract
         (only-in "protocol.rkt"
                  credential-backend
                  credential-backend?))

(provide make-memory-credential-backend)

(define (make-memory-credential-backend)
  (define store (make-hash))
  (credential-backend "memory"
                      ;; store!
                      (λ (be provider-name api-key) (hash-set! store provider-name api-key))
                      ;; load
                      (λ (be provider-name env-var)
                        (define val (hash-ref store provider-name #f))
                        (if val
                            (hasheq 'api-key val 'source "memory" 'provider provider-name)
                            #f))
                      ;; delete!
                      (λ (be provider-name) (hash-remove! store provider-name))
                      ;; list
                      (λ (be) (hash-keys store))
                      ;; available?
                      (λ (be) #t)))
