#lang racket/base

;; runtime/credentials/chained-backend.rkt — Chained backend + policy-aware wrapper
;; Extracted from credential-backend.rkt (v0.73.5 A-4)

(require racket/contract
         racket/list
         racket/function
         "../../util/error/error-helpers.rkt"
         "../../util/error/errors.rkt"
         (only-in "protocol.rkt"
                  credential-backend
                  credential-backend?
                  credential-backend-name
                  backend-name
                  backend-store!
                  backend-load
                  backend-delete!
                  backend-list-providers
                  backend-available?
                  credential-policy?))

(provide make-chained-credential-backend
         make-policy-aware-backend)

;; ═══════════════════════════════════════════════════════════════════
;; Chained backend — tries multiple backends in order
;; ═══════════════════════════════════════════════════════════════════

(define (make-chained-credential-backend backends)
  (credential-backend
   "chained"
   ;; store! — writes to first writable backend
   (λ (be provider-name api-key)
     (define stored #f)
     (for ([sub (in-list backends)]
           #:break stored)
       (with-safe-fallback (void) (backend-store! sub provider-name api-key) (set! stored #t)))
     (unless stored
       (raise-credential-error "No writable backend available" "chained" "all-backends-readonly")))
   ;; load — tries each backend in order, returns first hit
   (λ (be provider-name env-var)
     (for/or ([sub (in-list backends)])
       (with-safe-fallback #f (backend-load sub provider-name #:env-var (or env-var #f)))))
   ;; delete! — deletes from all backends
   (λ (be provider-name)
     (for ([sub (in-list backends)])
       (with-safe-fallback (void) (backend-delete! sub provider-name))))
   ;; list — merges from all backends
   (λ (be)
     (remove-duplicates (apply append
                               (for/list ([sub (in-list backends)])
                                 (with-safe-fallback '() (backend-list-providers sub))))))
   ;; available? — true if any backend available
   (λ (be)
     (for/or ([sub (in-list backends)])
       (with-safe-fallback #f (backend-available? sub))))))

;; ═══════════════════════════════════════════════════════════════════
;; Policy-aware backend wrapper
;; ═══════════════════════════════════════════════════════════════════

(define (make-policy-aware-backend inner
                                   #:policy [policy 'auto]
                                   #:warn-port [warn-port (current-error-port)])
  (case policy
    [(auto) inner]
    [else
     (define (emit-warning msg)
       (when warn-port
         (fprintf warn-port "WARNING [credential-policy ~a]: ~a\n" policy msg)))
     (define (file-fallback-violation action)
       (raise-credential-error
        (format "~a forbidden by credential policy '~a' — file fallback not allowed" action policy)
        "policy"
        (format "~a-forbidden" action)))
     (credential-backend
      (format "policy-aware(~a, ~a)" policy (backend-name inner))
      ;; store!
      (λ (be provider-name api-key)
        (define inner-name (backend-name inner))
        (cond
          [(eq? policy 'keychain-required)
           (when (member inner-name '("file" "chained"))
             (file-fallback-violation "store"))
           (backend-store! inner provider-name api-key)]
          [(eq? policy 'env-only)
           (when (member inner-name '("file" "chained" "keychain"))
             (file-fallback-violation "store"))
           (backend-store! inner provider-name api-key)]
          [(eq? policy 'keychain-preferred)
           (when (member inner-name '("file"))
             (emit-warning
              (format "storing credential for '~a' in file backend — consider using keychain or env"
                      provider-name)))
           (backend-store! inner provider-name api-key)]
          [else (backend-store! inner provider-name api-key)]))
      ;; load
      (λ (be provider-name env-var)
        (define inner-name (backend-name inner))
        (cond
          [(eq? policy 'env-only)
           (if (member inner-name '("file" "keychain" "chained"))
               (begin
                 (emit-warning
                  (format "loading credential for '~a' from '~a' blocked by env-only policy"
                          provider-name
                          inner-name))
                 #f)
               (backend-load inner provider-name #:env-var (or env-var #f)))]
          [else
           (define result (backend-load inner provider-name #:env-var (or env-var #f)))
           (when (and result (eq? policy 'keychain-preferred) (member inner-name '("file")))
             (emit-warning
              (format "loaded credential for '~a' from file backend — consider using keychain"
                      provider-name)))
           result]))
      ;; delete!
      (λ (be provider-name) (backend-delete! inner provider-name))
      ;; list
      (λ (be) (backend-list-providers inner))
      ;; available?
      (λ (be) (backend-available? inner)))]))
