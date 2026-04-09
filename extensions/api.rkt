#lang racket

;; extensions/api.rkt — stable extension registration API
;;
;; Provides:
;;   - extension struct (name, version, api-version, hooks)
;;   - extension-registry: mutable registry for extensions
;;   - register-extension!, unregister-extension!, lookup-extension
;;   - list-extensions, handlers-for-point

(require racket/contract)

(provide
 (struct-out extension)
 make-extension-registry
 extension-registry?
 register-extension!
 unregister-extension!
 lookup-extension
 list-extensions
 handlers-for-point)

;; ============================================================
;; Extension struct
;; ============================================================

(struct extension (name version api-version hooks) #:transparent)
;; name       : string?
;; version    : string?
;; api-version: string?
;; hooks      : (hash/c symbol? procedure?)

;; ============================================================
;; Extension registry (thread-safe)
;; ============================================================

(struct extension-registry (extensions-box semaphore)
  #:constructor-name make-extension-registry-internal)

(define (make-extension-registry)
  (make-extension-registry-internal (box (hasheq)) (make-semaphore 1)))

;; ============================================================
;; register-extension! : extension-registry? extension? -> void?
;; ============================================================

(define (register-extension! registry ext)
  (call-with-semaphore (extension-registry-semaphore registry)
    (λ ()
      (set-box! (extension-registry-extensions-box registry)
                (hash-set (unbox (extension-registry-extensions-box registry))
                          (extension-name ext)
                          ext)))))

;; ============================================================
;; unregister-extension! : extension-registry? string? -> void?
;; ============================================================

(define (unregister-extension! registry name)
  (call-with-semaphore (extension-registry-semaphore registry)
    (λ ()
      (set-box! (extension-registry-extensions-box registry)
                (hash-remove (unbox (extension-registry-extensions-box registry))
                             name)))))

;; ============================================================
;; lookup-extension : extension-registry? string? -> (or/c extension? #f)
;; ============================================================

(define (lookup-extension registry name)
  (hash-ref (unbox (extension-registry-extensions-box registry)) name #f))

;; ============================================================
;; list-extensions : extension-registry? -> (listof extension?)
;; ============================================================

(define (list-extensions registry)
  (hash-values (unbox (extension-registry-extensions-box registry))))

;; ============================================================
;; handlers-for-point : extension-registry? symbol? -> (listof (cons/c string? procedure?))
;; Returns list of (extension-name . handler) for the given hook point,
;; in registration order.
;; ============================================================

(define (handlers-for-point registry hook-point)
  (define exts (unbox (extension-registry-extensions-box registry)))
  ;; Preserve registration order: hash-keys returns keys in insertion order
  ;; for mutable hashes. Filter to only extensions with this hook point.
  (for*/list ([name (hash-keys exts)]
              [ext (in-value (hash-ref exts name))]
              #:when (hash-has-key? (extension-hooks ext) hook-point))
    (cons name (hash-ref (extension-hooks ext) hook-point))))
