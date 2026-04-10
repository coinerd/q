#lang racket/base

;; extensions/api.rkt — extension registry with registration-order dispatch
;;
;; Provides:
;;   - extension struct (name, version, api-version, hooks)
;;   - extension-registry (thread-safe, insertion-ordered)
;;   - register-extension!, unregister-extension!, lookup-extension
;;   - list-extensions, handlers-for-point

(require racket/contract
         racket/list)

(provide
 (struct-out extension)
 extension-registry?
 (contract-out
  [make-extension-registry (-> extension-registry?)]
  [register-extension!     (-> extension-registry? extension? void?)]
  [unregister-extension!   (-> extension-registry? string? void?)]
  [lookup-extension        (-> extension-registry? string? (or/c #f extension?))]
  [list-extensions         (-> extension-registry? (listof extension?))]
  [handlers-for-point      (-> extension-registry? symbol? (listof pair?))]))

;; ============================================================
;; Extension struct
;; ============================================================

(struct extension (name version api-version hooks) #:transparent)
;; name       : string?
;; version    : string?
;; api-version: string?
;; hooks      : (hash/c symbol? procedure?)

;; ============================================================
;; Extension registry (thread-safe, insertion-ordered)
;; ============================================================
;;
;; Uses two parallel structures:
;;   - hash for O(1) lookup by name
;;   - list for ordered iteration (insertion order)
;; Both are stored in a box and protected by a semaphore.

(struct extension-registry (data-box semaphore)
  #:constructor-name make-extension-registry-internal)

;; Internal: data is (cons ordered-list hash)
;;   ordered-list : (listof extension?) in registration order
;;   hash         : (hash/c string? extension?) for O(1) lookup

(define (make-extension-registry)
  (make-extension-registry-internal (box (cons '() (hasheq))) (make-semaphore 1)))

;; ============================================================
;; register-extension! : extension-registry? extension? -> void?
;; ============================================================

(define (register-extension! registry ext)
  (call-with-semaphore (extension-registry-semaphore registry)
    (λ ()
      (define data (unbox (extension-registry-data-box registry)))
      (define old-list (car data))
      (define old-hash (cdr data))
      ;; Remove old entry if same name exists (overwrite semantics)
      (define filtered-list
        (filter (λ (e) (not (equal? (extension-name e) (extension-name ext))))
                old-list))
      ;; Append new extension at end (preserves insertion order)
      (set-box! (extension-registry-data-box registry)
                (cons (append filtered-list (list ext))
                      (hash-set old-hash (extension-name ext) ext))))))

;; ============================================================
;; unregister-extension! : extension-registry? string? -> void?
;; ============================================================

(define (unregister-extension! registry name)
  (call-with-semaphore (extension-registry-semaphore registry)
    (λ ()
      (define data (unbox (extension-registry-data-box registry)))
      (define old-list (car data))
      (define old-hash (cdr data))
      (set-box! (extension-registry-data-box registry)
                (cons (filter (λ (e) (not (equal? (extension-name e) name)))
                              old-list)
                      (hash-remove old-hash name))))))

;; ============================================================
;; lookup-extension : extension-registry? string? -> (or/c extension? #f)
;; ============================================================

(define (lookup-extension registry name)
  (define data (unbox (extension-registry-data-box registry)))
  (hash-ref (cdr data) name #f))

;; ============================================================
;; list-extensions : extension-registry? -> (listof extension?)
;; ============================================================

(define (list-extensions registry)
  (car (unbox (extension-registry-data-box registry))))

;; ============================================================
;; handlers-for-point : extension-registry? symbol? -> (listof (cons/c string? procedure?))
;; Returns list of (extension-name . handler) for the given hook point,
;; in registration order.
;; ============================================================

(define (handlers-for-point registry hook-point)
  (define exts (list-extensions registry))
  (for*/list ([ext exts]
              #:when (hash-has-key? (extension-hooks ext) hook-point))
    (cons (extension-name ext)
          (hash-ref (extension-hooks ext) hook-point))))
