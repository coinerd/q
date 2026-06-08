#lang racket/base
;; STABILITY: public

;; extensions/api.rkt — extension registry with registration-order dispatch
;;
;; Provides:
;;   - extension struct (name, version, api-version, hooks) [from util/extensions.rkt]
;;   - extension-registry (thread-safe, insertion-ordered)
;;   - register-extension!, unregister-extension!, lookup-extension
;;   - list-extensions, handlers-for-point
;;   - injection-event-topic [re-export for backward compat]

(require racket/contract
         racket/list
         ;; v0.33.0 W1: extension struct moved to foundation layer
         (only-in "../util/extension/extensions.rkt"
                  extension
                  extension?
                  extension-name
                  extension-version
                  extension-api-version
                  extension-hooks)
         ;; ARCH-04 (v0.22.0): event bus re-exports for extensions
         ;; NOTE (W12 v0.72.5): Event bus is a foundational utility (util-layer). Despite living
;; in agent/, it has no upward dependencies and is safe to import from extensions/.
;; Moving it to util/ would break 20+ import paths for minimal architectural benefit.
         (only-in "../agent/event-bus.rkt" publish! subscribe! unsubscribe! make-event-bus event-bus?)
         ;; Backward-compat: re-export injection-event-topic
         (rename-in "../util/event/event-types.rkt" [injection-event-topic api-injection-event-topic]))

;; JSON Schema predicate (for extension tool schemas)
(define (json-schema? v)
  (and (hash? v)
       (or (hash-has-key? v 'type) (hash-has-key? v 'properties) (hash-has-key? v 'schema))))

;; ============================================================
;; Extension event topics (backward-compat re-exports)
;; ============================================================

(define injection-event-topic api-injection-event-topic)

;; Event bus re-exports (ARCH-04)
(provide publish!
         subscribe!
         unsubscribe!
         make-event-bus
         event-bus?

         ;; Extension struct and registry
         json-schema?
         injection-event-topic
         extension
         extension?
         extension-name
         extension-version
         extension-api-version
         extension-hooks
         extension-registry?
         (contract-out [make-extension-registry (-> extension-registry?)]
                       [register-extension! (-> extension-registry? extension? void?)]
                       [unregister-extension! (-> extension-registry? string? void?)]
                       [lookup-extension (-> extension-registry? string? (or/c #f extension?))]
                       [list-extensions (-> extension-registry? (listof extension?))]
                       [handlers-for-point (-> extension-registry? symbol? (listof pair?))]))

;; ============================================================
;; Extension registry (thread-safe, insertion-ordered)
;; ============================================================

;; M-04: Named struct replacing raw (cons list hash) pair.
(struct ext-registry-data (list index) #:transparent)

(struct extension-registry (data-box semaphore) #:constructor-name make-extension-registry-internal)

;;; make-extension-registry : -> extension-registry?
(define (make-extension-registry)
  (make-extension-registry-internal (box (ext-registry-data '() (hasheq))) (make-semaphore 1)))

;;; register-extension! : extension-registry? extension? -> void?
(define (register-extension! registry ext)
  (call-with-semaphore
   (extension-registry-semaphore registry)
   (lambda ()
     (define data (unbox (extension-registry-data-box registry)))
     (define old-list (ext-registry-data-list data))
     (define old-hash (ext-registry-data-index data))
     (define filtered-list
       (filter (lambda (e) (not (equal? (extension-name e) (extension-name ext)))) old-list))
     (set-box! (extension-registry-data-box registry)
               (ext-registry-data (append filtered-list (list ext))
                                  (hash-set old-hash (extension-name ext) ext))))))

;;; unregister-extension! : extension-registry? string? -> void?
(define (unregister-extension! registry name)
  (call-with-semaphore
   (extension-registry-semaphore registry)
   (lambda ()
     (define data (unbox (extension-registry-data-box registry)))
     (define old-list (ext-registry-data-list data))
     (define old-hash (ext-registry-data-index data))
     (set-box! (extension-registry-data-box registry)
               (ext-registry-data (filter (lambda (e) (not (equal? (extension-name e) name)))
                                          old-list)
                                  (hash-remove old-hash name))))))

;;; lookup-extension : extension-registry? string? -> (or/c #f extension?)
(define (lookup-extension registry name)
  (call-with-semaphore (extension-registry-semaphore registry)
                       (lambda ()
                         (define data (unbox (extension-registry-data-box registry)))
                         (hash-ref (ext-registry-data-index data) name #f))))

;;; list-extensions : extension-registry? -> (listof extension?)
(define (list-extensions registry)
  (call-with-semaphore (extension-registry-semaphore registry)
                       (lambda ()
                         (ext-registry-data-list (unbox (extension-registry-data-box registry))))))

;;; handlers-for-point : extension-registry? symbol? -> (listof pair?)
(define (handlers-for-point registry hook-point)
  (define exts (list-extensions registry))
  (for*/list ([ext exts]
              #:when (hash-has-key? (extension-hooks ext) hook-point))
    (cons (extension-name ext) (hash-ref (extension-hooks ext) hook-point))))
