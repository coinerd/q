#lang racket/base

;; q/gui/extension-slots/custom-renderer.rkt — Custom renderer registry for GUI
;;
;; Allows extensions to register custom renderers for specific
;; content types (code blocks, images, tables, etc.).

(require racket/contract)

(provide renderer-registry?
         (contract-out [make-renderer-registry (-> renderer-registry?)]
                       [register-renderer!
                        (-> renderer-registry? symbol? procedure? renderer-registry?)]
                       [unregister-renderer! (-> renderer-registry? symbol? renderer-registry?)]
                       [lookup-renderer (-> renderer-registry? symbol? (or/c procedure? #f))]
                       [render-with-registry (-> renderer-registry? symbol? any/c any)]))

;; ──────────────────────────────
;; Struct
;; ──────────────────────────────
(struct renderer-registry (renderers-box) #:transparent)

;; ──────────────────────────────
;; Constructor
;; ──────────────────────────────
(define (make-renderer-registry)
  (renderer-registry (box (hasheq))))

;; ──────────────────────────────
;; Register / unregister
;; ──────────────────────────────
(define (register-renderer! registry content-type renderer-fn)
  (define current (unbox (renderer-registry-renderers-box registry)))
  (set-box! (renderer-registry-renderers-box registry) (hash-set current content-type renderer-fn))
  registry)

(define (unregister-renderer! registry content-type)
  (define current (unbox (renderer-registry-renderers-box registry)))
  (set-box! (renderer-registry-renderers-box registry) (hash-remove current content-type))
  registry)

;; ──────────────────────────────
;; Lookup
;; ──────────────────────────────
(define (lookup-renderer registry content-type)
  (hash-ref (unbox (renderer-registry-renderers-box registry)) content-type #f))

;; ──────────────────────────────
;; Render with registry (falls back to default)
;; ──────────────────────────────
(define (render-with-registry registry content-type data)
  (define renderer (lookup-renderer registry content-type))
  (if renderer
      (renderer data)
      data)) ; default: pass through
