#lang racket/base

;; extensions/custom-renderer-registry.rkt — FEAT-70: Custom renderer registry
;;
;; Standalone registry for custom tool renderers. No dependency on tui/render.rkt
;; to avoid circular imports.

(provide custom-renderer
         custom-renderer?
         custom-renderer-tool-name
         custom-renderer-render-call
         custom-renderer-render-result
         ;; Registry
         register-custom-renderer!
         unregister-custom-renderer!
         lookup-custom-renderer
         lookup-custom-renderer-for-tool)

;; A custom renderer associates a tool name with render functions
(struct custom-renderer
        (tool-name ; string — tool name to match
         render-call ; (any -> (listof styled-line)) or #f
         render-result ; (any -> (listof styled-line)) or #f
         )
  #:transparent)

;; Global registry: box of (listof custom-renderer)
(define custom-renderer-registry (box '()))

;; register-custom-renderer! : custom-renderer? -> void?
(define (register-custom-renderer! renderer)
  (define existing (unbox custom-renderer-registry))
  (define filtered
    (filter (lambda (r)
              (not (equal? (custom-renderer-tool-name r) (custom-renderer-tool-name renderer))))
            existing))
  (set-box! custom-renderer-registry (cons renderer filtered)))

;; unregister-custom-renderer! : string? -> void?
(define (unregister-custom-renderer! tool-name)
  (define existing (unbox custom-renderer-registry))
  (set-box! custom-renderer-registry
            (filter (lambda (r) (not (equal? (custom-renderer-tool-name r) tool-name))) existing)))

;; lookup-custom-renderer : string? -> (or/c custom-renderer? #f)
(define (lookup-custom-renderer tool-name)
  (findf (lambda (r) (equal? (custom-renderer-tool-name r) tool-name))
         (unbox custom-renderer-registry)))

;; lookup-custom-renderer-for-tool : string? symbol? -> (or/c procedure? #f)
(define (lookup-custom-renderer-for-tool tool-name render-type)
  (define r (lookup-custom-renderer tool-name))
  (and r
       (case render-type
         [(call) (custom-renderer-render-call r)]
         [(result) (custom-renderer-render-result r)]
         [else #f])))
