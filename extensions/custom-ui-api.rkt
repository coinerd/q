#lang racket/base

;; extensions/custom-ui-api.rkt — Custom Footer, Header, Tool Rendering (#717-#720)
;;
;; Provides:
;;   #717: ctx-set-footer, ctx-set-header for extension overrides
;;   #718: render-call/render-result support on tools
;;   #719: Custom tool rendering in transcript
;;   #720: Parent feature

(require "ui-surface.rkt"
         (only-in "custom-renderer-registry.rkt"
                  custom-renderer
                  custom-renderer?
                  custom-renderer-tool-name
                  custom-renderer-render-call
                  custom-renderer-render-result
                  lookup-custom-renderer))

;; #717: Header/footer API
(provide ctx-set-footer
         ctx-set-header
         ctx-clear-footer
         ctx-clear-header

         ;; #718-#719: Custom tool rendering
         render-tool-call
         render-tool-result

         ;; #720: Struct for registered custom renderer
         )

;; Re-export registry functions from custom-renderer-registry.rkt

;; ============================================================
;; #717: Header/footer
;; ============================================================

(define (ctx-set-footer ui-state-box lines)
  (ui-set-footer! ui-state-box lines))

(define (ctx-set-header ui-state-box lines)
  (ui-set-header! ui-state-box lines))

(define (ctx-clear-footer ui-state-box)
  (ui-clear-footer! ui-state-box))

(define (ctx-clear-header ui-state-box)
  (ui-clear-header! ui-state-box))

;; ============================================================
;; #718-#719: Custom tool rendering
;; ============================================================

;; Custom renderer struct is in custom-renderer-registry.rkt
;; (re-exported)

;; Render a tool call using custom renderer if available, else default.
(define (render-tool-call tool-name args-text custom-renderers)
  (define renderer
    (findf (lambda (r) (equal? (custom-renderer-tool-name r) tool-name)) custom-renderers))
  (if (and renderer (custom-renderer-render-call renderer))
      ((custom-renderer-render-call renderer) args-text)
      ;; Default rendering
      (list (ui-make-styled-line (list (ui-make-styled-segment (format "[TOOL: ~a] ~a" tool-name args-text) '()))))))

;; Render a tool result using custom renderer if available, else default.
(define (render-tool-result tool-name result custom-renderers)
  (define renderer
    (findf (lambda (r) (equal? (custom-renderer-tool-name r) tool-name)) custom-renderers))
  (if (and renderer (custom-renderer-render-result renderer))
      ((custom-renderer-render-result renderer) result)
      ;; Default rendering
      (list (ui-make-styled-line (list (ui-make-styled-segment (format "[OK: ~a]" tool-name) '()))))))

;; ============================================================
;; FEAT-70: Custom renderer registry
;; ============================================================

;; Global registry: box of (listof custom-renderer)
(define custom-renderer-registry (box '()))

;; register-custom-renderer! : custom-renderer? -> void?
(define (register-custom-renderer! renderer)
  (define existing (unbox custom-renderer-registry))
  ;; Remove any previous renderer for same tool
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
;; Look up a custom renderer for a tool name and type (:call or :result).
(define (lookup-custom-renderer-for-tool tool-name render-type)
  (define r (lookup-custom-renderer tool-name))
  (and r
       (case render-type
         [(call) (custom-renderer-render-call r)]
         [(result) (custom-renderer-render-result r)]
         [else #f])))
