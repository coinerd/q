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
                  register-custom-renderer!
                  unregister-custom-renderer!
                  lookup-custom-renderer
                  lookup-custom-renderer-for-tool))

;; #717: Header/footer API
(provide ctx-set-footer
         ctx-set-header
         ctx-clear-footer
         ctx-clear-header

         ;; #718-#719: Custom tool rendering
         render-tool-call
         render-tool-result

         ;; #720: Struct for registered custom renderer
         register-custom-renderer!
         unregister-custom-renderer!
         lookup-custom-renderer-for-tool)

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
      (list (ui-make-styled-line
             (list (ui-make-styled-segment (format "[TOOL: ~a] ~a" tool-name args-text) '()))))))

;; Render a tool result using custom renderer if available, else default.
(define (render-tool-result tool-name result custom-renderers)
  (define renderer
    (findf (lambda (r) (equal? (custom-renderer-tool-name r) tool-name)) custom-renderers))
  (if (and renderer (custom-renderer-render-result renderer))
      ((custom-renderer-render-result renderer) result)
      ;; Default rendering
      (list (ui-make-styled-line (list (ui-make-styled-segment (format "[OK: ~a]" tool-name) '()))))))

;; ============================================================
;; FEAT-70: Custom renderer registry — imported from custom-renderer-registry.rkt
;; ============================================================
