#lang racket/base

;; extensions/custom-ui-api.rkt — Custom Footer, Header, Tool Rendering (#717-#720)
;;
;; Provides:
;;   #717: ctx-set-footer, ctx-set-header for extension overrides
;;   #718: render-call/render-result support on tools
;;   #719: Custom tool rendering in transcript
;;   #720: Parent feature

(require "../tui/state.rkt"
         "../tui/render.rkt")

(provide
 ;; #717: Header/footer API
 ctx-set-footer
 ctx-set-header
 ctx-clear-footer
 ctx-clear-header

 ;; #718-#719: Custom tool rendering
 render-tool-call
 render-tool-result

 ;; #720: Struct for registered custom renderer
 (struct-out custom-renderer))

;; ============================================================
;; #717: Header/footer
;; ============================================================

(define (ctx-set-footer ui-state-box lines)
  (define state (unbox ui-state-box))
  (set-box! ui-state-box (set-custom-footer state lines)))

(define (ctx-set-header ui-state-box lines)
  (define state (unbox ui-state-box))
  (set-box! ui-state-box (set-custom-header state lines)))

(define (ctx-clear-footer ui-state-box)
  (define state (unbox ui-state-box))
  (set-box! ui-state-box (clear-custom-footer state)))

(define (ctx-clear-header ui-state-box)
  (define state (unbox ui-state-box))
  (set-box! ui-state-box (clear-custom-header state)))

;; ============================================================
;; #718-#719: Custom tool rendering
;; ============================================================

;; A custom renderer associates a tool name with render functions
(struct custom-renderer
  (tool-name        ; string — tool name to match
   render-call      ; (hash? -> (listof styled-line)) or #f
   render-result    ; (any -> (listof styled-line)) or #f
   )
  #:transparent)

;; Render a tool call using custom renderer if available, else default.
(define (render-tool-call tool-name args-text custom-renderers)
  (define renderer (findf (lambda (r) (equal? (custom-renderer-tool-name r) tool-name))
                          custom-renderers))
  (if (and renderer (custom-renderer-render-call renderer))
      ((custom-renderer-render-call renderer) args-text)
      ;; Default rendering
      (list (styled-line (list (styled-segment (format "[TOOL: ~a] ~a" tool-name args-text)
                                                '()))))))

;; Render a tool result using custom renderer if available, else default.
(define (render-tool-result tool-name result custom-renderers)
  (define renderer (findf (lambda (r) (equal? (custom-renderer-tool-name r) tool-name))
                          custom-renderers))
  (if (and renderer (custom-renderer-render-result renderer))
      ((custom-renderer-render-result renderer) result)
      ;; Default rendering
      (list (styled-line (list (styled-segment (format "[OK: ~a]" tool-name) '()))))))
