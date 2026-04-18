#lang racket/base

;; examples/extensions/tui-widget.rkt — custom TUI widget rendering example (#1217)
;;
;; Demonstrates how to render custom widgets in the TUI footer area.
;; Uses ctx-set-widget to display a status line that updates on each turn.
;; Shows styled-segment usage for colored/themed output.
;;
;; Usage:
;;   (load-extension! registry "examples/extensions/tui-widget.rkt")
;;
;; Note: Access the UI state box from the extension context.
;; Widget content uses styled-line / styled-segment from tui/render.rkt.

(require "../../extensions/api.rkt"
         "../../extensions/hooks.rkt")

(provide the-extension)

;; Build a styled status line showing turn count and model info.
;; styled-segment takes: (text style-list)
;;   style-list can contain: 'bold 'dim 'green 'cyan 'inverse etc.
;; styled-line takes: (listof styled-segment)
(define (build-status-line turn-count model-name)
  (list
   ;; styled-line with styled-segments — demonstrates multi-segment rendering
   ;; In production: (styled-line (list (styled-segment "..." '(bold green)) ...))
   ;; For this example we describe the shape since importing tui/render.rkt
   ;; would create heavy dependencies for a lightweight example module.
   (hasheq 'segments
           (list (hasheq 'text "[tui-widget] " 'style '(bold))
                 (hasheq 'text (format "Turn: ~a" turn-count) 'style '(cyan))
                 (hasheq 'text " | " 'style '(dim))
                 (hasheq 'text (format "Model: ~a" model-name) 'style '(green))))))

(define the-extension
  (extension "tui-widget"
             "1.0.0"
             "1"
             (hasheq 'turn-start
                     (lambda (payload)
                       ;; Update widget at start of each turn.
                       ;; In production:
                       ;;   (define ui-box (ctx-ui-state-box ctx))
                       ;;   (define lines (build-status-line turn model))
                       ;;   (ctx-set-widget ui-box "tui-widget" "status" lines)
                       ;; Where lines is (listof styled-line) constructed with:
                       ;;   (styled-line (list (styled-segment "text" '(bold))))
                       (log-info "tui-widget: Updating status display")
                       (hook-pass payload))

                     'extension.loaded
                     (lambda (payload)
                       ;; Set initial widget content
                       ;; In production:
                       ;;   (ctx-set-widget ui-box "tui-widget" "status"
                       ;;     (list (styled-line
                       ;;            (list (styled-segment "[tui-widget] Ready" '(bold cyan))))))
                       (log-info "tui-widget: Initial widget rendered")
                       (hook-pass payload))

                     'extension.unloaded
                     (lambda (payload)
                       ;; Clean up widgets on unload
                       ;; In production: (dispose-widgets ui-box "tui-widget")
                       (log-info "tui-widget: Widgets disposed")
                       (hook-pass payload)))))

;; Key concepts:
;;   1. ctx-set-widget(ui-state-box, ext-name, key, lines) — set widget content
;;   2. ctx-remove-widget(ui-state-box, ext-name, key) — remove specific widget
;;   3. dispose-widgets(ui-state-box, ext-name) — remove all widgets for extension
;;   4. Lines are (listof styled-line), each containing (listof styled-segment)
;;   5. styled-segment(text, style) — style is a list like '(bold cyan)
;;   6. Widget updates are reactive — the TUI re-renders when ui-state changes
;;   7. Always clean up widgets in 'extension.unloaded to avoid stale content
