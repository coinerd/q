#lang racket/base

;; extensions/widget-api.rkt — Extension Widget API (#714, #715)
;;
;; Provides:
;;   ctx-set-widget     — set widget content from extension
;;   ctx-remove-widget  — remove a widget
;;   dispose-widgets    — remove all widgets for an extension (on unload)
;;   apply-widget-event — apply widget event to ui-state

(require racket/contract
         racket/match
         "../agent/event-bus.rkt"
         "../util/protocol-types.rkt"
         "ui-surface.rkt")

;; Widget API — uses ui-state box for state updates
(provide ctx-set-widget
         ctx-remove-widget
         dispose-widgets
         ;; For testing: apply widget event to state
         apply-widget-event)

;; Apply a widget event to ui-state.
;; This is called by the event handler in the TUI render loop.
(define (apply-widget-event state evt)
  (define payload (event-payload evt))
  (define action (hash-ref payload 'action 'set))
  (define ext-name (hash-ref payload 'extension ""))
  (define key (hash-ref payload 'key ""))
  (case action
    [(set)
     (define lines (hash-ref payload 'lines '()))
     (ui-set-extension-widget! state ext-name key lines)]
    [(remove) (ui-remove-extension-widget! state ext-name key)]
    [(dispose) (ui-remove-all-extension-widgets! state ext-name)]
    [else state]))

;; Set a widget from an extension.
;; ui-state-box: box containing ui-state
;; ext-name: extension name
;; key: widget key within the extension
;; lines: (listof styled-line) to display
(define (ctx-set-widget ui-state-box ext-name key lines)
  (define state (unbox ui-state-box))
  (set-box! ui-state-box (ui-set-extension-widget! state ext-name key lines)))

;; Remove a specific widget.
(define (ctx-remove-widget ui-state-box ext-name key)
  (define state (unbox ui-state-box))
  (set-box! ui-state-box (ui-remove-extension-widget! state ext-name key)))

;; Dispose all widgets for an extension (called on unload).
(define (dispose-widgets ui-state-box ext-name)
  (define state (unbox ui-state-box))
  (set-box! ui-state-box (ui-remove-all-extension-widgets! state ext-name)))
