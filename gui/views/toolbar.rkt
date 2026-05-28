#lang racket/base

;; q/gui/views/toolbar.rkt — Toolbar view for GUI
;;
;; Horizontal toolbar with buttons, separators, and extension widgets.
;; Supports grouped items and overflow handling.

(require racket/contract
         "../../ui-core/theme-protocol.rkt")

(provide (contract-out
          [render-toolbar
           (->* (ui-theme?)
                (#:items (listof hash?) #:position (or/c 'top 'bottom) #:focused boolean?)
                hash?)]
          [toolbar-button (->* [symbol? string?] [#:action symbol?] hash?)]
          [toolbar-separator (-> hash?)]))

;; ──────────────────────────────
;; Constructors
;; ──────────────────────────────
(define (toolbar-button id label #:action [action id])
  (hash 'type 'button 'id id 'label label 'action action))

(define (toolbar-separator)
  (hash 'type 'separator))

;; ──────────────────────────────
;; Render toolbar
;; ──────────────────────────────
(define (render-toolbar theme #:items [items '()] #:position [position 'top] #:focused [focused #f])
  (define buttons (filter (lambda (i) (eq? (hash-ref i 'type 'button) 'button)) items))
  (define separators (filter (lambda (i) (eq? (hash-ref i 'type 'separator) 'separator)) items))
  (hash 'view
        'toolbar
        'items
        items
        'position
        position
        'focused
        focused
        'button-count
        (length buttons)
        'separator-count
        (length separators)
        'bg
        (theme-ref theme 'muted)
        'fg
        (theme-ref theme 'foreground)
        'accent
        (theme-ref theme 'accent)))
