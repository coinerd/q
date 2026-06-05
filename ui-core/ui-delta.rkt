#lang racket

;; q/ui-core/ui-delta.rkt — UI delta struct for backend-neutral state changes
;;
;; Defines the ui-delta struct that represents a state change instruction.
;; Backend adapters (TUI, GUI) consume ui-delta values to update their
;; backend-specific state.
;;
;; Architecture:
;;   UI action → reducer → ui-delta → backend adapter → backend state
;;
;; W1.1 (v0.94.1): Delta types covering header, footer, status, widget,
;; theme, layout, focus, overlay.

(require racket/contract)

(provide
 (struct-out ui-delta)
 ui-delta-type?

 ;; Delta type constants
 DELTA-SET-HEADER
 DELTA-CLEAR-HEADER
 DELTA-SET-FOOTER
 DELTA-CLEAR-FOOTER
 DELTA-SET-STATUS
 DELTA-ADD-MESSAGE
 DELTA-UPDATE-MESSAGE
 DELTA-SET-THEME
 DELTA-SET-LAYOUT
 DELTA-SET-FOCUS
 DELTA-REGISTER-WIDGET
 DELTA-UNREGISTER-WIDGET

 ;; All delta types
 all-delta-types

 ;; Pure reducer
 ui-action->deltas)

;; ── Delta type constants ───────────────────────────────────

(define DELTA-SET-HEADER      'set-header)
(define DELTA-CLEAR-HEADER    'clear-header)
(define DELTA-SET-FOOTER      'set-footer)
(define DELTA-CLEAR-FOOTER    'clear-footer)
(define DELTA-SET-STATUS      'set-status)
(define DELTA-ADD-MESSAGE     'add-message)
(define DELTA-UPDATE-MESSAGE  'update-message)
(define DELTA-SET-THEME       'set-theme)
(define DELTA-SET-LAYOUT      'set-layout)
(define DELTA-SET-FOCUS       'set-focus)
(define DELTA-REGISTER-WIDGET 'register-widget)
(define DELTA-UNREGISTER-WIDGET 'unregister-widget)

(define all-delta-types
  (list DELTA-SET-HEADER DELTA-CLEAR-HEADER
        DELTA-SET-FOOTER DELTA-CLEAR-FOOTER
        DELTA-SET-STATUS DELTA-ADD-MESSAGE DELTA-UPDATE-MESSAGE
        DELTA-SET-THEME DELTA-SET-LAYOUT DELTA-SET-FOCUS
        DELTA-REGISTER-WIDGET DELTA-UNREGISTER-WIDGET))

;; ── Delta struct ───────────────────────────────────────────

(struct ui-delta (type payload) #:transparent)

(define (ui-delta-type? sym)
  (and (symbol? sym)
       (member sym all-delta-types)
       #t))

;; ── Pure reducer: action event → list of ui-delta ──────────
;; Maps UI action names (from ui-actions.rkt) to ui-delta values.
;; Pure function: no side effects, no IO.

(define (ui-action->deltas action-type payload)
  (cond
    [(equal? action-type "ui.header.set")
     (list (ui-delta DELTA-SET-HEADER (hash-ref payload 'lines '())))]
    [(equal? action-type "ui.header.clear")
     (list (ui-delta DELTA-CLEAR-HEADER #f))]
    [(equal? action-type "ui.footer.set")
     (list (ui-delta DELTA-SET-FOOTER (hash-ref payload 'lines '())))]
    [(equal? action-type "ui.footer.clear")
     (list (ui-delta DELTA-CLEAR-FOOTER #f))]
    [(equal? action-type "ui.status.set")
     (list (ui-delta DELTA-SET-STATUS (hash-ref payload 'status 'idle)))]
    [(equal? action-type "ui.widget.register")
     (list (ui-delta DELTA-REGISTER-WIDGET
                     (list (hash-ref payload 'ext-name #f)
                           (hash-ref payload 'key #f)
                           (hash-ref payload 'descriptor (hash)))))]
    [(equal? action-type "ui.widget.unregister")
     (list (ui-delta DELTA-UNREGISTER-WIDGET
                     (cons (hash-ref payload 'ext-name #f)
                           (hash-ref payload 'key #f))))]
    [(equal? action-type "ui.widget.unregister-all")
     (list (ui-delta DELTA-UNREGISTER-WIDGET
                     (cons (hash-ref payload 'ext-name #f) #f)))]
    [(equal? action-type "ui.theme.change")
     (list (ui-delta DELTA-SET-THEME (hash-ref payload 'theme #f)))]
    [(equal? action-type "ui.layout.breakpoint")
     (list (ui-delta DELTA-SET-LAYOUT (hash-ref payload 'breakpoint 'standard)))]
    [(equal? action-type "ui.focus.request")
     (list (ui-delta DELTA-SET-FOCUS (hash-ref payload 'component 'input)))]
    [else '()]))
