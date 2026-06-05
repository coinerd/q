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

(require racket/contract
         (only-in "ui-actions.rkt"
                  UI-ACTION-HEADER-SET
                  UI-ACTION-HEADER-CLEAR
                  UI-ACTION-FOOTER-SET
                  UI-ACTION-FOOTER-CLEAR
                  UI-ACTION-STATUS-SET
                  UI-ACTION-WIDGET-REGISTER
                  UI-ACTION-WIDGET-UNREGISTER
                  UI-ACTION-WIDGET-UNREGISTER-ALL
                  UI-ACTION-OVERLAY-SHOW
                  UI-ACTION-OVERLAY-DISMISS
                  UI-ACTION-THEME-CHANGE
                  UI-ACTION-LAYOUT-BREAKPOINT
                  UI-ACTION-FOCUS-REQUEST))

(provide (struct-out ui-delta)
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
         DELTA-SHOW-OVERLAY
         DELTA-DISMISS-OVERLAY

         ;; All delta types
         all-delta-types

         ;; Pure reducer with contract
         (contract-out [ui-action->deltas (-> string? hash? (listof ui-delta?))]))

;; ── Delta type constants ───────────────────────────────────

(define DELTA-SET-HEADER 'set-header)
(define DELTA-CLEAR-HEADER 'clear-header)
(define DELTA-SET-FOOTER 'set-footer)
(define DELTA-CLEAR-FOOTER 'clear-footer)
(define DELTA-SET-STATUS 'set-status)
(define DELTA-ADD-MESSAGE 'add-message)
(define DELTA-UPDATE-MESSAGE 'update-message)
(define DELTA-SET-THEME 'set-theme)
(define DELTA-SET-LAYOUT 'set-layout)
(define DELTA-SET-FOCUS 'set-focus)
(define DELTA-REGISTER-WIDGET 'register-widget)
(define DELTA-UNREGISTER-WIDGET 'unregister-widget)
(define DELTA-SHOW-OVERLAY 'show-overlay)
(define DELTA-DISMISS-OVERLAY 'dismiss-overlay)

(define all-delta-types
  (list DELTA-SET-HEADER
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
        DELTA-SHOW-OVERLAY
        DELTA-DISMISS-OVERLAY))

;; ── Delta struct ───────────────────────────────────────────

(struct ui-delta (type payload) #:transparent)

(define (ui-delta-type? sym)
  (and (symbol? sym) (member sym all-delta-types) #t))

;; ── Pure reducer: action event → list of ui-delta ──────────
;; Maps UI action names (from ui-actions.rkt) to ui-delta values.
;; Pure function: no side effects, no IO.

(define (ui-action->deltas action-type payload)
  (cond
    [(equal? action-type UI-ACTION-HEADER-SET)
     (list (ui-delta DELTA-SET-HEADER (hash-ref payload 'lines '())))]
    [(equal? action-type UI-ACTION-HEADER-CLEAR) (list (ui-delta DELTA-CLEAR-HEADER #f))]
    [(equal? action-type UI-ACTION-FOOTER-SET)
     (list (ui-delta DELTA-SET-FOOTER (hash-ref payload 'lines '())))]
    [(equal? action-type UI-ACTION-FOOTER-CLEAR) (list (ui-delta DELTA-CLEAR-FOOTER #f))]
    [(equal? action-type UI-ACTION-STATUS-SET)
     (list (ui-delta DELTA-SET-STATUS (hash-ref payload 'status 'idle)))]
    [(equal? action-type UI-ACTION-WIDGET-REGISTER)
     (list (ui-delta DELTA-REGISTER-WIDGET
                     (list (hash-ref payload 'ext-name #f)
                           (hash-ref payload 'key #f)
                           (hash-ref payload 'descriptor (hash)))))]
    [(equal? action-type UI-ACTION-WIDGET-UNREGISTER)
     (list (ui-delta DELTA-UNREGISTER-WIDGET
                     (cons (hash-ref payload 'ext-name #f) (hash-ref payload 'key #f))))]
    [(equal? action-type UI-ACTION-WIDGET-UNREGISTER-ALL)
     (list (ui-delta DELTA-UNREGISTER-WIDGET (cons (hash-ref payload 'ext-name #f) #f)))]
    [(equal? action-type UI-ACTION-THEME-CHANGE)
     (list (ui-delta DELTA-SET-THEME (hash-ref payload 'theme #f)))]
    [(equal? action-type UI-ACTION-LAYOUT-BREAKPOINT)
     (list (ui-delta DELTA-SET-LAYOUT (hash-ref payload 'breakpoint 'standard)))]
    [(equal? action-type UI-ACTION-FOCUS-REQUEST)
     (list (ui-delta DELTA-SET-FOCUS (hash-ref payload 'component 'input)))]
    [(equal? action-type UI-ACTION-OVERLAY-SHOW)
     (list (ui-delta DELTA-SHOW-OVERLAY (hash-ref payload 'content (hash))))]
    [(equal? action-type UI-ACTION-OVERLAY-DISMISS) (list (ui-delta DELTA-DISMISS-OVERLAY #f))]
    [else '()]))
