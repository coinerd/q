#lang racket/base

;; q/gui/views/overlay.rkt — Overlay/dialog views for GUI
;;
;; Provides overlay views (modals, confirmation dialogs, alerts,
;; prompts) that render on top of the main application.

(require racket/contract
         "../../ui-core/theme-protocol.rkt")

(provide (contract-out [render-alert
                        (->* (ui-theme? string?)
                             (#:title (or/c string? #f)
                                      #:severity (or/c 'info 'warning 'error 'success))
                             hash?)]
                       [render-confirm
                        (->* (ui-theme? string?)
                             (#:title (or/c string? #f) #:confirm-text string? #:cancel-text string?)
                             hash?)]
                       [render-prompt
                        (->* (ui-theme? string?)
                             (#:title (or/c string? #f)
                                      #:default-value (or/c string? #f)
                                      #:placeholder (or/c string? #f))
                             hash?)]))

;; ──────────────────────────────
;; Severity → color mapping
;; ──────────────────────────────
(define (severity-color severity theme)
  (case severity
    [(error) (theme-ref theme 'error)]
    [(warning) (theme-ref theme 'warning)]
    [(success) (theme-ref theme 'success)]
    [else (theme-ref theme 'accent)]))

;; ──────────────────────────────
;; Alert
;; ──────────────────────────────
(define (render-alert theme message #:title [title "Alert"] #:severity [severity 'info])
  (hash 'view
        'alert
        'type
        'alert
        'title
        title
        'message
        message
        'severity
        severity
        'color
        (severity-color severity theme)
        'bg
        (theme-ref theme 'background)
        'fg
        (theme-ref theme 'foreground)))

;; ──────────────────────────────
;; Confirm
;; ──────────────────────────────
(define (render-confirm theme
                        message
                        #:title [title "Confirm"]
                        #:confirm-text [confirm-text "OK"]
                        #:cancel-text [cancel-text "Cancel"])
  (hash 'view
        'confirm
        'type
        'confirm
        'title
        title
        'message
        message
        'confirm-text
        confirm-text
        'cancel-text
        cancel-text
        'color
        (severity-color 'warning theme)
        'bg
        (theme-ref theme 'background)
        'fg
        (theme-ref theme 'foreground)))

;; ──────────────────────────────
;; Prompt
;; ──────────────────────────────
(define (render-prompt theme
                       message
                       #:title [title "Input"]
                       #:default-value [default-value #f]
                       #:placeholder [placeholder "Type here..."])
  (hash 'view
        'prompt
        'type
        'prompt
        'title
        title
        'message
        message
        'default-value
        default-value
        'placeholder
        placeholder
        'color
        (theme-ref theme 'accent)
        'bg
        (theme-ref theme 'background)
        'fg
        (theme-ref theme 'foreground)))
