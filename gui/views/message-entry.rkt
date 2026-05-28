#lang racket/base

;; q/gui/views/message-entry.rkt — Message entry view for GUI
;;
;; Renders a single message entry (user, assistant, system, tool).
;; Handles role-based styling, code block detection, and metadata display.

(require racket/contract
         racket/format
         racket/string
         "../../ui-core/theme-protocol.rkt")

(provide (contract-out [render-message-entry
                        (->* (ui-theme? hash?) (#:show-timestamps boolean? #:compact boolean?) hash?)]
                       [message-role-style (-> symbol? ui-theme? (listof (cons/c symbol? any/c)))]))

;; ──────────────────────────────
;; Role-based style mapping
;; ──────────────────────────────
(define (message-role-style role theme)
  (case role
    [(user) (list (cons 'fg (theme-ref theme 'foreground)) (cons 'label "You"))]
    [(assistant) (list (cons 'fg (theme-ref theme 'accent)) (cons 'label "Assistant"))]
    [(system) (list (cons 'fg (theme-ref theme 'warning)) (cons 'label "System"))]
    [(tool) (list (cons 'fg (theme-ref theme 'muted)) (cons 'label "Tool"))]
    [(error) (list (cons 'fg (theme-ref theme 'error)) (cons 'label "Error"))]
    [else (list (cons 'fg (theme-ref theme 'foreground)) (cons 'label "Unknown"))]))

;; ──────────────────────────────
;; Detect code blocks
;; ──────────────────────────────
(define (has-code-blocks? text)
  (and (string? text) (regexp-match? #rx"```" text)))

(define (count-code-blocks text)
  (length (regexp-match* #rx"```[a-z]*\n" text)))

;; ──────────────────────────────
;; Render message entry
;; ──────────────────────────────
(define (render-message-entry theme msg #:show-timestamps [show-timestamps #f] #:compact [compact #f])
  (define role (hash-ref msg 'role 'unknown))
  (define text (hash-ref msg 'text ""))
  (define style (message-role-style role theme))
  (define label (cdr (assoc 'label style)))
  (define fg (cdr (assoc 'fg style)))
  (hash 'view
        'message-entry
        'role
        role
        'text
        text
        'label
        label
        'fg
        fg
        'bg
        (theme-ref theme 'background)
        'has-code
        (has-code-blocks? text)
        'code-blocks
        (count-code-blocks text)
        'timestamp
        (and show-timestamps (hash-ref msg 'timestamp #f))
        'compact
        compact
        'tokens
        (hash-ref msg 'tokens #f)))
