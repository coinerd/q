#lang racket/base

;; q/gui/views/status.rkt — Status bar view for GUI
;;
;; Renders a status bar showing model name, session status,
;; token count, and turn number. Produces view descriptors
;; consumed by the concrete renderer.

(require racket/contract
         racket/format
         "../../ui-core/theme-protocol.rkt")

(provide (contract-out [render-status-bar
                        (->* (ui-theme?)
                             (#:model (or/c string? #f)
                                      #:status (or/c symbol? string? #f)
                                      #:turn exact-nonnegative-integer?
                                      #:tokens (or/c exact-nonnegative-integer? #f)
                                      #:width exact-nonnegative-integer?)
                             hash?)]
                       [status-text (-> (or/c symbol? string?) string?)]))

;; ──────────────────────────────
;; Status symbol → display text
;; ──────────────────────────────
(define (status-text status)
  (case status
    [(idle ready) "Ready"]
    [(processing thinking streaming) "Processing..."]
    [(waiting input) "Waiting for input"]
    [(error) "Error"]
    [(cancelled) "Cancelled"]
    [else (if (string? status) status "Ready")]))

;; ──────────────────────────────
;; Render status bar
;; ──────────────────────────────
(define (render-status-bar theme
                           #:model [model #f]
                           #:status [status 'idle]
                           #:turn [turn 0]
                           #:tokens [tokens #f]
                           #:width [width 120])
  (define left-section
    (format "~a | ~a" (or model "q") (status-text (if (symbol? status) status 'idle))))
  (define right-section
    (format "Turn: ~a~a"
            turn
            (if tokens
                (format " | Tokens: ~a" tokens)
                "")))
  (hash 'view
        'status-bar
        'left
        left-section
        'right
        right-section
        'status
        (if (symbol? status) status 'idle)
        'model
        model
        'width
        width
        'bg
        (theme-ref theme 'muted)
        'fg
        (theme-ref theme 'foreground)
        'accent
        (theme-ref theme 'accent)))
