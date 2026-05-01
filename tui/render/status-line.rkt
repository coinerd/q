#lang racket/base

;; q/tui/render/status-line.rkt — status bar and input line rendering
;;
;; Pure functions that compose status bar and input line.

(require racket/string
         "../state.rkt"
         "../input.rkt"
         "../char-width.rkt"
         "../theme.rkt"
         "message-layout.rkt")

(provide render-status-bar
         render-input-line)

;; Render the status bar.
(define (render-status-bar state width)
  (define session-name (or (ui-state-session-id state) "q"))
  (define model-name (or (ui-state-model-name state) ""))
  (define busy (ui-state-busy? state))
  (define status-text
    (cond
      [busy (format "  ⋯ ~a | ~a" session-name model-name)]
      [else (format "  ✓ ~a | ~a" session-name model-name)]))
  (define style (if busy (theme->style 'status-busy) (theme->style 'status-idle)))
  (list (styled-line (list (styled-segment status-text style)))))

;; Render the input line with prompt.
(define (render-input-line input-st width)
  (define text (input-current-text input-st))
  (define-values (visible offset cursor-col) (input-visible-window input-st width))
  (define prompt ">> ")
  (list (styled-line
         (list (styled-segment prompt (theme->style 'prompt '(bold)))
               (styled-segment visible (theme->style 'input))))))
