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
;; Always uses inverse style (bg=7) for visibility.
;; Composes: prefix | session | model [tool] [thinking] [queues] [scroll] [No API key]
(define (render-status-bar state width)
  (define session-name (or (ui-state-session-id state) "q"))
  (define model-name (or (ui-state-model-name state) ""))
  (define busy (ui-state-busy? state))
  (define status-msg (ui-state-status-message state))
  (define pending-tool (ui-state-pending-tool-name state))
  (define queue-cts (ui-state-queue-counts state))
  (define scroll-off (ui-state-scroll-offset state))
  ;; Build status text
  (define base-text
    (cond
      ;; Status message takes priority when set
      [(and status-msg (not (string=? status-msg "")))
       (format " q * ~a | ~a" session-name status-msg)]
      [busy
       (define tool-part
         (if pending-tool
             (format " [~a]" pending-tool)
             ""))
       (define think-part (if (ui-state-streaming-text state) "" " [thinking...]"))
       (format " q * ~a | ~a~a~a" session-name model-name tool-part think-part)]
      [(string=? model-name "") (format " q   ~a [No API key]" session-name)]
      [else (format " q   ~a | ~a" session-name model-name)]))
  ;; Append queue counts if present
  (define queue-text
    (if (and queue-cts (hash? queue-cts) (positive? (hash-count queue-cts)))
        (let* ([parts (for/list ([(k v) (in-hash queue-cts)])
                        (format "~a:~a" k v))]
               [joined (string-join parts " ")])
          (format " [~a]" joined))
        ""))
  ;; Append scroll indicator if scrolled
  (define scroll-text (if (and scroll-off (positive? scroll-off)) " \u2191" ""))
  (define full-text (string-append base-text queue-text scroll-text))
  (define style (theme->style 'status-busy '(inverse)))
  (styled-line (list (styled-segment full-text style))))

;; Render the input line with prompt.
(define (render-input-line input-st width)
  (define text (input-current-text input-st))
  (define-values (visible offset cursor-col) (input-visible-window input-st width))
  (define prompt "q> ")
  (styled-line (list (styled-segment prompt (theme->style 'prompt '(bold cyan)))
                     (styled-segment visible (theme->style 'input)))))
