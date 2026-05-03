#lang racket/base

;; q/tui/render/status-line.rkt — status bar and input line rendering
;;
;; Pure functions that compose status bar and input line.
;; v0.28.14: Multi-segment status bar — inverse content + normal context/cost.

(require racket/string
         "../../util/cost-tracker.rkt"
         "../state.rkt"
         "../input.rkt"
         "../char-width.rkt"
         "../theme.rkt"
         "message-layout.rkt")

(provide render-status-bar
         render-input-line)

;; ── Token formatting helper ──

(define (format-tokens n)
  (cond
    [(>= n 1000000) (format "~aM" (exact->inexact (/ (round (/ n 1000)) 1000)))]
    [(>= n 1000) (format "~aK" (exact->inexact (/ (round (/ n 100)) 100)))]
    [else (format "~a" n)]))

;; ── Status bar ──

;; Render the status bar as a multi-segment styled-line.
;; Inverse video covers ONLY the content portion; the rest of the line
;; is plain spaces (no inverse fill to end of line).
;; Segments: prefix | session | model | state | [ctx] [cost] [scroll]
(define (render-status-bar state width)
  (define session-name (ui-state-session-id state))
  (define session-label
    (if (and session-name (> (string-length session-name) 8))
        (substring session-name (- (string-length session-name) 8))
        (or session-name "q")))
  (define model-name (or (ui-state-model-name state) ""))
  (define busy (ui-state-busy? state))
  (define status-msg (ui-state-status-message state))
  (define pending-tool (ui-state-pending-tool-name state))
  (define streaming (ui-state-streaming-text state))
  (define queue-cts (ui-state-queue-counts state))
  (define scroll-off (ui-state-scroll-offset state))
  (define ctx-tokens (ui-state-context-tokens state))
  (define cost-trk (ui-state-cost-tracker state))

  ;; Build state indicator based on busy state
  (define state-indicator
    (cond
      [(and status-msg (not (string=? status-msg ""))) (format " | ~a" status-msg)]
      [(and busy pending-tool) (format " | ~a [~a]" model-name pending-tool)]
      [(and busy (not streaming))
       (if (string=? model-name "")
           " [thinking...]"
           (format " | ~a [thinking...]" model-name))]
      [(and busy streaming)
       (if (string=? model-name "")
           " [streaming...]"
           (format " | ~a [streaming...]" model-name))]
      [(string=? model-name "") ""]
      [else (format " | ~a" model-name)]))

  ;; Queue counts
  (define queue-text
    (if (and queue-cts (hash? queue-cts) (positive? (hash-count queue-cts)))
        (let* ([parts (for/list ([(k v) (in-hash queue-cts)])
                        (format "~a:~a" k v))]
               [joined (string-join parts " ")])
          (format " [~a]" joined))
        ""))

  ;; Inverse segment (prefix + session + model + state)
  (define busy-marker (if busy " *" "  "))
  (define inv-text (string-append " q" busy-marker " " session-label state-indicator queue-text))

  ;; Normal-style segments (context + cost + scroll)
  (define ctx-part
    (if ctx-tokens
        (format " ctx:~a" (format-tokens ctx-tokens))
        ""))
  (define cost-part
    (if (and cost-trk (positive? (cost-tracker-input-tokens-total cost-trk)))
        (format " ~a" (format-cost (cost-tracker-total cost-trk)))
        ""))
  (define scroll-text (if (and scroll-off (positive? scroll-off)) " \u2191" ""))
  (define mock-warn (if (ui-state-mock-provider? state) " [No API key]" ""))
  (define normal-text (string-append ctx-part cost-part scroll-text mock-warn))

  ;; Padding to fill remaining width (plain spaces, no inverse)
  (define used (+ (string-length inv-text) (string-length normal-text)))
  (define padding (make-string (max 0 (- width used)) #\space))

  ;; Build styled-line with 3 segments: inverse-content, normal-info, padding
  (define inv-style (theme->style 'status-busy '(inverse)))
  (styled-line (list (styled-segment inv-text inv-style)
                     (styled-segment normal-text '())
                     (styled-segment padding '()))))

;; ── Input line ──

;; Render the input line with prompt.
(define (render-input-line input-st width)
  (define text (input-current-text input-st))
  (define-values (visible offset cursor-col) (input-visible-window input-st width))
  (define prompt "q> ")
  (styled-line (list (styled-segment prompt (theme->style 'prompt '(bold cyan)))
                     (styled-segment visible (theme->style 'input)))))
