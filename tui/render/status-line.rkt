#lang racket/base

;; q/tui/render/status-line.rkt — status bar and input line rendering
;;
;; Pure functions that compose status bar and input line.
;; Single inverse segment covering entire status bar (v0.28.17+).

(require racket/string
         "../../util/cost-tracker.rkt"
         "../state.rkt"
         "../input.rkt"
         "../char-width.rkt"
         "../theme.rkt"
         "message-layout.rkt")

(provide render-status-bar
         render-input-line
         current-show-context-pressure?)

;; ── Parameter ──

(define current-show-context-pressure? (make-parameter #t))

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
  (define ctx-pressure-level (ui-state-context-pressure-level state))
  (define ctx-pressure-pct (ui-state-context-pressure-percent state))

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

  ;; Goal badge
  (define goal-info (ui-state-active-goal state))
  (define goal-badge
    (if goal-info
        (let* ([t (goal-display-info-turns-used goal-info)]
               [m (goal-display-info-max-turns goal-info)]
               [s (goal-display-info-status goal-info)]
               [status-text (case s
                              [(active) "active"]
                              [(achieved) "\u2713"]
                              [(failed) "\u2717"]
                              [(cancelled) "\u2717"]
                              [else "?"])]
               [text (format "\u25ce goal ~a/~a \u00b7 ~a" t m status-text)])
          (format " ~a" text))
        ""))

  ;; Inverse segment (prefix + session + model + state)
  (define busy-marker (if busy " *" "  "))
  (define inv-text
    (string-append " q" busy-marker " " session-label state-indicator queue-text goal-badge))

  ;; Normal-style segments (context + cost + scroll)
  ;; Show context pressure percentage when available, else token count
  (define ctx-part
    (cond
      [(and (current-show-context-pressure?) ctx-pressure-pct (real? ctx-pressure-pct))
       (define level-marker
         (case ctx-pressure-level
           [(yellow) "!"]
           [(red) "!!"]
           [else ""]))
       (format " ctx:~a~a%" (inexact->exact (round ctx-pressure-pct)) level-marker)]
      [else (format " ctx:~a" (format-tokens (or ctx-tokens 0)))]))
  (define cost-part
    (if (and cost-trk (positive? (cost-tracker-input-tokens-total cost-trk)))
        (format " ~a" (format-cost (cost-tracker-total cost-trk)))
        ""))
  (define scroll-text (if (and scroll-off (positive? scroll-off)) " \u2191" ""))
  (define mock-warn (if (ui-state-mock-provider? state) " [No API key]" ""))
  (define normal-text (string-append ctx-part cost-part scroll-text mock-warn))

  ;; Padding to fill remaining width

  ;; Build styled-line with single inverse segment — entire line visible on all terminals
  (define full-text (string-append inv-text normal-text))
  (define padding (make-string (max 0 (- width (string-length full-text))) #\space))
  (define inv-style (theme->style 'status-busy '(inverse)))
  (styled-line (list (styled-segment (string-append full-text padding) inv-style))))

;; ── Input line ──

;; Render the input line with prompt.
(define (render-input-line input-st width)
  (define text (input-current-text input-st))
  (define-values (visible offset cursor-col) (input-visible-window input-st width))
  (define prompt "q> ")
  (styled-line (list (styled-segment prompt (theme->style 'prompt '(bold cyan)))
                     (styled-segment visible (theme->style 'input)))))
