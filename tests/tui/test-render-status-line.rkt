#lang racket

;; tests/tui/test-render-status-line.rkt — comprehensive tests for render/status-line
;; v0.28.17: Single inverse segment status bar, busy/thinking/streaming, ctx, cost, scroll

(require rackunit
         rackunit/text-ui
         "../../tui/render/status-line.rkt"
         "../../tui/render/message-layout.rkt"
         "../../tui/state.rkt"
         "../../tui/input.rkt"
         "../../util/cost-tracker.rkt")

(define (get-segments result)
  (styled-line-segments result))

(define (segment-texts result)
  (map styled-segment-text (get-segments result)))

(define (first-segment result)
  (car (get-segments result)))

(define status-tests
  (test-suite "Render Status Line"

    ;; --------------------------------------------------------
    ;; Basic structure
    ;; --------------------------------------------------------
    (test-case "render-status-bar returns a styled-line"
      (define state (initial-ui-state))
      (define result (render-status-bar state 80))
      (check-pred styled-line? result))

    (test-case "render-input-line returns a styled-line"
      (define inp (initial-input-state))
      (define result (render-input-line inp 80))
      (check-pred styled-line? result))

    ;; --------------------------------------------------------
    ;; Single-segment structure (v0.28.17: unified inverse)
    ;; --------------------------------------------------------
    (test-case "status bar has 1 segment (unified inverse)"
      (define state (initial-ui-state))
      (define result (render-status-bar state 80))
      (define segs (get-segments result))
      (check-equal? (length segs) 1)
      ;; Single segment has inverse style
      (check-not-false (member 'inverse (styled-segment-style (car segs)))))

    ;; --------------------------------------------------------
    ;; Idle state
    ;; --------------------------------------------------------
    (test-case "idle state shows prefix and session label"
      (define state (initial-ui-state #:session-id "test-session-1234"))
      (define result (render-status-bar state 80))
      (define text (styled-segment-text (first-segment result)))
      (check-true (string-contains? text " q"))
      (check-true (string-contains? text "1234"))) ;; last 8 chars of session id

    (test-case "idle state with model shows model name"
      (define state (initial-ui-state #:model-name "gpt-4o"))
      (define result (render-status-bar state 80))
      (define text (styled-segment-text (first-segment result)))
      (check-true (string-contains? text "gpt-4o")))

    ;; --------------------------------------------------------
    ;; Busy + thinking
    ;; --------------------------------------------------------
    (test-case "busy with no streaming shows [thinking...]"
      (define state (struct-copy ui-state (initial-ui-state) [busy? #t]))
      (define result (render-status-bar state 80))
      (define text (styled-segment-text (first-segment result)))
      (check-true (string-contains? text "[thinking...]")))

    ;; --------------------------------------------------------
    ;; Busy + tool
    ;; --------------------------------------------------------
    (test-case "busy with pending tool shows [tool-name]"
      (define state (struct-copy ui-state (initial-ui-state) [busy? #t] [pending-tool-name "bash"]))
      (define result (render-status-bar state 80))
      (define text (styled-segment-text (first-segment result)))
      (check-true (string-contains? text "[bash]")))

    ;; --------------------------------------------------------
    ;; Busy + streaming
    ;; --------------------------------------------------------
    (test-case "busy with streaming text shows [streaming...]"
      (define state (struct-copy ui-state (initial-ui-state) [busy? #t] [streaming-text "Hello"]))
      (define result (render-status-bar state 80))
      (define text (styled-segment-text (first-segment result)))
      (check-true (string-contains? text "[streaming...]")))

    ;; --------------------------------------------------------
    ;; Context tokens (v0.28.17: in single segment)
    ;; --------------------------------------------------------
    (test-case "context-tokens shows in status bar text"
      (define state (struct-copy ui-state (initial-ui-state) [context-tokens 12000]))
      (define result (render-status-bar state 80))
      (define text (styled-segment-text (first-segment result)))
      (check-true (string-contains? text "ctx:"))
      (check-true (string-contains? text "1.2K")))

    (test-case "context-tokens 1500000 formats as M"
      (define state (struct-copy ui-state (initial-ui-state) [context-tokens 1500000]))
      (define result (render-status-bar state 80))
      (define text (styled-segment-text (first-segment result)))
      (check-true (string-contains? text "M")))

    ;; --------------------------------------------------------
    ;; Cost tracker (v0.28.17: in single segment)
    ;; --------------------------------------------------------
    (test-case "cost tracker with tokens shows cost in status bar text"
      (define trk (make-cost-tracker "gpt-4o"))
      (cost-tracker-update! trk 1000 500)
      (define state (struct-copy ui-state (initial-ui-state) [cost-tracker trk]))
      (define result (render-status-bar state 80))
      (define text (styled-segment-text (first-segment result)))
      (check-true (string-contains? text "$")))

    ;; --------------------------------------------------------
    ;; Scroll indicator (v0.28.17: in single segment)
    ;; --------------------------------------------------------
    (test-case "scroll offset shows arrow in status bar text"
      (define state (struct-copy ui-state (initial-ui-state) [scroll-offset 5]))
      (define result (render-status-bar state 80))
      (define text (styled-segment-text (first-segment result)))
      (check-true (string-contains? text "\u2191")))

    (test-case "zero scroll offset shows no arrow"
      (define state (struct-copy ui-state (initial-ui-state) [scroll-offset 0]))
      (define result (render-status-bar state 80))
      (define text (styled-segment-text (first-segment result)))
      (check-false (string-contains? text "\u2191")))

    ;; --------------------------------------------------------
    ;; Width fitting (v0.28.17: single segment fills width)
    ;; --------------------------------------------------------
    (test-case "single segment length fits within width"
      (define state
        (struct-copy ui-state
                     (initial-ui-state #:model-name "gpt-4o")
                     [busy? #t]
                     [context-tokens 12000]))
      (define width 80)
      (define result (render-status-bar state width))
      (define text (styled-segment-text (first-segment result)))
      (check-true (<= (string-length text) width)
                  (format "length ~a > width ~a" (string-length text) width)))))

(module+ main
  (run-tests status-tests))
(module+ test
  (run-tests status-tests))
