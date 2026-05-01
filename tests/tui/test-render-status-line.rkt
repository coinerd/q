#lang racket

;; tests/tui/test-render-status-line.rkt — tests for render/status-line

(require rackunit
         rackunit/text-ui
         "../../tui/render/status-line.rkt"
         "../../tui/render/message-layout.rkt"
         "../../tui/state.rkt"
         "../../tui/input.rkt")

(define status-tests
  (test-suite "Render Status Line"

    (test-case "render-status-bar returns a styled-line"
      (define state (initial-ui-state))
      (define result (render-status-bar state 80))
      (check-pred styled-line? result))

    (test-case "render-input-line returns a styled-line"
      (define inp (initial-input-state))
      (define result (render-input-line inp 80))
      (check-pred styled-line? result))))

(module+ main
  (run-tests status-tests))
(module+ test
  (run-tests status-tests))
