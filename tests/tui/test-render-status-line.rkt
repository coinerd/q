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

    (test-case "render-status-bar returns list of styled-lines"
      (define state (initial-ui-state))
      (define result (render-status-bar state 80))
      (check-true (list? result))
      (check-true (andmap styled-line? result)))

    (test-case "render-input-line returns list of styled-lines"
      (define inp (initial-input-state))
      (define result (render-input-line inp 80))
      (check-true (list? result))
      (check-true (andmap styled-line? result)))))

(module+ main
  (run-tests status-tests))
(module+ test
  (run-tests status-tests))
