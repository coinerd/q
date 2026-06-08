#lang racket/base

;; @speed fast  ;; @suite tui

;; tests/test-tui-render-phases.rkt -- Tests for TUI render phase helpers
;; v0.74.5: Pure function tests for extracted render helpers.

(require rackunit
         rackunit/text-ui
         (only-in "../tui/tui-render-loop.rkt"
                  clip-visible-lines
                  compute-pad-count
                  clip-overlay-content))

(define-test-suite tui-render-phase-tests

  ;; -- clip-visible-lines ------------------------------------------------
  (test-case "clip-visible-lines returns all when within height"
    (check-equal? (clip-visible-lines '(a b c) 5) '(a b c)))

  (test-case "clip-visible-lines clips from bottom when exceeds height"
    (check-equal? (clip-visible-lines '(a b c d e) 3) '(c d e)))

  (test-case "clip-visible-lines with empty list"
    (check-equal? (clip-visible-lines '() 5) '()))

  (test-case "clip-visible-lines with exact height"
    (check-equal? (clip-visible-lines '(a b c) 3) '(a b c)))

  (test-case "clip-visible-lines with height 0"
    (check-equal? (clip-visible-lines '(a b c) 0) '()))

  ;; -- compute-pad-count -------------------------------------------------
  (test-case "compute-pad-count returns 0 when lines fill height"
    (check-equal? (compute-pad-count '(a b c) 3) 0))

  (test-case "compute-pad-count computes padding needed"
    (check-equal? (compute-pad-count '(a) 5) 4))

  (test-case "compute-pad-count with empty list"
    (check-equal? (compute-pad-count '() 5) 5))

  (test-case "compute-pad-count with more lines than height"
    (check-equal? (compute-pad-count '(a b c d e) 3) 0))

  ;; -- clip-overlay-content ----------------------------------------------
  (test-case "clip-overlay-content returns all when within height"
    (check-equal? (clip-overlay-content '(x y) 5) '(x y)))

  (test-case "clip-overlay-content clips when exceeds height"
    (check-equal? (clip-overlay-content '(a b c d e) 3) '(c d e)))

  (test-case "clip-overlay-content with empty content"
    (check-equal? (clip-overlay-content '() 5) '())))

(module+ main
  (run-tests tui-render-phase-tests))
