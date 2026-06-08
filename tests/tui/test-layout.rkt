#lang racket

;; BOUNDARY: io

;; tests/tui/layout.rkt — Tests for tui/layout module
;; Updated v0.59.0: tests use legacy (cols, rows) convention matching renderer tests;
;; compute-layout auto-detects and swaps when first arg > second.

(require rackunit
         rackunit/text-ui
         "../../tui/layout.rkt")

(define layout-tests
  (test-suite "TUI Layout"

    ;; --------------------------------------------------------
    ;; compute-layout: 80x24 standard terminal (legacy order)
    ;; --------------------------------------------------------
    (test-case "standard 80x24 terminal layout"
      (let ([L (compute-layout 24 80)])
        (check-equal? (tui-layout-cols L) 80)
        (check-equal? (tui-layout-rows L) 24)
        (check-false (tui-layout-header-row L))
        ;; header=1, input=3 → 4 fixed; transcript = 24-4 = 20
        (check-equal? (tui-layout-transcript-start-row L) 1)
        (check-equal? (tui-layout-transcript-height L) 20)
        (check-equal? (tui-layout-status-row L) 21)
        (check-equal? (tui-layout-input-row L) 22)))

    ;; --------------------------------------------------------
    ;; compute-layout: minimum size clamped to header+input = 4
    ;; --------------------------------------------------------
    (test-case "minimum size 80x2 clamped to 4 rows"
      (let ([L (compute-layout 2 80)])
        (check-equal? (tui-layout-rows L) 4)
        (check-equal? (tui-layout-transcript-height L) 0)
        (check-false (tui-layout-header-row L))
        (check-equal? (tui-layout-transcript-start-row L) 1)
        (check-equal? (tui-layout-status-row L) 1)
        (check-equal? (tui-layout-input-row L) 2)))

    ;; --------------------------------------------------------
    ;; compute-layout: tall 120x50 terminal
    ;; --------------------------------------------------------
    (test-case "tall 120x50 terminal layout"
      (let ([L (compute-layout 50 120)])
        (check-equal? (tui-layout-cols L) 120)
        (check-equal? (tui-layout-rows L) 50)
        (check-false (tui-layout-header-row L))
        (check-equal? (tui-layout-transcript-start-row L) 1)
        (check-equal? (tui-layout-transcript-height L) 46)
        (check-equal? (tui-layout-status-row L) 47)
        (check-equal? (tui-layout-input-row L) 48)))

    ;; --------------------------------------------------------
    ;; Transcript height = rows - 4 for rows >= 4 (no widgets)
    ;; --------------------------------------------------------
    (test-case "transcript height equals rows minus 4 for rows 4..29"
      (for ([r (in-range 4 30)])
        (define L (compute-layout r 80))
        (check-equal? (tui-layout-transcript-height L)
                      (- r 4)
                      (format "transcript height for rows=~a" r))))

    ;; --------------------------------------------------------
    ;; Row positions are consistent
    ;; --------------------------------------------------------
    (test-case "row positions are internally consistent for rows 4..19"
      (for ([r (in-range 4 20)])
        (define L (compute-layout r 80))
        (check-false (tui-layout-header-row L))
        (check-equal? (tui-layout-transcript-start-row L) 1)
        (check-equal? (+ (tui-layout-transcript-start-row L) (tui-layout-transcript-height L))
                      (tui-layout-status-row L)
                      (format "rows=~a: transcript-start + height = status-row" r))
        ;; input-row is the second line of input region (where user types)
        (check-equal? (tui-layout-input-row L)
                      (add1 (tui-layout-status-row L))
                      (format "rows=~a: input-row = status-row + 1" r))
        ;; total rows check (input ends at input_y + input_height)
        (check-equal? (tui-layout-rows L)
                      (+ (layout-region-y (layout-input L)) (layout-region-height (layout-input L)))
                      (format "rows=~a: rows = input_y + input_height" r))))

    ;; --------------------------------------------------------
    ;; tui-layout predicate
    ;; --------------------------------------------------------
    (test-case "tui-layout? predicate returns #t"
      (check-true (tui-layout? (compute-layout 24 80))))

    ;; --------------------------------------------------------
    ;; Narrow width doesn't crash
    ;; --------------------------------------------------------
    (test-case "narrow width 10x24 does not crash"
      ;; Use canonical (height, width) order for unambiguous call
      (let ([L (compute-layout 24 10)])
        (check-equal? (tui-layout-cols L) 10)
        (check-equal? (tui-layout-rows L) 24)))

    ;; --------------------------------------------------------
    ;; clip-to-region clips to region height
    ;; --------------------------------------------------------
    (test-case "clip-to-region clips correctly"
      (define region (layout-region 'test 0 3 80))
      (check-equal? (clip-to-region '("a" "b") region) '("a" "b" ""))
      (check-equal? (clip-to-region '("a" "b" "c" "d") region) '("a" "b" "c"))
      (check-equal? (clip-to-region '() region) '("" "" "")))

    ;; --------------------------------------------------------
    ;; compute-layout-with-widgets adds widget bar
    ;; --------------------------------------------------------
    (test-case "compute-layout-with-widgets includes widget bar"
      (define L (compute-layout-with-widgets 80 24 3))
      (check-equal? (tui-layout-rows L) 24)
      (check-equal? (tui-layout-cols L) 80)
      ;; header(1) + widget(3) + input(3) = 7 fixed; transcript = 24-7 = 17
      (check-equal? (tui-layout-transcript-height L) 17))

    ;; --------------------------------------------------------
    ;; Widget bar with zero lines is same as no widgets
    ;; --------------------------------------------------------
    (test-case "compute-layout-with-widgets zero lines same as no widgets"
      (define L-nowidget (compute-layout 24 80))
      (define L-widget0 (compute-layout-with-widgets 80 24 0))
      (check-equal? (tui-layout-transcript-height L-nowidget)
                    (tui-layout-transcript-height L-widget0)))))

(run-tests layout-tests)
