#lang racket

;; tests/tui/layout.rkt — Tests for tui/layout module

(require rackunit
         rackunit/text-ui
         "../../../q/tui/layout.rkt")

(define layout-tests
  (test-suite
   "TUI Layout"

   ;; --------------------------------------------------------
   ;; compute-layout: 80x24 standard terminal
   ;; --------------------------------------------------------
   (test-case "standard 80x24 terminal layout"
     (let ([L (compute-layout 80 24)])
       (check-equal? (tui-layout-cols L) 80)
       (check-equal? (tui-layout-rows L) 24)
       (check-equal? (tui-layout-header-row L) 1)
       (check-equal? (tui-layout-transcript-start-row L) 2)
       (check-equal? (tui-layout-transcript-height L) 21)   ;; 24 - 3
       (check-equal? (tui-layout-status-row L) 23)           ;; 2 + 21
       (check-equal? (tui-layout-input-row L) 24)))          ;; 3 + 21

   ;; --------------------------------------------------------
   ;; compute-layout: minimum size clamped to 4 rows
   ;; --------------------------------------------------------
   (test-case "minimum size 80x3 clamped to 4 rows"
     (let ([L (compute-layout 80 3)])
       (check-equal? (tui-layout-rows L) 4)
       (check-equal? (tui-layout-transcript-height L) 1)
       (check-equal? (tui-layout-header-row L) 1)
       (check-equal? (tui-layout-status-row L) 3)
       (check-equal? (tui-layout-input-row L) 4)))

   ;; --------------------------------------------------------
   ;; compute-layout: tall terminal 120x50
   ;; --------------------------------------------------------
   (test-case "tall 120x50 terminal layout"
     (let ([L (compute-layout 120 50)])
       (check-equal? (tui-layout-cols L) 120)
       (check-equal? (tui-layout-rows L) 50)
       (check-equal? (tui-layout-transcript-height L) 47)   ;; 50 - 3
       (check-equal? (tui-layout-status-row L) 49)
       (check-equal? (tui-layout-input-row L) 50)))

   ;; --------------------------------------------------------
   ;; Transcript height = rows - 3 for rows >= 4
   ;; --------------------------------------------------------
   (test-case "transcript height equals rows minus 3 for rows 4..29"
     (for ([r (in-range 4 30)])
       (define L (compute-layout 80 r))
       (check-equal? (tui-layout-transcript-height L) (- r 3)
                     (format "transcript height for rows=~a" r))))

   ;; --------------------------------------------------------
   ;; Row positions are consistent
   ;; --------------------------------------------------------
   (test-case "row positions are internally consistent for rows 4..19"
     (for ([r (in-range 4 20)])
       (define L (compute-layout 80 r))
       (check-equal? (tui-layout-header-row L) 1)
       (check-equal? (tui-layout-transcript-start-row L) 2)
       (check-equal? (+ (tui-layout-transcript-start-row L)
                        (tui-layout-transcript-height L))
                     (tui-layout-status-row L)
                     (format "rows=~a: transcript-start + height = status-row" r))
       (check-equal? (+ (tui-layout-status-row L) 1)
                     (tui-layout-input-row L)
                     (format "rows=~a: status-row + 1 = input-row" r))))

   ;; --------------------------------------------------------
   ;; tui-layout predicate
   ;; --------------------------------------------------------
   (test-case "tui-layout? predicate returns #t"
     (check-true (tui-layout? (compute-layout 80 24))))

   ;; --------------------------------------------------------
   ;; Narrow width doesn't crash
   ;; --------------------------------------------------------
   (test-case "narrow width 10x24 does not crash"
     (let ([L (compute-layout 10 24)])
       (check-equal? (tui-layout-cols L) 10)
       (check-equal? (tui-layout-rows L) 24)))))

(run-tests layout-tests)
