#lang racket

;;; tests/test-tui-layout.rkt — Layout region computation tests (#5260)

(require rackunit
         rackunit/text-ui
         "../tui/layout.rkt"
         "../tui/state-ui.rkt")

(define tui-layout-tests
  (test-suite "tui layout"

    ;; ============================================================
    ;; Basic layout computation
    ;; ============================================================

    (test-case "compute-layout with 24-line terminal"
      (define layout (compute-layout 24 80))
      (define hdr (layout-header layout))
      (define tr (layout-transcript layout))
      (define wb (layout-widget-bar layout))
      (define inp (layout-input layout))
      ;; Header at row 0, height 1
      (check-equal? (layout-region-y hdr) 0)
      (check-equal? (layout-region-height hdr) 1)
      ;; Transcript at row 1, flexible
      (check-equal? (layout-region-y tr) 1)
      ;; Widget bar — no widgets active, so height 0
      (check-equal? (layout-region-height wb) 0)
      ;; Input at bottom, height 3
      (check-equal? (layout-region-height inp) 3)
      ;; All regions have width 80
      (for ([r (in-list (list hdr tr wb inp))])
        (check-equal? (layout-region-width r) 80)))

    (test-case "transcript gets remaining space"
      ;; 24 lines total, header=1, widget=0 (no widgets), input=3
      ;; transcript = 24 - 1 - 0 - 3 = 20
      (define layout (compute-layout 24 80))
      (check-equal? (layout-region-height (layout-transcript layout)) 20))

    (test-case "layout with widget bar active"
      (define layout (compute-layout 24 80 #:has-widgets? #t))
      ;; header=1, widget=3 (default), input=3, transcript=24-1-3-3=17
      (check-equal? (layout-region-height (layout-transcript layout)) 17)
      (check-equal? (layout-region-height (layout-widget-bar layout)) 3)
      ;; widget-bar starts after transcript
      (check-equal? (layout-region-y (layout-widget-bar layout)) 18))

    (test-case "layout with custom widget-bar height"
      (define layout (compute-layout 30 100 #:has-widgets? #t #:widget-bar-h 5))
      ;; header=1, widget=5, input=3, transcript=30-1-5-3=21
      (check-equal? (layout-region-height (layout-transcript layout)) 21)
      (check-equal? (layout-region-height (layout-widget-bar layout)) 5)
      (check-equal? (layout-region-width (layout-transcript layout)) 100))

    (test-case "regions stack without gaps"
      (define layout (compute-layout 24 80 #:has-widgets? #t))
      (define hdr (layout-header layout))
      (define tr (layout-transcript layout))
      (define wb (layout-widget-bar layout))
      (define inp (layout-input layout))
      ;; Each region starts where previous ends
      (check-equal? (layout-region-y tr) (+ (layout-region-y hdr) (layout-region-height hdr)))
      (check-equal? (layout-region-y wb) (+ (layout-region-y tr) (layout-region-height tr)))
      (check-equal? (layout-region-y inp) (+ (layout-region-y wb) (layout-region-height wb))))

    (test-case "total height matches terminal height"
      (define layout (compute-layout 24 80 #:has-widgets? #t))
      (define inp (layout-input layout))
      (check-equal? (+ (layout-region-y inp) (layout-region-height inp)) 24))

    ;; ============================================================
    ;; Edge cases
    ;; ============================================================

    (test-case "very small terminal — transcript gets 0 height"
      ;; 5 lines: header=1, widget=3, input=3 → transcript = 5-1-3-3 = -2 → clamped to 0
      (define layout (compute-layout 5 40 #:has-widgets? #t))
      (check-equal? (layout-region-height (layout-transcript layout)) 0))

    (test-case "no widgets — widget-bar height is 0"
      (define layout (compute-layout 24 80 #:has-widgets? #f))
      (check-equal? (layout-region-height (layout-widget-bar layout)) 0))

    (test-case "zero widget-bar-h with has-widgets? true — no widget space"
      (define layout (compute-layout 24 80 #:has-widgets? #t #:widget-bar-h 0))
      (check-equal? (layout-region-height (layout-widget-bar layout)) 0)
      (check-equal? (layout-region-height (layout-transcript layout)) 20))

    ;; ============================================================
    ;; clip-to-region
    ;; ============================================================

    (test-case "clip-to-region truncates to region height"
      (define region (layout-region 'test 0 3 80))
      (define lines '("a" "b" "c" "d" "e"))
      (define clipped (clip-to-region lines region))
      (check-equal? (length clipped) 3)
      (check-equal? clipped '("a" "b" "c")))

    (test-case "clip-to-region pads if fewer lines than region height"
      (define region (layout-region 'test 0 5 80))
      (define lines '("a" "b"))
      (define clipped (clip-to-region lines region))
      (check-equal? (length clipped) 5)
      (check-equal? (take clipped 2) '("a" "b")))

    ;; ============================================================
    ;; widget-bar-height parameter
    ;; ============================================================

    (test-case "layout uses widget-bar-height parameter"
      (parameterize ([widget-bar-height 7])
        (define layout (compute-layout 30 80 #:has-widgets? #t))
        (check-equal? (layout-region-height (layout-widget-bar layout)) 7)))))

(module+ main
  (run-tests tui-layout-tests))
