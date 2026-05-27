#lang racket

;; tests/test-vdom-layout.rkt — Tests for tui/vdom-layout.rkt

(require rackunit
         "../tui/vdom.rkt"
         "../tui/vdom-layout.rkt"
         "../tui/render/message-layout.rkt")

;; ============================================================
;; vtext layout
;; ============================================================

(test-case "layout vtext produces single styled-line"
  (define lines (vdom-layout (vtext "Hello" '(bold)) 80))
  (check-equal? (length lines) 1)
  (define segs (styled-line-segments (car lines)))
  (check-equal? (length segs) 1)
  (check-equal? (styled-segment-text (car segs)) "Hello")
  (check-equal? (styled-segment-style (car segs)) '(bold)))

(test-case "layout vtext truncates to width"
  (define lines (vdom-layout (vtext "Hello World" '()) 5))
  (define segs (styled-line-segments (car lines)))
  (check-equal? (styled-segment-text (car segs)) "Hello"))

;; ============================================================
;; vhbox layout
;; ============================================================

(test-case "layout vhbox concatenates horizontally"
  (define lines (vdom-layout (vhbox (list (vtext "AB" '()) (vtext "CD" '()))) 80))
  (check-equal? (length lines) 1)
  (define segs (styled-line-segments (car lines)))
  ;; Should have two segments
  (check-equal? (length segs) 2)
  (check-equal? (styled-segment-text (car segs)) "AB")
  (check-equal? (styled-segment-text (cadr segs)) "CD"))

(test-case "layout vhbox respects width"
  (define lines (vdom-layout (vhbox (list (vtext "AAAA" '()) (vtext "BBBB" '()))) 5))
  (check-equal? (length lines) 1)
  (define segs (styled-line-segments (car lines)))
  (define total (apply + (map (lambda (s) (string-length (styled-segment-text s))) segs)))
  (check-true (<= total 5)))

(test-case "layout empty vhbox"
  (define lines (vdom-layout (vhbox '()) 80))
  (check-equal? (length lines) 1))

;; ============================================================
;; vvbox layout
;; ============================================================

(test-case "layout vvbox stacks vertically"
  (define lines (vdom-layout (vvbox (list (vtext "Line1" '()) (vtext "Line2" '()))) 80))
  (check-equal? (length lines) 2)
  (define seg1 (styled-line-segments (car lines)))
  (define seg2 (styled-line-segments (cadr lines)))
  (check-equal? (styled-segment-text (car seg1)) "Line1")
  (check-equal? (styled-segment-text (car seg2)) "Line2"))

(test-case "layout empty vvbox"
  (define lines (vdom-layout (vvbox '()) 80))
  (check-equal? lines '()))

(test-case "layout vvbox with hbox children"
  (define lines
    (vdom-layout (vvbox (list (vhbox (list (vtext "Left" '()) (vtext "Right" '())))
                              (vtext "Second row" '())))
                 80))
  (check-equal? (length lines) 2))

;; ============================================================
;; vfill layout
;; ============================================================

(test-case "layout vfill produces padding"
  (define lines (vdom-layout (vfill 10 #\space '()) 80))
  (check-equal? (length lines) 1)
  (define segs (styled-line-segments (car lines)))
  (check-equal? (styled-segment-text (car segs)) (make-string 10 #\space)))

(test-case "layout vfill respects width"
  (define lines (vdom-layout (vfill 100 #\. '()) 20))
  (define segs (styled-line-segments (car lines)))
  (check-equal? (string-length (styled-segment-text (car segs))) 20))

;; ============================================================
;; voverlay layout
;; ============================================================

(test-case "layout voverlay merges content"
  (define lines (vdom-layout (voverlay (vtext "OVER" '(inverse)) (vtext "BASE LINE" '()) 2 0) 80))
  (check-equal? (length lines) 1)
  (define text (segs->text (styled-line-segments (car lines))))
  ;; Should have "BA" + "OVER" at position 2
  (check-true (string-contains? text "OVER")))

;; ============================================================
;; Complex tree layout
;; ============================================================

(test-case "layout complex header bar"
  (define header (vhbox (list (vtext " q " '(bold cyan)) (vfill* 10) (vtext " model " '()))))
  (define lines (vdom-layout header 80))
  (check-equal? (length lines) 1))

(test-case "layout status bar with overlay"
  (define bar
    (vvbox (list (vhbox (list (vtext "Status:" '(bold)) (vtext " OK" '(green))))
                 (voverlay (vtext "[popup]" '(inverse)) (vtext "Normal content here" '()) 5 0))))
  (define lines (vdom-layout bar 80))
  (check-true (>= (length lines) 2)))
