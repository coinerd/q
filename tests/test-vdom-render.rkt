#lang racket

;; @speed fast
;; @suite default

;; tests/test-vdom-render.rkt — Tests for tui/vdom-render.rkt

(require rackunit
         "../tui/vdom.rkt"
         "../tui/vdom-layout.rkt"
         "../tui/vdom-render.rkt"
         "../tui/cell-buffer.rkt"
         "../tui/render/message-layout.rkt")

;; Helper: read back a row from cell buffer as string
(define (row-string buf row)
  (cell-buffer-row-string buf row))

;; ============================================================
;; Render vtext to buffer
;; ============================================================

(test-case "render vtext to buffer"
  (define buf (make-cell-buffer 20 5))
  (render-vdom-to-buffer! (vtext "Hello" '()) buf 20)
  (check-true (string-prefix? (row-string buf 0) "Hello")))

(test-case "render vtext with style"
  (define buf (make-cell-buffer 20 5))
  (render-vdom-to-buffer! (vtext "Bold" '(bold)) buf 20)
  ;; Check the character is there
  (define cell (cell-buffer-ref buf 0 0))
  (check-equal? (cell-char cell) #\B)
  (check-true (cell-bold? cell)))

;; ============================================================
;; Render vhbox to buffer
;; ============================================================

(test-case "render vhbox to buffer"
  (define buf (make-cell-buffer 20 5))
  (render-vdom-to-buffer! (vhbox (list (vtext "AB" '()) (vtext "CD" '()))) buf 20)
  (check-true (string-prefix? (row-string buf 0) "ABCD")))

;; ============================================================
;; Render vvbox to buffer
;; ============================================================

(test-case "render vvbox to buffer"
  (define buf (make-cell-buffer 20 5))
  (render-vdom-to-buffer! (vvbox (list (vtext "Line1" '()) (vtext "Line2" '()))) buf 20)
  (check-true (string-prefix? (row-string buf 0) "Line1"))
  (check-true (string-prefix? (row-string buf 1) "Line2")))

;; ============================================================
;; Render styled-lines directly
;; ============================================================

(test-case "render-styled-lines-to-buffer!"
  (define buf (make-cell-buffer 20 5))
  (define lines (list (styled-line (list (styled-segment "Test" '(bold))))))
  (render-styled-lines-to-buffer! lines buf 20)
  (check-true (string-prefix? (row-string buf 0) "Test"))
  (check-true (cell-bold? (cell-buffer-ref buf 0 0))))

(test-case "render-styled-lines with start-row"
  (define buf (make-cell-buffer 20 5))
  (define lines (list (styled-line (list (styled-segment "Row2" '())))))
  (render-styled-lines-to-buffer! lines buf 20 #:start-row 2)
  (check-true (string-prefix? (row-string buf 2) "Row2")))

;; ============================================================
;; Truncation
;; ============================================================

(test-case "truncate long text to buffer width"
  (define buf (make-cell-buffer 5 3))
  (render-vdom-to-buffer! (vtext "Hello World" '()) buf 5)
  (check-true (string-prefix? (row-string buf 0) "Hello")))

;; ============================================================
;; Complex tree
;; ============================================================

(test-case "render complex vnode tree"
  (define tree
    (vvbox (list (vhbox (list (vtext "Header" '(bold)) (vfill* 5) (vtext "End" '())))
                 (vtext "Body" '()))))
  (define buf (make-cell-buffer 30 5))
  (render-vdom-to-buffer! tree buf 30)
  (check-true (string-contains? (row-string buf 0) "Header"))
  (check-true (string-contains? (row-string buf 0) "End"))
  (check-true (string-prefix? (row-string buf 1) "Body")))

;; ============================================================
;; Section rendering with positioning
;; ============================================================

(test-case "render-vdom-section-to-buffer! positions at start-row"
  (define vnodes (list (vtext "Line1" '()) (vtext "Line2" '())))
  (define buf (make-cell-buffer 20 5))
  (render-vdom-section-to-buffer! vnodes buf 20 2) ; start at row 2
  (check-true (string-prefix? (row-string buf 2) "Line1"))
  (check-true (string-prefix? (row-string buf 3) "Line2")))

(test-case "render-vdom-section-to-buffer! clips to max-rows"
  (define vnodes (list (vtext "A" '()) (vtext "B" '()) (vtext "C" '()) (vtext "D" '())))
  (define buf (make-cell-buffer 20 6))
  (render-vdom-section-to-buffer! vnodes buf 20 1 2) ; max 2 rows
  (check-true (string-prefix? (row-string buf 1) "C")) ; takes last 2: C, D
  (check-true (string-prefix? (row-string buf 2) "D")))

(test-case "render-vdom-section-to-buffer! pads with blanks"
  (define vnodes (list (vtext "Only" '())))
  (define buf (make-cell-buffer 20 6))
  (render-vdom-section-to-buffer! vnodes buf 20 0 3) ; max 3 rows, only 1 line
  (check-true (string-prefix? (row-string buf 0) "")) ; padding row
  (check-true (string-prefix? (row-string buf 1) "")) ; padding row
  (check-true (string-prefix? (row-string buf 2) "Only"))) ; content at bottom
