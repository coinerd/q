#lang racket

;; BOUNDARY: io

;; tests/test-tui-renderer.rkt — Tests for tui/renderer module
;;
;; Tests the ubuf-based renderer using a mock ubuf implementation.

(require rackunit
         rackunit/text-ui
         "../tui/renderer.rkt"
         "../tui/render.rkt"
         "../tui/layout.rkt"
         "../tui/state.rkt"
         "../tui/input.rkt"
         "../tui/char-width.rkt"
         "../util/protocol-types.rkt"
         "tui/event-simulator.rkt")

;; ============================================================
;; Mock ubuf implementation for testing
;; ============================================================

;; A mock cell holding character and style attributes
;; Matches tui-ubuf: fg/bg as integers, bold, underline
(struct mock-cell (char bold underline fg bg) #:transparent #:mutable)

;; Create an empty cell (default fg=7, bg=0 like tui-ubuf)
(define (make-empty-cell)
  (mock-cell #\space #f #f 7 0))

;; A mock ubuf buffer
(struct mock-ubuf (cols rows cells) #:transparent)

;; Create a mock ubuf with given dimensions
(define (make-mock-ubuf cols rows)
  (define cells (make-vector (* cols rows) (make-empty-cell)))
  ;; Initialize with fresh cells (not shared)
  (for ([i (in-range (* cols rows))])
    (vector-set! cells i (make-empty-cell)))
  (mock-ubuf cols rows cells))

;; Get cell at (col, row) — 0-indexed (matching real ubuf)
(define (mock-ubuf-cell ubuf col row)
  (define cols (mock-ubuf-cols ubuf))
  (define idx (+ (* row cols) col))
  (vector-ref (mock-ubuf-cells ubuf) idx))

;; Set cell character
(define (mock-ubuf-set-cell! ubuf col row char bold underline fg bg)
  (define cell (mock-ubuf-cell ubuf col row))
  (set-mock-cell-char! cell char)
  (set-mock-cell-bold! cell bold)
  (set-mock-cell-underline! cell underline)
  (set-mock-cell-fg! cell fg)
  (set-mock-cell-bg! cell bg))

;; Clear the mock ubuf
(define (mock-ubuf-clear! ubuf)
  (define cols (mock-ubuf-cols ubuf))
  (define rows (mock-ubuf-rows ubuf))
  (for* ([row (in-range 0 rows)]
         [col (in-range 0 cols)])
    (mock-ubuf-set-cell! ubuf col row #\space #f #f 7 0)))

;; Put a string to the mock ubuf
;; Matches real tui-ubuf signature exactly:
;;   (ubuf x y str #:fg [7] #:bg [0] #:bold [#f] #:underline [#f] #:italic [#f] #:blink [#f])
(define (mock-ubuf-putstring! ubuf
                              col
                              row
                              str
                              #:fg [fg 7]
                              #:bg [bg 0]
                              #:bold [bold #f]
                              #:underline [underline #f]
                              #:italic [italic #f]
                              #:blink [blink #f])
  (for ([i (in-range (string-length str))])
    (mock-ubuf-set-cell! ubuf (+ col i) row (string-ref str i) bold underline fg bg)))

;; Read a row as string (ignoring styles)
(define (mock-ubuf-row-string ubuf row)
  (define cols (mock-ubuf-cols ubuf))
  (list->string (for/list ([col (in-range 0 cols)])
                  (mock-cell-char (mock-ubuf-cell ubuf col row)))))

;; Check if a row has a specific style attribute
(define (mock-ubuf-row-has-style? ubuf row style-pred)
  (define cols (mock-ubuf-cols ubuf))
  (for/or ([col (in-range 0 cols)])
    (style-pred (mock-ubuf-cell ubuf col row))))

;; Count cells with a specific style in a row
(define (mock-ubuf-row-style-count ubuf row style-accessor)
  (define cols (mock-ubuf-cols ubuf))
  (for/sum ([col (in-range 0 cols)]) (if (style-accessor (mock-ubuf-cell ubuf col row)) 1 0)))

;; ============================================================
;; Test helpers
;; ============================================================

;; ============================================================
;; Test suite
;; ============================================================

(define renderer-tests
  (test-suite "TUI Renderer"

    ;; --------------------------------------------------------
    ;; style->ubuf-kws
    ;; --------------------------------------------------------
    (test-case "style->ubuf-kws converts bold"
      (define-values (kws vals) (style->ubuf-kws '(bold)))
      (check-not-false (member '#:bold kws) "bold keyword present")
      (check-equal? (list-ref vals (index-of kws '#:bold)) #t "bold value is #t"))

    (test-case "style->ubuf-kws converts inverse to fg/bg swap"
      (define-values (kws vals) (style->ubuf-kws '(inverse)))
      (check-not-false (member '#:fg kws) "fg keyword present")
      (check-not-false (member '#:bg kws) "bg keyword present")
      (check-equal? (list-ref vals (index-of kws '#:fg)) 0 "inverse fg=0")
      (check-equal? (list-ref vals (index-of kws '#:bg)) 7 "inverse bg=7"))

    (test-case "style->ubuf-kws converts underline"
      (define-values (kws vals) (style->ubuf-kws '(underline)))
      (check-not-false (member '#:underline kws) "underline keyword present")
      (check-equal? (list-ref vals (index-of kws '#:underline)) #t "underline value is #t"))

    (test-case "style->ubuf-kws converts dim to fg=8"
      (define-values (kws vals) (style->ubuf-kws '(dim)))
      (check-not-false (member '#:fg kws) "fg keyword present")
      (check-equal? (list-ref vals (index-of kws '#:fg)) 8 "dim fg=8"))

    (test-case "style->ubuf-kws converts red to fg=1"
      (define-values (kws vals) (style->ubuf-kws '(red)))
      (check-not-false (member '#:fg kws) "fg keyword present")
      (check-equal? (list-ref vals (index-of kws '#:fg)) 1 "red fg=1"))

    (test-case "style->ubuf-kws converts green to fg=2"
      (define-values (kws vals) (style->ubuf-kws '(green)))
      (check-not-false (member '#:fg kws) "fg keyword present")
      (check-equal? (list-ref vals (index-of kws '#:fg)) 2 "green fg=2"))

    (test-case "style->ubuf-kws converts multiple styles"
      (define-values (kws vals) (style->ubuf-kws '(bold red)))
      (check-not-false (member '#:bold kws) "bold present")
      (check-not-false (member '#:fg kws) "fg present")
      (check-equal? (list-ref vals (index-of kws '#:fg)) 1 "red fg=1"))

    (test-case "style->ubuf-kws converts bright-black to fg=8"
      (define-values (kws vals) (style->ubuf-kws '(bright-black)))
      (check-not-false (member '#:fg kws) "fg keyword present")
      (check-equal? (list-ref vals (index-of kws '#:fg)) 8 "bright-black fg=8"))

    (test-case "style->ubuf-kws converts bright-green to fg=10"
      (define-values (kws vals) (style->ubuf-kws '(bright-green)))
      (check-not-false (member '#:fg kws) "fg keyword present")
      (check-equal? (list-ref vals (index-of kws '#:fg)) 10 "bright-green fg=10"))

    (test-case "style->ubuf-kws converts bright-white to fg=15"
      (define-values (kws vals) (style->ubuf-kws '(bright-white)))
      (check-not-false (member '#:fg kws) "fg keyword present")
      (check-equal? (list-ref vals (index-of kws '#:fg)) 15 "bright-white fg=15"))

    (test-case "style->ubuf-kws bright-blue overrides dim"
      (define-values (kws vals) (style->ubuf-kws '(dim bright-blue)))
      (check-not-false (member '#:fg kws) "fg keyword present")
      (check-equal? (list-ref vals (index-of kws '#:fg)) 12 "bright-blue fg=12 (overrides dim 8)"))

    (test-case "style->ubuf-kws handles empty style list"
      (define-values (kws vals) (style->ubuf-kws '()))
      (check-equal? kws '() "empty styles → no keywords")
      (check-equal? vals '() "empty styles → no values"))

    (test-case "style->ubuf-kws ignores unknown styles"
      (define-values (kws vals) (style->ubuf-kws '(bold unknown-style)))
      (check-not-false (member '#:bold kws) "bold present")
      ;; Only bold should appear (unknown-style ignored)
      (check-equal? (length kws) 1 "only 1 keyword for unknown style"))

    (test-case "style->ubuf-kws bold+underline returns sorted keywords"
      (define-values (kws vals) (style->ubuf-kws '(bold underline)))
      ;; keyword-apply requires sorted keywords
      (check-equal? kws '(#:bold #:underline) "keywords must be sorted"))

    (test-case "style->ubuf-kws inverse returns sorted keywords"
      (define-values (kws vals) (style->ubuf-kws '(inverse)))
      (check-equal? kws '(#:bg #:fg) "inverse keywords must be sorted: bg before fg"))

    (test-case "style->ubuf-kws cyan+underline returns sorted keywords"
      (define-values (kws vals) (style->ubuf-kws '(cyan underline)))
      (check-equal? kws '(#:fg #:underline) "cyan+underline keywords must be sorted"))

    (test-case "style->ubuf-kws bold+italic returns sorted keywords"
      (define-values (kws vals) (style->ubuf-kws '(bold italic)))
      (check-equal? kws '(#:bold #:italic) "bold+italic keywords must be sorted"))

    (test-case "style->ubuf-kws all styles returns sorted keywords"
      (define-values (kws vals) (style->ubuf-kws '(inverse bold underline)))
      (check-equal? kws '(#:bg #:bold #:fg #:underline) "all styles keywords must be sorted"))

    (test-case "style->ubuf-kws blue converts to fg=4"
      (define-values (kws vals) (style->ubuf-kws '(blue)))
      (check-equal? kws '(#:fg))
      (check-equal? vals '(4)))

    (test-case "style->ubuf-kws yellow converts to fg=3"
      (define-values (kws vals) (style->ubuf-kws '(yellow)))
      (check-equal? kws '(#:fg))
      (check-equal? vals '(3)))

    (test-case "style->ubuf-kws bold+yellow returns sorted keywords"
      (define-values (kws vals) (style->ubuf-kws '(bold yellow)))
      (check-equal? kws '(#:bold #:fg) "bold+yellow sorted")
      (check-equal? vals '(#t 3)))))

(run-tests renderer-tests)
