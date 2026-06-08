#lang racket

;; @speed fast
;; @suite default

;; tests/test-cell-buffer.rkt — Tests for tui/cell-buffer.rkt

(require rackunit
         "../tui/cell-buffer.rkt")

;; ============================================================
;; Creation
;; ============================================================

(test-case "make-cell-buffer creates buffer with correct dimensions"
  (define buf (make-cell-buffer 80 24))
  (check-equal? (cell-buffer-cols buf) 80)
  (check-equal? (cell-buffer-rows buf) 24))

(test-case "make-cell-buffer fills with default cells"
  (define buf (make-cell-buffer 10 5))
  (for* ([r (in-range 5)]
         [c (in-range 10)])
    (define cell (cell-buffer-ref buf c r))
    (check-equal? (cell-char cell) #\space)
    (check-equal? (cell-fg cell) 7)
    (check-equal? (cell-bg cell) 0)
    (check-false (cell-bold? cell))
    (check-false (cell-underline? cell))))

(test-case "new buffer is dirty"
  (define buf (make-cell-buffer 10 5))
  (check-true (cell-buffer-dirty? buf)))

;; ============================================================
;; Cell access and mutation
;; ============================================================

(test-case "cell-buffer-set! writes a cell"
  (define buf (make-cell-buffer 10 5))
  (cell-buffer-set! buf 3 2 #:char #\X #:fg 31 #:bold #t)
  (define cell (cell-buffer-ref buf 3 2))
  (check-equal? (cell-char cell) #\X)
  (check-equal? (cell-fg cell) 31)
  (check-true (cell-bold? cell)))

(test-case "cell-buffer-set! preserves defaults for unspecified attrs"
  (define buf (make-cell-buffer 10 5))
  (cell-buffer-set! buf 0 0 #:char #\A)
  (define cell (cell-buffer-ref buf 0 0))
  (check-equal? (cell-char cell) #\A)
  (check-equal? (cell-fg cell) 7)
  (check-false (cell-bold? cell)))

(test-case "cell-buffer-set! marks buffer dirty"
  (define buf (make-cell-buffer 10 5))
  (set-cell-buffer-dirty?! buf #f)
  (cell-buffer-set! buf 0 0 #:char #\Z)
  (check-true (cell-buffer-dirty? buf)))

;; ============================================================
;; Clear
;; ============================================================

(test-case "cell-buffer-clear! resets all cells to defaults"
  (define buf (make-cell-buffer 10 5))
  (cell-buffer-set! buf 5 3 #:char #\Q #:fg 31 #:bold #t)
  (cell-buffer-clear! buf)
  (define cell (cell-buffer-ref buf 5 3))
  (check-equal? (cell-char cell) #\space)
  (check-equal? (cell-fg cell) 7)
  (check-false (cell-bold? cell)))

(test-case "cell-buffer-clear! marks buffer dirty"
  (define buf (make-cell-buffer 10 5))
  (set-cell-buffer-dirty?! buf #f)
  (cell-buffer-clear! buf)
  (check-true (cell-buffer-dirty? buf)))

;; ============================================================
;; Resize
;; ============================================================

(test-case "cell-buffer-resize! changes dimensions"
  (define buf (make-cell-buffer 80 24))
  (cell-buffer-resize! buf 120 40)
  (check-equal? (cell-buffer-cols buf) 120)
  (check-equal? (cell-buffer-rows buf) 40))

(test-case "cell-buffer-resize! preserves existing content"
  (define buf (make-cell-buffer 80 24))
  (cell-buffer-set! buf 10 5 #:char #\K #:fg 32)
  (cell-buffer-resize! buf 120 40)
  (define cell (cell-buffer-ref buf 10 5))
  (check-equal? (cell-char cell) #\K)
  (check-equal? (cell-fg cell) 32))

(test-case "cell-buffer-resize! fills new area with defaults"
  (define buf (make-cell-buffer 10 5))
  (cell-buffer-resize! buf 20 10)
  (define cell (cell-buffer-ref buf 15 8))
  (check-equal? (cell-char cell) #\space)
  (check-equal? (cell-fg cell) 7))

(test-case "cell-buffer-resize! shrinks buffer"
  (define buf (make-cell-buffer 80 24))
  (cell-buffer-set! buf 5 5 #:char #\A)
  (cell-buffer-resize! buf 40 12)
  (check-equal? (cell-buffer-cols buf) 40)
  (check-equal? (cell-buffer-rows buf) 12)
  ;; Cell (5,5) is within new bounds, should be preserved
  (check-equal? (cell-char (cell-buffer-ref buf 5 5)) #\A))

(test-case "cell-buffer-resize! marks buffer dirty"
  (define buf (make-cell-buffer 10 5))
  (set-cell-buffer-dirty?! buf #f)
  (cell-buffer-resize! buf 20 10)
  (check-true (cell-buffer-dirty? buf)))

;; ============================================================
;; putstring!
;; ============================================================

(test-case "cell-buffer-putstring! writes string to buffer"
  (define buf (make-cell-buffer 80 24))
  (cell-buffer-putstring! buf 5 3 "Hello")
  (check-equal? (cell-char (cell-buffer-ref buf 5 3)) #\H)
  (check-equal? (cell-char (cell-buffer-ref buf 6 3)) #\e)
  (check-equal? (cell-char (cell-buffer-ref buf 7 3)) #\l)
  (check-equal? (cell-char (cell-buffer-ref buf 8 3)) #\l)
  (check-equal? (cell-char (cell-buffer-ref buf 9 3)) #\o))

(test-case "cell-buffer-putstring! applies attributes to all chars"
  (define buf (make-cell-buffer 80 24))
  (cell-buffer-putstring! buf 0 0 "XY" #:fg 31 #:bg 4 #:bold #t)
  (check-equal? (cell-fg (cell-buffer-ref buf 0 0)) 31)
  (check-equal? (cell-bg (cell-buffer-ref buf 0 0)) 4)
  (check-true (cell-bold? (cell-buffer-ref buf 0 0)))
  (check-equal? (cell-fg (cell-buffer-ref buf 1 0)) 31))

(test-case "cell-buffer-putstring! truncates at buffer boundary"
  (define buf (make-cell-buffer 10 5))
  ;; Write starting at col 8 — only "AB" fits (cols 8, 9)
  (cell-buffer-putstring! buf 8 0 "ABCDE")
  (check-equal? (cell-char (cell-buffer-ref buf 8 0)) #\A)
  (check-equal? (cell-char (cell-buffer-ref buf 9 0)) #\B)
  ;; Col 10+ doesn't exist, so only 2 chars written
  ;; Verify the cell after the write boundary is still default
  ;; (not applicable since buffer is only 10 cols, index 10 would be OOB)
  )

(test-case "cell-buffer-putstring! handles empty string"
  (define buf (make-cell-buffer 10 5))
  (cell-buffer-set! buf 0 0 #:char #\X)
  (cell-buffer-putstring! buf 0 0 "")
  (check-equal? (cell-char (cell-buffer-ref buf 0 0)) #\X))

;; ============================================================
;; Cell comparison
;; ============================================================

(test-case "cell-equal? returns #t for identical cells"
  (define a (vector #\A 31 4 #t #f #f #f))
  (define b (vector #\A 31 4 #t #f #f #f))
  (check-true (cell-equal? a b)))

(test-case "cell-equal? returns #f for different chars"
  (define a (vector #\A 7 0 #f #f #f #f))
  (define b (vector #\B 7 0 #f #f #f #f))
  (check-false (cell-equal? a b)))

(test-case "cell-equal? returns #f for different fg"
  (define a (vector #\A 7 0 #f #f #f #f))
  (define b (vector #\A 31 0 #f #f #f #f))
  (check-false (cell-equal? a b)))

(test-case "cell-equal? returns #f for different bold"
  (define a (vector #\A 7 0 #t #f #f #f))
  (define b (vector #\A 7 0 #f #f #f #f))
  (check-false (cell-equal? a b)))

;; ============================================================
;; Row helpers
;; ============================================================

(test-case "cell-buffer-row-string extracts row as string"
  (define buf (make-cell-buffer 10 5))
  (cell-buffer-putstring! buf 0 2 "Hello")
  (check-equal? (cell-buffer-row-string buf 2) "Hello     "))

(test-case "cell-buffer-row-string for empty row"
  (define buf (make-cell-buffer 5 3))
  (check-equal? (cell-buffer-row-string buf 0) "     "))

;; ============================================================
;; Performance (smoke test)
;; ============================================================

(test-case "1000x80 buffer fill completes in reasonable time"
  (define buf (make-cell-buffer 80 1000))
  (define start (current-inexact-milliseconds))
  (for ([row (in-range 1000)])
    (cell-buffer-putstring! buf 0 row (make-string 80 #\X) #:fg 31))
  (define elapsed (- (current-inexact-milliseconds) start))
  ;; Should complete in well under 100ms
  (check-true (< elapsed 100.0) (format "Buffer fill took ~ams (expected < 100ms)" elapsed)))

;; ============================================================
;; Snapshot
;; ============================================================

(test-case "cell-buffer-snapshot creates independent copy"
  (define buf (make-cell-buffer 10 3))
  (cell-buffer-set! buf 0 0 #:char #\H #:fg 2)
  (cell-buffer-set! buf 1 0 #:char #\i #:fg 2)
  (cell-buffer-set! buf 5 1 #:char #\X #:bold #t)
  (define snap (cell-buffer-snapshot buf))
  ;; Dimensions match
  (check-equal? (cell-buffer-cols snap) 10)
  (check-equal? (cell-buffer-rows snap) 3)
  ;; Content matches
  (check-equal? (cell-char (cell-buffer-ref snap 0 0)) #\H)
  (check-equal? (cell-fg (cell-buffer-ref snap 0 0)) 2)
  (check-equal? (cell-bold? (cell-buffer-ref snap 5 1)) #t)
  ;; Snapshot is independent — mutating original does not affect snapshot
  (cell-buffer-set! buf 0 0 #:char #\Z)
  (check-equal? (cell-char (cell-buffer-ref snap 0 0)) #\H)
  (check-equal? (cell-char (cell-buffer-ref buf 0 0)) #\Z))

(test-case "cell-buffer-snapshot of empty buffer works"
  (define buf (make-cell-buffer 5 2))
  (define snap (cell-buffer-snapshot buf))
  (for* ([r (in-range 2)]
         [c (in-range 5)])
    (check-equal? (cell-char (cell-buffer-ref snap c r)) #\space)))

(test-case "snapshot of modified cell preserves all attributes"
  (define buf (make-cell-buffer 3 1))
  (cell-buffer-set! buf 1 0 #:char #\! #:fg 1 #:bg 4 #:bold #t #:underline #t #:italic #t #:blink #t)
  (define snap (cell-buffer-snapshot buf))
  (define cell (cell-buffer-ref snap 1 0))
  (check-equal? (cell-char cell) #\!)
  (check-equal? (cell-fg cell) 1)
  (check-equal? (cell-bg cell) 4)
  (check-true (cell-bold? cell))
  (check-true (cell-underline? cell))
  (check-true (cell-italic? cell))
  (check-true (cell-blink? cell)))
