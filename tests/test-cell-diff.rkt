#lang racket

;; @speed fast
;; @suite default

;; tests/test-cell-diff.rkt — Tests for tui/cell-diff.rkt

(require rackunit
         "../tui/cell-buffer.rkt"
         "../tui/cell-diff.rkt")

;; ============================================================
;; Row hashing
;; ============================================================

(test-case "row-hash returns fixnum"
  (define buf (make-cell-buffer 10 5))
  (check-true (integer? (row-hash buf 0))))

(test-case "row-hash is consistent"
  (define buf (make-cell-buffer 10 5))
  (check-equal? (row-hash buf 0) (row-hash buf 0)))

(test-case "row-hash changes when cell changes"
  (define buf (make-cell-buffer 10 5))
  (define h1 (row-hash buf 0))
  (cell-buffer-set! buf 3 0 #:char #\X)
  (define h2 (row-hash buf 0))
  (check-not-equal? h1 h2))

(test-case "row-hash same for identical rows"
  (define a (make-cell-buffer 10 5))
  (define b (make-cell-buffer 10 5))
  (check-equal? (row-hash a 0) (row-hash b 0)))

(test-case "row-hash: even-width uniform row does NOT cancel to 0"
  ;; XOR-cancellation bug: an even number of identical values XOR'd together = 0.
  ;; This caused diff to skip rows when all cells were identical (common case).
  (define a (make-cell-buffer 4 1))
  (define b (make-cell-buffer 4 1))
  ;; Buffer A: all spaces (default). Buffer B: all 'X's (completely different!).
  (for ([c (in-range 4)])
    (cell-buffer-set! b c 0 #:char #\X))
  (define h-a (row-hash a 0))
  (define h-b (row-hash b 0))
  ;; Must detect the difference — hashes must differ
  (check-not-equal? h-a h-b (format "row-hash collision: both rows hash to ~a" h-a)))

(test-case "diff: even-width uniform row change is detected"
  ;; Regression: row-hash collision caused diff to miss entire row changes
  (define a (make-cell-buffer 4 1))
  (define b (make-cell-buffer 4 1))
  (for ([c (in-range 4)])
    (cell-buffer-set! b c 0 #:char #\X))
  (define deltas (diff-cell-buffers a b))
  ;; All 4 cells changed — diff MUST detect them
  (check-equal? (length deltas)
                4
                (format "expected 4 deltas, got ~a (row-hash collision?)" (length deltas))))

;; ============================================================
;; Diff — identical buffers
;; ============================================================

(test-case "diff identical buffers returns empty list"
  (define a (make-cell-buffer 10 5))
  (define b (make-cell-buffer 10 5))
  (check-equal? (diff-cell-buffers a b) '()))

;; ============================================================
;; Diff — single cell change
;; ============================================================

(test-case "diff detects single cell change"
  (define a (make-cell-buffer 10 5))
  (define b (make-cell-buffer 10 5))
  (cell-buffer-set! b 3 2 #:char #\Z)
  (define deltas (diff-cell-buffers a b))
  (check-equal? (length deltas) 1)
  (define d (car deltas))
  (check-equal? (cell-delta-col d) 3)
  (check-equal? (cell-delta-row d) 2)
  (check-equal? (cell-char (cell-delta-new-cell d)) #\Z))

;; ============================================================
;; Diff — row change
;; ============================================================

(test-case "diff detects multiple cells in same row"
  (define a (make-cell-buffer 10 5))
  (define b (make-cell-buffer 10 5))
  (cell-buffer-set! b 0 0 #:char #\A)
  (cell-buffer-set! b 1 0 #:char #\B)
  (cell-buffer-set! b 2 0 #:char #\C)
  (define deltas (diff-cell-buffers a b))
  (check-equal? (length deltas) 3)
  (check-equal? (delta-changed-rows deltas) 1))

;; ============================================================
;; Diff — multiple rows
;; ============================================================

(test-case "diff detects changes across multiple rows"
  (define a (make-cell-buffer 10 5))
  (define b (make-cell-buffer 10 5))
  (cell-buffer-set! b 0 0 #:char #\A)
  (cell-buffer-set! b 0 3 #:char #\B)
  (define deltas (diff-cell-buffers a b))
  (check-equal? (delta-changed-rows deltas) 2))

;; ============================================================
;; Diff — no previous buffer (full diff)
;; ============================================================

(test-case "diff with #f prev returns all cells"
  (define buf (make-cell-buffer 5 3))
  (define deltas (diff-cell-buffers #f buf))
  (check-equal? (length deltas) (* 5 3)))

;; ============================================================
;; Diff — different dimensions
;; ============================================================

(test-case "diff with different dimensions returns full diff"
  (define a (make-cell-buffer 5 3))
  (define b (make-cell-buffer 5 4))
  (define deltas (diff-cell-buffers a b))
  ;; Should have 5*4 = 20 deltas (all cells of new buffer)
  (check-equal? (length deltas) (* 5 4)))

;; ============================================================
;; Diff — attribute changes
;; ============================================================

(test-case "diff detects fg color change"
  (define a (make-cell-buffer 10 5))
  (define b (make-cell-buffer 10 5))
  (cell-buffer-set! b 0 0 #:fg 31)
  (define deltas (diff-cell-buffers a b))
  (check-equal? (length deltas) 1)
  (check-equal? (cell-fg (cell-delta-new-cell (car deltas))) 31))

(test-case "diff detects bold change"
  (define a (make-cell-buffer 10 5))
  (define b (make-cell-buffer 10 5))
  (cell-buffer-set! b 5 3 #:bold #t)
  (define deltas (diff-cell-buffers a b))
  (check-equal? (length deltas) 1)
  (check-true (cell-bold? (cell-delta-new-cell (car deltas)))))

;; ============================================================
;; Delta statistics
;; ============================================================

(test-case "delta-count returns correct count"
  (define a (make-cell-buffer 10 5))
  (define b (make-cell-buffer 10 5))
  (cell-buffer-set! b 0 0 #:char #\A)
  (cell-buffer-set! b 1 0 #:char #\B)
  (define deltas (diff-cell-buffers a b))
  (check-equal? (delta-count deltas) 2))

(test-case "delta-full-redraw? detects full redraw"
  (define buf (make-cell-buffer 5 3))
  (define deltas (diff-cell-buffers #f buf))
  (check-true (delta-full-redraw? deltas 5 3)))

(test-case "delta-full-redraw? returns #f for partial diff"
  (define a (make-cell-buffer 10 5))
  (define b (make-cell-buffer 10 5))
  (cell-buffer-set! b 0 0 #:char #\A)
  (define deltas (diff-cell-buffers a b))
  (check-false (delta-full-redraw? deltas 10 5)))
