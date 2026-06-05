#lang racket

;; tests/test-cell-width-alignment.rkt — Width-aware cell buffer alignment tests
;;
;; TDD RED gate for emoji/wide-char column misalignment fix.
;; These tests verify that cell-buffer-width-aware-putstring! correctly
;; aligns cell buffer positions with terminal display columns for width-2+ chars.

(require rackunit
         "../tui/cell-buffer.rkt"
         "../tui/cell-diff.rkt"
         "../tui/cell-diff-render.rkt"
         "../tui/char-width.rkt")

;; Continuation cell char used by width-aware-putstring
(define CONTINUATION-CHAR #\nul)

;; ============================================================
;; Part 1: char-width correctness (should already pass)
;; ============================================================

(test-case "char-width: emoji U+2705 has width 2"
  (check-equal? (char-width #\✅) 2))

(test-case "char-width: CJK char has width 2"
  (check-equal? (char-width #\日) 2)
  (check-equal? (char-width #\本) 2)
  (check-equal? (char-width #\語) 2))

(test-case "char-width: ASCII has width 1"
  (check-equal? (char-width #\A) 1)
  (check-equal? (char-width #\space) 1))

(test-case "char-width: combining mark has width 0"
  (check-equal? (char-width (integer->char #x0300)) 0))

;; ============================================================
;; Part 2: width-aware putstring alignment (RED gate)
;; ============================================================

(test-case "width-aware: emoji ✅ occupies 2 cells starting at col 0"
  (define buf (make-cell-buffer 20 1))
  (cell-buffer-width-aware-putstring! buf 0 0 "✅")
  ;; Cell 0 = ✅ (the actual char)
  (check-equal? (cell-char (cell-buffer-ref buf 0 0)) #\✅)
  ;; Cell 1 = continuation marker
  (check-equal? (cell-char (cell-buffer-ref buf 1 0)) CONTINUATION-CHAR)
  ;; Cell 2 = default (space)
  (check-equal? (cell-char (cell-buffer-ref buf 2 0)) #\space))

(test-case "width-aware: emoji + ASCII text aligns correctly"
  (define buf (make-cell-buffer 20 1))
  (cell-buffer-width-aware-putstring! buf 0 0 "✅ OK")
  ;; Col 0: ✅, Col 1: continuation, Col 2: space, Col 3: O, Col 4: K
  (check-equal? (cell-char (cell-buffer-ref buf 0 0)) #\✅)
  (check-equal? (cell-char (cell-buffer-ref buf 1 0)) CONTINUATION-CHAR)
  (check-equal? (cell-char (cell-buffer-ref buf 2 0)) #\space)
  (check-equal? (cell-char (cell-buffer-ref buf 3 0)) #\O)
  (check-equal? (cell-char (cell-buffer-ref buf 4 0)) #\K))

(test-case "width-aware: multiple emoji on same line"
  (define buf (make-cell-buffer 20 1))
  (cell-buffer-width-aware-putstring! buf 0 0 "✅✅✅")
  ;; Each ✅ takes 2 cells → 6 cells used
  (check-equal? (cell-char (cell-buffer-ref buf 0 0)) #\✅)
  (check-equal? (cell-char (cell-buffer-ref buf 1 0)) CONTINUATION-CHAR)
  (check-equal? (cell-char (cell-buffer-ref buf 2 0)) #\✅)
  (check-equal? (cell-char (cell-buffer-ref buf 3 0)) CONTINUATION-CHAR)
  (check-equal? (cell-char (cell-buffer-ref buf 4 0)) #\✅)
  (check-equal? (cell-char (cell-buffer-ref buf 5 0)) CONTINUATION-CHAR)
  ;; Cell 6 = default (space)
  (check-equal? (cell-char (cell-buffer-ref buf 6 0)) #\space))

(test-case "width-aware: CJK characters align correctly"
  (define buf (make-cell-buffer 20 1))
  (cell-buffer-width-aware-putstring! buf 0 0 "日本語")
  ;; 日 at cols 0-1, 本 at cols 2-3, 語 at cols 4-5
  (check-equal? (cell-char (cell-buffer-ref buf 0 0)) #\日)
  (check-equal? (cell-char (cell-buffer-ref buf 1 0)) CONTINUATION-CHAR)
  (check-equal? (cell-char (cell-buffer-ref buf 2 0)) #\本)
  (check-equal? (cell-char (cell-buffer-ref buf 3 0)) CONTINUATION-CHAR)
  (check-equal? (cell-char (cell-buffer-ref buf 4 0)) #\語)
  (check-equal? (cell-char (cell-buffer-ref buf 5 0)) CONTINUATION-CHAR))

(test-case "width-aware: emoji at non-zero start column"
  (define buf (make-cell-buffer 20 1))
  (cell-buffer-width-aware-putstring! buf 3 0 "✅")
  ;; Col 0-2: default space, Col 3: ✅, Col 4: continuation
  (check-equal? (cell-char (cell-buffer-ref buf 0 0)) #\space)
  (check-equal? (cell-char (cell-buffer-ref buf 2 0)) #\space)
  (check-equal? (cell-char (cell-buffer-ref buf 3 0)) #\✅)
  (check-equal? (cell-char (cell-buffer-ref buf 4 0)) CONTINUATION-CHAR))

(test-case "width-aware: respects buffer bounds — emoji at last column"
  (define buf (make-cell-buffer 5 1))
  (cell-buffer-width-aware-putstring! buf 4 0 "✅")
  ;; Col 4: ✅ (fits), but col 5 doesn't exist — continuation truncated
  (check-equal? (cell-char (cell-buffer-ref buf 4 0)) #\✅)
  ;; No crash from overflow
  )

(test-case "width-aware: preserves style attributes"
  (define buf (make-cell-buffer 20 1))
  (cell-buffer-width-aware-putstring! buf 0 0 "✅" #:fg 31 #:bold #t)
  (check-equal? (cell-fg (cell-buffer-ref buf 0 0)) 31)
  (check-true (cell-bold? (cell-buffer-ref buf 0 0)))
  ;; Continuation cell inherits same style
  (check-equal? (cell-fg (cell-buffer-ref buf 1 0)) 31)
  (check-true (cell-bold? (cell-buffer-ref buf 1 0))))

(test-case "width-aware: pure ASCII works same as regular putstring"
  (define buf-a (make-cell-buffer 20 1))
  (define buf-b (make-cell-buffer 20 1))
  (cell-buffer-putstring! buf-a 0 0 "Hello World")
  (cell-buffer-width-aware-putstring! buf-b 0 0 "Hello World")
  ;; Both should have identical cell contents for ASCII
  (for ([c (in-range 11)])
    (check-equal? (cell-char (cell-buffer-ref buf-a c 0)) (cell-char (cell-buffer-ref buf-b c 0)))))

;; ============================================================
;; Part 3: continuation-cell? predicate
;; ============================================================

(test-case "continuation-cell? recognizes #\\nul cells"
  (define cell (vector #\nul 7 0 #f #f #f #f))
  (check-true (continuation-cell? cell)))

(test-case "continuation-cell? returns #f for normal cells"
  (define cell (vector #\A 7 0 #f #f #f #f))
  (check-false (continuation-cell? cell)))

(test-case "continuation-cell? returns #f for space cells"
  (define cell (vector #\space 7 0 #f #f #f #f))
  (check-false (continuation-cell? cell)))

;; ============================================================
;; Part 4: diff treats continuation cells correctly
;; ============================================================

(test-case "diff: continuation cell equals its predecessor — no spurious delta"
  (define a (make-cell-buffer 20 1))
  (define b (make-cell-buffer 20 1))
  ;; Write "✅ OK" to both buffers using width-aware putstring
  (cell-buffer-width-aware-putstring! a 0 0 "✅ OK")
  (cell-buffer-width-aware-putstring! b 0 0 "✅ OK")
  ;; Diffs should be 0 — continuation cells match
  (define deltas (diff-cell-buffers a b))
  (check-equal? (length deltas) 0))

(test-case "diff: changing emoji produces delta for base cell only"
  (define a (make-cell-buffer 20 1))
  (define b (make-cell-buffer 20 1))
  (cell-buffer-width-aware-putstring! a 0 0 "✅ OK")
  (cell-buffer-width-aware-putstring! b 0 0 "❌ OK")
  ;; ✅→❌ changes col 0 base cell + col 1 continuation
  ;; The continuation at col 1 inherits from base, so both change
  (define deltas (diff-cell-buffers a b))
  ;; Should have deltas for col 0 (base) and col 1 (continuation inherits new base)
  (check-true (>= (length deltas) 1) (format "expected at least 1 delta, got ~a" (length deltas)))
  ;; But should NOT have deltas for cols 2-4 (space, O, K unchanged)
  (define changed-cols (map cell-delta-col deltas))
  (for ([c (in-list '(2 3 4))])
    (check-false (member c changed-cols)
                 (format "col ~a should not be in changed cols ~a" c changed-cols))))

;; ============================================================
;; Part 5: delta render skips continuation cells
;; ============================================================

(test-case "render-deltas: no cursor positioning for continuation cells"
  (define a (make-cell-buffer 20 1))
  (define b (make-cell-buffer 20 1))
  ;; Write text with emoji in b only
  (cell-buffer-width-aware-putstring! b 0 0 "✅ OK")
  (define deltas (diff-cell-buffers a b))
  (define out (open-output-string))
  (render-deltas-to-port! deltas b out #:sync? #f)
  (define result (get-output-string out))
  ;; Should NOT contain a cursor move to col 2 (1-indexed col=2 → position ";2H")
  ;; because col 1 (0-indexed) is a continuation cell that should be skipped
  ;; The cursor move "\x1b[1;1H" for col 0 is fine (base cell)
  ;; "\x1b[1;2H" for col 1 should NOT appear
  (check-false (string-contains? result "\x1b[1;2H")
               "continuation cell at col 1 should not get its own cursor move"))

(test-case "render-deltas: emoji renders as single character at correct position"
  (define a (make-cell-buffer 20 1))
  (define b (make-cell-buffer 20 1))
  (cell-buffer-width-aware-putstring! b 0 0 "✅")
  (define deltas (diff-cell-buffers a b))
  (define out (open-output-string))
  (render-deltas-to-port! deltas b out #:sync? #f)
  (define result (get-output-string out))
  ;; Should position cursor at col 0 (1;1H) and emit ✅
  (check-true (string-contains? result "\x1b[1;1H") "base cell should be positioned at col 0")
  ;; ✅ should appear in output (the actual emoji character)
  (check-true (string-contains? result "✅") "emoji character should be in output"))

(test-case "render-full: emoji with continuation renders correctly"
  (define buf (make-cell-buffer 10 1))
  (cell-buffer-width-aware-putstring! buf 0 0 "✅ OK")
  (define out (open-output-string))
  (render-buffer-to-port! buf out #:sync? #f)
  (define result (get-output-string out))
  ;; Full render should contain ✅ and OK
  (check-true (string-contains? result "✅"))
  (check-true (string-contains? result "OK")))

(test-case "render-smart: delta path with emoji skips continuation cells"
  (define a (make-cell-buffer 80 1))
  (define b (make-cell-buffer 80 1))
  (cell-buffer-width-aware-putstring! b 0 0 "\u2705 Phase 8 complete")
  (define deltas (diff-cell-buffers a b))
  ;; Should be below 50% threshold (20 changed cells out of 80 total)
  (check-true (< (length deltas) 40) (format "expected <40 deltas, got ~a" (length deltas)))
  (define out (open-output-string))
  (render-smart! a b out #:sync? #f)
  (define result (get-output-string out))
  ;; Should NOT be a full render (no home cursor)
  (check-false (string-contains? result "\x1b[H") "should use delta path, not full render")
  ;; Continuation cell at col 1 should not get cursor move
  (check-false (string-contains? result "\x1b[1;2H")
               "continuation cell should not get cursor move in delta path"))
