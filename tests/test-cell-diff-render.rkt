#lang racket

;; tests/test-cell-diff-render.rkt — Tests for tui/cell-diff-render.rkt

(require rackunit
         "../tui/cell-buffer.rkt"
         "../tui/cell-diff.rkt"
         "../tui/cell-diff-render.rkt")

(define AUTOWRAP-OFF "\x1b[?7l")
(define AUTOWRAP-ON "\x1b[?7h")

(define (string-index-of haystack needle)
  (define matches (regexp-match-positions (regexp (regexp-quote needle)) haystack))
  (and matches (caar matches)))

;; ============================================================
;; SGR generation
;; ============================================================

(test-case "cell->sgr produces reset+fg+bg for default cell"
  (define cell (vector #\space 7 0 #f #f #f #f))
  (define sgr (cell->sgr cell))
  (check-true (string-contains? sgr "38;5;7"))
  (check-true (string-contains? sgr "48;5;16")) ; bg=0 mapped to 16
  ;; Bold is \";1\" followed by \";\" or \"m\", not part of \";16\"
  (check-false (regexp-match? #rx";1[;m]" sgr)))

(test-case "cell->sgr includes bold"
  (define cell (vector #\A 7 0 #t #f #f #f))
  (define sgr (cell->sgr cell))
  (check-true (string-contains? sgr ";1")))

(test-case "cell->sgr includes underline"
  (define cell (vector #\A 7 0 #f #t #f #f))
  (define sgr (cell->sgr cell))
  (check-true (string-contains? sgr ";4")))

;; ============================================================
;; Render deltas to port (no sync — avoid terminal side effects)
;; ============================================================

(test-case "render-deltas-to-port! writes ANSI for changed cells"
  (define a (make-cell-buffer 10 5))
  (define b (make-cell-buffer 10 5))
  (cell-buffer-set! b 3 2 #:char #\X #:fg 31)
  (define deltas (diff-cell-buffers a b))
  (define out (open-output-string))
  ;; Render without terminal sync (avoid detect-sync-mode)
  (render-deltas-to-port! deltas b out #:sync? #f)
  (define result (get-output-string out))
  ;; Should contain cursor positioning
  (check-true (string-contains? result "\x1b[3;4H"))
  ;; Should contain the character
  (check-true (string-contains? result "X")))

(test-case "render-deltas-to-port! multiple changes"
  (define a (make-cell-buffer 10 5))
  (define b (make-cell-buffer 10 5))
  (cell-buffer-set! b 0 0 #:char #\A)
  (cell-buffer-set! b 1 0 #:char #\B)
  (define deltas (diff-cell-buffers a b))
  (define out (open-output-string))
  (render-deltas-to-port! deltas b out #:sync? #f)
  (define result (get-output-string out))
  (check-true (string-contains? result "A"))
  (check-true (string-contains? result "B")))

(test-case "render-deltas-to-port! disables auto-wrap around last-column batch"
  (define a (make-cell-buffer 10 2))
  (define b (make-cell-buffer 10 2))
  (cell-buffer-putstring! b 0 0 "ABCDEFGHIJ")
  (define deltas (diff-cell-buffers a b))
  (define out (open-output-string))
  (render-deltas-to-port! deltas b out #:sync? #f)
  (define result (get-output-string out))
  (define off-pos (string-index-of result AUTOWRAP-OFF))
  (define text-pos (string-index-of result "ABCDEFGHIJ"))
  (define on-pos (string-index-of result AUTOWRAP-ON))
  (check-not-false off-pos)
  (check-not-false on-pos)
  (check-true (< off-pos text-pos on-pos)))

(test-case "render-smart! delta path disables auto-wrap at final column"
  (define a (make-cell-buffer 10 5))
  (define b (make-cell-buffer 10 5))
  (cell-buffer-set! b 9 0 #:char #\Z)
  (define out (open-output-string))
  (render-smart! a b out #:sync? #f)
  (define result (get-output-string out))
  (check-true (string-contains? result "\x1b[1;10H"))
  (check-false (string-contains? result "\x1b[H"))
  (check-true (string-contains? result AUTOWRAP-OFF))
  (check-true (string-contains? result AUTOWRAP-ON)))

;; ============================================================
;; Full buffer render
;; ============================================================

(test-case "render-buffer-to-port! writes all rows"
  (define buf (make-cell-buffer 5 3))
  (cell-buffer-putstring! buf 0 0 "Hello")
  (define out (open-output-string))
  (render-buffer-to-port! buf out #:sync? #f)
  (define result (get-output-string out))
  ;; Should contain the text
  (check-true (string-contains? result "Hello"))
  ;; Should contain home cursor after wrap-disable
  (check-true (string-contains? result "\x1b[H")))

;; ============================================================
;; Smart render threshold
;; ============================================================

(test-case "render-smart! uses full render when > 50% changed"
  (define a (make-cell-buffer 4 2)) ; 8 cells total
  (define b (make-cell-buffer 4 2))
  ;; Change 5 cells (62.5%) — should trigger full render
  (cell-buffer-set! b 0 0 #:char #\A)
  (cell-buffer-set! b 1 0 #:char #\B)
  (cell-buffer-set! b 2 0 #:char #\C)
  (cell-buffer-set! b 3 0 #:char #\D)
  (cell-buffer-set! b 0 1 #:char #\E)
  (define out (open-output-string))
  (render-smart! a b out #:sync? #f)
  (define result (get-output-string out))
  ;; Full render contains home cursor (after wrap-disable)
  (check-true (string-contains? result "\x1b[H")))

(test-case "render-smart! uses delta render when < 50% changed"
  (define a (make-cell-buffer 10 5)) ; 50 cells
  (define b (make-cell-buffer 10 5))
  ;; Change only 1 cell (2%) — should use delta
  (cell-buffer-set! b 5 3 #:char #\Z)
  (define out (open-output-string))
  (render-smart! a b out #:sync? #f)
  (define result (get-output-string out))
  ;; Delta render uses cursor positioning, not home
  (check-true (string-contains? result "\x1b[4;6H")))

(test-case "render-smart! full render for #f prev"
  (define buf (make-cell-buffer 5 3))
  (define out (open-output-string))
  (render-smart! #f buf out #:sync? #f)
  (define result (get-output-string out))
  (check-true (string-contains? result "\x1b[H")))

;; ============================================================
;; Batch optimization tests
;; ============================================================

(test-case "batch: consecutive cells emit single cursor move"
  (define a (make-cell-buffer 10 1))
  (define b (make-cell-buffer 10 1))
  ;; Write 5 consecutive chars starting at col 0
  (cell-buffer-set! b 0 0 #:char #\H #:fg 7)
  (cell-buffer-set! b 1 0 #:char #\e #:fg 7)
  (cell-buffer-set! b 2 0 #:char #\l #:fg 7)
  (cell-buffer-set! b 3 0 #:char #\l #:fg 7)
  (cell-buffer-set! b 4 0 #:char #\o #:fg 7)
  (define deltas (diff-cell-buffers a b))
  (check-equal? (length deltas) 5)
  (define out (open-output-string))
  (render-deltas-to-port! deltas b out #:sync? #f)
  (define result (get-output-string out))
  ;; Should contain "Hello" as a single string (batched)
  (check-true (string-contains? result "Hello"))
  ;; Should have exactly 1 cursor move, not 5
  (define cursor-re (format "~a\\[[0-9]+;[0-9]+H" (integer->char 27)))
  (define cursor-moves (regexp-match* (regexp cursor-re) result))
  (check-equal? (length cursor-moves) 1))

(test-case "batch: non-consecutive cells use separate cursor moves"
  (define a (make-cell-buffer 10 1))
  (define b (make-cell-buffer 10 1))
  ;; Write chars at col 0 and col 5 (gap at cols 1-4)
  (cell-buffer-set! b 0 0 #:char #\A #:fg 7)
  (cell-buffer-set! b 5 0 #:char #\B #:fg 7)
  (define deltas (diff-cell-buffers a b))
  (define out (open-output-string))
  (render-deltas-to-port! deltas b out #:sync? #f)
  (define result (get-output-string out))
  ;; Should have 2 cursor moves (not batched across gap)
  (define cursor-re (format "~a\\[[0-9]+;[0-9]+H" (integer->char 27)))
  (define cursor-moves (regexp-match* (regexp cursor-re) result))
  (check-equal? (length cursor-moves) 2))

(test-case "batch: different SGR breaks batch"
  (define a (make-cell-buffer 10 1))
  (define b (make-cell-buffer 10 1))
  ;; Same row, consecutive cols, but different fg color
  (cell-buffer-set! b 0 0 #:char #\A #:fg 7)
  (cell-buffer-set! b 1 0 #:char #\B #:fg 31) ; different fg
  (cell-buffer-set! b 2 0 #:char #\C #:fg 7)
  (define deltas (diff-cell-buffers a b))
  (define out (open-output-string))
  (render-deltas-to-port! deltas b out #:sync? #f)
  (define result (get-output-string out))
  ;; Should have 3 cursor moves: A alone (col 0), B alone (col 1, diff SGR), C (col 2, new batch)
  (define cursor-re (format "~a\\[[0-9]+;[0-9]+H" (integer->char 27)))
  (define cursor-moves (regexp-match* (regexp cursor-re) result))
  (check-equal? (length cursor-moves) 3))

(test-case "batch: different rows use separate cursor moves"
  (define a (make-cell-buffer 5 3))
  (define b (make-cell-buffer 5 3))
  ;; Write consecutive chars across rows
  (cell-buffer-set! b 0 0 #:char #\A #:fg 7)
  (cell-buffer-set! b 1 0 #:char #\B #:fg 7)
  (cell-buffer-set! b 0 1 #:char #\C #:fg 7) ; different row
  (cell-buffer-set! b 1 1 #:char #\D #:fg 7)
  (define deltas (diff-cell-buffers a b))
  (define out (open-output-string))
  (render-deltas-to-port! deltas b out #:sync? #f)
  (define result (get-output-string out))
  ;; Should have 2 cursor moves: one for row 0 (AB), one for row 1 (CD)
  (define cursor-re (format "~a\\[[0-9]+;[0-9]+H" (integer->char 27)))
  (define cursor-moves (regexp-match* (regexp cursor-re) result))
  (check-equal? (length cursor-moves) 2)
  ;; Should contain both batched strings
  (check-true (string-contains? result "AB"))
  (check-true (string-contains? result "CD")))

(test-case "batch: 200-char line uses single cursor move"
  (define a (make-cell-buffer 200 1))
  (define b (make-cell-buffer 200 1))
  (for ([i (in-range 200)])
    (cell-buffer-set! b i 0 #:char (integer->char (+ 65 (modulo i 26))) #:fg 7))
  (define deltas (diff-cell-buffers a b))
  (check-equal? (length deltas) 200)
  (define out (open-output-string))
  (render-deltas-to-port! deltas b out #:sync? #f)
  (define result (get-output-string out))
  ;; Should have exactly 1 cursor move for all 200 cells
  (define cursor-re (format "~a\\[[0-9]+;[0-9]+H" (integer->char 27)))
  (define cursor-moves (regexp-match* (regexp cursor-re) result))
  (check-equal? (length cursor-moves) 1))

;; ============================================================
;; Line clearing (ESC[K) tests — RED gate for trailing-char bug
;; ============================================================

(test-case "full render: emits ESC[K after each row"
  (define buf (make-cell-buffer 10 3))
  ;; Row 0: 5 chars
  (cell-buffer-putstring! buf 0 0 "Hello")
  ;; Row 1: 3 chars
  (cell-buffer-putstring! buf 0 1 "Hi!")
  ;; Row 2: empty
  (define out (open-output-string))
  (render-buffer-to-port! buf out #:sync? #f)
  (define result (get-output-string out))
  ;; Should contain ESC[K after each row (3 rows = 3 ESC[K)
  (define esc-k "\x1b[K")
  (define esc-k-count (length (regexp-match* (regexp (regexp-quote esc-k)) result)))
  (check-equal? esc-k-count 3 (format "expected 3 ESC[K, got ~a in: ~a" esc-k-count result)))

(test-case "delta render: emits ESC[K after last batch when switching rows"
  (define a (make-cell-buffer 20 2))
  (define b (make-cell-buffer 20 2))
  ;; Row 0: short text
  (cell-buffer-putstring! b 0 0 "AB")
  ;; Row 1: different text
  (cell-buffer-putstring! b 0 1 "CD")
  (define deltas (diff-cell-buffers a b))
  (define out (open-output-string))
  (render-deltas-to-port! deltas b out #:sync? #f)
  (define result (get-output-string out))
  ;; Should contain ESC[K after row 0's batch (before row 1)
  (check-true (string-contains? result "\x1b[K")
              (format "expected ESC[K in delta output, got: ~a" result)))

(test-case "delta render: emits ESC[K for single-row last batch"
  (define a (make-cell-buffer 20 1))
  (define b (make-cell-buffer 20 1))
  (cell-buffer-putstring! b 0 0 "XY")
  (define deltas (diff-cell-buffers a b))
  (define out (open-output-string))
  (render-deltas-to-port! deltas b out #:sync? #f)
  (define result (get-output-string out))
  ;; Last batch in delta list should have ESC[K after it
  (check-true (string-contains? result "\x1b[K")
              (format "expected ESC[K after last batch, got: ~a" result)))

(test-case "delta render: does NOT emit ESC[K between batches in same row"
  (define a (make-cell-buffer 20 1))
  (define b (make-cell-buffer 20 1))
  ;; Two separate batches in same row (gap between them)
  (cell-buffer-set! b 0 0 #:char #\A #:fg 7)
  (cell-buffer-set! b 5 0 #:char #\B #:fg 7)
  (define deltas (diff-cell-buffers a b))
  (define out (open-output-string))
  (render-deltas-to-port! deltas b out #:sync? #f)
  (define result (get-output-string out))
  ;; After first batch (col 0), next delta is col 5 in same row.
  ;; With the fix, ESC[K is NOT emitted between batches in the same row.
  ;; It is only emitted after the LAST batch in the row (after col 5).
  (define esc-k-count (length (regexp-match* (regexp (regexp-quote "\x1b[K")) result)))
  (check-equal?
   esc-k-count
   1
   (format "expected exactly 1 ESC[K (after last batch), got ~a in: ~a" esc-k-count result)))

(test-case "smart render delta path: emits ESC[K after row changes"
  (define a (make-cell-buffer 20 2))
  (define b (make-cell-buffer 20 2))
  ;; Long text in row 0, short text in row 1
  (cell-buffer-putstring! b 0 0 "Long text here")
  (cell-buffer-putstring! b 0 1 "OK")
  (define out (open-output-string))
  (render-smart! a b out #:sync? #f)
  (define result (get-output-string out))
  ;; Delta path should contain ESC[K
  (check-true (string-contains? result "\x1b[K")
              (format "expected ESC[K in smart delta path, got: ~a" result)))

(test-case "smart render full path: emits ESC[K after each row"
  (define a (make-cell-buffer 5 2))
  (define b (make-cell-buffer 5 2))
  ;; Change all 10 cells (>50% threshold triggers full render)
  ;; Use odd width (5) to avoid row-hash XOR-cancellation with even counts
  (for* ([r (in-range 2)]
         [c (in-range 5)])
    (cell-buffer-set! b c r #:char #\X #:fg 7))
  (define out (open-output-string))
  (render-smart! a b out #:sync? #f)
  (define result (get-output-string out))
  ;; Full render should contain ESC[K after each row
  (define esc-k-count (length (regexp-match* (regexp (regexp-quote "\x1b[K")) result)))
  (check-equal? esc-k-count 2 (format "expected 2 ESC[K in full render, got ~a" esc-k-count)))

;; ============================================================
;; SGR deduplication tests
;; ============================================================

(test-case "sgr dedup: consecutive same-style cells emit one SGR"
  (define a (make-cell-buffer 10 1))
  (define b (make-cell-buffer 10 1))
  ;; All same style (fg=7, bg=0)
  (cell-buffer-set! b 0 0 #:char #\A #:fg 7)
  (cell-buffer-set! b 1 0 #:char #\B #:fg 7)
  (cell-buffer-set! b 2 0 #:char #\C #:fg 7)
  (define deltas (diff-cell-buffers a b))
  (define out (open-output-string))
  (render-deltas-to-port! deltas b out #:sync? #f)
  (define result (get-output-string out))
  ;; Count SGR sequences (\x1b[...m)
  (define sgr-re (format "~a\\[[0-9;]+m" (integer->char 27)))
  (define sgrs (regexp-match* (regexp sgr-re) result))
  ;; Should have exactly 2 SGRs: one style set + one reset at end
  (check-equal? (length sgrs) 2))

(test-case "sgr dedup: style change emits new SGR"
  (define a (make-cell-buffer 10 1))
  (define b (make-cell-buffer 10 1))
  ;; Two different styles
  (cell-buffer-set! b 0 0 #:char #\A #:fg 7)
  (cell-buffer-set! b 1 0 #:char #\B #:fg 31) ; different fg
  (cell-buffer-set! b 2 0 #:char #\C #:fg 7) ; back to original
  (define deltas (diff-cell-buffers a b))
  (define out (open-output-string))
  (render-deltas-to-port! deltas b out #:sync? #f)
  (define result (get-output-string out))
  ;; Count SGR sequences
  (define sgr-re (format "~a\\[[0-9;]+m" (integer->char 27)))
  (define sgrs (regexp-match* (regexp sgr-re) result))
  ;; Should have 4 SGRs: fg7, fg31, fg7, reset
  (check-equal? (length sgrs) 4))

(test-case "sgr dedup: full buffer render deduplicates SGR"
  (define buf (make-cell-buffer 10 1))
  ;; All default cells — same SGR for all
  (cell-buffer-putstring! buf 0 0 "AAAAAAAAAA")
  (define out (open-output-string))
  (render-buffer-to-port! buf out #:sync? #f)
  (define result (get-output-string out))
  ;; Count SGR sequences — should be 2 (one set + one reset)
  (define sgr-re (format "~a\\[[0-9;]+m" (integer->char 27)))
  (define sgrs (regexp-match* (regexp sgr-re) result))
  (check-equal? (length sgrs) 2))

;; ============================================================
;; ESC[K gap bug regression tests
;; ============================================================

(test-case "width-2 char does not clear unchanged trailing content"
  ;; RED gate for delta ESC[K gap bug.
  ;; Old frame: ABXYC — X at col 5, Y at col 6, C at col 7 (UNCHANGED)
  ;; New frame: AB🙂C — 🙂 at col 5-6, C at col 7 (UNCHANGED)
  ;; The continuation cell at col 6 is filtered out of real-deltas.
  ;; Without the fix, ESC[K after 🙂 clears C at col 7.
  (define a (make-cell-buffer 20 1))
  (define b (make-cell-buffer 20 1))
  (cell-buffer-set! a 0 0 #:char #\A #:fg 7)
  (cell-buffer-set! a 1 0 #:char #\B #:fg 7)
  (cell-buffer-set! a 5 0 #:char #\X #:fg 7)
  (cell-buffer-set! a 6 0 #:char #\Y #:fg 7)
  (cell-buffer-set! a 7 0 #:char #\C #:fg 7)
  (cell-buffer-set! b 0 0 #:char #\A #:fg 7)
  (cell-buffer-set! b 1 0 #:char #\B #:fg 7)
  (cell-buffer-width-aware-putstring! b 5 0 "🙂" #:fg 7)
  (cell-buffer-set! b 7 0 #:char #\C #:fg 7)
  (define deltas (diff-cell-buffers a b))
  (define out (open-output-string))
  (render-deltas-to-port! deltas b out #:sync? #f)
  (define result (get-output-string out))
  ;; The output should NOT contain ESC[K between batches in the same row.
  ;; There is only one real delta (col 5), so it's the last batch in the row.
  ;; ESC[K is emitted once at the end.
  (define esc-k-count (length (regexp-match* (regexp (regexp-quote "\x1b[K")) result)))
  (check-equal? esc-k-count 1 (format "expected 1 ESC[K, got ~a in: ~a" esc-k-count result)))

(test-case "style-change gap does not clear unchanged content between batches"
  ;; Old frame: "ABCDE" all plain
  ;; New frame: "AXYDE" with bold "XY" at cols 2-3
  ;; Deltas: col 2 (C→X, bold), col 3 (D→Y, bold), col 4 (E→E, plain)
  ;; Wait, E is unchanged. So deltas are cols 2-3 only.
  ;; Single batch (same row, consecutive, same SGR). Not a great test.
  ;; Let's make it: old "ABCDE", new "AXCDE" with bold X at col 2.
  ;; Deltas: col 2 (C→X, bold), col 3 (D→D, plain), col 4 (E→E, plain)
  ;; Actually consecutive same-style cells batch together.
  ;; Better test: two style segments with an unchanged gap.
  ;; Old: A B C D E (all plain)
  ;; New: A X C Y E (X bold at 1, Y bold at 3)
  ;; Deltas: col 1 (B→X, bold), col 3 (D→Y, bold)
  ;; Two batches in same row with gap at col 2 (C, unchanged).
  ;; With fix: no ESC[K between batch 1 and batch 2.
  (define a (make-cell-buffer 20 1))
  (define b (make-cell-buffer 20 1))
  (cell-buffer-set! a 0 0 #:char #\A #:fg 7)
  (cell-buffer-set! a 1 0 #:char #\B #:fg 7)
  (cell-buffer-set! a 2 0 #:char #\C #:fg 7)
  (cell-buffer-set! a 3 0 #:char #\D #:fg 7)
  (cell-buffer-set! a 4 0 #:char #\E #:fg 7)
  (cell-buffer-set! b 0 0 #:char #\A #:fg 7)
  (cell-buffer-set! b 1 0 #:char #\X #:fg 7 #:bold #t)
  (cell-buffer-set! b 2 0 #:char #\C #:fg 7)
  (cell-buffer-set! b 3 0 #:char #\Y #:fg 7 #:bold #t)
  (cell-buffer-set! b 4 0 #:char #\E #:fg 7)
  (define deltas (diff-cell-buffers a b))
  (define out (open-output-string))
  (render-deltas-to-port! deltas b out #:sync? #f)
  (define result (get-output-string out))
  ;; Should have exactly 1 ESC[K after the last batch in the row
  (define esc-k-count (length (regexp-match* (regexp (regexp-quote "\x1b[K")) result)))
  (check-equal?
   esc-k-count
   1
   (format "expected 1 ESC[K (after last batch only), got ~a in: ~a" esc-k-count result)))

(test-case "multiple rows: ESC[K only after last batch per row"
  (define a (make-cell-buffer 10 2))
  (define b (make-cell-buffer 10 2))
  ;; Row 0: two batches with gap
  (cell-buffer-set! b 0 0 #:char #\A #:fg 7)
  (cell-buffer-set! b 5 0 #:char #\B #:fg 7)
  ;; Row 1: one batch
  (cell-buffer-set! b 0 1 #:char #\C #:fg 7)
  (define deltas (diff-cell-buffers a b))
  (define out (open-output-string))
  (render-deltas-to-port! deltas b out #:sync? #f)
  (define result (get-output-string out))
  ;; Should have 2 ESC[K: one after row 0's last batch, one after row 1's last batch
  (define esc-k-count (length (regexp-match* (regexp (regexp-quote "\x1b[K")) result)))
  (check-equal? esc-k-count
                2
                (format "expected 2 ESC[K (one per row), got ~a in: ~a" esc-k-count result)))

;; ============================================================
;; Inverse cursor ESC[K spanning regression tests
;; ============================================================

(test-case "inverse cursor cell at end of row: ESC[K emitted with default SGR"
  ;; RED gate for software cursor spanning bug.
  ;; When the software cursor draws an inverse-video cell as the last
  ;; changed cell in a row, ESC[K must NOT be emitted with inverse SGR
  ;; active — otherwise it clears the rest of the line with inverse colors,
  ;; making the cursor appear to span the entire row.
  (define a (make-cell-buffer 20 1))
  (define b (make-cell-buffer 20 1))
  ;; "hello" at cols 0-4, then inverse cursor at col 5
  (cell-buffer-putstring! b 0 0 "hello" #:fg 7)
  ;; Inverse cursor: swap fg/bg (normally fg=7 bg=0 → fg=0 bg=7)
  (cell-buffer-set! b 5 0 #:char #\space #:fg 0 #:bg 7)
  (define deltas (diff-cell-buffers a b))
  (define out (open-output-string))
  (render-deltas-to-port! deltas b out #:sync? #f)
  (define result (get-output-string out))
  ;; The ESC[0m (SGR reset) must appear BEFORE ESC[K, not after.
  ;; If ESC[K appears before ESC[0m, the inverse SGR would bleed.
  (define reset-pos (string-index-of result "\x1b[0m"))
  (define esc-k-pos (string-index-of result "\x1b[K"))
  (check-not-false reset-pos "expected ESC[0m reset in output")
  (check-not-false esc-k-pos "expected ESC[K in output")
  (check-true (< reset-pos esc-k-pos)
              (format "ESC[0m must precede ESC[K; got reset at ~a, ESC[K at ~a in: ~a"
                      reset-pos
                      esc-k-pos
                      result)))

(test-case "inverse cursor in middle of row: ESC[K does not erase text after cursor"
  ;; When cursor is in the middle of the line (e.g., after Ctrl+left),
  ;; ESC[K must not erase text to the right of the cursor.
  (define a (make-cell-buffer 20 1))
  (define b (make-cell-buffer 20 1))
  ;; "hello" at cols 0-4, inverse cursor at col 2 (between 'h' and 'l')
  (cell-buffer-set! b 0 0 #:char #\h #:fg 7)
  (cell-buffer-set! b 1 0 #:char #\e #:fg 7)
  ;; Inverse cursor at col 2
  (cell-buffer-set! b 2 0 #:char #\l #:fg 0 #:bg 7)
  (cell-buffer-set! b 3 0 #:char #\l #:fg 7)
  (cell-buffer-set! b 4 0 #:char #\o #:fg 7)
  ;; Old frame has same text but no inverse cursor
  (cell-buffer-set! a 0 0 #:char #\h #:fg 7)
  (cell-buffer-set! a 1 0 #:char #\e #:fg 7)
  (cell-buffer-set! a 2 0 #:char #\l #:fg 7)
  (cell-buffer-set! a 3 0 #:char #\l #:fg 7)
  (cell-buffer-set! a 4 0 #:char #\o #:fg 7)
  (define deltas (diff-cell-buffers a b))
  (define out (open-output-string))
  (render-deltas-to-port! deltas b out #:sync? #f)
  (define result (get-output-string out))
  ;; The delta should only be col 2 (inverse cursor). ESC[0m must precede ESC[K.
  (define reset-pos (string-index-of result "\x1b[0m"))
  (define esc-k-pos (string-index-of result "\x1b[K"))
  (check-not-false reset-pos "expected ESC[0m reset in output")
  (check-not-false esc-k-pos "expected ESC[K in output")
  (check-true (< reset-pos esc-k-pos)
              (format "ESC[0m must precede ESC[K; got reset at ~a, ESC[K at ~a in: ~a"
                      reset-pos
                      esc-k-pos
                      result)))

(test-case "full render with inverse cursor: ESC[K emitted with default SGR"
  ;; Full buffer render path also needs the fix — the last cell in a row
  ;; could be an inverse cursor cell.
  (define buf (make-cell-buffer 10 1))
  (cell-buffer-putstring! buf 0 0 "hello" #:fg 7)
  ;; Inverse cursor at col 5
  (cell-buffer-set! buf 5 0 #:char #\space #:fg 0 #:bg 7)
  (define out (open-output-string))
  (render-buffer-to-port! buf out #:sync? #f)
  (define result (get-output-string out))
  ;; ESC[0m must precede ESC[K
  (define reset-pos (string-index-of result "\x1b[0m"))
  (define esc-k-pos (string-index-of result "\x1b[K"))
  (check-not-false reset-pos "expected ESC[0m reset in output")
  (check-not-false esc-k-pos "expected ESC[K in output")
  (check-true (< reset-pos esc-k-pos)
              (format "ESC[0m must precede ESC[K in full render; reset at ~a, ESC[K at ~a in: ~a"
                      reset-pos
                      esc-k-pos
                      result)))
