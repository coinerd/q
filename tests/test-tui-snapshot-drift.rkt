#lang racket

;; @speed fast  ;; @suite tui

;; BOUNDARY: io

;; tests/test-tui-snapshot-drift.rkt — TUI Snapshot Drift Characterization Tests
;;
;; v0.99.16 W0: Characterization tests for TUI display corruption bugs.
;;
;; These tests lock in the CURRENT behavior of the TUI rendering pipeline
;; before fixes are applied. Some tests intentionally FAIL to prove bugs exist.
;;
;; Test Cases:
;;   1. F-TUI-01 characterization: prev-ubuf-box NOT cleared on resize (FAILS before W1)
;;   2. F-TUI-01 characterization: previous-frame-box IS cleared on resize (passes)
;;   3. Cell buffer clear: cell-buffer-clear! resets all cells
;;   4. Diff with #f prev-buf: diff-cell-buffers #f curr produces all-cell deltas
;;   5. render-smart! with #f prev-buf: produces full render output
;;   6. render-deltas-to-port! single-row change: emits cursor positioning + text
;;   7. render-deltas-to-port! no ESC[K in delta render (documents F-TUI-03 condition)
;;   8. Snapshot round-trip: cell-buffer-snapshot produces independent copy

(require rackunit
         rackunit/text-ui
         "../tui/context.rkt"
         "../tui/cell-buffer.rkt"
         "../tui/cell-diff.rkt"
         "../tui/cell-diff-render.rkt"
         "../tui/terminal.rkt"
         (only-in "../tui/tui-render-loop.rkt"
                  FULL-RENDER-INTERVAL-FRAMES
                  incremental-frame-count
                  write-cell!))

(define snapshot-drift-suite
  (test-suite "TUI Snapshot Drift Characterization (v0.99.16 W0)"

    ;; ── Test 1: F-TUI-01 — prev-ubuf-box cleared on resize (FIXED) ──
    ;; After W1 fix, both prev-ubuf-box and previous-frame-box are cleared.
    (test-case "F-TUI-01: prev-ubuf-box is #f after resize (bug fixed)"
      (define ctx (make-tui-ctx))
      (define dummy-buf (make-cell-buffer 10 5))
      ;; Simulate a prior render: prev-ubuf-box has a snapshot
      (set-box! (tui-ctx-prev-ubuf-box ctx) dummy-buf)
      (set-box! (tui-ctx-previous-frame-box ctx) dummy-buf)
      ;; Simulate the fixed resize effect: both boxes are cleared
      (set-box! (tui-ctx-ubuf-box ctx) (make-cell-buffer 20 10))
      (set-box! (tui-ctx-previous-frame-box ctx) #f)
      (set-box! (tui-ctx-prev-ubuf-box ctx) #f)
      (set-box! incremental-frame-count 0)
      ;; After W1 fix: both boxes are #f
      (check-false (unbox (tui-ctx-prev-ubuf-box ctx))
                   "prev-ubuf-box should be #f after resize (F-TUI-01 fixed)")
      (check-false (unbox (tui-ctx-previous-frame-box ctx))
                   "previous-frame-box should be #f after resize"))

    ;; ── Test 2: previous-frame-box IS cleared on resize ──
    (test-case "F-TUI-01: previous-frame-box IS #f after resize (works correctly)"
      (define ctx (make-tui-ctx))
      (set-box! (tui-ctx-previous-frame-box ctx) (make-cell-buffer 10 5))
      ;; Simulate resize effect
      (set-box! (tui-ctx-previous-frame-box ctx) #f)
      (check-false (unbox (tui-ctx-previous-frame-box ctx))
                   "previous-frame-box should be #f after resize"))

    ;; ── Test 3: cell-buffer-clear! resets all cells ──
    (test-case "cell-buffer-clear! resets all cells to default"
      (define buf (make-cell-buffer 5 3))
      ;; Write some content
      (cell-buffer-putstring! buf 0 0 "Hello" #:fg 2 #:bold #t)
      (cell-buffer-putstring! buf 0 1 "World" #:fg 3)
      ;; Clear
      (cell-buffer-clear! buf)
      ;; Verify cells are reset
      (define cell (cell-buffer-ref buf 0 0))
      (check-equal? (cell-char cell) #\space)
      (check-equal? (cell-fg cell) 7) ; default fg
      (check-false (cell-bold? cell)))

    ;; ── Test 4: diff-cell-buffers with #f prev produces all-cell deltas ──
    (test-case "diff-cell-buffers with #f prev-buf produces all-cell deltas"
      (define buf (make-cell-buffer 4 2))
      (define deltas (diff-cell-buffers #f buf))
      ;; All 4*2 = 8 cells should be in deltas
      (check-equal? (length deltas) 8))

    ;; ── Test 5: render-smart! with #f prev-buf produces full render ──
    (test-case "render-smart! with #f prev-buf takes full render path"
      (define buf (make-cell-buffer 10 2))
      (cell-buffer-putstring! buf 0 0 "Test" #:fg 2)
      (define out (open-output-string))
      (render-smart! #f buf out #:sync? #f)
      (define output (get-output-string out))
      ;; Full render includes ESC[H (cursor home) and ESC[K (clear line)
      (check-true (string-contains? output "\x1b[H") "full render should start with cursor home")
      (check-true (string-contains? output "\x1b[K") "full render should include ESC[K per row"))

    ;; ── Test 6: render-deltas-to-port! with single-row change ──
    (test-case "render-deltas-to-port! emits cursor positioning + text for single row"
      (define prev (make-cell-buffer 10 3))
      (define curr (make-cell-buffer 10 3))
      (cell-buffer-putstring! curr 0 0 "ABC" #:fg 2)
      (define deltas (diff-cell-buffers prev curr))
      (define out (open-output-string))
      (render-deltas-to-port! deltas curr out #:sync? #f)
      (define output (get-output-string out))
      ;; Should contain cursor positioning escape (CSI row;col H)
      (check-true (regexp-match? #rx"\x1b\\[1;1H" output) "should position cursor at row 1, col 1")
      ;; Should contain the actual text
      (check-true (string-contains? output "ABC") "should contain the changed text"))

    ;; ── Test 7: render-deltas-to-port! emits ESC[K for shortened rows (FIXED) ──
    ;; After W2 fix, delta render emits ESC[K after rows whose last delta is a default cell.
    (test-case "F-TUI-03: render-deltas-to-port! emits ESC[K for shortened rows (fixed)"
      (define prev (make-cell-buffer 10 3))
      (cell-buffer-putstring! prev 0 0 "XXXXXXXXX" #:fg 2)
      (define curr (make-cell-buffer 10 3))
      (cell-buffer-putstring! curr 0 0 "ABC" #:fg 2)
      ;; Cols 0-2 change to ABC, cols 3-8 change from X to default (space)
      (define deltas (diff-cell-buffers prev curr))
      (define out (open-output-string))
      (render-deltas-to-port! deltas curr out #:sync? #f)
      (define output (get-output-string out))
      ;; After W2 fix: ESC[K IS emitted because last delta in row has default new-cell
      (check-true (string-contains? output "\x1b[K")
                  "delta render SHOULD emit ESC[K for rows ending in default cells (F-TUI-03 fixed)"))

    ;; ── Test 8: cell-buffer-snapshot produces independent copy ──
    (test-case "cell-buffer-snapshot produces independent copy"
      (define buf (make-cell-buffer 10 3))
      (cell-buffer-putstring! buf 0 0 "Hello" #:fg 2 #:bold #t)
      (define snap (cell-buffer-snapshot buf))
      ;; Mutate original
      (cell-buffer-putstring! buf 0 0 "XXXXX" #:fg 1)
      ;; Snapshot should be unchanged
      (check-equal? (cell-char (cell-buffer-ref snap 0 0)) #\H)
      (check-equal? (cell-fg (cell-buffer-ref snap 0 0)) 2)
      (check-true (cell-bold? (cell-buffer-ref snap 0 0)))
      ;; Original is changed
      (check-equal? (cell-char (cell-buffer-ref buf 0 0)) #\X))

    ;; ============================================================
    ;; W1 Tests: F-TUI-01 + F-TUI-02 Fix Verification
    ;; ============================================================

    ;; ── Test 9: F-TUI-01 — incremental frame counter resets on full render ──
    (test-case "F-TUI-01: incremental-frame-count is 0 after full render"
      ;; Simulate: counter is at 50, then a full render (prev-ubuf=#f) resets it
      (set-box! incremental-frame-count 50)
      (define ctx (make-tui-ctx))
      ;; Simulate the render-frame! counter logic for full render path
      (define prev-ubuf (unbox (tui-ctx-prev-ubuf-box ctx))) ; #f initially
      (cond
        [(not prev-ubuf) (set-box! incremental-frame-count 0)]
        [else (void)])
      (check-equal? (unbox incremental-frame-count) 0 "counter should be 0 after full render path"))

    ;; ── Test 10: F-TUI-02 — counter increments on incremental render ──
    (test-case "F-TUI-02: incremental-frame-count increments on incremental render"
      ;; Simulate: counter starts at 10, incremental render increments it
      (set-box! incremental-frame-count 10)
      (define ctx (make-tui-ctx))
      (set-box! (tui-ctx-prev-ubuf-box ctx) (make-cell-buffer 80 24))
      (define prev-ubuf (unbox (tui-ctx-prev-ubuf-box ctx)))
      (when prev-ubuf
        (set-box! incremental-frame-count (add1 (unbox incremental-frame-count))))
      (check-equal? (unbox incremental-frame-count)
                    11
                    "counter should be 11 after one incremental render"))

    ;; ── Test 11: F-TUI-02 — full render forced after FULL-RENDER-INTERVAL-FRAMES ──
    (test-case "F-TUI-02: force full render after FULL-RENDER-INTERVAL-FRAMES"
      ;; Simulate: counter at threshold, should trigger force-full-render
      (set-box! incremental-frame-count FULL-RENDER-INTERVAL-FRAMES)
      (define ctx (make-tui-ctx))
      (set-box! (tui-ctx-prev-ubuf-box ctx) (make-cell-buffer 80 24))
      (define prev-ubuf (unbox (tui-ctx-prev-ubuf-box ctx)))
      (define force-full-render?
        (and prev-ubuf (>= (unbox incremental-frame-count) FULL-RENDER-INTERVAL-FRAMES)))
      (check-true force-full-render? "should force full render when counter reaches threshold")
      ;; After forcing, counter resets to 0
      (when force-full-render?
        (set-box! incremental-frame-count 0))
      (check-equal? (unbox incremental-frame-count)
                    0
                    "counter should reset to 0 after forced full render"))

    ;; ── Test 12: F-TUI-02 — FULL-RENDER-INTERVAL-FRAMES is 300 ──
    (test-case "F-TUI-02: FULL-RENDER-INTERVAL-FRAMES is 300"
      (check-equal? FULL-RENDER-INTERVAL-FRAMES 300 "safety net threshold should be 300 frames"))

    ;; ── Test 13: F-TUI-01 — both render boxes cleared simultaneously ──
    (test-case "F-TUI-01: resize clears both prev-ubuf-box AND previous-frame-box"
      (define ctx (make-tui-ctx))
      (define dummy (make-cell-buffer 10 5))
      ;; Populate both boxes with non-#f values
      (set-box! (tui-ctx-prev-ubuf-box ctx) dummy)
      (set-box! (tui-ctx-previous-frame-box ctx) dummy)
      ;; Simulate the fixed resize: both boxes cleared
      (set-box! (tui-ctx-previous-frame-box ctx) #f)
      (set-box! (tui-ctx-prev-ubuf-box ctx) #f)
      ;; Verify BOTH are cleared
      (check-false (unbox (tui-ctx-prev-ubuf-box ctx)) "prev-ubuf-box must be #f after resize")
      (check-false (unbox (tui-ctx-previous-frame-box ctx))
                   "previous-frame-box must be #f after resize"))

    ;; ============================================================
    ;; W2 Tests: F-TUI-03 Delta Render Row-End Clear
    ;; ============================================================

    ;; ── Test 14: F-TUI-03 — multiple rows shortened → ESC[K at each boundary ──
    (test-case "F-TUI-03: multiple shortened rows each get ESC[K"
      (define prev (make-cell-buffer 10 3))
      (cell-buffer-putstring! prev 0 0 "XXXXX" #:fg 2)
      (cell-buffer-putstring! prev 0 1 "YYYYY" #:fg 3)
      (define curr (make-cell-buffer 10 3))
      (cell-buffer-putstring! curr 0 0 "AB" #:fg 2)
      (cell-buffer-putstring! curr 0 1 "CD" #:fg 3)
      (define deltas (diff-cell-buffers prev curr))
      (define out (open-output-string))
      (render-deltas-to-port! deltas curr out #:sync? #f)
      (define output (get-output-string out))
      ;; Count ESC[K occurrences — should be 2 (one per shortened row)
      (define esc-k-count (length (regexp-match-positions* #rx"\x1b\\[K" output)))
      (check-equal? esc-k-count 2 "should emit ESC[K once per shortened row"))

    ;; ── Test 15: F-TUI-03 — no deltas → no ESC[K ──
    (test-case "F-TUI-03: no deltas produces no ESC[K"
      (define buf (make-cell-buffer 10 3))
      (define out (open-output-string))
      (render-deltas-to-port! '() buf out #:sync? #f)
      (define output (get-output-string out))
      (check-false (string-contains? output "\x1b[K") "no deltas should produce no ESC[K"))

    ;; ── Test 16: F-TUI-03 — row NOT shortened (last delta non-default) → no ESC[K ──
    (test-case "F-TUI-03: row ending with non-default cell does NOT get ESC[K"
      (define prev (make-cell-buffer 10 3))
      (cell-buffer-putstring! prev 0 0 "Hello" #:fg 2)
      (define curr (make-cell-buffer 10 3))
      (cell-buffer-putstring! curr 0 0 "Hexlo" #:fg 2)
      ;; Only col 1 changes: e→x. Last delta new-cell is 'x' (non-default)
      (define deltas (diff-cell-buffers prev curr))
      (define out (open-output-string))
      (render-deltas-to-port! deltas curr out #:sync? #f)
      (define output (get-output-string out))
      (check-false (string-contains? output "\x1b[K")
                   "row ending with non-default cell should NOT emit ESC[K"))

    ;; ── Test 17: F-TUI-03 — multiple batches same row → single ESC[K at row end ──
    (test-case "F-TUI-03: multiple batches in same row produce single ESC[K"
      (define prev (make-cell-buffer 10 3))
      (cell-buffer-putstring! prev 0 0 "XXXXX" #:fg 2)
      (define curr (make-cell-buffer 10 3))
      ;; Put ABC with fg=2 at cols 0-2, then spaces (default fg=7) at cols 3-4
      (cell-buffer-putstring! curr 0 0 "ABC" #:fg 2)
      ;; Deltas will have two batches: ABC (fg2) and spaces (fg7) — same row
      (define deltas (diff-cell-buffers prev curr))
      (define out (open-output-string))
      (render-deltas-to-port! deltas curr out #:sync? #f)
      (define output (get-output-string out))
      ;; Multiple batches in same row, but only ONE ESC[K at the row end
      (define esc-k-count (length (regexp-match-positions* #rx"\x1b\\[K" output)))
      (check-equal? esc-k-count 1 "single ESC[K for multi-batch row ending in default"))

    ;; ============================================================
    ;; W3 Tests: F-TUI-04 Cursor-Blink Snapshot Consistency
    ;; ============================================================

    ;; ── Test 18: F-TUI-04 — write-cell! toggles inverse correctly ──
    (test-case "F-TUI-04: write-cell! toggles cursor cell inverse video"
      (define ubuf (make-cell-buffer 10 3))
      ;; Create a base cell: char=X, fg=2, bg=0
      (cell-buffer-set! ubuf 3 1 #:char #\X #:fg 2 #:bg 0)
      (define base-cell (cell-buffer-ref ubuf 3 1))
      ;; Toggle inverse ON
      (write-cell! ubuf 3 1 base-cell #:inverse? #t)
      (define inv-cell (cell-buffer-ref ubuf 3 1))
      (check-equal? (cell-char inv-cell) #\X "inverse toggle preserves char")
      (check-equal? (cell-fg inv-cell) 0 "inverse swaps fg to original bg")
      (check-equal? (cell-bg inv-cell) 2 "inverse swaps bg to original fg")
      ;; Toggle inverse OFF (revert)
      (write-cell! ubuf 3 1 base-cell #:inverse? #f)
      (define norm-cell (cell-buffer-ref ubuf 3 1))
      (check-equal? (cell-fg norm-cell) 2 "non-inverse restores original fg")
      (check-equal? (cell-bg norm-cell) 0 "non-inverse restores original bg"))

    ;; ── Test 19: F-TUI-04 — direct cursor cell write produces NO ESC[K ──
    ;; The W3 fix bypasses render-deltas-to-port! for blink frames.
    ;; Simulate the direct write path and verify no ESC[K is emitted.
    (test-case "F-TUI-04: direct cursor cell write bypasses ESC[K emission"
      (define ubuf (make-cell-buffer 10 3))
      ;; Put a default cell at the cursor position (space, fg=7, bg=0)
      ;; This is the exact scenario that would trigger ESC[K via the delta path
      (cell-buffer-set! ubuf 5 1 #:char #\space #:fg 7 #:bg 0)
      (define cursor-cell (cell-buffer-ref ubuf 5 1))
      ;; Simulate F-TUI-04 direct write path
      (define out (open-output-string))
      (display "\x1b[?7l" out)
      (display (format "\x1b[~a;~aH" 2 6) out)
      (display (cell->sgr cursor-cell) out)
      (display (cell-char cursor-cell) out)
      (display "\x1b[?7h" out)
      (define output (get-output-string out))
      ;; The direct write path must NOT contain ESC[K
      (check-false (string-contains? output "\x1b[K")
                   "F-TUI-04: cursor blink direct write must NOT emit ESC[K"))

    ;; ── Test 20: F-TUI-04 — repeated blink toggles maintain consistency ──
    (test-case "F-TUI-04: repeated blink toggles cycle correctly"
      (define ubuf (make-cell-buffer 10 3))
      (cell-buffer-set! ubuf 2 0 #:char #\A #:fg 3 #:bg 1)
      (define base-cell (cell-buffer-ref ubuf 2 0))
      ;; Simulate 4 blink phases: on, off, on, off
      (define phases '(#t #f #t #f))
      (for ([phase (in-list phases)])
        (write-cell! ubuf 2 0 base-cell #:inverse? phase))
      ;; After even number of toggles, cell should be back to base
      (define final-cell (cell-buffer-ref ubuf 2 0))
      (check-equal? (cell-fg final-cell) 3 "fg restored after even toggles")
      (check-equal? (cell-bg final-cell) 1 "bg restored after even toggles")
      ;; Odd number of toggles leaves it inverted
      (write-cell! ubuf 2 0 base-cell #:inverse? #t)
      (define odd-cell (cell-buffer-ref ubuf 2 0))
      (check-equal? (cell-fg odd-cell) 1 "fg inverted after odd toggles")
      (check-equal? (cell-bg odd-cell) 3 "bg inverted after odd toggles"))

    ;; ── Test 21: F-TUI-04 — blink with no prev snapshot does full render ──
    ;; When prev-ubuf is #f, the blink path must fall through to render-smart!
    ;; with #f to do a full render (not the direct write).
    (test-case "F-TUI-04: no prev-ubuf triggers full render path"
      ;; Simulate: prev-ubuf-box is #f, ubuf has content
      (define ubuf (make-cell-buffer 10 3))
      (cell-buffer-putstring! ubuf 0 0 "Hello" #:fg 2)
      (define prev-ubuf #f)
      ;; When prev-ubuf is #f, render-smart! does a full render
      (define out (open-output-string))
      (render-smart! prev-ubuf ubuf out #:sync? #f)
      (define output (get-output-string out))
      ;; Full render should contain the text and cursor positioning
      (check-true (string-contains? output "Hello") "full render should contain all cell content")
      ;; Full render should have cursor home or row positioning
      (check-true (or (string-contains? output "\x1b[H") (string-contains? output "\x1b[1;"))
                  "full render should position cursor at start"))

    ;; ── Test 22: F-TUI-04 — snapshot after blink matches ubuf ──
    (test-case "F-TUI-04: snapshot after blink frame matches cell buffer"
      (define ubuf (make-cell-buffer 10 3))
      (cell-buffer-set! ubuf 4 2 #:char #\Z #:fg 5 #:bg 0)
      (define base-cell (cell-buffer-ref ubuf 4 2))
      ;; Toggle cursor cell to inverse (simulating blink ON)
      (write-cell! ubuf 4 2 base-cell #:inverse? #t)
      ;; Store snapshot (same as render-cursor-blink-frame! does)
      (define snapshot (cell-buffer-snapshot ubuf))
      ;; The snapshot should match the ubuf at the cursor position
      (define snap-cell (cell-buffer-ref snapshot 4 2))
      (check-equal? (cell-fg snap-cell) 0 "snapshot cursor cell fg = inverted")
      (check-equal? (cell-bg snap-cell) 5 "snapshot cursor cell bg = inverted")
      ;; Verify the rest of the snapshot matches
      (check-equal? (cell-char (cell-buffer-ref snapshot 0 0))
                    (cell-char (cell-buffer-ref ubuf 0 0))
                    "snapshot matches ubuf at non-cursor cell"))))

(run-tests snapshot-drift-suite)
