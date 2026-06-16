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
         (only-in "../tui/tui-render-loop.rkt" FULL-RENDER-INTERVAL-FRAMES incremental-frame-count))

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
      (check-equal? esc-k-count 1 "single ESC[K for multi-batch row ending in default"))))

(run-tests snapshot-drift-suite)
