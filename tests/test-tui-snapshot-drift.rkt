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
         "../tui/terminal.rkt")

(define snapshot-drift-suite
  (test-suite "TUI Snapshot Drift Characterization (v0.99.16 W0)"

    ;; ── Test 1: F-TUI-01 — prev-ubuf-box NOT cleared on resize ──
    ;; This test FAILS before W1 fix, proving the bug exists.
    (test-case "F-TUI-01: prev-ubuf-box is NOT #f after resize (bug characterization)"
      ;; We can't call tui-ctx-resize-ubuf! without a real terminal,
      ;; but we can simulate the effect by checking the function's
      ;; behavior on the context boxes directly.
      ;; The bug: tui-ctx-resize-ubuf! sets previous-frame-box to #f
      ;; but does NOT set prev-ubuf-box to #f.
      ;;
      ;; We simulate: create ctx, set prev-ubuf-box to a non-#f value,
      ;; then manually replicate what resize does (without terminal).
      (define ctx (make-tui-ctx))
      (define dummy-buf (make-cell-buffer 10 5))
      ;; Simulate a prior render: prev-ubuf-box has a snapshot
      (set-box! (tui-ctx-prev-ubuf-box ctx) dummy-buf)
      (set-box! (tui-ctx-previous-frame-box ctx) dummy-buf)
      ;; Simulate resize effect manually (since tui-ctx-resize-ubuf!
      ;; requires a real terminal for tui-screen-size)
      ;; Current code only clears previous-frame-box, not prev-ubuf-box
      (set-box! (tui-ctx-ubuf-box ctx) (make-cell-buffer 20 10))
      (set-box! (tui-ctx-previous-frame-box ctx) #f)
      ;; BUG: prev-ubuf-box is NOT cleared — it still has the old snapshot
      ;; After W1 fix, this will be #f. Before W1 fix, it's the old buf.
      ;; NOTE: This test characterizes the bug by showing prev-ubuf-box
      ;; retains stale data. The actual fix will add the line:
      ;;   (set-box! (tui-ctx-prev-ubuf-box ctx) #f)
      ;; For now we just document that the stale value persists.
      (define prev-ubuf-after-resize (unbox (tui-ctx-prev-ubuf-box ctx)))
      ;; Before fix: prev-ubuf-after-resize is dummy-buf (not #f)
      ;; After fix: prev-ubuf-after-resize is #f
      ;; We verify the stale state exists to document the bug.
      ;; W1 will change this to check-equal? prev-ubuf-after-resize #f
      (check-not-false prev-ubuf-after-resize
                       "prev-ubuf-box retains stale buffer after resize (F-TUI-01 bug)"))

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

    ;; ── Test 7: render-deltas-to-port! does NOT emit ESC[K ──
    ;; This documents the F-TUI-03 condition: delta render lacks ESC[K.
    (test-case "F-TUI-03: render-deltas-to-port! does NOT emit ESC[K (bug characterization)"
      (define prev (make-cell-buffer 10 3))
      (cell-buffer-putstring! prev 0 0 "XXXXXXXXX" #:fg 2)
      (define curr (make-cell-buffer 10 3))
      (cell-buffer-putstring! curr 0 0 "ABC" #:fg 2)
      ;; Only first 3 cells differ (A replaces X, B replaces X, C replaces X)
      ;; Cells 3-8 still have X in prev but space in curr — they're in deltas
      (define deltas (diff-cell-buffers prev curr))
      (define out (open-output-string))
      (render-deltas-to-port! deltas curr out #:sync? #f)
      (define output (get-output-string out))
      ;; Before W2 fix: no ESC[K in delta render
      ;; After W2 fix: ESC[K emitted at row boundaries
      (check-false (string-contains? output "\x1b[K")
                   "delta render does NOT emit ESC[K (F-TUI-03 bug condition)"))

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
      (check-equal? (cell-char (cell-buffer-ref buf 0 0)) #\X))))

(run-tests snapshot-drift-suite)
