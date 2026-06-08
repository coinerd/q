#lang racket

;; @speed slow
;; @suite default

;; test-cell-diff-integration.rkt — Full pipeline integration tests
;;
;; Tests the complete cell-diff rendering pipeline:
;;   cell-buffer → snapshot → diff → deltas → ANSI output
;;
;; Addresses audit finding C2: no integration test for cell-diff snapshot path.

(require rackunit
         rackunit/text-ui
         "../tui/cell-buffer.rkt"
         "../tui/cell-diff.rkt"
         "../tui/cell-diff-render.rkt")

(define test-cell-diff-integration
  (test-suite "cell-diff-integration"

    ;; --------------------------------------------------
    ;; Test 1: Streaming 100 lines through cell-diff pipeline
    ;; --------------------------------------------------
    (test-case "streaming 100 lines produces correct ANSI output"
      (define cols 80)
      (define rows 24)
      (define buf (make-cell-buffer cols rows))
      (define snap #f)

      (define out (open-output-string))

      ;; Frame 0: initial snapshot (first frame = full render)
      (cell-buffer-clear! buf)
      (cell-buffer-putstring! buf 0 0 "Line 001" #:fg 7)
      (set! snap (cell-buffer-snapshot buf))
      (render-smart! #f buf out #:sync? #f)
      (define frame0 (get-output-string out))

      ;; Verify first frame has home cursor + content
      (check-true (> (string-length frame0) 100) "first frame should have substantial output")
      (check-not-false (string-contains? frame0 "Line 001") "first frame should contain the text")

      ;; Frames 1-99: streaming updates
      (for ([i (in-range 2 101)])
        (define line-text (format "Line ~a" (~a i #:min-width 3 #:pad-string "0")))
        (define row-idx (min (sub1 i) (sub1 rows))) ;; Wrap within buffer
        (cell-buffer-putstring! buf 0 row-idx line-text #:fg 7)
        (define new-snap (cell-buffer-snapshot buf))
        (define deltas (diff-cell-buffers snap buf))
        (check-true (> (length deltas) 0) (format "frame ~a should detect changes" i))
        (set! out (open-output-string))
        (render-smart! snap buf out #:sync? #f)
        (define frame-output (get-output-string out))
        (check-true (> (string-length frame-output) 0) (format "frame ~a should produce output" i))
        (set! snap new-snap)))

    ;; --------------------------------------------------
    ;; Test 2: Resize scenario — buffer resize + re-render
    ;; --------------------------------------------------
    (test-case "resize produces valid full render"
      (define buf (make-cell-buffer 40 12))
      (cell-buffer-putstring! buf 0 0 "Small buffer" #:fg 7)
      (define snap (cell-buffer-snapshot buf))

      ;; Resize to larger
      (define buf2 (make-cell-buffer 80 24))
      (cell-buffer-putstring! buf2 0 0 "Large buffer" #:fg 7)

      ;; Diff between different sizes → full diff
      (define deltas (diff-cell-buffers snap buf2))
      (check-true (> (length deltas) 0) "resize should produce many deltas")

      ;; Render should produce full render (different dimensions)
      (define out (open-output-string))
      (render-smart! snap buf2 out #:sync? #f)
      (define output (get-output-string out))
      (check-not-false (string-contains? output "Large buffer")
                       "resized render should contain new text"))

    ;; --------------------------------------------------
    ;; Test 3: Snapshot independence
    ;; --------------------------------------------------
    (test-case "snapshot remains independent after buffer mutation"
      (define buf (make-cell-buffer 40 5))
      (cell-buffer-putstring! buf 0 0 "Original" #:fg 2 #:bold #t)
      (cell-buffer-putstring! buf 5 3 "Row3" #:fg 4)

      (define snap (cell-buffer-snapshot buf))

      ;; Mutate buffer heavily
      (cell-buffer-clear! buf)
      (cell-buffer-putstring! buf 0 0 "MODIFIED" #:fg 1)

      ;; Verify snapshot is unchanged
      (check-equal? (cell-char (cell-buffer-ref snap 0 0)) #\O)
      (check-equal? (cell-fg (cell-buffer-ref snap 0 0)) 2)
      (check-true (cell-bold? (cell-buffer-ref snap 0 0)))
      (check-equal? (cell-char (cell-buffer-ref snap 5 3)) #\R)
      (check-equal? (cell-fg (cell-buffer-ref snap 5 3)) 4)

      ;; Verify buffer changed
      (check-equal? (cell-char (cell-buffer-ref buf 0 0)) #\M)
      (check-equal? (cell-fg (cell-buffer-ref buf 0 0)) 1))

    ;; --------------------------------------------------
    ;; Test 4: No-change frame produces no output (or minimal)
    ;; --------------------------------------------------
    (test-case "no-change between frames produces zero deltas"
      (define buf (make-cell-buffer 20 5))
      (cell-buffer-putstring! buf 0 0 "Same content" #:fg 7)
      (define snap (cell-buffer-snapshot buf))

      ;; Same content, re-snapshot
      (define snap2 (cell-buffer-snapshot buf))
      (define deltas (diff-cell-buffers snap snap2))
      (check-equal? (length deltas) 0 "identical buffers should produce zero deltas"))

    ;; --------------------------------------------------
    ;; Test 5: Full render fallback for >50% change
    ;; --------------------------------------------------
    (test-case "smart render falls back to full render for large changes"
      (define buf (make-cell-buffer 10 3))
      ;; Fill buffer with unique per-cell content (avoids row-hash XOR cancellation)
      (for* ([r (in-range 3)]
             [c (in-range 10)])
        (cell-buffer-set! buf
                          c
                          r
                          #:char (integer->char (+ 65 (modulo (+ (* r 10) c) 26)))
                          #:fg (+ 1 (modulo (+ r c) 7))))
      (define snap (cell-buffer-snapshot buf))

      ;; Change > 50% of cells (rows 0-1)
      (for* ([r (in-range 2)]
             [c (in-range 10)])
        (cell-buffer-set! buf c r #:char #\Z #:fg 3))

      (define deltas (diff-cell-buffers snap buf))
      (define total-cells (* 10 3))
      (check-true (> (length deltas) (* total-cells 0.5)) "should detect >50% change")

      ;; Smart render should choose full render path
      (define out (open-output-string))
      (render-smart! snap buf out #:sync? #f)
      (define output (get-output-string out))
      ;; Full render has home cursor
      (check-not-false (string-contains? output "\x1b[H")
                       "large change should trigger full render with home cursor"))

    ;; --------------------------------------------------
    ;; Test 6: SGR changes detected in diff
    ;; --------------------------------------------------
    (test-case "diff detects SGR-only changes (color change, no text change)"
      (define buf (make-cell-buffer 10 2))
      (cell-buffer-putstring! buf 0 0 "Hello" #:fg 7)
      (define snap (cell-buffer-snapshot buf))

      ;; Change color only (same text)
      (cell-buffer-putstring! buf 0 0 "Hello" #:fg 1)

      (define deltas (diff-cell-buffers snap buf))
      (check-true (> (length deltas) 0) "color-only change should produce deltas")

      ;; Render should include SGR change
      (define out (open-output-string))
      (render-deltas-to-port! deltas buf out #:sync? #f)
      (define output (get-output-string out))
      (check-not-false (regexp-match? #rx"38;5;1" output) "should contain new fg color SGR"))

    ;; --------------------------------------------------
    ;; Test 7: Multi-row incremental diff correctness
    ;; --------------------------------------------------
    (test-case "multi-row incremental diff produces correct ANSI positioning"
      (define buf (make-cell-buffer 20 5))
      (cell-buffer-putstring! buf 0 0 "Row 0" #:fg 7)
      (cell-buffer-putstring! buf 0 2 "Row 2" #:fg 7)
      (cell-buffer-putstring! buf 0 4 "Row 4" #:fg 7)
      (define snap (cell-buffer-snapshot buf))

      ;; Change rows 0, 2, and 4
      (cell-buffer-putstring! buf 0 0 "ROW 0" #:fg 1)
      (cell-buffer-putstring! buf 0 2 "ROW 2" #:fg 2)
      (cell-buffer-putstring! buf 0 4 "ROW 4" #:fg 3)

      (define deltas (diff-cell-buffers snap buf))
      (define out (open-output-string))
      (render-deltas-to-port! deltas buf out #:sync? #f)
      (define output (get-output-string out))

      ;; Should contain cursor positioning for each changed row
      (check-not-false (string-contains? output "\x1b[1;") "should position cursor at row 1 (0+1)")
      (check-not-false (string-contains? output "\x1b[3;") "should position cursor at row 3 (2+1)")
      (check-not-false (string-contains? output "\x1b[5;") "should position cursor at row 5 (4+1)"))

    ;; --------------------------------------------------
    ;; Test 8: Empty initial buffer → first frame full render
    ;; --------------------------------------------------
    (test-case "first frame with no previous buffer does full render"
      (define buf (make-cell-buffer 40 5))
      (cell-buffer-putstring! buf 0 0 "First frame" #:fg 7 #:bg 4)

      (define out (open-output-string))
      (render-smart! #f buf out #:sync? #f)
      (define output (get-output-string out))

      ;; Full render should have home cursor and DECAWM off
      (check-not-false (string-contains? output "\x1b[?7l") "should disable auto-wrap")
      (check-not-false (string-contains? output "\x1b[H") "should have home cursor positioning")
      (check-not-false (string-contains? output "First frame") "should contain the rendered text"))))

(run-tests test-cell-diff-integration)
