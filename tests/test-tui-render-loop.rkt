#lang racket

;; BOUNDARY: io

;; test-tui-render-loop.rkt — Tests for tui/tui-render-loop.rkt
;;
;; Tests module loading, export binding, and pure helpers.
;; render-frame!, tui-main-loop, etc. require a real terminal,
;; so we test only loadable exports and pure functions.

(require rackunit
         rackunit/text-ui
         "../tui/tui-render-loop.rkt"
         "../tui/tui-keybindings.rkt"
         "../tui/sgr.rkt"
         "../tui/terminal.rkt"
         "../tui/cell-buffer.rkt"
         "../tui/cell-diff.rkt"
         "../tui/cell-diff-render.rkt"
         "../tui/state.rkt"
         "../tui/input.rkt"
         "../tui/layout.rkt"
         "../tui/vdom-bridge.rkt")

(define test-tui-render-loop
  (test-suite "tui/tui-render-loop"

    ;; --------------------------------------------------
    ;; Test 1: Module exports are bound
    ;; --------------------------------------------------
    (test-case "render-frame! is bound"
      (check-pred procedure? render-frame!))

    (test-case "draw-frame is bound (deprecated alias)"
      (check-pred procedure? draw-frame))

    (test-case "next-message is bound"
      (check-pred procedure? next-message))

    (test-case "tui-main-loop is bound"
      (check-pred procedure? tui-main-loop))

    (test-case "drain-events! is bound"
      (check-pred procedure? drain-events!))

    ;; --------------------------------------------------
    ;; Test 2: Re-exported functions are accessible
    ;; --------------------------------------------------
    (test-case "fix-sgr-bg-black is bound"
      (check-pred procedure? fix-sgr-bg-black))

    (test-case "decode-mouse-x10 is bound"
      (check-pred procedure? decode-mouse-x10))

    (test-case "tui-ctx-init-terminal! is bound"
      (check-pred procedure? tui-ctx-init-terminal!))

    (test-case "tui-ctx-resize-ubuf! is bound"
      (check-pred procedure? tui-ctx-resize-ubuf!))

    ;; --------------------------------------------------
    ;; Test 3: fix-sgr-bg-black pure function works
    ;; --------------------------------------------------
    (test-case "fix-sgr-bg-black replaces bg=0 SGR sequences"
      ;; SGR 40 is "bg black" (ANSI code 40). fix-sgr-bg-black should replace
      ;; occurrences of ;40m or 40m (terminal default bg) with ;49m (default bg)
      (define input "\x1b[30;40mhello\x1b[0m")
      (define result (fix-sgr-bg-black input))
      (check-not-false (regexp-match? #rx"49m" result)
                       "bg=0 (40) should be replaced with default bg (49)"))

    (test-case "fix-sgr-bg-black leaves non-black bg unchanged"
      (define input "\x1b[30;41mhello\x1b[0m")
      (define result (fix-sgr-bg-black input))
      (check-false (regexp-match? #rx"49m" result) "bg=red (41) should not be replaced"))

    ;; --------------------------------------------------
    ;; Test 4: decode-mouse-x10 decodes mouse bytes
    ;; --------------------------------------------------
    (test-case "decode-mouse-x10 returns list with mouse type"
      (define result (decode-mouse-x10 32 49 49))
      (check-pred list? result)
      (check-true (>= (length result) 1)))

    ;; --------------------------------------------------
    ;; Test 5: drain-events! on empty channel is safe
    ;; --------------------------------------------------
    (test-case "drain-events! on empty channel does not crash"
      (define ctx (make-tui-ctx))
      (check-not-exn (lambda () (drain-events! ctx))))

    ;; --------------------------------------------------
    ;; Test 6: Synchronized output (DEC mode 2026)
    ;; --------------------------------------------------
    (test-case "terminal-sync-begin! is callable (no terminal)"
      ;; In headless/stub mode, sync-begin! should be a no-op
      (check-not-exn (lambda () (terminal-sync-begin!))))

    (test-case "terminal-sync-end! is callable (no terminal)"
      (check-not-exn (lambda () (terminal-sync-end!))))

    (test-case "terminal-sync-available? returns boolean"
      (check-pred boolean? (terminal-sync-available?)))

    (test-case "detect-sync-mode-support! runs without error"
      (check-not-exn (lambda () (detect-sync-mode-support!))))

    (test-case "sync mode wraps render output when supported"
      ;; Test that sync brackets appear when mode is force-enabled
      ;; We capture stdout to verify the escape sequences
      (define captured (open-output-string))
      (parameterize ([current-output-port captured])
        ;; Force-enable sync for this test
        (detect-sync-mode-support!)
        (terminal-sync-begin!)
        (display "test-output")
        (terminal-sync-end!))
      (define output (get-output-string captured))
      ;; When supported: output should contain \x1b[?2026h and \x1b[?2026l
      ;; When not supported: output is just "test-output"
      (when (terminal-sync-available?)
        (check-true (string-contains? output "\x1b[?2026h")
                    "sync begin escape present when supported")
        (check-true (string-contains? output "\x1b[?2026l")
                    "sync end escape present when supported")))

    ;; --------------------------------------------------
    ;; Test 7: Cell-diff snapshot integration
    ;; --------------------------------------------------
    (test-case "cell-buffer-snapshot produces independent copy for diffing"
      ;; Simulates the snapshot logic in render-frame! cell-diff branch
      (define buf (make-cell-buffer 20 5))
      (cell-buffer-putstring! buf 0 0 "Hello" #:fg 2)
      (cell-buffer-putstring! buf 0 1 "World" #:fg 3 #:bold #t)
      ;; Snapshot (same as render-frame! does)
      (define snap (cell-buffer-snapshot buf))
      ;; Mutate original
      (cell-buffer-putstring! buf 0 0 "XXXXX" #:fg 1)
      ;; Snapshot should be unchanged
      (check-equal? (cell-char (cell-buffer-ref snap 0 0)) #\H)
      (check-equal? (cell-fg (cell-buffer-ref snap 0 0)) 2)
      (check-equal? (cell-char (cell-buffer-ref buf 0 0)) #\X))

    (test-case "cell-diff snapshot → diff → render produces output"
      ;; End-to-end test of the cell-diff pipeline
      (define prev (make-cell-buffer 10 3))
      (cell-buffer-putstring! prev 0 0 "AAA" #:fg 1)
      ;; Snapshot prev
      (define snap (cell-buffer-snapshot prev))
      ;; Mutate to create current
      (cell-buffer-putstring! prev 0 0 "BBB" #:fg 2)
      ;; Diff
      (define deltas (diff-cell-buffers snap prev))
      (check-true (> (length deltas) 0) "should detect changes")
      ;; Render to string
      (define out (open-output-string))
      (render-smart! snap prev out #:sync? #t)
      (define output (get-output-string out))
      (check-true (> (string-length output) 0) "should produce output")
      (check-not-false (string-contains? output "B") "should contain new character"))

    ;; ============================================================
    ;; Legacy path removal tests
    ;; ============================================================

    (test-case "render-frame-vdom! is the only render path"
      ;; Verify render-frame-vdom! works correctly as sole render path
      (define ubuf (make-cell-buffer 80 24))
      (define st (initial-ui-state))
      (define inp (initial-input-state))
      (define layout (compute-layout 24 80))
      (define-values (cursor-col cursor-row st* frame-lines) (render-frame-vdom! ubuf st inp layout))
      (check-true (exact-nonnegative-integer? cursor-col))
      (check-true (exact-nonnegative-integer? cursor-row))
      (check-true (ui-state? st*))
      ;; frame-lines is now always empty (legacy removed)
      (check-equal? frame-lines '()))

    (test-case "use-vdom-render? defaults to #t"
      (check-true (use-vdom-render?)))))

(run-tests test-tui-render-loop)
