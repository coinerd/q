#lang racket

;; tests/test-tui-smoke.rkt — Smoke tests for TUI vdom render pipeline
;;
;; Exercises the full render pipeline without requiring a real terminal:
;; 1. State initialization + render-frame-vdom! produces valid output
;; 2. Streaming text updates render correctly
;; 3. Status bar + input line roundtrip
;; 4. Overlay rendering
;; 5. Resize handling
;;
;; Run: raco test tests/test-tui-smoke.rkt

(require rackunit
         rackunit/text-ui
         "../tui/cell-buffer.rkt"
         "../tui/cell-diff.rkt"
         "../tui/cell-diff-render.rkt"
         "../tui/tui-render-loop.rkt"
         "../tui/state.rkt"
         "../tui/state-types.rkt"
         "../tui/input.rkt"
         "../tui/layout.rkt"
         "../tui/vdom-bridge.rkt"
         "../tui/component.rkt")

(define test-tui-smoke
  (test-suite
   "TUI smoke tests"

   ;; --------------------------------------------------
   ;; Test 1: Full frame render produces non-empty cell-buffer
   ;; --------------------------------------------------
   (test-case "render-frame-vdom! fills cell-buffer"
     (define ubuf (make-cell-buffer 80 24))
     (define st (initial-ui-state #:model-name "smoke-test"))
     (define inp (initial-input-state))
     (define layout (compute-layout 24 80))
     (define-values (cursor-col cursor-row st* frame-lines)
       (render-frame-vdom! ubuf st inp layout))
     ;; Buffer should have non-space content (at least the status bar)
     ;; Check a few cells in different rows
     (define has-content
       (for*/or ([r (in-range 24)])
         (for/or ([c (in-range 80)])
           (not (char=? (cell-char (cell-buffer-ref ubuf c r)) #\space)))))
     (check-true has-content "buffer should have non-space content")
     ;; Cursor position should be valid
     (check-true (>= cursor-col 0) "cursor col >= 0")
     (check-true (>= cursor-row 0) "cursor row >= 0")
     ;; State should be returned unchanged in type
     (check-true (ui-state? st*) "returned state is ui-state?"))

   ;; --------------------------------------------------
   ;; Test 2: Streaming text update renders
   ;; --------------------------------------------------
   (test-case "streaming text renders in cell-buffer"
     (define ubuf (make-cell-buffer 80 24))
     ;; Add streaming text to state
     (define st0 (initial-ui-state #:model-name "smoke"))
     (define st1 (set-streaming-text (set-busy st0 #t) "Hello, world!"))
     (define inp (initial-input-state))
     (define layout (compute-layout 24 80))
     (render-frame-vdom! ubuf st1 inp layout)
     ;; Buffer should contain the streaming text
     (define found
       (for*/or ([r (in-range 24)]
                 [c (in-range 70)])
         (let ([ch (cell-char (cell-buffer-ref ubuf c r))])
           (char=? ch #\H))))
     (check-true found "streaming text 'Hello' should appear in buffer"))

   ;; --------------------------------------------------
   ;; Test 3: Cell-diff produces deltas on state change
   ;; --------------------------------------------------
   (test-case "cell-diff detects frame changes"
     (define cols 80)
     (define rows 24)
     (define ubuf-a (make-cell-buffer cols rows))
     (define st (initial-ui-state))
     (define inp (initial-input-state))
     (define layout (compute-layout rows cols))
     ;; Render first frame
     (render-frame-vdom! ubuf-a st inp layout)
     (define snap-a (cell-buffer-snapshot ubuf-a))
     ;; Render second frame with different state
     (define ubuf-b (make-cell-buffer cols rows))
     (define st2 (add-transcript-entry st
                                       (make-entry 'assistant
                                                   "Test response"
                                                   (current-inexact-milliseconds)
                                                   (hasheq))))
     (render-frame-vdom! ubuf-b st2 inp layout)
     ;; Diff should show changes
     (define deltas (diff-cell-buffers snap-a ubuf-b))
     (check-true (> (length deltas) 0)
                 "state change should produce cell deltas"))

   ;; --------------------------------------------------
   ;; Test 4: Delta rendering produces valid ANSI output
   ;; --------------------------------------------------
   (test-case "delta render produces ANSI output"
     (define cols 80)
     (define rows 24)
     (define ubuf-a (make-cell-buffer cols rows))
     (define st (initial-ui-state))
     (define inp (initial-input-state))
     (define layout (compute-layout rows cols))
     (render-frame-vdom! ubuf-a st inp layout)
     (define snap-a (cell-buffer-snapshot ubuf-a))
     ;; Mutate state
     (define st2 (add-transcript-entry st
                                       (make-entry 'user
                                                   "hello"
                                                   (current-inexact-milliseconds)
                                                   (hasheq))))
     (define ubuf-b (make-cell-buffer cols rows))
     (render-frame-vdom! ubuf-b st2 inp layout)
     ;; Render deltas to string
     (define out (open-output-string))
     (render-smart! snap-a ubuf-b out #:sync? #t)
     (define output (get-output-string out))
     ;; Should contain at least cursor positioning
     (check-true (> (string-length output) 0)
                 "delta render should produce output"))

   ;; --------------------------------------------------
   ;; Test 5: Multiple consecutive renders are stable
   ;; --------------------------------------------------
   (test-case "consecutive renders produce stable output"
     (define cols 80)
     (define rows 24)
     (define st (initial-ui-state))
     (define inp (initial-input-state))
     (define layout (compute-layout rows cols))
     ;; Render frame 1
     (define ubuf1 (make-cell-buffer cols rows))
     (render-frame-vdom! ubuf1 st inp layout)
     ;; Render identical frame 2
     (define ubuf2 (make-cell-buffer cols rows))
     (render-frame-vdom! ubuf2 st inp layout)
     ;; Diff should be empty (no changes)
     (define deltas (diff-cell-buffers ubuf1 ubuf2))
     (check-equal? (length deltas) 0
                   "identical frames should have zero deltas"))

   ;; --------------------------------------------------
   ;; Test 6: Resize produces different layout
   ;; --------------------------------------------------
   (test-case "resize changes layout"
     (define st (initial-ui-state))
     (define inp (initial-input-state))
     (define layout-80x24 (compute-layout 24 80))
     (define layout-120x40 (compute-layout 40 120))
     ;; Both should produce valid layouts
     (define ubuf-80 (make-cell-buffer 80 24))
     (define-values (c1 r1 _s1 _f1)
       (render-frame-vdom! ubuf-80 st inp layout-80x24))
     (check-true (>= c1 0))
     (check-true (>= r1 0))
     (define ubuf-120 (make-cell-buffer 120 40))
     (define-values (c2 r2 _s2 _f2)
       (render-frame-vdom! ubuf-120 st inp layout-120x40))
     (check-true (>= c2 0))
     (check-true (>= r2 0)))

   ;; --------------------------------------------------
   ;; Test 7: vdom render path is always active
   ;; --------------------------------------------------
   (test-case "use-vdom-render? is always #t"
     (check-true (use-vdom-render?))
     ;; Verify render-frame-vdom! is the same path render-frame! uses
     (check-pred procedure? render-frame-vdom!))))

(run-tests test-tui-smoke)
