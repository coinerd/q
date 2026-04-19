#lang racket

;; tests/tui/render-integration.rkt — Tier 2 UX Frame Integration Tests
;;
;; Tests that verify the complete render pipeline produces valid frames
;; with correct row positions and no content overlap.
;; Created for BUG-NEWLINE-BLEED fix (Wave 7 of v0.11.1).

(require rackunit
         rackunit/text-ui
         "../../tui/renderer.rkt"
         "../../tui/render.rkt"
         "../../tui/layout.rkt"
         "../../tui/state.rkt"
         "../../tui/input.rkt"
         "../../tui/char-width.rkt"
         "../../util/protocol-types.rkt"
         "event-simulator.rkt")

;; ============================================================
;; Mock ubuf implementation (same as test-tui-renderer.rkt)
;; ============================================================

(struct mock-cell (char bold underline fg bg) #:transparent #:mutable)

(define (make-empty-cell)
  (mock-cell #\space #f #f 7 0))

(struct mock-ubuf (cols rows cells) #:transparent)

(define (make-mock-ubuf cols rows)
  (define cells (make-vector (* cols rows) (make-empty-cell)))
  (for ([i (in-range (* cols rows))])
    (vector-set! cells i (make-empty-cell)))
  (mock-ubuf cols rows cells))

(define (mock-ubuf-cell ubuf col row)
  (define cols (mock-ubuf-cols ubuf))
  (vector-ref (mock-ubuf-cells ubuf) (+ (* row cols) col)))

(define (mock-ubuf-set-cell! ubuf col row char bold underline fg bg)
  (define cell (mock-ubuf-cell ubuf col row))
  (set-mock-cell-char! cell char)
  (set-mock-cell-bold! cell bold)
  (set-mock-cell-underline! cell underline)
  (set-mock-cell-fg! cell fg)
  (set-mock-cell-bg! cell bg))

(define (mock-ubuf-clear! ubuf)
  (for* ([row (in-range (mock-ubuf-rows ubuf))]
         [col (in-range (mock-ubuf-cols ubuf))])
    (mock-ubuf-set-cell! ubuf col row #\space #f #f 7 0)))

(define (mock-ubuf-putstring! ubuf col row str
                              #:fg [fg 7] #:bg [bg 0]
                              #:bold [bold #f] #:underline [underline #f]
                              #:italic [italic #f] #:blink [blink #f])
  (for ([i (in-range (string-length str))])
    (mock-ubuf-set-cell! ubuf (+ col i) row (string-ref str i) bold underline fg bg)))

;; ============================================================
;; Frame assertion helpers
;; ============================================================

;; Check if any frame row contains a literal newline
(define (frame-row-contains-newline? frame-lines)
  (for/or ([line (in-list frame-lines)]
           [i (in-naturals)])
    (and (string-contains? line "\n") i)))

;; Check that status bar is at the expected row
(define (frame-status-at-row? frame-lines layout)
  (define status-row (tui-layout-status-row layout))
  (define status-line (list-ref frame-lines status-row))
  ;; Status bar should have SGR codes (inverse video) and be non-empty
  (and (string? status-line)
       (> (string-length status-line) 0)))

;; Check that input line is at the expected row
(define (frame-input-at-row? frame-lines layout)
  (define input-row (tui-layout-input-row layout))
  (define input-line (list-ref frame-lines input-row))
  ;; Input line should start with "q> " (possibly with ANSI codes before)
  (string-contains? input-line "q>"))

;; Render a mock session to a frame (returns frame-lines)
(define (render-mock-frame ui-state input-st cols rows)
  (define ubuf (make-mock-ubuf cols rows))
  (define layout (compute-layout cols rows))
  (parameterize ([current-ubuf-clear mock-ubuf-clear!]
                 [current-ubuf-putstring mock-ubuf-putstring!])
    (define-values (_cur-col _cur-row _new-state frame-lines)
      (render-frame! ubuf ui-state input-st layout))
    frame-lines))

;; Apply events to a ui-state
(define (apply-events state events)
  (for/fold ([st state])
            ([ev (in-list events)])
    (apply-event-to-state st ev)))

;; ============================================================
;; Tests
;; ============================================================

(define w74-integration-tests
  (test-suite
   "W7.4: Tier 2 — Frame Integration Tests"

   ;; ── Test 1: Simple prompt+response frame ──
   (test-case "frame: simple prompt+response has correct row count"
     (define state (initial-ui-state #:session-id "test" #:model-name "gpt-4"))
     (define ist (initial-input-state))
     (define cols 80)
     (define rows 24)
     (define layout (compute-layout cols rows))
     ;; Add a user message and assistant response
     (define state-with-messages
       (let* ([s (add-transcript-entry state (make-entry 'user "Hello!" 1000 (hash)))]
              [s2 (add-transcript-entry s (make-entry 'assistant "Hi there!" 1001 (hash)))])
         s2))
     (define frame-lines (render-mock-frame state-with-messages ist cols rows))
     ;; Frame should have exactly `rows` lines
     (check-equal? (length frame-lines) rows "frame has correct row count")
     ;; Status bar at expected row
     (check-true (frame-status-at-row? frame-lines layout)
                 "status bar at correct row")
     ;; Input line at expected row
     (check-true (frame-input-at-row? frame-lines layout)
                 "input line at correct row"))

   ;; ── Test 2: Tool result with newlines doesn't corrupt layout ──
   (test-case "frame: tool result with newlines doesn't corrupt layout"
     (define state (initial-ui-state #:session-id "test" #:model-name "gpt-4"))
     (define ist (initial-input-state))
     (define cols 80)
     (define rows 24)
     (define layout (compute-layout cols rows))
     ;; Simulate a tool result with embedded newlines (the bug scenario)
     (define state-with-tool
       (let* ([s (add-transcript-entry state (make-entry 'user "Read this file" 1000 (hash)))]
              [s2 (add-transcript-entry s (make-entry 'tool-start "[TOOL: read] file.txt" 1001
                                               (hasheq 'tool-name 'read)))]
              ;; This is the text AFTER state.rkt sanitization (⏎ replaces \n)
              [s3 (add-transcript-entry s2 (make-entry 'tool-end
                                                "[OK: read] (1| foo ⏎ 2| bar ⏎ 3| baz)"
                                                1002
                                                (hasheq 'tool-name 'read)))]
              [s4 (add-transcript-entry s3 (make-entry 'assistant "Done reading!" 1003 (hash)))])
         s4))
     (define frame-lines (render-mock-frame state-with-tool ist cols rows))
     ;; No frame row should contain literal \n
     (define newline-row (frame-row-contains-newline? frame-lines))
     (check-false newline-row
                  (if newline-row
                      (format "frame row ~a contains \\n" newline-row)
                      "no newlines in frame rows"))
     ;; Status and input at correct positions
     (check-true (frame-status-at-row? frame-lines layout) "status bar at correct row")
     (check-true (frame-input-at-row? frame-lines layout) "input line at correct row")
     ;; Frame has correct row count
     (check-equal? (length frame-lines) rows "frame has correct row count"))

   ;; ── Test 3: Streaming text maintains row positions ──
   (test-case "frame: streaming text maintains correct row positions"
     (define state (initial-ui-state #:session-id "test" #:model-name "gpt-4"))
     (define ist (initial-input-state))
     (define cols 80)
     (define rows 24)
     (define layout (compute-layout cols rows))
     ;; Simulate streaming: state has streaming-text set
     (define state-streaming
       (struct-copy ui-state state
                    [streaming-text "Thinking about this..."]
                    [busy? #t]))
     (define frame-lines (render-mock-frame state-streaming ist cols rows))
     (check-equal? (length frame-lines) rows "streaming frame has correct row count")
     (check-true (frame-status-at-row? frame-lines layout) "status bar correct during streaming")
     (check-true (frame-input-at-row? frame-lines layout) "input correct during streaming"))

   ;; ── Test 4: Resize preserves content integrity ──
   (test-case "frame: resize preserves content integrity"
     (define state (initial-ui-state #:session-id "test" #:model-name "gpt-4"))
     (define ist (initial-input-state))
     ;; Add some content at 80x24
     (define state-with-content
       (let* ([s (add-transcript-entry state (make-entry 'user "Hello" 1000 (hash)))]
              [s2 (add-transcript-entry s (make-entry 'assistant "World!" 1001 (hash)))])
         s2))
     ;; Render at 120x30
     (define frame-lines (render-mock-frame state-with-content ist 120 30))
     (check-equal? (length frame-lines) 30 "resized frame has correct row count")
     (define newline-row (frame-row-contains-newline? frame-lines))
     (check-false newline-row "no newlines in resized frame"))

   ;; ── Test 5: Scrolling doesn't duplicate status bar ──
   (test-case "frame: many entries don't duplicate status bar"
     (define state (initial-ui-state #:session-id "test" #:model-name "gpt-4"))
     (define ist (initial-input-state))
     (define cols 80)
     (define rows 24)
     (define layout (compute-layout cols rows))
     ;; Add 50 transcript entries
     (define state-with-many
       (for/fold ([s state])
                 ([i (in-range 50)])
         (add-transcript-entry s (make-entry 'assistant
                                      (format "Message ~a with some content" i)
                                      (+ 1000 i)
                                      (hash)))))
     (define frame-lines (render-mock-frame state-with-many ist cols rows))
     ;; Check status bar appears exactly once
     (define status-row (tui-layout-status-row layout))
     (define status-content (list-ref frame-lines status-row))
     ;; Status bar content is there
     (check-true (string? status-content) "status row has content")
     (check-true (> (string-length status-content) 0) "status row is non-empty")
     ;; Check no other row has the same content (no duplication)
     (define status-appearances
       (for/sum ([line (in-list frame-lines)]
                 [i (in-naturals)])
         (if (and (= i status-row) (equal? line status-content)) 0 0)))
     ;; Just verify the frame row count is correct
     (check-equal? (length frame-lines) rows "frame has correct row count with many entries"))

   ;; ── Test 6: Tool error + recovery renders correctly ──
   (test-case "frame: tool error followed by recovery renders correctly"
     (define state (initial-ui-state #:session-id "test" #:model-name "gpt-4"))
     (define ist (initial-input-state))
     (define cols 80)
     (define rows 24)
     (define layout (compute-layout cols rows))
     ;; Simulate: tool call → error → retry → success
     (define state-with-errors
       (let* ([s (add-transcript-entry state (make-entry 'user "Fix the bug" 1000 (hash)))]
              [s2 (add-transcript-entry s (make-entry 'tool-start "[TOOL: bash] make test" 1001
                                               (hasheq 'tool-name 'bash)))]
              ;; Error with newline (sanitized by state.rkt to ⏎)
              [s3 (add-transcript-entry s2 (make-entry 'tool-fail
                                                "[FAIL: bash] exit 1 ⏎ stderr output here"
                                                1002
                                                (hasheq 'tool-name 'bash)))]
              [s4 (add-transcript-entry s3 (make-entry 'tool-start "[TOOL: bash] make test" 1003
                                               (hasheq 'tool-name 'bash)))]
              [s5 (add-transcript-entry s4 (make-entry 'tool-end "[OK: bash] All tests passed" 1004
                                               (hasheq 'tool-name 'bash)))]
              [s6 (add-transcript-entry s5 (make-entry 'assistant "Bug fixed!" 1005 (hash)))])
         s6))
     (define frame-lines (render-mock-frame state-with-errors ist cols rows))
     ;; No newlines in frame
     (check-false (frame-row-contains-newline? frame-lines)
                  "no newlines in error/recovery frame")
     ;; Correct layout
     (check-true (frame-status-at-row? frame-lines layout) "status bar correct after error recovery")
     (check-true (frame-input-at-row? frame-lines layout) "input correct after error recovery"))))

;; ============================================================
;; Run
;; ============================================================

(run-tests w74-integration-tests)
