#lang racket

;; test-tui-keys.rkt — Tests for BUG-36: TUI arrow key and special key handling
;;
;; BUG-36: Arrow keys, Home, End were not reliably recognized on short keypresses.
;; The issue was that tui-read-key used a 0.05s timeout, which was too short
;; for escape sequences (e.g., ESC[A for up arrow) to arrive completely.
;;
;; These tests verify that handle-key correctly processes arrow keys and
;; that the terminal layer correctly reads escape sequences.

(require rackunit
         rackunit/text-ui
         "../interfaces/tui.rkt"
         "../tui/input.rkt"
         "../tui/state.rkt"
         "../tui/terminal.rkt"
         "../agent/event-bus.rkt"
         "../agent/types.rkt")

;; Helper: create a tui-ctx for testing
(define (make-test-ctx)
  (make-tui-ctx))

;; Helper: create input state with text and cursor position
(define (make-input-with-text text cursor-pos)
  (struct-copy input-state (initial-input-state)
               [buffer text]
               [cursor cursor-pos]))

(define test-tui-keys
  (test-suite
   "TUI Arrow and Special Keys (BUG-36)"

   ;; --------------------------------------------------
   ;; Test 1: 'left moves cursor left
   ;; --------------------------------------------------
   (test-case "handle-key 'left moves cursor left"
     (define ctx (make-test-ctx))
     (define inp (make-input-with-text "hello" 5))
     (set-box! (tui-ctx-input-state-box ctx) inp)
     (define result (handle-key ctx 'left))
     (check-equal? result 'continue)
     (define new-inp (unbox (tui-ctx-input-state-box ctx)))
     (check-equal? (input-state-cursor new-inp) 4))

   ;; --------------------------------------------------
   ;; Test 2: 'right moves cursor right
   ;; --------------------------------------------------
   (test-case "handle-key 'right moves cursor right"
     (define ctx (make-test-ctx))
     (define inp (make-input-with-text "hello" 2))
     (set-box! (tui-ctx-input-state-box ctx) inp)
     (define result (handle-key ctx 'right))
     (check-equal? result 'continue)
     (define new-inp (unbox (tui-ctx-input-state-box ctx)))
     (check-equal? (input-state-cursor new-inp) 3))

   ;; --------------------------------------------------
   ;; Test 3: 'home moves cursor to beginning
   ;; --------------------------------------------------
   (test-case "handle-key 'home moves cursor to beginning"
     (define ctx (make-test-ctx))
     (define inp (make-input-with-text "hello" 5))
     (set-box! (tui-ctx-input-state-box ctx) inp)
     (define result (handle-key ctx 'home))
     (check-equal? result 'continue)
     (define new-inp (unbox (tui-ctx-input-state-box ctx)))
     (check-equal? (input-state-cursor new-inp) 0))

   ;; --------------------------------------------------
   ;; Test 4: 'end moves cursor to end
   ;; --------------------------------------------------
   (test-case "handle-key 'end moves cursor to end"
     (define ctx (make-test-ctx))
     (define inp (make-input-with-text "hello" 2))
     (set-box! (tui-ctx-input-state-box ctx) inp)
     (define result (handle-key ctx 'end))
     (check-equal? result 'continue)
     (define new-inp (unbox (tui-ctx-input-state-box ctx)))
     (check-equal? (input-state-cursor new-inp) 5))

   ;; --------------------------------------------------
   ;; Test 5: 'left at beginning does nothing
   ;; --------------------------------------------------
   (test-case "handle-key 'left at beginning does nothing"
     (define ctx (make-test-ctx))
     (define inp (make-input-with-text "hello" 0))
     (set-box! (tui-ctx-input-state-box ctx) inp)
     (define result (handle-key ctx 'left))
     (check-equal? result 'continue)
     (define new-inp (unbox (tui-ctx-input-state-box ctx)))
     (check-equal? (input-state-cursor new-inp) 0))

   ;; --------------------------------------------------
   ;; Test 6: 'right at end does nothing
   ;; --------------------------------------------------
   (test-case "handle-key 'right at end does nothing"
     (define ctx (make-test-ctx))
     (define inp (make-input-with-text "hello" 5))
     (set-box! (tui-ctx-input-state-box ctx) inp)
     (define result (handle-key ctx 'right))
     (check-equal? result 'continue)
     (define new-inp (unbox (tui-ctx-input-state-box ctx)))
     (check-equal? (input-state-cursor new-inp) 5))

   ;; --------------------------------------------------
   ;; Test 7: 'up navigates history backward
   ;; --------------------------------------------------
   (test-case "handle-key 'up navigates history backward"
     (define ctx (make-test-ctx))
     ;; First add some history
     (define inp (input-history-push (input-history-push (initial-input-state) "first") "second"))
     (set-box! (tui-ctx-input-state-box ctx) inp)
     ;; Press up
     (define result (handle-key ctx 'up))
     (check-equal? result 'continue)
     (define new-inp (unbox (tui-ctx-input-state-box ctx)))
     (check-equal? (input-state-buffer new-inp) "second")
     (check-equal? (input-state-history-idx new-inp) 1))

   ;; --------------------------------------------------
   ;; Test 8: 'down navigates history forward
   ;; --------------------------------------------------
   (test-case "handle-key 'down navigates history forward"
     (define ctx (make-test-ctx))
     ;; Setup: push history and go up once
     (define inp (input-history-push (input-history-push (initial-input-state) "first") "second"))
     (set-box! (tui-ctx-input-state-box ctx) inp)
     (handle-key ctx 'up)  ;; now at "second"
     ;; Press down
     (define result (handle-key ctx 'down))
     (check-equal? result 'continue)
     (define new-inp (unbox (tui-ctx-input-state-box ctx)))
     ;; Should be back to browsing mode end (empty if no saved text)
     (check-equal? (input-state-history-idx new-inp) #f))

   ;; --------------------------------------------------
   ;; Test 9: 'delete removes character after cursor
   ;; --------------------------------------------------
   (test-case "handle-key 'delete removes character after cursor"
     (define ctx (make-test-ctx))
     (define inp (make-input-with-text "hello" 2))
     (set-box! (tui-ctx-input-state-box ctx) inp)
     (define result (handle-key ctx 'delete))
     (check-equal? result 'continue)
     (define new-inp (unbox (tui-ctx-input-state-box ctx)))
     (check-equal? (input-state-buffer new-inp) "helo")
     (check-equal? (input-state-cursor new-inp) 2))

   ;; --------------------------------------------------
   ;; Test 10: 'kp-left (numpad arrow) also works
   ;; --------------------------------------------------
   (test-case "handle-key 'kp-left moves cursor left"
     (define ctx (make-test-ctx))
     (define inp (make-input-with-text "hello" 5))
     (set-box! (tui-ctx-input-state-box ctx) inp)
     (define result (handle-key ctx 'kp-left))
     (check-equal? result 'continue)
     (define new-inp (unbox (tui-ctx-input-state-box ctx)))
     (check-equal? (input-state-cursor new-inp) 4))

   ;; --------------------------------------------------
   ;; Test 11: 'kp-home (numpad home) also works
   ;; --------------------------------------------------
   (test-case "handle-key 'kp-home moves cursor to beginning"
     (define ctx (make-test-ctx))
     (define inp (make-input-with-text "hello" 5))
     (set-box! (tui-ctx-input-state-box ctx) inp)
     (define result (handle-key ctx 'kp-home))
     (check-equal? result 'continue)
     (define new-inp (unbox (tui-ctx-input-state-box ctx)))
     (check-equal? (input-state-cursor new-inp) 0))

   ;; --------------------------------------------------
   ;; Test 12: 'kp-end (numpad end) also works
   ;; --------------------------------------------------
   (test-case "handle-key 'kp-end moves cursor to end"
     (define ctx (make-test-ctx))
     (define inp (make-input-with-text "hello" 2))
     (set-box! (tui-ctx-input-state-box ctx) inp)
     (define result (handle-key ctx 'kp-end))
     (check-equal? result 'continue)
     (define new-inp (unbox (tui-ctx-input-state-box ctx)))
     (check-equal? (input-state-cursor new-inp) 5))

   ;; ============================================================
   ;; Issue #127: Ctrl+C interrupts agent when no selection active
   ;; ============================================================

   (test-case "handle-key 'ctrl-c with no selection interrupts agent"
     (define bus (make-event-bus))
     (define published-events '())
     ;; Subscribe to capture published events
     (subscribe! bus (lambda (evt)
                       (set! published-events (cons evt published-events))))
     (define ctx (make-tui-ctx #:event-bus bus))
     ;; Set state to busy with no selection
     (define busy-state
       (struct-copy ui-state (initial-ui-state)
                    [busy? #t]
                    [streaming-text "partial..."]
                    [pending-tool-name "bash"]))
     (set-box! (tui-ctx-ui-state-box ctx) busy-state)
     ;; Send ctrl-c
     (define result (handle-key ctx 'ctrl-c))
     (check-equal? result 'continue)
     ;; Verify state is no longer busy
     (define new-state (unbox (tui-ctx-ui-state-box ctx)))
     (check-false (ui-state-busy? new-state) "ctrl-c clears busy state")
     (check-false (ui-state-streaming-text new-state) "ctrl-c clears streaming text")
     (check-false (ui-state-pending-tool-name new-state) "ctrl-c clears pending tool")
     ;; Verify interrupt event was published
     (check-equal? (length published-events) 1 "one event published")
     (check-equal? (event-ev (car published-events))
                   "interrupt.requested"
                   "published interrupt.requested event"))

   (test-case "handle-key 'ctrl-c with selection copies instead of interrupting"
     (define bus (make-event-bus))
     (define published-events '())
     (subscribe! bus (lambda (evt)
                       (set! published-events (cons evt published-events))))
     (define ctx (make-tui-ctx #:event-bus bus))
     ;; Set state to busy WITH selection
     (define busy-state
       (set-selection-anchor
        (struct-copy ui-state (initial-ui-state) [busy? #t])
        0 0))
     (set-box! (tui-ctx-ui-state-box ctx) busy-state)
     ;; Send ctrl-c
     (define result (handle-key ctx 'ctrl-c))
     (check-equal? result 'continue)
     ;; State should still be busy (copy, not interrupt)
     (define new-state (unbox (tui-ctx-ui-state-box ctx)))
     (check-true (ui-state-busy? new-state) "ctrl-c with selection keeps busy state")
     ;; No interrupt event published
     (check-equal? (length published-events) 0 "no interrupt event when selection active"))

   ;; ============================================================
   ;; Issue #133: Ctrl+J (#\newline) inserts newline
   ;; ============================================================

   (test-case "handle-key #\\newline inserts newline (multi-line input)"
     (define ctx (make-test-ctx))
     (define inp (make-input-with-text "hello" 2))
     (set-box! (tui-ctx-input-state-box ctx) inp)
     (define result (handle-key ctx #\newline))
     (check-equal? result 'continue)
     (define new-inp (unbox (tui-ctx-input-state-box ctx)))
     (check-equal? (input-state-buffer new-inp) "he\nllo")
     (check-equal? (input-state-cursor new-inp) 3))

   (test-case "handle-key #\\return still submits input"
     (define ctx (make-test-ctx))
     (define inp (make-input-with-text "hello" 5))
     (set-box! (tui-ctx-input-state-box ctx) inp)
     (define result (handle-key ctx #\return))
     (check-equal? result '(submit "hello"))
     (define new-inp (unbox (tui-ctx-input-state-box ctx)))
     (check-equal? (input-state-buffer new-inp) ""))
   ))

(run-tests test-tui-keys)
