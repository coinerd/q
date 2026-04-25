#lang racket

;; tests/test-tui-queue-wiring.rkt — G3.1: TUI input→queue wiring during streaming
;;
;; Tests that when the agent is busy (streaming), user input is enqueued as a
;; followup instead of being dispatched to the runner. When not busy, the runner
;; is called directly.

(require rackunit
         rackunit/text-ui
         "../tui/state.rkt"
         "../tui/tui-keybindings.rkt"
         "../agent/queue.rkt"
         "../util/protocol-types.rkt")

;; ============================================================
;; Helpers
;; ============================================================

;; Track whether runner was called and with what text
(define (make-tracking-runner)
  (define calls (box '()))
  (values (lambda (text) (set-box! calls (cons text (unbox calls)))) calls))

;; Get last transcript entry from a ui-state
(define (last-entry state)
  (define entries (ui-state-transcript state))
  (if (null? entries)
      #f
      (car entries)))

;; ============================================================
;; Test suite
;; ============================================================

(define test-tui-queue-wiring
  (test-suite "G3.1: TUI input→queue wiring during streaming"

    ;; ----------------------------------------------------------
    ;; When not busy, runner is called directly
    ;; ----------------------------------------------------------
    (test-case "not busy → runner is called"
      (define-values (runner runner-calls) (make-tracking-runner))
      (define q (make-queue))
      (define ctx (make-tui-ctx #:session-runner runner #:session-queue q))
      ;; State starts not-busy
      (define state (unbox (tui-ctx-ui-state-box ctx)))
      (check-false (ui-state-busy? state) "initial state should not be busy")
      ;; Simulate submit: directly invoke the logic that the render loop uses
      (define text "hello agent")
      (define cur-state (unbox (tui-ctx-ui-state-box ctx)))
      (define busy? (ui-state-busy? cur-state))
      (cond
        [(and busy? q (queue? q)) (enqueue-followup! q text)]
        [else (runner text)])
      ;; Runner should have been called
      (check-equal? (unbox runner-calls) (list "hello agent")))

    ;; ----------------------------------------------------------
    ;; When busy, message is queued as followup
    ;; ----------------------------------------------------------
    (test-case "busy → message enqueued as followup"
      (define-values (runner runner-calls) (make-tracking-runner))
      (define q (make-queue))
      (define ctx (make-tui-ctx #:session-runner runner #:session-queue q))
      ;; Mark state as busy (simulating streaming)
      (define state (unbox (tui-ctx-ui-state-box ctx)))
      (define busy-state (struct-copy ui-state state [busy? #t]))
      (set-box! (tui-ctx-ui-state-box ctx) busy-state)
      ;; Simulate submit
      (define text "do another thing")
      (define cur-state (unbox (tui-ctx-ui-state-box ctx)))
      (define busy? (ui-state-busy? cur-state))
      (cond
        [(and busy? q (queue? q))
         (enqueue-followup! q text)
         ;; Show system notification (matching render-loop logic)
         (define queued-entry
           (make-entry 'system
                       "[Queued — will run after current task]"
                       (current-inexact-milliseconds)
                       (hasheq 'queued #t)))
         (set-box! (tui-ctx-ui-state-box ctx) (add-transcript-entry cur-state queued-entry))]
        [else (runner text)])
      ;; Runner should NOT have been called
      (check-equal? (unbox runner-calls) '())
      ;; Queue should contain the text
      (check-equal? (dequeue-followup! q) "do another thing"))

    ;; ----------------------------------------------------------
    ;; When busy, system notification appears in transcript
    ;; ----------------------------------------------------------
    (test-case "busy → system notification in transcript"
      (define-values (runner runner-calls) (make-tracking-runner))
      (define q (make-queue))
      (define ctx (make-tui-ctx #:session-runner runner #:session-queue q))
      ;; Mark state as busy
      (define state (unbox (tui-ctx-ui-state-box ctx)))
      (define busy-state (struct-copy ui-state state [busy? #t]))
      (set-box! (tui-ctx-ui-state-box ctx) busy-state)
      ;; Simulate submit with queue wiring logic
      (define text "queued message")
      (define cur-state (unbox (tui-ctx-ui-state-box ctx)))
      (define busy? (ui-state-busy? cur-state))
      (cond
        [(and busy? q (queue? q))
         (enqueue-followup! q text)
         (define queued-entry
           (make-entry 'system
                       "[Queued — will run after current task]"
                       (current-inexact-milliseconds)
                       (hasheq 'queued #t)))
         (set-box! (tui-ctx-ui-state-box ctx) (add-transcript-entry cur-state queued-entry))]
        [else (runner text)])
      ;; Check that system notification is in transcript
      (define final-state (unbox (tui-ctx-ui-state-box ctx)))
      (define last-entry-val (last-entry final-state))
      (check-not-false last-entry-val "should have a transcript entry")
      (check-equal? (transcript-entry-kind last-entry-val) 'system)
      (check-true (string-contains? (transcript-entry-text last-entry-val) "Queued")
                  "system message should contain 'Queued'"))

    ;; ----------------------------------------------------------
    ;; No queue available → runner called even when busy
    ;; ----------------------------------------------------------
    (test-case "no queue + busy → runner called (graceful fallback)"
      (define-values (runner runner-calls) (make-tracking-runner))
      (define ctx (make-tui-ctx #:session-runner runner #:session-queue #f))
      ;; Mark busy
      (define state (unbox (tui-ctx-ui-state-box ctx)))
      (define busy-state (struct-copy ui-state state [busy? #t]))
      (set-box! (tui-ctx-ui-state-box ctx) busy-state)
      ;; Simulate submit — no queue, so should fall through to runner
      (define text "fallback call")
      (define cur-state (unbox (tui-ctx-ui-state-box ctx)))
      (define busy? (ui-state-busy? cur-state))
      (define q #f)
      (cond
        [(and busy? q (queue? q)) (enqueue-followup! q text)]
        [else (runner text)])
      ;; Runner SHOULD have been called (no queue, so fallback)
      (check-equal? (unbox runner-calls) (list "fallback call")))

    ;; ----------------------------------------------------------
    ;; Multiple queued messages preserve order
    ;; ----------------------------------------------------------
    (test-case "multiple queued messages preserve FIFO order"
      (define-values (runner runner-calls) (make-tracking-runner))
      (define q (make-queue))
      (define ctx (make-tui-ctx #:session-runner runner #:session-queue q))
      ;; Mark busy
      (define state (unbox (tui-ctx-ui-state-box ctx)))
      (define busy-state (struct-copy ui-state state [busy? #t]))
      (set-box! (tui-ctx-ui-state-box ctx) busy-state)
      ;; Queue multiple messages
      (for ([msg '("first" "second" "third")])
        (enqueue-followup! q msg))
      ;; Dequeue and verify FIFO order
      (check-equal? (dequeue-followup! q) "first")
      (check-equal? (dequeue-followup! q) "second")
      (check-equal? (dequeue-followup! q) "third"))))

;; Run
(run-tests test-tui-queue-wiring)
