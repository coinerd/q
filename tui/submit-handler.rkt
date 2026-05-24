#lang racket/base

;; q/tui/submit-handler.rkt — User input submission handler
;;
;; Extracted from tui-render-loop.rkt (W17) to reduce hotspot complexity
;; and fix the contract lie (was declared as returning tui-ctx?, actually
;; returns void since it mutates state box and spawns thread).
;;
;; Contract: (-> tui-ctx? string? void?)

(require racket/contract
         racket/hash
         "context.rkt"
         "state.rkt"
         "../agent/event-bus.rkt"
         "../util/event.rkt"
         "../agent/queue.rkt"
         (only-in "../runtime/session-lifecycle.rkt" write-crash-log!))

;; Handle a user submit: enqueue if busy, debounce duplicates, or run via runner.
;; Mutates ctx's ui-state-box and spawns a thread for the runner.
;; Returns void (was incorrectly declared as returning tui-ctx?).
(define (handle-user-submit! ctx text)
  ;; G3.1: If agent is busy, enqueue as followup instead of calling runner
  (define cur-state (unbox (tui-ctx-ui-state-box ctx)))
  (define busy? (ui-state-busy? cur-state))
  (define q (unbox (tui-ctx-session-queue-box ctx)))
  (cond
    [(and busy? q (queue? q))
     ;; Agent is streaming — enqueue as followup
     (enqueue-followup! q text)
     ;; Show system notification in transcript
     (define queued-entry
       (make-entry 'system
                   "[Queued — will run after current task]"
                   (current-inexact-milliseconds)
                   (hasheq 'queued #t)))
     (set-box! (tui-ctx-ui-state-box ctx) (add-transcript-entry cur-state queued-entry))
     (mark-dirty! ctx)]
    [else
     ;; Not busy (or no queue) — submit to runtime (non-blocking)
     ;; B2-D: Double-submit debounce — ignore rapid identical submits
     (define entries (ui-state-transcript cur-state))
     (define last-entry (and (pair? entries) (car entries)))
     (define is-duplicate
       (and last-entry
            (eq? (transcript-entry-kind last-entry) 'user)
            (string=? (transcript-entry-text last-entry) text)
            (< (- (current-inexact-milliseconds) (transcript-entry-timestamp last-entry)) 500)))
     (cond
       [is-duplicate
        (define dup-entry
          (make-entry 'system "[Duplicate input ignored]" (current-inexact-milliseconds) (hash)))
        (set-box! (tui-ctx-ui-state-box ctx) (add-transcript-entry cur-state dup-entry))
        (mark-dirty! ctx)]
       [else
        ;; F1: Immediately set busy? so status bar shows [thinking...]
        ;; before the LLM responds (was waiting for turn.started event)
        (set-box! (tui-ctx-ui-state-box ctx)
                  (clear-streaming (set-pending-tool-name (set-busy cur-state #t) #f)))
        (mark-dirty! ctx)
        ;; F1: Show user message in transcript immediately
        ;; (don't wait for JSONL events — the TUI transcript is display-only)
        (define user-entry (make-entry 'user text (current-inexact-milliseconds) (hash)))
        (set-box! (tui-ctx-ui-state-box ctx)
                  (add-transcript-entry (unbox (tui-ctx-ui-state-box ctx)) user-entry))
        (mark-dirty! ctx)
        (define runner (tui-ctx-session-runner ctx))
        ;; Wrap runner thread with exception handler to prevent TUI hang
        (thread
         (lambda ()
           (with-handlers
               ([exn:fail? (lambda (e)
                             (define bus (tui-ctx-event-bus ctx))
                             (define sid (ui-state-session-id (unbox (tui-ctx-ui-state-box ctx))))
                             ;; B3-A: Write crash log for unhandled exceptions
                             (write-crash-log! sid (exn-message e) "run-prompt")
                             (when (and bus sid)
                               (publish! bus
                                         (make-event
                                          "runtime.error"
                                          (current-inexact-milliseconds)
                                          sid
                                          #f
                                          (hasheq 'error (exn-message e) 'errorType 'internal-error)))
                               (publish! bus
                                         (make-event "turn.completed"
                                                     (current-inexact-milliseconds)
                                                     sid
                                                     #f
                                                     (hasheq 'reason "error")))))])
             (runner text))))])])
  (void))

(provide (contract-out [handle-user-submit! (-> tui-ctx? string? void?)]))
