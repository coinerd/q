#lang racket/base

;; q/tui/commands/runtime-control.rkt — Runtime control commands
;;   (compact, interrupt, retry, quit)
;;
;; Extracted from commands.rkt (W19) to thin the commands parent.

(require racket/hash
         "../state.rkt"
         "../../util/protocol-types.rkt"
         "../../agent/event-bus.rkt"
         "context.rkt")

;; Handle /compact — request compaction
(define (handle-compact-command cctx state)
  (define entry (make-system-entry "[compact requested]"))
  (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
  (when (cmd-ctx-event-bus cctx)
    (publish! (cmd-ctx-event-bus cctx)
              (make-event "compact.requested"
                          (inexact->exact (truncate (/ (current-inexact-milliseconds) 1000)))
                          (or (ui-state-session-id state) "")
                          #f
                          (hash))))
  'continue)

;; Handle /interrupt — interrupt current operation
(define (handle-interrupt-command cctx state)
  (when (cmd-ctx-event-bus cctx)
    (publish! (cmd-ctx-event-bus cctx)
              (make-event "interrupt.requested"
                          (inexact->exact (truncate (/ (current-inexact-milliseconds) 1000)))
                          (or (ui-state-session-id state) "")
                          #f
                          (hash))))
  (define entry (make-system-entry "[interrupt requested]"))
  (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
  'continue)

;; Handle /retry — resubmit last prompt
(define (handle-retry-command cctx state)
  (define last-prompt (unbox (cmd-ctx-last-prompt-box cctx)))
  (cond
    [last-prompt
     (define entry
       (make-entry 'system
                   (format "[retry: resubmitting]")
                   (current-inexact-milliseconds)
                   (hash)))
     (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
     ;; v0.14.2 Wave 2: Enrich retry with tool summary from previous turn
     (define tool-summary (get-last-turn-tool-summary (unbox (cmd-ctx-state-box cctx))))
     (define enriched-prompt
       (if tool-summary
           (format "~a\n\n[Context from previous attempt: ~a]" last-prompt tool-summary)
           last-prompt))
     (define runner (cmd-ctx-session-runner cctx))
     (when runner
       (thread
        (lambda ()
          (with-handlers ([exn:fail?
                           (lambda (e)
                             (define bus (cmd-ctx-event-bus cctx))
                             (define sid (ui-state-session-id (unbox (cmd-ctx-state-box cctx))))
                             (when (and bus sid)
                               (publish! bus
                                         (make-event "runtime.error"
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
            (runner enriched-prompt)))))]
    [else
     (define entry (make-error-entry "No previous prompt to retry."))
     (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))])
  'continue)

;; Handle /quit
(define (handle-quit-command cctx)
  (set-box! (cmd-ctx-running-box cctx) #f)
  'quit)

(provide handle-compact-command
         handle-interrupt-command
         handle-retry-command
         handle-quit-command)
