#lang racket/base

;; q/tui/render-loop/watchdog.rkt — Busy-state watchdog for TUI
;;
;; Detects and force-clears stale busy states after a configurable timeout.
;; Extracted from tui-render-loop.rkt (v0.96.16, AX1-1).
;; BF1b (v0.99.4): Added streaming stall detection — clears busy+streaming
;; states when no delta has been received for current-streaming-watchdog-ms.
;; STABILITY: internal

(require (only-in "../state.rkt"
                  ui-state-busy?
                  ui-state-busy-since
                  ui-state-streaming-text
                  ui-state-last-delta-ms
                  set-busy
                  set-busy-since
                  set-status-message
                  set-pending-tool-name
                  clear-streaming
                  add-transcript-entry
                  make-entry))

(provide current-busy-watchdog-ms
         current-streaming-watchdog-ms
         check-busy-watchdog)

(define current-busy-watchdog-ms (make-parameter (* 5 60 1000)))
;; B2 fix: 5 min covers 2-3 bash timeouts (120s each), network delays

(define current-streaming-watchdog-ms (make-parameter (* 3 60 1000)))
;; BF1b (v0.99.4): 3 min — if no stream delta for 3 minutes while
;; streaming-text is non-#f, the stream has stalled. Clear it.

(define (check-busy-watchdog state now-ms watchdog-ms)
  (cond
    ;; Case 1: Non-streaming busy timeout (existing logic)
    [(and (ui-state-busy? state) (not (ui-state-streaming-text state)))
     (let ([since (ui-state-busy-since state)])
       (if (and since (> (- now-ms since) watchdog-ms))
           (let* ([cleared (set-busy-since (set-status-message (clear-streaming (set-pending-tool-name
                                                                                 (set-busy state #f)
                                                                                 #f))
                                                               "watchdog: busy timeout")
                                           #f)]
                  [watchdog-entry
                   (make-entry 'system
                               (format "[Watchdog: busy state timed out — force-cleared after ~a min]"
                                       (/ watchdog-ms 60000))
                               now-ms
                               (hasheq 'watchdog #t))])
             (add-transcript-entry cleared watchdog-entry))
           #f))]
    ;; Case 2: BF1b (v0.99.4) — Streaming stall detection
    ;; When busy and streaming-text exists but no delta received for
    ;; current-streaming-watchdog-ms, the stream has stalled.
    [(and (ui-state-busy? state) (ui-state-streaming-text state))
     (let* ([last-delta (ui-state-last-delta-ms state)]
            [swd-ms (current-streaming-watchdog-ms)])
       (if (and last-delta (> (- now-ms last-delta) swd-ms))
           (let* ([cleared (set-busy-since (set-status-message (clear-streaming (set-pending-tool-name
                                                                                 (set-busy state #f)
                                                                                 #f))
                                                               "watchdog: streaming stall")
                                           #f)]
                  [watchdog-entry
                   (make-entry 'system
                               (format "[Watchdog: streaming stalled — force-cleared after ~a min]"
                                       (/ swd-ms 60000))
                               now-ms
                               (hasheq 'watchdog #t 'stall #t))])
             (add-transcript-entry cleared watchdog-entry))
           #f))]
    [else #f]))
