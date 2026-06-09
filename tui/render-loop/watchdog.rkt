#lang racket/base

;; q/tui/render-loop/watchdog.rkt — Busy-state watchdog for TUI
;;
;; Detects and force-clears stale busy states after a configurable timeout.
;; Extracted from tui-render-loop.rkt (v0.96.16, AX1-1).
;; STABILITY: internal

(require (only-in "../state.rkt"
                  ui-state-busy?
                  ui-state-busy-since
                  ui-state-streaming-text
                  set-busy
                  set-busy-since
                  set-status-message
                  set-pending-tool-name
                  clear-streaming
                  add-transcript-entry
                  make-entry))

(provide current-busy-watchdog-ms
         check-busy-watchdog)

(define current-busy-watchdog-ms (make-parameter (* 30 60 1000)))

(define (check-busy-watchdog state now-ms watchdog-ms)
  (if (and (ui-state-busy? state) (not (ui-state-streaming-text state)))
      (let ([since (ui-state-busy-since state)])
        (if (and since (> (- now-ms since) watchdog-ms))
            (let* ([cleared (set-busy-since
                             (set-status-message
                              (clear-streaming (set-pending-tool-name (set-busy state #f) #f))
                              "watchdog: busy timeout")
                             #f)]
                   [watchdog-entry
                    (make-entry 'system
                                "[Watchdog: busy state timed out — force-cleared after 30 min]"
                                now-ms
                                (hasheq 'watchdog #t))])
              (add-transcript-entry cleared watchdog-entry))
            #f))
      #f))
