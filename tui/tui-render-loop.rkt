#lang racket/base

;; q/tui/tui-render-loop.rkt — Main render loop, cell-buffer lifecycle, event draining
;;
;; Extracted from interfaces/tui.rkt for modularity (Issue #194).
;;
;; Dependency chain: tui-init.rkt → tui-render-loop.rkt → tui-keybindings.rkt
;;
;; Provides:
;;   render-ubuf-to-terminal! — cell-buffer → terminal output with SGR fix
;;   tui-ctx-init-terminal!    — terminal + cell-buffer initialization
;;   tui-ctx-resize-ubuf!      — resize cell-buffer on terminal resize
;;   tui-ctx-ubuf, tui-ctx-term — ubuf/term accessors
;;   render-frame!, draw-frame  — frame rendering
;;   next-message               — terminal message adapter
;;   tui-main-loop              — main event loop
;;   drain-events!              — runtime event draining
;;   Re-exports: fix-sgr-bg-black, decode-mouse-x10

(require "../util/error-helpers.rkt")
(require racket/contract
         racket/bytes
         "../tui/terminal.rkt"
         "../tui/state.rkt"
         "../tui/input.rkt"
         "../tui/sgr.rkt"
         (prefix-in renderer: "../tui/renderer.rkt")
         "../tui/layout.rkt"
         "../tui/frame-diff.rkt"
         "../tui/cell-buffer.rkt"
         "../tui/cell-diff.rkt"
         "../tui/cell-diff-render.rkt"
         "../util/protocol-types.rkt"
         "../agent/event-bus.rkt"
         "../tui/tui-keybindings.rkt"
         "../tui/terminal-input.rkt"
         "../util/output-guard.rkt"
         "../agent/queue.rkt"
         (only-in "../runtime/session-lifecycle.rkt" write-crash-log!)
         "../tui/tree-view.rkt"
         ;; W17: handle-user-submit! extracted to submit-handler.rkt
         "submit-handler.rkt"
         ;; W18: process-tui-message! extracted to message-dispatch.rkt
         "message-dispatch.rkt")

(define (tui-output-port)
  (or (guarded-real-output-port) (current-output-port)))

;; ── Ubuf/terminal lifecycle ──
(provide fix-sgr-bg-black
         decode-mouse-x10
         decode-mouse-tui-term
         use-cell-diff?
         current-busy-watchdog-ms
         (contract-out [check-busy-watchdog (-> any/c number? number? (or/c any/c #f))]
                       [render-ubuf-to-terminal! (-> tui-ctx? void?)]
                       [tui-ctx-init-terminal! (-> tui-ctx? void?)]
                       [tui-ctx-resize-ubuf! (-> tui-ctx? void?)]
                       [tui-ctx-ubuf (-> tui-ctx? any/c)]
                       [tui-ctx-term (-> tui-ctx? any/c)]
                       [render-frame! (-> tui-ctx? void?)]
                       [draw-frame (-> tui-ctx? void?)]
                       [next-message (->* (tui-ctx?) (#:timeout number?) (or/c any/c #f))]
                       [tui-main-loop (-> tui-ctx? void?)]
                       [drain-events! (-> tui-ctx? void?)])
         ;; W17: handle-user-submit! extracted to submit-handler.rkt
         (all-from-out "submit-handler.rkt"))

;; ============================================================
;; Cell buffer (native)
;; ============================================================

;; Minimum render interval in milliseconds.
;; Coalesces rapid state changes (e.g., streaming) into single frames.
;; 16ms ≈ 60fps, prevents flicker during fast streaming output.
(define MIN-RENDER-INTERVAL-MS 16)

;; Render mode: #t = cell-level diffing, #f = row-level frame diffing
(define use-cell-diff? (make-parameter #t))

;; Track last render timestamp for debouncing.
(define last-render-ms (box 0.0))

;; Native cell-buffer operations (no dynamic-require)
(define make-ubuf make-cell-buffer)
(define ubuf-clear! cell-buffer-clear!)
(define ubuf-putstring! cell-buffer-putstring!)

;; ============================================================
;; Render ubuf to terminal
;; ============================================================

;; Render cell buffer to terminal.
;; Iterates through cells and emits minimal ANSI sequences.
;; Uses DEC 2026 (Synchronized Output) when available.
(define (render-ubuf-to-terminal! ctx)
  (define ubuf (tui-ctx-ubuf ctx))
  (define cols (cell-buffer-cols ubuf))
  (define rows (cell-buffer-rows ubuf))
  (define out (tui-output-port))
  (terminal-sync-begin!)
  ;; Move cursor to origin and clear screen
  (display "\x1b[H" out)
  (for ([row (in-range rows)])
    (when (> row 0)
      (newline out))
    (define prev-fg #f)
    (define prev-bg #f)
    (define prev-bold? #f)
    (define prev-underline? #f)
    (for ([col (in-range cols)])
      (define cell (cell-buffer-ref ubuf col row))
      (define fg (cell-fg cell))
      (define bg (cell-bg cell))
      (define bold? (cell-bold? cell))
      (define underline? (cell-underline? cell))
      ;; Only emit SGR when attributes change
      (unless (and (= fg prev-fg)
                   (= bg prev-bg)
                   (eq? bold? prev-bold?)
                   (eq? underline? prev-underline?))
        (display "\x1b[0" out)
        (when bold?
          (display ";1" out))
        (when underline?
          (display ";4" out))
        (display (format ";38;5;~a" fg) out)
        (display (format ";48;5;~a" (if (= bg 0) 16 bg)) out)
        (display "m" out)
        (set! prev-fg fg)
        (set! prev-bg bg)
        (set! prev-bold? bold?)
        (set! prev-underline? underline?))
      (display (string (cell-char cell)) out)))
  (display "\x1b[0m" out)
  (terminal-sync-end!))

;; ============================================================
;; Terminal/ubuf lifecycle helpers
;; ============================================================

;; Initialize terminal and ubuf for a context
(define (tui-ctx-init-terminal! ctx)
  (define term (tui-term-open))
  (set-box! (tui-ctx-term-box ctx) term)
  (define-values (cols rows) (tui-screen-size))
  (define ubuf (make-ubuf cols rows))
  (set-box! (tui-ctx-ubuf-box ctx) ubuf)
  ;; Configure renderer to use real ubuf operations
  (renderer:current-ubuf-clear ubuf-clear!)
  (renderer:current-ubuf-putstring ubuf-putstring!)
  ;; Enable mouse tracking for scroll wheel support
  (enable-mouse-tracking)
  ;; Detect synchronized output support
  (detect-sync-mode-support!)
  (void))

;; Resize ubuf when terminal size changes
(define (tui-ctx-resize-ubuf! ctx)
  (define-values (cols rows) (tui-screen-size))
  (define ubuf (make-ubuf cols rows))
  (set-box! (tui-ctx-ubuf-box ctx) ubuf)
  ;; Clear previous frame on resize to force full redraw
  (set-box! (tui-ctx-previous-frame-box ctx) #f))

;; Get current ubuf from context
(define (tui-ctx-ubuf ctx)
  (unbox (tui-ctx-ubuf-box ctx)))

;; Get current term from context
(define (tui-ctx-term ctx)
  (unbox (tui-ctx-term-box ctx)))

;; ============================================================
;; Frame rendering (ubuf-based)
;; ============================================================

;; Deep copy a single cell vector
(define (vector-copy-cell c)
  (vector (vector-ref c 0)
          (vector-ref c 1)
          (vector-ref c 2)
          (vector-ref c 3)
          (vector-ref c 4)
          (vector-ref c 5)
          (vector-ref c 6)))

;; Render the complete frame to the terminal using ubuf.
;; Hides cursor during redraw to prevent flicker, shows after.
;; Clears needs-redraw flag after drawing.
(define (render-frame! ctx)
  (define state (unbox (tui-ctx-ui-state-box ctx)))
  (define inp (unbox (tui-ctx-input-state-box ctx)))
  (define ubuf (tui-ctx-ubuf ctx))
  (define term (tui-ctx-term ctx))

  (define-values (cols rows) (tui-screen-size))
  (define widget-lines (get-widget-lines-above state))
  (define layout
    (compute-layout rows
                    cols
                    #:widget-bar-h (length widget-lines)
                    #:has-widgets? (positive? (length widget-lines))))

  ;; Render to ubuf (returns cursor position, state, frame lines)
  (define-values (cursor-col cursor-row state* frame-lines)
    (renderer:render-frame! ubuf state inp layout))

  ;; Write back state with updated render cache
  (set-box! (tui-ctx-ui-state-box ctx) state*)

  ;; Diff-based terminal output
  (tui-cursor-hide)
  (if (use-cell-diff?)
      ;; Cell-level incremental diff
      (let ()
        (define prev-ubuf (unbox (tui-ctx-prev-ubuf-box ctx)))
        (define out (tui-output-port))
        (render-smart! prev-ubuf ubuf out #:sync? #t)
        ;; Store snapshot for next diff
        (define snapshot (make-cell-buffer (cell-buffer-cols ubuf) (cell-buffer-rows ubuf)))
        (for* ([r (in-range (cell-buffer-rows ubuf))]
               [c (in-range (cell-buffer-cols ubuf))])
          (define idx (+ (* r (cell-buffer-cols ubuf)) c))
          (vector-set! (cell-buffer-cells snapshot)
                       idx
                       ((lambda (v)
                          (build-vector (vector-length v)
                                        (lambda (i)
                                          (let ([c (vector-ref v i)])
                                            (vector (vector-ref c 0)
                                                    (vector-ref c 1)
                                                    (vector-ref c 2)
                                                    (vector-ref c 3)
                                                    (vector-ref c 4)
                                                    (vector-ref c 5)
                                                    (vector-ref c 6))))))
                        (vector-ref (cell-buffer-cells ubuf) idx))))
        (set-box! (tui-ctx-prev-ubuf-box ctx) snapshot))
      ;; Row-level frame diff (fallback)
      (let ()
        (define prev-frame (unbox (tui-ctx-previous-frame-box ctx)))
        (define diffs (diff-frames prev-frame frame-lines))
        (cond
          [(null? diffs) (void)]
          [(and (= (length diffs) 1) (eq? (diff-cmd-type (car diffs)) 'full))
           (render-ubuf-to-terminal! ctx)]
          [else
           (for ([cmd (in-list diffs)])
             (case (diff-cmd-type cmd)
               [(write)
                (tui-cursor 1 (+ (diff-cmd-row cmd) 1))
                (display "\x1b[2K" (tui-output-port))
                (display (fix-sgr-bg-black (diff-cmd-content cmd)) (tui-output-port))]
               [(clear-from)
                (tui-cursor 1 (+ (diff-cmd-row cmd) 1))
                (display "\x1b[J" (tui-output-port))]
               [else (void)]))
           (tui-flush)])
        (set-box! (tui-ctx-previous-frame-box ctx) frame-lines)))

  ;; Position cursor at input location (renderer returns 0-indexed, ANSI is 1-indexed)
  (tui-cursor (+ cursor-col 1) (+ cursor-row 1))
  ;; Emit IME cursor marker for CJK input support
  ;; Uses APC protocol — silently ignored by terminals that don't understand it
  (display CURSOR-MARKER (tui-output-port))
  (tui-cursor-show)
  (tui-flush)
  (set-box! last-render-ms (current-inexact-milliseconds))
  (set-box! (tui-ctx-needs-redraw-box ctx) #f))

;; Deprecated: Old draw-frame is now an alias for render-frame!
;; Use render-frame! for new code.
(define (draw-frame ctx)
  (render-frame! ctx))

;; ============================================================
;; Message adapter
;; ============================================================

;; Read the next message from the terminal.
;; This is the ONLY place that touches terminal input details.
;; All other code (update, handle-key) works with the abstract message.
(define (next-message ctx #:timeout [timeout 0.05])
  (define msg (tui-read-key #:timeout timeout))
  (cond
    ;; No input available
    [(not msg) #f]
    ;; Paste event (bracketed paste DEC 2004)
    [(paste-event? msg) (list 'paste (vector-ref msg 1))]
    ;; Key message
    [(tkeymsg? msg)
     (define keycode (tui-keycode msg))
     (if keycode
         (list 'key keycode)
         #f)]
    ;; Resize message
    [(tsizemsg? msg) (list 'resize (tsizemsg-cols msg) (tsizemsg-rows msg))]
    ;; Mouse message — native vector dispatch
    [(tmousemsg? msg) (decode-mouse-tui-term msg)]
    ;; Command message (redraw, etc.)
    [(tcmdmsg? msg)
     (case (tcmdmsg-cmd msg)
       [(redraw) (list 'redraw)]
       [else #f])]
    ;; Unknown message type
    [else #f]))

;; ============================================================
;; Main TUI loop
;; ============================================================

;; handle-user-submit! extracted to submit-handler.rkt (W17)
;; Re-exported via (all-from-out "submit-handler.rkt")

;; v0.45.12 L3: Configurable via parameter so tests can override it.
;; Default: 30 minutes. When busy? stays true for this long without
;; a turn.completed event, the watchdog force-clears it.
(define current-busy-watchdog-ms (make-parameter (* 30 60 1000)))

;; v0.45.12 L4: Extracted watchdog check to a testable pure function.
;; v0.45.14: Added streaming guard — don't fire if agent is actively streaming.
;; Returns #f if no action needed, or the updated state if watchdog fires.
(define (check-busy-watchdog state now-ms watchdog-ms)
  (if (and (ui-state-busy? state)
           (not (ui-state-streaming-text state))) ;; agent is actively streaming — don't fire
      (let ([since (ui-state-busy-since state)])
        (if (and since (> (- now-ms since) watchdog-ms))
            (let* ([cleared (set-status-message
                             (clear-streaming (set-pending-tool-name (set-busy state #f) #f))
                             "watchdog: busy timeout")]
                   [watchdog-entry
                    (make-entry 'system
                                "[Watchdog: busy state timed out — force-cleared after 30 min]"
                                now-ms
                                (hasheq 'watchdog #t))])
              (add-transcript-entry cleared watchdog-entry))
            #f))
      #f))

(define (tui-main-loop ctx)
  (let loop ()
    ;; Drain all pending events from the channel (non-blocking)
    (drain-events! ctx)

    ;; Busy-state watchdog — force-clear stale busy state (v0.45.12 L3+L4)
    (define cur-state (unbox (tui-ctx-ui-state-box ctx)))
    (define watchdog-result
      (check-busy-watchdog cur-state (current-inexact-milliseconds) (current-busy-watchdog-ms)))
    (when watchdog-result
      (set-box! (tui-ctx-ui-state-box ctx) watchdog-result)
      (mark-dirty! ctx))

    ;; Poll for terminal resize (fallback for stub path where SIGWINCH
    ;; doesn't produce tsizemsg events).
    (when (tui-screen-size-changed?)
      (tui-ctx-resize-ubuf! ctx)
      (mark-dirty! ctx))

    ;; Draw the frame only when state changed (with debouncing)
    (when (unbox (tui-ctx-needs-redraw-box ctx))
      (define now (current-inexact-milliseconds))
      (define elapsed (- now (unbox last-render-ms)))
      (cond
        [(< elapsed MIN-RENDER-INTERVAL-MS)
         ;; Too soon — sleep the remainder, then render
         (sleep (/ (- MIN-RENDER-INTERVAL-MS elapsed) 1000.0))
         ;; Re-check in case resize happened during sleep
         (when (unbox (tui-ctx-needs-redraw-box ctx))
           (render-frame! ctx))]
        ;; Enough time elapsed — render immediately
        [else (render-frame! ctx)]))

    (when (unbox (tui-ctx-running-box ctx))
      ;; Get next message from terminal (adapter pattern)
      (define msg (next-message ctx #:timeout 0.05))

      (when msg
        (define result (process-tui-message! ctx msg))
        (when (eq? result 'resize)
          (tui-ctx-resize-ubuf! ctx)
          (tui-screen-size-cache-reset!)
          (mark-dirty! ctx)))

      (when (unbox (tui-ctx-running-box ctx))
        (loop)))))

;; Drain all pending events from the channel and apply them to ui-state.
;; Uses sync/timeout for non-blocking drain.
;; Sets needs-redraw when events are processed.
(define (drain-events! ctx)
  (define ch (tui-ctx-event-ch ctx))
  (let loop ()
    (define evt (sync/timeout 0 ch))
    (when evt
      (define state (unbox (tui-ctx-ui-state-box ctx)))
      (with-handlers ([exn:fail? (lambda (e)
                                   (log-warning "TUI: error processing event: ~a" (exn-message e)))])
        (set-box! (tui-ctx-ui-state-box ctx) (apply-event-to-state state evt)))
      (set-box! (tui-ctx-needs-redraw-box ctx) #t)
      (loop))))
