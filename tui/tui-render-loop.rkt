#lang racket/base

;; q/tui/tui-render-loop.rkt — Main render loop, cell-buffer lifecycle, event draining
;;
;; Extracted from interfaces/tui.rkt for modularity (Issue #194).
;;
;; Dependency chain: tui-init.rkt → tui-render-loop.rkt → tui-keybindings.rkt
;;
;; Provides:
;;   tui-ctx-init-terminal!    — terminal + cell-buffer initialization
;;   tui-ctx-resize-ubuf!      — resize cell-buffer on terminal resize
;;   tui-ctx-ubuf, tui-ctx-term — ubuf/term accessors
;;   render-frame!, draw-frame  — frame rendering (vdom + cell-diff)
;;   next-message               — terminal message adapter
;;   tui-main-loop              — main event loop
;;   drain-events!              — runtime event draining
;;   Re-exports: fix-sgr-bg-black, decode-mouse-x10

(require "../util/error-helpers.rkt")
(require racket/contract
         racket/bytes
         racket/string
         racket/list
         "../tui/terminal.rkt"
         "../tui/state.rkt"
         "../tui/input.rkt"
         "../tui/sgr.rkt"
         (prefix-in renderer: "../tui/renderer.rkt")
         "../tui/layout.rkt"
         "../tui/cell-buffer.rkt"
         "../tui/cell-diff.rkt"
         "../tui/cell-diff-render.rkt"
         "../tui/vdom-render.rkt"
         (prefix-in vdom-comp: "../tui/vdom-components.rkt")
         (only-in "../tui/component.rkt" q-component-render-fn)
         (only-in "../tui/render.rkt"
                  render-transcript
                  render-status-bar
                  render-input-line
                  styled-line->ansi
                  styled-line->text
                  plain-line
                  apply-selection-highlight)
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
         decode-mouse-message
         current-busy-watchdog-ms
         (contract-out [check-busy-watchdog (-> any/c number? number? (or/c any/c #f))]
                       [tui-ctx-init-terminal! (-> tui-ctx? void?)]
                       [tui-ctx-resize-ubuf! (-> tui-ctx? void?)]
                       [tui-ctx-ubuf (-> tui-ctx? any/c)]
                       [tui-ctx-term (-> tui-ctx? any/c)]
                       [render-frame! (-> tui-ctx? void?)]
                       [draw-frame (-> tui-ctx? void?)]
                       [render-frame-vdom!
                        (-> any/c
                            ui-state?
                            input-state?
                            hash?
                            (values exact-nonnegative-integer?
                                    exact-nonnegative-integer?
                                    ui-state?
                                    (listof string?)))]
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

;; Track last render timestamp for debouncing.
(define last-render-ms (box 0.0))

;; Native cell-buffer operations (no dynamic-require)

;; ============================================================
;; Terminal/ubuf lifecycle helpers
;; ============================================================

;; Initialize terminal and ubuf for a context
(define (tui-ctx-init-terminal! ctx)
  (define term (tui-term-open))
  (set-box! (tui-ctx-term-box ctx) term)
  (define-values (cols rows) (tui-screen-size))
  (define ubuf (make-cell-buffer cols rows))
  (set-box! (tui-ctx-ubuf-box ctx) ubuf)
  ;; Configure renderer to use cell-buffer operations
  (renderer:current-ubuf-clear cell-buffer-clear!)
  (renderer:current-ubuf-putstring cell-buffer-putstring!)
  ;; Enable mouse tracking for scroll wheel support
  (enable-mouse-tracking)
  ;; Detect synchronized output support
  (detect-sync-mode-support!)
  (void))

;; Resize ubuf when terminal size changes
(define (tui-ctx-resize-ubuf! ctx)
  (define-values (cols rows) (tui-screen-size))
  (define ubuf (make-cell-buffer cols rows))
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
;; Frame rendering (cell-buffer-based)
;; ============================================================

;; Render the complete frame to the terminal using ubuf.
;; Hides cursor during redraw to prevent flicker, shows after.
;; Clears needs-redraw flag after drawing.
(define (render-frame-vdom! ubuf ui-state input-st layout)
  ;; Render a complete frame using the vdom pipeline.
  (define header-region (layout-header layout))
  (define transcript-region (layout-transcript layout))
  (define input-region (layout-input layout))
  (define cols (layout-region-width header-region))
  (define rows (+ (layout-region-y input-region) (layout-region-height input-region)))
  (define header-row (layout-region-y header-region))
  (define transcript-start-row (layout-region-y transcript-region))
  (define transcript-height (layout-region-height transcript-region))
  (define status-y (layout-region-y input-region))
  (define input-y (min (sub1 rows) (add1 status-y)))

  ;; 1. Clear the buffer
  (cell-buffer-clear! ubuf)

  ;; 2. Draw header row via vdom component
  (when header-row
    (define header-comp (vdom-comp:make-header-vdom-component))
    (define header-vnodes ((q-component-render-fn header-comp) ui-state cols))
    (render-vdom-section-to-buffer! header-vnodes ubuf cols header-row 1))

  ;; 3. Render transcript → styled-lines → vnodes → cell-buffer
  (define-values (trans-lines-raw ui-state*) (render-transcript ui-state transcript-height cols))
  (define visible-lines-raw
    (if (> (length trans-lines-raw) transcript-height)
        (take-right trans-lines-raw transcript-height)
        trans-lines-raw))
  (define pad-count (- transcript-height (length visible-lines-raw)))
  (define sel (ui-state-selection ui-state))
  (define sel-anchor (selection-state-anchor sel))
  (define sel-end (selection-state-end sel))
  (define trans-lines
    (if (and sel-anchor sel-end)
        (apply-selection-highlight visible-lines-raw
                                   sel-anchor
                                   sel-end
                                   transcript-start-row
                                   pad-count)
        visible-lines-raw))
  ;; Convert styled-lines to vnodes and render via section renderer
  (define trans-pad-vnodes
    (for/list ([_ (in-range pad-count)])
      (vdom-comp:styled-line->vnode (plain-line ""))))
  (define trans-content-vnodes (vdom-comp:styled-lines->vnodes trans-lines))
  (render-vdom-section-to-buffer! (append trans-pad-vnodes trans-content-vnodes)
                                  ubuf
                                  cols
                                  transcript-start-row)

  ;; 4. Draw widget lines via vdom section renderer
  (define widget-lines (get-widget-lines-above ui-state))
  (when (> (length widget-lines) 0)
    (define widget-vnodes (vdom-comp:styled-lines->vnodes widget-lines))
    (for ([vn (in-list widget-vnodes)]
          [i (in-naturals)])
      (define widget-y (+ transcript-start-row transcript-height i))
      (when (< widget-y status-y)
        (render-vdom-to-buffer! vn ubuf cols #:start-row widget-y))))

  ;; 5. Draw status bar via vdom component
  (define status-comp (vdom-comp:make-status-bar-vdom-component))
  (define status-vnodes ((q-component-render-fn status-comp) ui-state cols))
  (render-vdom-section-to-buffer! status-vnodes ubuf cols status-y 1)

  ;; 6. Draw input line via vdom component
  (define input-comp (vdom-comp:make-input-vdom-component/istate input-st))
  (define input-vnodes ((q-component-render-fn input-comp) ui-state cols))
  (render-vdom-section-to-buffer! input-vnodes ubuf cols input-y 1)

  ;; 7. Draw overlay if active (via vdom section renderer)
  (define overlay (ui-state-active-overlay ui-state))
  (when overlay
    (define ov-content (overlay-state-content overlay))
    (define ov-lines
      (if (> (length ov-content) transcript-height)
          (take-right ov-content transcript-height)
          ov-content))
    ;; Convert overlay content to vnodes with background style
    (define ov-vnodes
      (for/list ([line (in-list ov-lines)]
                 [_ (in-naturals)])
        #:break (>= _ transcript-height)
        (vdom-comp:styled-line->vnode line)))
    ;; Clear overlay area with bg=8 then render content
    (for ([i (in-range transcript-height)])
      (cell-buffer-putstring! ubuf 0 (+ transcript-start-row i) (make-string cols #\space) #:bg 8))
    (when (pair? ov-vnodes)
      (render-vdom-section-to-buffer! ov-vnodes ubuf cols transcript-start-row (length ov-vnodes))))

  ;; 8. Return cursor position
  (define-values (_visible-text _scroll-offset cursor-display-col)
    (input-visible-window input-st cols))
  (values cursor-display-col input-y ui-state* '()))

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

  ;; Render to ubuf — always use vdom path
  (define-values (cursor-col cursor-row state* frame-lines)
    (render-frame-vdom! ubuf state inp layout))

  ;; Write back state with updated render cache
  (set-box! (tui-ctx-ui-state-box ctx) state*)

  ;; Cell-level incremental diff (always)
  (tui-cursor-hide)
  (define prev-ubuf (unbox (tui-ctx-prev-ubuf-box ctx)))
  (define out (tui-output-port))
  (render-smart! prev-ubuf ubuf out #:sync? #t)
  ;; Store snapshot for next diff
  (set-box! (tui-ctx-prev-ubuf-box ctx) (cell-buffer-snapshot ubuf))

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
    [(tmousemsg? msg) (decode-mouse-message msg)]
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
