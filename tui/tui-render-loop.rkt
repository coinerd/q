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

(require "../util/error/error-helpers.rkt")
(require racket/contract
         racket/bytes
         racket/string
         racket/list
         "../tui/terminal.rkt"
         "../tui/state.rkt"
         "../tui/input.rkt"
         "../tui/sgr.rkt"
         "../tui/layout.rkt"
         "../tui/cell-buffer.rkt"
         "../tui/cell-diff.rkt"
         "../tui/cell-diff-render.rkt"
         "../tui/vdom-render.rkt"
         (prefix-in vdom-comp: "../tui/vdom-components.rkt")
         (only-in "../tui/component.rkt"
                  q-component-render-fn
                  component-render
                  component-invalidate!
                  component-state-ref
                  component-state-update)
         (only-in "../tui/context.rkt" tui-ctx-component-registry-box)
         (only-in "../tui/render.rkt"
                  render-transcript
                  render-status-bar
                  render-input-line
                  styled-line->ansi
                  styled-line->text
                  plain-line
                  apply-selection-highlight)
         (only-in "../util/event/event.rkt" event)
         "../agent/event-bus.rkt"
         "../tui/tui-keybindings.rkt"
         "../tui/terminal-input.rkt"
         "../util/error/output-guard.rkt"
         "../agent/queue.rkt"
         (only-in "../runtime/session/session-lifecycle.rkt" write-crash-log!)
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
         current-resize-poll-interval-ms
         resize-poll-due?
         reset-idle-render-state!
         apply-cursor-blink-timer!
         cursor-blink-redraw-needed?
         render-cursor-blink-frame!
         (contract-out [check-busy-watchdog (-> any/c number? number? (or/c any/c #f))]
                       [apply-busy-watchdog! (-> tui-ctx? number? number? boolean?)]
                       [tui-ctx-init-terminal! (-> tui-ctx? void?)]
                       [tui-ctx-resize-ubuf! (-> tui-ctx? void?)]
                       [tui-ctx-ubuf (-> tui-ctx? any/c)]
                       [tui-ctx-term (-> tui-ctx? any/c)]
                       [render-frame! (-> tui-ctx? void?)]
                       [draw-frame (-> tui-ctx? void?)]
                       [render-frame-vdom!
                        (->* (any/c ui-state? input-state? hash?)
                             (#:component-registry (or/c (hash/c symbol? any/c) #f))
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

;; Cursor blink interval (milliseconds). Standard terminal blink is ~530ms.
(define BLINK-INTERVAL-MS 530)

;; Track last blink toggle and current phase (#t = cursor visible).
(define last-blink-toggle-ms (box 0.0))
(define blink-phase (box #t))

;; Idle resize fallback polling. Resize events remain immediate, but fallback
;; polling must be coarse enough to avoid repeated stty subprocess churn.
(define current-resize-poll-interval-ms (make-parameter 1000.0))
(define last-resize-poll-ms (box #f))

;; Cursor-only redraw state. A blink tick should not force component cache
;; invalidation or full VDOM rendering.
(define cursor-blink-redraw-needed-box (box #f))
(define last-cursor-col-box (box #f))
(define last-cursor-row-box (box #f))
(define last-cursor-base-cell-box (box #f))

(define (reset-idle-render-state!)
  (set-box! last-render-ms 0.0)
  (set-box! last-blink-toggle-ms 0.0)
  (set-box! blink-phase #t)
  (set-box! last-resize-poll-ms #f)
  (set-box! cursor-blink-redraw-needed-box #f)
  (set-box! last-cursor-col-box #f)
  (set-box! last-cursor-row-box #f)
  (set-box! last-cursor-base-cell-box #f))

(define (resize-poll-due? now-ms last-ms interval-ms)
  (or (not last-ms) (>= (- now-ms last-ms) interval-ms)))

(define (apply-resize-poll! ctx now-ms)
  (cond
    [(resize-poll-due? now-ms (unbox last-resize-poll-ms) (current-resize-poll-interval-ms))
     (set-box! last-resize-poll-ms now-ms)
     (when (tui-screen-size-changed?)
       (tui-ctx-resize-ubuf! ctx)
       (mark-dirty! ctx)
       #t)]
    [else #f]))

(define (apply-cursor-blink-timer! now-ms)
  (cond
    [(> (- now-ms (unbox last-blink-toggle-ms)) BLINK-INTERVAL-MS)
     (set-box! blink-phase (not (unbox blink-phase)))
     (set-box! last-blink-toggle-ms now-ms)
     (set-box! cursor-blink-redraw-needed-box #t)
     #t]
    [else #f]))

(define (cursor-blink-redraw-needed?)
  (unbox cursor-blink-redraw-needed-box))

;; Native cell-buffer operations (no dynamic-require)

;; ============================================================
;; Terminal/ubuf lifecycle helpers
;; ============================================================

;; Initialize terminal and ubuf for a context
(define (tui-ctx-init-terminal! ctx)
  (define term (tui-term-open))
  (set-box! (tui-ctx-term-box ctx) term)
  ;; Sync cursor visibility tracking: hardware cursor is hidden for the
  ;; entire TUI session; we draw a software cursor in the cell buffer.
  (tui-cursor-hide)
  (define-values (cols rows) (tui-screen-size))
  (define ubuf (make-cell-buffer cols rows))
  (set-box! (tui-ctx-ubuf-box ctx) ubuf)
  ;; Configure renderer to use cell-buffer operations
  ;; Enable mouse tracking for scroll wheel support
  (enable-mouse-tracking)
  ;; Detect synchronized output support
  (detect-sync-mode-support!)
  ;; Start idle timers from terminal initialization so an idle TUI does not
  ;; immediately blink/poll as if the last tick occurred at process start.
  (reset-idle-render-state!)
  (set-box! last-blink-toggle-ms (current-inexact-milliseconds))
  (set-box! last-resize-poll-ms (current-inexact-milliseconds))
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

;; ============================================================
;; Pure render helpers (extracted for testability, v0.74.5)
;; ============================================================

;; clip-visible-lines : (listof any/c) natural? -> (listof any/c)
;; Pure: clips lines to visible height from the bottom.
(define (clip-visible-lines lines height)
  (if (> (length lines) height)
      (take-right lines height)
      lines))

;; compute-pad-count : (listof any/c) natural? -> natural?
;; Pure: computes number of padding lines needed.
(define (compute-pad-count lines height)
  (max 0 (- height (length lines))))

;; clip-overlay-content : (listof any/c) natural? -> (listof any/c)
;; Pure: clips overlay content to fit transcript height.
(define (clip-overlay-content content height)
  (if (> (length content) height)
      (take-right content height)
      content))

(provide (contract-out [clip-visible-lines (-> list? exact-nonnegative-integer? list?)]
                       [compute-pad-count
                        (-> list? exact-nonnegative-integer? exact-nonnegative-integer?)]
                       [clip-overlay-content (-> list? exact-nonnegative-integer? list?)]))

;; Render the complete frame to the terminal using ubuf.
;; Hides cursor during redraw to prevent flicker, shows after.
;; Clears needs-redraw flag after drawing.
(define (render-frame-vdom! ubuf ui-state input-st layout #:component-registry [comp-registry #f])
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

  ;; Get component from registry (if available) or create ephemeral
  (define (get-comp id make-fn)
    (if comp-registry
        (hash-ref comp-registry id (lambda () (make-fn)))
        (make-fn)))

  ;; 1. Clear the buffer
  (cell-buffer-clear! ubuf)

  ;; 2. Draw header row via vdom component
  (when header-row
    (define header-comp (get-comp 'header-vdom vdom-comp:make-header-vdom-component))
    (define header-vnodes (component-render header-comp ui-state cols))
    (render-vdom-section-to-buffer! header-vnodes ubuf cols header-row 1))

  ;; 3. Render transcript → styled-lines → vnodes → cell-buffer
  ;; NOTE: Direct render-transcript call is required because it returns (values styled-lines ui-state*)
  ;; where ui-state* carries the updated render cache. The transcript vdom component wrapper
  ;; (make-transcript-vdom-component) discards this state, so it can only be used in test scenarios.
  ;; However, the registered transcript component's state-box is used for frame-level metadata.
  (define trans-comp (and comp-registry (hash-ref comp-registry 'transcript-vdom #f)))
  (define trans-frame-count
    (if trans-comp
        (add1 (component-state-ref trans-comp 'render-count 0))
        1))
  (when trans-comp
    (component-state-update trans-comp 'render-count trans-frame-count)
    (component-state-update trans-comp 'last-width cols)
    ;; Shadow scroll-offset from ui-state into component state
    ;; This proves component-state-ref/set! works for scroll-relevant data
    (component-state-update trans-comp 'last-scroll-offset (ui-state-scroll-offset ui-state)))
  (define-values (trans-lines-raw ui-state*) (render-transcript ui-state transcript-height cols))
  (define visible-lines-raw (clip-visible-lines trans-lines-raw transcript-height))
  (define pad-count (compute-pad-count trans-lines-raw transcript-height))
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
  (define status-comp (get-comp 'status-bar-vdom vdom-comp:make-status-bar-vdom-component))
  (define status-vnodes (component-render status-comp ui-state cols))
  (render-vdom-section-to-buffer! status-vnodes ubuf cols status-y 1)

  ;; 6. Draw input line via vdom component
  (define input-comp (vdom-comp:make-input-vdom-component/istate input-st))
  (define input-vnodes (component-render input-comp ui-state cols))
  (render-vdom-section-to-buffer! input-vnodes ubuf cols input-y 1)

  ;; 7. Draw overlay if active (via vdom section renderer)
  (define overlay (ui-state-active-overlay ui-state))
  (when overlay
    (define ov-content (overlay-state-content overlay))
    (define ov-lines (clip-overlay-content ov-content transcript-height))
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

  ;; Initialize component registry on first render
  (define reg-box (tui-ctx-component-registry-box ctx))
  (when (not (unbox reg-box))
    (set-box! reg-box
              (hash 'header-vdom
                    (vdom-comp:make-header-vdom-component)
                    'status-bar-vdom
                    (vdom-comp:make-status-bar-vdom-component)
                    'transcript-vdom
                    (vdom-comp:make-transcript-vdom-component))))
  (define comp-registry (unbox reg-box))

  ;; Invalidate all component caches before rendering — state may have changed
  ;; since last frame (busy, thinking, context, cost, goal, etc.)
  (when comp-registry
    (for ([comp (in-hash-values comp-registry)])
      (component-invalidate! comp)))

  ;; Render to ubuf — always use vdom path
  (define-values (cursor-col cursor-row state* frame-lines)
    (render-frame-vdom! ubuf state inp layout #:component-registry comp-registry))

  ;; Remember base cursor cell before applying software-cursor inversion.
  ;; Cursor blink can then update only this one cell without rebuilding VDOM.
  (define cursor-cell (cell-buffer-ref ubuf cursor-col cursor-row))
  (set-box! last-cursor-col-box cursor-col)
  (set-box! last-cursor-row-box cursor-row)
  (set-box! last-cursor-base-cell-box cursor-cell)

  ;; Draw software cursor (inverse video) when blink phase is on.
  ;; Hardware cursor is hidden for the entire session; this provides
  ;; a stable, self-controlled cursor that never resets the terminal's
  ;; hardware blink timer.
  (when (unbox blink-phase)
    (cell-buffer-set! ubuf
                      cursor-col
                      cursor-row
                      #:char (cell-char cursor-cell)
                      #:fg (cell-bg cursor-cell)
                      #:bg (cell-fg cursor-cell)
                      #:bold (cell-bold? cursor-cell)
                      #:underline (cell-underline? cursor-cell)
                      #:italic (cell-italic? cursor-cell)
                      #:blink (cell-blink? cursor-cell)))

  ;; Write back state with updated render cache
  (set-box! (tui-ctx-ui-state-box ctx) state*)

  ;; Cell-level incremental diff with synchronized output.
  ;; Hardware cursor remains hidden; we draw the cursor in the cell buffer.
  ;; Sync bracket prevents frame tearing on capable terminals.
  (define prev-ubuf (unbox (tui-ctx-prev-ubuf-box ctx)))
  (define out (tui-output-port))
  (terminal-sync-begin!)
  (render-smart! prev-ubuf ubuf out #:sync? #f)
  ;; Store snapshot for next diff
  (set-box! (tui-ctx-prev-ubuf-box ctx) (cell-buffer-snapshot ubuf))

  ;; Position hardware cursor at input location for IME tracking.
  ;; Hardware cursor is hidden (ESC[?25l at startup), so this is invisible
  ;; but tells the terminal/IME where composition should occur.
  (tui-cursor (+ cursor-col 1) (+ cursor-row 1) #f)
  ;; Emit IME cursor marker for CJK input support
  ;; Uses APC protocol — silently ignored by terminals that don't understand it
  (display CURSOR-MARKER out)
  (terminal-sync-end!)
  (tui-flush)
  (set-box! last-render-ms (current-inexact-milliseconds))
  (set-box! cursor-blink-redraw-needed-box #f)
  (set-box! (tui-ctx-needs-redraw-box ctx) #f))

(define (write-cell! ubuf col row c #:inverse? [inverse? #f])
  (cell-buffer-set! ubuf
                    col
                    row
                    #:char (cell-char c)
                    #:fg (if inverse?
                             (cell-bg c)
                             (cell-fg c))
                    #:bg (if inverse?
                             (cell-fg c)
                             (cell-bg c))
                    #:bold (cell-bold? c)
                    #:underline (cell-underline? c)
                    #:italic (cell-italic? c)
                    #:blink (cell-blink? c)))

(define (render-cursor-blink-frame! ctx)
  (define col (unbox last-cursor-col-box))
  (define row (unbox last-cursor-row-box))
  (define base-cell (unbox last-cursor-base-cell-box))
  (cond
    [(and col row base-cell (tui-ctx-ubuf ctx))
     (define ubuf (tui-ctx-ubuf ctx))
     (write-cell! ubuf col row base-cell #:inverse? (unbox blink-phase))
     (define prev-ubuf (unbox (tui-ctx-prev-ubuf-box ctx)))
     (define out (tui-output-port))
     (terminal-sync-begin!)
     (render-smart! prev-ubuf ubuf out #:sync? #f)
     (tui-cursor (+ col 1) (+ row 1) #f)
     (display CURSOR-MARKER out)
     (terminal-sync-end!)
     (tui-flush)
     (set-box! (tui-ctx-prev-ubuf-box ctx) (cell-buffer-snapshot ubuf))
     (set-box! cursor-blink-redraw-needed-box #f)
     (set-box! last-render-ms (current-inexact-milliseconds))
     #t]
    [else
     ;; No previous full frame exists yet; leave a full redraw request for the
     ;; normal path and consume the cursor-only request.
     (set-box! cursor-blink-redraw-needed-box #f)
     (mark-dirty! ctx)
     #f]))

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

;; Apply the pure watchdog state transition to the live TUI context.
;; When the watchdog fires, also signal goal cancellation so the UI watchdog
;; does not leave an autonomous goal thread running behind an idle-looking UI.
(define (apply-busy-watchdog! ctx now-ms watchdog-ms)
  (define cur-state (unbox (tui-ctx-ui-state-box ctx)))
  (define watchdog-result (check-busy-watchdog cur-state now-ms watchdog-ms))
  (cond
    [watchdog-result
     (set-box! (tui-ctx-ui-state-box ctx) watchdog-result)
     (set-box! (tui-ctx-goal-cancel-box ctx) #t)
     (mark-dirty! ctx)
     #t]
    [else #f]))

(define (tui-main-loop ctx)
  (let loop ()
    ;; Drain all pending events from the channel (non-blocking)
    (drain-events! ctx)

    ;; Busy-state watchdog — force-clear stale busy state (v0.45.12 L3+L4)
    ;; and cancel any active goal thread so work does not continue behind an idle UI.
    (apply-busy-watchdog! ctx (current-inexact-milliseconds) (current-busy-watchdog-ms))

    ;; Poll for terminal resize only on a coarse fallback interval. Resize
    ;; messages remain immediate; this path exists for terminals/test stubs
    ;; that do not deliver tsizemsg/SIGWINCH events.
    (define now-ms (current-inexact-milliseconds))
    (apply-resize-poll! ctx now-ms)

    ;; Cursor blink phase toggle. A blink only needs a cursor-cell redraw, not
    ;; full component invalidation and VDOM rendering.
    (apply-cursor-blink-timer! now-ms)

    ;; Draw full frames only when state changed. Cursor-only blink frames use
    ;; the stored cursor cell and avoid rebuilding the full VDOM tree.
    (cond
      [(unbox (tui-ctx-needs-redraw-box ctx))
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
         [else (render-frame! ctx)])]
      [(cursor-blink-redraw-needed?) (render-cursor-blink-frame! ctx)])

    (when (unbox (tui-ctx-running-box ctx))
      ;; Get next message from terminal (adapter pattern)
      (define msg (next-message ctx #:timeout 0.05))

      (when msg
        (define result (process-tui-message! ctx msg))
        (when (eq? result 'resize)
          ;; Reset before querying dimensions so resize events are immediate
          ;; even though idle fallback size polling is coarsely cached.
          (tui-screen-size-cache-reset!)
          (tui-ctx-resize-ubuf! ctx)
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
