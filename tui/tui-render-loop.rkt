#lang racket/base
;;
;; COMPOSITION ROOT: This module wires together dependencies from
;; lower layers. It should not be imported by other production modules.
;;

;; q/tui/tui-render-loop.rkt — Main render loop, cell-buffer lifecycle, event draining
;;
;; Extracted from interfaces/tui.rkt for modularity (Issue #194).
;;

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
         "message-dispatch.rkt"
         (only-in "render-loop/frame-vdom.rkt"
                  render-frame-vdom!
                  clip-visible-lines
                  compute-pad-count
                  clip-overlay-content)
         (only-in "render-loop/watchdog.rkt" current-busy-watchdog-ms check-busy-watchdog))

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
;; ============================================================

(define MIN-RENDER-INTERVAL-MS 16)

(define last-render-ms (box 0.0))

(define BLINK-INTERVAL-MS 530)

(define last-blink-toggle-ms (box 0.0))
(define blink-phase (box #t))

(define current-resize-poll-interval-ms (make-parameter 1000.0))
(define last-resize-poll-ms (box #f))

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

(define (tui-ctx-init-terminal! ctx)
  (define term (tui-term-open))
  (set-box! (tui-ctx-term-box ctx) term)
  ;; Sync cursor visibility tracking: hardware cursor is hidden for the
  ;; entire TUI session; we draw a software cursor in the cell buffer.
  (tui-cursor-hide)
  (define-values (cols rows) (tui-screen-size))
  (define ubuf (make-cell-buffer cols rows))
  (set-box! (tui-ctx-ubuf-box ctx) ubuf)
  (enable-mouse-tracking)
  (detect-sync-mode-support!)
  ;; Start idle timers from terminal initialization so an idle TUI does not
  ;; immediately blink/poll as if the last tick occurred at process start.
  (reset-idle-render-state!)
  (set-box! last-blink-toggle-ms (current-inexact-milliseconds))
  (set-box! last-resize-poll-ms (current-inexact-milliseconds))
  (void))

(define (tui-ctx-resize-ubuf! ctx)
  (define-values (cols rows) (tui-screen-size))
  (define ubuf (make-cell-buffer cols rows))
  (set-box! (tui-ctx-ubuf-box ctx) ubuf)
  (set-box! (tui-ctx-previous-frame-box ctx) #f))

(define (tui-ctx-ubuf ctx)
  (unbox (tui-ctx-ubuf-box ctx)))

(define (tui-ctx-term ctx)
  (unbox (tui-ctx-term-box ctx)))

(provide (contract-out [clip-visible-lines (-> list? exact-nonnegative-integer? list?)]
                       [compute-pad-count
                        (-> list? exact-nonnegative-integer? exact-nonnegative-integer?)]
                       [clip-overlay-content (-> list? exact-nonnegative-integer? list?)]))

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

(define (draw-frame ctx)
  (render-frame! ctx))

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
