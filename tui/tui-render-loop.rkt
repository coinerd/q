#lang racket/base

;; q/tui/tui-render-loop.rkt — Main render loop, ubuf lifecycle, event draining
;;
;; Extracted from interfaces/tui.rkt for modularity (Issue #194).
;;
;; Dependency chain: tui-init.rkt → tui-render-loop.rkt → tui-keybindings.rkt
;;
;; Provides:
;;   render-ubuf-to-terminal! — ubuf → terminal output with SGR fix
;;   tui-ctx-init-terminal!    — terminal + ubuf initialization
;;   tui-ctx-resize-ubuf!      — resize ubuf on terminal resize
;;   tui-ctx-ubuf, tui-ctx-term — ubuf/term accessors
;;   render-frame!, draw-frame  — frame rendering
;;   next-message               — terminal message adapter
;;   tui-main-loop              — main event loop
;;   drain-events!              — runtime event draining
;;   Re-exports: fix-sgr-bg-black, decode-mouse-x10

(require racket/bytes
         racket/string
         "../tui/terminal.rkt"
         "../tui/state.rkt"
         "../tui/input.rkt"
         "../tui/sgr.rkt"
         (prefix-in renderer: "../tui/renderer.rkt")
         "../tui/layout.rkt"
         "../tui/frame-diff.rkt"
         "../util/protocol-types.rkt"
         "../agent/event-bus.rkt"
         "../tui/tui-keybindings.rkt"
         "../tui/terminal-input.rkt")

;; ── Ubuf/terminal lifecycle ──
(provide render-ubuf-to-terminal!
         tui-ctx-init-terminal!
         tui-ctx-resize-ubuf!
         tui-ctx-ubuf
         tui-ctx-term
         ;; ── Frame rendering ──
         render-frame!
         draw-frame
         ;; ── Message adapter ──
         next-message
         ;; ── Main loop ──
         tui-main-loop
         drain-events!
         ;; ── Re-exports ──
         fix-sgr-bg-black
         decode-mouse-x10
         decode-mouse-tui-term)

;; ============================================================
;; Ubuf FFI/stubs
;; ============================================================

;; Import tui-ubuf for output buffering (dynamically with fallback)
(define tui-ubuf-available?
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (collection-path "tui")
    #t))

;; Minimum render interval in milliseconds.
;; Coalesces rapid state changes (e.g., streaming) into single frames.
;; 16ms ≈ 60fps, prevents flicker during fast streaming output.
(define MIN-RENDER-INTERVAL-MS 16)

;; Track last render timestamp for debouncing.
(define last-render-ms (box 0.0))

(define make-ubuf-fn
  (and tui-ubuf-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/ubuf 'make-ubuf))))

(define ubuf-clear!-fn
  (and tui-ubuf-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/ubuf 'ubuf-clear!))))

(define ubuf-putstring!-fn
  (and tui-ubuf-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/ubuf 'ubuf-putstring!))))

(define display-ubuf!-fn
  (and tui-ubuf-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/ubuf/output 'display-ubuf!))))

;; Fallback stubs for when tui-ubuf is not available
(define (stub-make-ubuf cols rows)
  (make-hash `((cols . ,cols) (rows . ,rows))))
(define (stub-ubuf-clear! ubuf)
  (void))
(define (stub-ubuf-putstring! ubuf col row str . attrs)
  (void))
(define (stub-display-ubuf! ubuf port . opts)
  (void))

(define make-ubuf (or make-ubuf-fn stub-make-ubuf))
(define ubuf-clear! (or ubuf-clear!-fn stub-ubuf-clear!))
(define ubuf-putstring! (or ubuf-putstring!-fn stub-ubuf-putstring!))
(define display-ubuf! (or display-ubuf!-fn stub-display-ubuf!))

;; ============================================================
;; Render ubuf to terminal
;; ============================================================

;; Render ubuf to terminal with bg=0 replaced by terminal default bg.
;; Captures display-ubuf! output, post-processes SGR sequences,
;; then writes the fixed output to the real terminal.
;; Uses DEC mode 2026 (Synchronized Output) when available to
;; prevent torn frames during rapid streaming output.
(define (render-ubuf-to-terminal! ubuf)
  (define out (open-output-bytes))
  (display-ubuf! ubuf out #:only-dirty #f #:linear #f)
  (define bs (get-output-bytes out))
  (define str (bytes->string/utf-8 bs))
  (terminal-sync-begin!)
  (display (fix-sgr-bg-black str) (current-output-port))
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
  term)

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
  (define layout (compute-layout-with-widgets cols rows (length widget-lines)))

  ;; Render to ubuf (returns cursor position, state, frame lines)
  (define-values (cursor-col cursor-row state* frame-lines)
    (renderer:render-frame! ubuf state inp layout))

  ;; Write back state with updated render cache
  (set-box! (tui-ctx-ui-state-box ctx) state*)

  ;; Diff-based terminal output
  (define prev-frame (unbox (tui-ctx-previous-frame-box ctx)))
  (define diffs (diff-frames prev-frame frame-lines))
  (tui-cursor-hide)
  (cond
    ;; No changes — skip terminal output entirely
    [(null? diffs) (void)]
    [(and (= (length diffs) 1) (eq? (diff-cmd-type (car diffs)) 'full))
     ;; Full redraw needed (first frame or resize)
     (render-ubuf-to-terminal! ubuf)]
    [else
     ;; Incremental: write only changed lines
     (for ([cmd (in-list diffs)])
       (case (diff-cmd-type cmd)
         [(write)
          (tui-cursor 1 (+ (diff-cmd-row cmd) 1)) ; ANSI is 1-based
          (display "\x1b[2K" (current-output-port)) ; clear line
          (display (fix-sgr-bg-black (diff-cmd-content cmd)) (current-output-port))]
         [(clear-from)
          ;; Clear from given row to end of screen
          (tui-cursor 1 (+ (diff-cmd-row cmd) 1))
          (display "\x1b[J" (current-output-port))]
         [else (void)]))
     (tui-flush)])

  ;; Store current frame for next diff
  (set-box! (tui-ctx-previous-frame-box ctx) frame-lines)

  ;; Position cursor at input location (renderer returns 0-indexed, ANSI is 1-indexed)
  (tui-cursor (+ cursor-col 1) (+ cursor-row 1))
  ;; Emit IME cursor marker for CJK input support
  ;; Uses APC protocol — silently ignored by terminals that don't understand it
  (display CURSOR-MARKER (current-output-port))
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
    ;; Mouse message — dual-path dispatch (#1120)
    ;; tui-term returns actual structs, fallback path uses X10 vectors
    [(tmousemsg? msg)
     (if (tmousemsg-tui-term? msg)
         (decode-mouse-tui-term msg)
         (decode-mouse-x10 (tmousemsg-cb msg) (tmousemsg-cx msg) (tmousemsg-cy msg)))]
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

(define (tui-main-loop ctx)
  (let loop ()
    ;; Drain all pending events from the channel (non-blocking)
    (drain-events! ctx)

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
        (case (car msg)
          ;; Key press: dispatch to handler
          [(key)
           (define keycode (cadr msg))
           (define result (handle-key ctx keycode))
           (cond
             [(eq? result 'quit) (set-box! (tui-ctx-running-box ctx) #f)]
             [(and (list? result) (eq? (car result) 'submit))
              (define text (cadr result))
              ;; Submit to runtime (non-blocking)
              (define runner (tui-ctx-session-runner ctx))
              (thread (lambda () (runner text)))]
             [(and (list? result) (eq? (car result) 'command))
              (define cmd (cadr result))
              (process-slash-command ctx cmd)]
             [else (void)])]

          ;; Resize event: resize ubuf and mark dirty
          [(resize)
           (define cols (cadr msg))
           (define rows (caddr msg))
           (tui-ctx-resize-ubuf! ctx)
           (tui-screen-size-cache-reset!)
           (mark-dirty! ctx)]

          ;; Redraw command: mark dirty
          [(redraw) (mark-dirty! ctx)]

          ;; Paste event: insert as single undo entry
          [(paste)
           (define text (cadr msg))
           (define inp (unbox (tui-ctx-input-state-box ctx)))
           (set-box! (tui-ctx-input-state-box ctx) (input-insert-string inp text))
           (mark-dirty! ctx)]

          ;; Mouse event: dispatch to handler
          [(mouse) (handle-mouse ctx (cdr msg))]

          ;; Unknown message type - ignore
          [else (void)]))

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
