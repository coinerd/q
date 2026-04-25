#lang racket/base

;; q/tui/tui-keybindings.rkt — Key/mouse handling, selection, slash commands
;;
;; Extracted from interfaces/tui.rkt for modularity (Issue #194).
;;
;; This is the LEAF module in the TUI decomposition dependency chain:
;;   tui-init.rkt → tui-render-loop.rkt → tui-keybindings.rkt
;;
;; Provides:
;;   tui-ctx struct + make-tui-ctx + all accessors
;;   mark-dirty!
;;   handle-key, handle-mouse
;;   selection-text
;;   process-slash-command, tui-ctx->cmd-ctx
;;   Re-exports: copy-text!, copy-selection!, current-clipboard-mode,
;;              clipboard-backend-available?, styled-line->text

(require racket/string
         racket/list
         racket/async-channel
         "../tui/terminal.rkt"
         "../tui/state.rkt"
         "../tui/input.rkt"
         "../tui/render.rkt"
         "../tui/layout.rkt"
         "../tui/clipboard.rkt"
         "../tui/char-width.rkt"
         "../util/protocol-types.rkt"
         "../agent/event-bus.rkt"
         (prefix-in commands: "../tui/commands.rkt")
         "../tui/keymap.rkt")

;; ── tui-ctx struct ──
(provide (struct-out tui-ctx)
         make-tui-ctx
         ;; ── Key/mouse handling ──
         mark-dirty!
         handle-key
         handle-mouse
         ;; ── Selection ──
         selection-text
         ;; ── Slash commands ──
         process-slash-command
         tui-ctx->cmd-ctx
         ;; ── Re-exports from tui/terminal.rkt ──
         copy-text!
         copy-selection!
         current-clipboard-mode
         clipboard-backend-available?
         clipboard-paste
         ;; ── Re-exports from tui/render.rkt ──
         styled-line->text
         ;; ── Re-exports from tui/input.rkt ──
         input-undo
         input-redo
         input-kill-word-backward
         input-kill-to-beginning
         input-kill-to-end
         input-yank
         input-cursor-word-left
         input-cursor-word-right
         input-insert-string
         ;; ── Keybindings config (#1117) ──
         reload-keymap!
         current-keybindings-path)

;; ============================================================
;; TUI context
;; ============================================================

;; Holds mutable references for the running TUI
(struct tui-ctx
        (ui-state-box ; (boxof ui-state)
         input-state-box ; (boxof input-state)
         event-bus ; event-bus? or #f
         session-runner ; (string -> void) — called with user prompts
         running-box ; (boxof boolean) — set to #f to exit
         event-ch ; async-channel — serializes runtime events into main loop (unbounded buffer)
         session-dir ; (or/c path-string? #f) — session directory for index loading
         needs-redraw-box ; (boxof boolean) — #t when state changed and frame needs redraw
         term-box ; (boxof any) — terminal instance for tui-term
         ubuf-box ; (boxof any) — ubuf buffer for output
         model-registry-box ; (boxof (or/c model-registry? #f)) — model registry for /model
         previous-frame-box ; (boxof (or/c (listof string) #f)) — last rendered frame for diffing
         last-prompt-box ; (boxof (or/c string? #f)) — last user prompt for /retry
         extension-registry-box ; (or/c (boxof (or/c extension-registry? #f)) #f)
         session-queue-box) ; (boxof (or/c queue? #f)) — agent queue for followup during streaming (G3.1)
  #:transparent)

(define (make-tui-ctx #:event-bus [bus #f]
                      #:session-runner [runner (lambda (prompt) (void))]
                      #:session-dir [sess-dir #f]
                      #:model-registry [reg #f]
                      #:extension-registry [ext-reg #f]
                      #:session-queue [sess-queue #f])
  (tui-ctx (box (initial-ui-state))
           (box (initial-input-state))
           bus
           runner
           (box #t)
           (make-async-channel)
           sess-dir
           (box #t) ; needs-redraw: #t for first frame
           (box #f) ; term-box - set when terminal opened
           (box #f) ; ubuf-box - set when buffer created
           (box reg) ; model-registry-box
           (box #f) ; previous-frame-box - #f means no previous frame
           (box #f) ; last-prompt-box - #f until first submit
           (box ext-reg) ; extension-registry-box
           (box sess-queue))) ; session-queue-box

;; ============================================================
;; mark-dirty!
;; ============================================================

;; Mark that the frame needs redraw.
(define (mark-dirty! ctx)
  (set-box! (tui-ctx-needs-redraw-box ctx) #t))

;; ============================================================
;; Selection math
;; ============================================================

;; Extract plain text from the current selection.
;; Uses rendered lines to map screen coordinates to text.
(define (selection-text ctx state)
  ;; Bounds validation (#1121): guard against out-of-range indices
  (with-handlers ([exn:fail? (lambda (e) "")])
    (define anchor (ui-state-sel-anchor state))
    (define end (ui-state-sel-end state))
    (and
     anchor
     end
     (let ()
       ;; Normalize so start <= end
       (define-values (start-col start-row end-col end-row) (normalize-selection-range anchor end))
       ;; Get rendered lines for the transcript area
       (define-values (cols rows) (tui-screen-size))
       (define layout (compute-layout cols rows))
       (define trans-y (tui-layout-transcript-start-row layout))
       (define trans-height (tui-layout-transcript-height layout))
       (define-values (all-lines _state*) (render-transcript state trans-height cols))
       ;; BUG-57: Compute pad-count for correct coordinate mapping
       (define visible-lines
         (if (> (length all-lines) trans-height)
             (take-right all-lines trans-height)
             all-lines))
       (define pad-count (- trans-height (length visible-lines)))
       ;; Map screen rows to line indices (account for padding)
       ;; Mouse y is 0-based: row 0 = header, row 1 = first transcript line.
       ;; Content starts at (trans-y + pad-count). So:
       ;;   line-index = screen-row - trans-y - pad-count
       (define start-idx (max 0 (- start-row trans-y pad-count)))
       (define end-idx (min (sub1 (length visible-lines)) (- end-row trans-y pad-count)))
       (if (> start-idx end-idx)
           ""
           (string-join
            (for/list ([i (in-range start-idx (add1 end-idx))])
              (define line (list-ref visible-lines i))
              (define text (styled-line->text line))
              (cond
                [(= i start-idx end-idx)
                 ;; Single line: extract column range (display-col→string-offset)
                 (substring text
                            (min (display-col->string-offset text start-col) (string-length text))
                            (min (display-col->string-offset text (add1 end-col))
                                 (string-length text)))]
                ;; First line: from start-col to end
                [(= i start-idx)
                 (substring text
                            (min (display-col->string-offset text start-col) (string-length text)))]
                ;; Last line: from 0 to end-col
                [(= i end-idx)
                 (substring text
                            0
                            (min (display-col->string-offset text (add1 end-col))
                                 (string-length text)))]
                [else text]))
            "\n"))))))

;; ============================================================
;; Slash command processing
;; ============================================================

;; Convert tui-ctx to commands:cmd-ctx for the commands module.
;; This avoids a circular dependency (commands.rkt cannot import
;; interfaces/tui.rkt where tui-ctx is defined).
(define (tui-ctx->cmd-ctx ctx)
  (commands:cmd-ctx (tui-ctx-ui-state-box ctx)
                    (tui-ctx-running-box ctx)
                    (tui-ctx-event-bus ctx)
                    (tui-ctx-session-dir ctx)
                    (tui-ctx-needs-redraw-box ctx)
                    (tui-ctx-model-registry-box ctx)
                    (tui-ctx-last-prompt-box ctx)
                    (tui-ctx-session-runner ctx)
                    (box "")
                    (tui-ctx-extension-registry-box ctx)))

;; Process a slash command. Returns 'continue | 'quit
;; cmd can be: symbol | (list symbol args...)
;; Public API — delegates to commands:process-slash-command.
(define (process-slash-command ctx cmd [raw-text ""])
  (define cctx (tui-ctx->cmd-ctx ctx))
  (set-box! (commands:cmd-ctx-input-text-box cctx) raw-text)
  (commands:process-slash-command cctx cmd))

;; ============================================================
;; Key handling
;; ============================================================

;; Handle a single key event.
;; Returns: 'continue | 'quit | (list 'submit string) | (list 'command symbol)
;; Cached merged keymap (default + user overrides)
(define cached-keymap #f)

;; Parameter for custom keybindings file path (#1117)
(define current-keybindings-path (make-parameter #f))

(define (get-active-keymap)
  ;; Return the merged keymap (default + user overrides).
  ;; Loads and caches on first call. Uses current-keybindings-path
  ;; if set, otherwise defaults to ~/.q/keybindings.json.
  (cond
    [cached-keymap cached-keymap]
    [else
     (define base
       (if (current-keybindings-path)
           (load-keybindings (current-keybindings-path))
           (default-keymap)))
     ;; If using default-keymap, also merge user keybindings from default location
     (when (not (current-keybindings-path))
       (define user (load-user-keymap))
       (when user
         (keymap-merge base user)))
     (set! cached-keymap base)
     base]))

;; Convert a raw keycode (char/symbol) from the terminal to a key-spec
;; for keymap lookup. Handles modifier-prefixed symbols like 'ctrl-z.
(define (keycode->key-spec-from-msg keycode)
  (cond
    [(char? keycode) (key-spec keycode #f #f #f)]
    [(symbol? keycode)
     (define s (symbol->string keycode))
     (cond
       ;; ctrl-X pattern
       [(and (> (string-length s) 5) (string-prefix? s "ctrl-"))
        (define rest (substring s 5))
        (key-spec (string->symbol rest) #t #f #f)]
       ;; shift-X pattern
       [(and (> (string-length s) 6) (string-prefix? s "shift-"))
        (define rest (substring s 6))
        (key-spec (string->symbol rest) #f #t #f)]
       [else (key-spec keycode #f #f #f)])]
    [else #f]))

;; Dispatch a keymap action to the appropriate handler.
;; Returns 'handled if handled (maps to 'continue in handle-key),
;; or #f if not (falls through to hardcoded).
(define (dispatch-keymap-action ctx inp state action)
  (case action
    [(tui.input.submit submit) #f] ;; Complex — fall through to hardcoded for proper submit flow
    [(tui.input.backspace backspace)
     (set-box! (tui-ctx-input-state-box ctx) (input-backspace inp))
     'handled]
    [(tui.input.delete delete)
     (set-box! (tui-ctx-input-state-box ctx) (input-delete inp))
     'handled]
    [(tui.navigation.home home)
     (set-box! (tui-ctx-input-state-box ctx) (input-home inp))
     'handled]
    [(tui.navigation.end end)
     (set-box! (tui-ctx-input-state-box ctx) (input-end inp))
     'handled]
    [(tui.navigation.history-up history-up)
     (set-box! (tui-ctx-input-state-box ctx) (input-history-up inp))
     'handled]
    [(tui.navigation.history-down history-down)
     (set-box! (tui-ctx-input-state-box ctx) (input-history-down inp))
     'handled]
    [(tui.editor.word-left word-left)
     (set-box! (tui-ctx-input-state-box ctx) (input-cursor-word-left inp))
     'handled]
    [(tui.editor.word-right word-right)
     (set-box! (tui-ctx-input-state-box ctx) (input-cursor-word-right inp))
     'handled]
    [(tui.editor.clear-input clear-input)
     (set-box! (tui-ctx-input-state-box ctx) (input-kill-to-beginning inp))
     'handled]
    [(tui.display.clear-screen clear-screen)
     (mark-dirty! ctx)
     'handled]
    [(tui.editor.copy copy) #f] ;; Complex — let hardcoded handle
    [(tui.editor.cut cut) #f]
    [(tui.editor.paste paste)
     (define text (clipboard-paste))
     (when text
       (set-box! (tui-ctx-input-state-box ctx) (input-insert-string inp text)))
     'handled]
    [(tui.editor.select-all select-all) #f] ;; Complex — let hardcoded handle
    [(tui.navigation.scroll-up scroll-up)
     (set-box! (tui-ctx-ui-state-box ctx) (scroll-up state 1))
     'handled]
    [(tui.navigation.scroll-down scroll-down)
     (set-box! (tui-ctx-ui-state-box ctx) (scroll-down state 1))
     'handled]
    [(tui.navigation.page-up page-up)
     (define-values (_cols rows) (tui-screen-size))
     (define layout (compute-layout _cols rows))
     (set-box! (tui-ctx-ui-state-box ctx)
               (scroll-up state (max 1 (tui-layout-transcript-height layout))))
     'handled]
    [(tui.navigation.page-down page-down)
     (define-values (_cols rows) (tui-screen-size))
     (define layout (compute-layout _cols rows))
     (set-box! (tui-ctx-ui-state-box ctx)
               (scroll-down state (max 1 (tui-layout-transcript-height layout))))
     'handled]
    [(scroll-top)
     (set-box! (tui-ctx-ui-state-box ctx) (scroll-to-top state))
     'handled]
    [(scroll-bottom)
     (set-box! (tui-ctx-ui-state-box ctx) (scroll-to-bottom state))
     'handled]
    [else #f]))

(define (reload-keymap!)
  ;; Force reload of keymap (e.g., after user edits keybindings.json)
  (set! cached-keymap #f)
  (void))

(define (handle-key ctx keycode)
  (define inp (unbox (tui-ctx-input-state-box ctx)))
  (define state (unbox (tui-ctx-ui-state-box ctx)))
  ;; Any key that reaches here may change state — mark for redraw
  (mark-dirty! ctx)

  ;; Check configurable keymap first
  (define km (get-active-keymap))
  (define ks (keycode->key-spec-from-msg keycode))
  (define action (and ks (keymap-lookup km ks)))
  (cond
    [(and action (eq? (dispatch-keymap-action ctx inp state action) 'handled)) 'continue]
    ;; Fallback to hardcoded behavior
    [(char? keycode)
     (case keycode
       [(#\return)
        ;; Enter → submit input
        (define-values (text new-inp) (input-submit inp))
        (set-box! (tui-ctx-input-state-box ctx) new-inp)
        (cond
          [(not text) 'continue]
          [(input-slash-command text)
           (define cmd (parse-tui-slash-command text))
           (list 'command (or cmd 'unknown) text)]
          [else
           ;; Add user message to transcript
           (define user-entry (make-entry 'user text (current-inexact-milliseconds) (hash)))
           (set-box! (tui-ctx-ui-state-box ctx) (add-transcript-entry state user-entry))
           (list 'submit text)])]
       [(#\newline)
        ;; Ctrl+J → insert newline (multi-line input)
        (set-box! (tui-ctx-input-state-box ctx) (input-insert-newline inp))
        'continue]
       [(#\backspace #\rubout)
        (set-box! (tui-ctx-input-state-box ctx) (input-backspace inp))
        'continue]
       [(#\u001b) 'continue]
       [else
        ;; Regular printable character
        (set-box! (tui-ctx-input-state-box ctx) (input-insert-char inp keycode))
        'continue])]

    ;; Symbol keys (arrows, function keys, etc.)
    [(symbol? keycode)
     (case keycode
       ;; Enter key — charterm sends 'return symbol, not #\return char
       [(return kp-return enter kp-enter)
        ;; Submit input (same logic as #\return/#\newline in char branch)
        (define-values (text new-inp) (input-submit inp))
        (set-box! (tui-ctx-input-state-box ctx) new-inp)
        (cond
          [(not text) 'continue]
          [(input-slash-command text)
           (define cmd (parse-tui-slash-command text))
           (list 'command (or cmd 'unknown) text)]
          [else
           ;; Add user message to transcript
           (define user-entry (make-entry 'user text (current-inexact-milliseconds) (hash)))
           (set-box! (tui-ctx-ui-state-box ctx) (add-transcript-entry state user-entry))
           (list 'submit text)])]
       [(left kp-left)
        (set-box! (tui-ctx-input-state-box ctx) (input-cursor-left inp))
        'continue]
       [(right kp-right)
        (set-box! (tui-ctx-input-state-box ctx) (input-cursor-right inp))
        'continue]
       [(up kp-up)
        (set-box! (tui-ctx-input-state-box ctx) (input-history-up inp))
        'continue]
       [(down kp-down)
        (set-box! (tui-ctx-input-state-box ctx) (input-history-down inp))
        'continue]
       [(backspace)
        (set-box! (tui-ctx-input-state-box ctx) (input-backspace inp))
        'continue]
       [(delete kp-delete)
        (set-box! (tui-ctx-input-state-box ctx) (input-delete inp))
        'continue]
       [(home kp-home)
        (set-box! (tui-ctx-input-state-box ctx) (input-home inp))
        'continue]
       [(end kp-end)
        (set-box! (tui-ctx-input-state-box ctx) (input-end inp))
        'continue]
       [(escape) 'continue]
       ;; Undo/Redo
       [(ctrl-z)
        (set-box! (tui-ctx-input-state-box ctx) (input-undo inp))
        'continue]
       [(ctrl-y)
        (set-box! (tui-ctx-input-state-box ctx) (input-redo inp))
        'continue]
       ;; Kill ring
       [(ctrl-w)
        (set-box! (tui-ctx-input-state-box ctx) (input-kill-word-backward inp))
        'continue]
       [(ctrl-u)
        (set-box! (tui-ctx-input-state-box ctx) (input-kill-to-beginning inp))
        'continue]
       [(ctrl-k)
        (set-box! (tui-ctx-input-state-box ctx) (input-kill-to-end inp))
        'continue]
       ;; Paste from system clipboard
       [(ctrl-v)
        (define text (clipboard-paste))
        (when text
          (set-box! (tui-ctx-input-state-box ctx) (input-insert-string inp text)))
        'continue]
       ;; Word navigation
       [(ctrl-left)
        (set-box! (tui-ctx-input-state-box ctx) (input-cursor-word-left inp))
        'continue]
       [(ctrl-right)
        (set-box! (tui-ctx-input-state-box ctx) (input-cursor-word-right inp))
        'continue]
       [(page-up pgup kp-pgup)
        ;; Page up: scroll by one viewport height (page)
        (define-values (_cols rows) (tui-screen-size))
        (define layout (compute-layout _cols rows))
        (set-box! (tui-ctx-ui-state-box ctx)
                  (scroll-up state (max 1 (tui-layout-transcript-height layout))))
        'continue]
       [(page-down pgdn kp-pgdn)
        ;; Page down: scroll by one viewport height (page)
        (define-values (_cols2 rows2) (tui-screen-size))
        (define layout2 (compute-layout _cols2 rows2))
        (set-box! (tui-ctx-ui-state-box ctx)
                  (scroll-down state (max 1 (tui-layout-transcript-height layout2))))
        'continue]
       [(ctrl-c)
        (if (has-selection? state)
            ;; Selection active → copy to clipboard
            (let ([text (selection-text ctx state)])
              (when (and text (not (string=? text "")))
                (copy-text! text)))
            ;; No selection → interrupt agent
            (begin
              (when (tui-ctx-event-bus ctx)
                (publish! (tui-ctx-event-bus ctx)
                          (make-event "interrupt.requested"
                                      (inexact->exact (truncate (/ (current-inexact-milliseconds)
                                                                   1000)))
                                      (or (ui-state-session-id state) "")
                                      #f
                                      (hash))))
              ;; Clear any streaming text and busy state
              (set-box!
               (tui-ctx-ui-state-box ctx)
               (struct-copy ui-state state [busy? #f] [streaming-text #f] [pending-tool-name #f]))))
        'continue]
       [else 'continue])]

    [else 'continue]))

;; ============================================================
;; Mouse handling
;; ============================================================

;; Handle a mouse event.
;; msg-data is (list type x y) or (list type button x y)
;; Returns 'continue.
(define (handle-mouse ctx msg-data)
  ;; Error guard (#1121): catch any exception from mouse handling
  ;; to prevent crashes from corrupt or unexpected mouse data.
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning "TUI: mouse handler error: ~a" (exn-message e))
                               'continue)])
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (define mouse-type (car msg-data))
    (mark-dirty! ctx)
    (case mouse-type
      [(scroll-up)
       (set-box! (tui-ctx-ui-state-box ctx) (scroll-up state 3))
       'continue]
      [(scroll-down)
       (set-box! (tui-ctx-ui-state-box ctx) (scroll-down state 3))
       'continue]
      [(click)
       ;; Start selection: set anchor at click position
       (define button (cadr msg-data))
       (define x (caddr msg-data))
       (define y (cadddr msg-data))
       (when (= button 0) ;; left click only
         (set-box! (tui-ctx-ui-state-box ctx) (set-selection-anchor state x y)))
       'continue]
      [(drag)
       ;; Update selection end during drag
       (define x (cadr msg-data))
       (define y (caddr msg-data))
       (when (has-selection? state)
         (set-box! (tui-ctx-ui-state-box ctx) (set-selection-end state x y)))
       'continue]
      [(release)
       ;; Copy selection to clipboard (platform tool + OSC 52 fallback).
       ;; Do NOT call set-selection-end here — the drag handler already
       ;; tracks sel-end correctly, and right-click releases would
       ;; corrupt the selection if we moved the endpoint.
       (when (has-selection? state)
         (define text (selection-text ctx state))
         (when (and text (not (string=? text "")))
           (copy-text! text)))
       'continue]
      [else 'continue])))

;; ============================================================
;; Tree browser overlay key handling (G1.1)
;; ============================================================

(require "tree-view.rkt")
(require racket/set)

(define (handle-tree-overlay-key ctx keycode)
  ;; Handle key events when tree-browser overlay is active.
  ;; Returns 'handled (consumed), 'dismiss (close overlay), or 'pass (not ours).
  (define state (unbox (tui-ctx-ui-state-box ctx)))
  (define ov (ui-state-active-overlay state))
  (define tbs (and ov (overlay-state-extra ov)))
  (cond
    [(not (and ov (eq? (overlay-state-type ov) 'tree-browser) (tree-browser-state? tbs))) 'pass]
    ;; Escape — dismiss overlay
    [(memq keycode '(escape #\e))
     (set-box! (tui-ctx-ui-state-box ctx) (dismiss-overlay state))
     (mark-dirty! ctx)
     'handled]
    ;; Down arrow — move selection down
    [(memq keycode '(down kp-down))
     (define new-idx
       (tree-next-node (tree-browser-state-rendered-lines tbs) (tree-browser-state-selected-idx tbs)))
     (update-tree-overlay! ctx state ov tbs new-idx (tree-browser-state-folded-set tbs))
     'handled]
    ;; Up arrow — move selection up
    [(memq keycode '(up kp-up))
     (define new-idx
       (tree-prev-node (tree-browser-state-rendered-lines tbs) (tree-browser-state-selected-idx tbs)))
     (update-tree-overlay! ctx state ov tbs new-idx (tree-browser-state-folded-set tbs))
     'handled]
    ;; Enter — toggle fold or navigate to entry
    [(memq keycode '(return kp-return enter kp-enter #\return))
     (define nodes (tree-browser-state-nodes tbs))
     (define idx (tree-browser-state-selected-idx tbs))
     (cond
       [(and (>= idx 0) (< idx (length nodes)))
        (define entry (list-ref nodes idx))
        (define entry-id (list-ref entry 0))
        (define new-folded (tree-toggle-fold (tree-browser-state-folded-set tbs) entry-id))
        (update-tree-overlay! ctx state ov tbs idx new-folded)]
       [else (void)])
     'handled]
    ;; f — fold/unfold
    [(eq? keycode #\f)
     (define nodes (tree-browser-state-nodes tbs))
     (define idx (tree-browser-state-selected-idx tbs))
     (cond
       [(and (>= idx 0) (< idx (length nodes)))
        (define entry (list-ref nodes idx))
        (define entry-id (list-ref entry 0))
        (define new-folded (tree-toggle-fold (tree-browser-state-folded-set tbs) entry-id))
        (update-tree-overlay! ctx state ov tbs idx new-folded)]
       [else (void)])
     'handled]
    ;; q — dismiss
    [(eq? keycode #\q)
     (set-box! (tui-ctx-ui-state-box ctx) (dismiss-overlay state))
     (mark-dirty! ctx)
     'handled]
    [else 'pass]))

(define (update-tree-overlay! ctx state ov tbs new-idx new-folded)
  ;; Re-render tree with updated selection/fold state and update overlay
  (define nodes (tree-browser-state-nodes tbs))
  (define-values (cols rows) (tui-screen-size))
  (define active-leaf-id #f) ;; Could look up from session index
  (define rendered (render-session-tree-folded nodes active-leaf-id cols new-folded))
  ;; Build styled lines with selection highlight
  (define styled-lines
    (for/list ([line (in-list rendered)]
               [i (in-naturals)])
      (if (= i new-idx)
          (list (cons (format "► ~a" line) '()))
          (list (cons (format "  ~a" line) '())))))
  (define new-tbs (tree-browser-state nodes new-idx new-folded rendered))
  (define new-ov (struct-copy overlay-state ov [content styled-lines] [extra new-tbs]))
  (set-box! (tui-ctx-ui-state-box ctx) (struct-copy ui-state state [active-overlay new-ov]))
  (mark-dirty! ctx))

(provide handle-tree-overlay-key
         update-tree-overlay!)
