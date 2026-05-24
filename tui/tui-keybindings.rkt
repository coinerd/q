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

(require racket/contract
         racket/string
         racket/match
         racket/list
         "../tui/terminal.rkt"
         "../tui/state.rkt"
         "../tui/input.rkt"
         "../tui/render.rkt"
         "../tui/layout.rkt"
         "../tui/clipboard.rkt"
         "../tui/char-width.rkt"
         "../util/protocol-types.rkt"
         "../agent/event-bus.rkt"
         ;; W15: tui-ctx struct, constructor, mark-dirty! extracted to context.rkt
         (only-in "context.rkt"
                  tui-ctx
                  tui-ctx?
                  tui-ctx-ui-state-box
                  tui-ctx-input-state-box
                  tui-ctx-event-bus
                  tui-ctx-session-runner
                  tui-ctx-running-box
                  tui-ctx-event-ch
                  tui-ctx-session-dir
                  tui-ctx-needs-redraw-box
                  tui-ctx-term-box
                  tui-ctx-ubuf-box
                  tui-ctx-model-registry-box
                  tui-ctx-previous-frame-box
                  tui-ctx-last-prompt-box
                  tui-ctx-extension-registry-box
                  tui-ctx-session-queue-box
                  tui-ctx-session-factory-runner
                  make-tui-ctx
                  mark-dirty!)
         ;; W16: selection-text, handle-mouse, tree overlay extracted to selection.rkt
         (only-in "selection.rkt"
                  selection-text
                  handle-mouse
                  handle-tree-overlay-key
                  update-tree-overlay!)
         (prefix-in commands: "../tui/commands.rkt")
         (only-in "../tui/commands.rkt" cmd-ctx?)
         (only-in "../tui/command-parse.rkt" parsed-command?)
         "../tui/keymap.rkt"
         "keybindings/binding-resolver.rkt"
         "keybindings/default-map.rkt"
         "keybindings/mode-map.rkt")

;; ── tui-ctx struct ──
(provide tui-ctx
         tui-ctx?
         tui-ctx-ui-state-box
         tui-ctx-input-state-box
         tui-ctx-event-bus
         tui-ctx-session-runner
         tui-ctx-running-box
         tui-ctx-event-ch
         tui-ctx-session-dir
         tui-ctx-needs-redraw-box
         tui-ctx-term-box
         tui-ctx-ubuf-box
         tui-ctx-model-registry-box
         tui-ctx-previous-frame-box
         tui-ctx-last-prompt-box
         tui-ctx-extension-registry-box
         tui-ctx-session-queue-box
         tui-ctx-session-factory-runner
         ;; Re-exports (no contract needed — re-exported from other modules)
         copy-text!
         copy-selection!
         current-clipboard-mode
         clipboard-backend-available?
         clipboard-paste
         styled-line->text
         input-undo
         input-redo
         input-kill-word-backward
         input-kill-to-beginning
         input-kill-to-end
         input-yank
         input-cursor-word-left
         input-cursor-word-right
         input-insert-string
         default-keymap
         load-user-keymap
         load-keybindings
         make-mode-keymap
         mode-overlay->keymap
         ;; W16: re-exported from selection.rkt
         handle-tree-overlay-key
         update-tree-overlay!
         (contract-out
          [make-tui-ctx
           (->* ()
                (#:event-bus (or/c event-bus? #f)
                             #:session-runner procedure?
                             #:session-dir (or/c path-string? #f)
                             #:model-registry any/c
                             #:extension-registry any/c
                             #:session-queue any/c
                             #:session-factory-runner any/c)
                tui-ctx?)]
          [mark-dirty! (-> tui-ctx? void?)]
          [handle-key (-> tui-ctx? any/c any/c)]
          [handle-mouse (-> tui-ctx? any/c any/c)]
          [selection-text (-> tui-ctx? ui-state? (or/c string? #f))]
          [process-slash-command
           (->* (tui-ctx? (or/c string? symbol? parsed-command?)) (string?) (or/c 'continue 'quit))]
          [tui-ctx->cmd-ctx (-> tui-ctx? cmd-ctx?)]
          [reload-keymap! (-> void?)]
          [current-keybindings-path (->* () ((or/c path-string? #f)) (or/c path-string? #f))]
          [input-expand-last-prompt (-> string? tui-ctx? string?)]))

;; ============================================================
;; TUI context — extracted to context.rkt (W15)
;; ============================================================
;; tui-ctx struct, make-tui-ctx, and mark-dirty! are now imported
;; from q/tui/context.rkt and re-exported for backward compatibility.

;; ============================================================

;; ============================================================
;; Selection math — extracted to selection.rkt (W16)
;; ============================================================

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
                    (tui-ctx-extension-registry-box ctx)
                    (tui-ctx-session-factory-runner ctx)))

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
;; get-active-keymap and keycode->key-spec-from-msg imported from binding-resolver

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
    [(tui.editor.paste paste)
     (define text (clipboard-paste))
     (when text
       (set-box! (tui-ctx-input-state-box ctx) (input-insert-string inp text)))
     'handled]
    [(tui.editor.expand-file-ref expand-file-ref)
     (set-box! (tui-ctx-input-state-box ctx) (input-expand-file-ref inp))
     'handled]
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

;; reload-keymap! imported from binding-resolver

(define (handle-key ctx keycode)
  (define inp (unbox (tui-ctx-input-state-box ctx)))
  (define state (unbox (tui-ctx-ui-state-box ctx)))
  ;; Any key that reaches here may change state — mark for redraw
  (mark-dirty! ctx)

  ;; Check configurable keymap first
  (define km (get-active-keymap))
  (define ks (keycode->key-spec-from-msg keycode))
  (define action (and ks (keymap-lookup km ks)))
  (match keycode
    [(? (lambda (k) (and action (eq? (dispatch-keymap-action ctx inp state action) 'handled))))
     'continue]
    ;; Fallback to hardcoded behavior
    [(? char?)
     (case keycode
       [(#\return)
        ;; Enter → submit input
        (define-values (text new-inp) (input-submit inp))
        (set-box! (tui-ctx-input-state-box ctx) new-inp)
        (match text
          [#f 'continue]
          [(? input-slash-command)
           (define cmd (parse-tui-slash-command text))
           (list 'command (or cmd 'unknown) text)]
          ;; User entry added by submit handler in tui-render-loop.rkt
          [_ (list 'submit text)])]
       [(#\newline)
        ;; Ctrl+J → insert newline (multi-line input)
        (set-box! (tui-ctx-input-state-box ctx) (input-insert-newline inp))
        'continue]

       [(#\tab)
        ;; Tab — expand @ file reference at cursor (G3.2)
        (set-box! (tui-ctx-input-state-box ctx) (input-expand-file-ref inp))
        'continue]
       [(#\u001b) 'continue]
       [(#\backspace)
        (set-box! (tui-ctx-input-state-box ctx) (input-backspace inp))
        'continue]
       [(#\rubout)
        (set-box! (tui-ctx-input-state-box ctx) (input-backspace inp))
        'continue]
       [else
        ;; Regular printable character
        (set-box! (tui-ctx-input-state-box ctx) (input-insert-char inp keycode))
        'continue])]

    ;; Symbol keys (arrows, function keys, etc.)
    [(? symbol?)
     (case keycode
       ;; Enter key — charterm sends 'return symbol, not #\return char
       [(return kp-return enter kp-enter)
        ;; Submit input (same logic as #\return/#\newline in char branch)
        (define-values (text new-inp) (input-submit inp))
        (set-box! (tui-ctx-input-state-box ctx) new-inp)
        (match text
          [#f 'continue]
          [(? input-slash-command)
           (define cmd (parse-tui-slash-command text))
           (list 'command (or cmd 'unknown) text)]
          ;; User entry added by submit handler in tui-render-loop.rkt
          [_ (list 'submit text)])]

       [(tab)
        ;; Tab — expand @ file reference at cursor (G3.2)
        (set-box! (tui-ctx-input-state-box ctx) (input-expand-file-ref inp))
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
              (set-box! (tui-ctx-ui-state-box ctx)
                        (clear-streaming (set-pending-tool-name (set-busy state #f) #f)))))
        'continue]

       ;; Plain arrow keys and navigation (no keymap entry for unmodified)
       [(left kp-left)
        (set-box! (tui-ctx-input-state-box ctx) (input-cursor-left inp))
        'continue]
       [(right kp-right)
        (set-box! (tui-ctx-input-state-box ctx) (input-cursor-right inp))
        'continue]
       [(home kp-home)
        (set-box! (tui-ctx-input-state-box ctx) (input-home inp))
        'continue]
       [(end kp-end)
        (set-box! (tui-ctx-input-state-box ctx) (input-end inp))
        'continue]
       [(up kp-up)
        (set-box! (tui-ctx-input-state-box ctx) (input-history-up inp))
        'continue]
       [(down kp-down)
        (set-box! (tui-ctx-input-state-box ctx) (input-history-down inp))
        'continue]
       [(delete kp-delete)
        (set-box! (tui-ctx-input-state-box ctx) (input-delete inp))
        'continue]
       [(pgup kp-pgup)
        (let ()
          (define-values (_cols rows) (tui-screen-size))
          (define layout (compute-layout _cols rows))
          (set-box! (tui-ctx-ui-state-box ctx)
                    (scroll-up state (max 1 (tui-layout-transcript-height layout)))))
        'continue]
       [(pgdn kp-pgdn)
        (let ()
          (define-values (_cols rows) (tui-screen-size))
          (define layout (compute-layout _cols rows))
          (set-box! (tui-ctx-ui-state-box ctx)
                    (scroll-down state (max 1 (tui-layout-transcript-height layout)))))
        'continue]
       [else 'continue])]

    [_ 'continue]))

;; ============================================================
;; Mouse, tree overlay — extracted to selection.rkt (W16)
;; ============================================================

;; ============================================================
;; Inline bash expansion helper (G3.3)
;; ============================================================

;; Convenience wrapper: expand !! in text using the last prompt from ctx.
(define (input-expand-last-prompt text ctx)
  (expand-inline-bash text (unbox (tui-ctx-last-prompt-box ctx))))
