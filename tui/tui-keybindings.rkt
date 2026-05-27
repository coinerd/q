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
                  tui-ctx-prev-ubuf-box
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
         ;; W20: key dispatch extracted to keybindings/key-dispatch.rkt
         (only-in "keybindings/key-dispatch.rkt" dispatch-keymap-action handle-key)
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
         tui-ctx-previous-frame-box
         tui-ctx-prev-ubuf-box
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
          [current-keybindings-path (->* () ((or/c path-string? #f)) any/c)]
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
                    tui-ctx-previous-frame-box
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
;; Key handling — extracted to keybindings/key-dispatch.rkt (W20)
;; ============================================================

;; ============================================================
;; Mouse, tree overlay — extracted to selection.rkt (W16)
;; ============================================================

;; ============================================================
;; Inline bash expansion helper (G3.3)
;; ============================================================

;; Convenience wrapper: expand !! in text using the last prompt from ctx.
(define (input-expand-last-prompt text ctx)
  (expand-inline-bash text (unbox (tui-ctx-last-prompt-box ctx))))
