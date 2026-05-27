#lang racket/base

;; interfaces/tui.rkt — facade re-exporting from tui/ sub-modules
;; Backward compatibility: all original exports preserved.
;; Decomposed in Issue #194 for QUAL-01/02.

(require "../tui/tui-keybindings.rkt"
         "../tui/tui-render-loop.rkt"
         "../tui/tui-init.rkt")

;; Match the ORIGINAL provide list exactly — do NOT use all-from-out
;; to avoid exporting internal helpers that could conflict.
;; ── from tui-init.rkt ──
(provide run-tui
         run-tui-with-runtime

         ;; ── from tui-keybindings.rkt (struct + make) ──
         make-tui-ctx
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

         ;; ── from tui-keybindings.rkt (key/mouse/selection/commands) ──
         handle-key
         handle-mouse
         selection-text
         process-slash-command
         mark-dirty!
         styled-line->text

         ;; ── from tui-keybindings.rkt (re-exports from terminal/clipboard) ──
         copy-text!
         copy-selection!
         current-clipboard-mode
         clipboard-backend-available?

         ;; ── from tui-render-loop.rkt ──
         fix-sgr-bg-black
         render-frame!
         draw-frame
         tui-ctx-resize-ubuf!
         decode-mouse-x10

         ;; ── from tui-init.rkt ──
         subscribe-runtime-events!)
