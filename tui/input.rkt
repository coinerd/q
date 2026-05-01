#lang racket/base

;; q/tui/input.rkt -- Input buffer, cursor movement, history, slash commands
;;
;; Façade re-exporting from focused sub-modules:
;;   - input/state-types.rkt    -- input-state struct, mouse events, IME markers
;;   - input/editing-ops.rkt    -- cursor movement, deletion, undo/redo, kill ring
;;   - input/history-ops.rkt    -- history traversal, filtering, submission
;;   - input/completion-ops.rkt -- file reference expansion, inline bash expansion
;;
;; This file keeps only the slash-command dispatch and re-exports the full public API.

(require racket/string
         "command-parse.rkt"
         "input/state-types.rkt"
         "input/editing-ops.rkt"
         "input/history-ops.rkt"
         "input/completion-ops.rkt")

(provide (struct-out input-state)
         (struct-out mouse-event)

         ;; Constructors
         initial-input-state

         ;; Editing
         input-insert-char
         input-insert-newline
         input-backspace
         input-delete
         input-cursor-left
         input-cursor-right
         input-home
         input-end
         input-clear

         ;; Undo/Redo
         input-undo
         input-redo

         ;; Kill ring
         input-kill-word-backward
         input-kill-to-beginning
         input-kill-to-end
         input-yank

         ;; Word navigation
         input-cursor-word-left
         input-cursor-word-right

         ;; Paste
         input-insert-string

         ;; History
         input-history-push
         input-history-up
         input-history-down

         ;; Submission
         input-submit
         input-current-text
         input-at-beginning?
         input-at-end?
         input-empty?

         ;; Slash commands
         input-slash-command
         parse-tui-slash-command

         ;; Horizontal scroll
         input-visible-window
         INPUT-PROMPT-WIDTH

         ;; Mouse events
         parse-mouse-event
         decode-mouse-x10
         decode-mouse-tui-term

         ;; Selection helpers
         normalize-selection-range

         ;; IME cursor markers
         CURSOR_MARKER
         cursor-marker-string
         strip-cursor-markers
         has-cursor-markers?
         insert-cursor-marker

         ;; File reference expansion
         input-expand-file-ref

         ;; Inline bash expansion
         expand-inline-bash)

;; ============================================================
;; Slash commands (kept here -- trivial dispatch)
;; ============================================================

(define (input-slash-command text)
  (and (> (string-length text) 0) (char=? (string-ref text 0) #\/)))

(define (parse-tui-slash-command text)
  (parse-command-name text))
