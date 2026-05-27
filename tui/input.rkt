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

(require racket/contract
         racket/string
         "command-parse.rkt"
         "input/state-types.rkt"
         "input/editing-ops.rkt"
         "input/history-ops.rkt"
         "input/completion-ops.rkt")

;; input-state is used with struct-copy in many consumers (tests + sub-modules),
;; so we keep struct-out for it.  mouse-event is not used with struct-copy,
;; so we export it via contract-out.
(provide input-state
         input-state?
         input-state-buffer
         input-state-cursor
         input-state-history
         input-state-history-idx
         input-state-saved-text
         input-state-scroll-offset
         input-state-undo-stack
         input-state-redo-stack
         input-state-kill-ring
         (contract-out
          [mouse-event? (-> any/c boolean?)]
          [mouse-event (->* (symbol? exact-integer? exact-integer? exact-integer?) () mouse-event?)]
          [mouse-event-type (-> mouse-event? symbol?)]
          [mouse-event-button (-> mouse-event? exact-integer?)]
          [mouse-event-x (-> mouse-event? exact-integer?)]
          [mouse-event-y (-> mouse-event? exact-integer?)]
          ;; Constructors
          [initial-input-state (-> input-state?)]
          ;; Editing
          [input-insert-char (-> input-state? char? input-state?)]
          [input-insert-newline (-> input-state? input-state?)]
          [input-backspace (-> input-state? input-state?)]
          [input-delete (-> input-state? input-state?)]
          [input-cursor-left (-> input-state? input-state?)]
          [input-cursor-right (-> input-state? input-state?)]
          [input-home (-> input-state? input-state?)]
          [input-end (-> input-state? input-state?)]
          [input-clear (-> input-state? input-state?)]
          ;; Undo/Redo
          [input-undo (-> input-state? input-state?)]
          [input-redo (-> input-state? input-state?)]
          ;; Kill ring
          [input-kill-word-backward (-> input-state? input-state?)]
          [input-kill-to-beginning (-> input-state? input-state?)]
          [input-kill-to-end (-> input-state? input-state?)]
          [input-yank (-> input-state? input-state?)]
          ;; Word navigation
          [input-cursor-word-left (-> input-state? input-state?)]
          [input-cursor-word-right (-> input-state? input-state?)]
          ;; Paste
          [input-insert-string (-> input-state? string? input-state?)]
          ;; History
          [input-history-push (-> input-state? string? input-state?)]
          [input-history-up (-> input-state? input-state?)]
          [input-history-down (-> input-state? input-state?)]
          ;; Submission
          [input-submit (-> input-state? (values (or/c string? #f) input-state?))]
          [input-current-text (-> input-state? string?)]
          [input-at-beginning? (-> input-state? boolean?)]
          [input-at-end? (-> input-state? boolean?)]
          [input-empty? (-> input-state? boolean?)]
          ;; Slash commands
          [input-slash-command (-> string? boolean?)]
          [parse-tui-slash-command (-> string? (or/c parsed-command? symbol? #f))]
          ;; Horizontal scroll
          [input-visible-window
           (-> input-state?
               exact-nonnegative-integer?
               (values string? exact-integer? exact-integer?))]
          [INPUT-PROMPT-WIDTH exact-nonnegative-integer?]
          ;; Mouse events
          [parse-mouse-event (-> any/c (or/c mouse-event? #f))]
          [decode-mouse-x10 (-> exact-integer? exact-integer? exact-integer? (or/c list? #f))]
          [decode-mouse-message (-> any/c (or/c list? #f))]
          ;; Selection helpers
          [normalize-selection-range
           (-> (cons/c exact-nonnegative-integer? exact-nonnegative-integer?)
               (cons/c exact-nonnegative-integer? exact-nonnegative-integer?)
               (values exact-nonnegative-integer?
                       exact-nonnegative-integer?
                       exact-nonnegative-integer?
                       exact-nonnegative-integer?))]
          ;; IME cursor markers
          [CURSOR_MARKER char?]
          [cursor-marker-string (-> string?)]
          [strip-cursor-markers (-> string? string?)]
          [has-cursor-markers? (-> string? boolean?)]
          [insert-cursor-marker (-> string? exact-nonnegative-integer? string?)]
          ;; File reference expansion
          [input-expand-file-ref (-> input-state? input-state?)]
          ;; Inline bash expansion
          [expand-inline-bash (-> string? (or/c string? #f) string?)]))

;; ============================================================
;; Slash commands (kept here -- trivial dispatch)
;; ============================================================

(define (input-slash-command text)
  (and (> (string-length text) 0) (char=? (string-ref text 0) #\/)))

(define (parse-tui-slash-command text)
  (parse-command-name text))
