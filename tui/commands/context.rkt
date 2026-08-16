#lang racket/base

;; tui/commands/context.rkt — command context struct
;;
;; Shared context module for TUI command handlers.
;; Extracted from commands.rkt (ARCH-06) to avoid circular dependencies
;; between the main dispatcher and sub-module handlers.

(require "../state.rkt"
         "../../util/event/event-bus.rkt"
         "../../runtime/provider/model-registry.rkt")

(provide cmd-ctx
         cmd-ctx?
         cmd-ctx-state-box
         cmd-ctx-running-box
         cmd-ctx-event-bus
         cmd-ctx-session-dir
         cmd-ctx-needs-redraw-box
         cmd-ctx-model-registry-box
         cmd-ctx-last-prompt-box
         cmd-ctx-session-runner
         cmd-ctx-input-text-box
         cmd-ctx-extension-registry-box
         cmd-ctx-session-factory-runner
         cmd-ctx-agent-session-box
         cmd-ctx-goal-cancel-box
         ext-command-dispatcher-box)

;; ============================================================
;; Command context — lightweight alternative to tui-ctx
;; ============================================================

;; Holds the mutable references that command handlers need.
;; Created by interfaces/tui.rkt from a tui-ctx.
(struct cmd-ctx
        (state-box ; (boxof ui-state)
         running-box ; (boxof boolean)
         event-bus ; event-bus? or #f
         session-dir ; (or/c path-string? #f)
         needs-redraw-box ; (boxof boolean)
         model-registry-box ; (or/c (boxof (or/c model-registry? #f)) #f)
         last-prompt-box ; (boxof (or/c string? #f)) — last user prompt for /retry
         session-runner ; (string -> void) or #f — for /retry resubmission
         input-text-box ; (boxof string?) — raw input text for commands like /activate
         extension-registry-box
         session-factory-runner
         agent-session-box ; (boxof (or/c agent-session? #f)) — live session for goal-runner
         goal-cancel-box) ; (boxof boolean?) — #t signals goal thread to stop
  #:transparent)

;; D2 (#9351): optional dispatcher that routes /retry of GSD wave EXECUTE
;; prompts through the extension /go pipeline (full campaign continuation:
;; attempt accounting, fresh executor sessions, durable state updates).
;; Held in a module-level box so the fifteen positional cmd-ctx constructor
;; call sites stay source-compatible. The TUI wiring populates it with
;; commands:process-extension-command; #f preserves the legacy plain
;; resubmit path (and keeps non-TUI embedders unchanged).
(define ext-command-dispatcher-box (box #f))
