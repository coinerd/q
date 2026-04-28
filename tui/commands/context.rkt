#lang racket/base

;; tui/commands/context.rkt — command context struct
;;
;; Shared context module for TUI command handlers.
;; Extracted from commands.rkt (ARCH-06) to avoid circular dependencies
;; between the main dispatcher and sub-module handlers.

(require "../state.rkt"
         "../../agent/event-bus.rkt"
         "../../runtime/model-registry.rkt")

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
         cmd-ctx-session-factory-runner)

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
         session-factory-runner) ; (or/c (boxof (or/c extension-registry? #f)) #f) — for ext command dispatch
  #:transparent)
