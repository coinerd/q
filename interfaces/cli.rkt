#lang racket/base

;; interfaces/cli.rkt — facade re-exporting from cli/ sub-modules
;; Backward compatibility: all original exports preserved.
;; Decomposed in Issue #193 for QUAL-01/02.

(require "../cli/args.rkt"
         "../cli/render.rkt"
         "../cli/interactive.rkt"
         "../cli/init-wizard.rkt")

;; Match the ORIGINAL provide list exactly — do NOT use all-from-out
;; because some internal helpers (truncate-string, render-tokens, etc.)
;; would conflict with other modules (e.g., tui/state.rkt).
;; cli-config — explicit re-exports (struct-out removed from cli/args.rkt in v0.48.1)
(provide cli-config
         cli-config?
         cli-config-command
         cli-config-session-id
         cli-config-prompt
         cli-config-model
         cli-config-mode
         cli-config-project-dir
         cli-config-config-path
         cli-config-verbose?
         cli-config-max-turns
         cli-config-no-tools?
         cli-config-tools
         cli-config-session-dir
         cli-config-sessions-subcommand
         cli-config-sessions-args
         cli-config-keybindings-path
         cli-config-print-mode?
         cli-config-context-profile
         ;; from args.rkt
         parse-cli-args
         cli-config->runtime-config
         print-usage
         print-version
         q-version
         ;; from render.rkt
         format-event-for-terminal
         make-stream-markdown-writer
         render-markdown
         format-classified-error
         ;; from interactive.rkt
         run-cli-interactive
         run-cli-single
         parse-slash-command
         ;; from init-wizard.rkt
         run-init-wizard)
