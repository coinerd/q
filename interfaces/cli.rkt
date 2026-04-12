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
(provide (struct-out cli-config)
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
