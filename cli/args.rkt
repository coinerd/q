#lang racket/base

;; q/cli/args.rkt — CLI argument parsing facade
;;
;; Thin facade re-exporting from sub-modules under cli/args/.
;; The implementation is split into:
;;   cli/args/flags.rkt          — flag-def struct, FLAG-DEFINITIONS table, acc helpers
;;   cli/args/parser.rkt         — parse-cli-args, apply-flag, handle-positional
;;   cli/args/config-builder.rkt — cli-config struct, acc->cli-config, cli-config->runtime-config
;;   cli/args/print.rkt          — print-usage, print-version
;;
;; Exports everything the original monolithic file exported.

(require racket/contract
         "args/flags.rkt" ; flag-def, FLAG-DEFINITIONS, acc-ref, acc-set, acc-cons, lookup tables
         "args/parser.rkt" ; parse-cli-args, apply-flag, handle-positional
         "args/config-builder.rkt" ; cli-config struct, make-initial-acc, acc->cli-config, cli-config->runtime-config
         "args/print.rkt" ; print-usage, print-version
         "../util/version.rkt" ; q-version
         )

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
         (contract-out [parse-cli-args
                        (->* () ((or/c (vectorof string?) (listof string?) #f)) cli-config?)]
                       [cli-config->runtime-config (-> cli-config? hash?)]
                       [print-usage (->* () ((or/c output-port? #f)) void?)]
                       [print-version (->* () (output-port?) void?)])
         ;; q-version imported from util/version.rkt (Issue #203)
         q-version
         cli-config-memory
         cli-config-agent-pool
         cli-config-parallel?)
