#lang racket/base

;; wiring/run-modes.rkt — Mode dispatch facade
;;
;; v0.99.43 W0: Monolithic module (658 lines) decomposed into sub-modules:
;;   run-modes/base.rkt — mode resolution + MCP governed execution plumbing
;;   run-modes/loop.rkt  — wire-runtime-parameters! + reload-config!
;;   run-modes/cli.rkt   — build-runtime-from-cli (CLI init + config wiring)
;;
;; This file is a pure facade: no logic, only re-exports. All mode runners
;; live in run-interactive.rkt / run-json-rpc.rkt.

(require racket/contract
         "../interfaces/cli.rkt"
         (only-in "../runtime/session/session-config.rkt" session-config?)
         (only-in "../runtime/provider/model-registry.rkt" model-registry?)
         (only-in "../runtime/agent-session.rkt" open-or-resume-session)
         (only-in "extension-setup.rkt" load-extensions-from-dir!)
         "run-modes/base.rkt"
         "run-modes/loop.rkt"
         "run-modes/cli.rkt"
         "run-interactive.rkt"
         "run-json-rpc.rkt")

(provide (contract-out [build-runtime-from-cli (-> cli-config? session-config?)]
                       [mode-for-config (-> cli-config? symbol?)]
                       [reload-config! (-> session-config? (values session-config? model-registry?))])
         ;; Direct re-exports (no contracts needed — re-exported)
         load-extensions-from-dir!
         make-terminal-subscriber
         run-interactive
         run-single-shot
         run-resume
         run-json
         run-rpc
         run-print-mode
         wire-runtime-parameters!
         make-mcp-governed-execute-fn
         ;; F-11: Re-export canonical session resolver
         open-or-resume-session)
