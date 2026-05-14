#lang racket/base

;; main.rkt — Entry point for q agent
;; Thin facade: CLI parsing → runtime construction → mode dispatch.
;; All business logic lives in wiring/ and interface modules.
;;
;; SDK consumers: prefer (require "interfaces/sdk-public.rkt") for the
;; stable, contracted public API. This module exports everything for
;; backward compatibility but includes many internal symbols.

(require racket/contract
         racket/dict
         "interfaces/cli.rkt"
         "interfaces/doctor.rkt"
         "interfaces/tui.rkt"
         "interfaces/sessions.rkt"
         "util/message.rkt"
         (only-in "llm/provider.rkt" provider? provider-name)
         (only-in "runtime/provider-factory.rkt" build-provider build-mock-provider local-provider?)
         (only-in "tools/registry-defaults.rkt" register-default-tools!)
         (only-in "wiring/run-modes.rkt"
                  build-runtime-from-cli
                  mode-for-config
                  run-interactive
                  run-single-shot
                  run-resume
                  run-json
                  run-rpc
                  run-print-mode)
         (only-in "runtime/session-config.rkt" session-config?)
         (only-in "runtime/agent-session.rkt"
                  make-agent-session
                  resume-agent-session
                  run-prompt!
                  session-id
                  session-history
                  fork-session
                  close-session!
                  agent-session?))

;; Minimal public surface. SDK consumers should use interfaces/sdk-public.rkt.
(provide main
         ;; Wiring functions (exported for testing)
         build-provider
         register-default-tools!
         build-runtime-from-cli
         mode-for-config)

;; Deprecated re-exports: use interfaces/sdk-public.rkt for SDK access.
;; These are kept for backward compatibility until v0.46.0.
(provide build-mock-provider
         local-provider?
         ;; Session functions (contract-out)
         (contract-out
           [make-agent-session (-> (or/c hash? session-config?) agent-session?)]
           [resume-agent-session (-> string? any/c agent-session?)]
           [fork-session (->* (agent-session?) ((or/c string? #f)) agent-session?)]
           [run-prompt!
            (->* (agent-session? (or/c string? message?))
                 (#:max-iterations (or/c exact-nonnegative-integer? #f)
                                   #:ensure-persisted! (or/c procedure? #f)
                                   #:buffer-or-append! (or/c procedure? #f))
                 any)]
           [close-session! (-> agent-session? void?)])
         session-id
         session-history
         run-interactive
         run-single-shot
         run-resume
         run-json
         run-rpc
         run-print-mode)

;; ============================================================
;; main — entry point
;; ============================================================

(define (main)
  (define cfg (parse-cli-args))
  (define mode (mode-for-config cfg))
  (case mode
    [(help)
     (print-usage)
     (exit 0)]
    [(version)
     (print-version)
     (exit 0)]
    [(doctor)
     (define code (run-doctor))
     (exit code)]
    [(init)
     (run-init-wizard)
     (exit 0)]
    [(sessions)
     (run-sessions-command cfg)
     (exit 0)]
    [(interactive)
     (define rt-config (build-runtime-from-cli cfg))
     (define prov (dict-ref rt-config 'provider #f))
     (define prov-name (and (provider? prov) (provider-name prov)))
     (if (eq? (cli-config-command cfg) 'resume)
         (run-resume cfg rt-config)
         (run-interactive cfg rt-config #:provider-name prov-name))]
    [(single)
     (define rt-config (build-runtime-from-cli cfg))
     (run-single-shot cfg rt-config)]
    [(print)
     (define rt-config (build-runtime-from-cli cfg))
     (run-print-mode cfg rt-config)]
    [(json)
     (define rt-config (build-runtime-from-cli cfg))
     (run-json cfg rt-config)]
    [(rpc)
     (define rt-config (build-runtime-from-cli cfg))
     (run-rpc cfg rt-config)]
    [(tui)
     (define rt-config (build-runtime-from-cli cfg))
     (run-tui-with-runtime rt-config cfg)]))

(module+ main
  (main))
