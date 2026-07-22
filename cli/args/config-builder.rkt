#lang racket/base

;; q/cli/args/config-builder.rkt — cli-config struct, acc->cli-config, cli-config->runtime-config
;;
;; Sub-module of cli/args.rkt (split for modularity).
;; Provides cli-config struct, make-initial-acc, acc->cli-config, cli-config->runtime-config.

(require racket/match
         "flags.rkt" ; acc-ref, acc-set, acc-cons
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
         cli-config-memory
         cli-config-agent-pool
         cli-config-parallel?
         make-initial-acc
         acc->cli-config
         cli-config->runtime-config)

;; ============================================================
;; CLI config struct
;; ============================================================

(struct cli-config
        (command ; symbol: 'chat | 'prompt | 'resume | 'help | 'version | 'doctor | 'sessions
         session-id ; string or #f
         prompt ; string or #f
         model ; string or #f
         mode ; symbol: 'interactive | 'single | 'json | 'rpc | 'tui | 'print | 'gui
         project-dir ; path-string or #f
         config-path ; path-string or #f
         verbose? ; boolean
         max-turns ; integer (default: 10)
         no-tools? ; boolean
         tools ; list of string (tool names to enable)
         session-dir ; path-string or #f
         sessions-subcommand ; symbol: 'list | 'info | 'delete | 'verify | #f
         sessions-args ; list of string (subcommand args)
         keybindings-path ; path-string or #f -- custom keybindings file (#1118)
         print-mode? ; boolean -- -p/--print flag (G9.3)
         context-profile ; symbol: off|observe|bounded|self-healing|full, or #f
         memory
         agent-pool ; integer or #f — max concurrent subagents (v0.99.23 §5.1)
         parallel?) ; boolean — --parallel mode (v0.99.23 §5.1)
  #:transparent)

;; Helper: construct a "help" config (used for parse errors and --help)
(define (make-help-config)
  (cli-config 'help #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f #f #f #f #f))

;; Build the initial accumulator alist from defaults.
(define (make-initial-acc)
  `((command . chat) (session-id . #f)
                     (prompt . #f)
                     (model . #f)
                     (mode . interactive)
                     (project-dir . #f)
                     (config-path . #f)
                     (verbose? . #f)
                     (max-turns . 10)
                     (no-tools? . #f)
                     (tools . ())
                     (session-dir . #f)
                     (keybindings-path . #f)
                     (print-mode? . #f)
                     (memory . #f)
                     (agent-pool . #f)
                     (parallel? . #f)))

;; Construct a cli-config from an accumulator alist.
(define (acc->cli-config acc)
  (define final-command
    (let ([cmd (acc-ref acc 'command)]
          [sid (acc-ref acc 'session-id)]
          [prompt (acc-ref acc 'prompt)])
      (match (cons cmd sid)
        [(cons 'help _) 'help]
        [(cons 'version _) 'version]
        [(cons 'init _) 'init]
        [(cons 'sessions _) 'sessions]
        [(cons 'chat (not #f)) 'resume]
        [_
         (match prompt
           [#f cmd]
           [_ 'prompt])])))
  (define final-mode
    (let ([m (acc-ref acc 'mode)])
      (match m
        ['json 'json]
        ['rpc 'rpc]
        ['tui 'tui]
        ['gui 'gui]
        ['print 'print]
        [_
         (match final-command
           ['prompt 'single]
           [_ 'interactive])])))
  (cli-config final-command
              (acc-ref acc 'session-id)
              (acc-ref acc 'prompt)
              (acc-ref acc 'model)
              final-mode
              (acc-ref acc 'project-dir)
              (acc-ref acc 'config-path)
              (acc-ref acc 'verbose?)
              (acc-ref acc 'max-turns)
              (acc-ref acc 'no-tools?)
              (reverse (acc-ref acc 'tools))
              (acc-ref acc 'session-dir)
              #f ; sessions-subcommand
              '() ; sessions-args
              (acc-ref acc 'keybindings-path)
              (acc-ref acc 'print-mode?)
              (acc-ref acc 'context-profile)
              (acc-ref acc 'memory)
              (acc-ref acc 'agent-pool)
              (acc-ref acc 'parallel?)))

;; ============================================================
;; Pure: cli-config->runtime-config
;; ============================================================

;; Convert cli-config to a hash suitable for make-agent-session / resume-agent-session.
;; Does NOT include 'provider or 'tool-registry or 'event-bus -- those are
;; filled in by the runner or the caller.

(define (cli-config->runtime-config cfg)
  ;; v0.35.2 (W-03): Build immutable hash instead of mutable
  (define base
    (make-immutable-hash (list (cons 'max-iterations (cli-config-max-turns cfg))
                               (cons 'no-tools? (cli-config-no-tools? cfg))
                               (cons 'tools (cli-config-tools cfg)))))
  (let* ([h (if (cli-config-model cfg)
                (hash-set base 'model (cli-config-model cfg))
                base)]
         [h (if (cli-config-project-dir cfg)
                (hash-set h 'project-dir (cli-config-project-dir cfg))
                h)]
         [h (if (cli-config-config-path cfg)
                (hash-set h 'config-path (cli-config-config-path cfg))
                h)]
         [h (if (cli-config-session-id cfg)
                (hash-set h 'session-id (cli-config-session-id cfg))
                h)]
         [h (if (cli-config-session-dir cfg)
                (hash-set h 'session-dir (cli-config-session-dir cfg))
                h)]
         [h (if (cli-config-print-mode? cfg)
                (hash-set h 'print-mode #t)
                h)]
         [h (if (cli-config-memory cfg)
                (hash-set h 'memory-backend (cli-config-memory cfg))
                h)])
    h))
