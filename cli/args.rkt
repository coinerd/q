#lang racket/base

;; q/cli/args.rkt — CLI argument parsing, config conversion, usage/version display
;;
;; Extracted from interfaces/cli.rkt for modularity (Issue #193).
;;
;; Provides:
;;   cli-config struct + accessors
;;   parse-cli-args      — pure argument parsing
;;   cli-config->runtime-config — config hash conversion
;;   print-usage         — usage display
;;   print-version       — version display
;;   q-version           — version constant

(require racket/string
         racket/format)

(provide (struct-out cli-config)
         parse-cli-args
         cli-config->runtime-config
         print-usage
         print-version
         q-version)

;; ============================================================
;; CLI config struct
;; ============================================================

(struct cli-config
        (command ; symbol: 'chat | 'prompt | 'resume | 'help | 'version | 'doctor | 'sessions
         session-id ; string or #f
         prompt ; string or #f
         model ; string or #f
         mode ; symbol: 'interactive | 'single | 'json | 'rpc | 'tui
         project-dir ; path-string or #f
         config-path ; path-string or #f
         verbose? ; boolean
         max-turns ; integer (default: 10)
         no-tools? ; boolean
         tools ; list of string (tool names to enable)
         session-dir ; path-string or #f
         sessions-subcommand ; symbol: 'list | 'info | 'delete | #f
         sessions-args ; list of string (subcommand args)
         )
  #:transparent)

;; ============================================================
;; Pure: parse-cli-args
;; ============================================================

;; Parse a vector of argument strings into a cli-config.
;; Returns a cli-config with command='help if parsing fails.

(define (parse-cli-args [args (current-command-line-arguments)])
  (define vec
    (if (vector? args)
        args
        (list->vector args)))
  (define n (vector-length vec))

  (let loop ([i 0]
             [command 'chat]
             [session-id #f]
             [prompt #f]
             [model #f]
             [mode 'interactive]
             [project-dir #f]
             [config-path #f]
             [verbose? #f]
             [max-turns 10]
             [no-tools? #f]
             [tools '()]
             [session-dir #f])
    (cond
      ;; ── Done ──
      [(>= i n)
       ;; Determine final command and mode
       (define final-command
         (cond
           [(eq? command 'help) 'help]
           [(eq? command 'version) 'version]
           [(eq? command 'init) 'init]
           [(eq? command 'sessions) 'sessions]
           [(and session-id (eq? command 'chat)) 'resume]
           [prompt 'prompt]
           [else command]))
       (define final-mode
         (cond
           [(eq? mode 'json) 'json]
           [(eq? mode 'rpc) 'rpc]
           [(eq? mode 'tui) 'tui]
           [(eq? final-command 'prompt) 'single]
           [else 'interactive]))
       (cli-config final-command
                   session-id
                   prompt
                   model
                   final-mode
                   project-dir
                   config-path
                   verbose?
                   max-turns
                   no-tools?
                   (reverse tools)
                   session-dir
                   #f ; sessions-subcommand
                   '() ; sessions-args
                   )]

      ;; ── --help / -h ──
      [(or (equal? (vector-ref vec i) "--help") (equal? (vector-ref vec i) "-h"))
       (cli-config 'help #f #f #f 'interactive #f #f #f 10 #f '() #f #f '())]

      ;; ── --version ──
      [(equal? (vector-ref vec i) "--version")
       (cli-config 'version #f #f #f 'interactive #f #f #f 10 #f '() #f #f '())]

      ;; ── --session <id> ──
      [(equal? (vector-ref vec i) "--session")
       (if (< (add1 i) n)
           (loop (+ i 2)
                 'resume
                 (vector-ref vec (add1 i))
                 prompt
                 model
                 mode
                 project-dir
                 config-path
                 verbose?
                 max-turns
                 no-tools?
                 tools
                 session-dir)
           (cli-config 'help #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))]

      ;; ── --model <name> ──
      [(equal? (vector-ref vec i) "--model")
       (if (< (add1 i) n)
           (loop (+ i 2)
                 command
                 session-id
                 prompt
                 (vector-ref vec (add1 i))
                 mode
                 project-dir
                 config-path
                 verbose?
                 max-turns
                 no-tools?
                 tools
                 session-dir)
           (cli-config 'help #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))]

      ;; ── --project-dir <path> ──
      [(equal? (vector-ref vec i) "--project-dir")
       (if (< (add1 i) n)
           (loop (+ i 2)
                 command
                 session-id
                 prompt
                 model
                 mode
                 (vector-ref vec (add1 i))
                 config-path
                 verbose?
                 max-turns
                 no-tools?
                 tools
                 session-dir)
           (cli-config 'help #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))]

      ;; ── --config <path> ──
      [(equal? (vector-ref vec i) "--config")
       (if (< (add1 i) n)
           (loop (+ i 2)
                 command
                 session-id
                 prompt
                 model
                 mode
                 project-dir
                 (vector-ref vec (add1 i))
                 verbose?
                 max-turns
                 no-tools?
                 tools
                 session-dir)
           (cli-config 'help #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))]

      ;; ── --verbose / -v ──
      [(or (equal? (vector-ref vec i) "--verbose") (equal? (vector-ref vec i) "-v"))
       (loop (add1 i)
             command
             session-id
             prompt
             model
             mode
             project-dir
             config-path
             #t
             max-turns
             no-tools?
             tools
             session-dir)]

      ;; ── --max-turns <n> ──
      [(equal? (vector-ref vec i) "--max-turns")
       (if (< (add1 i) n)
           (let ([n-str (vector-ref vec (add1 i))])
             (define n-val (string->number n-str))
             (if (and n-val (exact-positive-integer? n-val))
                 (loop (+ i 2)
                       command
                       session-id
                       prompt
                       model
                       mode
                       project-dir
                       config-path
                       verbose?
                       n-val
                       no-tools?
                       tools
                       session-dir)
                 ;; Non-numeric max-turns → help
                 (cli-config 'help #f #f #f 'interactive #f #f #f 10 #f '() #f #f '())))
           (cli-config 'help #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))]

      ;; ── --no-tools ──
      [(equal? (vector-ref vec i) "--no-tools")
       (loop (add1 i)
             command
             session-id
             prompt
             model
             mode
             project-dir
             config-path
             verbose?
             max-turns
             #t
             tools
             session-dir)]

      ;; ── --tool <name> (repeatable) ──
      [(equal? (vector-ref vec i) "--tool")
       (if (< (add1 i) n)
           (loop (+ i 2)
                 command
                 session-id
                 prompt
                 model
                 mode
                 project-dir
                 config-path
                 verbose?
                 max-turns
                 no-tools?
                 (cons (vector-ref vec (add1 i)) tools)
                 session-dir)
           (cli-config 'help #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))]

      ;; ── --tui ──
      [(equal? (vector-ref vec i) "--tui")
       (loop (add1 i)
             command
             session-id
             prompt
             model
             'tui
             project-dir
             config-path
             verbose?
             max-turns
             no-tools?
             tools
             session-dir)]

      ;; ── --json ──
      [(equal? (vector-ref vec i) "--json")
       (loop (add1 i)
             command
             session-id
             prompt
             model
             'json
             project-dir
             config-path
             verbose?
             max-turns
             no-tools?
             tools
             session-dir)]

      ;; ── --rpc ──
      [(equal? (vector-ref vec i) "--rpc")
       (loop (add1 i)
             command
             session-id
             prompt
             model
             'rpc
             project-dir
             config-path
             verbose?
             max-turns
             no-tools?
             tools
             session-dir)]

      ;; ── --session-dir <path> ──
      [(equal? (vector-ref vec i) "--session-dir")
       (if (< (add1 i) n)
           (loop (+ i 2)
                 command
                 session-id
                 prompt
                 model
                 mode
                 project-dir
                 config-path
                 verbose?
                 max-turns
                 no-tools?
                 tools
                 (vector-ref vec (add1 i)))
           (cli-config 'help #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))]

      ;; ── Positional argument (prompt) ──
      [(string-prefix? (vector-ref vec i) "--")
       ;; Unknown flag → help
       (cli-config 'help #f #f #f 'interactive #f #f #f 10 #f '() #f #f '())]

      [else
       ;; First positional — check for named subcommands
       (let ([arg (vector-ref vec i)])
         (cond
           ;; "doctor" subcommand
           [(equal? arg "doctor")
            (loop (add1 i)
                  'doctor
                  session-id
                  prompt
                  model
                  mode
                  project-dir
                  config-path
                  verbose?
                  max-turns
                  no-tools?
                  tools
                  session-dir)]
           ;; "init" subcommand
           [(equal? arg "init")
            (loop (add1 i)
                  'init
                  session-id
                  prompt
                  model
                  mode
                  project-dir
                  config-path
                  verbose?
                  max-turns
                  no-tools?
                  tools
                  session-dir)]
           ;; "sessions" subcommand — q sessions <list|info|delete> [args...]
           [(equal? arg "sessions")
            (if (< (add1 i) n)
                (let ([sub (vector-ref vec (add1 i))])
                  (define sub-sym
                    (cond
                      [(equal? sub "list") 'list]
                      [(equal? sub "info") 'info]
                      [(equal? sub "delete") 'delete]
                      [else #f]))
                  (if sub-sym
                      ;; Collect remaining args after subcommand
                      (let ([rest (for/list ([j (in-range (+ i 2) n)])
                                    (vector-ref vec j))])
                        (cli-config 'sessions
                                    #f
                                    #f
                                    #f
                                    'interactive
                                    #f
                                    #f
                                    #f
                                    10
                                    #f
                                    '()
                                    #f
                                    sub-sym
                                    rest))
                      ;; Bad subcommand → help
                      (cli-config 'help #f #f #f 'interactive #f #f #f 10 #f '() #f #f '())))
                ;; No subcommand → show help
                (cli-config 'help #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))]
           ;; Default: treat as prompt
           [prompt
            ;; Second positional — unexpected, just ignore
            (loop (add1 i)
                  command
                  session-id
                  prompt
                  model
                  mode
                  project-dir
                  config-path
                  verbose?
                  max-turns
                  no-tools?
                  tools
                  session-dir)]
           [else
            (loop (add1 i)
                  'prompt
                  session-id
                  (vector-ref vec i)
                  model
                  mode
                  project-dir
                  config-path
                  verbose?
                  max-turns
                  no-tools?
                  tools
                  session-dir)]))])))

;; ============================================================
;; Pure: cli-config->runtime-config
;; ============================================================

;; Convert cli-config to a hash suitable for make-agent-session / resume-agent-session.
;; Does NOT include 'provider or 'tool-registry or 'event-bus — those are
;; filled in by the runner or the caller.

(define (cli-config->runtime-config cfg)
  (define base
    (make-hash (list (cons 'max-iterations (cli-config-max-turns cfg))
                     (cons 'no-tools? (cli-config-no-tools? cfg))
                     (cons 'tools (cli-config-tools cfg)))))
  ;; Optionally add fields that are set
  (when (cli-config-model cfg)
    (hash-set! base 'model (cli-config-model cfg)))
  (when (cli-config-project-dir cfg)
    (hash-set! base 'project-dir (cli-config-project-dir cfg)))
  (when (cli-config-config-path cfg)
    (hash-set! base 'config-path (cli-config-config-path cfg)))
  (when (cli-config-session-id cfg)
    (hash-set! base 'session-id (cli-config-session-id cfg)))
  (when (cli-config-session-dir cfg)
    (hash-set! base 'session-dir (cli-config-session-dir cfg)))
  base)

;; ============================================================
;; I/O: print-usage
;; ============================================================

(define (print-usage [port (current-output-port)])
  (displayln "Usage: q [options] [prompt]" port)
  (newline port)
  (displayln "Commands:" port)
  (displayln "  q                          Start interactive chat session" port)
  (displayln "  q \"<prompt>\"               Run a single-shot prompt" port)
  (displayln "  q --session <id>           Resume an existing session" port)
  (displayln "  q --tui                    Terminal UI mode (TUI)" port)
  (displayln "  q --json                   JSON mode (machine-readable output)" port)
  (displayln "  q --rpc                    RPC mode (stdin/stdout JSONL protocol)" port)
  (displayln "  q doctor                   Run setup and provider diagnostics" port)
  (displayln "  q init                     Guided setup wizard" port)
  (displayln "  q sessions list            List sessions" port)
  (displayln "  q sessions info <id>       Show session details" port)
  (displayln "  q sessions delete <id>     Delete a session" port)
  (newline port)
  (displayln "Options:" port)
  (displayln "  --model <name>             Model to use (e.g. gpt-4, claude-3)" port)
  (displayln "  --project-dir <path>       Project directory for session storage" port)
  (displayln "  --session-dir <path>       Override session storage directory" port)
  (displayln "  --config <path>            Configuration file path" port)
  (displayln "  --session <id>             Resume session by ID" port)
  (displayln "  --max-turns <n>            Maximum agent loop iterations (default: 10)" port)
  (displayln "  --verbose, -v              Enable verbose output" port)
  (displayln "  --no-tools                 Disable tool use" port)
  (displayln "  --tool <name>              Enable specific tool (repeatable)" port)
  (displayln "  --help, -h                 Show this help message" port)
  (displayln "  --version                  Show version" port)
  (displayln "  doctor                     Run setup and provider diagnostics" port)
  (newline port)
  (displayln "Interactive commands:" port)
  (displayln "  /help                      Show help" port)
  (displayln "  /quit, /exit               Exit session" port)
  (displayln "  /compact                   Trigger compaction now" port)
  (displayln "  /history                   Show session history" port)
  (displayln "  /model [name]              Show or switch model" port)
  (displayln "  /fork [entry-id]           Fork session at given point" port)
  (displayln "  /clear                     Clear transcript (TUI only)" port)
  (displayln "  /interrupt                 Interrupt current turn (TUI only)" port)
  (displayln "  /branches                  List session branches (TUI only)" port)
  (displayln "  /leaves                    List leaf nodes (TUI only)" port)
  (displayln "  /switch <id>               Switch to branch (TUI only)" port)
  (displayln "  /children <id>             Show children of node (TUI only)" port)
  (displayln "  /sessions                  List recent sessions" port)
  (displayln "  /sessions info <id>        Show session details" port)
  (displayln "  /sessions delete <id>      Delete a session" port))

;; ============================================================
;; I/O: print-version
;; ============================================================

;; Single source of truth for q version — also update info.rkt
(define q-version "0.7.6")

(define (print-version [port (current-output-port)])
  (displayln (format "q version ~a" q-version) port))
