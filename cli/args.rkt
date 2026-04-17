#lang racket/base

;; q/cli/args.rkt — CLI argument parsing, config conversion, usage/version display
;;
;; Extracted from interfaces/cli.rkt for modularity (Issue #193).
;; Refactored: cond branches → data-driven flag table (QUAL-12, Issue #302).
;;
;; Provides:
;;   cli-config struct + accessors
;;   parse-cli-args      — pure argument parsing
;;   cli-config->runtime-config — config hash conversion
;;   print-usage         — usage display (generated from flag table)
;;   print-version       — version display
;;   q-version           — version constant

(require racket/string
         racket/format
         "../util/version.rkt")

(provide (struct-out cli-config)
         parse-cli-args
         cli-config->runtime-config
         print-usage
         print-version
         ;; q-version imported from util/version.rkt (Issue #203)
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
         keybindings-path ; path-string or #f — custom keybindings file (#1118)
         )
  #:transparent)

;; Helper: construct a "help" config (used for parse errors and --help)
(define (make-help-config)
  (cli-config 'help #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f))

;; ============================================================
;; Flag definition table (QUAL-12)
;; ============================================================

;; A flag definition describes one CLI option.
;;   name      — internal identifier (string)
;;   short     — short option character or #f
;;   long      — long option string (e.g. "--help")
;;   type      — 'boolean, 'string, 'integer, 'mode, 'accumulate, 'command
;;               'boolean   : no argument, sets a field to #t
;;               'string    : consumes next arg as a string value
;;               'integer   : consumes next arg, must be exact-positive-integer
;;               'mode      : no argument, sets mode field to a symbol
;;               'accumulate: consumes next arg, accumulates into a list
;;               'command   : no argument, sets command/produces immediate result
;;   help      — help text for usage display
;;   default   — default value (informational, used in usage generation)
;;   apply-fn  — procedure: (val acc-alist) → acc-alist
;;               Returns a new accumulator alist with the flag applied.
;;               val is #t for booleans, the string/int value for others.

(struct flag-def (name short long type help default apply-fn) #:transparent)

;; -- Accessors for the accumulator alist (mutable parameter would also work,
;;    but an alist is purely functional and easy to test).

(define (acc-ref acc key [default #f])
  (cond
    [(null? acc) default]
    [(eq? (caar acc) key) (cdar acc)]
    [else (acc-ref (cdr acc) key default)]))

(define (acc-set acc key val)
  (cons (cons key val) (filter (lambda (p) (not (eq? (car p) key))) acc)))

(define (acc-cons acc key val)
  ;; Prepend val to the list at key
  (let ([existing (acc-ref acc key '())]) (acc-set acc key (cons val existing))))

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
                     (keybindings-path . #f)))

;; Construct a cli-config from an accumulator alist.
(define (acc->cli-config acc)
  (define final-command
    (let ([cmd (acc-ref acc 'command)]
          [sid (acc-ref acc 'session-id)]
          [prompt (acc-ref acc 'prompt)])
      (cond
        [(eq? cmd 'help) 'help]
        [(eq? cmd 'version) 'version]
        [(eq? cmd 'init) 'init]
        [(eq? cmd 'sessions) 'sessions]
        [(and sid (eq? cmd 'chat)) 'resume]
        [prompt 'prompt]
        [else cmd])))
  (define final-mode
    (let ([m (acc-ref acc 'mode)])
      (cond
        [(eq? m 'json) 'json]
        [(eq? m 'rpc) 'rpc]
        [(eq? m 'tui) 'tui]
        [(eq? final-command 'prompt) 'single]
        [else 'interactive])))
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
              (acc-ref acc 'keybindings-path)))

;; ============================================================
;; Flag table — all option definitions
;; ============================================================

(define FLAG-DEFINITIONS
  ;; --help / -h → immediate help
  (list (flag-def "help"
                  #\h
                  "help"
                  'boolean
                  "Show this help message"
                  #f
                  (lambda (val acc)
                    ;; Returning 'help immediately — handled specially in the parser
                    'help))
        ;; --version → immediate version
        (flag-def "version" #f "version" 'boolean "Show version" #f (lambda (val acc) 'version))
        ;; --session <id>
        (flag-def "session"
                  #f
                  "session"
                  'string
                  "Resume session by ID"
                  #f
                  (lambda (val acc) (acc-set (acc-set acc 'session-id val) 'command 'resume)))
        ;; --model <name>
        (flag-def "model"
                  #f
                  "model"
                  'string
                  "Model to use (e.g. gpt-4, claude-3)"
                  #f
                  (lambda (val acc) (acc-set acc 'model val)))
        ;; --project-dir <path>
        (flag-def "project-dir"
                  #f
                  "project-dir"
                  'string
                  "Project directory for session storage"
                  #f
                  (lambda (val acc) (acc-set acc 'project-dir val)))
        ;; --config <path>
        (flag-def "config"
                  #f
                  "config"
                  'string
                  "Configuration file path"
                  #f
                  (lambda (val acc) (acc-set acc 'config-path val)))
        ;; --verbose / -v
        (flag-def "verbose"
                  #\v
                  "verbose"
                  'boolean
                  "Enable verbose output"
                  #f
                  (lambda (val acc) (acc-set acc 'verbose? #t)))
        ;; --max-turns <n>
        (flag-def "max-turns"
                  #f
                  "max-turns"
                  'integer
                  "Maximum agent loop iterations (default: 10)"
                  10
                  (lambda (val acc) (acc-set acc 'max-turns val)))
        ;; --no-tools
        (flag-def "no-tools"
                  #f
                  "no-tools"
                  'boolean
                  "Disable tool use"
                  #f
                  (lambda (val acc) (acc-set acc 'no-tools? #t)))
        ;; --tool <name> (repeatable)
        (flag-def "tool"
                  #f
                  "tool"
                  'accumulate
                  "Enable specific tool (repeatable)"
                  '()
                  (lambda (val acc) (acc-cons acc 'tools val)))
        ;; --session-dir <path>
        (flag-def "session-dir"
                  #f
                  "session-dir"
                  'string
                  "Override session storage directory"
                  #f
                  (lambda (val acc) (acc-set acc 'session-dir val)))
        ;; --tui
        (flag-def "tui"
                  #f
                  "tui"
                  'mode
                  "Terminal UI mode (TUI)"
                  #f
                  (lambda (val acc) (acc-set acc 'mode 'tui)))
        ;; --json
        (flag-def "json"
                  #f
                  "json"
                  'mode
                  "JSON mode (machine-readable output)"
                  #f
                  (lambda (val acc) (acc-set acc 'mode 'json)))
        ;; --rpc
        (flag-def "rpc"
                  #f
                  "rpc"
                  'mode
                  "RPC mode (stdin/stdout JSONL protocol)"
                  #f
                  (lambda (val acc) (acc-set acc 'mode 'rpc)))
        ;; --keybindings <path>
        (flag-def "keybindings"
                  #f
                  "keybindings"
                  'string
                  "Path to custom keybindings JSON file"
                  #f
                  (lambda (val acc) (acc-set acc 'keybindings-path val)))))

;; Build lookup tables for fast matching
(define long-flag-table
  (for/hash ([fd (in-list FLAG-DEFINITIONS)])
    (values (string-append "--" (flag-def-long fd)) fd)))

(define short-flag-table
  (for/hash ([fd (in-list FLAG-DEFINITIONS)]
             #:when (flag-def-short fd))
    (values (string-append "-" (string (flag-def-short fd))) fd)))

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
             [acc (make-initial-acc)])
    (cond
      ;; ── Done ──
      [(>= i n)
       ;; Handle sessions subcommand finalization
       (define cmd (acc-ref acc 'command))
       (cond
         ;; sessions was set — subcommand was already consumed inline
         [(eq? cmd 'sessions) acc]
         [else (acc->cli-config acc)])]

      ;; ── Known flag? ──
      [(hash-ref long-flag-table (vector-ref vec i) #f)
       =>
       (lambda (fd) (apply-flag fd vec n i acc loop))]

      [(hash-ref short-flag-table (vector-ref vec i) #f)
       =>
       (lambda (fd) (apply-flag fd vec n i acc loop))]

      ;; ── Positional argument (prompt) or subcommand ──
      ;; Unknown flag → help
      [(string-prefix? (vector-ref vec i) "--") (make-help-config)]

      [else (handle-positional vec n i acc loop)])))

;; Apply a flag-def: consume the arg at position i, possibly the next arg too,
;; call the apply-fn, and continue the loop.
(define (apply-flag fd vec n i acc loop)
  (define t (flag-def-type fd))
  (cond
    [(eq? t 'boolean)
     (let ([result ((flag-def-apply-fn fd) #t acc)])
       (cond
         [(eq? result 'help) (make-help-config)]
         [(eq? result 'version)
          (cli-config 'version #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f)]
         [else (loop (add1 i) result)]))]
    [(or (eq? t 'string) (eq? t 'integer) (eq? t 'accumulate))
     (if (< (add1 i) n)
         (let ([raw-val (vector-ref vec (add1 i))])
           (cond
             [(eq? t 'integer)
              (define num (string->number raw-val))
              (if (and num (exact-positive-integer? num))
                  (loop (+ i 2) ((flag-def-apply-fn fd) num acc))
                  (make-help-config))]
             [else (loop (+ i 2) ((flag-def-apply-fn fd) raw-val acc))]))
         (make-help-config))]
    [(eq? t 'mode) (loop (add1 i) ((flag-def-apply-fn fd) #t acc))]
    ;; Shouldn't happen
    [else (make-help-config)]))

;; Handle positional arguments and subcommands.
(define (handle-positional vec n i acc loop)
  (define arg (vector-ref vec i))
  (cond
    ;; "doctor" subcommand
    [(equal? arg "doctor") (loop (add1 i) (acc-set acc 'command 'doctor))]
    ;; "init" subcommand
    [(equal? arg "init") (loop (add1 i) (acc-set acc 'command 'init))]
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
               (let ([rest (for/list ([j (in-range (+ i 2) n)])
                             (vector-ref vec j))])
                 (cli-config 'sessions #f #f #f 'interactive #f #f #f 10 #f '() #f sub-sym rest #f))
               (make-help-config)))
         (make-help-config))]
    ;; Default: treat as prompt
    ;; Second positional — ignore
    [(acc-ref acc 'prompt #f) (loop (add1 i) acc)]
    [else (loop (add1 i) (acc-set (acc-set acc 'prompt arg) 'command 'prompt))]))

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
;; I/O: print-usage (generated from flag table)
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
  ;; Generate option lines from FLAG-DEFINITIONS
  (for ([fd (in-list FLAG-DEFINITIONS)])
    (define short-part
      (if (flag-def-short fd)
          (format "-~a, " (flag-def-short fd))
          "    "))
    (define long-part (format "--~a" (flag-def-long fd)))
    (define type-placeholder
      (cond
        [(eq? (flag-def-type fd) 'string) " <value>"]
        [(eq? (flag-def-type fd) 'integer) " <n>"]
        [(eq? (flag-def-type fd) 'accumulate) " <name>"]
        [else ""]))
    (define help-text (flag-def-help fd))
    (define line
      (format "  ~a~a~a~a"
              short-part
              long-part
              type-placeholder
              ;; Pad to column 30
              (make-string (max 1
                                (- 26
                                   (string-length short-part)
                                   (string-length long-part)
                                   (string-length type-placeholder)))
                           #\space)))
    (displayln (string-append line help-text) port))
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

;; q-version imported from util/version.rkt (Issue #203)
;; Single source of truth — do not redefine here.

(define (print-version [port (current-output-port)])
  (displayln (format "q version ~a" q-version) port))
