#lang racket/base

;; q/cli/args/flags.rkt — Flag definition struct and the full FLAG-DEFINITIONS table
;;
;; Sub-module of cli/args.rkt (split for modularity).
;; Provides flag-def struct, FLAG-DEFINITIONS, acc helpers, lookup tables.

(require racket/match)

(provide flag-def
         flag-def?
         flag-def-name
         flag-def-short
         flag-def-long
         flag-def-type
         flag-def-help
         flag-def-default
         flag-def-apply-fn
         FLAG-DEFINITIONS
         long-flag-table
         short-flag-table
         acc-ref
         acc-set
         acc-cons)

;; ============================================================
;; Flag definition struct (QUAL-12)
;; ============================================================

;; A flag definition describes one CLI option.
;;   name      -- internal identifier (string)
;;   short     -- short option character or #f
;;   long      -- long option string (e.g. "--help")
;;   type      -- 'boolean, 'string, 'integer, 'mode, 'accumulate, 'command
;;               'boolean   : no argument, sets a field to #t
;;               'string    : consumes next arg as a string value
;;               'integer   : consumes next arg, must be exact-positive-integer
;;               'mode      : no argument, sets mode field to a symbol
;;               'accumulate: consumes next arg, accumulates into a list
;;               'command   : no argument, sets command/produces immediate result
;;   help      -- help text for usage display
;;   default   -- default value (informational, used in usage generation)
;;   apply-fn  -- procedure: (val acc-alist) -> acc-alist
;;               Returns a new accumulator alist with the flag applied.
;;               val is #t for booleans, the string/int value for others.

(struct flag-def (name short long type help default apply-fn) #:transparent)

;; -- Accessors for the accumulator alist (mutable parameter would also work,
;;    but an alist is purely functional and easy to test).

(define (acc-ref acc key [default #f])
  (match acc
    ['() default]
    [(cons (cons (== key) val) _) val]
    [(cons _ rest) (acc-ref rest key default)]))

(define (acc-set acc key val)
  (cons (cons key val) (filter (lambda (p) (not (eq? (car p) key))) acc)))

(define (acc-cons acc key val)
  ;; Prepend val to the list at key
  (let ([existing (acc-ref acc key '())]) (acc-set acc key (cons val existing))))

;; ============================================================
;; Flag table — all option definitions
;; ============================================================

(define FLAG-DEFINITIONS
  ;; --help / -h -> immediate help
  (list (flag-def "help"
                  #\h
                  "help"
                  'boolean
                  "Show this help message"
                  #f
                  (lambda (val acc)
                    ;; Returning 'help immediately -- handled specially in the parser
                    'help))
        ;; --version -> immediate version
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
        ;; --gui
        (flag-def "gui"
                  #f
                  "gui"
                  'mode
                  "GUI mode (native graphical interface)"
                  #f
                  (lambda (val acc) (acc-set acc 'mode 'gui)))
        ;; --keybindings <path>
        (flag-def "keybindings"
                  #f
                  "keybindings"
                  'string
                  "Path to custom keybindings JSON file"
                  #f
                  (lambda (val acc) (acc-set acc 'keybindings-path val)))
        ;; -p / --print -- plain-text stdout output (G9.3)
        (flag-def "print"
                  #\p
                  "print"
                  'boolean
                  "Print mode: run once, output plain text to stdout"
                  #f
                  (lambda (val acc) (acc-set (acc-set acc 'print-mode? #t) 'mode 'print)))
        ;; --memory <backend>
        (flag-def "memory"
                  #\m
                  "memory"
                  'string
                  "Memory backend: hash|file-jsonl|disabled (default: disabled)"
                  #f
                  (lambda (val acc)
                    (define sym (string->symbol val))
                    (if (memq sym '(hash file-jsonl disabled))
                        (acc-set acc 'memory sym)
                        (acc-set acc 'memory #f))))
        ;; --context-profile <profile>
        (flag-def "context-profile"
                  #f
                  "context-profile"
                  'string
                  "Context assembly profile: off|observe|bounded|self-healing|full"
                  #f
                  (lambda (val acc)
                    (define sym (string->symbol val))
                    (if (memq sym '(off observe bounded self-healing full))
                        (acc-set acc 'context-profile sym)
                        (acc-set acc 'context-profile 'off))))
        ;; --agent-pool <n> (v0.99.23 §5.1)
        (flag-def "agent-pool"
                  #f
                  "agent-pool"
                  'integer
                  "Max concurrent subagents (default: 3)"
                  3
                  (lambda (val acc) (acc-set acc 'agent-pool val)))
        ;; --parallel (v0.99.23 §5.1)
        (flag-def "parallel"
                  #f
                  "parallel"
                  'boolean
                  "Enable parallel execution mode: auto-partition task into subtasks"
                  #f
                  (lambda (val acc) (acc-set acc 'parallel? #t)))))

;; Build lookup tables for fast matching
(define long-flag-table
  (for/hash ([fd (in-list FLAG-DEFINITIONS)])
    (values (string-append "--" (flag-def-long fd)) fd)))

(define short-flag-table
  (for/hash ([fd (in-list FLAG-DEFINITIONS)]
             #:when (flag-def-short fd))
    (values (string-append "-" (string (flag-def-short fd))) fd)))
