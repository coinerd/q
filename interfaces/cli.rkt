#lang racket/base

;; interfaces/cli.rkt — argument parsing and direct terminal interaction
;;
;; This module is an interface layer that:
;;   - Parses CLI arguments into a cli-config struct (pure)
;;   - Converts CLI config to runtime config hash (pure)
;;   - Formats runtime events for terminal display (pure)
;;   - Parses /commands from interactive input (pure)
;;   - Runs interactive or single-shot sessions (I/O, delegates to runtime)
;;
;; Invariants:
;;   - Never owns canonical session state
;;   - Never bypasses runtime APIs
;;   - All pure functions are independently testable

(require "../agent/types.rkt"
         "../util/ansi.rkt"
         "../util/markdown.rkt"
         "../interfaces/sessions.rkt"
         json
         racket/match
         racket/string
         racket/format
         racket/list
         racket/file
         (only-in ffi/unsafe ffi-lib get-ffi-obj _fun _string _int))

;; readline support: try to load, fall back to plain read-line
;; Prefer GNU libreadline over libedit for proper UTF-8 support.
;; libedit has known issues with multi-byte UTF-8 characters (äüö → C<C$C6C).
(void (unless (getenv "PLT_READLINE_LIB")
        (putenv "PLT_READLINE_LIB" "libreadline.so.8")))

(define readline-available?
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (void (dynamic-require 'readline/rktrl #f))
    ;; Disable bracketed paste mode — readline 8.2 enables it by default,
    ;; but the Racket rl_getc_function wrapper can mishandle the ANSI
    ;; escape sequences, causing pasted text to be duplicated in the
    ;; returned line (first few characters get prepended).
    (with-handlers ([exn:fail? void])
      (define librl (ffi-lib "libreadline" '("8.2" "8.1" "8" #f)))
      (define rl-variable-bind (get-ffi-obj "rl_variable_bind" librl (_fun _string _string -> _int)))
      (rl-variable-bind "enable-bracketed-paste" "off"))
    #t))

(define (read-line-with-history prompt in [out (current-output-port)])
  ;; Use readline when available and stdin is a terminal-like port
  ;; (not a pipe or string port, which would cause readline to fail)
  (cond
    [(and readline-available? (eq? in (current-input-port)))
     ;; Only use readline for real stdin — not for test ports
     (define rl-readline (dynamic-require 'readline/rktrl 'readline))
     (define rl-add-history (dynamic-require 'readline/rktrl 'add-history))
     (with-handlers ([exn:fail? (lambda (_)
                                  ;; readline failed (not a TTY) — fall back
                                  (display prompt out)
                                  (flush-output out)
                                  (read-line in))])
       (define line (rl-readline prompt))
       (when (and (string? line) (not (string=? (string-trim line) "")))
         (rl-add-history line))
       line)]
    [else
     ;; Non-stdin port (test mode) — display prompt to specified output
     (display prompt out)
     (flush-output out)
     (read-line in)]))

;; Struct
(provide (struct-out cli-config)

         ;; Pure functions
         parse-cli-args
         cli-config->runtime-config
         format-event-for-terminal
         make-stream-markdown-writer
         render-markdown
         parse-slash-command

         ;; I/O functions
         run-cli-interactive
         run-cli-single
         print-usage
         print-version
         run-init-wizard)

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
;; Pure: format-event-for-terminal
;; ============================================================

;; Convert an event struct to a human-readable string for terminal output.
;; Different formatting for different event types.
;; Returns "" for events that don't need terminal display.

;; Convert tool-result content (list of content-parts, strings, or hashes) to a
;; display string. Handles both the structured content-part format and plain strings.
(define (tool-result-content->string content)
  (cond
    [(string? content) content]
    [(list? content)
     (string-join (for/list ([part (in-list content)])
                    (cond
                      [(string? part) part]
                      [(hash? part) (hash-ref part 'text (format "~a" part))]
                      [else (format "~a" part)]))
                  "\n")]
    [(hash? content) (hash-ref content 'text (format "~a" content))]
    [else (format "~a" content)]))

;; ============================================================
;; Markdown → Terminal renderer
;; ============================================================

;; Renders basic Markdown to ANSI-styled terminal text.
;; Delegates to the shared token-based parser in q/util/markdown.rkt,
;; then converts each token to an ANSI string using constants from q/util/ansi.rkt.

(define (render-markdown text)
  (if (not (color-enabled?))
      text
      (render-tokens (parse-markdown text))))

(define (render-tokens tokens)
  (string-append* (map render-token tokens)))

(define (render-token tok)
  (case (md-token-type tok)
    [(text) (md-token-content tok)]
    [(bold) (string-append ANSI-BOLD (md-token-content tok) ANSI-RESET)]
    [(italic) (string-append ANSI-ITALIC (md-token-content tok) ANSI-RESET)]
    [(code) (string-append ANSI-CYAN (md-token-content tok) ANSI-RESET)]
    [(header)
     (define content (md-token-content tok))
     (string-append ANSI-BOLD ANSI-YELLOW (cdr content) ANSI-RESET)]
    [(code-block)
     (define content (md-token-content tok))
     (define code (cdr content))
     (string-append ANSI-GREEN
                    (string-join (for/list ([line (string-split code "\n" #:trim? #f)])
                                   (string-append "  " line))
                                 (string-append "\n" ANSI-GREEN))
                    ANSI-RESET)]
    [(link)
     (define content (md-token-content tok))
     (string-append ANSI-BLUE ANSI-UNDERLINE (cdr content) ANSI-RESET)]
    [(newline) "\n"]
    [else (format "~a" (md-token-content tok))]))

(define MAX-TOOL-DISPLAY-LEN 300)

;; ============================================================
;; Stream Markdown Writer — line-buffered Markdown rendering
;; ============================================================

;; Creates a stateful stream writer that buffers incoming text
;; fragments and renders complete lines through render-markdown.
;;
;; Strategy:
;;   1. Buffer incoming delta text
;;   2. When buffer contains complete lines (ending in \n),
;;      extract and render them through render-markdown
;;   3. Partial lines (no trailing \n) are printed raw for
;;      immediate feedback, tracked for later replacement
;;   4. When a partial line later completes, erase the raw
;;      output with \033[2K\r and re-render the full line
;;   5. On flush, render any remaining partial line
;;
;; Returns a procedure: (writer text [port])
;; The writer writes rendered output to the given port.
;; Also returns a second value: the flush procedure.

(define (make-stream-markdown-writer)
  (define line-buffer (box "")) ;; accumulated text for current line

  ;; The writer procedure — buffers deltas, renders complete lines
  (define (writer text [port (current-output-port)])
    (when (and (string? text) (> (string-length text) 0))
      ;; Append to line buffer
      (set-box! line-buffer (string-append (unbox line-buffer) text))
      ;; Process any complete lines
      (define buf (unbox line-buffer))
      (when (string-contains? buf "\n")
        (define lines (string-split buf "\n" #:trim? #f))
        (define complete-lines (drop-right lines 1))
        (define remaining (last lines))
        ;; Render and print each complete line
        (for ([line (in-list complete-lines)])
          (display (render-markdown line) port)
          (newline port))
        (set-box! line-buffer remaining)
        (flush-output port))))

  ;; Flush procedure — call at stream end
  (define (flush! [port (current-output-port)])
    (define remaining (unbox line-buffer))
    (when (> (string-length remaining) 0)
      ;; Render and print the final partial line
      (display (render-markdown remaining) port)
      (newline port)
      (set-box! line-buffer "")
      (flush-output port)))

  (values writer flush!))

(define (truncate-string s max-len)
  (if (> (string-length s) max-len)
      (string-append (substring s 0 max-len) "...")
      s))

(define (format-event-for-terminal evt)
  (define ev (event-ev evt))
  (define payload (event-payload evt))
  (case ev
    [("model.stream.delta") (hash-ref payload 'delta "")]
    ;; Emit trailing newline after streaming — puts cursor on fresh line
    [("model.stream.completed") "\n"]
    ;; Suppress — content already displayed via model.stream.delta events.
    ;; Return empty string so the subscriber prints nothing.
    [("assistant.message.completed") ""]
    [("tool.call.started")
     (define name (hash-ref payload 'name "?"))
     (define args-raw (hash-ref payload 'arguments #f))
     (define args
       (cond
         [(hash? args-raw) args-raw]
         [(string? args-raw)
          (with-handlers ([exn:fail? (lambda (_) #f)])
            (string->jsexpr args-raw))]
         [else #f]))
     (define detail
       (cond
         [(and args (hash? args))
          (define cmd
            (or (hash-ref args 'command #f) (hash-ref args 'path #f) (hash-ref args 'pattern #f) #f))
          (if cmd
              (truncate-string (format "~a" cmd) 100)
              #f)]
         [else #f]))
     (if detail
         (styled (format "[tool: ~a: ~a]" name detail) '(bold yellow))
         (styled (format "[tool: ~a]" name) '(bold yellow)))]
    [("tool.call.completed")
     (define name (hash-ref payload 'name "?"))
     (define result (hash-ref payload 'result #f))
     (define content-str
       (if result
           (truncate-string (tool-result-content->string result) MAX-TOOL-DISPLAY-LEN)
           name))
     (styled (format "[tool result: ~a]" content-str) '(dim))]
    [("tool.call.failed")
     (styled (format "[tool failed: ~a \u2014 ~a]"
                     (hash-ref payload 'name "?")
                     (hash-ref payload 'error "unknown"))
             '(red))]
    [("turn.started") ""]
    [("turn.completed") ""]
    [("runtime.error") (styled (format "Error: ~a" (hash-ref payload 'error "unknown error")) '(red))]
    [("session.started")
     (styled (format "[session started: ~a]" (hash-ref payload 'sessionId "")) '(dim))]
    [("session.resumed")
     (styled (format "[session resumed: ~a]" (hash-ref payload 'sessionId "")) '(dim))]
    [("compaction.warning")
     (styled (format "[compaction warning: ~a tokens]" (hash-ref payload 'tokenCount "?")) '(yellow))]
    [("session.forked")
     (styled (format "[session forked: ~a]" (hash-ref payload 'newSessionId "")) '(dim))]
    [else ""]))

;; ============================================================
;; Pure: parse-slash-command
;; ============================================================

;; Parse a /command from interactive input.
;; Returns a list like '(help), '(quit), '(compact), '(history),
;; '(fork), '(fork "entry-id"), or #f if not a /command.

(define (parse-slash-command line)
  (define trimmed (string-trim line))
  (cond
    [(string=? trimmed "") #f]
    [(not (string-prefix? trimmed "/")) #f]
    [else
     (define parts (string-split trimmed))
     (define cmd (car parts))
     (define args (cdr parts))
     (case cmd
       [("/help") '(help)]
       [("/quit" "/exit") '(quit)]
       [("/compact") '(compact)]
       [("/history") '(history)]
       [("/model")
        (if (null? args)
            '(model)
            (list 'model (car args)))]
       [("/fork")
        (if (null? args)
            '(fork)
            (list 'fork (car args)))]
       [("/clear") '(clear)]
       [("/interrupt") '(interrupt)]
       [("/branches") '(branches)]
       [("/leaves") '(leaves)]
       [("/switch")
        (if (null? args)
            '(switch)
            (list 'switch (car args)))]
       [("/children")
        (if (null? args)
            '(children)
            (list 'children (car args)))]
       [("/sessions")
        (cond
          [(null? args) '(sessions)]
          [(equal? (car args) "list") '(sessions list)]
          [(equal? (car args) "info")
           (if (>= (length args) 2)
               (list 'sessions 'info (cadr args))
               '(sessions info))]
          [(equal? (car args) "delete")
           (if (>= (length args) 2)
               (list 'sessions 'delete (cadr args))
               '(sessions delete))]
          [else '(sessions)])]
       [else #f])]))

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
(define q-version "0.7.3")

(define (print-version [port (current-output-port)])
  (displayln (format "q version ~a" q-version) port))

;; ============================================================
;; I/O: run-cli-interactive
;; ============================================================

;; Run an interactive CLI session. Reads lines, dispatches /commands,
;; submits prompts to the runtime via the provided session-runner function.
;;
;; session-fn: (string -> void) — called with each user prompt
;;   The actual implementation passes run-prompt! or similar.
;;
;; For testability, this accepts optional input/output ports.

(define (run-cli-interactive cfg
                             #:session-fn [session-fn (lambda (prompt) (void))]
                             #:compact-fn [compact-fn #f]
                             #:history-fn [history-fn #f]
                             #:fork-fn [fork-fn #f]
                             #:model-fn [model-fn #f]
                             #:sessions-fn [sessions-fn #f]
                             #:provider-name [provider-name #f]
                             #:in [in (current-input-port)]
                             #:out [out (current-output-port)])
  ;; First-run welcome detection
  (define q-config-dir (build-path (find-system-path 'home-dir) ".q"))
  (define first-run?
    (not (or (directory-exists? q-config-dir)
             (file-exists? (build-path q-config-dir "config.json")))))

  (when first-run?
    (displayln "" out)
    (displayln "Welcome to q! Your AI coding assistant." out)
    (displayln "Type a message to start chatting, or /help for commands." out)
    (displayln "Run 'q config' to set up your API key." out)
    (displayln "" out))

  ;; Mock provider warning (Issue #141)
  (when (and provider-name (string? provider-name) (string-contains? provider-name "mock"))
    (displayln "" out)
    (displayln "\u26A0 [SYS] No LLM provider configured. Using mock provider (canned responses)." out)
    (displayln "  Set OPENAI_API_KEY, ANTHROPIC_API_KEY, or GEMINI_API_KEY environment variable," out)
    (displayln "  or run 'q init' to set up your configuration." out)
    (displayln "" out))

  ;; Don't print prompt here — read-line-with-history handles it
  (let loop ()
    (define line (read-line-with-history (styled-prompt "q> ") in out))
    (cond
      [(eof-object? line) (displayln "Goodbye." out)]
      [else
       (define trimmed (string-trim line))
       (cond
         [(string=? trimmed "") (void)] ; empty line, just re-prompt
         [(parse-slash-command trimmed)
          =>
          (lambda (cmd)
            (match (car cmd)
              ['help (print-usage out)]
              ['quit
               (displayln "Goodbye." out)
               (void)]
              ['compact
               (if compact-fn
                   (compact-fn)
                   (displayln "[compacting...]" out))]
              ['history
               (if history-fn
                   (history-fn out)
                   (displayln "[history not yet connected]" out))]
              ['model
               (if model-fn
                   (model-fn (and (>= (length cmd) 2) (cadr cmd)))
                   (displayln "[model command not yet connected]" out))]
              ['fork
               (if fork-fn
                   (fork-fn (and (>= (length cmd) 2) (cadr cmd)))
                   (displayln "[fork not yet connected]" out))]
              ['clear (displayln "[clear: available in TUI mode (--tui)]" out)]
              ['interrupt (displayln "[interrupt: available in TUI mode (--tui)]" out)]
              ['branches (displayln "[branches: available in TUI mode (--tui)]" out)]
              ['leaves (displayln "[leaves: available in TUI mode (--tui)]" out)]
              ['switch (displayln "[switch: available in TUI mode (--tui)]" out)]
              ['children (displayln "[children: available in TUI mode (--tui)]" out)]
              ['sessions
               (if sessions-fn
                   (sessions-fn cmd out)
                   (displayln "[sessions command not yet connected]" out))]
              [_ (displayln (format "Unknown command: ~a" cmd) out)]))]
         [else
          ;; Submit prompt to runtime
          (with-handlers ([exn:fail? (lambda (e)
                                       (displayln (format "Error: ~a" (exn-message e)) out))])
            (session-fn trimmed))])
       ;; Continue loop (unless quit was issued)
       (when (and (string? line) (not (member (string-trim line) '("/quit" "/exit"))))
         (loop))])))

;; ============================================================
;; I/O: run-init-wizard (Issue #143)
;; ============================================================

;; Guided setup wizard that creates ~/.q/config.json.
;; For testability, accepts optional input/output ports.
(define (run-init-wizard #:in [in (current-input-port)] #:out [out (current-output-port)])
  (define config-dir (build-path (find-system-path 'home-dir) ".q"))
  (define config-path (build-path config-dir "config.json"))

  ;; Helper: read a line, handling EOF
  (define (read-input)
    (define line (read-line in))
    (if (eof-object? line)
        ""
        (string-trim line)))

  ;; Use aborted? flag for early exit
  (define aborted? (box #f))

  ;; Check if config already exists
  (when (file-exists? config-path)
    (display "Config already exists at ~/.q/config.json. Overwrite? (y/N): " out)
    (flush-output out)
    (define answer (read-input))
    (unless (or (string=? answer "y") (string=? answer "Y"))
      (displayln "Aborted." out)
      (set-box! aborted? #t)))

  (unless (unbox aborted?)
    ;; Ask for provider
    (display "Choose provider (openai/anthropic/gemini): " out)
    (flush-output out)
    (define provider (read-input))
    (cond
      [(not (member provider '("openai" "anthropic" "gemini")))
       (displayln "Invalid provider. Choose openai, anthropic, or gemini." out)
       (set-box! aborted? #t)]
      [else
       ;; Ask for API key
       (display "API key (will be visible): " out)
       (flush-output out)
       (define api-key (read-input))

       ;; Ask for default model
       (display "Default model (press Enter for default): " out)
       (flush-output out)
       (define model (read-input))

       ;; Build config hash
       (define provider-hash (make-hash (list (cons 'api-key (if (string=? api-key "") "" api-key)))))
       (when (and model (not (string=? model "")))
         (hash-set! provider-hash 'default-model model))

       (define config
         (make-hash (list (cons 'default-provider provider)
                          (cons 'providers
                                (make-hash (list (cons (string->symbol provider) provider-hash)))))))

       ;; Write config
       (make-directory* config-dir)
       (call-with-output-file config-path (lambda (port) (write-json config port)) #:exists 'replace)

       (displayln "Configuration saved to ~/.q/config.json." out)
       (displayln "Run 'q' to start chatting." out)])))

;; ============================================================
;; I/O: run-cli-single
;; ============================================================

;; Run a single-shot prompt and exit.

(define (run-cli-single cfg
                        #:session-fn [session-fn (lambda (prompt) (void))]
                        #:out [out (current-output-port)])
  (define prompt (cli-config-prompt cfg))
  (when prompt
    (with-handlers ([exn:fail? (lambda (e) (displayln (format "Error: ~a" (exn-message e)) out))])
      (let-values ([(_sess _result) (session-fn prompt)])
        (void)))))
