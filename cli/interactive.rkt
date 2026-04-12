#lang racket/base

;; q/cli/interactive.rkt — Interactive REPL loop, single-shot execution,
;;                          readline support, slash command parsing
;;
;; Extracted from interfaces/cli.rkt for modularity (Issue #193).
;;
;; Provides:
;;   readline-available?     — boolean: was readline loaded?
;;   read-line-with-history  — readline wrapper with history
;;   run-cli-interactive     — interactive REPL loop
;;   run-cli-single          — single-shot prompt execution
;;   parse-slash-command     — /command parsing

(require racket/match
         racket/string
         racket/format
         racket/list
         "../util/readline.rkt"
         "../cli/args.rkt"
         "../cli/render.rkt"
         "../util/ansi.rkt")

(provide readline-available?
         read-line-with-history
         run-cli-interactive
         run-cli-single
         parse-slash-command)

;; ============================================================
;; Readline support — imported from util/readline.rkt (Issue #203)
;; ============================================================
;; readline-available? and read-line-with-history are now provided
;; by util/readline.rkt and imported above.

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
          (with-handlers ([exn:fail?
                           (lambda (e)
                             (displayln (format-classified-error e (cli-config-verbose? cfg)) out))])
            (session-fn trimmed))])
       ;; Continue loop (unless quit was issued)
       (when (and (string? line) (not (member (string-trim line) '("/quit" "/exit"))))
         (loop))])))

;; ============================================================
;; I/O: run-cli-single
;; ============================================================

;; Run a single-shot prompt and exit.

(define (run-cli-single cfg
                        #:session-fn [session-fn (lambda (prompt) (void))]
                        #:out [out (current-output-port)])
  (define prompt (cli-config-prompt cfg))
  (when prompt
    (with-handlers ([exn:fail? (lambda (e)
                                 (displayln (format-classified-error e (cli-config-verbose? cfg))
                                            (current-error-port)))])
      (call-with-values (lambda () (session-fn prompt))
                        (case-lambda
                          [(sess result) (void)]
                          [(v) (void)]
                          [() (void)])))))
