#lang racket/base

;; q/tui/commands/general.rkt — General TUI commands (help, clear, status)
;;
;; Extracted from commands.rkt (W19) to thin the commands parent.

(require racket/hash
         racket/list
         racket/string
         "../state.rkt"
         "../palette.rkt"
         "context.rkt")

;; Handle /help — show command list from palette registry
(define (handle-help-command cctx state)
  (define reg (make-command-registry))
  (define cmds (all-commands reg))
  (define help-entries
    (cons
     (make-entry 'system "Commands:" 0 (hash))
     (for/list ([c (in-list cmds)])
       (define args-str
         (if (null? (cmd-entry-args-spec c))
             ""
             (string-append " " (string-join (cmd-entry-args-spec c) " "))))
       (define aliases-str
         (if (null? (cmd-entry-aliases c))
             ""
             (format " (~a)" (string-join (cmd-entry-aliases c) ", "))))
       (make-entry
        'system
        (format "  ~a~a~a  ~a" (cmd-entry-name c) args-str aliases-str (cmd-entry-summary c))
        0
        (hash)))))
  (for/fold ([s state]) ([e (in-list help-entries)])
    (add-transcript-entry s e)))

;; Handle /clear — clear transcript
(define (handle-clear-command cctx state)
  (struct-copy ui-state state [transcript '()]))

;; Handle /status — show session/provider status
(define (handle-status-command cctx state)
  (define sid (ui-state-session-id state))
  (define model-name (ui-state-model-name state))
  (define busy (ui-state-busy? state))
  (define status-msg (ui-state-status-message state))
  (define sess-dir (cmd-ctx-session-dir cctx))
  (define lines
    (list (format "Session: ~a" (or sid "none"))
          (format "Model: ~a" (or model-name "none"))
          (format "Prompt running: ~a" (if busy "yes" "no"))
          (format "Last status: ~a" (or status-msg "none"))
          (format "Session dir: ~a" (or sess-dir "not set"))))
  (for/fold ([s state]) ([line (in-list lines)])
    (add-transcript-entry s (make-entry 'system (format "[STATUS] ~a" line) 0 (hash)))))

(provide handle-help-command
         handle-clear-command
         handle-status-command)
