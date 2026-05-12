#lang racket/base

;; q/tui/commands.rkt — Slash command handlers for the TUI
;;
;; ARCH-06: Refactored into sub-modules:
;;   tui/commands/context.rkt   — cmd-ctx struct
;;   tui/commands/branch.rkt    — /branches, /leaves, /switch, /children, /tree
;;   tui/commands/session.rkt   — /history, /fork, /name, /sessions
;;   tui/commands/model.rkt     — /model
;;   tui/commands/extension.rkt — /activate, /deactivate, /reload
;;
;; This module re-exports everything and contains the main dispatcher.

(require racket/base
         racket/match
         racket/string
         racket/list
         "state.rkt"
         (only-in "command-parse.rkt"
                  parsed-command?
                  parsed-command-canonical-name
                  parsed-command-args
                  parsed-command-arg-kind)
         "palette.rkt"
         "../util/protocol-types.rkt"
         "../agent/event-bus.rkt"
         "../extensions/hooks.rkt"
         "../extensions/api.rkt"
         ;; Sub-module imports
         (only-in "commands/context.rkt"
                  cmd-ctx
                  cmd-ctx?
                  cmd-ctx-state-box
                  cmd-ctx-running-box
                  cmd-ctx-event-bus
                  cmd-ctx-session-dir
                  cmd-ctx-needs-redraw-box
                  cmd-ctx-model-registry-box
                  cmd-ctx-last-prompt-box
                  cmd-ctx-session-runner
                  cmd-ctx-input-text-box
                  cmd-ctx-extension-registry-box
                  cmd-ctx-session-factory-runner)
         (only-in "commands/branch.rkt"
                  handle-branches-command
                  handle-leaves-command
                  handle-switch-command
                  handle-children-command
                  handle-tree-command)
         (only-in "commands/session.rkt"
                  handle-history-command
                  handle-fork-command
                  handle-name-command
                  handle-sessions-tui-command)
         (only-in "commands/model.rkt" handle-model-command)
         (only-in "commands/extension.rkt"
                  handle-activate-command
                  handle-reload-command
                  handle-deactivate-command))

;; Re-export all public APIs
(provide cmd-ctx
         cmd-ctx?
         cmd-ctx-state-box
         cmd-ctx-running-box
         cmd-ctx-event-bus
         cmd-ctx-session-dir
         cmd-ctx-needs-redraw-box
         cmd-ctx-model-registry-box
         cmd-ctx-last-prompt-box
         cmd-ctx-session-runner
         cmd-ctx-input-text-box
         cmd-ctx-extension-registry-box

         ;; Main command dispatcher
         process-slash-command
         process-extension-command
         cmd-ctx-session-factory-runner)

;; ============================================================
;; Extension command dispatch (extracted W-09)
;; ============================================================

(define (process-extension-command cctx state)
  ;; Try extension command dispatch before showing error
  (define ext-reg-box (cmd-ctx-extension-registry-box cctx))
  (define ext-reg (and ext-reg-box (unbox ext-reg-box)))
  (define input-text (unbox (cmd-ctx-input-text-box cctx)))
  (define cmd-name
    (let ([trimmed (string-trim input-text)])
      (and (> (string-length trimmed) 0)
           (char=? (string-ref trimmed 0) #\/)
           (let ([parts (string-split trimmed)]) (and (pair? parts) (car parts))))))
  (log-debug "command dispatch: cmd=~a has-ext-reg=~a" cmd-name (and ext-reg #t))
  (define ext-result
    (and ext-reg
         cmd-name
         (dispatch-hooks 'execute-command (hasheq 'command cmd-name 'input input-text) ext-reg)))
  (log-debug "command dispatch result: action=~a" (and ext-result (hook-result-action ext-result)))
  (cond
    [(and ext-result (hook-result? ext-result) (eq? (hook-result-action ext-result) 'amend))
     ;; Extension handled the command
     (define payload (hook-result-payload ext-result))
     (define new-session-text (hash-ref payload 'new-session #f))
     (define submit-text (hash-ref payload 'submit #f))
     (define display-text (hash-ref payload 'text #f))
     (cond
       [new-session-text
        (when display-text
          (define entry (make-system-entry display-text))
          (set-box! (cmd-ctx-state-box cctx)
                    (add-transcript-entry (unbox (cmd-ctx-state-box cctx)) entry)))
        (define factory (cmd-ctx-session-factory-runner cctx))
        (cond
          [factory
           (thread (lambda ()
                     (with-handlers
                         ([exn:fail?
                           (lambda (e)
                             (define err-msg (format "[ERROR] /go failed: ~a" (exn-message e)))
                             (define entry
                               (make-entry 'system err-msg (current-inexact-milliseconds) (hash)))
                             (set-box! (cmd-ctx-state-box cctx)
                                       (add-transcript-entry (unbox (cmd-ctx-state-box cctx)) entry))
                             (set-box! (cmd-ctx-needs-redraw-box cctx) #t))])
                       (factory new-session-text))))]
          [else
           (define runner (cmd-ctx-session-runner cctx))
           (when runner
             (thread
              (lambda ()
                (with-handlers
                    ([exn:fail?
                      (lambda (e)
                        (define err-msg (format "[ERROR] Session runner failed: ~a" (exn-message e)))
                        (define entry
                          (make-entry 'system err-msg (current-inexact-milliseconds) (hash)))
                        (set-box! (cmd-ctx-state-box cctx)
                                  (add-transcript-entry (unbox (cmd-ctx-state-box cctx)) entry))
                        (set-box! (cmd-ctx-needs-redraw-box cctx) #t))])
                  (runner new-session-text)))))])]
       [submit-text
        (when display-text
          (define entry (make-system-entry display-text))
          (set-box! (cmd-ctx-state-box cctx)
                    (add-transcript-entry (unbox (cmd-ctx-state-box cctx)) entry)))
        (define runner (cmd-ctx-session-runner cctx))
        (when runner
          (thread (lambda ()
                    (with-handlers
                        ([exn:fail?
                          (lambda (e)
                            (define err-msg (format "[ERROR] Prompt failed: ~a" (exn-message e)))
                            (define entry
                              (make-entry 'system err-msg (current-inexact-milliseconds) (hash)))
                            (set-box! (cmd-ctx-state-box cctx)
                                      (add-transcript-entry (unbox (cmd-ctx-state-box cctx)) entry))
                            (set-box! (cmd-ctx-needs-redraw-box cctx) #t))])
                      (runner submit-text)))))]
       [display-text
        (define entry (make-system-entry display-text))
        (set-box! (cmd-ctx-state-box cctx)
                  (add-transcript-entry (unbox (cmd-ctx-state-box cctx)) entry))])
     'continue]
    [else
     (log-debug "command fell through: cmd=~a" cmd-name)
     (define entry (make-error-entry "Unknown command. Type /help for commands."))
     (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
     'continue]))

;; ============================================================
;; Main command dispatcher
;; ============================================================

;; Process a slash command. Returns 'continue | 'quit
;; cmd can be: symbol | (list symbol args...)
(define (process-slash-command cctx cmd)
  ;; Mark dirty (defensive: slash commands always change state)
  (set-box! (cmd-ctx-needs-redraw-box cctx) #t)
  (define state (unbox (cmd-ctx-state-box cctx)))
  ;; R-17: Normalize to internal dispatch form from parsed-command struct
  (define sym
    (if (parsed-command? cmd)
        (parsed-command-canonical-name cmd)
        cmd))
  (define args
    (if (parsed-command? cmd)
        (parsed-command-args cmd)
        '()))
  ;; Handle structured commands (lists from legacy path or parsed-command)
  (cond
    [(and (list? cmd) (not (parsed-command? cmd)))
     (match (car cmd)
       ['switch (handle-switch-command cctx (cadr cmd))]
       ['children (handle-children-command cctx (cadr cmd))]
       ['model (handle-model-command cctx (and (>= (length cmd) 2) (cadr cmd)))]
       ['name (handle-name-command cctx (and (>= (length cmd) 2) (cadr cmd)))]
       ['fork (handle-fork-command cctx (and (>= (length cmd) 2) (cadr cmd)))]
       ['sessions (handle-sessions-tui-command cctx cmd)]
       [(or 'switch-error 'children-error)
        (define entry (make-entry 'error (cadr cmd) 0 (hash)))
        (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
        'continue]
       [_ 'continue])]
    [else
     ;; R-17: Unified dispatch — sym comes from parsed-command or symbol
     (when (parsed-command? cmd)
       (set! cmd sym))
     (match cmd
       ['model (handle-model-command cctx)]
       ['history (handle-history-command cctx)]
       ['help
        ;; Generate help from palette registry
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
        (define new-state
          (for/fold ([s state]) ([e (in-list help-entries)])
            (add-transcript-entry s e)))
        (set-box! (cmd-ctx-state-box cctx) new-state)
        'continue]
       ['clear
        (set-box! (cmd-ctx-state-box cctx) (struct-copy ui-state state [transcript '()]))
        'continue]
       ['compact
        ;; Compact: add status message and notify runtime
        (define entry (make-system-entry "[compact requested]"))
        (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
        (when (cmd-ctx-event-bus cctx)
          (publish! (cmd-ctx-event-bus cctx)
                    (make-event "compact.requested"
                                (inexact->exact (truncate (/ (current-inexact-milliseconds) 1000)))
                                (or (ui-state-session-id state) "")
                                #f
                                (hash))))
        'continue]
       ['status
        ;; Show session and provider status
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
        (define new-state
          (for/fold ([s state]) ([line (in-list lines)])
            (add-transcript-entry s (make-entry 'system (format "[STATUS] ~a" line) 0 (hash)))))
        (set-box! (cmd-ctx-state-box cctx) new-state)
        'continue]
       ['interrupt
        ;; Interrupt: notify runtime
        (when (cmd-ctx-event-bus cctx)
          (publish! (cmd-ctx-event-bus cctx)
                    (make-event "interrupt.requested"
                                (inexact->exact (truncate (/ (current-inexact-milliseconds) 1000)))
                                (or (ui-state-session-id state) "")
                                #f
                                (hash))))
        (define entry (make-system-entry "[interrupt requested]"))
        (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
        'continue]
       ['tree (handle-tree-command cctx)]
       ['branches (handle-branches-command cctx)]
       ['leaves (handle-leaves-command cctx)]
       ['name (handle-name-command cctx)]
       ['sessions (handle-sessions-tui-command cctx #f)]
       ['retry
        ;; /retry: resubmit last prompt, enriched with previous turn context
        (define last-prompt (unbox (cmd-ctx-last-prompt-box cctx)))
        (cond
          [last-prompt
           (define entry
             (make-entry 'system
                         (format "[retry: resubmitting]")
                         (current-inexact-milliseconds)
                         (hash)))
           (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
           ;; v0.14.2 Wave 2: Enrich retry with tool summary from previous turn
           (define tool-summary (get-last-turn-tool-summary (unbox (cmd-ctx-state-box cctx))))
           (define enriched-prompt
             (if tool-summary
                 (format "~a\n\n[Context from previous attempt: ~a]" last-prompt tool-summary)
                 last-prompt))
           (define runner (cmd-ctx-session-runner cctx))
           (when runner
             (thread
              (lambda ()
                (with-handlers ([exn:fail?
                                 (lambda (e)
                                   (define bus (cmd-ctx-event-bus cctx))
                                   (define sid (ui-state-session-id (unbox (cmd-ctx-state-box cctx))))
                                   (when (and bus sid)
                                     (publish!
                                      bus
                                      (make-event
                                       "runtime.error"
                                       (current-inexact-milliseconds)
                                       sid
                                       #f
                                       (hasheq 'error (exn-message e) 'errorType 'internal-error)))
                                     (publish! bus
                                               (make-event "turn.completed"
                                                           (current-inexact-milliseconds)
                                                           sid
                                                           #f
                                                           (hasheq 'reason "error")))))])
                  (runner enriched-prompt)))))]
          [else
           (define entry (make-error-entry "No previous prompt to retry."))
           (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))])
        'continue]
       ['activate (handle-activate-command cctx)]
       ['deactivate (handle-deactivate-command cctx)]
       ['reload (handle-reload-command cctx)]
       ['quit
        (set-box! (cmd-ctx-running-box cctx) #f)
        'quit]
       ['unknown (process-extension-command cctx state)]
       [else 'continue])]))
