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
         "../runtime/goal-checks.rkt"
         (only-in "../runtime/goal-state.rkt" goal-check-label goal-check-command)
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
                  handle-deactivate-command)
         ;; W19: general commands extracted
         (only-in "commands/general.rkt"
                  handle-help-command
                  handle-clear-command
                  handle-status-command)
         ;; W19: runtime control commands extracted
         (only-in "commands/runtime-control.rkt"
                  handle-compact-command
                  handle-interrupt-command
                  handle-retry-command
                  handle-quit-command
                  handle-login-command))

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
         execute-extension-command
         cmd-ctx-session-factory-runner)

;; ============================================================
;; Extension command dispatch (extracted W-09)
;; ============================================================

;; Parse a slash command from raw input text
;; Returns the command name (e.g. "/go") or #f
(define (parse-extension-command input-text)
  (define trimmed (string-trim input-text))
  (and (> (string-length trimmed) 0)
       (char=? (string-ref trimmed 0) #\/)
       (let ([parts (string-split trimmed)]) (and (pair? parts) (car parts)))))

;; Validate that an extension command is dispatchable
;; Returns a hook-result if the extension handles it, #f otherwise
(define (validate-extension-command ext-reg cmd-name input-text)
  (and ext-reg
       cmd-name
       (dispatch-hooks 'execute-command (hasheq 'command cmd-name 'input input-text) ext-reg)))

;; Execute an extension command amendment payload
;; Handles new-session, submit, and display-text actions
(define (execute-extension-command cctx state payload)
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
        (thread
         (lambda ()
           (with-handlers ([exn:fail?
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
        (cond
          [runner
           (thread (lambda ()
                     (with-handlers
                         ([exn:fail?
                           (lambda (e)
                             (define err-msg
                               (format "[ERROR] Session runner failed: ~a" (exn-message e)))
                             (define entry
                               (make-entry 'system err-msg (current-inexact-milliseconds) (hash)))
                             (set-box! (cmd-ctx-state-box cctx)
                                       (add-transcript-entry (unbox (cmd-ctx-state-box cctx)) entry))
                             (set-box! (cmd-ctx-needs-redraw-box cctx) #t))])
                       (runner new-session-text))))]
          [else
           ;; P1 hardening: no runner/factory available — show explicit error
           (define err-entry
             (make-entry
              'system
              "[ERROR] No session runner or factory available. The session may not be fully initialized."
              (current-inexact-milliseconds)
              (hash)))
           (set-box! (cmd-ctx-state-box cctx)
                     (add-transcript-entry (unbox (cmd-ctx-state-box cctx)) err-entry))
           (set-box! (cmd-ctx-needs-redraw-box cctx) #t)])])]
    [submit-text
     (when display-text
       (define entry (make-system-entry display-text))
       (set-box! (cmd-ctx-state-box cctx)
                 (add-transcript-entry (unbox (cmd-ctx-state-box cctx)) entry)))
     (define runner (cmd-ctx-session-runner cctx))
     (cond
       [runner
        (thread
         (lambda ()
           (with-handlers ([exn:fail?
                            (lambda (e)
                              (define err-msg (format "[ERROR] Prompt failed: ~a" (exn-message e)))
                              (define entry
                                (make-entry 'system err-msg (current-inexact-milliseconds) (hash)))
                              (set-box! (cmd-ctx-state-box cctx)
                                        (add-transcript-entry (unbox (cmd-ctx-state-box cctx)) entry))
                              (set-box! (cmd-ctx-needs-redraw-box cctx) #t))])
             (runner submit-text))))]
       [else
        ;; P1 hardening: /plan no-op — when no runner available, show explicit error
        (define err-entry
          (make-entry 'system
                      "[ERROR] No session runner available. The session may not be fully initialized."
                      (current-inexact-milliseconds)
                      (hash)))
        (set-box! (cmd-ctx-state-box cctx)
                  (add-transcript-entry (unbox (cmd-ctx-state-box cctx)) err-entry))
        (set-box! (cmd-ctx-needs-redraw-box cctx) #t)])]
    [display-text
     (define entry (make-system-entry display-text))
     (set-box! (cmd-ctx-state-box cctx)
               (add-transcript-entry (unbox (cmd-ctx-state-box cctx)) entry))]))

(define (process-extension-command cctx state)
  ;; Try extension command dispatch before showing error
  (define ext-reg-box (cmd-ctx-extension-registry-box cctx))
  (define ext-reg (and ext-reg-box (unbox ext-reg-box)))
  (define input-text (unbox (cmd-ctx-input-text-box cctx)))
  (define cmd-name (parse-extension-command input-text))
  (log-debug "command dispatch: cmd=~a has-ext-reg=~a" cmd-name (and ext-reg #t))
  (define ext-result (validate-extension-command ext-reg cmd-name input-text))
  (log-debug "command dispatch result: action=~a" (and ext-result (hook-result-action ext-result)))
  (cond
    [(and ext-result (hook-result? ext-result) (eq? (hook-result-action ext-result) 'amend))
     (execute-extension-command cctx state (hook-result-payload ext-result))
     'continue]
    [(and ext-result (hook-result? ext-result) (eq? (hook-result-action ext-result) 'block))
     (log-debug "command blocked by extension: cmd=~a" cmd-name)
     (define block-reason (hook-result-payload ext-result))
     (define msg
       (if (and block-reason (not (equal? block-reason (hasheq))))
           (format
            "Command ~a could not be dispatched: ~a. Try again or use /help for available commands."
            cmd-name
            block-reason)
           (format (string-append "Command ~a could not be dispatched. "
                                  "This may be caused by a large PLAN or a slow extension. "
                                  "Try again or use /help for available commands.")
                   cmd-name)))
     (define entry (make-error-entry msg))
     (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
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
        (set-box! (cmd-ctx-state-box cctx) (handle-help-command cctx state))
        'continue]
       ['clear
        (set-box! (cmd-ctx-state-box cctx) (handle-clear-command cctx state))
        'continue]
       ['compact (handle-compact-command cctx state args)]
       ['login (handle-login-command cctx state args)]
       ['status
        (set-box! (cmd-ctx-state-box cctx) (handle-status-command cctx state))
        'continue]
       ['interrupt (handle-interrupt-command cctx state)]
       ['tree (handle-tree-command cctx)]
       ['branches (handle-branches-command cctx)]
       ['leaves (handle-leaves-command cctx)]
       ['name (handle-name-command cctx)]
       ['sessions (handle-sessions-tui-command cctx #f)]
       ['retry (handle-retry-command cctx state)]
       ['activate (handle-activate-command cctx)]
       ['deactivate (handle-deactivate-command cctx)]
       ['reload (handle-reload-command cctx)]
       ['goal (handle-goal-command cctx state args)]
       ['quit (handle-quit-command cctx)]
       ['unknown (process-extension-command cctx state)]
       [else 'continue])]))

;; ============================================================
;; Goal command handler
;; ============================================================

(define (handle-goal-command cctx state args)
  (define arg-text (string-trim (string-join args " ")))
  (cond
    ;; /goal clear — cancel active goal
    [(string=? arg-text "clear")
     (define cleared-state (struct-copy ui-state state [active-goal #f]))
     (define entry (make-system-entry "[goal] Active goal cancelled."))
     (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry cleared-state entry))
     'continue]
    ;; /goal status or /goal (no args) — show status
    [(or (string=? arg-text "") (string=? arg-text "status"))
     (define goal-info (ui-state-active-goal state))
     (define entry
       (if goal-info
           (make-system-entry (format "[goal] Active: ~a\nStatus: ~a | Turns: ~a/~a"
                                      (goal-display-info-goal-text goal-info)
                                      (goal-display-info-status goal-info)
                                      (goal-display-info-turns-used goal-info)
                                      (goal-display-info-max-turns goal-info)))
           (make-system-entry "[goal] No active goal. Use /goal \"<description>\" to set one.")))
     (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
     'continue]
    ;; /goal "<description>" [--check 'cmd'] — set a goal with optional checks
    [else
     ;; Check for --check arguments
     (define-values (goal-text checks)
       (if (string-contains? arg-text "--check")
           (parse-goal-checks arg-text)
           (values arg-text '())))
     ;; Strip surrounding quotes from goal text if present
     (define clean-text
       (let ([t goal-text])
         (if (and (> (string-length t) 1)
                  (or (char=? (string-ref t 0) #\") (char=? (string-ref t 0) #\'))
                  (or (char=? (string-ref t (sub1 (string-length t))) #\")
                      (char=? (string-ref t (sub1 (string-length t))) #\')))
             (substring t 1 (sub1 (string-length t)))
             t)))
     ;; Check for --evaluator flag
     (define evaluator-mode
       (if (string-contains? arg-text "--evaluator")
           (let ([parts (string-split arg-text)]) (if (member "agent" parts) 'agent 'transcript))
           'transcript))
     ;; Validate check safety
     (define safety-reasons (validate-check-safety checks))
     (cond
       [(pair? safety-reasons)
        (define entry
          (make-system-entry (format "[goal] REJECTED — unsafe check commands:\n~a"
                                     (string-join safety-reasons "\n"))))
        (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
        'continue]
       [else
        (define check-info
          (if (null? checks)
              ""
              (format "\nChecks: ~a"
                      (string-join
                       (map (lambda (c) (format "~a: ~a" (goal-check-label c) (goal-check-command c)))
                            checks)
                       ", "))))
        (define eval-info (if (eq? evaluator-mode 'agent) " [agent evaluator]" ""))
        (define entry
          (make-system-entry
           (format
            "[goal] Goal set: ~a~a~a\nThe autonomous goal loop will be available in a future update."
            clean-text
            check-info
            eval-info)))
        (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
        'continue])]))
