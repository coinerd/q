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
         "../util/event/event-bus.rkt"
         "../extensions/hooks.rkt"
         "../extensions/api.rkt"
         "../runtime/goal/goal-checks.rkt"
         (only-in "../runtime/goal/goal-state.rkt"
                  goal-check-label
                  goal-check-command
                  goal-state-turns-used
                  goal-state-status)
         (only-in "../runtime/agent-session.rkt"
                  agent-session?
                  agent-session-model-name
                  session-provider
                  session-event-bus
                  session-id)
         (only-in "../runtime/runtime-helpers.rkt" emit-session-event!)
         (only-in "../runtime/session/session-config.rkt" current-goal-loop-enabled?)
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
                  handle-login-command)
         ;; Goal runner + bridge
         (only-in "../runtime/goal/goal-runner.rkt" goal-run! current-goal-session-log-path)
         (only-in "commands/goal-bridge.rkt"
                  make-goal-event-bridge
                  make-goal-run-prompt!
                  render-goal-history)
         (only-in "commands/context.rkt" cmd-ctx-agent-session-box cmd-ctx-goal-cancel-box)
         (only-in "context.rkt" atomic-state-update!)
         (only-in "commands/goal-bridge.rkt" render-goal-evidence)
         (only-in "../runtime/session/session-types.rkt" session-log-path-for)
         (only-in "../runtime/goal/goal-runner.rkt" current-repo-base-sha current-working-tree-hash)
         (only-in "../extensions/gsd/go-orchestrator.rkt"
                  execute-campaign-token!
                  campaign-result-status)
         ;; W2 disclosure controls: /toggle-detail fallback for terminals that
         ;; cannot deliver a distinct Ctrl+O sequence.
         (only-in "../ui-core/disclosure-state.rkt"
                  resolve-toggle-target
                  disclosure-toggle
                  active-streaming-artifact-id))

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
         apply-slash-command
         process-extension-command
         execute-extension-command
         cmd-ctx-session-factory-runner
         cmd-ctx-goal-cancel-box)

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
;; Handles campaigns, new-session, submit, and display-text actions
(define (append-campaign-message! cctx text)
  (define entry (make-system-entry text))
  (define st (unbox (cmd-ctx-state-box cctx)))
  ;; v0.99.83: When the campaign finishes (with any result), clear the
  ;; busy flag.  The campaign runs in a background thread; the TUI's
  ;; busy-since may persist from the initial /go turn submission.
  (set-box! (cmd-ctx-state-box cctx)
            (set-busy-since (set-busy (add-transcript-entry st entry) #f) #f))
  (set-box! (cmd-ctx-needs-redraw-box cctx) #t))

(define (execute-campaign-command cctx campaign-token display-text)
  (when display-text
    (append-campaign-message! cctx display-text))
  (define factory (cmd-ctx-session-factory-runner cctx))
  ;; v0.99.96: Save pre-campaign session so we can restore it after the
  ;; campaign completes or fails.  make-campaign-runner switches the TUI
  ;; to a dedicated campaign session; without restoration, subsequent
  ;; prompts (/retry, user input) would publish events with a session-id
  ;; that doesn't match the TUI state, causing event filtering and a
  ;; permanent busy hang.
  (define pre-campaign-sess (unbox (cmd-ctx-agent-session-box cctx)))
  (define pre-campaign-sid
    (and pre-campaign-sess (agent-session? pre-campaign-sess) (session-id pre-campaign-sess)))
  (define pre-campaign-model
    (and pre-campaign-sess
         (agent-session? pre-campaign-sess)
         (agent-session-model-name pre-campaign-sess)))
  (define runner
    (cond
      [(and factory (procedure-arity-includes? factory 0)) (factory)]
      [factory factory]
      [else (cmd-ctx-session-runner cctx)]))
  ;; v0.99.96: Restore helper — switches back to the pre-campaign session
  ;; so the TUI state and agent-session-box are consistent for subsequent
  ;; user interactions.
  (define (restore-pre-campaign-session!)
    (when (and pre-campaign-sess (agent-session? pre-campaign-sess))
      (set-box! (cmd-ctx-agent-session-box cctx) pre-campaign-sess)
      (define cur-state (unbox (cmd-ctx-state-box cctx)))
      (set-box! (cmd-ctx-state-box cctx)
                (struct-copy ui-state
                             cur-state
                             [session-id pre-campaign-sid]
                             [model-name (or pre-campaign-model (ui-state-model-name cur-state))]))
      (set-box! (cmd-ctx-needs-redraw-box cctx) #t)))
  (if runner
      (thread (lambda ()
                (with-handlers ([exn:fail? (lambda (e)
                                             (append-campaign-message!
                                              cctx
                                              (format "[ERROR] /go campaign failed: ~a"
                                                      (exn-message e))))])
                  (define result (execute-campaign-token! campaign-token runner))
                  (unless (eq? (campaign-result-status result) 'campaign-complete)
                    (append-campaign-message! cctx
                                              (format "[ERROR] /go campaign stopped: ~a"
                                                      (campaign-result-status result)))))
                ;; v0.99.96: Always restore the pre-campaign session after
                ;; the campaign thread completes (success, failure, or exception).
                (restore-pre-campaign-session!)))
      (begin
        (append-campaign-message! cctx
                                  "[ERROR] No session runner or factory available for /go campaign.")
        (restore-pre-campaign-session!))))

(define (execute-extension-command cctx state payload)
  (define campaign-token (hash-ref payload 'campaign-token #f))
  (define new-session-text (hash-ref payload 'new-session #f))
  (define submit-text (hash-ref payload 'submit #f))
  (define display-text (hash-ref payload 'text #f))
  (cond
    [campaign-token (execute-campaign-command cctx campaign-token display-text)]
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

;; Pure-ish state transition for slash commands.
;; Returns (values new-state result) where result is 'continue | 'quit.
;; W6 (v0.72.7): Return-based wrapper — callers can adopt this instead of
;; relying on set-box! side effects. Currently delegates to process-slash-command.
(define (apply-slash-command state cctx cmd)
  (define result (process-slash-command cctx cmd))
  (define new-state (unbox (cmd-ctx-state-box cctx)))
  (values new-state result))

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
       ['model (handle-model-command cctx (and (pair? args) (car args)))]
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
       ['toggle-detail
        (handle-toggle-detail-command cctx state)
        'continue]
       ['quit (handle-quit-command cctx)]
       ['unknown (process-extension-command cctx state)]
       [else 'continue])]))

;; ============================================================
;; Toggle-detail command handler (W2 disclosure controls)
;; ============================================================
;; Text-command fallback for ui.transcript.toggle-detail. Terminals that
;; cannot disambiguate the raw Ctrl+O control sequence remain usable via
;; /toggle-detail (alias /expand-reasoning, registered in command-parse).
;; Routes to the identical intent as the keymap-registered key binding.

(define (handle-toggle-detail-command cctx state)
  (define target-id
    (resolve-toggle-target state
                           (ui-state-focused-component state)
                           active-streaming-artifact-id))
  (cond
    [target-id
     (define new-state
       (struct-copy ui-state state
                    [disclosure (disclosure-toggle
                                 (ui-state-disclosure state)
                                 target-id)]))
     (set-box! (cmd-ctx-state-box cctx) new-state)]
    [else
     ;; Harmless status hint when no detail artifact exists (W2 Done #7).
     (define hint-entry
       (make-entry 'system "No reasoning to expand" 0 (hash)))
     (set-box! (cmd-ctx-state-box cctx)
               (add-transcript-entry state hint-entry))]))

;; ============================================================
;; Goal command handler
;; ============================================================

(define (handle-goal-command cctx state args)
  (define arg-text (string-trim (string-join args " ")))
  (cond
    ;; /goal clear — cancel active goal
    [(string=? arg-text "clear")
     ;; Signal the running goal thread to stop
     (define cancel-box (cmd-ctx-goal-cancel-box cctx))
     (when cancel-box
       (set-box! cancel-box #t))
     (define cleared-state (struct-copy ui-state state [active-goal #f]))
     (define entry (make-system-entry "[goal] Active goal cancelled."))
     (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry cleared-state entry))
     'continue]
    ;; /goal history — render persisted evaluator decision trail (W1, G-8)
    [(string=? arg-text "history")
     (define sess (unbox (cmd-ctx-agent-session-box cctx)))
     (define log-path (and (agent-session? sess) (session-log-path-for sess)))
     (define entry (make-system-entry (render-goal-history log-path)))
     (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
     'continue]
    ;; /goal evidence — render persisted verification evidence (W3, G-5)
    [(string=? arg-text "evidence")
     (define sess (unbox (cmd-ctx-agent-session-box cctx)))
     (define log-path (and (agent-session? sess) (session-log-path-for sess)))
     (define entry
       (make-system-entry
        (render-goal-evidence log-path (current-repo-base-sha) (current-working-tree-hash))))
     (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
     'continue]
    ;; /goal status or /goal (no args) — show status
    [(or (string=? arg-text "") (string=? arg-text "status"))
     (define goal-info (ui-state-active-goal state))
     (define entry
       (if goal-info
           (make-system-entry
            (format "[goal] ~a: ~a\nStatus: ~a | Turns: ~a/~a"
                    (if (eq? (goal-display-info-status goal-info) 'active) "Active" "Last")
                    (goal-display-info-goal-text goal-info)
                    (goal-display-info-status goal-info)
                    (goal-display-info-turns-used goal-info)
                    (goal-display-info-max-turns goal-info)))
           (make-system-entry "[goal] No active goal. Use /goal \"<description>\" to set one.")))
     (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
     'continue]
    ;; /goal "<description>" [--check 'cmd'] — set a goal with optional checks
    [else
     ;; Parse flags before stripping/quotes; clean-text must not reference an
     ;; internal definition before it is initialized.
     (define flag-parts (string-split arg-text))
     (define (flag-value flag default)
       (define idx (index-of flag-parts flag))
       (cond
         [(and idx
               (< (add1 idx) (length flag-parts))
               (string->number (list-ref flag-parts (add1 idx))))
          =>
          (lambda (n) (inexact->exact (floor n)))]
         [else default]))
     (define turns-n (flag-value "--turns" 8))
     (define timeout-secs (flag-value "--turn-timeout-secs" 1800))
     (define evaluator-mode
       (let ([eval-idx (index-of flag-parts "--evaluator")])
         (if (and eval-idx
                  (< (add1 eval-idx) (length flag-parts))
                  (equal? (list-ref flag-parts (add1 eval-idx)) "agent"))
             'agent
             'transcript)))
     (define flag-stripped-text
       (let loop ([toks flag-parts]
                  [acc '()])
         (cond
           [(null? toks) (string-join (reverse acc) " ")]
           [(member (car toks) '("--turns" "--turn-timeout-secs" "--evaluator"))
            (loop (if (null? (cdr toks))
                      '()
                      (cddr toks))
                  acc)]
           [else (loop (cdr toks) (cons (car toks) acc))])))
     (define clean-text
       (let ([t flag-stripped-text])
         (if (and (> (string-length t) 1)
                  (or (char=? (string-ref t 0) #\") (char=? (string-ref t 0) #\'))
                  (or (char=? (string-ref t (sub1 (string-length t))) #\")
                      (char=? (string-ref t (sub1 (string-length t))) #\')))
             (substring t 1 (sub1 (string-length t)))
             t)))
     ;; Check for --check arguments after flag parsing/stripping
     (define-values (goal-text checks)
       (if (string-contains? clean-text "--check")
           (parse-goal-checks clean-text)
           (values clean-text '())))
     ;; Concurrent goal guard — reject only while the recorded goal is live.
     (define active-info (ui-state-active-goal state))
     (define goal-live? (and active-info (eq? (goal-display-info-status active-info) 'active)))
     (cond
       [goal-live?
        (define entry
          (make-system-entry "[goal] REJECTED — a goal is already active. Use /goal clear first."))
        (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
        'continue]
       [else
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
                          (map (lambda (c)
                                 (format "~a: ~a" (goal-check-label c) (goal-check-command c)))
                               checks)
                          ", "))))
           (define eval-info (if (eq? evaluator-mode 'agent) " [agent evaluator]" ""))
           ;; Feature flag guard
           (cond
             [(not (current-goal-loop-enabled?))
              (define entry
                (make-system-entry
                 "[goal] Goal loop is currently disabled. Enable with (current-goal-loop-enabled? #t)"))
              (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
              'continue]
             [else
              ;; Set initial display state
              (define goal-info (goal-display-info clean-text 0 turns-n 'active))
              (define init-state (struct-copy ui-state state [active-goal goal-info]))
              ;; Get session from cmd-ctx
              (define sess (unbox (cmd-ctx-agent-session-box cctx)))
              (cond
                [(not (agent-session? sess))
                 (define entry (make-system-entry "[goal] No active session. Start a session first."))
                 (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
                 'continue]
                [else
                 (define provider (session-provider sess))
                 (define bus (session-event-bus sess))
                 (define sid (session-id sess))
                 ;; Determine evaluator model
                 (define evaluator-model "default")
                 ;; Create adapters
                 (define on-event (make-goal-event-bridge bus sid))
                 (define on-status
                   (lambda (msg) (emit-session-event! bus sid "goal.status" (hasheq 'message msg))))
                 (define shutdown-box (cmd-ctx-running-box cctx))
                 (define cancel-box (cmd-ctx-goal-cancel-box cctx))
                 (define run-prompt! (make-goal-run-prompt! sess))
                 ;; Set initial state in UI
                 (set-box! (cmd-ctx-state-box cctx) init-state)
                 ;; Spawn autonomous loop in background thread
                 (thread
                  (lambda ()
                    (with-handlers
                        ([exn:fail?
                          (lambda (e)
                            (on-event
                             'goal-failed
                             (hasheq 'goal-text clean-text 'reason (exn-message e) 'turns-used 0))
                            (displayln (format "goal loop failed: ~a" (exn-message e))))])
                      (parameterize ([current-goal-session-log-path (session-log-path-for sess)])
                        (define result
                          (goal-run! clean-text
                                     provider
                                     evaluator-model
                                     run-prompt!
                                     #:max-turns turns-n
                                     #:turn-timeout-secs timeout-secs
                                     #:evaluator-mode evaluator-mode
                                     #:checks checks
                                     #:on-event on-event
                                     #:on-status on-status
                                     #:shutdown-check (lambda ()
                                                        (or (and cancel-box (unbox cancel-box))
                                                            (not (unbox shutdown-box))))))
                        ;; Reset cancel-box for next goal (no state mutation — events handle display)
                        (when cancel-box
                          (set-box! cancel-box #f))))))
                 (define entry
                   (make-system-entry (format "[goal] Autonomous loop started: ~a~a~a"
                                              clean-text
                                              check-info
                                              eval-info)))
                 (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry init-state entry))
                 'continue])])])])]))
