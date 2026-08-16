#lang racket/base

;; q/gui/slash-commands.rkt — GUI slash command handler
;;
;; Extracted from gui/main.rkt to reduce monolith size.
;; Provides make-slash-command-handler factory, add-system-msg! helper,
;; and try-extension-dispatch for extension hook integration.

(require racket/contract
         racket/string
         racket/format
         "../util/event/event-bus.rkt"
         "../runtime/agent-session.rkt"
         "../extensions/hooks.rkt"
         "../tui/command-parse.rkt"
         (only-in "../runtime/goal/goal-runner.rkt" goal-run! current-goal-session-log-path)
         (only-in "../tui/commands/goal-bridge.rkt" make-goal-event-bridge make-goal-run-prompt!)
         (only-in "../runtime/session/session-config.rkt" current-goal-loop-enabled?)
         (only-in "../runtime/session/session-types.rkt" session-log-path-for)
         (only-in "../runtime/goal/goal-state.rkt" goal-state-turns-used goal-state-status)
         ;; GAP-CR (v0.98.8 W1): Dynamic command registry lookup
         (only-in "../ui-core/command-registry.rkt"
                  ui-registry-lookup
                  ui-registry-all
                  ui-command?
                  ui-command-name
                  ui-command-summary
                  ui-command-gui?
                  canonical-commands
                  make-ui-command-registry)
         "gui-types.rkt"
         "../runtime/goal/goal-checks.rkt"
         (only-in "../extensions/gsd/go-orchestrator.rkt"
                  execute-campaign-token!
                  campaign-result-status)
         (only-in "../runtime/session/session-types.rkt" agent-session-config)
         (only-in "../runtime/session/session-config.rkt" config-max-iterations)
         (only-in "../extensions/gsd/policy.rkt" gsd-session-iteration-budget)
         (only-in "../ui-core/ui-intents.rkt" make-toggle-detail-intent))

;; GAP-CR (v0.98.8 W1): Cache command registry at module level instead of rebuilding per invocation.
(define the-canonical-registry (make-ui-command-registry canonical-commands))

;; M-01 (v0.98.10 W0): Tighten sess from any/c to (or/c agent-session? #f).
(provide (contract-out [make-slash-command-handler
                        (->* ((or/c agent-session? #f) (box/c gui-state?) semaphore?)
                             (procedure? #:goal-cancel-box (box/c boolean?))
                             (-> string? boolean?))]
                       [add-system-msg!
                        (->* (string? (box/c gui-state?) semaphore?) (procedure?) void?)]
                       [try-extension-dispatch
                        (-> (or/c agent-session? #f) (box/c gui-state?) semaphore? string? boolean?)])
         make-gui-campaign-runner)

;; --------------------------------------------------
;; Helper: add a system message to the transcript
;; --------------------------------------------------
(define (add-system-msg! text state-box gui-state-lock [notify! void])
  (call-with-semaphore
   gui-state-lock
   (lambda ()
     (set-box! state-box (gui-state-add-message (unbox state-box) (make-gui-message "system" text)))
     (notify!))))

(define (make-gui-campaign-runner initiating-session)
  (define config (agent-session-config initiating-session))
  (define campaign-session (make-agent-session config))
  (values campaign-session
          (lambda (prompt)
            (run-prompt! campaign-session
                         prompt
                         #:max-iterations
                         (gsd-session-iteration-budget (config-max-iterations config))))))

;; --------------------------------------------------
;; Extension dispatch
;;
;; Tries to dispatch the input to an extension hook.
;; Returns #t if handled (amend or block), #f otherwise.
;; --------------------------------------------------
(define (try-extension-dispatch sess state-box gui-state-lock input-text)
  (define ext-reg (and sess (agent-session-extension-registry sess)))
  (define cmd-name
    (let* ([trimmed (string-trim input-text)]
           [parts (string-split trimmed)])
      (and (pair? parts) (car parts))))
  (define ext-result
    (and ext-reg
         cmd-name
         (dispatch-hooks 'execute-command (hasheq 'command cmd-name 'input input-text) ext-reg)))
  (cond
    [(and (hook-result? ext-result) (eq? (hook-result-action ext-result) 'amend))
     (define payload (hook-result-payload ext-result))
     (define campaign-token (hash-ref payload 'campaign-token #f))
     (when (hash-ref payload 'text #f)
       (add-system-msg! (hash-ref payload 'text) state-box gui-state-lock))
     (when (hash-ref payload 'submit #f)
       (thread (lambda ()
                 (with-handlers ([exn:fail? (lambda (e)
                                              (add-system-msg! (format "[ERROR] submit failed: ~a"
                                                                       (exn-message e))
                                                               state-box
                                                               gui-state-lock))])
                   (run-prompt! sess (hash-ref payload 'submit))))))
     ;; Campaign tokens take precedence over the legacy all-plan prompt.  The
     ;; coordinator calls run-prompt! once per isolated wave and advances only
     ;; after its verifier accepts the returned attempt.
     (when campaign-token
       (thread
        (lambda ()
          (with-handlers ([exn:fail? (lambda (e)
                                       (add-system-msg! (format "[ERROR] /go campaign failed: ~a"
                                                                (exn-message e))
                                                        state-box
                                                        gui-state-lock))])
            (define result
              (execute-campaign-token! campaign-token
                                       (lambda (prompt)
                                         (define-values (_campaign-session campaign-runner)
                                           (make-gui-campaign-runner sess))
                                         (campaign-runner prompt))
                                       ;; D4 (#9351): name the lease after the
                                       ;; orchestrating GUI session.
                                       #:lease-owner (session-id sess)))
            (unless (eq? (campaign-result-status result) 'campaign-complete)
              (add-system-msg! (format "[ERROR] /go campaign stopped: ~a"
                                       (campaign-result-status result))
                               state-box
                               gui-state-lock))))))
     ;; Legacy extension compatibility when no runtime campaign token exists.
     (when (and (not campaign-token) (hash-ref payload 'new-session #f))
       (thread (lambda ()
                 (with-handlers ([exn:fail? (lambda (e)
                                              (add-system-msg! (format "[ERROR] /go failed: ~a"
                                                                       (exn-message e))
                                                               state-box
                                                               gui-state-lock))])
                   (run-prompt! sess (hash-ref payload 'new-session))))))
     #t]
    [(and (hook-result? ext-result) (eq? (hook-result-action ext-result) 'block))
     (add-system-msg! (format "Command ~a blocked. Try /help." cmd-name) state-box gui-state-lock)
     #t]
    [else #f]))

;; --------------------------------------------------
;; Factory: create a slash command handler
;;
;; Returns (-> string? boolean?) — #t if handled, #f otherwise
;; --------------------------------------------------
(define (make-slash-command-handler sess
                                    state-box
                                    gui-state-lock
                                    [notify! void]
                                    #:goal-cancel-box [goal-cancel-box (box #f)])
  (lambda (input-text)
    (define parsed (parse-command-name input-text))
    (cond
      [(not parsed) #f]
      [(eq? parsed 'unknown)
       (or (try-extension-dispatch sess state-box gui-state-lock input-text)
           (begin
             (add-system-msg! (format "Unknown command: ~a. Type /help for available commands."
                                      input-text)
                              state-box
                              gui-state-lock
                              notify!)
             #t))]
      [else
       (define cmd
         (if (parsed-command? parsed)
             (parsed-command-canonical-name parsed)
             parsed))
       (define args
         (if (parsed-command? parsed)
             (parsed-command-args parsed)
             '()))
       (case cmd
         [(quit)
          (close-session! sess)
          (exit 0)]
         [(clear)
          (call-with-semaphore gui-state-lock
                               (lambda ()
                                 (set-box! state-box
                                           (struct-copy gui-state (unbox state-box) [messages '()]))
                                 (notify!)))
          #t]
         [(help)
          ;; G-CR1 (v0.98.12): Generate help from command registry.
          (define all-cmds (ui-registry-all the-canonical-registry))
          (define gui-cmds (filter ui-command-gui? all-cmds))
          (define help-lines
            (for/list ([c (in-list gui-cmds)])
              (format "  /~a    ~a" (ui-command-name c) (ui-command-summary c))))
          (add-system-msg! (string-append "Available commands:\n" (string-join help-lines "\n"))
                           state-box
                           gui-state-lock
                           notify!)
          #t]
         [(status)
          (add-system-msg! (format "Session: ~a\nModel: ~a\nStatus: ~a\nMessages: ~a"
                                   (session-id sess)
                                   (agent-session-model-name sess)
                                   (if (session-active? sess) "active" "closed")
                                   (length (gui-state-messages (unbox state-box))))
                           state-box
                           gui-state-lock
                           notify!)
          #t]
         [(model)
          (add-system-msg! (if (null? args)
                               (format "Current model: ~a" (agent-session-model-name sess))
                               (format "Model switching not yet supported in GUI. Current: ~a"
                                       (agent-session-model-name sess)))
                           state-box
                           gui-state-lock
                           notify!)
          #t]
         [(compact)
          (add-system-msg! "Context compaction triggered (runs on next turn)."
                           state-box
                           gui-state-lock
                           notify!)
          #t]
         [(goal)
          (define goal-arg (string-trim (string-join args " ")))
          (cond
            [(string=? goal-arg "clear")
             (set-box! goal-cancel-box #t)
             (call-with-semaphore gui-state-lock
                                  (lambda ()
                                    (set-box! state-box
                                              (gui-state-set-active-goal (unbox state-box) #f))
                                    (notify!)))
             (add-system-msg! "[goal] Active goal cancelled." state-box gui-state-lock notify!)
             #t]
            [(or (string=? goal-arg "") (string=? goal-arg "status"))
             (define gs (unbox state-box))
             (define goal-info (gui-state-active-goal gs))
             (if goal-info
                 (add-system-msg!
                  (format "[goal] ~a: ~a\nStatus: ~a | Turns: ~a/~a"
                          (if (eq? (hash-ref goal-info 'status 'active) 'active) "Active" "Last")
                          (hash-ref goal-info 'goal-text "?")
                          (hash-ref goal-info 'status 'active)
                          (hash-ref goal-info 'turns-used 0)
                          (hash-ref goal-info 'max-turns 8))
                  state-box
                  gui-state-lock
                  notify!)
                 (add-system-msg! "[goal] No active goal. Use /goal \"<description>\" to set one."
                                  state-box
                                  gui-state-lock
                                  notify!))
             #t]
            [else
             ;; Feature flag guard
             (if (not (current-goal-loop-enabled?))
                 (begin
                   (add-system-msg!
                    "[goal] Goal loop disabled. Enable with (current-goal-loop-enabled? #t)"
                    state-box
                    gui-state-lock
                    notify!)
                   #t)
                 ;; Goal loop enabled — set up and spawn
                 (let ()
                   (define current-info (gui-state-active-goal (unbox state-box)))
                   (define live-goal?
                     (and current-info (eq? (hash-ref current-info 'status 'active) 'active)))
                   (cond
                     [live-goal?
                      (add-system-msg!
                       "[goal] REJECTED — a goal is already active. Use /goal clear first."
                       state-box
                       gui-state-lock
                       notify!)
                      #t]
                     [(not sess)
                      (add-system-msg! "[goal] No active session. Start a session first."
                                       state-box
                                       gui-state-lock
                                       notify!)
                      #t]
                     [else (void)])
                   ;; Strip surrounding quotes from goal text
                   (define clean-text
                     (let ([t goal-arg])
                       (if (and (> (string-length t) 1)
                                (or (char=? (string-ref t 0) #\") (char=? (string-ref t 0) #\'))
                                (or (char=? (string-ref t (sub1 (string-length t))) #\")
                                    (char=? (string-ref t (sub1 (string-length t))) #\')))
                           (substring t 1 (sub1 (string-length t)))
                           t)))
                   (define-values (parsed-goal-text checks)
                     (if (string-contains? clean-text "--check")
                         (parse-goal-checks clean-text)
                         (values clean-text '())))
                   (define safety-reasons (validate-check-safety checks))
                   (when (pair? safety-reasons)
                     (add-system-msg! (format "[goal] REJECTED — unsafe check commands:\n~a"
                                              (string-join safety-reasons "\n"))
                                      state-box
                                      gui-state-lock
                                      notify!))
                   (define eligible? (and (not live-goal?) sess (null? safety-reasons)))
                   ;; Set initial goal state only after guards above
                   (define goal-info
                     (hash 'goal-text parsed-goal-text 'turns-used 0 'max-turns 8 'status 'active))
                   (when eligible?
                     (call-with-semaphore
                      gui-state-lock
                      (lambda ()
                        (set-box! state-box (gui-state-set-active-goal (unbox state-box) goal-info))
                        (notify!))))
                   ;; Get session resources (guard for no session)
                   (define provider (and sess (session-provider sess)))
                   (define bus (and sess (session-event-bus sess)))
                   (define sid (and sess (session-id sess)))
                   (define on-event (and bus sid (make-goal-event-bridge bus sid)))
                   (define run-prompt! (and sess (make-goal-run-prompt! sess)))
                   (when eligible?
                     (add-system-msg! (format "[goal] Autonomous loop started: ~a" parsed-goal-text)
                                      state-box
                                      gui-state-lock
                                      notify!))
                   ;; Spawn autonomous loop in background thread only after all guards.
                   (when (and eligible? provider on-event run-prompt!)
                     (thread
                      (lambda ()
                        (with-handlers ([exn:fail? (lambda (e)
                                                     (on-event 'goal-failed
                                                               (hasheq 'goal-text
                                                                       clean-text
                                                                       'reason
                                                                       (exn-message e)
                                                                       'turns-used
                                                                       0)))])
                          (define result
                            (parameterize ([current-goal-session-log-path
                                            (session-log-path-for sess)])
                              (goal-run! parsed-goal-text
                                         provider
                                         "default"
                                         run-prompt!
                                         #:max-turns 8
                                         #:checks checks
                                         #:on-event on-event
                                         #:on-status (lambda (msg) (void))
                                         #:shutdown-check (lambda () (unbox goal-cancel-box)))))
                          ;; Update display with final result (skip if cancelled)
                          (define was-cancelled (unbox goal-cancel-box))
                          (unless was-cancelled
                            (define final-info
                              (hash 'goal-text
                                    clean-text
                                    'turns-used
                                    (goal-state-turns-used result)
                                    'max-turns
                                    8
                                    'status
                                    (goal-state-status result)))
                            (call-with-semaphore
                             gui-state-lock
                             (lambda ()
                               (set-box! state-box
                                         (gui-state-set-active-goal (unbox state-box) final-info))
                               (notify!))))
                          ;; Reset cancel box for next goal
                          (set-box! goal-cancel-box #f)))))
                   #t))])]
         [(toggle-detail)
          (define changed? #f)
          (call-with-semaphore gui-state-lock
                               (lambda ()
                                 (define old (unbox state-box))
                                 (define next
                                   (gui-state-apply-intent old (make-toggle-detail-intent #f)))
                                 (unless (eq? next old)
                                   (set-box! state-box next)
                                   (set! changed? #t))))
          (if changed?
              (notify!)
              (add-system-msg! "No reasoning to expand" state-box gui-state-lock notify!))
          #t]
         [(interrupt)
          (add-system-msg! "Interrupt not yet supported in GUI mode."
                           state-box
                           gui-state-lock
                           notify!)
          #t]
         ;; G-CR2 (v0.98.12): Explicit stubs for TUI-only session management commands.
         [(branches tree leaves fork switch children history name sessions deactivate reload)
          (define cmd-str (symbol->string cmd))
          (define reg (ui-registry-lookup the-canonical-registry (format "/~a" cmd-str)))
          (add-system-msg!
           (format "[/~a] ~a — available in TUI mode. Use --tui flag for full session management."
                   cmd-str
                   (if reg
                       (ui-command-summary reg)
                       "Session management"))
           state-box
           gui-state-lock
           notify!)
          #t]
         [else
          ;; GAP-CR (v0.98.8 W1): Try dynamic command registry lookup before falling back.
          ;; Hardcoded commands still work via case above; this is a FALLBACK for commands
          ;; NOT in the hardcoded list. Unknown commands get "Unknown command" message.
          (define registered-cmd (ui-registry-lookup the-canonical-registry (format "/~a" cmd)))
          (or (and registered-cmd
                   (ui-command-gui? registered-cmd)
                   (begin
                     (add-system-msg! (format "Command /~a registered (not yet implemented in GUI)."
                                              (ui-command-name registered-cmd))
                                      state-box
                                      gui-state-lock
                                      notify!)
                     #t))
              (try-extension-dispatch sess state-box gui-state-lock input-text)
              (begin
                (add-system-msg! (format "Unknown command: ~a. Type /help for available commands."
                                         input-text)
                                 state-box
                                 gui-state-lock
                                 notify!)
                #t))])])))
