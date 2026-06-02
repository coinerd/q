#lang racket/base

;; q/gui/slash-commands.rkt — GUI slash command handler
;;
;; Extracted from gui/main.rkt to reduce monolith size.
;; Provides make-slash-command-handler factory, add-system-msg! helper,
;; and try-extension-dispatch for extension hook integration.

(require racket/contract
         racket/string
         racket/format
         "../agent/event-bus.rkt"
         "../runtime/agent-session.rkt"
         "../extensions/hooks.rkt"
         "../tui/command-parse.rkt"
         (only-in "../runtime/goal/goal-runner.rkt" goal-run!)
         (only-in "../tui/commands/goal-bridge.rkt" make-goal-event-bridge make-goal-run-prompt!)
         (only-in "../runtime/session/session-config.rkt" current-goal-loop-enabled?)
         (only-in "../runtime/goal-state.rkt" goal-state-turns-used goal-state-status)
         "gui-types.rkt")

(provide make-slash-command-handler
         add-system-msg!
         try-extension-dispatch)

;; --------------------------------------------------
;; Helper: add a system message to the transcript
;; --------------------------------------------------
(define (add-system-msg! text state-box gui-state-lock [notify! void])
  (call-with-semaphore
   gui-state-lock
   (lambda ()
     (set-box! state-box (gui-state-add-message (unbox state-box) (make-gui-message "system" text)))
     (notify!))))

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
     ;; B1: Handle 'new-session from GSD /go (matches TUI execute-extension-command)
     (when (hash-ref payload 'new-session #f)
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
(define (make-slash-command-handler sess state-box gui-state-lock [notify! void])
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
          (add-system-msg! (string-append "Available commands:\n"
                                          "  /help, /h, /?    Show this help\n"
                                          "  /quit, /q, /exit Quit\n"
                                          "  /clear, /cls     Clear transcript\n"
                                          "  /status, /st     Show session status\n"
                                          "  /model, /m       Show or change model\n"
                                          "  /compact         Run context compaction\n"
                                          "  /plan            GSD plan command\n"
                                          "  /go              GSD execute command\n"
                                          "  /activate, /a    Activate extensions\n"
                                          "  /goal            Set/show/clear autonomous goal\n")
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
                 (add-system-msg! (format "[goal] Active: ~a\nStatus: ~a | Turns: ~a/~a"
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
                   ;; Strip surrounding quotes from goal text
                   (define clean-text
                     (let ([t goal-arg])
                       (if (and (> (string-length t) 1)
                                (or (char=? (string-ref t 0) #\") (char=? (string-ref t 0) #\'))
                                (or (char=? (string-ref t (sub1 (string-length t))) #\")
                                    (char=? (string-ref t (sub1 (string-length t))) #\')))
                           (substring t 1 (sub1 (string-length t)))
                           t)))
                   ;; Set initial goal state
                   (define goal-info
                     (hash 'goal-text clean-text 'turns-used 0 'max-turns 8 'status 'active))
                   (call-with-semaphore
                    gui-state-lock
                    (lambda ()
                      (set-box! state-box (gui-state-set-active-goal (unbox state-box) goal-info))
                      (notify!)))
                   ;; Get session resources (guard for no session)
                   (define provider (and sess (session-provider sess)))
                   (define bus (and sess (session-event-bus sess)))
                   (define sid (and sess (session-id sess)))
                   (define on-event (and bus sid (make-goal-event-bridge bus sid)))
                   (define run-prompt! (and sess (make-goal-run-prompt! sess)))
                   (add-system-msg! (format "[goal] Autonomous loop started: ~a" clean-text)
                                    state-box
                                    gui-state-lock
                                    notify!)
                   ;; Spawn autonomous loop in background thread (only if session available)
                   (when (and provider on-event run-prompt!)
                     (thread (lambda ()
                               (with-handlers ([exn:fail? (lambda (e)
                                                            (on-event 'goal-failed
                                                                      (hasheq 'goal-text
                                                                              clean-text
                                                                              'reason
                                                                              (exn-message e)
                                                                              'turns-used
                                                                              0)))])
                                 (define result
                                   (goal-run! clean-text
                                              provider
                                              "default"
                                              run-prompt!
                                              #:max-turns 8
                                              #:on-event on-event
                                              #:on-status (lambda (msg) (void))
                                              #:shutdown-check (lambda () #f)))
                                 ;; Update display with final result
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
                                              (gui-state-set-active-goal (unbox state-box)
                                                                         final-info))
                                    (notify!)))))))
                   #t))])]
         [(interrupt)
          (add-system-msg! "Interrupt not yet supported in GUI mode."
                           state-box
                           gui-state-lock
                           notify!)
          #t]
         [else
          (or (try-extension-dispatch sess state-box gui-state-lock input-text)
              (begin
                (add-system-msg! (format "Unknown command: ~a. Type /help for available commands."
                                         input-text)
                                 state-box
                                 gui-state-lock
                                 notify!)
                #t))])])))
