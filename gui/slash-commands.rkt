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
                                          "  /activate, /a    Activate extensions\n")
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
