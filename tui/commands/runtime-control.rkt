#lang racket/base

;; q/tui/commands/runtime-control.rkt — Runtime control commands
;;   (compact, interrupt, retry, quit)
;;
;; Extracted from commands.rkt (W19) to thin the commands parent.

(require racket/hash
         racket/string
         racket/system
         "../state.rkt"
         "../../util/protocol-types.rkt"
         "../../agent/event-bus.rkt"
         "../../runtime/oauth.rkt"
         "../../runtime/oauth-callback.rkt"
         "../../runtime/auth-store.rkt"
         "context.rkt")

;; Handle /compact — request compaction with optional --dry-run
(define (handle-compact-command cctx state [args '()])
  ;; Block during active tool loop
  (cond
    [(ui-state-busy? state)
     (define entry (make-error-entry "Cannot compact while a tool is running. Wait for completion."))
     (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
     (set-box! (cmd-ctx-needs-redraw-box cctx) #t)
     'continue]
    [else
     (define dry-run? (member "--dry-run" args))
     (cond
       [dry-run?
        (define transcript (ui-state-transcript state))
        (define entry-count (length transcript))
        (define msg
          (format "[compact dry-run: ~a transcript entries would be compacted]" entry-count))
        (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state (make-system-entry msg)))
        'continue]
       [else
        (define entry (make-system-entry "[compact requested]"))
        (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
        (when (cmd-ctx-event-bus cctx)
          (publish! (cmd-ctx-event-bus cctx)
                    (make-event "compact.requested"
                                (inexact->exact (truncate (/ (current-inexact-milliseconds) 1000)))
                                (or (ui-state-session-id state) "")
                                #f
                                (hash))))
        'continue])]))

;; Handle /interrupt — interrupt current operation
(define (handle-interrupt-command cctx state)
  (when (cmd-ctx-event-bus cctx)
    (publish! (cmd-ctx-event-bus cctx)
              (make-event "interrupt.requested"
                          (inexact->exact (truncate (/ (current-inexact-milliseconds) 1000)))
                          (or (ui-state-session-id state) "")
                          #f
                          (hash))))
  (define entry (make-system-entry "[interrupt requested]"))
  (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state entry))
  'continue)

;; Handle /retry — resubmit last prompt
(define (handle-retry-command cctx state)
  (define last-prompt (unbox (cmd-ctx-last-prompt-box cctx)))
  (cond
    [last-prompt
     (define entry
       (make-entry 'system (format "[retry: resubmitting]") (current-inexact-milliseconds) (hash)))
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
                               (publish! bus
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
  'continue)

;; Handle /quit
(define (handle-quit-command cctx)
  (set-box! (cmd-ctx-running-box cctx) #f)
  'quit)

;; Handle /login — OAuth2 login flow
(define (handle-login-command cctx state [args '()])
  (cond
    [(not (oauth-available?))
     (set-box! (cmd-ctx-state-box cctx)
               (add-transcript-entry state
                                     (make-error-entry "OAuth not available. Check configuration.")))
     'continue]
    [else
     (define provider-name
       (if (and (pair? args) (not (string=? (car args) "")))
           (car args)
           #f))
     (define cfg (and provider-name (get-oauth-config provider-name)))
     (cond
       [(and provider-name (not cfg))
        (set-box! (cmd-ctx-state-box cctx)
                  (add-transcript-entry state
                                        (make-error-entry (format "Unknown OAuth provider: ~a"
                                                                  provider-name))))
        'continue]
       [(and cfg (not (valid-oauth-config? cfg)))
        (set-box!
         (cmd-ctx-state-box cctx)
         (add-transcript-entry
          state
          (make-error-entry
           (format "OAuth provider ~a not configured — missing client ID. Use API key auth instead."
                   provider-name))))
        'continue]
       [else
        ;; Show "logging in..." message
        (define wait-msg
          (if provider-name
              (format "[logging in to ~a...]" provider-name)
              "[login: use /login <provider> to start OAuth flow]"))
        (set-box! (cmd-ctx-state-box cctx) (add-transcript-entry state (make-system-entry wait-msg)))
        (when cfg
          ;; Start OAuth flow in background thread
          (thread
           (lambda ()
             (with-handlers ([exn:fail? (lambda (e)
                                          (set-box! (cmd-ctx-state-box cctx)
                                                    (add-transcript-entry
                                                     (unbox (cmd-ctx-state-box cctx))
                                                     (make-error-entry (format "Login failed: ~a"
                                                                               (exn-message e)))))
                                          (set-box! (cmd-ctx-needs-redraw-box cctx) #t))])
               (define-values (port state-param verifier get-code)
                 (start-callback-server #:timeout 120))
               (define challenge (hash-base64url verifier))
               (define auth-url
                 (oauth-authorize-url (struct-copy oauth-config cfg [redirect-port port])
                                      state-param
                                      challenge))
               ;; Open browser
               (launch-browser auth-url)
               ;; Wait for callback
               (define code (get-code))
               (cond
                 [code
                  (define tok (oauth-exchange-code cfg code #:code-verifier verifier))
                  (cond
                    [tok
                     (store-oauth-token! (or provider-name "default") tok)
                     (set-box! (cmd-ctx-state-box cctx)
                               (add-transcript-entry
                                (unbox (cmd-ctx-state-box cctx))
                                (make-system-entry (format "[login successful: ~a]"
                                                           (or provider-name "default")))))
                     (set-box! (cmd-ctx-needs-redraw-box cctx) #t)]
                    [else
                     (set-box! (cmd-ctx-state-box cctx)
                               (add-transcript-entry
                                (unbox (cmd-ctx-state-box cctx))
                                (make-error-entry "Login failed: token exchange returned no token.")))
                     (set-box! (cmd-ctx-needs-redraw-box cctx) #t)])]
                 [else
                  (set-box! (cmd-ctx-state-box cctx)
                            (add-transcript-entry
                             (unbox (cmd-ctx-state-box cctx))
                             (make-error-entry "Login timed out or was denied.")))
                  (set-box! (cmd-ctx-needs-redraw-box cctx) #t)])))))
        'continue])]))

;; Get OAuth config for a known provider.
;; Returns oauth-config or #f.
(define (get-oauth-config provider-name)
  (define known-providers
    (hash "openai"
          (oauth-config "https://auth0.openai.com/authorize"
                        "https://auth0.openai.com/oauth/token"
                        #f
                        ""
                        '("openid" "email" "profile")
                        8089)
          "anthropic"
          (oauth-config "https://console.anthropic.com/oauth/authorize"
                        "https://console.anthropic.com/oauth/token"
                        #f
                        ""
                        '("openid")
                        8089)
          "google"
          (oauth-config "https://accounts.google.com/o/oauth2/v2/auth"
                        "https://oauth2.googleapis.com/token"
                        #f
                        ""
                        '("openid" "email" "profile")
                        8089)
          "openrouter"
          (oauth-config "https://openrouter.ai/auth"
                        "https://openrouter.ai/api/v1/auth/token"
                        #f
                        ""
                        '("openid")
                        8089)))
  (hash-ref known-providers provider-name #f))

;; Open browser cross-platform using subprocess (no shell string injection).
;; The url is passed as a direct argv argument, never through shell interpolation.
;; Returns #t on success, #f on failure (with error logged).
(define (open-browser url)
  (define cmd+args
    (case (system-type 'os)
      [(macosx) (list "open" url)]
      [(unix) (list "xdg-open" url)]
      ;; Windows: use powershell Start-Process for safe argv passing.
      ;; Avoids cmd /c start which can misinterpret URLs.
      [(windows) (list "powershell" "-Command" (format "Start-Process '~a'" url))]
      [else #f]))
  (if (not cmd+args)
      #f
      (with-handlers ([exn:fail? (lambda (e)
                                   ((error-display-handler) (format "open-browser failed: ~a"
                                                                    (exn-message e)))
                                   #f)])
        (define-values (sp out in err) (apply subprocess #f #f #f (car cmd+args) (cdr cmd+args)))
        (close-output-port in)
        (close-input-port out)
        (close-input-port err)
        (define status (subprocess-status sp))
        (subprocess-wait sp)
        (eq? (subprocess-status sp) 'running))))

;; Injectable browser launcher for testing.
;; When set, called instead of open-browser.
(define current-browser-launcher (make-parameter #f))

;; Launch browser or test mock.
(define (launch-browser url)
  (define launcher (current-browser-launcher))
  (if launcher
      (launcher url)
      (open-browser url)))

(provide handle-compact-command
         handle-interrupt-command
         handle-retry-command
         handle-quit-command
         open-browser
         handle-login-command
         get-oauth-config
         current-browser-launcher
         launch-browser)
