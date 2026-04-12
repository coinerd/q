#lang racket/base

;; runtime/cli-builder.rkt — Runtime construction and mode runners
;;
;; Extracted from main.rkt. Contains:
;;   - build-runtime-from-cli: assemble runtime config from CLI args
;;   - load-extensions-from-dir!: helper for extension discovery
;;   - mode-for-config: determine interface mode from cli-config
;;   - make-terminal-subscriber: streaming Markdown event renderer
;;   - Mode runners: run-interactive, run-single-shot, run-resume,
;;     run-json, run-rpc

(require racket/string
         racket/file
         json
         "../interfaces/cli.rkt"
         "../interfaces/sessions.rkt"
         "../interfaces/json-mode.rkt"
         "../interfaces/rpc-mode.rkt"
         (only-in "../runtime/agent-session.rkt"
                  make-agent-session
                  resume-agent-session
                  run-prompt!
                  session-id
                  session-history
                  fork-session
                  close-session!)
         "../runtime/settings.rkt"
         "../skills/types.rkt"
         "../runtime/model-registry.rkt"
         (only-in "../runtime/provider-factory.rkt" build-provider)
         "../tools/tool.rkt"
         (only-in "../tools/registry-defaults.rkt" register-default-tools!)
         "../agent/event-bus.rkt"
         (only-in "../util/protocol-types.rkt"
                  event-ev
                  event-payload
                  message-role
                  message-content
                  text-part?
                  text-part-text)
         "../extensions/api.rkt"
         (only-in "../extensions/loader.rkt" load-extension!)
         (only-in "../util/cancellation.rkt" cancellation-token? cancel-token!))

(provide build-runtime-from-cli
         load-extensions-from-dir!
         mode-for-config
         make-terminal-subscriber
         run-interactive
         run-single-shot
         run-resume
         run-json
         run-rpc)

;; ============================================================
;; build-runtime-from-cli
;; ============================================================

;; Build a complete runtime config hash from a cli-config.
;; Creates event bus, tool registry (with defaults), provider,
;; extension registry, and session directory.
;;
;; The returned hash is suitable for make-agent-session or
;; resume-agent-session.

(define (build-runtime-from-cli cfg)
  (define base-config (cli-config->runtime-config cfg))
  (define bus (make-event-bus))
  (define reg (make-tool-registry))
  ;; Register default tools unless --no-tools
  (unless (cli-config-no-tools? cfg)
    (define only-tools (cli-config-tools cfg))
    (register-default-tools! reg #:only (if (null? only-tools) #f only-tools)))

  ;; Load settings ONCE — for both provider building and session-dir
  (define project-dir (or (hash-ref base-config 'project-dir #f) (current-directory)))
  (define config-path (hash-ref base-config 'config-path #f))
  (define settings (load-settings project-dir #:config-path config-path))

  ;; Load resources (system instructions, skills, templates)
  (define global-resources (load-global-resources))
  (define project-resources (load-project-resources project-dir))
  (define all-resources (merge-resources global-resources project-resources))
  (define system-instrs (resource-set-instructions all-resources))

  ;; Provider uses the shared settings
  (define prov (build-provider base-config settings))

  ;; Session dir: from CLI override, or from settings, or default
  (define session-dir
    (or (hash-ref base-config 'session-dir #f)
        (let ([sd (session-dir-from-settings settings)])
          (if (path? sd)
              (path->string sd)
              sd))))
  (make-directory* session-dir)

  ;; Extension registry — discover from project dir
  (define ext-reg (make-extension-registry))
  (load-extensions-from-dir! ext-reg (build-path project-dir ".q" "extensions") #:event-bus bus)
  (load-extensions-from-dir! ext-reg (build-path project-dir ".pi" "extensions") #:event-bus bus)

  ;; Model name from CLI --model flag
  (define model-name (hash-ref base-config 'model #f))

  ;; Build final config: merge CLI options with runtime components
  (hash-set! base-config 'provider prov)
  (hash-set! base-config 'tool-registry reg)
  (hash-set! base-config 'event-bus bus)
  (hash-set! base-config 'session-dir session-dir)
  (hash-set! base-config 'extension-registry ext-reg)
  ;; Resolve effective model name: CLI flag > settings default
  ;; Also persist model-registry for /model command
  (define model-reg (make-model-registry-from-config (q-settings-merged settings)))
  (hash-set! base-config 'model-registry model-reg)
  (define effective-model-name (or model-name (default-model model-reg)))
  (hash-set! base-config 'model-name effective-model-name)
  (hash-set! base-config 'system-instructions system-instrs)
  (hash-set! base-config 'verbose? (cli-config-verbose? cfg))
  base-config)

;; ============================================================
;; load-extensions-from-dir! helper
;; ============================================================

;; Load all .rkt extension files from a directory into the registry.
;; Silently skips if directory doesn't exist or individual loads fail.

(define (load-extensions-from-dir! ext-reg dir #:event-bus [event-bus #f])
  (when (directory-exists? dir)
    (define files
      (filter (λ (f) (regexp-match? #rx"\\.rkt$" (path->string f))) (directory-list dir #:build? #t)))
    (for ([f (in-list files)])
      (with-handlers ([exn:fail? (λ (e)
                                   (fprintf (current-error-port)
                                            "Warning: failed to load extension ~a: ~a\n"
                                            f
                                            (exn-message e)))])
        (load-extension! ext-reg f #:event-bus event-bus)))))

;; ============================================================
;; mode-for-config
;; ============================================================

;; Determine which interface mode to run based on cli-config.
;; Returns a symbol: 'interactive | 'single | 'json | 'rpc | 'tui | 'help | 'version

(define (mode-for-config cfg)
  (define cmd (cli-config-command cfg))
  (case cmd
    [(help) 'help]
    [(version) 'version]
    [(doctor) 'doctor]
    [(sessions) 'sessions]
    [else (cli-config-mode cfg)]))

;; ============================================================
;; Mode dispatch helpers
;; ============================================================

;; Create a terminal event subscriber with line-buffered Markdown rendering.
;; Tracks stream state per-turn: deltas go through the Markdown writer,
;; non-delta events are printed as before.

(define (make-terminal-subscriber)
  (define-values (writer flush!) (make-stream-markdown-writer))
  (lambda (evt)
    (define ev (event-ev evt))
    (cond
      [(equal? ev "model.stream.delta")
       (define delta (hash-ref (event-payload evt) 'delta ""))
       (when (> (string-length delta) 0)
         (writer delta))]
      [(equal? ev "model.stream.completed")
       (flush!)
       ;; Reset writer for next turn
       (define-values (new-writer new-flush!) (make-stream-markdown-writer))
       (set! writer new-writer)
       (set! flush! new-flush!)]
      [(equal? ev "turn.started")
       ;; Reset writer at turn start
       (define-values (new-writer new-flush!) (make-stream-markdown-writer))
       (set! writer new-writer)
       (set! flush! new-flush!)]
      [else
       (define text (format-event-for-terminal evt))
       (unless (string=? text "")
         (displayln text))])))

;; Handle /sessions interactive command
(define (handle-sessions-interactive-command cmd out session-dir)
  (define sdir (or session-dir (default-session-dir)))
  (cond
    [(or (equal? cmd '(sessions)) (equal? cmd '(sessions list)))
     (define sess-list (sessions-list sdir #:limit 10))
     (for-each displayln (sessions-list->strings sess-list))]
    [(and (list? cmd) (>= (length cmd) 3) (equal? (cadr cmd) 'info))
     (define sid (caddr cmd))
     (define info (sessions-info sdir sid))
     (displayln (sessions-info->string info))]
    [(and (list? cmd) (>= (length cmd) 3) (equal? (cadr cmd) 'delete))
     (define sid (caddr cmd))
     (define result (sessions-delete sdir sid #:confirm? #t #:out out))
     (case result
       [(ok) (displayln (format "Session ~a deleted." sid))]
       [(not-found) (displayln (format "Session not found: ~a" sid))]
       [(cancelled) (displayln "Cancelled.")])]
    [else (displayln "Usage: /sessions [list|info <id>|delete <id>]")]))

(define (run-interactive cfg rt-config #:provider-name [prov-name #f])
  (define sess (make-agent-session rt-config))
  (define bus (hash-ref rt-config 'event-bus))
  (subscribe! bus (make-terminal-subscriber))
  (run-cli-interactive
   cfg
   #:session-fn (lambda (prompt) (run-prompt! sess prompt))
   #:compact-fn (lambda () (displayln "[compaction requested]"))
   #:history-fn (lambda ([out (current-output-port)])
                  (define hist (session-history sess))
                  (for ([msg (in-list hist)])
                    (define role (message-role msg))
                    (define text-parts (filter text-part? (message-content msg)))
                    (define text (string-join (map text-part-text text-parts) " "))
                    (displayln (format "[~a] ~a" role text) out)))
   #:fork-fn (lambda (entry-id)
               (define new-sess (fork-session sess entry-id))
               (displayln (format "[forked session: ~a]" (session-id new-sess))))
   #:model-fn
   (lambda (arg)
     (define reg (hash-ref rt-config 'model-registry #f))
     (cond
       [(not reg) (displayln "[model registry not available]")]
       [(not arg)
        (displayln "Available models:")
        (for ([m (in-list (available-models reg))])
          (define marker (if (equal? (model-entry-name m) (default-model reg)) " *" "  "))
          (displayln
           (format "~a ~a (~a)" marker (model-entry-name m) (model-entry-provider-name m))))]
       [else
        (define resolution (resolve-model reg arg))
        (if resolution
            (displayln (format "[switched to model: ~a (provider: ~a)]"
                               (model-resolution-model-name resolution)
                               (model-resolution-provider-name resolution)))
            (displayln (format "Model not found: ~a. Use /model to list." arg)))]))
   #:sessions-fn (lambda (cmd out)
                   (define sdir (hash-ref rt-config 'session-dir #f))
                   (handle-sessions-interactive-command cmd out sdir))
   #:provider-name prov-name))

(define (run-single-shot cfg rt-config)
  (define sess (make-agent-session rt-config))
  (define bus (hash-ref rt-config 'event-bus))
  (subscribe! bus (make-terminal-subscriber))
  (run-cli-single cfg #:session-fn (lambda (prompt) (run-prompt! sess prompt))))

(define (run-resume cfg rt-config)
  (define sid (cli-config-session-id cfg))
  (define sess (resume-agent-session sid rt-config))
  (define bus (hash-ref rt-config 'event-bus))
  (subscribe! bus (make-terminal-subscriber))
  (run-cli-interactive
   cfg
   #:session-fn (lambda (prompt) (run-prompt! sess prompt))
   #:compact-fn (lambda () (displayln "[compaction requested]"))
   #:history-fn (lambda ([out (current-output-port)])
                  (define hist (session-history sess))
                  (for ([msg (in-list hist)])
                    (define role (message-role msg))
                    (define text-parts (filter text-part? (message-content msg)))
                    (define text (string-join (map text-part-text text-parts) " "))
                    (displayln (format "[~a] ~a" role text) out)))
   #:fork-fn (lambda (entry-id)
               (define new-sess (fork-session sess entry-id))
               (displayln (format "[forked session: ~a]" (session-id new-sess))))
   #:model-fn
   (lambda (arg)
     (define reg (hash-ref rt-config 'model-registry #f))
     (cond
       [(not reg) (displayln "[model registry not available]")]
       [(not arg)
        (displayln "Available models:")
        (for ([m (in-list (available-models reg))])
          (define marker (if (equal? (model-entry-name m) (default-model reg)) " *" "  "))
          (displayln
           (format "~a ~a (~a)" marker (model-entry-name m) (model-entry-provider-name m))))]
       [else
        (define resolution (resolve-model reg arg))
        (if resolution
            (displayln (format "[switched to model: ~a (provider: ~a)]"
                               (model-resolution-model-name resolution)
                               (model-resolution-provider-name resolution)))
            (displayln (format "Model not found: ~a. Use /model to list." arg)))]))
   #:sessions-fn (lambda (cmd out)
                   (define sdir (hash-ref rt-config 'session-dir #f))
                   (handle-sessions-interactive-command cmd out sdir))))

(define (run-json cfg rt-config)
  (define sess (make-agent-session rt-config))
  (define bus (hash-ref rt-config 'event-bus))
  (define sid (session-id sess))
  (define sub-id (start-json-mode! bus #:session-id sid))
  ;; Simple JSON mode: read intents from stdin, submit prompts
  (let loop ()
    (define line (read-line))
    (unless (eof-object? line)
      (define intent-obj (parse-json-intent line))
      (when intent-obj
        (case (intent-type intent-obj)
          [(prompt)
           (define text (hash-ref (intent-payload intent-obj) 'text #f))
           (when text
             (run-prompt! sess text))]
          [(interrupt)
           ;; R2-4: Cancel the cancellation token if available
           (define cancel-tok (hash-ref rt-config 'cancellation-token #f))
           (when (and cancel-tok (cancellation-token? cancel-tok))
             (cancel-token! cancel-tok))
           (displayln (jsexpr->string (hasheq 'type "interrupt" 'status "acknowledged")))]
          ;; R2-4: Compact session
          [(compact) (displayln (jsexpr->string (hasheq 'type "compact" 'status "requested")))]
          [(fork)
           ;; R2-4: Fork session
           (define entry-id
             (and (intent-payload intent-obj) (hash-ref (intent-payload intent-obj) 'entryId #f)))
           (define new-sess (fork-session sess entry-id))
           (displayln (jsexpr->string
                       (hasheq 'type "fork" 'status "ok" 'newSessionId (session-id new-sess))))]
          [(quit)
           (stop-json-mode! bus sub-id)
           (void)]
          [else (void)]))
      (unless (and intent-obj (eq? (intent-type intent-obj) 'quit))
        (loop)))))

(define (run-rpc cfg rt-config)
  (define bus (hash-ref rt-config 'event-bus))
  ;; Forward events as RPC notifications
  (start-rpc-event-forwarding! bus (current-output-port))
  ;; Sessions table: session-id → agent-session
  (define sessions (make-hash))
  ;; Auto-open a default session for backward compatibility
  (define default-sess (make-agent-session rt-config))
  (hash-set! sessions (session-id default-sess) default-sess)
  ;; Build handlers
  (define handlers
    (make-hash ;; ---- Prompt ----
     (list (cons 'prompt
                 (lambda (params)
                   (define text (hash-ref params 'text ""))
                   (define sid (hash-ref params 'sessionId #f))
                   (define sess
                     (if sid
                         (hash-ref sessions sid #f)
                         default-sess))
                   (unless sess
                     (error 'prompt "session not found: ~a" sid))
                   (run-prompt! sess text)
                   (hasheq 'status "ok")))
           ;; ---- Ping ----
           (cons 'ping (lambda (params) (hasheq 'pong #t)))
           ;; ---- Shutdown ----
           (cons 'shutdown (lambda (params) (hasheq 'status "shutting-down")))
           ;; ---- Session management ----
           (cons 'session.open
                 (lambda (params)
                   (define sess (make-agent-session rt-config))
                   (hash-set! sessions (session-id sess) sess)
                   (hasheq 'sessionId (session-id sess))))
           (cons 'session.list
                 (lambda (params)
                   (define session-dir (hash-ref rt-config 'session-dir))
                   (define session-ids
                     (if (directory-exists? session-dir)
                         (for/list ([e (in-list (directory-list session-dir))]
                                    #:when (directory-exists? (build-path session-dir e)))
                           (path->string e))
                         '()))
                   (hasheq 'sessions session-ids)))
           (cons 'session.resume
                 (lambda (params)
                   (define sid (hash-ref params 'sessionId #f))
                   (unless sid
                     (error 'session.resume "sessionId parameter required"))
                   (define sess (resume-agent-session sid rt-config))
                   (hash-set! sessions (session-id sess) sess)
                   (hasheq 'sessionId (session-id sess) 'status "resumed")))
           (cons 'session.close
                 (lambda (params)
                   (define sid (hash-ref params 'sessionId #f))
                   (unless sid
                     (error 'session.close "sessionId parameter required"))
                   (define sess (hash-ref sessions sid #f))
                   (unless sess
                     (error 'session.close "session not found: ~a" sid))
                   (close-session! sess)
                   (hash-remove! sessions sid)
                   (hasheq 'sessionId sid 'status "closed"))))))
  (run-rpc-loop handlers))
