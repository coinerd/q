#lang racket/base

;; wiring/run-json-rpc.rkt — JSON and RPC mode runners
;;
;; Contains:
;;   - run-json: JSON mode (stdin/stdout intents)
;;   - run-rpc: RPC mode (structured request/response over stdio)

(require json
         "../interfaces/cli.rkt"
         "../interfaces/json-mode.rkt"
         "../interfaces/rpc-mode.rkt"
         (only-in "../runtime/agent-session.rkt"
                  make-agent-session
                  resume-agent-session
                  run-prompt!
                  session-id
                  fork-session
                  close-session!)
         "../agent/event-bus.rkt"
         "../extensions/api.rkt"
         (only-in "../util/cancellation.rkt" cancellation-token? cancel-token!))

(provide run-json
         run-rpc)

;; ============================================================
;; run-json — JSON mode
;; ============================================================

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

;; ============================================================
;; run-rpc — RPC mode
;; ============================================================

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
