#lang racket/base

;; wiring/run-json-rpc.rkt — JSON and RPC mode runners
;;
;; Contains:
;;   - run-json: JSON mode (stdin/stdout intents)
;;   - run-rpc: RPC mode (structured request/response over stdio)

(require json
         racket/hash
         "../interfaces/cli.rkt"
         "../interfaces/json-mode.rkt"
         "../interfaces/rpc-mode.rkt"
         (only-in "../runtime/agent-session.rkt"
                  make-agent-session
                  resume-agent-session
                  run-prompt!
                  session-id
                  fork-session
                  close-session!
                  agent-session-queue)
         "../agent/event-bus.rkt"
         "../extensions/api.rkt"
         "./rpc-methods.rkt"
         "./rpc-ui-adapter.rkt"
         (only-in "../agent/queue.rkt" enqueue-steering! enqueue-followup!)
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
  ;; ---- Build deps for make-core-rpc-handlers ----
  (define core-deps
    (hasheq 'cancel-token
            (lambda ()
              (define tok (hash-ref rt-config 'cancellation-token #f))
              (when (and tok (cancellation-token? tok))
                (cancel-token! tok)))
            'session-info-fn
            (lambda () (hasheq 'session-id (session-id default-sess) 'active? #t))
            'compact-fn
            (lambda () (hasheq 'status 'not-implemented))
            'fork-fn
            (lambda (entry-id)
              (define new-sess (fork-session default-sess entry-id))
              (hash-set! sessions (session-id new-sess) new-sess)
              (hasheq 'newSessionId (session-id new-sess) 'status "ok"))
            'subscribe-fn
            (lambda (filter)
              (subscribe! bus
                          (lambda (evt) (displayln (jsexpr->string evt)))
                          #:filter
                          (and filter (lambda (evt) (member (hash-ref evt 'type #f) filter)))))
            'prompt-fn
            (lambda (msg)
              (run-prompt! default-sess msg)
              (hasheq 'status "ok"))
            'steer-fn
            (lambda (msg)
              (enqueue-steering! (agent-session-queue default-sess) msg)
              (hasheq 'status "steered"))
            'follow-up-fn
            (lambda (msg)
              (enqueue-followup! (agent-session-queue default-sess) msg)
              (hasheq 'status "queued"))
            'navigate-fn
            (lambda (target) (hasheq 'status 'not-implemented))
            'send-message-fn
            (lambda (role text) (hasheq 'status 'not-implemented))))

  ;; ---- Core handlers from make-core-rpc-handlers ----
  (define core-handlers (make-core-rpc-handlers core-deps))

  ;; ---- Session management + utility handlers ----
  (define session-handlers
    (make-hash (list (cons 'ping (lambda (params) (hasheq 'pong #t)))
                     (cons 'shutdown (lambda (params) (hasheq 'status "shutting-down")))
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

  ;; ---- #1185: Extension UI bridge ----
  ;; Create a UI channel for extensions and wire it to RPC notifications
  (define ui-ch (make-channel))
  (hash-set! rt-config 'ui-channel ui-ch)
  (start-rpc-ui-bridge! ui-ch (current-output-port))
  (define ui-response-handler (make-rpc-ui-response-handler ui-ch))

  ;; ---- Merge core + session + UI handlers ----
  (define ui-handlers
    (make-hash (list (cons 'ui.respond ui-response-handler)
                     (cons 'ui_response ui-response-handler))))
  (define handlers (hash-union core-handlers session-handlers ui-handlers #:combine (lambda (a b) b)))
  (run-rpc-loop handlers))
