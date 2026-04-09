#lang racket

;; interfaces/sdk.rkt — embeddable library surface
;;
;; Thin, stable wrapper around runtime/session orchestration.
;; Every SDK function delegates to an existing runtime module.
;; No business logic duplication.
;;
;; Key design:
;;   - Immutable runtime: functions return new runtime structs
;;   - Event bus passthrough: subscribe-events! wraps event bus directly
;;   - Graceful errors: no-session → return 'no-active-session, not crash
;;   - Configurable defaults: make-runtime provides sensible defaults

(require "../llm/provider.rkt"
         (only-in "../tools/tool.rkt"
                  make-tool-registry
                  tool-registry?)
         "../agent/event-bus.rkt"
         "../agent/types.rkt"
         (prefix-in session: "../runtime/agent-session.rkt")
         (only-in "../runtime/compactor.rkt"
                  compact-history compact-and-persist!
                  compaction-result-removed-count compaction-result?)
         (only-in "../runtime/settings.rkt"
                  default-session-dir)
         (only-in "../extensions/api.rkt"
                  list-extensions)
         "../util/cancellation.rkt")

(provide
 ;; Structs
 (struct-out runtime-config)
 (struct-out runtime)
 runtime?

 ;; Cancellation token
 make-cancellation-token
 cancellation-token?
 cancellation-token-cancelled?
 cancel-token!

 ;; API
 make-runtime
 open-session
 run-prompt!
 subscribe-events!
 interrupt!
 fork-session!
 compact-session!
 session-info

 ;; Compaction types
 compaction-result?)

;; ============================================================
;; Cancellation token — imported from util/cancellation.rkt
;; ============================================================
;; (struct, constructor, predicate, cancelled?, cancel! all come from
;;  the shared module to avoid upward dependencies from runtime layers)

;; ============================================================
;; Structs
;; ============================================================

;; Runtime configuration — all the components needed to run
(struct runtime-config
  (provider           ; provider? — the LLM provider
   tool-registry      ; tool-registry? — registered tools
   extension-registry ; extension-registry? or #f
   event-bus          ; event-bus? — shared event bus
   session-dir        ; path — base directory for sessions
   model-name         ; string or #f — default model
   max-iterations     ; integer — max tool-call loops (default 10)
   system-instructions ; (listof string) — injected system prompts
   token-budget-threshold) ; integer — token budget warning threshold
  #:transparent)

;; Runtime handle — wraps all configured components + active session
;; Uses #:constructor-name to avoid collision between runtime-config
;; (the struct) and the auto-generated runtime-config accessor.
(struct runtime (rt-config rt-session rt-cancellation-token)
  #:transparent
  #:constructor-name make-runtime-internal)

;; ============================================================
;; Internal helpers
;; ============================================================

(define (rt-cfg rt) (runtime-rt-config rt))
(define (rt-sess rt) (runtime-rt-session rt))
(define (rt-token rt) (runtime-rt-cancellation-token rt))

;; ============================================================
;; make-runtime
;; ============================================================

(define (make-runtime #:provider provider
                       #:session-dir [session-dir (default-session-dir)]
                       #:tool-registry [tool-registry (make-tool-registry)]
                       #:extension-registry [extension-registry #f]
                       #:event-bus [event-bus (make-event-bus)]
                       #:model-name [model-name #f]
                       #:max-iterations [max-iterations 10]
                       #:system-instructions [system-instructions '()]
                       #:token-budget-threshold [token-budget-threshold 100000]
                       #:cancellation-token [cancellation-token #f])
  (make-runtime-internal
   (runtime-config provider
                   tool-registry
                   extension-registry
                   event-bus
                   session-dir
                   model-name
                   max-iterations
                   system-instructions
                   token-budget-threshold)
   #f
   (or cancellation-token (make-cancellation-token))))

;; ============================================================
;; open-session
;; ============================================================

(define (open-session rt [session-id #f])
  (define cfg (rt-cfg rt))
  (define agent-cfg
    (hasheq 'provider      (runtime-config-provider cfg)
            'tool-registry  (runtime-config-tool-registry cfg)
            'event-bus      (runtime-config-event-bus cfg)
            'session-dir    (runtime-config-session-dir cfg)
            'max-iterations (runtime-config-max-iterations cfg)
            'extension-registry (runtime-config-extension-registry cfg)
            'model-name     (runtime-config-model-name cfg)
            'system-instructions (runtime-config-system-instructions cfg)
            'token-budget-threshold (runtime-config-token-budget-threshold cfg)
            'cancellation-token (rt-token rt)))
  (define sess
    (if session-id
        (session:resume-agent-session session-id agent-cfg)
        (session:make-agent-session agent-cfg)))
  (make-runtime-internal cfg sess (rt-token rt)))

;; ============================================================
;; run-prompt!
;; ============================================================

(define (run-prompt! rt prompt)
  (define sess (rt-sess rt))
  (cond
    [(not sess)
     (values rt 'no-active-session)]
    [else
     (define-values (updated-sess result)
       (session:run-prompt! sess prompt
                          #:max-iterations (runtime-config-max-iterations (rt-cfg rt))))
     (values (make-runtime-internal (rt-cfg rt) updated-sess (rt-token rt))
             result)]))

;; ============================================================
;; subscribe-events!
;; ============================================================

(define (subscribe-events! rt handler [filter #f])
  (define bus (runtime-config-event-bus (rt-cfg rt)))
  (if filter
      (subscribe! bus handler #:filter filter)
      (subscribe! bus handler)))

;; ============================================================
;; interrupt!
;; ============================================================

(define (interrupt! rt)
  (define sess (rt-sess rt))
  (define bus (runtime-config-event-bus (rt-cfg rt)))
  ;; 1. Cancel the token — cooperative signal
  ;; NOTE: The cancellation token is a future-ready hook.
  ;; It is NOT currently consumed by the agent loop (run-prompt!/run-iteration-loop).
  ;; Callers can poll the token for their own cooperative cancellation logic.
  (cancel-token! (rt-token rt))
  ;; 2. Emit event (existing behavior)
  (when sess
    (publish! bus
              (make-event "interrupt.requested"
                          (exact-truncate (/ (current-inexact-milliseconds) 1000))
                          (session:session-id sess)
                          #f
                          (hasheq 'sessionId (session:session-id sess)))))
  rt)

;; ============================================================
;; fork-session!
;; ============================================================

(define (fork-session! rt [entry-id #f])
  (define sess (rt-sess rt))
  (cond
    [(not sess)
     'no-active-session]
    [else
     (define forked (session:fork-session sess entry-id))
     (make-runtime-internal (rt-cfg rt) forked (rt-token rt))]))

;; ============================================================
;; compact-session!
;; ============================================================

;; Compact session history.
;;
;; When #:persist? is #f (default), compaction is advisory-only:
;;   - Returns (values rt compaction-result)
;;   - The session log is NOT modified
;;   - Caller can inspect the result and decide
;;
;; When #:persist? is #t, compaction is persisting:
;;   - Returns (values rt compaction-result)
;;   - The summary entry is appended to the session log
;;   - Original messages remain in the log (append-only)
;;   - Future context assembly can use summary + recent messages

(define (compact-session! rt #:persist? [persist? #f])
  (define sess (rt-sess rt))
  (cond
    [(not sess)
     'no-active-session]
    [else
     (define bus (runtime-config-event-bus (rt-cfg rt)))
     (define sid (session:session-id sess))
     ;; Emit compaction.started
     (publish! bus
               (make-event "compaction.started"
                           (exact-truncate (/ (current-inexact-milliseconds) 1000))
                           sid
                           #f
                           (hasheq 'sessionId sid
                                   'persist? persist?)))
     ;; Get history and compact
     (define history (session:session-history sess))
     (define log-path (build-path (runtime-config-session-dir (rt-cfg rt))
                                    (session:session-id sess)
                                    "session.jsonl"))
     (define compaction-res
       (if persist?
           (compact-and-persist! history log-path)
           (compact-history history)))
     ;; Emit compaction.completed
     (publish! bus
               (make-event "compaction.completed"
                           (exact-truncate (/ (current-inexact-milliseconds) 1000))
                           sid
                           #f
                           (hasheq 'sessionId sid
                                   'persist? persist?
                                   'removedCount (compaction-result-removed-count compaction-res))))
     ;; Return (values runtime compaction-result)
     ;; The caller can use compaction-result->message-list for the compacted view
     (values rt compaction-res)]))

;; ============================================================
;; session-info
;; ============================================================

(define (session-info rt)
  (define sess (rt-sess rt))
  (define cfg (rt-cfg rt))
  (cond
    [(not sess) #f]
    [else
     (hasheq 'session-id        (session:session-id sess)
             'active?           (session:session-active? sess)
             'history-length    (length (session:session-history sess))
             'model-name        (runtime-config-model-name cfg)
             'extension-registry-count (let ([reg (runtime-config-extension-registry cfg)])
                                            (if reg
                                                (length (list-extensions reg))
                                                0))
             'system-instructions (runtime-config-system-instructions cfg)
             'max-iterations (runtime-config-max-iterations cfg)
             'token-budget-threshold (runtime-config-token-budget-threshold cfg))]))
