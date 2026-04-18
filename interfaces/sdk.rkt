#lang racket/base

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

(require racket/contract
         racket/math
         "../llm/provider.rkt"
         (only-in "../tools/tool.rkt" make-tool-registry tool-registry?)
         "../agent/event-bus.rkt"
         "../util/protocol-types.rkt"
         (prefix-in session: "../runtime/agent-session.rkt")
         (only-in "../runtime/compactor.rkt"
                  compact-history
                  compact-and-persist!
                  compaction-result-removed-count
                  compaction-result?)
         (only-in "../runtime/settings.rkt" default-session-dir)
         (only-in "../llm/token-budget.rkt"
                  DEFAULT-TOKEN-BUDGET-THRESHOLD
                  estimate-context-tokens
                  get-context-usage
                  context-usage?
                  context-usage-total-tokens
                  context-usage-max-tokens
                  context-usage-usage-percent
                  context-usage-compaction-threshold
                  context-usage-near-threshold?)
         (only-in "../extensions/api.rkt" list-extensions)
         (only-in "../agent/queue.rkt" enqueue-steering! enqueue-followup!)
         (only-in "../runtime/session-index.rkt"
                  navigate-to-entry!
                  navigate-next-leaf!
                  navigate-prev-leaf!
                  navigate-result?)
         (only-in "../runtime/session-store.rkt"
                  make-in-memory-session-manager
                  in-memory-session-manager?
                  in-memory-append!
                  in-memory-append-entries!
                  in-memory-load
                  in-memory-list-sessions
                  in-memory-fork!)
         "../util/cancellation.rkt")

;; Structs
(provide (struct-out runtime-config)
         (struct-out runtime)
         runtime?

         ;; Cancellation token
         make-cancellation-token
         cancellation-token?
         cancellation-token-cancelled?
         cancel-token!

         ;; API
         (contract-out [make-runtime
                        (->* (#:provider any/c)
                             (#:session-dir (or/c path-string? path? #f)
                                            #:tool-registry any/c
                                            #:extension-registry any/c
                                            #:event-bus any/c
                                            #:model-name (or/c string? #f)
                                            #:max-iterations exact-positive-integer?
                                            #:system-instructions (listof string?)
                                            #:token-budget-threshold exact-positive-integer?
                                            #:cancellation-token any/c)
                             runtime?)]
                       [open-session (->* (runtime?) ((or/c string? #f)) runtime?)]
                       [run-prompt! (runtime? string? . -> . (values runtime? any/c))]
                       [subscribe-events!
                        (->* (runtime? procedure?) ((or/c procedure? #f)) exact-nonnegative-integer?)]
                       [interrupt! (runtime? . -> . runtime?)]
                       [fork-session!
                        (->* (runtime?) ((or/c string? #f)) (or/c runtime? 'no-active-session))]
                       [compact-session! (->* (runtime?) (#:persist? boolean?) any)]
                       [session-info (runtime? . -> . (or/c #f hash?))]
                       [steer! (runtime? string? . -> . runtime?)]
                       [follow-up! (runtime? string? . -> . runtime?)]
                       [navigate!
                        (-> runtime?
                            (or/c string? exact-integer?)
                            (or/c navigate-result? 'no-active-session 'invalid-target))])

         ;; Compaction types
         compaction-result?

         ;; Navigation types
         navigate-result?

         ;; In-memory session manager (GC-18)
         make-in-memory-session-manager
         in-memory-session-manager?
         in-memory-append!
         in-memory-append-entries!
         in-memory-load
         in-memory-list-sessions
         in-memory-fork!

         ;; Unified factory (#1152)
         create-agent-session

         ;; Thinking level (#1153) — re-exported from agent-session
         session:thinking-levels
         session:thinking-level?
         session:thinking-level->budget
         session:agent-session-thinking-level
         session:set-thinking-level!

         ;; Context usage (#1154) — re-exported from token-budget
         context-usage?
         context-usage-total-tokens
         context-usage-max-tokens
         context-usage-usage-percent
         context-usage-compaction-threshold
         get-context-usage
         context-usage-near-threshold?

         ;; #1196: Enriched SDK API aliases
         q:create-session
         q:session-send
         q:session-subscribe
         q:session-interrupt
         q:session-fork
         q:session-compact
         q:session-info)

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
        (provider ; provider? — the LLM provider
         tool-registry ; tool-registry? — registered tools
         extension-registry ; extension-registry? or #f
         event-bus ; event-bus? — shared event bus
         session-dir ; path — base directory for sessions
         model-name ; string or #f — default model
         max-iterations ; integer — max tool-call loops (default 10)
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

(define (rt-cfg rt)
  (runtime-rt-config rt))
(define (rt-sess rt)
  (runtime-rt-session rt))
(define (rt-token rt)
  (runtime-rt-cancellation-token rt))

;; ============================================================
;; make-runtime
;; ============================================================

;;; make-runtime : #:provider any/c [#:session-dir ...] [#:tool-registry ...]
;;;               ... -> runtime?
;;;
;;; Constructs a runtime handle with sensible defaults. Creates a new tool
;;; registry, event bus, session directory, and cancellation token if not
;;; provided. The runtime wraps all components needed for agent execution.
(define (make-runtime #:provider provider
                      #:session-dir [session-dir (default-session-dir)]
                      #:tool-registry [tool-registry (make-tool-registry)]
                      #:extension-registry [extension-registry #f]
                      #:event-bus [event-bus (make-event-bus)]
                      #:model-name [model-name #f]
                      #:max-iterations [max-iterations 10]
                      #:system-instructions [system-instructions '()]
                      #:token-budget-threshold [token-budget-threshold DEFAULT-TOKEN-BUDGET-THRESHOLD]
                      #:cancellation-token [cancellation-token #f])
  (make-runtime-internal (runtime-config provider
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

;;; open-session : runtime? [(or/c string? #f)] -> runtime?
;;;
;;; Creates a new agent session or resumes an existing one by ID.
;;; Returns a new runtime with the session attached. If session-id is
;;; #f, creates a fresh session; otherwise resumes the session with that ID.
(define (open-session rt [session-id #f])
  (define cfg (rt-cfg rt))
  (define agent-cfg
    (hasheq 'provider
            (runtime-config-provider cfg)
            'tool-registry
            (runtime-config-tool-registry cfg)
            'event-bus
            (runtime-config-event-bus cfg)
            'session-dir
            (runtime-config-session-dir cfg)
            'max-iterations
            (runtime-config-max-iterations cfg)
            'extension-registry
            (runtime-config-extension-registry cfg)
            'model-name
            (runtime-config-model-name cfg)
            'system-instructions
            (runtime-config-system-instructions cfg)
            'token-budget-threshold
            (runtime-config-token-budget-threshold cfg)
            'cancellation-token
            (rt-token rt)))
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
    [(not sess) (values rt 'no-active-session)]
    [else
     (define-values (updated-sess result)
       (session:run-prompt! sess prompt #:max-iterations (runtime-config-max-iterations (rt-cfg rt))))
     (values (make-runtime-internal (rt-cfg rt) updated-sess (rt-token rt)) result)]))

;; ============================================================
;; subscribe-events!
;; ============================================================

;;; subscribe-events! : (runtime? procedure? [(or/c procedure? #f)])
;;;                     -> exact-nonnegative-integer?
;;;
;;; Registers an event handler on the runtime's event bus. Optional filter
;;; predicate narrows which events are delivered. Returns subscription ID
;;; for later unsubscription.
(define (subscribe-events! rt handler [filter #f])
  (define bus (runtime-config-event-bus (rt-cfg rt)))
  (if filter
      (subscribe! bus handler #:filter filter)
      (subscribe! bus handler)))

;; ============================================================
;; interrupt!
;; ============================================================

;;; interrupt! : runtime? -> runtime?
;;; Cancels the runtime's cancellation token and emits an interrupt.requested
;;; event. Returns the same runtime for chaining.
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

;;; fork-session! : (runtime? [(or/c string? #f)])
;;;               -> (or/c runtime? 'no-active-session)
;;;
;;; Forks the active session, optionally at a specific entry ID. Returns
;;; a new runtime with the forked session, or 'no-active-session if no
;;; session is currently open.
(define (fork-session! rt [entry-id #f])
  (define sess (rt-sess rt))
  (cond
    [(not sess) 'no-active-session]
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

;;; compact-session! : (runtime? [#:persist? boolean?]) -> any
;;;
;;; Compacts session history. With #:persist? #f (default) is advisory-only;
;;; with #:persist? #t appends a summary entry to the session log.
;;; Returns (values rt compaction-result).
(define (compact-session! rt #:persist? [persist? #f])
  (define sess (rt-sess rt))
  (cond
    [(not sess) 'no-active-session]
    [else
     (define bus (runtime-config-event-bus (rt-cfg rt)))
     (define sid (session:session-id sess))
     ;; Emit compaction.started
     (publish! bus
               (make-event "compaction.started"
                           (exact-truncate (/ (current-inexact-milliseconds) 1000))
                           sid
                           #f
                           (hasheq 'sessionId sid 'persist? persist?)))
     ;; Get history and compact
     (define history (session:session-history sess))
     (define log-path
       (build-path (runtime-config-session-dir (rt-cfg rt))
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
                           (hasheq 'sessionId
                                   sid
                                   'persist?
                                   persist?
                                   'removedCount
                                   (compaction-result-removed-count compaction-res))))
     ;; Return (values runtime compaction-result)
     ;; The caller can use compaction-result->message-list for the compacted view
     (values rt compaction-res)]))

;; ============================================================
;; session-info
;; ============================================================

;;; session-info : runtime? -> (or/c #f hash?)
;;;
;;; Returns a hash with session metadata (session-id, active?, history-length,
;;; model-name, extension-registry-count, system-instructions, max-iterations,
;;; token-budget-threshold), or #f if no session is open.
(define (session-info rt)
  (define sess (rt-sess rt))
  (define cfg (rt-cfg rt))
  (cond
    [(not sess) #f]
    [else
     (hasheq 'session-id
             (session:session-id sess)
             'active?
             (session:session-active? sess)
             'history-length
             (length (session:session-history sess))
             'model-name
             (runtime-config-model-name cfg)
             'extension-registry-count
             (let ([reg (runtime-config-extension-registry cfg)])
               (if reg
                   (length (list-extensions reg))
                   0))
             'system-instructions
             (runtime-config-system-instructions cfg)
             'max-iterations
             (runtime-config-max-iterations cfg)
             'token-budget-threshold
             (runtime-config-token-budget-threshold cfg))]))

;; ============================================================
;; steer!
;; ============================================================

;;; steer! : runtime? string? -> runtime?
;;; Enqueues a steering message into the session's agent queue for
;;; mid-turn guidance. No-op if no session is active.
(define (steer! rt message)
  (define sess (rt-sess rt))
  (cond
    [(not sess) rt]
    [else
     (define q (session:agent-session-queue sess))
     (enqueue-steering! q message)
     rt]))

;; ============================================================
;; follow-up!
;; ============================================================

;;; follow-up! : runtime? string? -> runtime?
;;; Enqueues a follow-up message into the session's agent queue for
;;; post-turn continuation. No-op if no session is active.
(define (follow-up! rt message)
  (define sess (rt-sess rt))
  (cond
    [(not sess) rt]
    [else
     (define q (session:agent-session-queue sess))
     (enqueue-followup! q message)
     rt]))

;; ============================================================
;; navigate!
;; ============================================================

;;; navigate! : runtime? (or/c string? exact-integer?)
;;;           -> (or/c navigate-result? 'no-active-session 'invalid-target)
;;;
;;; Navigate the session tree. String target = entry ID;
;;; positive integer = next leaf; negative integer = prev leaf.
(define (navigate! rt target)
  (define sess (rt-sess rt))
  (cond
    [(not sess) 'no-active-session]
    [else
     (define idx (session:agent-session-index sess))
     (cond
       [(not idx) 'no-active-session]
       [(string? target) (or (navigate-to-entry! idx target) 'invalid-target)]
       [(and (exact-integer? target) (> target 0)) (navigate-next-leaf! idx)]
       [(and (exact-integer? target) (< target 0)) (navigate-prev-leaf! idx)]
       [else 'invalid-target])]))

;; ============================================================
;; create-agent-session — unified factory (#1152)
;; ============================================================

;;; create-agent-session : #:provider any/c
;;;                        [#:session-dir (or/c path-string? path? #f)]
;;;                        [#:session-id (or/c string? #f)]
;;;                        [#:model-name (or/c string? #f)]
;;;                        [#:max-iterations exact-positive-integer?]
;;;                        [#:system-instructions (listof string?)]
;;;                        [#:token-budget-threshold exact-positive-integer?]
;;;                        [#:tool-registry any/c]
;;;                        [#:extension-registry any/c]
;;;                        [#:event-bus any/c]
;;;                        [#:thinking-level (or/c symbol? #f)]
;;;                        -> runtime?
;;;
;;; Unified single-call entry point: creates a runtime AND opens a session
;;; in one step. Delegates to make-runtime + open-session internally.
;;; If #:thinking-level is provided, it is set on the session.
(define (create-agent-session #:provider provider
                              #:session-dir [session-dir (default-session-dir)]
                              #:session-id [session-id #f]
                              #:model-name [model-name #f]
                              #:max-iterations [max-iterations 10]
                              #:system-instructions [system-instructions '()]
                              #:token-budget-threshold
                              [token-budget-threshold DEFAULT-TOKEN-BUDGET-THRESHOLD]
                              #:tool-registry [tool-registry (make-tool-registry)]
                              #:extension-registry [extension-registry #f]
                              #:event-bus [event-bus (make-event-bus)]
                              #:thinking-level [thinking-level #f])
  (define rt
    (make-runtime #:provider provider
                  #:session-dir session-dir
                  #:tool-registry tool-registry
                  #:extension-registry extension-registry
                  #:event-bus event-bus
                  #:model-name model-name
                  #:max-iterations max-iterations
                  #:system-instructions system-instructions
                  #:token-budget-threshold token-budget-threshold))
  (define opened (open-session rt session-id))
  (when (and thinking-level (runtime-rt-session opened))
    (session:set-thinking-level! (runtime-rt-session opened) thinking-level))
  opened)

;; ============================================================
;; #1196: Enriched SDK API — convenience aliases
;; ============================================================

;; q:create-session — factory with full config override
;; Creates a runtime, opens a session, and returns the runtime.
;; All parameters from create-agent-session are supported.
(define q:create-session create-agent-session)

;; q:session-send — send prompt and collect response programmatically
;; Returns (values runtime result) where result is the assistant response.
(define q:session-send run-prompt!)

;; q:session-subscribe — event subscription for streaming tokens etc.
;; Returns subscription ID for unsubscription.
(define q:session-subscribe subscribe-events!)

;; q:session-interrupt — cancel the current operation
(define q:session-interrupt interrupt!)

;; q:session-fork — fork the session at an entry
(define q:session-fork fork-session!)

;; q:session-compact — compact the session context
(define q:session-compact compact-session!)

;; q:session-info — get session metadata
(define q:session-info session-info)
