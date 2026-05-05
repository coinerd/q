#lang racket/base
;; STABILITY: stable

;; interfaces/sdk-core.rkt — core runtime types and session operations
;;
;; Extracted from sdk.rkt (v0.22.9 W3).
;; Provides runtime/runtime-config structs, make-runtime, open-session,
;; run-prompt!, subscribe-events!, interrupt!, fork-session!, compact-session!,
;; session-info, steer!, follow-up!, navigate!, create-agent-session.
;; Re-exports cancellation token, navigation types, in-memory manager,
;; thinking level, and context usage types.

(require racket/contract
         racket/math
         racket/list
         "../llm/provider.rkt"
         (only-in "../tools/tool.rkt" make-tool-registry tool-registry?)
         (only-in "../tools/registry-defaults.rkt" register-default-tools!)
         "../agent/event-bus.rkt"
         "../util/protocol-types.rkt"
         (prefix-in session: "../runtime/agent-session.rkt")
         (only-in "../runtime/iteration.rkt" register-session-extensions!)
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
         (only-in "../extensions/api.rkt" list-extensions make-extension-registry extension-registry?)
         (only-in "../extensions/hooks.rkt"
                  dispatch-hooks
                  hook-result?
                  hook-result-action
                  hook-result-payload)
         (only-in "../extensions/loader.rkt" discover-extensions load-extension!)
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

(provide (struct-out runtime-config)
         (struct-out runtime)
         runtime?

         cancel-token!
         cancellation-token?
         cancellation-token-cancelled?

         ;; Core API
         (contract-out
          [make-runtime
           (->* (#:provider provider?)
                (#:session-dir (or/c path-string? path? #f)
                               #:tool-registry tool-registry?
                               #:extension-registry (or/c extension-registry? #f)
                               #:event-bus event-bus?
                               #:model-name (or/c string? #f)
                               #:max-iterations exact-positive-integer?
                               #:system-instructions (listof string?)
                               #:token-budget-threshold exact-positive-integer?
                               #:cancellation-token (or/c cancellation-token? #f)
                               #:register-default-tools? boolean?
                               #:auto-load-extensions? boolean?
                               #:project-dir (or/c path-string? path? #f))
                runtime?)]
          [open-session (->* (runtime?) ((or/c string? #f)) runtime?)]
          [run-prompt! (runtime? string? . -> . (values runtime? (or/c hash? #f 'no-active-session)))]
          [make-cancellation-token
           (->* () (#:callback (or/c (-> any/c any/c) #f)) cancellation-token?)]
          [make-in-memory-session-manager (-> in-memory-session-manager?)]
          [subscribe-events!
           (->* (runtime? procedure?) ((or/c procedure? #f)) exact-nonnegative-integer?)]
          [interrupt! (runtime? . -> . runtime?)]
          [fork-session! (->* (runtime?) ((or/c string? #f)) (or/c runtime? 'no-active-session))]
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
         context-usage-near-threshold?)

;; ============================================================
;; Structs
;; ============================================================

(struct runtime-config
        (provider tool-registry
                  extension-registry
                  event-bus
                  session-dir
                  model-name
                  max-iterations
                  system-instructions
                  token-budget-threshold
                  resource-loader
                  session-manager)
  #:transparent)

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

(define (make-runtime #:provider provider
                      #:session-dir [session-dir (default-session-dir)]
                      #:tool-registry [tool-registry (make-tool-registry)]
                      #:extension-registry [extension-registry #f]
                      #:event-bus [event-bus (make-event-bus)]
                      #:model-name [model-name #f]
                      #:max-iterations [max-iterations 10]
                      #:system-instructions [system-instructions '()]
                      #:token-budget-threshold [token-budget-threshold DEFAULT-TOKEN-BUDGET-THRESHOLD]
                      #:cancellation-token [cancellation-token #f]
                      #:resource-loader [resource-loader #f]
                      #:session-manager [session-mgr #f]
                      #:register-default-tools? [register-default-tools? #t]
                      #:auto-load-extensions? [auto-load-extensions? #f]
                      #:project-dir [project-dir #f])
  (when register-default-tools?
    (register-default-tools! tool-registry))
  (when (and auto-load-extensions? project-dir)
    (define ext-dir (build-path project-dir "extensions"))
    (when (directory-exists? ext-dir)
      (for ([ext (in-list (discover-extensions ext-dir))])
        (load-extension! (or extension-registry (make-extension-registry))
                         ext
                         #:event-bus event-bus))))
  (make-runtime-internal (runtime-config provider
                                         tool-registry
                                         extension-registry
                                         event-bus
                                         session-dir
                                         model-name
                                         max-iterations
                                         system-instructions
                                         token-budget-threshold
                                         resource-loader
                                         session-mgr)
                         #f
                         (or cancellation-token (make-cancellation-token))))

;; ============================================================
;; open-session
;; ============================================================

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
  (define ext-reg (runtime-config-extension-registry cfg))
  (define tool-reg (runtime-config-tool-registry cfg))
  (define bus (runtime-config-event-bus cfg))
  (when (and ext-reg tool-reg bus sess)
    (register-session-extensions! tool-reg ext-reg bus (session:session-id sess)))
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
  (cancel-token! (rt-token rt))
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
    [(not sess) 'no-active-session]
    [else
     (define forked (session:fork-session sess entry-id))
     (make-runtime-internal (rt-cfg rt) forked (rt-token rt))]))

;; ============================================================
;; compact-session!
;; ============================================================

(define (compact-session! rt #:persist? [persist? #f])
  (define sess (rt-sess rt))
  (cond
    [(not sess) 'no-active-session]
    [else
     (define bus (runtime-config-event-bus (rt-cfg rt)))
     (define sid (session:session-id sess))
     (publish! bus
               (make-event "compaction.started"
                           (exact-truncate (/ (current-inexact-milliseconds) 1000))
                           sid
                           #f
                           (hasheq 'sessionId sid 'persist? persist?)))
     (define history (session:session-history sess))
     (define log-path
       (build-path (runtime-config-session-dir (rt-cfg rt))
                   (session:session-id sess)
                   "session.jsonl"))
     (define compaction-res
       (if persist?
           (compact-and-persist! history log-path)
           (compact-history history)))
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
     (values rt compaction-res)]))

;; ============================================================
;; session-info, steer!, follow-up!
;; ============================================================

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

(define (steer! rt message)
  (define sess (rt-sess rt))
  (cond
    [(not sess) rt]
    [else
     (define q (session:agent-session-queue sess))
     (enqueue-steering! q message)
     rt]))

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
;; create-agent-session (#1152)
;; ============================================================

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
                              #:thinking-level [thinking-level #f]
                              #:resource-loader [resource-loader #f]
                              #:session-manager [session-mgr #f])
  (define rt
    (make-runtime #:provider provider
                  #:session-dir session-dir
                  #:tool-registry tool-registry
                  #:extension-registry extension-registry
                  #:event-bus event-bus
                  #:model-name model-name
                  #:max-iterations max-iterations
                  #:system-instructions system-instructions
                  #:token-budget-threshold token-budget-threshold
                  #:resource-loader resource-loader
                  #:session-manager session-mgr))
  (define opened (open-session rt session-id))
  (when (and thinking-level (runtime-rt-session opened))
    (session:set-thinking-level! (runtime-rt-session opened) thinking-level))
  opened)
