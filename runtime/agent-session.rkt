#lang racket/base
;;
;; COMPOSITION ROOT: This module wires together dependencies from
;; lower layers. It should not be imported by other production modules.
;;

;; runtime/agent-session.rkt — session lifecycle orchestration (façade)
;; STABILITY: evolving
;;
;; Central runtime layer that ties together session store, core loop,
;; provider, tool registry, and event bus into a coherent agent session.
;; v0.22.9: Refactored into façade. Core prompt execution pipeline
;; extracted to session-lifecycle.rkt. This module retains session
;; creation/resume/fork/close and re-exports all public symbols.
;;
;; Provides:
;;   make-agent-session     — create new session (generate ID, init log)
;;   resume-agent-session   — resume existing session (load log + index)
;;   fork-session           — fork session at a given point
;;   run-prompt!            — run a user prompt through the full agent loop
;;   session-id             — get the session ID
;;   session-history        — get full message history (replayed from log)
;;   session-active?        — check if session is active (not closed)
;;   close-session!         — close/deactivate a session
;;   agent-session?         — predicate

(require racket/contract
         racket/string
         racket/file
         racket/list
         racket/runtime-path
         racket/path
         racket/dict
         (only-in "../util/error/errors.rkt" raise-session-error)
         (only-in "../util/loop-result.rkt" make-loop-result)
         (only-in "../util/message/message.rkt" message-id message-kind message?)
         "../agent/queue.rkt"
         "../agent/event-bus.rkt"
         (only-in "../util/hook-types.rkt" hook-result-action hook-result-payload)
         "session/session-store.rkt"
         "../runtime/session-index.rkt"
         (only-in "../util/event/event-types.rkt" injection-event-topic)
         "compaction/compactor.rkt"
         (only-in "../util/extension/extensions.rkt" extension-name)
         (only-in "layer-adapters.rkt" list-extensions)
         ;; TR BOUNDARY: event-payloads.rkt is #lang typed/racket.
         (only-in "../util/event/event-payloads.rkt"
                  session-start-payload
                  session-end-payload
                  session-switch-payload)
         "../util/ids.rkt"
         (only-in "runtime-helpers.rkt" emit-session-event! maybe-dispatch-hooks)
         (only-in "../agent/event-emitter.rkt" emit-typed-event!)
         (only-in "../agent/event-structs/session-events.rkt"
                  session-start-event
                  session-shutdown-event)
         "session/session-types.rkt"
         (only-in "session/lifecycle-state.rkt" make-lifecycle-state lifecycle-state? lifecycle-state)
         (only-in "session/session-events.rkt" wire-session-event-handlers!)
         (only-in "../llm/token-budget.rkt" estimate-context-tokens)
         (only-in "session/session-controls.rkt"
                  set-model!
                  cycle-model!
                  thinking-levels
                  thinking-level?
                  thinking-level->budget
                  set-thinking-level!
                  request-shutdown!
                  force-shutdown!
                  shutdown-requested?
                  force-shutdown-requested?
                  reset-shutdown-flags!)
         ;; v0.22.9: extracted from this module
         (only-in "session/session-lifecycle.rkt" run-prompt! ensure-persisted! buffer-or-append!)
         (only-in "compaction/session-compaction.rkt" maybe-compact-context)
         (only-in "session/session-config.rkt"
                  session-config?
                  hash->session-config
                  current-task-state-aware-rollout-rate
                  current-context-assembly-profile
                  config-provider
                  config-tool-registry
                  config-event-bus
                  config-extension-registry
                  config-model-name
                  config-system-instructions
                  config-thinking-level
                  config-session-dir
                  config-settings
                  config-memory-enabled?)
         (only-in "provider/model-registry.rkt" model-registry?)
         ;; LF2 (GAP-10): conclusion-to-memory bridge
         (only-in "memory/conclusion-bridge.rkt"
                  persist-high-value-conclusions!
                  current-conclusion-to-memory-bridge-enabled)
         (only-in "memory/service.rkt" current-memory-backend)
         ;; F8: browser service lifecycle
         (only-in "../browser/service.rkt"
                  make-secure-browser-service
                  current-browser-service
                  secure-browser-service?)
         (only-in "../browser/settings.rkt"
                  load-browser-settings
                  default-browser-settings
                  browser-settings-enabled?
                  browser-settings-sidecar-path
                  browser-settings-sidecar-timeout-ms
                  browser-settings-headless?)
         (only-in "../browser/adapters/mock.rkt"
                  make-mock-adapter
                  mock-open
                  mock-close
                  mock-navigate
                  mock-observe
                  mock-act
                  mock-screenshot)
         (only-in "../browser/adapters/playwright-sidecar.rkt" make-playwright-adapter)
         (only-in "../browser/adapter.rkt" make-browser-adapter)
         (only-in "../runtime/memory/service.rkt" initialize-memory-backend!))
(require "session/session-mutation.rkt")

(provide agent-session?
         ;; v0.32.8: Convenience accessors (stable API)
         session-provider
         session-tool-registry
         session-event-bus
         session-extension-registry
         agent-session-session-dir
         agent-session-queue
         agent-session-index
         agent-session-extension-registry
         agent-session-model-name
         agent-session-system-instructions
         agent-session-compacting?
         agent-session-last-compaction-time
         agent-session-persisted?
         agent-session-pending-entries
         agent-session-prompt-running?
         buffer-or-append!
         (contract-out ;; NOTE (W-01): This is the ONLY entry point that accepts raw hashes for backward compat.
          [make-agent-session (-> (or/c hash? session-config?) agent-session?)]
          [resume-agent-session (-> string? (or/c session-config? hash?) agent-session?)]
          [fork-session (->* (agent-session?) ((or/c string? #f)) agent-session?)]
          [run-prompt!
           (->* (agent-session? (or/c string? message?))
                (#:max-iterations (or/c exact-nonnegative-integer? #f)
                                  #:ensure-persisted! (or/c procedure? #f)
                                  #:buffer-or-append! (or/c procedure? #f))
                any)]
          [session-id (-> agent-session? string?)]
          [session-history (-> agent-session? (listof message?))]
          [session-active? (-> agent-session? boolean?)]
          [close-session! (-> agent-session? void?)])
         ;; v0.42.2: Session control contracts (B5-01)
         ;; Session controls
         (contract-out [set-model! (-> agent-session? string? void?)]
                       [cycle-model! (-> agent-session? model-registry? (or/c string? #f))]
                       [set-thinking-level! (-> agent-session? thinking-level? void?)]
                       [request-shutdown! (-> agent-session? void?)]
                       [force-shutdown! (-> agent-session? void?)]
                       [reset-shutdown-flags! (-> agent-session? void?)]
                       [shutdown-requested? (-> agent-session? boolean?)]
                       [force-shutdown-requested? (-> agent-session? boolean?)]
                       [ensure-persisted! (-> agent-session? void?)])
         ;; Pure helpers (W1 #4191)
         slice-entries-up-to
         make-session-struct
         maybe-compact-context
         ;; Thinking level control (#1153)
         thinking-levels
         thinking-level?
         thinking-level->budget
         agent-session-thinking-level
         ;; Graceful shutdown (#1158)
         agent-session-shutdown-requested?
         agent-session-force-shutdown?
         session-rollout-enabled?)

;; ============================================================
;; ARCH-05: struct definition moved to session-types.rkt
;; ============================================================

;; ============================================================
;; Helpers
;; ============================================================

;; ============================================================
;; Pure helpers (extracted for testability)
;; ============================================================

;; slice-entries-up-to : (listof message?) (or/c string? #f) -> (listof message?)
;; Pure: returns entries truncated up to and including parent-entry-id.
;; If parent-entry-id is #f or not found, returns all entries.
(define (slice-entries-up-to entries parent-entry-id)
  (if parent-entry-id
      (let ([id-exists? (for/or ([e (in-list entries)])
                          (equal? (message-id e) parent-entry-id))])
        (cond
          [(not id-exists?) entries]
          [else
           (let loop ([remaining entries]
                      [acc '()])
             (cond
               [(null? remaining) (reverse acc)]
               [(equal? (message-id (car remaining)) parent-entry-id)
                (reverse (cons (car remaining) acc))]
               [else (loop (cdr remaining) (cons (car remaining) acc))]))]))
      entries))

;; make-session-struct : keyword-based pure constructor wrapping agent-session.
;; All fields are keyword args with sensible defaults.
(define (make-session-struct #:id id
                             #:dir dir
                             #:provider [provider #f]
                             #:tool-registry [tool-registry #f]
                             #:event-bus [event-bus #f]
                             #:extension-registry [extension-registry #f]
                             #:model-name [model-name #f]
                             #:system-instructions [system-instructions '()]
                             #:index [index #f]
                             #:queue [queue #f]
                             #:config [config #f]
                             #:active? [active? #t]
                             #:start-time [start-time (now-seconds)]
                             #:pending-entries [pending-entries '()]
                             #:thinking-level [thinking-level #f]
                             #:lifecycle [lifecycle (make-lifecycle-state)])
  (agent-session id
                 dir
                 provider
                 tool-registry
                 event-bus
                 extension-registry
                 model-name
                 system-instructions
                 index
                 queue
                 config
                 active?
                 start-time
                 pending-entries
                 thinking-level
                 lifecycle))

;; session-rollout-enabled? : string? -> boolean?
;; Deterministic A/B assignment using session-id hash modulo 100.
(define (session-rollout-enabled? sid)
  (define rate (current-task-state-aware-rollout-rate))
  (define profile (current-context-assembly-profile))
  ;; v0.78.4 G4: If an explicit profile is set (not 'off), always allow
  (or (not (eq? profile 'off))
      (and (> rate 0.0)
           (< (modulo (equal-hash-code sid) 100) (inexact->exact (round (* rate 100)))))))

;; ============================================================
;; Browser adapter auto-detection
;; ============================================================

;; Default sidecar path, resolved relative to q/runtime/agent-session.rkt.
(define-runtime-path default-playwright-sidecar-path "../sidecars/playwright/q-playwright-sidecar.js")

;; Try to create a Playwright adapter; fall back to mock.
;; Returns (values adapter sidecar-state-or-#f)
(define (try-make-playwright-adapter browser-cfg)
  (define sidecar-path (browser-settings-sidecar-path browser-cfg))
  (define timeout-ms (browser-settings-sidecar-timeout-ms browser-cfg))
  (define headless? (browser-settings-headless? browser-cfg))
  (define sidecar-js
    (if sidecar-path
        (string->path sidecar-path)
        default-playwright-sidecar-path))
  (cond
    [(and (file-exists? sidecar-js) (find-executable-path "node"))
     (with-handlers ([exn:fail? (lambda (e)
                                  (log-warning "Playwright adapter failed, falling back to mock: ~a"
                                               (exn-message e))
                                  (values (make-mock-browser-adapter) #f))])
       (define adapter
         (make-playwright-adapter (path->string sidecar-js)
                                  #:timeout-ms timeout-ms
                                  #:headless? headless?))
       (values adapter #f))]
    [else
     (log-warning "Browser: Node.js or sidecar not found, using mock adapter")
     (values (make-mock-browser-adapter) #f)]))

;; Build a mock adapter inline (avoids circular dependency issues)
(define (make-mock-browser-adapter)
  (define mock (make-mock-adapter))
  (make-browser-adapter #:open (lambda (sid target) (mock-open mock target #f))
                        #:close (lambda (sid) (mock-close mock sid))
                        #:navigate (lambda (sid url) (mock-navigate mock sid url))
                        #:observe (lambda (sid selector) (mock-observe mock sid selector))
                        #:act (lambda (sid action) (mock-act mock sid action))
                        #:screenshot (lambda (sid sel fp) (mock-screenshot mock sid sel "png"))))

;; ============================================================
;; make-agent-session
;; ============================================================

(define (make-agent-session config)
  ;; v0.30.4: Convert hash to session-config for typed access
  (define cfg
    (if (session-config? config)
        config
        (hash->session-config config)))
  (define sid (generate-id))
  (define base-dir (config-session-dir cfg))
  (define dir (build-path base-dir sid))

  ;; M4 W0: Rollout gate — deterministically assign task-state-aware? based on session-id hash
  (define task-state-aware? (session-rollout-enabled? sid))
  (define cfg-with-rollout (dict-set cfg 'task-state-aware? task-state-aware?))

  (define session-created-at (now-seconds))

  (define sess
    (make-session-struct #:id sid
                         #:dir dir
                         #:provider (config-provider cfg-with-rollout)
                         #:tool-registry (config-tool-registry cfg-with-rollout)
                         #:event-bus (config-event-bus cfg-with-rollout)
                         #:extension-registry (config-extension-registry cfg-with-rollout)
                         #:model-name (config-model-name cfg-with-rollout)
                         #:system-instructions (config-system-instructions cfg-with-rollout)
                         #:queue (make-queue)
                         #:config cfg-with-rollout
                         #:start-time session-created-at
                         #:thinking-level (config-thinking-level cfg-with-rollout)))

  ;; Emit session.started
  (emit-typed-event! (agent-session-event-bus sess)
                     (session-start-event "session.started" (current-inexact-milliseconds) sid #f #f))

  ;; Eagerly persist session directory and version header
  (ensure-persisted! sess)

  ;; Dispatch 'session-start hook
  (define-values (_start-payload _session-start-res)
    (maybe-dispatch-hooks (config-extension-registry cfg)
                          'session-start
                          (session-start-payload sid config 'new)))

  ;; Subscribe to fork.requested and compact.requested events from TUI/CLI
  (wire-session-event-handlers! sess fork-session)

  ;; #668: Emit resources.discover event
  (let ([ext-reg (config-extension-registry cfg)])
    (emit-session-event! (agent-session-event-bus sess)
                         sid
                         "resources.discover"
                         (hasheq 'session-id
                                 sid
                                 'extensions
                                 (if ext-reg
                                     (map extension-name (list-extensions ext-reg))
                                     '()))))

  ;; F8: Browser service initialization (feature-flagged)
  ;; Uses Playwright sidecar when Node.js + sidecar are available,
  ;; falls back to mock adapter otherwise.
  (let ([q-cfg (config-settings cfg-with-rollout)])
    (when q-cfg
      (let ([browser-cfg (load-browser-settings q-cfg)])
        (when (browser-settings-enabled? browser-cfg)
          (define-values (adapter _sidecar-state) (try-make-playwright-adapter browser-cfg))
          (define browser-svc
            (make-secure-browser-service adapter
                                         #:settings browser-cfg
                                         #:event-bus (agent-session-event-bus sess)))
          (current-browser-service browser-svc)))))

  ;; v0.95.15 W2: Initialize memory backend from session config
  (when (config-memory-enabled? cfg-with-rollout)
    (initialize-memory-backend! cfg-with-rollout))

  sess)

;; ============================================================
;; resume-agent-session
;; ============================================================

(define (resume-agent-session session-id config)
  ;; v0.30.4: Convert hash to session-config
  (define cfg
    (if (session-config? config)
        config
        (hash->session-config config)))
  (define base-dir (config-session-dir cfg))
  (define dir (build-path base-dir session-id))

  (unless (directory-exists? dir)
    (raise-session-error 'resume-agent-session "session directory not found" session-id dir))

  (define log-path (session-log-path dir))
  (when (file-exists? log-path)
    (ensure-session-version-header! log-path))

  (define idx-path (session-index-path dir))

  (define idx
    (if (file-exists? log-path)
        (build-index! log-path idx-path)
        #f))

  ;; Dispatch 'session-before-switch hook
  (define switch-payload (session-switch-payload session-id 'resume))
  (define-values (_amended-switch switch-res)
    (maybe-dispatch-hooks (config-extension-registry cfg) 'session-before-switch switch-payload))
  (when (and switch-res (eq? (hook-result-action switch-res) 'block))
    (raise-session-error 'resume-agent-session "session resume blocked by extension" session-id))

  (define sess
    (make-session-struct #:id session-id
                         #:dir dir
                         #:provider (config-provider cfg)
                         #:tool-registry (config-tool-registry cfg)
                         #:event-bus (config-event-bus cfg)
                         #:extension-registry (config-extension-registry cfg)
                         #:model-name (config-model-name cfg)
                         #:system-instructions (config-system-instructions cfg)
                         #:index idx
                         #:queue (make-queue)
                         #:config cfg
                         #:thinking-level (config-thinking-level cfg)
                         #:lifecycle (lifecycle-state #f #f #t #f #f #f #f '() '())))

  ;; v0.75.6: Load persisted task state and conclusions
  (when (file-exists? log-path)
    (guarded-set-task-fsm-state! sess (load-latest-task-state log-path))
    (define conclusions (load-conclusions log-path))
    (unless (null? conclusions)
      (guarded-set-task-conclusions! sess conclusions)))

  (emit-session-event! (agent-session-event-bus sess)
                       session-id
                       "session.resumed"
                       (hasheq 'session-id session-id))

  (define resume-start-payload (session-start-payload session-id config 'resume))
  (maybe-dispatch-hooks (config-extension-registry cfg) 'session-start resume-start-payload)

  (wire-session-event-handlers! sess fork-session)

  ;; v0.95.15 W2: Initialize memory backend from session config
  (when (config-memory-enabled? cfg)
    (initialize-memory-backend! cfg))

  sess)

;; ============================================================
;; fork-session
;; ============================================================

(define (fork-session sess [parent-entry-id #f])
  (unless (session-active? sess)
    (raise-session-error (format "fork-session: session ~a is closed" (agent-session-session-id sess))
                         (agent-session-session-id sess)))
  (let* ([ext-reg (agent-session-extension-registry sess)]
         [fork-payload (hasheq 'session-id
                               (agent-session-session-id sess)
                               'parent-entry-id
                               parent-entry-id
                               'reason
                               "user-fork")])
    (maybe-dispatch-hooks ext-reg 'session-before-fork fork-payload)
    (fork-session-internal sess parent-entry-id)))

(define (fork-session-internal sess parent-entry-id)
  (define new-id (generate-id))
  (define base-dir (path-only (simple-form-path (agent-session-session-dir sess))))
  (define new-dir (build-path base-dir new-id))
  (make-directory* new-dir)

  (define log-path (session-log-path-for sess))
  (define entries
    (if (file-exists? log-path)
        (load-session-log log-path)
        '()))

  (define entries-to-copy (slice-entries-up-to entries parent-entry-id))

  (define new-log-path (session-log-path new-dir))
  (append-entries! new-log-path entries-to-copy)

  (define new-idx-path (session-index-path new-dir))
  (define new-idx
    (if (null? entries-to-copy)
        #f
        (build-index! new-log-path new-idx-path)))

  (define new-sess
    (make-session-struct #:id new-id
                         #:dir new-dir
                         #:provider (agent-session-provider sess)
                         #:tool-registry (agent-session-tool-registry sess)
                         #:event-bus (agent-session-event-bus sess)
                         #:extension-registry (agent-session-extension-registry sess)
                         #:model-name (agent-session-model-name sess)
                         #:system-instructions (agent-session-system-instructions sess)
                         #:index new-idx
                         #:queue (make-queue)
                         #:config (agent-session-config sess)
                         #:thinking-level (agent-session-thinking-level sess)
                         #:lifecycle (lifecycle-state #f #f #t #f #f #f #f '() '())))

  (emit-session-event! (agent-session-event-bus sess)
                       (agent-session-session-id sess)
                       "session.forked"
                       (hasheq 'newSessionId
                               new-id
                               'parentSessionId
                               (agent-session-session-id sess)
                               'forkPoint
                               (or parent-entry-id "latest")))

  (when (agent-session-extension-registry sess)
    (maybe-dispatch-hooks
     (agent-session-extension-registry sess)
     'session-start
     (hasheq 'session-id new-id 'reason 'fork 'parent-session-id (agent-session-session-id sess))))

  new-sess)

;; ============================================================
;; Accessors
;; ============================================================

(define (session-id sess)
  (agent-session-session-id sess))

(define (session-history sess)
  (define log-path (session-log-path-for sess))
  (if (file-exists? log-path)
      (filter (lambda (m) (not (eq? (message-kind m) 'session-info))) (load-session-log log-path))
      '()))

(define (session-active? sess)
  (agent-session-active? sess))

(define (close-session! sess)
  ;; F8: Browser service cleanup
  (when (current-browser-service)
    (current-browser-service #f))
  (when (session-active? sess)
    (ensure-persisted! sess)
    (emit-typed-event! (agent-session-event-bus sess)
                       (session-shutdown-event "session.closed"
                                               (current-inexact-milliseconds)
                                               (agent-session-session-id sess)
                                               #f
                                               "normal"))
    (define session-duration (- (now-seconds) (agent-session-start-time sess)))
    (define shutdown-payload (session-end-payload (agent-session-session-id sess) session-duration))
    (define-values (_shutdown-payload _shutdown-res)
      (maybe-dispatch-hooks (agent-session-extension-registry sess)
                            'session-shutdown
                            shutdown-payload))
    ;; LF2 (GAP-10): Persist high-value conclusions to memory on session end
    (persist-high-value-conclusions! (agent-session-task-conclusions sess)
                                     #:backend (current-memory-backend)
                                     #:session-id (agent-session-session-id sess))
    (guarded-set-active! sess #f)))

;; ============================================================
;; Re-exported from extracted sub-modules
;; ============================================================
;; run-prompt!, ensure-persisted!, buffer-or-append!
;;   → session-lifecycle.rkt
;; wire-session-event-handlers! → session-events.rkt
;; set-model!, cycle-model!, thinking/shutdown controls → session-controls.rkt
;; maybe-compact-context → session-compaction.rkt
;; struct definition → session-types.rkt
