#lang racket/base

;; runtime/session/session-types.rkt — agent-session struct definition

(require racket/contract
         (only-in "lifecycle-state.rkt"
                  lifecycle-state
                  make-lifecycle-state
                  lifecycle-state?
                  lifecycle-state-compacting?
                  lifecycle-state-last-compaction-time
                  lifecycle-state-persisted?
                  lifecycle-state-shutdown-requested?
                  lifecycle-state-force-shutdown?
                  lifecycle-state-prompt-running?
                  lifecycle-state-task-fsm-state
                  lifecycle-state-task-conclusions
                  lifecycle-state-recent-tool-calls
                  set-lifecycle-state-compacting?!
                  set-lifecycle-state-last-compaction-time!
                  set-lifecycle-state-persisted?!
                  set-lifecycle-state-shutdown-requested?!
                  set-lifecycle-state-force-shutdown?!
                  set-lifecycle-state-prompt-running?!
                  set-lifecycle-state-task-fsm-state!
                  set-lifecycle-state-task-conclusions!
                  set-lifecycle-state-recent-tool-calls!))
;; STABILITY: internal
;;
;; Extracted from agent-session.rkt (ARCH-05).
;; Shared types module so extracted sub-modules can access the session
;; struct without creating circular dependencies.
;;
;; v0.85.3 (A1-05): Lifecycle fields extracted to lifecycle-state struct.
;; agent-session now has 16 fields. Lifecycle accessors delegate to lifecycle-state.

(provide agent-session
         agent-session?
         agent-session-session-id
         agent-session-session-dir
         agent-session-provider
         agent-session-tool-registry
         agent-session-event-bus
         agent-session-extension-registry
         agent-session-model-name
         agent-session-system-instructions
         agent-session-index
         agent-session-queue
         agent-session-config
         agent-session-active?
         agent-session-start-time
         agent-session-pending-entries
         agent-session-thinking-level
         agent-session-lifecycle
         ;; Compatibility accessors — delegate to lifecycle-state
         agent-session-compacting?
         agent-session-last-compaction-time
         agent-session-persisted?
         agent-session-shutdown-requested?
         agent-session-force-shutdown?
         agent-session-prompt-running?
         agent-session-task-fsm-state
         agent-session-task-conclusions
         agent-session-recent-tool-calls
         session-log-path
         session-index-path
         lifecycle-state?
         (contract-out [session-log-path-for (-> agent-session? path?)]
                       [session-provider (-> agent-session? any/c)]
                       [session-tool-registry (-> agent-session? any/c)]
                       [session-event-bus (-> agent-session? any/c)]
                       [session-extension-registry (-> agent-session? any/c)]))

;; ============================================================
;; agent-session struct (16 fields after A1-05 extraction)
;; ============================================================

;; Identity (2 immutable):
;;   session-id, session-dir
;;
;; Runtime Services (5 immutable, injected):
;;   provider, tool-registry, event-bus, extension-registry, queue
;;
;; Session Data (8 mutable):
;;   model-name, system-instructions, index, config, active?,
;;   start-time, pending-entries, thinking-level
;;
;; Lifecycle State (1 boxed):
;;   lifecycle — lifecycle-state? with 9 mutable fields

(struct agent-session
        (session-id ; string
         session-dir ; path
         ;; Runtime Services (injected)
         provider ; provider?
         tool-registry ; tool-registry?
         event-bus ; event-bus?
         extension-registry ; extension-registry? or #f
         ;; Session Data (mutable)
         [model-name #:mutable] ; string or #f
         system-instructions ; (listof string)
         [index #:mutable] ; session-index? or #f
         queue ; queue?
         [config #:mutable] ; hash (runtime settings)
         [active? #:mutable] ; boolean
         [start-time #:mutable] ; integer (seconds since epoch)
         [pending-entries #:mutable] ; (listof message?) — buffered before persistence
         [thinking-level #:mutable] ; symbol — one of thinking-levels (#1153)
         ;; Lifecycle state (boxed sub-struct, A1-05)
         lifecycle) ; lifecycle-state?
  #:transparent)

;; ============================================================
;; Shared helper: session log path
;; ============================================================

(define (session-log-path dir)
  (build-path dir "session.jsonl"))

(define (session-index-path dir)
  (build-path dir "session.index"))

;; session-log-path-for : agent-session? -> path?
;; Convenience: returns session.jsonl path for a session struct.
(define (session-log-path-for sess)
  (session-log-path (agent-session-session-dir sess)))

;; ============================================================
;; Convenience accessors — stable API regardless of internal layout
;; ============================================================

(define (session-provider sess)
  (agent-session-provider sess))

(define (session-tool-registry sess)
  (agent-session-tool-registry sess))

(define (session-event-bus sess)
  (agent-session-event-bus sess))

(define (session-extension-registry sess)
  (agent-session-extension-registry sess))

;; ============================================================
;; Compatibility lifecycle accessors — delegate to lifecycle-state
;; These replace the old struct fields. All callers use the same names.
;; ============================================================

(define (agent-session-compacting? sess)
  (lifecycle-state-compacting? (agent-session-lifecycle sess)))

(define (agent-session-last-compaction-time sess)
  (lifecycle-state-last-compaction-time (agent-session-lifecycle sess)))

(define (agent-session-persisted? sess)
  (lifecycle-state-persisted? (agent-session-lifecycle sess)))

(define (agent-session-shutdown-requested? sess)
  (lifecycle-state-shutdown-requested? (agent-session-lifecycle sess)))

(define (agent-session-force-shutdown? sess)
  (lifecycle-state-force-shutdown? (agent-session-lifecycle sess)))

(define (agent-session-prompt-running? sess)
  (lifecycle-state-prompt-running? (agent-session-lifecycle sess)))

(define (agent-session-task-fsm-state sess)
  (lifecycle-state-task-fsm-state (agent-session-lifecycle sess)))

(define (agent-session-task-conclusions sess)
  (lifecycle-state-task-conclusions (agent-session-lifecycle sess)))

(define (agent-session-recent-tool-calls sess)
  (lifecycle-state-recent-tool-calls (agent-session-lifecycle sess)))

;; ============================================================
;; Internal setters — delegate to lifecycle-state
;; ============================================================

;; Internal sub-module: provides raw setters for session-mutation.rkt only.
;; These MUST NOT be used outside session-mutation.rkt.
(module+ internal
  (provide set-agent-session-model-name!
           set-agent-session-index!
           set-agent-session-config!
           set-agent-session-active?!
           set-agent-session-start-time!
           set-agent-session-pending-entries!
           set-agent-session-thinking-level!
           ;; Lifecycle setters — delegate to lifecycle-state
           set-agent-session-compacting?!
           set-agent-session-last-compaction-time!
           set-agent-session-persisted?!
           set-agent-session-shutdown-requested?!
           set-agent-session-force-shutdown?!
           set-agent-session-prompt-running?!
           set-agent-session-task-fsm-state!
           set-agent-session-task-conclusions!
           set-agent-session-recent-tool-calls!)

  (define (set-agent-session-compacting?! sess value)
    (set-lifecycle-state-compacting?! (agent-session-lifecycle sess) value))

  (define (set-agent-session-last-compaction-time! sess value)
    (set-lifecycle-state-last-compaction-time! (agent-session-lifecycle sess) value))

  (define (set-agent-session-persisted?! sess value)
    (set-lifecycle-state-persisted?! (agent-session-lifecycle sess) value))

  (define (set-agent-session-shutdown-requested?! sess value)
    (set-lifecycle-state-shutdown-requested?! (agent-session-lifecycle sess) value))

  (define (set-agent-session-force-shutdown?! sess value)
    (set-lifecycle-state-force-shutdown?! (agent-session-lifecycle sess) value))

  (define (set-agent-session-prompt-running?! sess value)
    (set-lifecycle-state-prompt-running?! (agent-session-lifecycle sess) value))

  (define (set-agent-session-task-fsm-state! sess value)
    (set-lifecycle-state-task-fsm-state! (agent-session-lifecycle sess) value))

  (define (set-agent-session-task-conclusions! sess value)
    (set-lifecycle-state-task-conclusions! (agent-session-lifecycle sess) value))

  (define (set-agent-session-recent-tool-calls! sess value)
    (set-lifecycle-state-recent-tool-calls! (agent-session-lifecycle sess) value)))
