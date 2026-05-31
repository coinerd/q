#lang racket/base

;; runtime/session-types.rkt — agent-session struct definition

(require racket/contract)
;; STABILITY: internal
;;
;; Extracted from agent-session.rkt (ARCH-05).
;; Shared types module so extracted sub-modules can access the session
;; struct without creating circular dependencies.
;;
;; ============================================================
;; FIELD GROUPINGS (v0.32.8 documentation)
;; ============================================================
;;
;; The agent-session struct has 21 fields organized into 5 logical groups:
;;
;; Identity (immutable):
;;   session-id, session-dir, start-time
;;
;; Runtime Services (immutable, injected at creation):
;;   provider, tool-registry, event-bus, extension-registry
;;
;; Session Data (mutable):
;;   model-name, system-instructions, config, index, queue,
;;   thinking-level, pending-entries
;;
;; Lifecycle Flags (mutable, internal — use session-controls.rkt):
;;   compacting?, persisted?, prompt-running?
;;   shutdown-requested?, force-shutdown?, active?
;;   last-compaction-time
;;
;; NOTE: Field layout is kept flat for performance and struct-copy
;; convenience. The groupings above are architectural documentation,
;; not sub-structs. See v0.32.6 TUI state decomposition for the
;; rationale (75 struct-copy callsites make nested structs worse).

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
         agent-session-compacting?
         agent-session-last-compaction-time
         agent-session-persisted?
         agent-session-pending-entries
         agent-session-thinking-level
         agent-session-shutdown-requested?
         agent-session-force-shutdown?
         agent-session-prompt-running?
         agent-session-task-fsm-state
         agent-session-task-conclusions
         session-log-path
         session-index-path
         (contract-out [session-log-path-for (-> agent-session? path?)]
                       [session-provider (-> agent-session? any/c)]
                       [session-tool-registry (-> agent-session? any/c)]
                       [session-event-bus (-> agent-session? any/c)]
                       [session-extension-registry (-> agent-session? any/c)]))

;; ============================================================
;; agent-session struct
;; ============================================================

;; Identity
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
         ;; Lifecycle Flags (mutable, internal)
         [compacting? #:mutable] ; boolean — guard against recursive compaction
         [last-compaction-time #:mutable] ; integer or #f — timestamp of last compaction
         [persisted? #:mutable] ; boolean — #f until directory + first write
         [pending-entries #:mutable] ; (listof message?) — buffered before persistence
         [thinking-level #:mutable] ; symbol — one of thinking-levels (#1153)
         [shutdown-requested? #:mutable] ; boolean — graceful shutdown flag (#1158)
         [force-shutdown? #:mutable]
         [prompt-running? #:mutable]
         [task-fsm-state #:mutable]
         [task-conclusions #:mutable]) ; boolean — concurrent prompt execution guard
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

;; Internal sub-module: provides raw setters for session-mutation.rkt only.
;; These MUST NOT be used outside session-mutation.rkt.
(module+ internal
  (provide set-agent-session-model-name!
           set-agent-session-index!
           set-agent-session-config!
           set-agent-session-active?!
           set-agent-session-start-time!
           set-agent-session-compacting?!
           set-agent-session-last-compaction-time!
           set-agent-session-persisted?!
           set-agent-session-pending-entries!
           set-agent-session-thinking-level!
           set-agent-session-shutdown-requested?!
           set-agent-session-force-shutdown?!
           set-agent-session-prompt-running?!
           set-agent-session-task-fsm-state!
           set-agent-session-task-conclusions!))
