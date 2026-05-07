#lang racket/base

;; runtime/session-types.rkt — agent-session struct definition
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

(provide (struct-out agent-session)
         session-log-path
         ;; Convenience accessors by group
         session-provider
         session-tool-registry
         session-event-bus
         session-extension-registry)

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
         [prompt-running? #:mutable]) ; boolean — concurrent prompt execution guard
  #:transparent)

;; ============================================================
;; Shared helper: session log path
;; ============================================================

(define (session-log-path dir)
  (build-path dir "session.jsonl"))

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
