#lang racket/base

;; runtime/session-types.rkt — agent-session struct definition
;;
;; Extracted from agent-session.rkt (ARCH-05).
;; Shared types module so extracted sub-modules can access the session
;; struct without creating circular dependencies.

(provide (struct-out agent-session))

;; ============================================================
;; agent-session struct
;; ============================================================

(struct agent-session
        (session-id ; string
         session-dir ; path
         provider ; provider?
         tool-registry ; tool-registry?
         event-bus ; event-bus?
         extension-registry ; extension-registry? or #f
         [model-name #:mutable] ; string or #f
         system-instructions ; (listof string)
         [index #:mutable] ; session-index? or #f
         queue ; queue?
         config ; hash (runtime settings)
         [active? #:mutable] ; boolean
         [start-time #:mutable] ; integer (seconds since epoch)
         [compacting? #:mutable] ; boolean — guard against recursive compaction
         [last-compaction-time #:mutable] ; integer or #f — timestamp of last compaction
         [persisted? #:mutable] ; boolean — #f until directory + first write
         [pending-entries #:mutable] ; (listof message?) — buffered before persistence
         [thinking-level #:mutable] ; symbol — one of thinking-levels (#1153)
         [shutdown-requested? #:mutable] ; boolean — graceful shutdown flag (#1158)
         [force-shutdown? #:mutable]
         [prompt-running? #:mutable]) ; boolean — concurrent prompt execution guard
  #:transparent)
