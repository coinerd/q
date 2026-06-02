#lang racket/base

;; util/event-types.rkt — Shared event type constants (foundation layer)
;;
;; Extracted from extensions/message-inject.rkt (v0.33.0 W1: fix upward imports).
;; These are plain constants with no dependencies on extensions/ or runtime/.

(require racket/contract)

(provide (contract-out [injection-event-topic string?]))

;; Topic string for injection events on the event bus.
;; Used by runtime/agent-session and runtime/session-lifecycle to subscribe
;; to injection events without importing from extensions/.
(define injection-event-topic "injection")
