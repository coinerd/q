#lang racket/base

;; agent/event-structs.rkt -- facade re-exporting all event struct definitions
;;
;; ARCH-05: Decomposed into focused sub-modules:
;;   - event-structs/base.rkt            -- typed-event base struct
;;   - event-structs/turn-events.rkt     -- turn start/end events
;;   - event-structs/message-events.rkt  -- message lifecycle events
;;   - event-structs/tool-events.rkt     -- tool execution + per-tool events
;;   - event-structs/provider-events.rkt -- provider/LLM events
;;   - event-structs/session-events.rkt  -- session/input/model/agent/context events
;;   - event-structs/iteration-events.rkt -- auto-retry/compaction/injection events
;;
;; This file re-exports everything for backward compatibility.
;; New code should import the focused sub-modules directly.

(require "event-structs/base.rkt"
         "event-structs/turn-events.rkt"
         "event-structs/message-events.rkt"
         "event-structs/tool-events.rkt"
         "event-structs/provider-events.rkt"
         "event-structs/session-events.rkt"
         "event-structs/iteration-events.rkt")

(provide (all-from-out "event-structs/base.rkt")
         (all-from-out "event-structs/turn-events.rkt")
         (all-from-out "event-structs/message-events.rkt")
         (all-from-out "event-structs/tool-events.rkt")
         (all-from-out "event-structs/provider-events.rkt")
         (all-from-out "event-structs/session-events.rkt")
         (all-from-out "event-structs/iteration-events.rkt"))
