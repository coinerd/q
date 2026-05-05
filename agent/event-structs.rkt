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
;;
;; v0.30.6 status: 30 typed event structs across 8 sub-modules.
;;   - runtime/ files: 15 emit-typed-event! sites (fully migrated)
;;   - agent/loop*.rkt: 28 raw emit! sites (use dotted event names)
;;   - These two systems coexist: typed events use hyphenated names
;;     ("turn-start"), raw emit! uses dotted names ("turn.started").
;;   - New hook-events.rkt sub-module adds: model-request-blocked,
;;     message-blocked, turn-cancelled, assistant-message-completed
;;   - New provider streaming events: model-stream-delta,
;;     model-stream-thinking, model-stream-completed

(require "event-structs/base.rkt"
         "event-structs/turn-events.rkt"
         "event-structs/message-events.rkt"
         "event-structs/tool-events.rkt"
         "event-structs/provider-events.rkt"
         "event-structs/session-events.rkt"
         "event-structs/iteration-events.rkt"
         "event-structs/hook-events.rkt")

(provide (all-from-out "event-structs/base.rkt")
         (all-from-out "event-structs/turn-events.rkt")
         (all-from-out "event-structs/message-events.rkt")
         (all-from-out "event-structs/tool-events.rkt")
         (all-from-out "event-structs/provider-events.rkt")
         (all-from-out "event-structs/session-events.rkt")
         (all-from-out "event-structs/iteration-events.rkt")
         (all-from-out "event-structs/hook-events.rkt"))
