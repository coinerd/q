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
;;   - event-structs/hook-events.rkt     -- hook interception events
;;   - event-structs/stream-events.rkt   -- streaming events
;;
;; This file re-exports everything for backward compatibility.
;; W11 (v0.72.6): Replaced wildcard re-exports with explicit identifier lists
;; to prevent accidental export leakage from sub-module changes.
;;
;; v0.30.6 status: 30+ typed event structs across 9 sub-modules.
;;
;; v0.96.1: Event API classification:
;;   PUBLIC (extension-facing, stability commitment):
;;     typed-event, context-event, injection-event, turn-start-event
;;     These are consumed by extensions/, gui/, tui/ and changing their
;;     field names or removing accessors is a BREAKING CHANGE.
;;   INTERNAL (runtime-internal, no stability commitment):
;;     All other events. Consumers are in agent/, runtime/, llm/, tools/.
;;     Field names may change between minor versions.

(require "event-structs/base.rkt"
         "event-structs/turn-events.rkt"
         "event-structs/message-events.rkt"
         "event-structs/tool-events.rkt"
         "event-structs/provider-events.rkt"
         "event-structs/session-events.rkt"
         "event-structs/iteration-events.rkt"
         "event-structs/hook-events.rkt"
         "event-structs/stream-events.rkt"
         ;; F10: browser events re-exported from facade
         (only-in "../browser/events.rkt"
                  browser-session-opened-event
                  browser-session-opened-event?
                  make-browser-session-opened-event
                  browser-session-opened-event-type
                  browser-session-opened-event-fields
                  browser-session-closed-event
                  browser-session-closed-event?
                  make-browser-session-closed-event
                  browser-session-closed-event-type
                  browser-session-closed-event-fields
                  browser-action-started-event
                  browser-action-started-event?
                  make-browser-action-started-event
                  browser-action-started-event-type
                  browser-action-started-event-fields
                  browser-action-completed-event
                  browser-action-completed-event?
                  make-browser-action-completed-event
                  browser-action-completed-event-type
                  browser-action-completed-event-fields
                  browser-action-failed-event
                  browser-action-failed-event?
                  make-browser-action-failed-event
                  browser-action-failed-event-type
                  browser-action-failed-event-fields
                  browser-page-loaded-event
                  browser-page-loaded-event?
                  make-browser-page-loaded-event
                  browser-page-loaded-event-type
                  browser-page-loaded-event-fields
                  browser-policy-blocked-event
                  browser-policy-blocked-event?
                  make-browser-policy-blocked-event
                  browser-policy-blocked-event-type
                  browser-policy-blocked-event-fields
                  browser-sidecar-started-event
                  browser-sidecar-started-event?
                  make-browser-sidecar-started-event
                  browser-sidecar-started-event-type
                  browser-sidecar-started-event-fields
                  browser-sidecar-stopped-event
                  browser-sidecar-stopped-event?
                  make-browser-sidecar-stopped-event
                  browser-sidecar-stopped-event-type
                  browser-sidecar-stopped-event-fields
                  browser-screenshot-captured-event
                  browser-screenshot-captured-event?
                  make-browser-screenshot-captured-event
                  browser-screenshot-captured-event-type
                  browser-screenshot-captured-event-fields))

;; From event-structs/base.rkt
;; PUBLIC API — stable for extensions
;; Re-export everything from all sub-modules
(provide (all-from-out "event-structs/base.rkt")
         (all-from-out "event-structs/turn-events.rkt")
         (all-from-out "event-structs/message-events.rkt")
         (all-from-out "event-structs/tool-events.rkt")
         (all-from-out "event-structs/provider-events.rkt")
         (all-from-out "event-structs/session-events.rkt")
         (all-from-out "event-structs/iteration-events.rkt")
         (all-from-out "event-structs/hook-events.rkt")
         (all-from-out "event-structs/stream-events.rkt")
         ;; Browser events
         browser-session-opened-event
         browser-session-opened-event?
         make-browser-session-opened-event
         browser-session-opened-event-type
         browser-session-opened-event-fields
         browser-session-closed-event
         browser-session-closed-event?
         make-browser-session-closed-event
         browser-session-closed-event-type
         browser-session-closed-event-fields)
