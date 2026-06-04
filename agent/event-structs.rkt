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

(provide
;; From event-structs/base.rkt
         (struct-out typed-event)
         typed-event?
         ;; From event-structs/turn-events.rkt
         (struct-out turn-start-event)
         make-turn-start-event
         turn-start-event-type
         turn-start-event-fields
         (struct-out turn-end-event)
         make-turn-end-event
         turn-end-event-type
         turn-end-event-fields
         ;; From event-structs/message-events.rkt
         (struct-out message-start-event)
         make-message-start-event
         message-start-event-type
         message-start-event-fields
         (struct-out message-update-event)
         make-message-update-event
         message-update-event-type
         message-update-event-fields
         (struct-out message-end-event)
         make-message-end-event
         message-end-event-type
         message-end-event-fields
         ;; From event-structs/tool-events.rkt
         (struct-out tool-execution-start-event)
         make-tool-execution-start-event
         tool-execution-start-event-type
         tool-execution-start-event-fields
         (struct-out tool-execution-update-event)
         make-tool-execution-update-event
         tool-execution-update-event-type
         tool-execution-update-event-fields
         (struct-out tool-execution-end-event)
         make-tool-execution-end-event
         tool-execution-end-event-type
         tool-execution-end-event-fields
         (struct-out tool-call-event)
         make-tool-call-event
         tool-call-event-type
         tool-call-event-fields
         (struct-out tool-result-event)
         make-tool-result-event
         tool-result-event-type
         tool-result-event-fields
         (struct-out bash-tool-call-event)
         (struct-out edit-tool-call-event)
         (struct-out write-tool-call-event)
         (struct-out read-tool-call-event)
         (struct-out grep-tool-call-event)
         (struct-out find-tool-call-event)
         (struct-out custom-tool-call-event)
         make-bash-tool-call-event
         bash-tool-call-event?
         bash-tool-call-event-fields
         make-edit-tool-call-event
         edit-tool-call-event?
         edit-tool-call-event-fields
         make-write-tool-call-event
         write-tool-call-event?
         write-tool-call-event-fields
         make-read-tool-call-event
         read-tool-call-event?
         read-tool-call-event-fields
         make-grep-tool-call-event
         grep-tool-call-event?
         grep-tool-call-event-fields
         make-find-tool-call-event
         find-tool-call-event?
         find-tool-call-event-fields
         make-custom-tool-call-event
         custom-tool-call-event?
         custom-tool-call-event-fields
         ;; From event-structs/provider-events.rkt
         (struct-out provider-request-event)
         make-provider-request-event
         provider-request-event-type
         provider-request-event-fields
         (struct-out provider-response-event)
         make-provider-response-event
         provider-response-event-type
         provider-response-event-fields
         (struct-out model-stream-delta-event)
         make-model-stream-delta-event
         model-stream-delta-event-type
         model-stream-delta-event-fields
         (struct-out model-stream-thinking-event)
         make-model-stream-thinking-event
         model-stream-thinking-event-type
         model-stream-thinking-event-fields
         (struct-out model-stream-completed-event)
         make-model-stream-completed-event
         model-stream-completed-event-type
         model-stream-completed-event-fields
         ;; From event-structs/session-events.rkt
         (struct-out session-start-event)
         make-session-start-event
         session-start-event-type
         session-start-event-fields
         (struct-out session-shutdown-event)
         make-session-shutdown-event
         session-shutdown-event-type
         session-shutdown-event-fields
         (struct-out input-event)
         make-input-event
         input-event-type
         input-event-fields
         (struct-out model-select-event)
         make-model-select-event
         model-select-event-type
         model-select-event-fields
         (struct-out agent-start-event)
         make-agent-start-event
         agent-start-event-type
         agent-start-event-fields
         (struct-out agent-end-event)
         make-agent-end-event
         agent-end-event-type
         agent-end-event-fields
         (struct-out context-event)
         make-context-event
         context-event-type
         context-event-fields
         (struct-out context-assembled-event)
         make-context-assembled-event
         context-assembled-event-type
         context-assembled-event-fields
         (struct-out context-blocked-event)
         make-context-blocked-event
         context-blocked-event-type
         context-blocked-event-fields
         (struct-out working-set-injected-event)
         make-working-set-injected-event
         working-set-injected-event-type
         working-set-injected-event-fields
         (struct-out context-assembly-detail-event)
         make-context-assembly-detail-event
         context-assembly-detail-event-type
         context-assembly-detail-event-fields
         (struct-out goal-start-event)
         make-goal-start-event
         goal-start-event-type
         goal-start-event-fields
         (struct-out goal-turn-start-event)
         make-goal-turn-start-event
         goal-turn-start-event-type
         goal-turn-start-event-fields
         (struct-out goal-evaluated-event)
         make-goal-evaluated-event
         goal-evaluated-event-type
         goal-evaluated-event-fields
         (struct-out goal-check-event)
         make-goal-check-event
         goal-check-event-type
         goal-check-event-fields
         (struct-out goal-achieved-event)
         make-goal-achieved-event
         goal-achieved-event-type
         goal-achieved-event-fields
         (struct-out goal-failed-event)
         make-goal-failed-event
         goal-failed-event-type
         goal-failed-event-fields
         ;; From event-structs/iteration-events.rkt
         (struct-out auto-retry-event)
         make-auto-retry-event
         auto-retry-event-type
         auto-retry-event-fields
         (struct-out auto-retry-start-event)
         make-auto-retry-start-event
         auto-retry-start-event-type
         auto-retry-start-event-fields
         (struct-out compaction-event)
         make-compaction-event
         compaction-event-type
         compaction-event-fields
         (struct-out injection-event)
         make-injection-event
         injection-event-type
         injection-event-fields
         (struct-out iteration-decision-event)
         make-iteration-decision-event
         iteration-decision-event-type
         iteration-decision-event-fields
         ;; From event-structs/hook-events.rkt
         (struct-out model-request-blocked-event)
         make-model-request-blocked-event
         model-request-blocked-event-type
         model-request-blocked-event-fields
         (struct-out message-blocked-event)
         make-message-blocked-event
         message-blocked-event-type
         message-blocked-event-fields
         (struct-out turn-cancelled-event)
         make-turn-cancelled-event
         turn-cancelled-event-type
         turn-cancelled-event-fields
         (struct-out assistant-message-completed-event)
         make-assistant-message-completed-event
         assistant-message-completed-event-type
         assistant-message-completed-event-fields
         ;; From event-structs/stream-events.rkt
         (struct-out stream-completed-event)
         make-stream-completed-event
         stream-completed-event-type
         stream-completed-event-fields
         (struct-out stream-delta-event)
         make-stream-delta-event
         stream-delta-event-type
         stream-delta-event-fields
         (struct-out stream-tool-call-delta-event)
         make-stream-tool-call-delta-event
         stream-tool-call-delta-event-type
         stream-tool-call-delta-event-fields
         (struct-out stream-thinking-event)
         make-stream-thinking-event
         stream-thinking-event-type
         stream-thinking-event-fields
         (struct-out stream-message-start-event)
         make-stream-message-start-event
         stream-message-start-event-type
         stream-message-start-event-fields
         (struct-out stream-message-delta-event)
         make-stream-message-delta-event
         stream-message-delta-event-type
         stream-message-delta-event-fields
         (struct-out stream-message-end-event)
         make-stream-message-end-event
         stream-message-end-event-type
         stream-message-end-event-fields
         (struct-out stream-turn-completed-event)
         make-stream-turn-completed-event
         stream-turn-completed-event-type
         stream-turn-completed-event-fields
         (struct-out stream-turn-cancelled-event)
         make-stream-turn-cancelled-event
         stream-turn-cancelled-event-type
         stream-turn-cancelled-event-fields
         (struct-out stream-tool-call-started-event)
         make-stream-tool-call-started-event
         stream-tool-call-started-event-type
         stream-tool-call-started-event-fields
         (struct-out stream-assistant-msg-completed-event)
         make-stream-assistant-msg-completed-event
         stream-assistant-msg-completed-event-type
         stream-assistant-msg-completed-event-fields
         ;; From browser/events.rkt (F10)
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
         browser-screenshot-captured-event-fields
)
