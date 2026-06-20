#lang racket/base

;; agent/event-json-helpers.rkt — Pure serialization boundary for typed events
;;
;; W4 v0.99.35: Extracted from event-json.rkt to separate pure data
;; extraction/conversion from registry mutation side-effects.
;;
;; All functions in this module are pure (no I/O, no mutation, no
;; parameters, no registry access). They can be tested in isolation.
;;
;; Boundary contract:
;;   INPUTS:  typed-event structs, hashes (plain data)
;;   OUTPUTS: hashes, strings, typed-event structs (plain data)
;;   EFFECTS: None — safe to call from any context

(require racket/match
         (only-in "event-structs/base.rkt"
                  typed-event
                  typed-event-type
                  typed-event-timestamp
                  typed-event-session-id
                  typed-event-turn-id)
         (only-in "event-structs/tool-events.rkt"
                  bash-tool-call-event
                  bash-tool-call-event?
                  bash-tool-call-event-command
                  bash-tool-call-event-timeout
                  bash-tool-call-event-cwd
                  edit-tool-call-event
                  edit-tool-call-event?
                  edit-tool-call-event-path
                  edit-tool-call-event-edits
                  write-tool-call-event
                  write-tool-call-event?
                  write-tool-call-event-path
                  write-tool-call-event-content
                  read-tool-call-event
                  read-tool-call-event?
                  read-tool-call-event-path
                  read-tool-call-event-offset
                  read-tool-call-event-limit
                  grep-tool-call-event
                  grep-tool-call-event?
                  grep-tool-call-event-pattern
                  grep-tool-call-event-path
                  grep-tool-call-event-glob
                  find-tool-call-event
                  find-tool-call-event?
                  find-tool-call-event-pattern
                  find-tool-call-event-path
                  custom-tool-call-event
                  custom-tool-call-event?
                  tool-call-event-tool-call-id
                  tool-call-event-tool-name
                  tool-call-event-arguments))

;; Pure mappings
(provide event-name->tool-name
         all-known-event-types
         ;; Per-tool serializer cores (pure: evt -> hasheq)
         serialize-bash-tool-call
         serialize-edit-tool-call
         serialize-write-tool-call
         serialize-read-tool-call
         serialize-grep-tool-call
         serialize-find-tool-call
         serialize-custom-tool-call
         ;; Per-tool deserializer cores (pure: type ts sid tid h -> event)
         deserialize-bash-tool-call
         deserialize-edit-tool-call
         deserialize-write-tool-call
         deserialize-read-tool-call
         deserialize-grep-tool-call
         deserialize-find-tool-call
         deserialize-custom-tool-call)

;; ============================================================
;; Pure mappings
;; ============================================================

(define (event-name->tool-name type)
  (match type
    ["tool.bash.called" "bash"]
    ["tool.edit.called" "edit"]
    ["tool.write.called" "write"]
    ["tool.read.called" "read"]
    ["tool.grep.called" "grep"]
    ["tool.find.called" "find"]
    [_ #f]))

(define (all-known-event-types)
  '("turn.started" "turn.completed"
                   "message.started"
                   "message.updated"
                   "message.completed"
                   "tool.execution.started"
                   "tool.execution.updated"
                   "tool.execution.completed"
                   "tool.called"
                   "tool.result"
                   "tool.bash.called"
                   "tool.edit.called"
                   "tool.write.called"
                   "tool.read.called"
                   "tool.grep.called"
                   "tool.find.called"
                   "tool.custom.called"
                   "model.request.started"
                   "model.request.completed"
                   "session.started"
                   "session.shutdown"
                   "input"
                   "model.selected"
                   "agent.started"
                   "agent.completed"
                   "context.built"
                   "context.assembled"
                   "context.assembly.blocked"
                   "working-set.injected"
                   "iteration.decision"
                   "auto-retry.start"
                   "model.stream.delta"
                   "model.stream.delta.tool-call"
                   "model.stream.thinking"
                   "model.stream.completed"
                   "tool.call.started"
                   "provider.stream.delta"
                   "provider.stream.thinking"
                   "provider.stream.completed"
                   "model.request.blocked"
                   "message.blocked"
                   "turn.cancelled"
                   "assistant.message.completed"
                   "browser.session.opened"
                   "browser.session.closed"
                   "browser.action.started"
                   "browser.action.completed"
                   "browser.action.failed"
                   "browser.page.loaded"
                   "browser.policy.blocked"
                   "browser.sidecar.started"
                   "browser.sidecar.stopped"
                   "browser.screenshot.captured"))

;; ============================================================
;; Per-tool serializer cores — pure: evt -> hasheq
;; ============================================================

(define (serialize-bash-tool-call evt)
  (hasheq 'toolName
          "bash"
          'toolCallId
          (tool-call-event-tool-call-id evt)
          'command
          (bash-tool-call-event-command evt)
          'timeout
          (bash-tool-call-event-timeout evt)
          'cwd
          (bash-tool-call-event-cwd evt)))

(define (serialize-edit-tool-call evt)
  (hasheq 'toolName
          "edit"
          'toolCallId
          (tool-call-event-tool-call-id evt)
          'path
          (edit-tool-call-event-path evt)
          'edits
          (edit-tool-call-event-edits evt)))

(define (serialize-write-tool-call evt)
  (hasheq 'toolName
          "write"
          'toolCallId
          (tool-call-event-tool-call-id evt)
          'path
          (write-tool-call-event-path evt)
          'content
          (write-tool-call-event-content evt)))

(define (serialize-read-tool-call evt)
  (hasheq 'toolName
          "read"
          'toolCallId
          (tool-call-event-tool-call-id evt)
          'path
          (read-tool-call-event-path evt)
          'offset
          (read-tool-call-event-offset evt)
          'limit
          (read-tool-call-event-limit evt)))

(define (serialize-grep-tool-call evt)
  (hasheq 'toolName
          "grep"
          'toolCallId
          (tool-call-event-tool-call-id evt)
          'pattern
          (grep-tool-call-event-pattern evt)
          'path
          (grep-tool-call-event-path evt)
          'glob
          (grep-tool-call-event-glob evt)))

(define (serialize-find-tool-call evt)
  (hasheq 'toolName
          "find"
          'toolCallId
          (tool-call-event-tool-call-id evt)
          'pattern
          (find-tool-call-event-pattern evt)
          'path
          (find-tool-call-event-path evt)))

(define (serialize-custom-tool-call evt)
  (hasheq 'toolName
          (tool-call-event-tool-name evt)
          'toolCallId
          (tool-call-event-tool-call-id evt)
          'arguments
          (tool-call-event-arguments evt)))

;; ============================================================
;; Per-tool deserializer cores — pure: type ts sid tid h -> event
;; ============================================================

(define (deserialize-bash-tool-call type ts sid tid h)
  (bash-tool-call-event type
                        ts
                        sid
                        tid
                        "bash"
                        (hasheq)
                        (hash-ref h 'toolCallId "")
                        (hash-ref h 'command "")
                        (hash-ref h 'timeout 30)
                        (hash-ref h 'cwd "")))

(define (deserialize-edit-tool-call type ts sid tid h)
  (edit-tool-call-event type
                        ts
                        sid
                        tid
                        "edit"
                        (hasheq)
                        (hash-ref h 'toolCallId "")
                        (hash-ref h 'path "")
                        (hash-ref h 'edits '())))

(define (deserialize-write-tool-call type ts sid tid h)
  (write-tool-call-event type
                         ts
                         sid
                         tid
                         "write"
                         (hasheq)
                         (hash-ref h 'toolCallId "")
                         (hash-ref h 'path "")
                         (hash-ref h 'content "")))

(define (deserialize-read-tool-call type ts sid tid h)
  (read-tool-call-event type
                        ts
                        sid
                        tid
                        "read"
                        (hasheq)
                        (hash-ref h 'toolCallId "")
                        (hash-ref h 'path "")
                        (hash-ref h 'offset #f)
                        (hash-ref h 'limit #f)))

(define (deserialize-grep-tool-call type ts sid tid h)
  (grep-tool-call-event type
                        ts
                        sid
                        tid
                        "grep"
                        (hasheq)
                        (hash-ref h 'toolCallId "")
                        (hash-ref h 'pattern "")
                        (hash-ref h 'path "")
                        (hash-ref h 'glob "")))

(define (deserialize-find-tool-call type ts sid tid h)
  (find-tool-call-event type
                        ts
                        sid
                        tid
                        "find"
                        (hasheq)
                        (hash-ref h 'toolCallId "")
                        (hash-ref h 'pattern "")
                        (hash-ref h 'path "")))

(define (deserialize-custom-tool-call type ts sid tid h)
  (custom-tool-call-event type
                          ts
                          sid
                          tid
                          (hash-ref h 'toolName "unknown")
                          (hash-ref h 'arguments (hasheq))
                          (hash-ref h 'toolCallId "")))
