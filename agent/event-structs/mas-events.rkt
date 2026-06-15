#lang racket/base

;; agent/event-structs/mas-events.rkt — MAS coordination events
;; STABILITY: evolving
;;
;; W3 (v0.99.7): Five typed events for MAS inter-agent coordination:
;;   - mas-artifact-produced-event  — emitted when an agent produces an artifact
;;   - mas-test-result-event        — emitted when a test result is recorded
;;   - mas-hypothesis-opened-event  — emitted when a hypothesis/question is opened
;;   - mas-hypothesis-resolved-event — emitted when a hypothesis is resolved
;;   - mas-blackboard-sync-event    — emitted for blackboard state synchronization
;;
;; Part of MAS Schritt 4: Blackboard & Event Log (milestone #793).

(require "base.rkt"
         "../../util/event/event-macro.rkt")

;; ============================================================
;; Artifact Production
;; ============================================================

;; Emitted when an agent produces a deliverable artifact.
;; name: human-readable artifact name
;; path: file path or URI
;; artifact-type: symbol like 'source, 'test, 'plan, 'doc
(define-typed-event mas-artifact-produced-event
                    "mas.artifact.produced"
                    (name path artifact-type)
                    #:optional ([agent-name #f])
                    #:schema-version 1)

;; ============================================================
;; Test Results
;; ============================================================

;; Emitted when a test result is recorded.
;; file: test file path
;; result: 'pass | 'fail | 'error | 'skip
(define-typed-event mas-test-result-event
                    "mas.test.result"
                    (file result)
                    #:optional ([duration-ms #f] [test-count #f] [pass-count #f])
                    #:schema-version 1)

;; ============================================================
;; Hypothesis Tracking
;; ============================================================

;; Emitted when an agent opens a hypothesis/question for investigation.
;; id: unique hypothesis identifier
;; question: the question being investigated
(define-typed-event mas-hypothesis-opened-event
                    "mas.hypothesis.opened"
                    (id question)
                    #:optional ([agent-name #f] [priority 'normal])
                    #:schema-version 1)

;; Emitted when a hypothesis is resolved or closed.
;; id: the hypothesis identifier being resolved
(define-typed-event mas-hypothesis-resolved-event
                    "mas.hypothesis.resolved"
                    (id)
                    #:optional ([resolution #f] [resolved-by #f])
                    #:schema-version 1)

;; ============================================================
;; Blackboard Sync
;; ============================================================

;; Emitted when a blackboard snapshot is synchronized.
;; state-snapshot: hash with key blackboard fields for sync
(define-typed-event mas-blackboard-sync-event
                    "mas.blackboard.sync"
                    (state-snapshot)
                    #:optional ([source-agent #f])
                    #:schema-version 1)

;; ============================================================
;; Agent Version Pinning (v0.99.8)
;; ============================================================

;; Emitted when agent versions are pinned for a session.
;; This ensures mid-session consistency — once pinned, the session
;; uses the pinned versions even if new versions are activated
;; mid-session for future sessions.
;; role-name: symbol identifying the agent role
;; version: string version that was pinned
(define-typed-event mas-agent-version-pinned-event
                    "mas.agent.version.pinned"
                    (role-name version)
                    #:schema-version 1)

;; ============================================================
;; Hot-Swap Registry Events (v0.99.8 W5)
;; ============================================================

;; Emitted when a new agent version is registered in the registry.
;; role-name: symbol identifying the agent role
;; version: string version that was registered
(define-typed-event mas-agent-registered-event
                    "mas.agent.registered"
                    (role-name version)
                    #:schema-version 1)

;; Emitted when an agent version is activated (hot-swap).
;; role-name: symbol identifying the agent role
;; version: string version that was activated
(define-typed-event mas-agent-activated-event
                    "mas.agent.activated"
                    (role-name version)
                    #:schema-version 1)

;; ============================================================
;; MCP Adapter Events (v0.99.9 W5)
;; ============================================================

;; Emitted when an MCP server connection is established.
;; server-name: name of the MCP server connected to
(define-typed-event mas-mcp-connected-event "mas.mcp.connected" (server-name) #:schema-version 1)

;; Emitted when an MCP tool is called.
;; tool-name: name of the MCP tool invoked
;; server-name: optional, the server providing the tool
(define-typed-event mas-mcp-tool-called-event
                    "mas.mcp.tool.called"
                    (tool-name)
                    #:optional ([server-name #f])
                    #:schema-version 1)
