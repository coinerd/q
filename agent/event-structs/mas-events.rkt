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
