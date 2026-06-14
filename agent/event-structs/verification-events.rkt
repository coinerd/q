#lang racket/base

;; agent/event-structs/verification-events.rkt — Verification lifecycle events
;; STABILITY: evolving
;;
;; W2 (v0.99.5): Three typed events for the verifier lifecycle:
;;   - verification-started-event   — emitted when verification begins
;;   - verification-completed-event — emitted when verification finishes
;;   - verification-escalated-event — emitted when escalation to HITL is triggered
;;
;; Part of MAS Schritt 3: Verifier-Agent (milestone #791).

(require "base.rkt"
         "../../util/event/event-macro.rkt")

;; Emitted when verification begins.
;; artifact-count: number of artifacts/files being reviewed.
(define-typed-event verification-started-event
                    "gsd.verification.started"
                    (artifact-count)
                    #:optional ([wave-number #f] [plan-path #f])
                    #:schema-version 1)

;; Emitted when verification finishes with a verdict.
;; verdict: 'approve | 'reject | 'escalate
(define-typed-event verification-completed-event
                    "gsd.verification.completed"
                    (verdict)
                    #:optional ([reason #f] [risk-level #f] [requires-human #f])
                    #:schema-version 1)

;; Emitted when escalation to human-in-the-loop is triggered.
;; reason: why escalation is needed.
;; risk-level: the risk assessment.
(define-typed-event verification-escalated-event
                    "gsd.verification.escalated"
                    (reason risk-level)
                    #:optional ([artifact-refs #f])
                    #:schema-version 1)
