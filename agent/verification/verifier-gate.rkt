#lang racket/base

;; agent/verification/verifier-gate.rkt — GSD Verification Gate
;; STABILITY: evolving
;;
;; W5 (v0.99.5): Wires run-verification into the GSD state machine.
;; When current-verifier-enabled is #t, the executing→verifying transition
;; triggers this gate, which calls the verifier LLM and routes based on verdict:
;;   approve   → transition to idle (wave accepted)
;;   reject    → transition to executing (wave rework needed)
;;   escalate  → emit escalation event, pause for human review (HITL)
;;
;; Feature-gated: when current-verifier-enabled is #f (default), this gate
;; is a no-op — existing manual done/rework transitions are preserved.
;;
;; Part of MAS Schritt 3: Verifier-Agent (milestone #791).

(require racket/contract
         racket/match
         "verifier-core.rkt"
         "verifier-types.rkt"
         "../event-structs/verification-events.rkt"
         (only-in "../../extensions/gsd/session-state.rkt"
                  gsd-session-ctx?
                  gsd-ctx-state-snapshot
                  gsd-ctx-state-update!
                  current-gsd-session-id)
         (only-in "../../extensions/gsd/state-machine.rkt"
                  gsd-runtime-state-mode
                  gsm-ctx-current
                  gsm-ctx-transition!)
         (only-in "../../extensions/gsd/events.rkt" ctx-emit-gsd-event! gsd-event-names))

;; ============================================================
;; Gate Result Type
;; ============================================================

;; The gate returns one of three symbols indicating the outcome.
(define GATE-RESULTS '(approved rejected escalated))

(define (gate-result? v)
  (and (symbol? v) (memq v GATE-RESULTS) #t))

;; ============================================================
;; Plan Context Extraction
;; ============================================================

;; Extract verifier inputs from a plan-context hash.
;; Returns (values plan-summary wave-name files-changed test-summary diff-excerpt).
(define (extract-plan-context plan-context)
  (values (hash-ref plan-context 'plan-summary "Plan summary not available")
          (hash-ref plan-context 'wave-name "unknown-wave")
          (hash-ref plan-context 'files-changed '())
          (hash-ref plan-context 'test-summary "Test summary not available")
          (hash-ref plan-context 'diff-excerpt "")))

;; ============================================================
;; v0.99.22 §6.1: Complexity Heuristic — Verifier Skip
;; ============================================================

;; Determine whether a wave is trivial enough to skip LLM-based verification.
;; A wave is "trivial" (auto-approved) when ALL of:
;;   1. ≤ 2 files changed
;;   2. Capabilities are explicitly provided AND contain only read-only
;;      (no shell-exec, git-write, or file-write)
;;
;; Conservative design: when 'capabilities-used is absent from plan-context,
;; returns #f (verify). This preserves backward compatibility with existing
;; callers that don't populate the capabilities-used field.
(define (should-skip-verification? plan-context)
  (define files-changed (hash-ref plan-context 'files-changed '()))
  (define capabilities-used (hash-ref plan-context 'capabilities-used #f))
  (and capabilities-used
       (list? capabilities-used)
       (<= (length files-changed) 2)
       (not (member 'shell-exec capabilities-used))
       (not (member 'git-write capabilities-used))
       (not (member 'file-write capabilities-used))))

;; ============================================================
;; Main Gate Entry Point
;; ============================================================

;; Execute the verification gate.
;;
;; When current-verifier-enabled is #f, returns 'approved immediately
;; (preserving existing behavior — no verification).
;;
;; When enabled:
;;   1. Emit verification-started event
;;   2. Call run-verification
;;   3. Emit verification-completed event
;;   4. Route based on verdict:
;;      approve  → transition verifying→idle via 'done, return 'approved
;;      reject   → transition verifying→executing via 'rework, return 'rejected
;;      escalate → emit verification-escalated event, return 'escalated (HITL pause)
(define (execute-verification-gate ctx plan-context)
  (define provider (current-verifier-provider))
  (cond
    ;; Feature flag OFF → skip verification entirely
    [(not (current-verifier-enabled))
     ;; Auto-transition to idle (preserve existing behavior)
     (gsm-ctx-transition! ctx 'idle #:event 'done)
     'approved]
    ;; v0.99.22 §6.1: Trivial wave → skip verification, auto-approve
    [(should-skip-verification? plan-context)
     (gsm-ctx-transition! ctx 'idle #:event 'done)
     'approved]
    ;; No provider available → auto-approve (safe default)
    [(not provider)
     (gsm-ctx-transition! ctx 'idle #:event 'done)
     'approved]
    [else
     ;; Feature flag ON → run verifier
     (define-values (plan-summary wave-name files-changed test-summary diff-excerpt)
       (extract-plan-context plan-context))

     ;; 1. Emit verification-started event
     (ctx-emit-gsd-event! ctx
                          'gsd.verification.started
                          (make-verification-started-event #:session-id (current-gsd-session-id)
                                                           #:turn-id 0
                                                           #:artifact-count (length files-changed)))

     ;; 2. Call run-verification (safe — never throws)
     (define decision
       (run-verification provider
                         plan-summary
                         wave-name
                         files-changed
                         test-summary
                         #:diff-excerpt diff-excerpt))

     ;; 3. Emit verification-completed event
     (ctx-emit-gsd-event! ctx
                          'gsd.verification.completed
                          (make-verification-completed-event
                           #:session-id (current-gsd-session-id)
                           #:turn-id 0
                           #:verdict (verifier-decision-verdict decision)
                           #:reason (verifier-decision-reason decision)
                           #:risk-level (verifier-decision-risk-level decision)
                           #:requires-human (verifier-decision-requires-human? decision)))

     ;; 4. Route based on verdict
     (match (verifier-decision-verdict decision)
       ['approve
        (gsm-ctx-transition! ctx 'idle #:event 'done)
        'approved]
       ['reject
        (gsm-ctx-transition! ctx 'executing #:event 'rework)
        'rejected]
       ['escalate
        ;; Emit escalation event for HITL
        (ctx-emit-gsd-event! ctx
                             'gsd.verification.escalated
                             (make-verification-escalated-event
                              #:session-id (current-gsd-session-id)
                              #:turn-id 0
                              #:reason (verifier-decision-reason decision)
                              #:risk-level (verifier-decision-risk-level decision)
                              #:artifact-refs (verifier-decision-artifact-refs decision)))
        'escalated])]))

;; ============================================================
;; Guard: Should the gate run?
;; ============================================================

;; Check if the verification gate should run for the given context.
;; Returns #t when the FSM is in 'verifying state AND verifier is enabled.
;; When plan-context is provided, also returns #f if the wave is trivial (§6.1).
(define (should-run-verification-gate? ctx [plan-context #f])
  (and (current-verifier-enabled)
       (eq? (gsm-ctx-current ctx) 'verifying)
       (or (not plan-context) (not (should-skip-verification? plan-context)))))

;; ============================================================
;; Provides
;; ============================================================

(provide current-verifier-enabled
         GATE-RESULTS
         gate-result?)

(provide (contract-out
          [execute-verification-gate (-> gsd-session-ctx? hash? gate-result?)]
          [should-run-verification-gate? (->* (gsd-session-ctx?) ((or/c hash? #f)) boolean?)]
          [extract-plan-context (-> hash? (values string? string? (listof string?) string? string?))]
          [should-skip-verification? (-> hash? boolean?)]))
