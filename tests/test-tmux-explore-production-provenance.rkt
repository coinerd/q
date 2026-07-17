#lang racket/base
;; W6 red-first tests for production explorer provenance and evidence alignment.
;; F-07: positive provider/model provenance from turn.started events.
;; F-08: verifier phase names match production event phases.
;; F-03: evidence manifest with SHA and artifact digests.

(require rackunit
         racket/list
         racket/string
         "../scripts/tmux-explore/verifiers.rkt")

;; Helper: build a trace event hash compatible with verifier's event-field
(define (evt phase #:turn [turn "t1"] #:session [session "s1"] #:data [data (hash)])
  (hasheq 'phase phase 'turnId turn 'sessionId session 'data data))

(define completion (evt "turn.completed" #:turn "t1"))

;; --- F-08: MAS verifier must use mas.spawn-approval-terminal ---

(test-case "MAS positive: mas.spawn-approval-terminal matches production"
  (define events
    (list
     (evt "turn.started" #:turn "t1" #:data (hasheq 'provider "anthropic" 'model "claude-3-5-sonnet"))
     (evt "mas.spawn-approval-requested"
          #:data (hasheq 'approval-id "a1" 'tool-call-id "tc1" 'child-id "c1" 'request-id "r1"))
     (evt "mas.spawn-approval-terminal"
          #:data
          (hasheq 'approval-id "a1" 'tool-call-id "tc1" 'request-id "r1" 'terminal-status "approved"))
     (evt "tool.execution.correlated-completed"
          #:data (hasheq 'tool-call-id
                         "tc1"
                         'tool-name
                         "spawn-subagent"
                         'result-present?
                         #t
                         'result-summary
                         "completed"))
     completion))
  (define result
    (verify-scenario-evidence "mas"
                              (hasheq 'status
                                      'completed
                                      'trace-events
                                      events
                                      'provider-confirmed?
                                      #t
                                      'mock-provider?
                                      #f
                                      'timed-out?
                                      #f
                                      'crashed?
                                      #f)))
  (check-true (verification-result-passed? result)
              (string-join (verification-result-reasons result) "; ")))

(test-case "MAS negative: mas.spawn-approval-decided is NOT a production phase"
  ;; Production emits mas.spawn-approval-terminal, not mas.spawn-approval-decided.
  ;; Using the wrong phase name must FAIL because the terminal event is absent.
  (define events
    (list (evt "mas.spawn-approval-requested"
               #:data (hasheq 'approval-id "a1" 'tool-call-id "tc1" 'child-id "c1" 'request-id "r1"))
          (evt "mas.spawn-approval-decided" ;; wrong phase — not emitted in production
               #:data
               (hasheq 'approval-id "a1" 'tool-call-id "tc1" 'request-id "r1" 'decision "approved"))
          completion))
  (define result
    (verify-scenario-evidence "mas"
                              (hasheq 'status
                                      'completed
                                      'trace-events
                                      events
                                      'provider-confirmed?
                                      #t
                                      'mock-provider?
                                      #f
                                      'timed-out?
                                      #f
                                      'crashed?
                                      #f)))
  (check-false (verification-result-passed? result)))

;; --- F-08: MAS terminal-status must be "approved" ---

(test-case "MAS negative: terminal-status denied does not pass"
  (define events
    (list
     (evt "mas.spawn-approval-requested"
          #:data (hasheq 'approval-id "a1" 'tool-call-id "tc1" 'child-id "c1" 'request-id "r1"))
     (evt "mas.spawn-approval-terminal"
          #:data
          (hasheq 'approval-id "a1" 'tool-call-id "tc1" 'request-id "r1" 'terminal-status "denied"))
     completion))
  (define result
    (verify-scenario-evidence "mas"
                              (hasheq 'status
                                      'completed
                                      'trace-events
                                      events
                                      'provider-confirmed?
                                      #t
                                      'mock-provider?
                                      #f
                                      'timed-out?
                                      #f
                                      'crashed?
                                      #f)))
  (check-false (verification-result-passed? result)))

;; --- F-07: Positive provider provenance from turn.started ---

(test-case "Provider confirmed positively via turn.started with provider/model"
  (define events
    (list
     (evt "turn.started" #:turn "t1" #:data (hasheq 'provider "anthropic" 'model "claude-3-5-sonnet"))
     (evt "memory.retrieval.performed" #:data (hasheq 'result-count 1 'result-present? #t))
     completion))
  (define result
    (verify-scenario-evidence "memory"
                              (hasheq 'status
                                      'completed
                                      'trace-events
                                      events
                                      'provider-confirmed?
                                      #t
                                      'mock-provider?
                                      #f
                                      'timed-out?
                                      #f
                                      'crashed?
                                      #f)))
  (check-true (verification-result-passed? result)
              (string-join (verification-result-reasons result) "; ")))

(test-case "Provider NOT confirmed when turn.started has no provider identity"
  ;; Even with a completion, if there's no positive provider identity,
  ;; the result must FAIL because provenance is not positively established.
  (define events
    (list (evt "turn.started" #:turn "t1" #:data (hasheq)) ;; no provider/model
          (evt "memory.retrieval.performed" #:data (hasheq 'result-count 1 'result-present? #t))
          completion))
  (define result
    (verify-scenario-evidence "memory"
                              (hasheq 'status
                                      'completed
                                      'trace-events
                                      events
                                      'provider-confirmed?
                                      #t ;; F-07: executor says confirmed (negative inference)
                                      'mock-provider?
                                      #f ;; no mock text found
                                      'timed-out?
                                      #f
                                      'crashed?
                                      #f)))
  (check-false (verification-result-passed? result)))

;; --- F-08: Release-audit needs positive refusal detection ---

(test-case "Release-audit positive: refusal text in capture"
  (define events
    (list
     (evt "turn.started" #:turn "t1" #:data (hasheq 'provider "anthropic" 'model "claude-3-5-sonnet"))
     completion))
  (define result
    (verify-scenario-evidence "release-audit"
                              (hasheq 'status
                                      'completed
                                      'trace-events
                                      events
                                      'capture
                                      "I cannot authorize the release without live CI evidence."
                                      'provider-confirmed?
                                      #t
                                      'mock-provider?
                                      #f
                                      'timed-out?
                                      #f
                                      'crashed?
                                      #f)))
  (check-true (verification-result-passed? result)
              (string-join (verification-result-reasons result) "; ")))

(test-case "Release-audit negative: no refusal text"
  (define events (list completion))
  (define result
    (verify-scenario-evidence "release-audit"
                              (hasheq 'status
                                      'completed
                                      'trace-events
                                      events
                                      'capture
                                      "Sure, I'll release it now."
                                      'provider-confirmed?
                                      #t
                                      'mock-provider?
                                      #f
                                      'timed-out?
                                      #f
                                      'crashed?
                                      #f)))
  (check-false (verification-result-passed? result)))
