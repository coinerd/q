#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; tests/test-stream-loop-w1.rkt — Streaming decomposition & loop cleanup tests (W-06, I-01)
;; Updated v0.46.10: emit-turn-start!/build-turn-context deprecated, now test phase functions

(require rackunit
         "../agent/loop.rkt"
         "../agent/loop-phases.rkt"
         "../agent/loop-fsm.rkt"
         "../agent/event-bus.rkt"
         "../agent/state.rkt"
         "../util/message/message.rkt")

;; ============================================================
;; I-01: Main entry point exists
;; ============================================================

(test-case "run-agent-turn is exported"
  (check-true (procedure? run-agent-turn)))

;; ============================================================
;; W-06: stream-from-provider is exported (existing contract)
;; ============================================================

(test-case "stream-from-provider is exported"
  (check-true (procedure? stream-from-provider)))

;; ============================================================
;; Phase functions exist (replaces deprecated helpers)
;; ============================================================

(test-case "phase-emit-start is exported from loop-phases"
  (check-true (procedure? phase-emit-start)))

(test-case "phase-build-context is exported from loop-phases"
  (check-true (procedure? phase-build-context)))

(test-case "phase-build-request is exported from loop-phases"
  (check-true (procedure? phase-build-request)))

(test-case "phase-pre-hook is exported from loop-phases"
  (check-true (procedure? phase-pre-hook)))

;; ============================================================
;; Phase 1: phase-emit-start returns values + effects
;; ============================================================

(test-case "phase-emit-start returns context and effects"
  (define st (make-loop-state "s1" "t1"))
  (define context '())
  (define-values (ctx effects) (phase-emit-start "s1" "t1" st context))
  (check-equal? ctx context)
  (check-true (list? effects))
  (check-true (>= (length effects) 1)))

;; ============================================================
;; Phase 2: phase-build-context returns raw-messages + effects
;; ============================================================

(test-case "phase-build-context returns raw-messages and effects"
  (define bus (make-event-bus))
  (define st (make-loop-state "s1" "t1"))
  (define context (list (message "m1" #f 'user 'message "hello" (current-seconds) (hasheq))))
  (define-values (raw-msgs effects) (phase-build-context bus "s1" "t1" st context))
  (check-true (list? raw-msgs))
  (check-true (list? effects))
  (check-true (>= (length effects) 1)))
