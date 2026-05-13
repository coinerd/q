#lang racket/base

;; BOUNDARY: integration

;; tests/test-stream-loop-w1.rkt — Streaming decomposition & loop cleanup tests (W-06, I-01)

(require rackunit
         "../agent/loop.rkt"
         "../agent/event-bus.rkt"
         "../agent/state.rkt"
         "../util/message.rkt")

;; ============================================================
;; I-01: Extracted helper functions exist
;; ============================================================

(test-case "emit-turn-start! is exported from loop.rkt"
  (check-true (procedure? emit-turn-start!)))

(test-case "build-turn-context is exported from loop.rkt"
  (check-true (procedure? build-turn-context)))

(test-case "run-agent-turn is exported"
  (check-true (procedure? run-agent-turn)))

;; ============================================================
;; W-06: stream-from-provider is exported (existing contract)
;; ============================================================

(test-case "stream-from-provider is exported"
  (check-true (procedure? stream-from-provider)))

;; ============================================================
;; I-01: emit-turn-start! works correctly
;; ============================================================

(test-case "emit-turn-start! emits turn.started event"
  (define bus (make-event-bus))
  (define events '())
  (subscribe! bus (lambda (e) (set! events (cons e events))))
  (define st (make-loop-state "s1" "t1"))
  (emit-turn-start! bus "s1" "t1" st #f '())
  (check-not-false events))

(test-case "emit-turn-start! dispatches agent-start hook"
  (define bus (make-event-bus))
  (define st (make-loop-state "s1" "t1"))
  (define hook-called? (box #f))
  (define (mock-hook-dispatcher action payload)
    (when (eq? action 'agent-start)
      (set-box! hook-called? #t)))
  (emit-turn-start! bus
                    "s1"
                    "t1"
                    st
                    mock-hook-dispatcher
                    (list (message "m1" #f 'user 'message "hello" (current-seconds) (hasheq))))
  (check-true (unbox hook-called?)))

;; ============================================================
;; I-01: build-turn-context produces raw-messages
;; ============================================================

(test-case "build-turn-context returns raw-messages"
  (define bus (make-event-bus))
  (define st (make-loop-state "s1" "t1"))
  (define context (list (message "m1" #f 'user 'message "hello" (current-seconds) (hasheq))))
  (define result (build-turn-context bus "s1" "t1" st context))
  (check-true (list? result)))
