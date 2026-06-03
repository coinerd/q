#lang racket/base

;; BOUNDARY: integration

;; tests/test-session-types-grouping.rkt — Verify agent-session field groupings
;;
;; v0.32.8: Documents the 5 logical groups of agent-session's 21 fields.
;; This test ensures the grouping documentation stays accurate.

(require rackunit
         racket/runtime-path
         "../runtime/session/session-types.rkt"
         (only-in "../runtime/session/session-mutation.rkt"
                  guarded-set-compacting!
                  guarded-set-persisted!
                  guarded-set-prompt-running!
                  guarded-set-shutdown-requested!)
         "../runtime/session/session-config.rkt"
         "../agent/queue.rkt"
         "../agent/event-bus.rkt")

(define-runtime-path here ".")

;; Helper: create a minimal agent-session for field testing
(define (make-test-session)
  (agent-session "test-id"
                 "/tmp/test-session"
                 #f ; provider
                 #f ; tool-registry
                 (make-event-bus)
                 #f ; extension-registry
                 "test-model"
                 '("system instruction")
                 #f ; index
                 (make-queue)
                 (hasheq) ; config
                 #t ; active?
                 (current-seconds)
                 #f ; compacting?
                 #f ; last-compaction-time
                 #f ; persisted?
                 '() ; pending-entries
                 'off ; thinking-level
                 #f ; shutdown-requested?
                 #f ; force-shutdown?
                 #f)) ; prompt-running?

(test-case "agent-session has 21 fields"
  (define sess (make-test-session))
  (define vec (struct->vector sess))
  ;; struct->vector includes struct name at index 0
  (check-equal? (vector-length vec) 22 "agent-session should have 21 fields + name slot"))

(test-case "identity fields are accessible"
  (define sess (make-test-session))
  (check-equal? (agent-session-session-id sess) "test-id")
  (check-equal? (agent-session-session-dir sess) "/tmp/test-session")
  (check-true (exact-integer? (agent-session-start-time sess))))

(test-case "runtime service fields are accessible via convenience accessors"
  (define sess (make-test-session))
  ;; provider is #f for test
  (check-false (session-provider sess))
  ;; tool-registry is #f for test
  (check-false (session-tool-registry sess))
  ;; event-bus should be a real event-bus
  (check-pred event-bus? (session-event-bus sess))
  ;; extension-registry is #f for test
  (check-false (session-extension-registry sess)))

(test-case "runtime service accessors match direct field access"
  (define sess (make-test-session))
  (check-eq? (session-provider sess) (agent-session-provider sess))
  (check-eq? (session-tool-registry sess) (agent-session-tool-registry sess))
  (check-eq? (session-event-bus sess) (agent-session-event-bus sess))
  (check-eq? (session-extension-registry sess) (agent-session-extension-registry sess)))

(test-case "mutable lifecycle flags default correctly"
  (define sess (make-test-session))
  (check-false (agent-session-compacting? sess))
  (check-false (agent-session-persisted? sess))
  (check-false (agent-session-prompt-running? sess))
  (check-false (agent-session-shutdown-requested? sess))
  (check-false (agent-session-force-shutdown? sess))
  (check-true (agent-session-active? sess)))

(test-case "mutable lifecycle flags can be set"
  (define sess (make-test-session))
  (guarded-set-compacting! sess #t)
  (check-true (agent-session-compacting? sess))
  (guarded-set-persisted! sess #t)
  (check-true (agent-session-persisted? sess))
  (guarded-set-prompt-running! sess #t)
  (check-true (agent-session-prompt-running? sess))
  (guarded-set-shutdown-requested! sess #t)
  (check-true (agent-session-shutdown-requested? sess)))

(test-case "session-log-path constructs correct path"
  (check-equal? (session-log-path "/tmp/sess") (string->path "/tmp/sess/session.jsonl")))
