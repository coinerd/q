#lang racket/base

;; @speed fast  ;; @suite runtime

;; tests/test-session-rollout.rkt -- Per-session config + rollout gate tests
;; v0.76.3 W0: Feature-flag activation with deterministic A/B assignment.

(require rackunit
         rackunit/text-ui
         racket/dict
         (only-in "../runtime/session/session-config.rkt"
                  session-config?
                  hash->session-config
                  current-task-state-aware-rollout-rate
                  config-task-state-aware?)
         (only-in "../runtime/agent-session.rkt" make-agent-session session-rollout-enabled?)
         (only-in "../agent/event-bus.rkt" make-event-bus)
         (only-in "../tools/tool.rkt" make-tool-registry)
         (only-in "../runtime/session/session-types.rkt" agent-session-config))

(define (make-test-cfg)
  (hash 'session-dir
        "/tmp/q-test-sessions"
        'event-bus
        (make-event-bus)
        'tool-registry
        (make-tool-registry)))

(define suite
  (test-suite "session-rollout"

    (test-case "default is #f when key missing"
      (define cfg (hash->session-config (hash)))
      (check-equal? (config-task-state-aware? cfg) #f))

    (test-case "returns #t when set"
      (define cfg (hash->session-config (hash 'task-state-aware? #t)))
      (check-equal? (config-task-state-aware? cfg) #t))

    (test-case "returns #f when explicitly set to #f"
      (define cfg (hash->session-config (hash 'task-state-aware? #f)))
      (check-equal? (config-task-state-aware? cfg) #f))

    (test-case "rollout-enabled? returns #f when rate is 0.0"
      (parameterize ([current-task-state-aware-rollout-rate 0.0])
        (check-false (session-rollout-enabled? "test-id-123"))
        (check-false (session-rollout-enabled? "any-id"))))

    (test-case "rollout-enabled? returns #t when rate is 1.0"
      (parameterize ([current-task-state-aware-rollout-rate 1.0])
        (check-true (session-rollout-enabled? "test-id-123"))
        (check-true (session-rollout-enabled? "any-id"))))

    (test-case "rollout-enabled? is deterministic for same ID"
      (parameterize ([current-task-state-aware-rollout-rate 0.5])
        (define r1 (session-rollout-enabled? "abc"))
        (define r2 (session-rollout-enabled? "abc"))
        (check-equal? r1 r2)))

    (test-case "rollout-enabled? at 0.5 gives mixed results across IDs"
      (parameterize ([current-task-state-aware-rollout-rate 0.5])
        (define results
          (for/list ([i (in-range 100)])
            (session-rollout-enabled? (format "session-~a" i))))
        (define enabled (length (filter values results)))
        (check-true (> enabled 10) (format "Expected some enabled, got ~a" enabled))
        (check-true (< enabled 90) (format "Expected some disabled, got ~a" enabled))))

    (test-case "rate 0.0 means no sessions get the flag"
      (parameterize ([current-task-state-aware-rollout-rate 0.0])
        (for ([i (in-range 10)])
          (define sess (make-agent-session (make-test-cfg)))
          (define cfg (agent-session-config sess))
          (check-equal? (config-task-state-aware? cfg) #f))))

    (test-case "rate 1.0 means all sessions get the flag"
      (parameterize ([current-task-state-aware-rollout-rate 1.0])
        (for ([i (in-range 10)])
          (define sess (make-agent-session (make-test-cfg)))
          (define cfg (agent-session-config sess))
          (check-equal? (config-task-state-aware? cfg) #t))))

    (test-case "task-state-aware? is accessible via dict-ref"
      (define cfg (hash->session-config (hash 'task-state-aware? #t)))
      (check-equal? (dict-ref cfg 'task-state-aware?) #t))))

(run-tests suite)
