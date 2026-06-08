#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: contract

;; tests/test-event-payload-contracts.rkt — Event payload contract validation
;; v0.22.8 W3 (T3)
;;
;; Golden payloads — version these as the event protocol evolves.
;; Adding new keys is OK. Removing or renaming keys will fail tests.

(require rackunit
         rackunit/text-ui
         "../util/event/event-contracts.rkt"
         "../agent/event-bus.rkt"
         "../runtime/runtime-helpers.rkt")

(define event-contract-tests
  (test-suite "Event Payload Contracts (v0.22.8 W3)"

    ;; -------------------------------------------------------
    ;; reason-payload/c
    ;; -------------------------------------------------------
    (test-case "reason-payload/c accepts valid payloads"
      (check-true (reason-payload/c (hasheq 'reason "extension-block")))
      (check-true (reason-payload/c (hasheq 'reason "extension-block" 'hook 'before-agent-start)))
      (check-true (reason-payload/c (hasheq 'reason "force-shutdown" 'iteration 3))))

    (test-case "reason-payload/c rejects missing reason"
      (check-false (reason-payload/c (hasheq 'foo "bar")))
      (check-false (reason-payload/c (hasheq))))

    ;; -------------------------------------------------------
    ;; session-id-payload/c
    ;; -------------------------------------------------------
    (test-case "session-id-payload/c accepts valid payloads"
      (check-true (session-id-payload/c (hasheq 'session-id "s1")))
      (check-true (session-id-payload/c (hasheq 'session-id "s1" 'extra "data"))))

    (test-case "session-id-payload/c rejects missing session-id"
      (check-false (session-id-payload/c (hasheq 'id "s1"))))

    ;; -------------------------------------------------------
    ;; iteration-payload/c
    ;; -------------------------------------------------------
    (test-case "iteration-payload/c accepts valid payloads"
      (check-true (iteration-payload/c (hasheq 'iteration 0)))
      (check-true (iteration-payload/c (hasheq 'iteration 5 'extra 'data))))

    (test-case "iteration-payload/c rejects missing iteration"
      (check-false (iteration-payload/c (hasheq 'count 5))))

    ;; -------------------------------------------------------
    ;; budget-payload/c
    ;; -------------------------------------------------------
    (test-case "budget-payload/c accepts valid payloads"
      (check-true (budget-payload/c (hasheq 'estimated-tokens 5000 'budget 8000 'max-tokens 128000))))

    (test-case "budget-payload/c rejects missing keys"
      (check-false (budget-payload/c (hasheq 'estimated-tokens 5000 'budget 8000)))
      (check-false (budget-payload/c (hasheq 'budget 8000 'max-tokens 128000)))
      (check-false (budget-payload/c (hasheq 'estimated-tokens 5000))))

    ;; -------------------------------------------------------
    ;; compact-result-payload/c
    ;; -------------------------------------------------------
    (test-case "compact-result-payload/c accepts valid payloads"
      (check-true (compact-result-payload/c (hasheq 'original-size 100 'new-size 50)))
      (check-true (compact-result-payload/c
                   (hasheq 'original-size 100 'new-size 50 'removed-count 50))))

    (test-case "compact-result-payload/c rejects missing keys"
      (check-false (compact-result-payload/c (hasheq 'original-size 100)))
      (check-false (compact-result-payload/c (hasheq 'new-size 50))))

    ;; -------------------------------------------------------
    ;; error-detail-payload/c
    ;; -------------------------------------------------------
    (test-case "error-detail-payload/c accepts valid payloads"
      (check-true (error-detail-payload/c (hasheq 'error "something went wrong")))
      (check-true (error-detail-payload/c (hasheq 'error "overflow" 'iteration 5))))

    (test-case "error-detail-payload/c rejects missing error key"
      (check-false (error-detail-payload/c (hasheq 'message "not error key"))))

    ;; -------------------------------------------------------
    ;; injection-count-payload/c
    ;; -------------------------------------------------------
    (test-case "injection-count-payload/c accepts valid payloads"
      (check-true (injection-count-payload/c (hasheq 'count 3)))
      (check-true (injection-count-payload/c (hasheq 'count 0 'source "drain"))))

    (test-case "injection-count-payload/c rejects missing count"
      (check-false (injection-count-payload/c (hasheq 'injected 3))))

    ;; -------------------------------------------------------
    ;; turn-cancelled-payload/c
    ;; -------------------------------------------------------
    (test-case "turn-cancelled-payload/c accepts valid payloads"
      (check-true (turn-cancelled-payload/c (hasheq 'reason "force-shutdown" 'iteration 3)))
      (check-true (turn-cancelled-payload/c (hasheq 'reason "cancellation-token" 'iteration 1))))

    (test-case "turn-cancelled-payload/c rejects missing keys"
      (check-false (turn-cancelled-payload/c (hasheq 'reason "force-shutdown")))
      (check-false (turn-cancelled-payload/c (hasheq 'iteration 3))))

    ;; -------------------------------------------------------
    ;; iteration-decision-payload/c
    ;; -------------------------------------------------------
    (test-case "iteration-decision-payload/c accepts valid payloads"
      (check-true (iteration-decision-payload/c (hasheq 'iteration 1 'termination 'completed)))
      (check-true (iteration-decision-payload/c
                   (hasheq 'iteration 5 'termination 'tool-calls-pending 'extra 'data))))

    (test-case "iteration-decision-payload/c rejects missing keys"
      (check-false (iteration-decision-payload/c (hasheq 'iteration 1)))
      (check-false (iteration-decision-payload/c (hasheq 'termination 'completed))))

    ;; ── R2 wiring tests (F7: test both symbol and string paths) ──

    (test-case "event-payload-contract maps agent.blocked (symbol)"
      (check-pred procedure? (event-payload-contract 'agent.blocked)))

    (test-case "event-payload-contract maps agent.blocked (string)"
      (check-pred procedure? (event-payload-contract "agent.blocked")))

    (test-case "event-payload-contract maps turn.cancelled (symbol and string)"
      (check-pred procedure? (event-payload-contract 'turn.cancelled))
      (check-pred procedure? (event-payload-contract "turn.cancelled")))

    (test-case "event-payload-contract returns #f for unknown event"
      (check-false (event-payload-contract 'unknown.event))
      (check-false (event-payload-contract "unknown.event")))

    (test-case "emit-session-event! accepts valid contracted payload (string event name)"
      (define bus (make-event-bus))
      (check-not-exn (lambda ()
                       (emit-session-event! bus "sid" "agent.blocked" (hasheq 'reason "hook")))))

    (test-case "emit-session-event! warns on invalid contracted payload (string event name)"
      (define bus (make-event-bus))
      ;; Should log warning but not raise
      (check-not-exn (lambda ()
                       (emit-session-event! bus "sid" "agent.blocked" (hasheq 'other "data")))))))

(run-tests event-contract-tests)
