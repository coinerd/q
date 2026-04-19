#lang racket

;; tests/tui/error-scenarios.rkt — Pre-built event sequences for error recovery tests
;;
;; Provides 6 common failure mode event sequences with corresponding
;; state assertion functions. These building blocks enable rapid creation
;; of error-recovery tests without manually constructing event sequences.
;;
;; Each scenario function returns:
;;   (listof (cons event? (-> ui-state? void?)))
;;   i.e., event + assertion pairs

(require rackunit
         rackunit/text-ui
         "../../util/protocol-types.rkt"
         "event-simulator.rkt"
         "state-assertions.rkt"
         "../../tui/state.rkt")

(provide scenario:provider-timeout
         scenario:rate-limit-retry
         scenario:auth-failure
         scenario:context-overflow
         scenario:max-iterations
         scenario:tool-failure
         ;; Utility
         run-scenario
         run-all-scenarios)

;; Helper: create a test event
(define (make-evt type payload)
  (make-test-event type (apply hasheq (append-map (lambda (p) (list (car p) (cdr p))) payload))))

;; ============================================================
;; Scenario 1: Provider timeout (SSE chunk timeout)
;; ============================================================

(define (scenario:provider-timeout)
  "Simulates a provider timeout with auto-retry."
  (define s0 (initial-ui-state))
  (define events
    (list (make-evt "model.stream.delta" '((delta . "Hello ")))
          (make-evt "runtime.error" '((error . "HTTP read timeout after 120s") (errorType . timeout)))
          (make-evt "auto-retry.start" '((attempt . 1) (max-retries . 2) (delay-ms . 1000)))
          (make-evt "model.stream.delta" '((delta . "Retried response")))
          (make-evt "model.stream.completed" '())
          (make-evt "turn.completed" '())))
  (define s-final (simulate-events s0 events))
  ;; Assertions
  (check-true (state-idle? s-final) "timeout scenario: state should be idle")
  (check-true (state-has-error? s-final) "timeout scenario: error should be displayed")
  (check-true (state-has-retry? s-final) "timeout scenario: retry should be visible")
  (check-true (state-has-recovery-hint? s-final) "timeout scenario: recovery hint should show")
  s-final)

;; ============================================================
;; Scenario 2: Rate limit (429 → retry → success)
;; ============================================================

(define (scenario:rate-limit-retry)
  "Simulates a rate limit error with auto-retry success."
  (define s0 (initial-ui-state))
  (define events
    (list (make-evt "runtime.error"
                    '((error . "HTTP 429 rate limit exceeded") (errorType . rate-limit)))
          (make-evt "auto-retry.start" '((attempt . 1) (max-retries . 2) (delay-ms . 1000)))
          (make-evt "model.stream.delta" '((delta . "Success after retry")))
          (make-evt "model.stream.completed" '())
          (make-evt "turn.completed" '())))
  (define s-final (simulate-events s0 events))
  (check-true (state-idle? s-final) "rate-limit scenario: state should be idle")
  (check-true (state-has-retry? s-final) "rate-limit scenario: retry should be visible")
  s-final)

;; ============================================================
;; Scenario 3: Auth failure (401)
;; ============================================================

(define (scenario:auth-failure)
  "Simulates an authentication failure."
  (define s0 (initial-ui-state))
  (define events
    (list (make-evt "runtime.error" '((error . "401 Unauthorized") (errorType . auth)))
          (make-evt "turn.completed" '())))
  (define s-final (simulate-events s0 events))
  (check-true (state-has-error? s-final) "auth scenario: error should be displayed")
  (check-true (state-has-recovery-hint? s-final) "auth scenario: recovery hint should mention config")
  (check-true (transcript-contains-text? s-final "config.json")
              "auth scenario: should mention config.json")
  s-final)

;; ============================================================
;; Scenario 4: Context overflow
;; ============================================================

(define (scenario:context-overflow)
  "Simulates a context overflow error."
  (define s0 (initial-ui-state))
  (define events
    (list
     (make-evt "runtime.error"
               '((error . "context_length exceeded: too many tokens") (errorType . context-overflow)))
     (make-evt "turn.completed" '())))
  (define s-final (simulate-events s0 events))
  (check-true (state-has-error? s-final) "context-overflow scenario: error should be displayed")
  (check-true (state-has-recovery-hint? s-final)
              "context-overflow scenario: recovery hint should mention /compact")
  (check-true (transcript-contains-text? s-final "/compact")
              "context-overflow scenario: should mention /compact")
  s-final)

;; ============================================================
;; Scenario 5: Max iterations exceeded
;; ============================================================

(define (scenario:max-iterations)
  "Simulates max iterations exceeded."
  (define s0 (initial-ui-state))
  (define events
    (list (make-evt "runtime.error"
                    '((error . "max.iterations reached: 25") (errorType . max-iterations)))
          (make-evt "turn.completed" '())))
  (define s-final (simulate-events s0 events))
  (check-true (state-has-error? s-final) "max-iterations scenario: error should be displayed")
  (check-true (state-idle? s-final) "max-iterations scenario: state should be idle")
  s-final)

;; ============================================================
;; Scenario 6: Tool execution failure
;; ============================================================

(define (scenario:tool-failure)
  "Simulates a tool execution failure."
  (define s0 (initial-ui-state))
  (define events
    (list (make-evt "tool.start" '((name . "bash") (id . "tool-1")))
          (make-evt "tool.result"
                    '((id . "tool-1") (content . "Error: command not found") (isError . #t)))
          (make-evt "model.stream.delta" '((delta . "The command was not found.")))
          (make-evt "model.stream.completed" '())
          (make-evt "turn.completed" '())))
  (define s-final (simulate-events s0 events))
  (check-true (state-idle? s-final) "tool-failure scenario: state should be idle")
  (assert-tool-complete? s-final "tool-failure scenario: tool should be complete")
  s-final)

;; ============================================================
;; Runner utilities
;; ============================================================

(define (run-scenario name thunk)
  (with-check-info (['scenario name]) (thunk)))

(define (run-all-scenarios)
  (run-scenario "provider-timeout" scenario:provider-timeout)
  (run-scenario "rate-limit-retry" scenario:rate-limit-retry)
  (run-scenario "auth-failure" scenario:auth-failure)
  (run-scenario "context-overflow" scenario:context-overflow)
  (run-scenario "max-iterations" scenario:max-iterations)
  (run-scenario "tool-failure" scenario:tool-failure)
  (displayln "All 6 error scenarios passed."))

;; Self-test when run directly
(module+ test
  (run-all-scenarios))
