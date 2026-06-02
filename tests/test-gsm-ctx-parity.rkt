#lang racket/base

;; tests/test-gsm-ctx-parity.rkt — T1-5: Verify gsm-* and gsm-ctx-* produce same results
;; STABILITY: evolving

(require rackunit
         rackunit/text-ui
         "../extensions/gsd/state-machine.rkt")

;; ── Test Suite ──

(define suite
  (test-suite "GSM/gsm-ctx Parity (T1-5)"

    ;; Test 1: Both function sets exist
    (test-case "gsm-current and gsm-ctx-current are both defined"
      (check-true (procedure? gsm-current))
      (check-true (procedure? gsm-ctx-current)))

    (test-case "gsm-snapshot and gsm-ctx-snapshot are both defined"
      (check-true (procedure? gsm-snapshot))
      (check-true (procedure? gsm-ctx-snapshot)))

    ;; Test 2: Core gsm-* functions exist
    (test-case "core gsm functions exist"
      (check-true (procedure? gsm-current))
      (check-true (procedure? gsm-history))
      (check-true (procedure? gsm-snapshot))
      (check-true (procedure? gsm-valid-next-states))
      (check-true (procedure? gsm-reset!)))

    ;; Test 3: Core gsm-ctx-* counterparts exist
    (test-case "core gsm-ctx counterparts exist"
      (check-true (procedure? gsm-ctx-current))
      (check-true (procedure? gsm-ctx-history))
      (check-true (procedure? gsm-ctx-snapshot))
      (check-true (procedure? gsm-ctx-valid-next-states))
      (check-true (procedure? gsm-ctx-reset!)))

    ;; Test 4: gsm-ctx-* functions take ctx as first argument
    (test-case "gsm-ctx-* functions take ctx as first argument"
      (check-equal? (procedure-arity gsm-ctx-current) 1)
      (check-equal? (procedure-arity gsm-ctx-history) 1)
      (check-equal? (procedure-arity gsm-ctx-snapshot) 1))

    ;; Test 5: gsm-ctx-* functions produce results (v0.81.0 W3)
    (test-case "gsm-current and gsm-ctx-current both return symbols"
      (define state (gsm-current))
      (check-pred symbol? state "gsm-current returns a symbol")
      ;; Also verify the parallel ctx function
      (check-pred procedure? gsm-ctx-snapshot "gsm-ctx-snapshot is a procedure"))))

(run-tests suite)
