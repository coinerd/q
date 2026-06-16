#lang racket

;; @speed fast  ;; @suite default

;; tests/test-verifier-deployment-gate.rkt
;; v0.99.15 W0: Verifier Characterization & Pre-Implementation Safety Net
;;
;; Updated in W3: Tests #1 and #4 now reflect the flipped defaults:
;;   verifier-enabled? default is now #t (was #f)
;;   verifier-risk-threshold default is now 'high (was 'medium)
;;
;; Test Cases:
;;   1. verifier-enabled? returns #t for default settings (post-W3 flip)
;;   2. verifier-enabled? returns #t when explicitly enabled
;;   3. verifier-enabled? returns #f when explicitly disabled
;;   4. verifier-risk-threshold returns 'high for default settings (post-W3)
;;   5. verifier-risk-threshold returns 'medium when explicitly set
;;   6. Gate with flag OFF returns 'approved (no LLM call)
;;   7. Gate with flag ON + no provider returns 'approved (safe fallback)

(require rackunit
         rackunit/text-ui
         (only-in "../runtime/settings-query.rkt" verifier-enabled? verifier-risk-threshold)
         (only-in "../runtime/settings-core.rkt" q-settings)
         (only-in "../agent/verification/verifier-gate.rkt"
                  execute-verification-gate
                  current-verifier-enabled)
         (only-in "../agent/verification/verifier-core.rkt" current-verifier-provider)
         (only-in "../extensions/gsd/session-state.rkt" make-gsd-context)
         (only-in "../extensions/gsd/state-machine.rkt" gsm-ctx-current gsm-ctx-transition!))

;; ── Helper: build a q-settings with a given merged hash ──
(define (make-settings merged-hash)
  (q-settings (hash) (hash) merged-hash))

;; ── Helper: create a GSD context in 'verifying state ──
(define (make-verifying-ctx)
  (define ctx (make-gsd-context))
  (gsm-ctx-transition! ctx 'exploring)
  (gsm-ctx-transition! ctx 'plan-written)
  (gsm-ctx-transition! ctx 'executing)
  (gsm-ctx-transition! ctx 'verifying)
  ctx)

(define deployment-gate-suite
  (test-suite "Verifier Deployment Gate (v0.99.15 W0)"

    ;; ── Test 1: Default is #t (post-W3 default-on) ──
    (test-case "verifier-enabled? returns #t for default settings"
      (define settings (make-settings (hash)))
      (check-true (verifier-enabled? settings) "default should be #t after W3 flip"))

    ;; ── Test 2: Explicit #t ──
    (test-case "verifier-enabled? returns #t when mas.verifier.enabled = #t"
      (define settings (make-settings (hash 'mas (hash 'verifier (hash 'enabled #t)))))
      (check-true (verifier-enabled? settings)))

    ;; ── Test 3: Explicit #f ──
    (test-case "verifier-enabled? returns #f when mas.verifier.enabled = #f"
      (define settings (make-settings (hash 'mas (hash 'verifier (hash 'enabled #f)))))
      (check-false (verifier-enabled? settings)))

    ;; ── Test 4: Risk threshold default is 'high (post-W3) ──
    (test-case "verifier-risk-threshold returns 'high for default settings"
      (define settings (make-settings (hash)))
      (check-equal? (verifier-risk-threshold settings) 'high))

    ;; ── Test 5: Risk threshold explicit 'medium ──
    (test-case "verifier-risk-threshold returns 'medium when explicitly set"
      (define settings (make-settings (hash 'mas (hash 'verifier (hash 'risk-threshold "medium")))))
      (check-equal? (verifier-risk-threshold settings) 'medium))

    ;; ── Test 6: Gate with flag OFF returns 'approved ──
    (test-case "execute-verification-gate returns 'approved when verifier disabled"
      (define ctx (make-verifying-ctx))
      (parameterize ([current-verifier-enabled #f]
                     [current-verifier-provider #f])
        (define plan-context
          (hash 'wave-name
                "W0"
                'plan-summary
                "test plan"
                'files-changed
                '()
                'test-summary
                "all pass"
                'diff-excerpt
                ""))
        (define result (execute-verification-gate ctx plan-context))
        (check-equal? result 'approved)
        ;; FSM should have transitioned to 'idle
        (check-equal? (gsm-ctx-current ctx) 'idle)))

    ;; ── Test 7: Gate with flag ON + no provider returns 'approved (safe fallback) ──
    (test-case "execute-verification-gate returns 'approved when enabled but no provider"
      (define ctx (make-verifying-ctx))
      (parameterize ([current-verifier-enabled #t]
                     [current-verifier-provider #f])
        (define plan-context
          (hash 'wave-name
                "W0"
                'plan-summary
                "test plan"
                'files-changed
                '()
                'test-summary
                "all pass"
                'diff-excerpt
                ""))
        (define result (execute-verification-gate ctx plan-context))
        (check-equal? result 'approved)
        ;; FSM should have transitioned to 'idle (auto-approve on no provider)
        (check-equal? (gsm-ctx-current ctx) 'idle)))))

(run-tests deployment-gate-suite)
