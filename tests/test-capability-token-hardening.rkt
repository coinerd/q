#lang racket

;; @speed fast  ;; @suite security

;; tests/test-capability-token-hardening.rkt — W0 (v0.99.10) Capability Token Hardening Characterization
;;
;; CHARACTERIZATION TESTS — These document the CURRENT (buggy) behavior.
;; They will be flipped in W3 to assert the CORRECT behavior.
;;
;; Blockers characterized:
;;   H5: Capability-token validation cannot enforce agent scope
;;   M1: HMAC comparison is not constant-time
;;   M2: Malformed token timestamp can raise
;;   M3: Empty/weak token secrets accepted

(require rackunit
         rackunit/text-ui
         "../util/security/capability-tokens.rkt")

(define SECRET "test-secret-key-for-hardening")

(define suite
  (test-suite "Capability Token Hardening (W0 Characterization)"

    ;; ════════════════════════════════════════════════════════════
    ;; H5: Validation returns only capability symbol, cannot enforce agent scope
    ;; ════════════════════════════════════════════════════════════

    (test-case "CHARACTERIZE H5: validate returns only capability symbol"
      ;; CURRENT: validate-capability-token returns a capability symbol or #f.
      ;; It does NOT return the agent-id from the token, so callers cannot
      ;; verify that the token belongs to the expected agent.
      ;; W3 FIX: add claims struct or expected-claims API.
      (define token (sign-capability-token 'read-only "agent-A" SECRET))
      (define result (validate-capability-token token SECRET))
      (check-equal? result 'read-only "returns capability symbol")
      ;; CHARACTERIZATION: result is a symbol, not a claims object
      (check-true (symbol? result) "CURRENT: returns bare symbol, no agent-id info")
      (check-false (hash? result) "not a claims hash")
      (check-false (struct? result) "not a claims struct"))

    (test-case "CHARACTERIZE H5: cannot distinguish agent scope from API output"
      ;; Token for agent-A is valid — but caller can't tell it's for agent-A.
      ;; Token for agent-B is also valid for the same capability.
      ;; Without agent enforcement, A's token works for B's context.
      (define token-a (sign-capability-token 'read-only "agent-A" SECRET))
      (define token-b (sign-capability-token 'read-only "agent-B" SECRET))
      (check-equal? (validate-capability-token token-a SECRET) 'read-only)
      (check-equal? (validate-capability-token token-b SECRET) 'read-only)
      ;; CHARACTERIZATION: both tokens return same result — no way to
      ;; reject token-b when expecting agent-A
      (check-equal? (validate-capability-token token-a SECRET)
                    (validate-capability-token token-b SECRET)
                    "CURRENT BUG: no API to distinguish agent scope"))

    ;; ════════════════════════════════════════════════════════════
    ;; M2: Malformed token timestamp can raise
    ;; ════════════════════════════════════════════════════════════

    (test-case "CHARACTERIZE M2: complex number timestamp can raise"
      ;; CURRENT: string->number accepts complex numbers like "1+2i".
      ;; The complex value then fails numeric comparisons.
      ;; W3 FIX: require exact-nonnegative-integer?
      (define token (format "cap:read-only:agent-A:1+2i:fake-mac"))
      (check-exn exn:fail?
                 (lambda () (validate-capability-token token SECRET))
                 "CURRENT BUG: complex timestamp raises"))

    (test-case "CHARACTERIZE M2: rational number timestamp may pass incorrectly"
      ;; "1/2" parses as 1/2 — an exact rational.
      ;; It should be rejected (not an exact nonnegative integer).
      (define token (format "cap:read-only:agent-A:1/2:fake-mac"))
      (define result
        (with-handlers ([exn:fail? (lambda (_) 'raised)])
          (validate-capability-token token SECRET)))
      ;; CHARACTERIZATION: either raises or returns #f — both are wrong-ish,
      ;; but at least it's not a valid token. Document what happens.
      (check-true (or (eq? result 'raised) (not result)) "rational timestamp: raises or returns #f"))

    (test-case "CHARACTERIZE M2: decimal timestamp passes string->number"
      ;; "100.0" parses as 100.0 — an inexact number.
      ;; It should be rejected as non-canonical (must be exact integer).
      (define token (format "cap:read-only:agent-A:100.0:fake-mac"))
      (define result
        (with-handlers ([exn:fail? (lambda (_) 'raised)])
          (validate-capability-token token SECRET)))
      ;; CHARACTERIZATION: at most returns #f because HMAC won't match
      (check-true (or (eq? result 'raised) (not result)) "decimal timestamp: raises or returns #f"))

    ;; ════════════════════════════════════════════════════════════
    ;; M3: Empty/weak token secrets accepted
    ;; ════════════════════════════════════════════════════════════

    (test-case "CHARACTERIZE M3: empty secret is accepted by sign and validate"
      ;; CURRENT: no validation on secret — empty string works.
      ;; W3 FIX: reject empty strings at primitive boundary.
      (define token (sign-capability-token 'read-only "agent-A" ""))
      (define result (validate-capability-token token ""))
      ;; CHARACTERIZATION: empty secret works (should be rejected in W3)
      (check-equal? result 'read-only "CURRENT BUG: empty secret is accepted"))

    ;; ════════════════════════════════════════════════════════════
    ;; Token grammar characterization
    ;; ════════════════════════════════════════════════════════════

    (test-case "CHARACTERIZE: trailing colon is silently accepted (string-split trims)"
      ;; CURRENT: string-split with #:trim? removes trailing empty strings,
      ;; so a trailing colon does NOT create a 6th field.
      ;; This means malformed tokens with trailing colons are silently accepted.
      ;; W3 FIX: strict token grammar should reject this.
      (define base (sign-capability-token 'read-only "agent-A" SECRET))
      (define token (string-append base ":"))
      (check-equal? (validate-capability-token token SECRET)
                    'read-only
                    "CURRENT BUG: trailing colon silently accepted"))

    (test-case "CHARACTERIZE: colon in agent-id creates extra fields (rejected)"
      ;; "agent:A" would create 6 colon-separated parts, rejected by 5-field check
      (define token (sign-capability-token 'read-only "agent:A" SECRET))
      (check-false (validate-capability-token token SECRET)
                   "colon in agent-id creates 6 parts, rejected (by accident, not design)"))

    ;; ════════════════════════════════════════════════════════════
    ;; Positive baseline — legitimate tokens work
    ;; ════════════════════════════════════════════════════════════

    (test-case "legitimate token validates correctly"
      (define token (sign-capability-token 'file-write "agent-X" SECRET))
      (check-equal? (validate-capability-token token SECRET) 'file-write))

    (test-case "wrong secret rejected"
      (define token (sign-capability-token 'file-write "agent-X" SECRET))
      (check-false (validate-capability-token token "wrong-secret")))

    (test-case "expired token rejected"
      (define token (sign-capability-token 'file-write "agent-X" SECRET #:timestamp 0))
      (check-false (validate-capability-token token SECRET #:now 10000)))

    (test-case "future token rejected"
      (define token (sign-capability-token 'file-write "agent-X" SECRET #:timestamp 10000))
      (check-false (validate-capability-token token SECRET #:now 100)))))

(run-tests suite)
