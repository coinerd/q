#lang racket

;; @speed fast  ;; @suite security

;; tests/test-capability-token-hardening.rkt — W3 (v0.99.10) Capability Token Hardening Remediation
;;
;; FIXED in W3:
;;   H5: Capability-token validation can enforce agent scope
;;   M1: HMAC comparison uses hardened comparison
;;   M2: Malformed token timestamps do not raise and are rejected
;;   M3: Empty token secrets are rejected

(require rackunit
         rackunit/text-ui
         "../util/security/capability-tokens.rkt")

(define SECRET "test-secret-key-for-hardening")

(define suite
  (test-suite "Capability Token Hardening (W3 Remediation)"

    ;; ════════════════════════════════════════════════════════════
    ;; H5: Validation can enforce agent scope
    ;; ════════════════════════════════════════════════════════════

    (test-case "H5 FIXED: claims validation returns capability and agent-id"
      (define token (sign-capability-token 'read-only "agent-A" SECRET #:timestamp 1000))
      (define claims (validate-capability-token/claims token SECRET #:now 1100))
      (check-true (capability-token-claims? claims))
      (check-equal? (capability-token-claims-capability claims) 'read-only)
      (check-equal? (capability-token-claims-agent-id claims) "agent-A")
      (check-equal? (capability-token-claims-timestamp claims) 1000))

    (test-case "H5 FIXED: expected agent scope rejects wrong agent"
      (define token-a (sign-capability-token 'read-only "agent-A" SECRET #:timestamp 1000))
      (define token-b (sign-capability-token 'read-only "agent-B" SECRET #:timestamp 1000))
      (check-equal? (validate-capability-token-for-agent token-a SECRET "agent-A" #:now 1100)
                    'read-only)
      (check-false (validate-capability-token-for-agent token-b SECRET "agent-A" #:now 1100)))

    (test-case "H5 compatibility: legacy validate still returns capability symbol"
      (define token (sign-capability-token 'read-only "agent-A" SECRET #:timestamp 1000))
      (check-equal? (validate-capability-token token SECRET #:now 1100) 'read-only))

    ;; ════════════════════════════════════════════════════════════
    ;; M1: Hardened MAC comparison primitive
    ;; ════════════════════════════════════════════════════════════

    (test-case "M1 FIXED: constant-time string comparison has correct semantics"
      (check-true (constant-time-string=? "abcdef" "abcdef"))
      (check-false (constant-time-string=? "abcdef" "abcdeg"))
      (check-false (constant-time-string=? "abcdef" "abc"))
      (check-false (constant-time-string=? "" "abcdef")))

    (test-case "M1 FIXED: tampered HMAC still rejected via hardened compare"
      (define token (sign-capability-token 'read-only "agent-A" SECRET #:timestamp 1000))
      (define tampered (regexp-replace #rx"[0-9a-f]$" token (lambda (m) (if (equal? m "0") "1" "0"))))
      (check-false (validate-capability-token tampered SECRET #:now 1100)))

    ;; ════════════════════════════════════════════════════════════
    ;; M2: Malformed token timestamps are rejected without raising
    ;; ════════════════════════════════════════════════════════════

    (test-case "M2 FIXED: complex number timestamp is rejected without raising"
      (define token "cap:read-only:agent-A:1+2i:fake-mac")
      (check-not-exn (lambda () (validate-capability-token token SECRET)))
      (check-false (validate-capability-token token SECRET)))

    (test-case "M2 FIXED: rational number timestamp is rejected without raising"
      (define token "cap:read-only:agent-A:1/2:fake-mac")
      (check-not-exn (lambda () (validate-capability-token token SECRET)))
      (check-false (validate-capability-token token SECRET)))

    (test-case "M2 FIXED: decimal timestamp is rejected without raising"
      (define token "cap:read-only:agent-A:100.0:fake-mac")
      (check-not-exn (lambda () (validate-capability-token token SECRET)))
      (check-false (validate-capability-token token SECRET)))

    (test-case "M2 FIXED: negative timestamp is rejected without raising"
      (define token "cap:read-only:agent-A:-1:fake-mac")
      (check-not-exn (lambda () (validate-capability-token token SECRET)))
      (check-false (validate-capability-token token SECRET)))

    ;; ════════════════════════════════════════════════════════════
    ;; M3: Empty token secrets rejected
    ;; ════════════════════════════════════════════════════════════

    (test-case "M3 FIXED: empty secret is rejected by sign and validate"
      (check-exn exn:fail:contract? (lambda () (sign-capability-token 'read-only "agent-A" "")))
      (check-exn exn:fail:contract?
                 (lambda () (validate-capability-token "cap:read-only:agent-A:1:fake" ""))))

    ;; ════════════════════════════════════════════════════════════
    ;; Token grammar hardening
    ;; ════════════════════════════════════════════════════════════

    (test-case "strict grammar: trailing colon is rejected"
      (define base (sign-capability-token 'read-only "agent-A" SECRET #:timestamp 1000))
      (define token (string-append base ":"))
      (check-false (validate-capability-token token SECRET #:now 1100)))

    (test-case "strict grammar: colon in agent-id is rejected at sign boundary"
      (check-exn exn:fail:contract? (lambda () (sign-capability-token 'read-only "agent:A" SECRET))))

    ;; ════════════════════════════════════════════════════════════
    ;; Positive baseline — legitimate tokens work
    ;; ════════════════════════════════════════════════════════════

    (test-case "legitimate token validates correctly"
      (define token (sign-capability-token 'file-write "agent-X" SECRET #:timestamp 1000))
      (check-equal? (validate-capability-token token SECRET #:now 1100) 'file-write))

    (test-case "wrong secret rejected"
      (define token (sign-capability-token 'file-write "agent-X" SECRET #:timestamp 1000))
      (check-false (validate-capability-token token "wrong-secret" #:now 1100)))

    (test-case "expired token rejected"
      (define token (sign-capability-token 'file-write "agent-X" SECRET #:timestamp 0))
      (check-false (validate-capability-token token SECRET #:now 10000)))

    (test-case "future token rejected"
      (define token (sign-capability-token 'file-write "agent-X" SECRET #:timestamp 10000))
      (check-false (validate-capability-token token SECRET #:now 100)))

    ;; ════════════════════════════════════════════════════════════
    ;; F-06 LOW: Symbol capabilities must be validated as strictly as strings
    ;; ════════════════════════════════════════════════════════════

    (test-case "F-06 FIXED: symbol with colons is rejected at signing"
      ;; A symbol like '|invalid:symbol| would produce a capability string
      ;; containing colons, which would break token parsing on validation.
      ;; After the fix, sign-capability-token should reject such symbols
      ;; via the capability-input? contract.
      (check-exn exn:fail:contract?
                 (lambda () (sign-capability-token '|invalid:symbol| "agent-A" SECRET))))

    (test-case "F-06 FIXED: normal symbol still works after tightening"
      (define token (sign-capability-token 'read-only "agent-A" SECRET #:timestamp 1000))
      (check-equal? (validate-capability-token token SECRET #:now 1100) 'read-only))))

(run-tests suite)
