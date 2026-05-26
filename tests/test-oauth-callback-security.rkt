#lang racket

;;; tests/test-oauth-callback-security.rkt — OAuth2 security regression tests
;;;
;;; Tests for v0.59.1 W0 (#5340): RFC7636 PKCE + CSPRNG primitives
;;; - RFC 7636 Appendix B test vector
;;; - CSPRNG uniqueness and entropy
;;; - Contract enforcement

(require rackunit
         rackunit/text-ui
         file/sha1
         racket/string
         "../runtime/oauth-callback.rkt")

(define security-tests
  (test-suite "OAuth2 security (v0.59.1)"

    ;; ============================================================
    ;; RFC 7636 PKCE test vector (Appendix B)
    ;; ============================================================

    (test-case "RFC 7636 Appendix B test vector (#5341)"
      ;; Known verifier → known S256 challenge
      (define verifier "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk")
      (define expected-challenge "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM")
      ;; Manually compute BASE64URL(SHA256(verifier))
      (define hashed (sha256-bytes (string->bytes/utf-8 verifier)))
      (define challenge (base64url-encode-bytes hashed))
      (check-equal? challenge
                    expected-challenge
                    "RFC 7636 Appendix B: SHA-256 PKCE challenge must match known vector"))

    (test-case "generate-pkce uses real SHA-256 (deterministic check) (#5341)"
      ;; Generate PKCE, then manually verify the challenge matches SHA256(verifier)
      (define-values (verifier challenge) (generate-pkce))
      (define expected (base64url-encode-bytes (sha256-bytes (string->bytes/utf-8 verifier))))
      (check-equal? challenge expected "PKCE challenge must equal BASE64URL(SHA256(verifier))"))

    ;; ============================================================
    ;; CSPRNG quality tests (#5342)
    ;; ============================================================

    (test-case "CSPRNG produces unique values (#5342)"
      (define vals
        (for/list ([_ (in-range 20)])
          (generate-state)))
      (define unique (remove-duplicates vals))
      (check-equal? (length unique) 20 "20 CSPRNG samples must all be unique"))

    (test-case "CSPRNG verifier has sufficient length (#5342)"
      (define-values (verifier _challenge) (generate-pkce))
      ;; RFC 7636: verifier must be 43-128 chars
      (check-true (>= (string-length verifier) 43)
                  (format "verifier too short: ~a chars" (string-length verifier))))

    (test-case "CSPRNG state has sufficient length (#5342)"
      (define state (generate-state))
      ;; 16 bytes → ~22 base64 chars minimum
      (check-true (>= (string-length state) 16)
                  (format "state too short: ~a chars" (string-length state))))

    (test-case "CSPRNG output is valid base64url (#5342)"
      (define state (generate-state))
      ;; base64url chars only: A-Z a-z 0-9 - _
      (check-true (regexp-match? #rx"^[A-Za-z0-9_-]+$" state)
                  "state must contain only base64url characters"))

    ;; ============================================================
    ;; Contract enforcement (#5343)
    ;; ============================================================

    (test-case "generate-pkce returns two strings (#5343)"
      (define-values (v c) (generate-pkce))
      (check-true (string? v))
      (check-true (string? c)))

    (test-case "generate-state returns string (#5343)"
      (define s (generate-state))
      (check-true (string? s))
      (check-true (> (string-length s) 0)))))

(module+ main
  (run-tests security-tests))
