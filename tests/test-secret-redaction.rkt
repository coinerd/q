#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-secret-redaction.rkt
;; v0.99.50 W3 (TMUX-07): Centralized credential redaction and leak detection.
;;
;; Verifies the three-way redaction policy is consistent and free of the
;; false positives identified in V9949-TMUX-07:
;; - benign `sk-` substrings inside words (risk-score, set-task-state)
;; - `Bearer authentication` (dictionary word, not a token)
;; - already-redacted `<REDACTED>` placeholders
;; While still catching real secrets.

(require json
         rackunit
         rackunit/text-ui
         racket/string
         "../util/credential-redaction.rkt")

(define suite
  (test-suite "Centralized Secret Redaction (v0.99.50 W3 / TMUX-07)"

    ;; ── Benign strings: must NOT be flagged as leaks ──

    (test-case "risk-score is not flagged as a leak"
      (check-false (contains-secret-leak? "risk-score is above threshold"))
      (check-false (contains-secret-leak? "the risk-score value")))

    (test-case "set-task-state is not flagged as a leak"
      (check-false (contains-secret-leak? "set-task-state completed")))

    (test-case "Bearer authentication is not flagged as a leak"
      (check-false (contains-secret-leak? "Bearer authentication failed"))
      (check-false (contains-secret-leak? "uses Bearer authentication headers")))

    (test-case "REDACTED placeholders are not flagged"
      (check-false (contains-secret-leak? "API_KEY=<REDACTED>"))
      (check-false (contains-secret-leak? "token=<REDACTED>"))
      (check-false (contains-secret-leak? "Bearer <REDACTED>")))

    (test-case "common tool names and phrases are clean"
      (check-false (contains-secret-leak? "task-state was set"))
      (check-false (contains-secret-leak? "the password field is optional"))
      (check-false (contains-secret-leak? "password=<REDACTED>")))

    ;; ── Real secrets: MUST be flagged ──

    (test-case "real sk- API key is flagged"
      (check-true (contains-secret-leak? "key=sk-ant-api03-abcdefghijklmnopqrstuvwxyz1234567890"))
      (check-true (contains-secret-leak?
                   "sk-proj-abcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOP")))

    (test-case "real Bearer token is flagged"
      (check-true
       (contains-secret-leak?
        "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.SflKxwRS"))
      (check-true (contains-secret-leak? "Bearer dGhpc19pc19hX3JlYWxfczNjcmV0X3QwazNu")))

    (test-case "real key=value assignments are flagged"
      (check-true (contains-secret-leak? "api_key=abcdef1234567890abcdef"))
      (check-true (contains-secret-leak? "token=ghp_abcdefghijklmnopqrstuvwxyz"))
      (check-true (contains-secret-leak? "secret=mysecretvalue123456"))
      (check-true (contains-secret-leak? "password=hunter2special")))

    (test-case "real JSON and header assignments are flagged"
      (check-true (contains-secret-leak? "{\"api_key\":\"realkey1234567890abcdef\"}"))
      (check-true (contains-secret-leak? "{\"token\" : \"ghp_realtoken1234567890\"}"))
      (check-true (contains-secret-leak? "x-api-key: realkey1234567890abcdef")))

    ;; ── Mixed redacted and real: MUST be flagged ──

    (test-case "mixed redacted and real values are flagged"
      (check-true (contains-secret-leak? "token=<REDACTED> api_key=realkey1234567890abcdef"))
      (check-true (contains-secret-leak?
                   "Bearer <REDACTED> and sk-real-key-abcdefghijklmnopqrstuvwxyz")))

    ;; ── Redaction: benign text is unchanged ──

    (test-case "redact-secrets preserves benign text"
      (check-equal? (redact-secrets "risk-score is 42") "risk-score is 42")
      (check-equal? (redact-secrets "set-task-state done") "set-task-state done")
      (check-equal? (redact-secrets "Bearer authentication") "Bearer authentication"))

    (test-case "redact-secrets preserves already-redacted placeholders"
      (check-equal? (redact-secrets "API_KEY=<REDACTED>") "API_KEY=<REDACTED>")
      (check-equal? (redact-secrets "token=<REDACTED>") "token=<REDACTED>"))

    ;; ── Redaction: real secrets are replaced ──

    (test-case "redact-secrets removes sk- API keys"
      (define r (redact-secrets "key=sk-ant-api03-abcdefghijklmnopqrstuvwxyz1234567890"))
      (check-false (string-contains? r "sk-ant-api03-"))
      (check-true (string-contains? r "<REDACTED>")))

    (test-case "redact-secrets removes Bearer tokens"
      (define r
        (redact-secrets
         "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxIn0.SflK"))
      (check-false (string-contains? r "eyJhbGci"))
      (check-true (string-contains? r "Bearer <REDACTED>")))

    (test-case "redact-secrets removes key=value assignments"
      (define r (redact-secrets "api_key=abcdef1234567890abcdef"))
      (check-false (string-contains? r "abcdef1234567890abcdef"))
      (check-true (string-contains? r "api_key=<REDACTED>")))

    (test-case "redact-secrets preserves valid JSON while replacing values"
      (define r (redact-secrets "{\"token\" : \"ghp_realtoken1234567890\",\"ok\":true}"))
      (check-false (string-contains? r "ghp_realtoken"))
      (check-equal? (hash-ref (string->jsexpr r) 'token) "<REDACTED>")
      (check-true (hash-ref (string->jsexpr r) 'ok)))

    (test-case "redact-secrets preserves credential header names"
      (define r (redact-secrets "x-api-key: realkey1234567890abcdef"))
      (check-equal? r "x-api-key: <REDACTED>"))

    (test-case "redact-secrets is idempotent"
      (define text "api_key=abcdef1234567890abcdef")
      (define once (redact-secrets text))
      (define twice (redact-secrets once))
      (check-equal? once twice "redacting already-redacted text should be a no-op"))

    ;; ── find-secret-leaks: returns descriptions ──

    (test-case "find-secret-leaks returns empty for clean text"
      (check-equal? (find-secret-leaks "risk-score is fine") '()))

    (test-case "find-secret-leaks returns descriptions for dirty text"
      (define leaks (find-secret-leaks "api_key=realsecret1234567890abcdef"))
      (check-true (pair? leaks) "should find at least one leak")
      (for ([l (in-list leaks)])
        (check-true (string? l) "each leak should be a description string")))))

(run-tests suite)
