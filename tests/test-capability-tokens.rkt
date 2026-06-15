#lang racket

;; @speed fast  ;; @suite security

;; tests/test-capability-tokens.rkt — W1 (v0.99.9) Capability Token Tests
;;
;; Tests HMAC-signed capability tokens: sign, validate, expiry, tamper detection.

(require rackunit
         rackunit/text-ui
         "../util/security/capability-tokens.rkt")

(define suite
  (test-suite "Capability Tokens (v0.99.9 W1)"

    ;; ── HMAC-SHA256 primitive ──

    (test-case "hmac-sha256 produces 64-char hex string"
      (define mac (hmac-sha256 "key" "message"))
      (check-equal? (string-length mac) 64)
      (check-true (regexp-match? #px"^[0-9a-f]{64}$" mac)))

    (test-case "hmac-sha256 is deterministic"
      (check-equal? (hmac-sha256 "key" "message") (hmac-sha256 "key" "message")))

    (test-case "hmac-sha256 differs for different keys"
      (check-false (string=? (hmac-sha256 "key1" "message") (hmac-sha256 "key2" "message"))))

    (test-case "hmac-sha256 differs for different messages"
      (check-false (string=? (hmac-sha256 "key" "msg1") (hmac-sha256 "key" "msg2"))))

    (test-case "hmac-sha256 handles empty key"
      (check-equal? (string-length (hmac-sha256 "" "message")) 64))

    (test-case "hmac-sha256 handles long key (> block size)"
      ;; Key longer than 64 bytes gets hashed first
      (define long-key (make-string 200 #\x))
      (check-equal? (string-length (hmac-sha256 long-key "message")) 64))

    ;; ── Sign + validate roundtrip ──

    (test-case "sign + validate roundtrip with symbol capability"
      (define token (sign-capability-token 'execute-tools "agent-001" "secret" #:timestamp 1000000))
      (check-equal? (validate-capability-token token "secret" #:now 1000100) 'execute-tools))

    (test-case "sign + validate roundtrip with string capability"
      (define token (sign-capability-token "write-files" "agent-002" "secret" #:timestamp 1000000))
      (check-equal? (validate-capability-token token "secret" #:now 1000100) 'write-files))

    ;; ── Wrong secret key ──

    (test-case "validation fails with wrong secret key"
      (define token
        (sign-capability-token 'execute-tools "agent-001" "correct-key" #:timestamp 1000000))
      (check-false (validate-capability-token token "wrong-key" #:now 1000100)))

    ;; ── Expired token ──

    (test-case "validation fails for expired token"
      (define token (sign-capability-token 'execute-tools "agent-001" "secret" #:timestamp 1000000))
      ;; 400 seconds later = expired (TTL is 300)
      (check-false (validate-capability-token token "secret" #:now 1000400)))

    (test-case "validation passes at exactly TTL boundary"
      (define token (sign-capability-token 'execute-tools "agent-001" "secret" #:timestamp 1000000))
      ;; Exactly 300 seconds later = still valid (boundary inclusive)
      (check-equal? (validate-capability-token token "secret" #:now 1000300) 'execute-tools))

    ;; ── Future timestamp ──

    (test-case "validation fails for future timestamp"
      (define token (sign-capability-token 'execute-tools "agent-001" "secret" #:timestamp 1000100))
      ;; Token timestamp is in the future relative to validation time
      (check-false (validate-capability-token token "secret" #:now 1000000)))

    ;; ── Malformed token ──

    (test-case "validation fails for malformed token (too few parts)"
      (check-false (validate-capability-token "cap:execute-tools" "secret")))

    (test-case "validation fails for malformed token (wrong prefix)"
      (check-false (validate-capability-token "tok:execute-tools:agent:1000:abcd" "secret")))

    (test-case "validation fails for garbage string"
      (check-false (validate-capability-token "not-a-token-at-all" "secret")))

    (test-case "validation fails for non-numeric timestamp"
      (check-false (validate-capability-token "cap:exec:agent:abc:def" "secret")))

    ;; ── Tampered HMAC ──

    (test-case "validation fails for tampered HMAC"
      (define token (sign-capability-token 'execute-tools "agent-001" "secret" #:timestamp 1000000))
      ;; Flip the last char of the HMAC
      (define tampered (regexp-replace #rx"[0-9a-f]$" token (lambda (m) (if (equal? m "0") "1" "0"))))
      (check-false (validate-capability-token tampered "secret" #:now 1000100)))

    ;; ── Token format ──

    (test-case "token starts with cap: prefix"
      (define token (sign-capability-token 'execute-tools "agent-001" "secret" #:timestamp 1000000))
      (check-true (string-prefix? token "cap:")))

    (test-case "token contains 5 colon-separated parts"
      (define token (sign-capability-token 'execute-tools "agent-001" "secret" #:timestamp 1000000))
      (check-equal? (length (string-split token ":")) 5))

    ;; ── Different capabilities produce different tokens ──

    (test-case "different capabilities produce different tokens"
      (define t1 (sign-capability-token 'execute-tools "agent-001" "secret" #:timestamp 1000000))
      (define t2 (sign-capability-token 'write-files "agent-001" "secret" #:timestamp 1000000))
      (check-false (string=? t1 t2)))

    ;; ── TTL constant ──

    (test-case "CAPABILITY-TOKEN-TTL is 300 seconds"
      (check-equal? CAPABILITY-TOKEN-TTL 300))

    ;; ── Default timestamp uses current time ──

    (test-case "sign without explicit timestamp uses current time"
      (define token (sign-capability-token 'execute-tools "agent-001" "secret"))
      (check-equal? (validate-capability-token token "secret") 'execute-tools))))

(run-tests suite)
