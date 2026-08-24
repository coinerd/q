#lang racket

;; @speed fast  ;; @suite security
;; @boundary integration  ;; @requires network

;;; tests/test-oauth-callback-security.rkt — OAuth2 security regression tests
;;;
;;; Tests for v0.59.1 W0 (#5340): RFC7636 PKCE + CSPRNG primitives
;;; Tests for v0.59.1 W1 (#5344): Callback lifecycle, query decoding, CSRF
;;;
;;; W2 remediation (current release train): fixed `sleep`/alarm-evt waits and unsafe
;;; listener probes replaced by the #:on-complete production seam + explicit
;;; semaphore waits from helpers/oauth-callback-fixtures.rkt. Every test-case
;;; and assertion is preserved one-for-one; no test deleted, weakened, or
;;; merged. (A listener probe before completion would be accepted as a bogus
;;; connection and complete the one-shot server with #f — probes are therefore
;;; only used after wait-for-callback-completion confirms the server is done.)

(require rackunit
         rackunit/text-ui
         file/sha1
         racket/string
         "../runtime/auth/oauth-callback.rkt"
         "helpers/oauth-callback-fixtures.rkt")

(define security-tests
  (test-suite "OAuth2 security (v0.59.1)"

    ;; ============================================================
    ;; RFC 7636 PKCE test vector (Appendix B) — W0
    ;; ============================================================

    (test-case "RFC 7636 Appendix B test vector (#5341)"
      (define verifier "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk")
      (define expected-challenge "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM")
      (define hashed (sha256-bytes (string->bytes/utf-8 verifier)))
      (define challenge (base64url-encode-bytes hashed))
      (check-equal? challenge
                    expected-challenge
                    "RFC 7636 Appendix B: SHA-256 PKCE challenge must match known vector"))

    (test-case "generate-pkce uses real SHA-256 (deterministic check) (#5341)"
      (define-values (verifier challenge) (generate-pkce))
      (define expected (base64url-encode-bytes (sha256-bytes (string->bytes/utf-8 verifier))))
      (check-equal? challenge expected "PKCE challenge must equal BASE64URL(SHA256(verifier))"))

    ;; ============================================================
    ;; CSPRNG quality tests — W0
    ;; ============================================================

    (test-case "CSPRNG produces unique values (#5342)"
      (define vals
        (for/list ([_ (in-range 20)])
          (generate-state)))
      (define unique (remove-duplicates vals))
      (check-equal? (length unique) 20 "20 CSPRNG samples must all be unique"))

    (test-case "CSPRNG verifier has sufficient length (#5342)"
      (define-values (verifier _challenge) (generate-pkce))
      (check-true (>= (string-length verifier) 43)
                  (format "verifier too short: ~a chars" (string-length verifier))))

    (test-case "CSPRNG state has sufficient length (#5342)"
      (define state (generate-state))
      (check-true (>= (string-length state) 16)
                  (format "state too short: ~a chars" (string-length state))))

    (test-case "CSPRNG output is valid base64url (#5342)"
      (define state (generate-state))
      (check-true (regexp-match? #rx"^[A-Za-z0-9_-]+$" state)
                  "state must contain only base64url characters"))

    ;; ============================================================
    ;; Contract enforcement — W0
    ;; ============================================================

    (test-case "generate-pkce returns two strings (#5343)"
      (define-values (v c) (generate-pkce))
      (check-true (string? v))
      (check-true (string? c)))

    (test-case "generate-state returns string (#5343)"
      (define s (generate-state))
      (check-true (string? s))
      (check-true (> (string-length s) 0)))

    ;; ============================================================
    ;; W1: Safe percent-decoding (#5345)
    ;; ============================================================

    (test-case "parse-query decodes percent-encoded values (#5345)"
      (define result (parse-query "/callback?code=abc%20def&state=xyz%21"))
      (define code-pair (assoc "code" result))
      (define state-pair (assoc "state" result))
      (check-equal? (cdr code-pair) "abc def")
      (check-equal? (cdr state-pair) "xyz!"))

    (test-case "parse-query keys are strings not symbols (#5345)"
      (define result (parse-query "/callback?code=abc&state=xyz"))
      (check-true (string? (caar result)) "keys must be strings (attacker-controlled)"))

    (test-case "safe-decode handles invalid percent sequences (#5345)"
      (define result (safe-decode "hello%ZZworld"))
      (check-true (string? result) "safe-decode must not crash on invalid input"))

    (test-case "parse-query handles empty values (#5345)"
      (define result (parse-query "/callback?code=&state=xyz"))
      (define code-pair (assoc "code" result))
      (check-equal? (cdr code-pair) ""))

    ;; ============================================================
    ;; W1: One-shot callback server (#5346)
    ;; ============================================================

    (test-case "callback server is one-shot after success (#5346)"
      (define-values (completion-sema on-complete) (make-callback-completion))
      (define-values (port state verifier get-code)
        (start-callback-server #:timeout 10 #:on-complete on-complete))
      ;; Send valid callback; completion event replaces the fixed waits.
      (callback-send-request port state "auth-code")
      (define code (get-code))
      (check-equal? code "auth-code")
      ;; Verify listener is closed — second connection should fail.
      ;; (get-code returned only after try-complete! closed the listener, so
      ;; no extra wait is needed before probing.)
      (check-eq? (callback-probe-listener port)
                 'connection-failed
                 "listener should be closed after one-shot"))

    (test-case "callback server closes after invalid state (#5346)"
      (define-values (port state verifier get-code) (start-callback-server #:timeout 10))
      (callback-send-request port "wrong" "abc")
      (define code (get-code))
      (check-false code "invalid state should return #f and close server"))

    ;; ============================================================
    ;; W1: CSRF strictness + timeout (#5347)
    ;; ============================================================

    (test-case "CSRF: missing state parameter rejects code (#5347)"
      (define-values (port state verifier get-code) (start-callback-server #:timeout 10))
      (callback-send-request port "" "abc")
      (define code (get-code))
      (check-false code "missing state must reject"))

    (test-case "deterministic timeout returns #f (#5347)"
      (define-values (port state verifier get-code) (start-callback-server #:timeout 1))
      (define code (get-code))
      (check-false code "timeout must return #f"))

    ;; ============================================================
    ;; W0: Atomic one-shot + delayed consumer (#5463)
    ;; ============================================================

    (test-case "delayed consumer: callback before get-code still works (#5463)"
      ;; Callback arrives immediately; get-code blocks until completion and
      ;; must still receive the stored code.
      (define-values (port state verifier get-code) (start-callback-server #:timeout 10))
      (callback-send-request port state "delayed-code")
      (define code (get-code))
      (check-equal? code "delayed-code" "delayed consumer must still receive code"))

    (test-case "concurrent double callback: only first wins (#5463)"
      (define-values (completion-sema on-complete) (make-callback-completion))
      (define-values (port state verifier get-code)
        (start-callback-server #:timeout 10 #:on-complete on-complete))
      ;; The original spaced the two callbacks 100 ms apart so the first was
      ;; guaranteed to be processed first; the completion event makes that
      ;; ordering deterministic: the first callback is fully processed before
      ;; the second is sent, so it must win.
      (callback-send-request port state "first-code")
      (check-true (wait-for-callback-completion completion-sema 10)
                  "first callback must complete the one-shot server")
      (callback-send-request port state "second-code")
      (define code (get-code))
      (check-equal? code "first-code" "first callback must win")
      ;; Second connection should fail after server shuts down.
      (check-eq? (callback-probe-listener port)
                 'connection-failed
                 "server must close after first result"))

    (test-case "atomic completion: no unsynchronized shared state (#5463)"
      ;; Verify the semaphore model: multiple rapid connections don't corrupt state
      (for ([_ (in-range 5)])
        (define-values (port state verifier get-code) (start-callback-server #:timeout 5))
        (callback-send-request port state "iter-code")
        (define code (get-code))
        (check-equal? code "iter-code" "each iteration must get the correct code")))

    ;; ============================================================
    ;; W1: Cleanup independence + nonblocking completion (#5497)
    ;; ============================================================

    (test-case "cleanup is independent of consumer (#5497)"
      ;; The listener must be closed BEFORE get-code is called,
      ;; so resources are reclaimed even if the consumer delays.
      (define-values (completion-sema on-complete) (make-callback-completion))
      (define-values (port state verifier get-code)
        (start-callback-server #:timeout 10 #:on-complete on-complete))
      (callback-send-request port state "cleanup-test")
      ;; Wait for the server to process the callback and close the listener —
      ;; the completion event replaces the 800 ms fixed wait.
      (check-true (wait-for-callback-completion completion-sema 10))
      ;; Listener should be closed — new connections must fail
      (check-eq? (callback-probe-listener port)
                 'connection-failed
                 "listener must be closed before consumer calls get-code")
      ;; Consumer can still get the code (delayed read from channel)
      (define code (get-code))
      (check-equal? code "cleanup-test" "delayed consumer must still receive code after cleanup"))

    (test-case "nonblocking completion: get-code returns immediately after callback (#5497)"
      ;; Once the callback has been processed, get-code should not block
      ;; — the result is already on the channel.
      (define-values (completion-sema on-complete) (make-callback-completion))
      (define-values (port state verifier get-code)
        (start-callback-server #:timeout 10 #:on-complete on-complete))
      (callback-send-request port state "nonblock-code")
      ;; Wait for the callback to be fully processed (event replaces the
      ;; 800 ms fixed wait), then get-code must return immediately.
      (check-true (wait-for-callback-completion completion-sema 10))
      (define before (current-inexact-milliseconds))
      (define code (get-code))
      (define after (current-inexact-milliseconds))
      (check-equal? code "nonblock-code")
      (check-true (< (- after before) 500)
                  (format "get-code should return immediately, but took ~a ms" (- after before))))))

(module+ main
  (run-tests security-tests))
