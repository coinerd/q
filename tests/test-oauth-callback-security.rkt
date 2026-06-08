#lang racket

;; @speed fast  ;; @suite security

;;; tests/test-oauth-callback-security.rkt — OAuth2 security regression tests
;;;
;;; Tests for v0.59.1 W0 (#5340): RFC7636 PKCE + CSPRNG primitives
;;; Tests for v0.59.1 W1 (#5344): Callback lifecycle, query decoding, CSRF

(require rackunit
         rackunit/text-ui
         file/sha1
         racket/string
         racket/tcp
         "../runtime/auth/oauth-callback.rkt")

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
      (define-values (port state verifier get-code) (start-callback-server #:timeout 10))
      ;; Send valid callback
      (thread (lambda ()
                (sync (alarm-evt (+ (current-inexact-milliseconds) 200)))
                (with-handlers ([exn:fail? (lambda (e) (void))])
                  (define-values (in out) (tcp-connect "127.0.0.1" port))
                  (fprintf out
                           "GET /callback?code=auth-code&state=~a HTTP/1.1\r\nHost: localhost\r\n\r\n"
                           state)
                  (flush-output out)
                  (close-output-port out)
                  (close-input-port in))))
      (define code (get-code))
      (check-equal? code "auth-code")
      ;; Verify listener is closed — second connection should fail
      (sync (alarm-evt (+ (current-inexact-milliseconds) 500)))
      (define second-connect
        (with-handlers ([exn:fail? (lambda (e) 'connection-failed)])
          (define-values (in out) (tcp-connect "127.0.0.1" port))
          (close-input-port in)
          (close-output-port out)
          'connected))
      (check-eq? second-connect 'connection-failed "listener should be closed after one-shot"))

    (test-case "callback server closes after invalid state (#5346)"
      (define-values (port state verifier get-code) (start-callback-server #:timeout 10))
      (thread (lambda ()
                (sync (alarm-evt (+ (current-inexact-milliseconds) 200)))
                (with-handlers ([exn:fail? (lambda (e) (void))])
                  (define-values (in out) (tcp-connect "127.0.0.1" port))
                  (fprintf out
                           "GET /callback?code=abc&state=wrong HTTP/1.1\r\nHost: localhost\r\n\r\n")
                  (flush-output out)
                  (close-output-port out)
                  (close-input-port in))))
      (define code (get-code))
      (check-false code "invalid state should return #f and close server"))

    ;; ============================================================
    ;; W1: CSRF strictness + timeout (#5347)
    ;; ============================================================

    (test-case "CSRF: missing state parameter rejects code (#5347)"
      (define-values (port state verifier get-code) (start-callback-server #:timeout 10))
      (thread (lambda ()
                (sync (alarm-evt (+ (current-inexact-milliseconds) 200)))
                (with-handlers ([exn:fail? (lambda (e) (void))])
                  (define-values (in out) (tcp-connect "127.0.0.1" port))
                  (fprintf out "GET /callback?code=abc HTTP/1.1\r\nHost: localhost\r\n\r\n")
                  (flush-output out)
                  (close-output-port out)
                  (close-input-port in))))
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
      ;; Callback arrives immediately; get-code is called after
      (define-values (port state verifier get-code) (start-callback-server #:timeout 10))
      (thread
       (lambda ()
         (with-handlers ([exn:fail? (lambda (e) (void))])
           (define-values (in out) (tcp-connect "127.0.0.1" port))
           (fprintf out "GET /callback?code=delayed-code&state=~a HTTP/1.1
Host: localhost

" state)
           (flush-output out)
           (close-output-port out)
           (close-input-port in))))
      ;; Wait a moment to let callback arrive and be processed
      (sync (alarm-evt (+ (current-inexact-milliseconds) 500)))
      (define code (get-code))
      (check-equal? code "delayed-code" "delayed consumer must still receive code"))

    (test-case "concurrent double callback: only first wins (#5463)"
      (define-values (port state verifier get-code) (start-callback-server #:timeout 10))
      ;; Send two callbacks nearly simultaneously
      (thread
       (lambda ()
         (with-handlers ([exn:fail? (lambda (e) (void))])
           (define-values (in out) (tcp-connect "127.0.0.1" port))
           (fprintf out "GET /callback?code=first-code&state=~a HTTP/1.1
Host: localhost

" state)
           (flush-output out)
           (close-output-port out)
           (close-input-port in))))
      (thread
       (lambda ()
         (sync (alarm-evt (+ (current-inexact-milliseconds) 100)))
         (with-handlers ([exn:fail? (lambda (e) (void))])
           (define-values (in out) (tcp-connect "127.0.0.1" port))
           (fprintf out "GET /callback?code=second-code&state=~a HTTP/1.1
Host: localhost

" state)
           (flush-output out)
           (close-output-port out)
           (close-input-port in))))
      (define code (get-code))
      (check-equal? code "first-code" "first callback must win")
      ;; Second connection should fail after server shuts down
      (sync (alarm-evt (+ (current-inexact-milliseconds) 500)))
      (define third-connect
        (with-handlers ([exn:fail? (lambda (e) 'connection-failed)])
          (define-values (in out) (tcp-connect "127.0.0.1" port))
          (close-input-port in)
          (close-output-port out)
          'connected))
      (check-eq? third-connect 'connection-failed "server must close after first result"))

    (test-case "atomic completion: no unsynchronized shared state (#5463)"
      ;; Verify the semaphore model: multiple rapid connections don't corrupt state
      (for ([_ (in-range 5)])
        (define-values (port state verifier get-code) (start-callback-server #:timeout 5))
        (thread
         (lambda ()
           (with-handlers ([exn:fail? (lambda (e) (void))])
             (define-values (in out) (tcp-connect "127.0.0.1" port))
             (fprintf out "GET /callback?code=iter-code&state=~a HTTP/1.1
Host: localhost

" state)
             (flush-output out)
             (close-output-port out)
             (close-input-port in))))
        (define code (get-code))
        (check-equal? code "iter-code" "each iteration must get the correct code")))

    ;; ============================================================
    ;; W1: Cleanup independence + nonblocking completion (#5497)
    ;; ============================================================

    (test-case "cleanup is independent of consumer (#5497)"
      ;; The listener must be closed BEFORE get-code is called,
      ;; so resources are reclaimed even if the consumer delays.
      (define-values (port state verifier get-code) (start-callback-server #:timeout 10))
      (thread
       (lambda ()
         (sync (alarm-evt (+ (current-inexact-milliseconds) 200)))
         (with-handlers ([exn:fail? (lambda (e) (void))])
           (define-values (in out) (tcp-connect "127.0.0.1" port))
           (fprintf out
                    "GET /callback?code=cleanup-test&state=~a HTTP/1.1\r\nHost: localhost\r\n\r\n"
                    state)
           (flush-output out)
           (close-output-port out)
           (close-input-port in))))
      ;; Wait for server to process callback and close listener
      (sync (alarm-evt (+ (current-inexact-milliseconds) 800)))
      ;; Listener should be closed — new connections must fail
      (define probe
        (with-handlers ([exn:fail? (lambda (e) 'connection-failed)])
          (define-values (in out) (tcp-connect "127.0.0.1" port))
          (close-input-port in)
          (close-output-port out)
          'connected))
      (check-eq? probe 'connection-failed "listener must be closed before consumer calls get-code")
      ;; Consumer can still get the code (delayed read from channel)
      (define code (get-code))
      (check-equal? code "cleanup-test" "delayed consumer must still receive code after cleanup"))

    (test-case "nonblocking completion: get-code returns immediately after callback (#5497)"
      ;; Once the callback has been processed, get-code should not block
      ;; — the result is already on the channel.
      (define-values (port state verifier get-code) (start-callback-server #:timeout 10))
      (thread
       (lambda ()
         (sync (alarm-evt (+ (current-inexact-milliseconds) 200)))
         (with-handlers ([exn:fail? (lambda (e) (void))])
           (define-values (in out) (tcp-connect "127.0.0.1" port))
           (fprintf out
                    "GET /callback?code=nonblock-code&state=~a HTTP/1.1\r\nHost: localhost\r\n\r\n"
                    state)
           (flush-output out)
           (close-output-port out)
           (close-input-port in))))
      ;; Wait for callback to be fully processed
      (sync (alarm-evt (+ (current-inexact-milliseconds) 800)))
      ;; get-code should return immediately (result already on channel)
      (define before (current-inexact-milliseconds))
      (define code (get-code))
      (define after (current-inexact-milliseconds))
      (check-equal? code "nonblock-code")
      (check-true (< (- after before) 500)
                  (format "get-code should return immediately, but took ~a ms" (- after before))))))

(module+ main
  (run-tests security-tests))
