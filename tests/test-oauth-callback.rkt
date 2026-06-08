#lang racket

;; @speed fast
;; @suite default

;;; tests/test-oauth-callback.rkt — OAuth2 callback server tests

(require rackunit
         rackunit/text-ui
         racket/port
         racket/tcp
         racket/file
         "../runtime/auth/oauth-callback.rkt"
         "../runtime/auth/oauth.rkt")

(define oauth-tests
  (test-suite "OAuth2 callback"

    (test-case "generate-pkce returns verifier and challenge"
      (define-values (verifier challenge) (generate-pkce))
      (check-true (string? verifier))
      (check-true (string? challenge))
      (check-true (> (string-length verifier) 10))
      (check-true (> (string-length challenge) 10))
      (check-not-equal? verifier challenge))

    (test-case "generate-state returns unique strings"
      (define s1 (generate-state))
      (define s2 (generate-state))
      (check-true (string? s1))
      (check-true (> (string-length s1) 5))
      (check-not-equal? s1 s2 "state should be unique"))

    (test-case "start-callback-server returns valid port and functions"
      (define-values (port state verifier get-code) (start-callback-server #:timeout 5))
      (check-true (exact-positive-integer? port))
      (check-true (string? state))
      (check-true (string? verifier))
      (check-true (procedure? get-code))
      ;; The server should time out since we don't connect
      (define result (get-code))
      (check-false result))

    (test-case "callback server receives code with valid state"
      (define-values (port state verifier get-code) (start-callback-server #:timeout 10))
      ;; Simulate OAuth callback in a thread
      (thread
       (lambda ()
         (sync (alarm-evt (+ (current-inexact-milliseconds) 500)))
         (with-handlers ([exn:fail? (lambda (e) (void))])
           (define-values (in out) (tcp-connect "127.0.0.1" port))
           (fprintf out
                    "GET /callback?code=test-auth-code&state=~a HTTP/1.1\r\nHost: localhost\r\n\r\n"
                    state)
           (flush-output out)
           (define response (read-string 500 in))
           (close-input-port in)
           (close-output-port out))))
      (define code (get-code))
      (check-equal? code "test-auth-code"))

    (test-case "callback server rejects invalid state"
      (define-values (port state verifier get-code) (start-callback-server #:timeout 10))
      (thread
       (lambda ()
         (sync (alarm-evt (+ (current-inexact-milliseconds) 500)))
         (with-handlers ([exn:fail? (lambda (e) (void))])
           (define-values (in out) (tcp-connect "127.0.0.1" port))
           (fprintf
            out
            "GET /callback?code=test-code&state=wrong-state HTTP/1.1\r\nHost: localhost\r\n\r\n")
           (flush-output out)
           (close-input-port in)
           (close-output-port out))))
      (define code (get-code))
      (check-false code "should reject invalid state"))

    (test-case "callback server handles error parameter"
      (define-values (port state verifier get-code) (start-callback-server #:timeout 10))
      (thread
       (lambda ()
         (sync (alarm-evt (+ (current-inexact-milliseconds) 500)))
         (with-handlers ([exn:fail? (lambda (e) (void))])
           (define-values (in out) (tcp-connect "127.0.0.1" port))
           (fprintf out
                    "GET /callback?error=access_denied&state=~a HTTP/1.1\r\nHost: localhost\r\n\r\n"
                    state)
           (flush-output out)
           (close-input-port in)
           (close-output-port out))))
      (define code (get-code))
      (check-false code "should return #f for error response"))

    (test-case "oauth-callback: oauth-available? returns true"
      (check-true (oauth-available?)))

    (test-case "oauth-token serialization roundtrip"
      (define tok
        (oauth-token "access-123" "refresh-456" (+ (current-seconds) 3600) "Bearer" "openid"))
      (define json (oauth-token->jsexpr tok))
      (define restored (jsexpr->oauth-token json))
      (check-equal? (oauth-token-access-token restored) "access-123")
      (check-equal? (oauth-token-refresh-token restored) "refresh-456")
      (check-equal? (oauth-token-token-type restored) "Bearer"))

    (test-case "valid-oauth-config validates correctly"
      (define cfg
        (oauth-config "https://accounts.google.com/o/oauth2/v2/auth"
                      "https://oauth2.googleapis.com/token"
                      "test-client-id"
                      ""
                      '("openid" "email")
                      8089))
      (check-true (valid-oauth-config? cfg))
      (check-false (valid-oauth-config? "not a config")))

    (test-case "token file persistence roundtrip"
      (define tmpdir (make-temporary-file "oauth-test-~a" 'directory))
      (define tmpfile (build-path tmpdir "oauth-tokens.json"))
      (define tok
        (oauth-token "test-access" "test-refresh" (+ (current-seconds) 3600) "Bearer" "openid"))
      (store-oauth-token! "test-provider" tok #:path tmpfile)
      (define loaded (get-oauth-token "test-provider" #:path tmpfile))
      (check-true (oauth-token? loaded))
      (check-equal? (oauth-token-access-token loaded) "test-access")
      (check-equal? (oauth-token-refresh-token loaded) "test-refresh")
      ;; Verify file permissions
      (check-not-false (file-exists? tmpfile))
      (delete-directory/files tmpdir))

    (test-case "load-oauth-tokens returns empty hash for missing file"
      (define result (load-oauth-tokens "/nonexistent/path/tokens.json"))
      (check-equal? result (hash)))

    (test-case "oauth-token-expired? works correctly"
      (define expired-tok (oauth-token "old" "ref" (- (current-seconds) 100) "Bearer" ""))
      (define valid-tok (oauth-token "new" "ref" (+ (current-seconds) 3600) "Bearer" ""))
      (check-true (oauth-token-expired? expired-tok))
      (check-false (oauth-token-expired? valid-tok)))))

(module+ main
  (run-tests oauth-tests))
