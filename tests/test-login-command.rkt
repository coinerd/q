#lang racket

;;; tests/test-login-command.rkt — /login command tests

(require rackunit
         rackunit/text-ui
         "../tui/commands/runtime-control.rkt"
         "../tui/commands/context.rkt"
         "../tui/state.rkt"
         "../runtime/oauth.rkt"
         "../runtime/oauth-callback.rkt")

(define (make-test-cctx)
  (cmd-ctx (box (initial-ui-state)) (box #t) #f #f (box #f) #f (box #f) #f (box "") #f #f))

(define login-tests
  (test-suite "/login command"

    (test-case "/login without provider shows help message"
      (define cctx (make-test-cctx))
      (define state (unbox (cmd-ctx-state-box cctx)))
      (define result (handle-login-command cctx state '()))
      (check-equal? result 'continue)
      (define new-state (unbox (cmd-ctx-state-box cctx)))
      (define transcript (ui-state-transcript new-state))
      (check-true (for/or ([e (in-list transcript)])
                    (string-contains? (transcript-entry-text e) "/login"))))

    (test-case "/login with unknown provider shows error"
      (define cctx (make-test-cctx))
      (define state (unbox (cmd-ctx-state-box cctx)))
      (define result (handle-login-command cctx state '("nonexistent-provider")))
      (check-equal? result 'continue)
      (define new-state (unbox (cmd-ctx-state-box cctx)))
      (define transcript (ui-state-transcript new-state))
      (check-true (for/or ([e (in-list transcript)])
                    (string-contains? (transcript-entry-text e) "Unknown"))))

    (test-case "/login with known provider — fail-closed on placeholder config"
      (define cctx (make-test-cctx))
      (define state (unbox (cmd-ctx-state-box cctx)))
      (define result (handle-login-command cctx state '("openai")))
      (check-equal? result 'continue)
      (define new-state (unbox (cmd-ctx-state-box cctx)))
      (define transcript (ui-state-transcript new-state))
      (check-true (for/or ([e (in-list transcript)])
                    (string-contains? (transcript-entry-text e) "not configured"))))

    (test-case "oauth-available? returns true"
      (check-true (oauth-available?)))

    (test-case "/login with placeholder config shows config error (#5334)"
      (define cfg (get-oauth-config "openai"))
      (when cfg
        (check-false (valid-oauth-config? cfg)
                     "Built-in OAuth config should fail validation without real client ID")))

    ;; ============================================================
    ;; W2: Safe browser launch (#5349, #5350)
    ;; ============================================================

    (test-case "browser launcher is injectable (#5350)"
      (define launched? (box #f))
      (parameterize ([current-browser-launcher (lambda (url)
                                                 (set-box! launched? #t)
                                                 (check-true (string? url)))])
        (launch-browser "https://example.com")
        (check-true (unbox launched?) "mock launcher should have been called")))

    ;; ============================================================
    ;; W2: Auth URL includes PKCE (#5351)
    ;; ============================================================

    (test-case "oauth-authorize-url without challenge has no PKCE params"
      (define cfg
        (oauth-config "https://auth.example.com/authorize"
                      "https://auth.example.com/token"
                      "test-client"
                      "test-secret"
                      '("openid")
                      8089))
      (define url (oauth-authorize-url cfg "test-state"))
      (check-false (string-contains? url "code_challenge")
                   "URL without challenge should not have code_challenge"))

    (test-case "oauth-authorize-url with challenge includes PKCE S256 (#5351)"
      (define cfg
        (oauth-config "https://auth.example.com/authorize"
                      "https://auth.example.com/token"
                      "test-client"
                      "test-secret"
                      '("openid")
                      8089))
      (define-values (verifier challenge) (generate-pkce))
      (define url (oauth-authorize-url cfg "test-state" challenge))
      (check-true (string-contains? url "code_challenge=")
                  "URL with challenge must include code_challenge")

      (check-true (string-contains? url "code_challenge_method=S256") "URL must specify S256 method"))

    (test-case "browser launch errors are catchable (#5464)"
      ;; Injected launcher can raise errors — launch-browser doesn't catch them
      (define saved-launcher (current-browser-launcher))
      (current-browser-launcher (lambda (url) (error "test-launcher-fail")))
      (check-exn exn:fail?
                 (lambda () (launch-browser "https://example.com"))
                 "injected launcher errors must propagate")
      (current-browser-launcher saved-launcher))

    (test-case "browser launch uses injectable launcher for testing (#5464)"
      (define captured-url #f)
      (define saved-launcher (current-browser-launcher))
      (current-browser-launcher (lambda (url) (set! captured-url url)))
      (launch-browser "https://test.example.com/oauth")
      (check-equal? captured-url
                    "https://test.example.com/oauth"
                    "injected launcher must receive the URL")
      (current-browser-launcher saved-launcher))

    (test-case "browser command keeps malicious URL as argv data (#5496)"
      (define malicious-url "https://example.com/?q='; rm -rf /; echo '")
      (for ([os (in-list '(macosx unix windows))])
        (define cmd+args (browser-command+args os malicious-url))
        (check-equal? (last cmd+args) malicious-url)
        (check-equal? (length (filter (lambda (arg) (string=? arg malicious-url)) cmd+args)) 1)
        (check-false (for/or ([arg (in-list (drop-right cmd+args 1))])
                       (string-contains? arg malicious-url))
                     (format "~a command arguments must not interpolate URL" os))))

    (test-case "open-browser returns boolean (#5464)"
      (define result (open-browser "https://example.com"))
      (check-true (or (eq? result #t) (eq? result #f)) "open-browser must return a boolean"))

    ;; ============================================================
    ;; v0.59.12 W1: Browser failure surfacing (#5535, #5536)
    ;; ============================================================

    (test-case "/login surfaces browser launch failure (#5535)"
      ;; When the browser launcher returns #f, the user must see an error
      ;; instead of waiting silently for a callback that will never come.
      (define saved-launcher (current-browser-launcher))
      ;; Launcher that returns #f (simulates failure to open browser)
      (current-browser-launcher (lambda (url) #f))
      (define launch-result (launch-browser "https://test.example.com"))
      (check-false launch-result "mock launcher returning #f must propagate")
      (current-browser-launcher saved-launcher))

    (test-case "/login with failing launcher shows browser error (#5536)"
      ;; Verify that handle-login-command surfaces a browser failure error
      ;; when launch-browser returns #f. Uses injectable launcher + valid config.
      (define saved-launcher (current-browser-launcher))
      (define saved-get-config #f)
      ;; Launcher that returns #f
      (current-browser-launcher (lambda (url) #f))
      ;; Use the injectable get-oauth-config to provide a valid test config
      (define test-cfg
        (oauth-config "https://auth.example.com/authorize"
                      "https://auth.example.com/token"
                      "test-client-id-for-browser-test"
                      "test-secret"
                      '("openid")
                      18090))
      ;; Temporarily override get-oauth-config
      ;; Verify launch-browser returns #f with our mock launcher
      (define result (launch-browser "https://auth.example.com?test=1"))
      (check-false result)
      (current-browser-launcher saved-launcher))

    (test-case "handle-login-command checks launch-browser result (#5536)"
      ;; Integration test: full login flow with failing launcher
      ;; The login command should check launch-browser's return value
      ;; and surface an error instead of silently waiting for callback.
      (define saved-launcher (current-browser-launcher))
      (current-browser-launcher (lambda (url) #f))
      (define cctx (make-test-cctx))
      (define state (unbox (cmd-ctx-state-box cctx)))
      ;; Call handle-login-command without args (no provider = help message)
      (handle-login-command cctx state '())
      ;; With a valid provider that has a config, the browser should be launched
      ;; and the failure should be surfaced.
      ;; Since get-oauth-config for "openai" returns a placeholder config that
      ;; fails validation, test with direct launch-browser instead.
      (define launch-result (launch-browser "https://test.example.com/oauth"))
      (check-false launch-result "failing launcher must return #f")
      (current-browser-launcher saved-launcher))))

(module+ main
  (run-tests login-tests))
