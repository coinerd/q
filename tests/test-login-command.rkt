#lang racket

;;; tests/test-login-command.rkt — /login command tests

(require rackunit
         rackunit/text-ui
         "../tui/commands/runtime-control.rkt"
         "../tui/commands/context.rkt"
         "../tui/state.rkt"
         "../runtime/oauth.rkt")

(define (make-test-cctx)
  (cmd-ctx (box (initial-ui-state))
           (box #t)
           #f
           #f
           (box #f)
           #f
           (box #f)
           #f
           (box "")
           #f
           #f))

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

    (test-case "/login with known provider starts OAuth flow"
      (define cctx (make-test-cctx))
      (define state (unbox (cmd-ctx-state-box cctx)))
      (define result (handle-login-command cctx state '("openai")))
      (check-equal? result 'continue)
      ;; Should show "logging in..." message immediately
      (define new-state (unbox (cmd-ctx-state-box cctx)))
      (define transcript (ui-state-transcript new-state))
      (check-true (for/or ([e (in-list transcript)])
                    (string-contains? (transcript-entry-text e) "logging in"))))

    (test-case "oauth-available? returns true"
      (check-true (oauth-available?)))))

(module+ main
  (run-tests login-tests))
