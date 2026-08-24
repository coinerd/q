#lang racket/base

;; q/tests/helpers/oauth-callback-fixtures.rkt — deterministic fixtures for
;; OAuth callback-server tests.
;;
;; W2 fast-timesink remediation (v1.00.16): hoisted from
;; tests/test-oauth-callback-security.rkt and
;; tests/test-oauth-callback-nonblocking.rkt, which previously waited on fixed
;; `sleep`/alarm-evt delays and probed the listener with raw TCP connects.
;;
;; Two hazards motivate this fixture layer:
;;  1. Probing the listener BEFORE the callback is processed is unsafe: the
;;     server is one-shot, so a probe connection (connect+close, no request)
;;     is accepted as a bogus "callback" and completes the server with #f
;;     before the real callback arrives. Probes must therefore be synchronized
;;     on the server's #:on-complete event, which fires only after the
;;     listener is closed and the result stored.
;;  2. Fixed sleeps make tests slow and flaky. The completion semaphore below
;;     is an explicit event wait: the test proceeds the instant completion
;;     happens, deterministically, with no polling.
;;
;; This file is a support module: the fast-suite inventory excludes
;; q/tests/helpers/*.rkt, so adding builders here never changes the suite
;; inventory or shard plan.

(provide make-callback-completion
         wait-for-callback-completion
         callback-send-request
         callback-probe-listener)

(require racket/tcp)

;; Builds a completion event for `start-callback-server`'s #:on-complete seam.
;; Returns (values completion-sema on-complete). Pass on-complete to the
;; server; the semaphore is posted exactly once, after the listener is closed
;; and the result stored — i.e. "fully complete, consumer not required".
(define (make-callback-completion)
  (define sema (make-semaphore 0))
  (values sema (lambda (_code) (semaphore-post sema))))

;; Blocks until the server's try-complete! has run (listener closed, result
;; stored, accept loop stopped). Returns #t on completion; returns #f if the
;; event did not arrive within timeout-sec — which only happens when the
;; completion path itself regresses to blocking (e.g. a channel-put), letting
;; the test fail explicitly instead of hanging the suite.
(define (wait-for-callback-completion completion-sema [timeout-sec 10])
  (not (eq? 'timeout (sync/timeout (* timeout-sec 1000) completion-sema))))

;; Sends one OAuth callback request (connect, write, close) and returns
;; without waiting for the server. Connection errors are swallowed: after
;; completion the listener is closed, so later sends legitimately fail.
(define (callback-send-request port state code)
  (with-handlers ([exn:fail? (lambda (e) (void))])
    (define-values (in out) (tcp-connect "127.0.0.1" port))
    (fprintf out
             "GET /callback?code=~a&state=~a HTTP/1.1\r\nHost: localhost\r\n\r\n"
             code
             state)
    (flush-output out)
    (close-output-port out)
    (close-input-port in)))

;; SAFE listener probe: returns 'connection-failed when the listener is
;; closed, 'connected otherwise. MUST only be called after
;; wait-for-callback-completion has returned #t — a probe before completion
;; would be accepted as a bogus connection and complete the one-shot server
;; with #f (see module comment).
(define (callback-probe-listener port)
  (with-handlers ([exn:fail? (lambda (e) 'connection-failed)])
    (define-values (in out) (tcp-connect "127.0.0.1" port))
    (close-input-port in)
    (close-output-port out)
    'connected))
