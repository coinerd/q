#lang racket

;; @speed fast
;; @suite default
;; @boundary integration  ;; @requires network

;;; tests/test-oauth-callback-nonblocking.rkt — v0.59.12 W0 (#5531)
;;; Tests proving OAuth callback completion is nonblocking without a consumer.
;;;
;;; W2 (v1.00.16) remediation: fixed `sleep`/alarm-evt waits and unsafe
;;; listener probes replaced by the #:on-complete production seam + explicit
;;; semaphore waits from helpers/oauth-callback-fixtures.rkt. Every test-case
;;; and assertion is preserved one-for-one; no test deleted, weakened, or
;;; merged. (A listener probe before completion would be accepted as a bogus
;;; connection and complete the one-shot server with #f — probes are therefore
;;; only used after wait-for-callback-completion confirms the server is done.)

(require rackunit
         rackunit/text-ui
         "../runtime/auth/oauth-callback.rkt"
         "helpers/oauth-callback-fixtures.rkt")

(define nonblocking-tests
  (test-suite "OAuth callback nonblocking completion (v0.59.12 W0)"

    ;; The core gap: if no consumer calls get-code, try-complete! must not
    ;; block. With an unbuffered channel, channel-put blocks until a receiver
    ;; arrives. This test proves completion returns promptly without a consumer.

    (test-case "completion is nonblocking without consumer (#5532)"
      ;; Start server, trigger callback, but never call get-code before
      ;; completion. The #:on-complete event fires when try-complete! has run
      ;; (listener closed, result stored) — no consumer involved, no fixed
      ;; delay needed.
      (define-values (completion-sema on-complete) (make-callback-completion))
      (define-values (port state verifier get-code)
        (start-callback-server #:timeout 10 #:on-complete on-complete))
      (callback-send-request port state "no-consumer-code")
      ;; If try-complete! blocks (channel-put regression), the event never
      ;; fires and the test fails explicitly instead of probing a half-open
      ;; listener.
      (check-true (wait-for-callback-completion completion-sema 10)
                  "completion must not block without a consumer")
      ;; Probe the port to verify listener was cleaned up despite no consumer.
      (check-eq? (callback-probe-listener port)
                 'connection-failed
                 "listener must be closed even without consumer calling get-code")
      ;; Now call get-code to verify result was stored
      (define code (get-code))
      (check-equal? code "no-consumer-code" "delayed consumer must still receive stored code"))

    (test-case "timeout completion is nonblocking without consumer (#5532)"
      ;; Server times out; no consumer calls get-code.
      ;; The timeout thread's try-complete!(#f) must not block.
      (define-values (completion-sema on-complete) (make-callback-completion))
      (define-values (port state verifier get-code)
        (start-callback-server #:timeout 1 #:on-complete on-complete))
      ;; Wait for the timeout's try-complete! to run (fires at ~1s).
      (check-true (wait-for-callback-completion completion-sema 10)
                  "timeout try-complete! must not block without a consumer")
      ;; Listener must be closed by timeout's try-complete!
      (check-eq? (callback-probe-listener port)
                 'connection-failed
                 "timeout must close listener even without consumer")
      ;; Delayed get-code should still return #f
      (define code (get-code))
      (check-false code "timeout must deliver #f to delayed consumer"))

    (test-case "try-complete! thread exits promptly without consumer (#5533)"
      ;; The thread running try-complete! must not be blocked at channel-put.
      ;; The completion event is the promptness proof: it fires only after
      ;; try-complete! has finished closing the listener and storing the result.
      (define-values (completion-sema on-complete) (make-callback-completion))
      (define-values (port state verifier get-code)
        (start-callback-server #:timeout 10 #:on-complete on-complete))
      (callback-send-request port state "thread-exit")
      (check-true (wait-for-callback-completion completion-sema 10)
                  "try-complete! must finish without a consumer")
      (define code (get-code))
      (check-equal? code "thread-exit"))

    (test-case "no-consumer cleanup: multiple servers don't leak ports (#5533)"
      ;; Start multiple servers, trigger callbacks, don't call get-code.
      ;; Verify each server's listener is cleaned up.
      (for ([i (in-range 3)])
        (define-values (completion-sema on-complete) (make-callback-completion))
        (define-values (port state verifier get-code)
          (start-callback-server #:timeout 5 #:on-complete on-complete))
        (callback-send-request port state (format "iter-~a" i))
        ;; Don't call get-code; wait for the completion event instead of a
        ;; fixed second, then verify the port is closed.
        (check-true (wait-for-callback-completion completion-sema 10))
        (check-eq? (callback-probe-listener port)
                   'connection-failed
                   (format "iteration ~a: listener must be closed without consumer" i))))))

(module+ main
  (run-tests nonblocking-tests))
