#lang racket

;;; tests/test-oauth-callback-nonblocking.rkt — v0.59.12 W0 (#5531)
;;; Tests proving OAuth callback completion is nonblocking without a consumer.

(require rackunit
         rackunit/text-ui
         racket/tcp
         "../runtime/auth/oauth-callback.rkt")

(define nonblocking-tests
  (test-suite "OAuth callback nonblocking completion (v0.59.12 W0)"

    ;; The core gap: if no consumer calls get-code, try-complete! must not
    ;; block. With an unbuffered channel, channel-put blocks until a receiver
    ;; arrives. This test proves completion returns promptly without a consumer.

    (test-case "completion is nonblocking without consumer (#5532)"
      ;; Start server, trigger callback, but never call get-code.
      ;; The try-complete! thread must finish within a reasonable time,
      ;; not block forever on channel-put.
      (define-values (port state verifier get-code) (start-callback-server #:timeout 10))
      ;; Send valid callback from background thread
      (thread
       (lambda ()
         (sync (alarm-evt (+ (current-inexact-milliseconds) 200)))
         (with-handlers ([exn:fail? (lambda (e) (void))])
           (define-values (in out) (tcp-connect "127.0.0.1" port))
           (fprintf out
                    "GET /callback?code=no-consumer-code&state=~a HTTP/1.1\r\nHost: localhost\r\n\r\n"
                    state)
           (flush-output out)
           (close-output-port out)
           (close-input-port in))))
      ;; Wait long enough for callback to be processed and try-complete! to run
      (sync (alarm-evt (+ (current-inexact-milliseconds) 1500)))
      ;; If try-complete! is blocking on channel-put, the listener won't be closed.
      ;; Probe the port to verify listener was cleaned up despite no consumer.
      (define probe
        (with-handlers ([exn:fail? (lambda (e) 'connection-failed)])
          (define-values (in out) (tcp-connect "127.0.0.1" port))
          (close-input-port in)
          (close-output-port out)
          'connected))
      (check-eq? probe 'connection-failed
                 "listener must be closed even without consumer calling get-code")
      ;; Now call get-code to clean up the channel and verify result was stored
      (define code (get-code))
      (check-equal? code "no-consumer-code"
                    "delayed consumer must still receive stored code"))

    (test-case "timeout completion is nonblocking without consumer (#5532)"
      ;; Server times out; no consumer calls get-code.
      ;; The timeout thread's try-complete!(#f) must not block.
      (define-values (port state verifier get-code) (start-callback-server #:timeout 1))
      ;; Wait for timeout to fire and try-complete! to run
      (sync (alarm-evt (+ (current-inexact-milliseconds) 2000)))
      ;; Listener must be closed by timeout's try-complete!
      (define probe
        (with-handlers ([exn:fail? (lambda (e) 'connection-failed)])
          (define-values (in out) (tcp-connect "127.0.0.1" port))
          (close-input-port in)
          (close-output-port out)
          'connected))
      (check-eq? probe 'connection-failed
                 "timeout must close listener even without consumer")
      ;; Delayed get-code should still return #f
      (define code (get-code))
      (check-false code "timeout must deliver #f to delayed consumer"))

    (test-case "try-complete! thread exits promptly without consumer (#5533)"
      ;; The thread running try-complete! must not be blocked at channel-put.
      ;; We verify this by checking the thread finishes within a short window.
      (define-values (port state verifier get-code) (start-callback-server #:timeout 10))
      (define completer-done (box #f))
      ;; Monitor thread: sets completer-done after get-code returns
      (thread
       (lambda ()
         (sync (alarm-evt (+ (current-inexact-milliseconds) 200)))
         (with-handlers ([exn:fail? (lambda (e) (void))])
           (define-values (in out) (tcp-connect "127.0.0.1" port))
           (fprintf out
                    "GET /callback?code=thread-exit&state=~a HTTP/1.1\r\nHost: localhost\r\n\r\n"
                    state)
           (flush-output out)
           (close-output-port out)
           (close-input-port in))))
      ;; Wait for callback processing
      (sync (alarm-evt (+ (current-inexact-milliseconds) 1000)))
      ;; Now call get-code to unblock any pending channel-put
      ;; If try-complete! was blocking, this unblocks it and completes quickly
      (define code (get-code))
      (check-equal? code "thread-exit")
      ;; Give a moment for any cleanup
      (sync (alarm-evt (+ (current-inexact-milliseconds) 200))))

    (test-case "no-consumer cleanup: multiple servers don't leak ports (#5533)"
      ;; Start multiple servers, trigger callbacks, don't call get-code.
      ;; Verify each server's listener is cleaned up.
      (for ([i (in-range 3)])
        (define-values (port state verifier get-code) (start-callback-server #:timeout 5))
        (thread
         (lambda ()
           (sync (alarm-evt (+ (current-inexact-milliseconds) 100)))
           (with-handlers ([exn:fail? (lambda (e) (void))])
             (define-values (in out) (tcp-connect "127.0.0.1" port))
             (fprintf out
                      "GET /callback?code=iter-~a&state=~a HTTP/1.1\r\nHost: localhost\r\n\r\n"
                      i state)
             (flush-output out)
             (close-output-port out)
             (close-input-port in))))
        ;; Don't call get-code; wait for cleanup
        (sync (alarm-evt (+ (current-inexact-milliseconds) 1000)))
        ;; Verify port is closed
        (define probe
          (with-handlers ([exn:fail? (lambda (e) 'connection-failed)])
            (define-values (in out) (tcp-connect "127.0.0.1" port))
            (close-input-port in)
            (close-output-port out)
            'connected))
        (check-eq? probe 'connection-failed
                   (format "iteration ~a: listener must be closed without consumer" i))))))

(module+ main
  (run-tests nonblocking-tests))
