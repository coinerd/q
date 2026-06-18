#lang racket

;; @speed fast
;; @suite tui

;; tests/test-approval-channel-teardown.rkt
;; v0.99.26 W0 E-2: Verify exception-safe channel cleanup.
;;
;; The fix wraps run-tui-loop in dynamic-wind so that
;; clear-approval-channel! runs even when an exception propagates.
;; This test verifies the channel-clearing behavior at the
;; approval-channel module level (dynamic-wind integration is
;; tested via build verification + existing TUI test suites).

(require rackunit
         rackunit/text-ui
         (only-in "../tui/approval-channel.rkt"
                  make-approval-channel
                  set-approval-channel!
                  clear-approval-channel!
                  current-approval-channel
                  approval-put!
                  approval-await-result))

(define suite
  (test-suite "Approval Channel Teardown (v0.99.26 W0 E-2)"

    (test-case "channel is cleared after explicit clear call"
      ;; Simulate TUI init
      (set-approval-channel! (make-approval-channel))
      (check-not-false (current-approval-channel))
      ;; Simulate TUI teardown
      (clear-approval-channel!)
      (check-false (current-approval-channel)))

    (test-case "channel is cleared even after simulated exception"
      ;; Simulate TUI init
      (set-approval-channel! (make-approval-channel))
      (check-not-false (current-approval-channel))
      ;; Simulate exception path with dynamic-wind cleanup
      (with-handlers ([exn:fail? (lambda (_) (void))])
        (dynamic-wind void
                      (lambda () (raise (exn:fail "simulated crash" (current-continuation-marks))))
                      (lambda () (clear-approval-channel!))))
      ;; Channel should be cleared despite exception
      (check-false (current-approval-channel)))

    (test-case "channel works correctly after re-init following exception"
      ;; First session crashes
      (set-approval-channel! (make-approval-channel))
      (with-handlers ([exn:fail? (lambda (_) (void))])
        (dynamic-wind void
                      (lambda () (raise (exn:fail "crash" (current-continuation-marks))))
                      (lambda () (clear-approval-channel!))))
      (check-false (current-approval-channel))
      ;; Second session starts fresh
      (set-approval-channel! (make-approval-channel))
      (check-not-false (current-approval-channel))
      ;; Approval works in new session
      (define result-box (box #f))
      (define t (thread (lambda () (set-box! result-box (approval-await-result)))))
      (sleep 0.05)
      (approval-put! #t)
      (thread-wait t)
      (check-equal? (unbox result-box) #t)
      (clear-approval-channel!))

    (test-case "stale channel from crashed session does not block"
      ;; Simulate: session 1 crashes, channel cleared, session 2 non-interactive
      (set-approval-channel! (make-approval-channel))
      (with-handlers ([exn:fail? (lambda (_) (void))])
        (dynamic-wind void
                      (lambda () (raise (exn:fail "crash" (current-continuation-marks))))
                      (lambda () (clear-approval-channel!))))
      ;; Non-interactive: no channel → permissive
      (check-false (current-approval-channel))
      (check-equal? (approval-await-result) #t))))

(run-tests suite)
