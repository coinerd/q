#lang racket

;; @speed fast  ;; @suite security

;; tests/test-hitl-approval-integration.rkt
;; v0.99.25 W3 §5.3: Integration tests for the complete HITL approval flow.
;;
;; Tests the wiring from TUI init (set-approval-channel!) through
;; spawn-subagent (approval-await-result) to TUI key handler (approval-put!)
;; and teardown (clear-approval-channel!).
;;
;; Tests:
;; 1. TUI init sets channel, teardown clears it
;; 2. Full approval flow: set channel → thread blocks → put #t → result #t
;; 3. Full denial flow: set channel → thread blocks → put #f → result #f
;; 4. Timeout flow: set channel with short timeout → no put → returns #f
;; 5. Explicit non-interactive mode is permissive
;; 6. Interactive teardown remains fail closed
;; 7. Cross-thread: approval-put! from one thread, await from another

(require rackunit
         rackunit/text-ui
         racket/async-channel
         (only-in "../tui/approval-channel.rkt"
                  make-approval-channel
                  approval-channel?
                  approval-channel-ch
                  approval-channel-timeout-ms
                  set-approval-channel!
                  clear-approval-channel!
                  set-headless-approval-mode!
                  current-approval-channel
                  approval-await-result
                  approval-put!))

;; ── Test Suite ──

(define suite
  (test-suite "HITL Approval Integration (v0.99.25 W3)"

    ;; ── TUI Init/Teardown Wiring ──

    (test-case "set-approval-channel! makes channel available"
      (clear-approval-channel!) ; Start clean
      (check-false (current-approval-channel))
      (define ch (make-approval-channel))
      (set-approval-channel! ch)
      (check-pred approval-channel? (current-approval-channel))
      (clear-approval-channel!))

    (test-case "clear-approval-channel! removes channel"
      (set-approval-channel! (make-approval-channel))
      (check-not-false (current-approval-channel))
      (clear-approval-channel!)
      (check-false (current-approval-channel)))

    (test-case "channel can be replaced (re-init scenario)"
      (define ch1 (make-approval-channel))
      (set-approval-channel! ch1)
      (check-equal? (current-approval-channel) ch1)
      (define ch2 (make-approval-channel))
      (set-approval-channel! ch2)
      (check-equal? (current-approval-channel) ch2)
      (clear-approval-channel!))

    ;; ── Full Approval Flow (approve) ──

    (test-case "full flow: approve via cross-thread put"
      (clear-approval-channel!)
      ;; TUI init
      (set-approval-channel! (make-approval-channel))
      ;; Session thread: block on approval
      (define result-box (box #f))
      (define t (thread (lambda () (set-box! result-box (approval-await-result)))))
      ;; Simulate TUI key handler: user presses 'y'
      (sleep 0.05) ; Let the thread start blocking
      (approval-put! #t)
      (thread-wait t)
      (check-equal? (unbox result-box) #t)
      (clear-approval-channel!))

    ;; ── Full Denial Flow ──

    (test-case "full flow: deny via cross-thread put"
      (clear-approval-channel!)
      (set-approval-channel! (make-approval-channel))
      (define result-box (box #f))
      (define t (thread (lambda () (set-box! result-box (approval-await-result)))))
      ;; Simulate TUI key handler: user presses 'n'
      (sleep 0.05)
      (approval-put! #f)
      (thread-wait t)
      (check-equal? (unbox result-box) #f)
      (clear-approval-channel!))

    ;; ── Timeout Flow ──

    (test-case "timeout: deny when no response"
      (clear-approval-channel!)
      ;; Create channel with very short timeout (100ms)
      (set-approval-channel! (make-approval-channel #:timeout-ms 100))
      (define result (approval-await-result))
      (check-equal? result #f) ; Timeout → deny
      (clear-approval-channel!))

    ;; ── Non-Interactive (No Channel) ──

    (test-case "explicit non-interactive mode is permissive when no channel"
      (set-headless-approval-mode!)
      (check-false (current-approval-channel))
      (check-true (approval-await-result))
      (check-false (current-approval-channel)))

    ;; ── Teardown → fail closed ──

    (test-case "after teardown approval remains closed"
      (set-approval-channel! (make-approval-channel))
      (check-not-false (current-approval-channel))
      (clear-approval-channel!)
      (check-false (current-approval-channel))
      (check-false (approval-await-result)))

    ;; ── Cross-thread Simulation (Full TUI Lifecycle) ──

    (test-case "full TUI lifecycle: init → spawn → approve → teardown"
      ;; 1. TUI init
      (clear-approval-channel!)
      (set-approval-channel! (make-approval-channel))
      (check-not-false (current-approval-channel))

      ;; 2. Session thread spawns and requests approval
      (define result-box (box #f))
      (define t (thread (lambda () (set-box! result-box (approval-await-result)))))

      ;; 3. User approves via TUI
      (sleep 0.05)
      (approval-put! #t)
      (thread-wait t)
      (check-equal? (unbox result-box) #t)

      ;; 4. TUI teardown
      (clear-approval-channel!)
      (check-false (current-approval-channel))
      ;; 5. Surviving work cannot become permissive after teardown.
      (check-false (approval-await-result)))

    (test-case "full TUI lifecycle: init → spawn → deny → teardown"
      (clear-approval-channel!)
      (set-approval-channel! (make-approval-channel))

      (define result-box (box #f))
      (define t (thread (lambda () (set-box! result-box (approval-await-result)))))

      (sleep 0.05)
      (approval-put! #f)
      (thread-wait t)
      (check-equal? (unbox result-box) #f)

      (clear-approval-channel!)
      (check-false (current-approval-channel)))))

(run-tests suite)
