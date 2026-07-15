#lang racket/base

;; @speed fast
;; @suite security

;; tests/test-approval-channel.rkt
;; v0.99.25 W0 §5.3: Tests for the HITL approval channel infrastructure.

(require rackunit
         rackunit/text-ui
         racket/async-channel
         "../tui/approval-channel.rkt")

(define suite
  (test-suite "HITL Approval Channel Infrastructure (v0.99.25 W0)"

    ;; ── make-approval-channel ──

    (test-case "make-approval-channel returns approval-channel?"
      (define ch (make-approval-channel))
      (check-true (approval-channel? ch)))

    (test-case "make-approval-channel with custom timeout"
      (define ch (make-approval-channel #:timeout-ms 5000))
      (check-equal? (approval-channel-timeout-ms ch) 5000))

    (test-case "make-approval-channel default timeout is 120000"
      (define ch (make-approval-channel))
      (check-equal? (approval-channel-timeout-ms ch) 120000))

    (test-case "approval-channel-ch returns an async-channel"
      (define ch (make-approval-channel))
      (check-true (async-channel? (approval-channel-ch ch))))

    ;; ── current-approval-channel (box) ──

    (test-case "current-approval-channel is #f by default (non-interactive)"
      (clear-approval-channel!)
      (check-false (current-approval-channel)))

    (test-case "set-approval-channel! then current-approval-channel returns it"
      (clear-approval-channel!)
      (define ch (make-approval-channel))
      (set-approval-channel! ch)
      (check-eq? (current-approval-channel) ch)
      (clear-approval-channel!))

    (test-case "clear-approval-channel! resets to #f"
      (define ch (make-approval-channel))
      (set-approval-channel! ch)
      (clear-approval-channel!)
      (check-false (current-approval-channel)))

    ;; ── approval-await-result ──

    (test-case "approval-await-result permits explicit headless mode"
      (set-headless-approval-mode!)
      (check-true (approval-await-result)))

    (test-case "approval-await-result returns #f on timeout (no channel-put)"
      (clear-approval-channel!)
      (define ch (make-approval-channel #:timeout-ms 50))
      (set-approval-channel! ch)
      (check-false (approval-await-result))
      (clear-approval-channel!))

    ;; ── approval-put! + approval-await-result (cross-thread) ──

    (test-case "approval-put! puts #t then await returns #t"
      (clear-approval-channel!)
      (define ch (make-approval-channel #:timeout-ms 5000))
      (set-approval-channel! ch)
      (approval-put! #t)
      (check-true (approval-await-result))
      (clear-approval-channel!))

    (test-case "approval-put! puts #f then await returns #f"
      (clear-approval-channel!)
      (define ch (make-approval-channel #:timeout-ms 5000))
      (set-approval-channel! ch)
      (approval-put! #f)
      (check-false (approval-await-result))
      (clear-approval-channel!))

    (test-case "approval-put! is no-op when no channel set"
      (clear-approval-channel!)
      (approval-put! #t)
      (approval-put! #f)
      (check-true #t))))

(run-tests suite)
