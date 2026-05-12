#lang racket

;; tests/test-handle-user-submit.rkt — T-01: Test handle-user-submit! branches
;;
;; Tests the 3 branches of handle-user-submit! extracted from tui-render-loop.rkt:
;;   1. Busy-queue path: enqueue followup when agent is streaming
;;   2. Duplicate-debounce path: ignore rapid identical submits within 500ms
;;   3. Normal-submit path: set busy, add user entry, spawn runner thread

(require rackunit
         rackunit/text-ui
         "../tui/tui-render-loop.rkt"
         "../tui/state-types.rkt"
         "../tui/tui-keybindings.rkt"
         "../agent/queue.rkt")

;; Channel-based mock runner for deterministic thread synchronization
(define (make-sync-mock-runner ch)
  (lambda (text) (channel-put ch text)))

;; Build a ctx with given ui-state and optional queue
(define (make-test-ctx ui-state [q #f] [runner #f])
  (make-tui-ctx #:event-bus #f #:session-runner (or runner void) #:session-queue q))

;; Build a ui-state with a transcript containing a single user entry
(define (make-ui-state-with-user-entry text [timestamp (current-inexact-milliseconds)])
  (define entry (make-entry 'user text timestamp (hash)))
  (define base (initial-ui-state))
  (struct-copy ui-state base [transcript (list entry)]))

;; Build a busy ui-state
(define (make-busy-ui-state)
  (set-busy (initial-ui-state) #t))

(define submit-suite
  (test-suite "handle-user-submit! tests"

    ;; ── Branch 1: busy + queue → enqueue followup ──
    (test-case "busy with queue enqueues followup and shows notification"
      (define q (make-queue))
      (define busy-state (make-busy-ui-state))
      (define ctx (make-test-ctx busy-state q))
      (set-box! (tui-ctx-ui-state-box ctx) busy-state)
      (handle-user-submit! ctx "hello")
      ;; Queue should have the followup
      (define drained (dequeue-all-followups! q))
      (check-equal? drained '("hello"))
      ;; UI state should have a queued notification entry
      (define new-state (unbox (tui-ctx-ui-state-box ctx)))
      (check-true (> (length (ui-state-transcript new-state)) 0))
      (check-equal? (transcript-entry-kind (car (ui-state-transcript new-state))) 'system))

    ;; ── Branch 2: duplicate within 500ms → ignored ──
    (test-case "duplicate submit within 500ms is ignored"
      (define now (current-inexact-milliseconds))
      (define state (make-ui-state-with-user-entry "hello" now))
      (define ctx (make-test-ctx state))
      (set-box! (tui-ctx-ui-state-box ctx) state)
      (handle-user-submit! ctx "hello")
      ;; Should show duplicate notification
      (define new-state (unbox (tui-ctx-ui-state-box ctx)))
      (check-true (> (length (ui-state-transcript new-state)) 1))
      (check-equal? (transcript-entry-text (car (ui-state-transcript new-state)))
                    "[Duplicate input ignored]"))

    ;; ── Branch 3: normal submit → busy, user entry, runner ──
    (test-case "normal submit sets busy, adds user entry, calls runner"
      (define ch (make-channel))
      (define state (initial-ui-state))
      (define runner (make-sync-mock-runner ch))
      (define ctx (make-test-ctx state #f runner))
      (set-box! (tui-ctx-ui-state-box ctx) state)
      (handle-user-submit! ctx "run this")
      ;; Deterministic channel-based sync (2s timeout for slow CI)
      (define runner-result (sync/timeout 2.0 ch))
      (check-equal? runner-result "run this")
      ;; State is busy
      (define new-state (unbox (tui-ctx-ui-state-box ctx)))
      (check-true (ui-state-busy? new-state))
      ;; User entry added
      (check-true (> (length (ui-state-transcript new-state)) 0))
      (check-equal? (transcript-entry-text (car (ui-state-transcript new-state))) "run this")
      (check-equal? (transcript-entry-kind (car (ui-state-transcript new-state))) 'user))

    ;; NOTE: We do not test a #f-runner scenario because:
    ;;   1. make-tui-ctx contract requires #:session-runner procedure?
    ;;   2. handle-user-submit! calls (runner text) directly -- no guard
    ;;   3. A #f runner would need to bypass the contract boundary
    ;; If the contract is relaxed in future, add a guard in handle-user-submit!
    ;; and a test for it here.
    ))

(run-tests submit-suite 'verbose)
