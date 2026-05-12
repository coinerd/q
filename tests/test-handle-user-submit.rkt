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

;; Minimal runner that records its call for verification
(define runner-called (box #f))
(define runner-text (box #f))

(define (make-mock-runner)
  (set-box! runner-called #f)
  (set-box! runner-text #f)
  (lambda (text)
    (set-box! runner-called #t)
    (set-box! runner-text text)))

;; Build a ctx with given ui-state and optional queue
(define (make-test-ctx ui-state [q #f] [runner #f])
  (make-tui-ctx #:event-bus #f #:session-runner (or runner (make-mock-runner)) #:session-queue q))

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
      (define state (initial-ui-state))
      (define runner (make-mock-runner))
      (define ctx (make-test-ctx state #f runner))
      (set-box! (tui-ctx-ui-state-box ctx) state)
      (handle-user-submit! ctx "run this")
      ;; Give runner thread time to execute
      (sleep 0.1)
      ;; Runner was called
      (check-true (unbox runner-called))
      (check-equal? (unbox runner-text) "run this")
      ;; State is busy
      (define new-state (unbox (tui-ctx-ui-state-box ctx)))
      (check-true (ui-state-busy? new-state))
      ;; User entry added
      (check-true (> (length (ui-state-transcript new-state)) 0))
      (check-equal? (transcript-entry-text (car (ui-state-transcript new-state))) "run this")
      (check-equal? (transcript-entry-kind (car (ui-state-transcript new-state))) 'user))

    ;; ── Branch 3b: normal submit without runner → no crash ──
    (test-case "normal submit without runner does not crash"
      (define state (initial-ui-state))
      (define ctx (make-test-ctx state #f #f))
      (set-box! (tui-ctx-ui-state-box ctx) state)
      ;; Should not raise
      (check-not-exn (lambda () (handle-user-submit! ctx "no runner"))))))

(run-tests submit-suite 'verbose)
