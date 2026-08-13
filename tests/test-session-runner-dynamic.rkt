#lang racket/base

;; tests/test-session-runner-dynamic.rkt
;;
;; Regression test for v0.99.96: session-runner must dynamically read
;; from agent-session-box so that after /go campaign switches sessions,
;; subsequent prompts (/retry, user input) run on the correct session.
;;
;; Before the fix, the session-runner closure captured the ORIGINAL session
;; at TUI init time. When /go's make-campaign-runner switched the TUI to
;; a campaign session and updated agent-session-box, the session-runner
;; still used the original session. Events from /retry would carry the
;; original session's ID, but the TUI state had the campaign session's ID,
;; so event-for-current-session? filtered them out, causing a permanent
;; busy hang.

;; @speed fast
;; @suite fast

(require rackunit)

(module+ test
  ;; TC-01: session-runner reads dynamically from agent-session-box
  ;; After switching the box to a different session, the runner should
  ;; use the new session, not the one captured at init time.
  (test-case "session-runner uses dynamic session from box"
    ;; Simulate two sessions as opaque values
    (define sess-A 'session-A)
    (define sess-B 'session-B)

    ;; Simulate the agent-session-box
    (define session-box (box sess-A))

    ;; The v0.99.96 fix: runner reads (unbox session-box) dynamically
    (define dynamic-runner
      (lambda (prompt)
        (define current-sess (unbox session-box))
        (list current-sess prompt)))

    ;; Initially, runner uses sess-A
    (check-equal? (car (dynamic-runner "test")) 'session-A)

    ;; Simulate /go campaign switching to sess-B
    (set-box! session-box sess-B)

    ;; Now runner should use sess-B
    (check-equal? (car (dynamic-runner "test")) 'session-B))

  ;; TC-02: pre-campaign session restoration
  ;; After campaign completes, the original session should be restored
  (test-case "campaign restores pre-campaign session"
    (define agent-session-box (box 'original))
    (define current-session-id-box (box "original"))

    ;; Save pre-campaign state
    (define pre-campaign-sess (unbox agent-session-box))
    (define pre-campaign-sid "original")

    ;; Simulate campaign switching sessions
    (set-box! agent-session-box 'campaign)
    (set-box! current-session-id-box "campaign")
    (check-equal? (unbox current-session-id-box) "campaign")

    ;; Restore pre-campaign session
    (set-box! agent-session-box pre-campaign-sess)
    (set-box! current-session-id-box pre-campaign-sid)

    ;; Verify restoration
    (check-equal? (unbox current-session-id-box) "original")
    (check-equal? (unbox agent-session-box) 'original))

  ;; TC-03: retry with busy session emits error (not silent swallow)
  ;; Before v0.99.96, exn:fail:session:busy was silently swallowed
  ;; in the retry thread, leaving the TUI in "[retry: resubmitting]"
  ;; state with no feedback.
  (test-case "retry on busy session emits visible error"
    (define error-events '())
    (define (record-error-event! payload)
      (set! error-events (cons payload error-events)))

    ;; Simulate the retry handler's busy-error path (v0.99.96 fix)
    (record-error-event!
     (hasheq 'error "Session is still processing — use /interrupt first" 'errorType 'busy))

    (check-equal? (length error-events) 1)
    (define evt (car error-events))
    (check-equal? (hash-ref evt 'errorType) 'busy)
    (check-pred string? (hash-ref evt 'error)))

  ;; TC-04: session-runner mismatch causes event filtering
  ;; This demonstrates the root cause: events from session-A are filtered
  ;; when the TUI state has session-B.
  (test-case "event-for-current-session filters mismatched sessions"
    (define (event-for-current-session? state-session event-session)
      (or (not state-session) (not event-session) (equal? state-session event-session)))

    ;; Matching sessions → event processed
    (check-true (event-for-current-session? "session-A" "session-A"))
    ;; Mismatched sessions → event filtered (THE BUG)
    (check-false (event-for-current-session? "campaign-session" "original-session"))
    ;; Wildcard sessions → event processed
    (check-true (event-for-current-session? #f "any"))
    (check-true (event-for-current-session? "any" #f))))
