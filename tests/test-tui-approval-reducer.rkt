#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-tui-approval-reducer.rkt
;; v0.99.25 W1 §5.3: Tests for the TUI event reducer for spawn approval.
;;
;; Tests verify:
;; - handle-spawn-approval-requested creates 'approval-prompt overlay
;; - Overlay content has correct prompt text
;; - Capabilities and task-preview are stored in overlay extra
;; - Existing overlay is replaced
;; - Event is registered in the reducer registry
;; - Multiple capabilities are formatted correctly

(require rackunit
         rackunit/text-ui
         racket/string
         "../tui/state-types.rkt"
         (only-in "../tui/state-events/registry.rkt"
                  event-reducer-registered?
                  apply-event-to-state
                  call-with-test-registry)
         (only-in "../tui/state-events/core-handlers.rkt"
                  handle-spawn-approval-requested
                  handle-spawn-approval-terminal)
         (only-in "../tui/render/message-layout.rkt"
                  styled-line?
                  styled-line-segments
                  styled-segment-text)
         (only-in "../util/event/event.rkt" make-event)
         (only-in "../tui/approval-channel.rkt"
                  make-approval-channel
                  set-approval-channel!
                  clear-approval-channel!
                  register-approval-request!
                  cancel-approval-request!))

(define (make-approval-event caps task-preview [request-id (register-approval-request!)])
  (make-event "mas.spawn-approval-requested"
              (current-inexact-milliseconds)
              "test-session"
              #f
              (hasheq 'capabilities caps 'task-preview task-preview 'request-id request-id)))

(define (make-terminal-event request-id)
  (make-event "mas.spawn-approval-terminal"
              (current-inexact-milliseconds)
              "test-session"
              #f
              (hasheq 'request-id request-id)))

(define (make-initial-state)
  (initial-ui-state))

(define (with-approval-channel thunk)
  (dynamic-wind (lambda () (set-approval-channel! (make-approval-channel #:timeout-ms 5000)))
                thunk
                clear-approval-channel!))

(define-syntax-rule (approval-test-case name body ...)
  (test-case name
    (with-approval-channel (lambda ()
                             body ...))))

(define suite
  (test-suite "TUI Approval Event Reducer (v0.99.25 W1)"

    ;; ── Handler creates overlay ──

    (approval-test-case "handle-spawn-approval-requested creates approval-prompt overlay"
                        (define state (make-initial-state))
                        (define evt (make-approval-event '(shell-exec) "Run deploy script"))
                        (define new-state (handle-spawn-approval-requested state evt))
                        (define overlay (ui-state-active-overlay new-state))
                        (check-not-false overlay)
                        (check-equal? (overlay-state-type overlay) 'approval-prompt))

    (approval-test-case "overlay content is list of styled-lines"
                        (define state (make-initial-state))
                        (define evt (make-approval-event '(shell-exec) "Run deploy"))
                        (define new-state (handle-spawn-approval-requested state evt))
                        (define content (overlay-state-content (ui-state-active-overlay new-state)))
                        (check-true (list? content))
                        (check-true (> (length content) 0))
                        (for ([line (in-list content)])
                          (check-true (styled-line? line))))

    (approval-test-case
     "overlay content includes approval prompt text"
     (define state (make-initial-state))
     (define evt (make-approval-event '(shell-exec) "Run deploy script"))
     (define new-state (handle-spawn-approval-requested state evt))
     (define content (overlay-state-content (ui-state-active-overlay new-state)))
     ;; Combine all segment texts to check for key phrases
     (define all-text
       (apply string-append
              (for/list ([line (in-list content)])
                (apply string-append
                       (for/list ([seg (in-list (styled-line-segments line))])
                         (styled-segment-text seg))))))
     (check-true (string-contains? all-text "Approval") "Should contain 'Approval'")
     (check-true (string-contains? all-text "shell-exec") "Should contain capability name")
     (check-true (string-contains? all-text "Run deploy script") "Should contain task preview"))

    (approval-test-case
     "multijob batch manifest renders one inspectable row per dangerous job"
     (define state (make-initial-state))
     (define evt
       (make-approval-event '(shell-exec git-write)
                            (string-append "Dangerous subagent batch (2 jobs)\n"
                                           "[job-a] task=\"deploy\" caps=shell-exec child=child-a\n"
                                           "[job-b] task=\"commit\" caps=git-write child=child-b")))
     (define new-state (handle-spawn-approval-requested state evt))
     (define content (overlay-state-content (ui-state-active-overlay new-state)))
     (define row-texts
       (for/list ([line (in-list content)])
         (apply string-append
                (for/list ([segment (in-list (styled-line-segments line))])
                  (styled-segment-text segment)))))
     (check-true (ormap (lambda (text) (string-contains? text "job-a")) row-texts))
     (check-true (ormap (lambda (text) (string-contains? text "job-b")) row-texts))
     (check-true (ormap (lambda (text) (string-contains? text "child-a")) row-texts))
     (check-true (ormap (lambda (text) (string-contains? text "child-b")) row-texts)))

    ;; ── Extra field stores data ──

    (approval-test-case "overlay extra stores capabilities and task-preview"
                        (define state (make-initial-state))
                        (define evt (make-approval-event '(shell-exec git-write) "Push to prod"))
                        (define new-state (handle-spawn-approval-requested state evt))
                        (define extra (overlay-state-extra (ui-state-active-overlay new-state)))
                        (check-equal? (hash-ref extra 'capabilities) '(shell-exec git-write))
                        (check-equal? (hash-ref extra 'task-preview) "Push to prod"))

    ;; ── Overlay replacement ──

    (approval-test-case
     "handle-spawn-approval-requested replaces existing overlay"
     (define state (make-initial-state))
     ;; First create a command-palette overlay
     (define state-with-overlay
       (struct-copy ui-state
                    state
                    [active-overlay (overlay-state 'command-palette '() "" 'top-left #f #f 0 #f)]))
     (define evt (make-approval-event '(file-write) "Edit config"))
     (define new-state (handle-spawn-approval-requested state-with-overlay evt))
     (define overlay (ui-state-active-overlay new-state))
     (check-equal? (overlay-state-type overlay) 'approval-prompt))

    ;; ── Multiple capabilities formatting ──

    (approval-test-case "multiple capabilities are formatted with commas"
                        (define state (make-initial-state))
                        (define evt
                          (make-approval-event '(shell-exec git-write file-write) "Complex task"))
                        (define new-state (handle-spawn-approval-requested state evt))
                        (define content (overlay-state-content (ui-state-active-overlay new-state)))
                        (define all-text
                          (apply string-append
                                 (for/list ([line (in-list content)])
                                   (apply string-append
                                          (for/list ([seg (in-list (styled-line-segments line))])
                                            (styled-segment-text seg))))))
                        (check-true (string-contains? all-text "shell-exec, git-write, file-write")))

    ;; ── Registration ──

    (approval-test-case "approval lifecycle reducers are registered in global registry"
                        ;; This checks the global registry, not a test-local one
                        (check-true (event-reducer-registered? "mas.spawn-approval-requested"))
                        (check-true (event-reducer-registered? "mas.spawn-approval-terminal")))

    (approval-test-case "apply-event-to-state dispatches to approval handler"
                        (define state (make-initial-state))
                        (define evt (make-approval-event '(shell-exec) "Test task"))
                        (define new-state (apply-event-to-state state evt))
                        (define overlay (ui-state-active-overlay new-state))
                        (check-not-false overlay)
                        (check-equal? (overlay-state-type overlay) 'approval-prompt))

    ;; ── Edge cases ──

    (approval-test-case "empty capabilities list handled gracefully"
                        (define state (make-initial-state))
                        (define evt (make-approval-event '() "Harmless task"))
                        (define new-state (handle-spawn-approval-requested state evt))
                        (define overlay (ui-state-active-overlay new-state))
                        (check-not-false overlay)
                        (check-equal? (overlay-state-type overlay) 'approval-prompt))

    (approval-test-case "empty task-preview handled gracefully"
                        (define state (make-initial-state))
                        (define evt (make-approval-event '(shell-exec) ""))
                        (define new-state (handle-spawn-approval-requested state evt))
                        (define overlay (ui-state-active-overlay new-state))
                        (check-not-false overlay))

    (approval-test-case
     "missing request-id is rejected at the UI boundary"
     (define evt
       (make-event "mas.spawn-approval-requested"
                   (current-inexact-milliseconds)
                   "test-session"
                   #f
                   (hasheq 'capabilities '(shell-exec) 'task-preview "uncorrelated")))
     (define state (make-initial-state))
     (define rejected (handle-spawn-approval-requested state evt))
     (check-eq? rejected state)
     (check-false (ui-state-active-overlay rejected)))

    (approval-test-case "cancelled request-id is rejected at the UI boundary"
                        (define stale-id (register-approval-request!))
                        (check-true (cancel-approval-request! stale-id))
                        (define state (make-initial-state))
                        (define rejected
                          (handle-spawn-approval-requested
                           state
                           (make-approval-event '(shell-exec) "already terminal" stale-id)))
                        (check-eq? rejected state)
                        (check-false (ui-state-active-overlay rejected)))

    (approval-test-case
     "terminal reducer promotes the next queued request"
     (define id-a (register-approval-request!))
     (define id-b (register-approval-request!))
     (define state-a
       (handle-spawn-approval-requested (make-initial-state)
                                        (make-approval-event '(shell-exec) "A" id-a)))
     (define state-ab
       (handle-spawn-approval-requested state-a (make-approval-event '(file-write) "B" id-b)))
     (define promoted (handle-spawn-approval-terminal state-ab (make-terminal-event id-a)))
     (define extra (overlay-state-extra (ui-state-active-overlay promoted)))
     (check-equal? (hash-ref extra 'request-id) id-b)
     (check-equal? (hash-ref extra 'approval-queue) '()))))

(run-tests suite)
