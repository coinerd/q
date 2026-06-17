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
         (only-in "../tui/state-events/core-handlers.rkt" handle-spawn-approval-requested)
         (only-in "../tui/render/message-layout.rkt"
                  styled-line?
                  styled-line-segments
                  styled-segment-text)
         (only-in "../util/event/event.rkt" make-event))

(define (make-approval-event caps task-preview)
  (make-event "mas.spawn-approval-requested"
              (current-inexact-milliseconds)
              "test-session"
              #f
              (hasheq 'capabilities caps 'task-preview task-preview)))

(define (make-initial-state)
  (initial-ui-state))

(define suite
  (test-suite "TUI Approval Event Reducer (v0.99.25 W1)"

    ;; ── Handler creates overlay ──

    (test-case "handle-spawn-approval-requested creates approval-prompt overlay"
      (define state (make-initial-state))
      (define evt (make-approval-event '(shell-exec) "Run deploy script"))
      (define new-state (handle-spawn-approval-requested state evt))
      (define overlay (ui-state-active-overlay new-state))
      (check-not-false overlay)
      (check-equal? (overlay-state-type overlay) 'approval-prompt))

    (test-case "overlay content is list of styled-lines"
      (define state (make-initial-state))
      (define evt (make-approval-event '(shell-exec) "Run deploy"))
      (define new-state (handle-spawn-approval-requested state evt))
      (define content (overlay-state-content (ui-state-active-overlay new-state)))
      (check-true (list? content))
      (check-true (> (length content) 0))
      (for ([line (in-list content)])
        (check-true (styled-line? line))))

    (test-case "overlay content includes approval prompt text"
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

    ;; ── Extra field stores data ──

    (test-case "overlay extra stores capabilities and task-preview"
      (define state (make-initial-state))
      (define evt (make-approval-event '(shell-exec git-write) "Push to prod"))
      (define new-state (handle-spawn-approval-requested state evt))
      (define extra (overlay-state-extra (ui-state-active-overlay new-state)))
      (check-equal? (hash-ref extra 'capabilities) '(shell-exec git-write))
      (check-equal? (hash-ref extra 'task-preview) "Push to prod"))

    ;; ── Overlay replacement ──

    (test-case "handle-spawn-approval-requested replaces existing overlay"
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

    (test-case "multiple capabilities are formatted with commas"
      (define state (make-initial-state))
      (define evt (make-approval-event '(shell-exec git-write file-write) "Complex task"))
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

    (test-case "mas.spawn-approval-requested is registered in global registry"
      ;; This checks the global registry, not a test-local one
      (check-true (event-reducer-registered? "mas.spawn-approval-requested")))

    (test-case "apply-event-to-state dispatches to approval handler"
      (define state (make-initial-state))
      (define evt (make-approval-event '(shell-exec) "Test task"))
      (define new-state (apply-event-to-state state evt))
      (define overlay (ui-state-active-overlay new-state))
      (check-not-false overlay)
      (check-equal? (overlay-state-type overlay) 'approval-prompt))

    ;; ── Edge cases ──

    (test-case "empty capabilities list handled gracefully"
      (define state (make-initial-state))
      (define evt (make-approval-event '() "Harmless task"))
      (define new-state (handle-spawn-approval-requested state evt))
      (define overlay (ui-state-active-overlay new-state))
      (check-not-false overlay)
      (check-equal? (overlay-state-type overlay) 'approval-prompt))

    (test-case "empty task-preview handled gracefully"
      (define state (make-initial-state))
      (define evt (make-approval-event '(shell-exec) ""))
      (define new-state (handle-spawn-approval-requested state evt))
      (define overlay (ui-state-active-overlay new-state))
      (check-not-false overlay))))

(run-tests suite)
