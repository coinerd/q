#lang racket/base

;; tests/test-tui-dedup.rkt — TUI tool-start dedup guard tests
;; v0.29.17 W1: Verify duplicate tool-start events do not append duplicate
;; transcript entries.

(require rackunit
         (only-in "../tui/state-types.rkt"
                  initial-ui-state
                  ui-state-transcript
                  ui-state-pending-tool-name
                  transcript-entry-kind
                  transcript-entry-text
                  transcript-entry-meta)
         (only-in "../util/event.rkt" make-event)
         "../tui/state-events.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-tool-start-event name args)
  (make-event "tool-execution-start"
              (current-inexact-milliseconds)
              "test-session"
              #f
              (hasheq 'tool-name name 'arguments args)))

(define (make-raw-tool-start-event name args)
  (make-event "tool.call.started"
              (current-inexact-milliseconds)
              "test-session"
              #f
              (hasheq 'name name 'arguments args)))

;; ============================================================
;; Dedup tests
;; ============================================================

(test-case "typed tool-execution-start appends entry on first occurrence"
  (define state (initial-ui-state))
  (define evt (make-tool-start-event "read" "{\"path\":\"/tmp/x.rkt\"}"))
  (define new-state (apply-event-to-state state evt))
  (check-equal? (length (ui-state-transcript new-state)) 1))

(test-case "duplicate typed tool-execution-start is suppressed"
  (define state (initial-ui-state))
  (define evt1 (make-tool-start-event "read" "{\"path\":\"/tmp/x.rkt\"}"))
  (define evt2 (make-tool-start-event "read" "{\"path\":\"/tmp/x.rkt\"}"))
  (define s1 (apply-event-to-state state evt1))
  (define s2 (apply-event-to-state s1 evt2))
  (check-equal? (length (ui-state-transcript s2)) 1))

(test-case "raw tool.call.started after typed is suppressed"
  (define state (initial-ui-state))
  (define typed-evt (make-tool-start-event "edit" "{\"path\":\"/tmp/x.rkt\"}"))
  (define raw-evt (make-raw-tool-start-event "edit" "{\"path\":\"/tmp/x.rkt\"}"))
  (define s1 (apply-event-to-state state typed-evt))
  (define s2 (apply-event-to-state s1 raw-evt))
  (check-equal? (length (ui-state-transcript s2)) 1))

(test-case "different tool names are not deduplicated"
  (define state (initial-ui-state))
  (define evt1 (make-tool-start-event "read" "{\"path\":\"/tmp/a.rkt\"}"))
  (define evt2 (make-tool-start-event "grep" "{\"pattern\":\"foo\"}"))
  (define s1 (apply-event-to-state state evt1))
  (define s2 (apply-event-to-state s1 evt2))
  (check-equal? (length (ui-state-transcript s2)) 2))

(test-case "pending-tool-name is still updated on dedup"
  (define state (initial-ui-state))
  (define evt (make-tool-start-event "write" "{\"path\":\"/tmp/x.rkt\"}"))
  (define s1 (apply-event-to-state state evt))
  (define s2 (apply-event-to-state s1 evt))
  (check-equal? (ui-state-pending-tool-name s2) "write"))
