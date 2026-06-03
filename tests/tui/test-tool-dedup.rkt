#lang racket/base

;; BOUNDARY: io

;; tests/tui/test-tool-dedup.rkt — Tests for TUI tool-start/tool-end dedup
;;
;; Covers (NF4): TUI shows [TOOL] ?: ghost entries for tool calls
;; when tool.call.started and tool.execution.started fire in rapid
;; succession for the same tool. The dedup guard must suppress the
;; duplicate entry.

(require rackunit
         "../../tui/state-events.rkt"
         "../../tui/state-types.rkt"
         "../../util/message/protocol-types.rkt"
         "../tui/event-simulator.rkt")

;; ============================================================
;; NF4: tool-start dedup — rapid tool.call.started + tool.execution.started
;; ============================================================

(define tool-start-dedup-tests
  (test-suite "TUI Tool-Start Dedup"

    (test-case "single tool.call.started produces one tool-start entry"
      (define st (initial-ui-state))
      (define evt (make-test-event "tool.call.started" (hasheq 'name "bash" 'arguments "ls")))
      (define result (apply-event-to-state st evt))
      (check-equal? (transcript-length result) 1)
      (check-equal? (car (transcript-types result)) 'tool-start))

    (test-case "tool.call.started then tool.execution.started for same tool — no duplicate"
      (define st (initial-ui-state))
      (define evt1 (make-test-event "tool.call.started" (hasheq 'name "bash" 'arguments "ls")))
      (define evt2
        (make-test-event "tool.execution.started" (hasheq 'toolName "bash" 'arguments "ls")))
      (define result (simulate-events st (list evt1 evt2)))
      ;; Only one tool-start entry should appear
      (define tool-starts (filter (lambda (k) (eq? k 'tool-start)) (transcript-types result)))
      (check-equal? (length tool-starts) 1 "duplicate tool-start suppressed for same tool name"))

    (test-case "different tool names — both produce tool-start entries"
      (define st (initial-ui-state))
      (define evt1 (make-test-event "tool.call.started" (hasheq 'name "bash" 'arguments "ls")))
      (define evt2
        (make-test-event "tool.execution.started" (hasheq 'toolName "read" 'arguments "foo.txt")))
      (define result (simulate-events st (list evt1 evt2)))
      (define tool-starts (filter (lambda (k) (eq? k 'tool-start)) (transcript-types result)))
      (check-equal? (length tool-starts) 2 "different tools should each produce a tool-start"))

    (test-case "tool-end dedup — rapid completion events for same tool"
      (define st (initial-ui-state))
      (define events
        (list (make-test-event "tool.call.started" (hasheq 'name "bash" 'arguments "ls"))
              (make-test-event "tool.execution.completed"
                               (hasheq 'toolName "bash" 'resultSummary 'completed))
              (make-test-event "tool.execution.completed"
                               (hasheq 'toolName "bash" 'resultSummary 'completed))))
      (define result (simulate-events st events))
      (define tool-ends
        (filter (lambda (k) (memq k '(tool-end tool-fail))) (transcript-types result)))
      (check-equal? (length tool-ends) 1 "duplicate tool-end suppressed for same tool name"))

    (test-case "dedup window — tool-start 11 events apart is not suppressed"
      (define st (initial-ui-state))
      ;; First tool.call.started for "bash"
      (define evt-first (make-test-event "tool.call.started" (hasheq 'name "bash" 'arguments "ls")))
      ;; 10 intervening events for different tools (exceeds dedup-window-size of 10)
      (define intervening
        (for/list ([i (in-range 10)])
          (make-test-event "tool.call.started" (hasheq 'name (format "other-~a" i) 'arguments ""))))
      ;; Second tool.call.started for "bash" after 10 intervening events
      (define evt-second (make-test-event "tool.call.started" (hasheq 'name "bash" 'arguments "ls")))
      (define result (simulate-events st (cons evt-first (append intervening (list evt-second)))))
      (define tool-starts (filter (lambda (k) (eq? k 'tool-start)) (transcript-types result)))
      ;; 12 tool-start entries: 10 "other-N" + 2 "bash" (window exceeded)
      (check-equal? (length tool-starts)
                    12
                    "tool-start beyond dedup window should not be suppressed"))))

;; ============================================================
;; Run tests
;; ============================================================

(module+ main
  (require rackunit/text-ui)
  (exit (run-tests tool-start-dedup-tests)))
