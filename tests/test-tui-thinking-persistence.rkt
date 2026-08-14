#lang racket/base

;; @speed fast  ;; @suite tui

;; BOUNDARY: io

;; test-tui-thinking-persistence.rkt — TDD tests for v0.28.21 W0
;; Tests that thinking text is persisted as a permanent transcript entry
;; when the turn completes with empty content (tool-call turns).

(require rackunit
         rackunit/text-ui
         "../tui/state-types.rkt"
         "../tui/state-events.rkt"
         "../util/message/protocol-types.rkt")

;; Helpers to build events
(define (make-test-event ev payload [ts 0])
  (make-event ev ts "test-session" "test-turn" payload))

(define (apply-events state events)
  (for/fold ([s state]) ([evt (in-list events)])
    (apply-event-to-state s evt)))

(define (find-entries state kind)
  (filter (lambda (e) (eq? (transcript-entry-kind e) kind)) (ui-state-transcript state)))

(define thinking-persistence-suite
  (test-suite "TUI thinking persistence"

    ;; T1: Thinking persisted on assistant.message.completed with empty content
    (test-case "thinking persisted when content is empty"
      (define state (initial-ui-state))
      (define events
        (list (make-test-event "turn.started" (hasheq))
              (make-test-event "model.stream.thinking" (hasheq 'delta "I need to check the files..."))
              (make-test-event "assistant.message.completed" (hasheq 'content ""))))
      (define result (apply-events state events))
      (define thinking-entries (find-entries result 'thinking))
      (check-equal? (length thinking-entries) 1)
      (check-equal? (transcript-entry-text (car thinking-entries)) "I need to check the files...")
      (check-false (ui-state-streaming-thinking result)))

    ;; T2 (BUG-0003): thinking IS persisted when assistant has content,
    ;; as a completed entry ordered before the assistant entry.
    (test-case "thinking persisted when assistant has content"
      (define state (initial-ui-state))
      (define events
        (list (make-test-event "turn.started" (hasheq))
              (make-test-event "model.stream.thinking" (hasheq 'delta "thinking about this..."))
              (make-test-event "model.stream.delta" (hasheq 'delta "response text"))
              (make-test-event "assistant.message.completed" (hasheq 'content "response text"))))
      (define result (apply-events state events))
      (define thinking-entries (find-entries result 'thinking))
      (check-equal? (length thinking-entries) 1)
      (check-equal? (transcript-entry-text (car thinking-entries)) "thinking about this...")
      (define assistant-entries (find-entries result 'assistant))
      (check-equal? (length assistant-entries) 1)
      ;; order: transcript list is newest-first, so the chronologically
      ;; earlier thinking entry sits immediately after the assistant entry.
      (define kinds (map transcript-entry-kind (ui-state-transcript result)))
      (check-equal? kinds '(assistant thinking) "thinking precedes assistant chronologically")
      (check-false (ui-state-streaming-thinking result)))

    ;; T3: Thinking NOT persisted when no thinking occurred
    (test-case "thinking NOT persisted when no thinking occurred"
      (define state (initial-ui-state))
      (define events
        (list (make-test-event "turn.started" (hasheq))
              (make-test-event "model.stream.delta" (hasheq 'delta "hello"))
              (make-test-event "assistant.message.completed" (hasheq 'content "hello"))))
      (define result (apply-events state events))
      (define thinking-entries (find-entries result 'thinking))
      (check-equal? (length thinking-entries) 0))

    ;; T4: Thinking persisted preserves full accumulated text
    (test-case "thinking preserves full accumulated text"
      (define state (initial-ui-state))
      (define events
        (list (make-test-event "turn.started" (hasheq))
              (make-test-event "model.stream.thinking" (hasheq 'delta "abc"))
              (make-test-event "model.stream.thinking" (hasheq 'delta " def"))
              (make-test-event "assistant.message.completed" (hasheq 'content ""))))
      (define result (apply-events state events))
      (define thinking-entries (find-entries result 'thinking))
      (check-equal? (length thinking-entries) 1)
      (check-equal? (transcript-entry-text (car thinking-entries)) "abc def"))

    ;; T5: Thinking NOT persisted when whitespace-only
    (test-case "thinking NOT persisted when whitespace-only"
      (define state (initial-ui-state))
      (define events
        (list (make-test-event "turn.started" (hasheq))
              (make-test-event "model.stream.thinking" (hasheq 'delta "  "))
              (make-test-event "assistant.message.completed" (hasheq 'content ""))))
      (define result (apply-events state events))
      (define thinking-entries (find-entries result 'thinking))
      (check-equal? (length thinking-entries) 0))

    ;; T6: Streaming thinking and text both cleared after completion
    (test-case "streaming state cleared after completion with thinking persisted"
      (define state (initial-ui-state))
      (define events
        (list (make-test-event "turn.started" (hasheq))
              (make-test-event "model.stream.thinking" (hasheq 'delta "hmm"))
              (make-test-event "assistant.message.completed" (hasheq 'content ""))))
      (define result (apply-events state events))
      (check-false (ui-state-streaming-thinking result))
      (check-false (ui-state-streaming-text result))
      (check-false (ui-state-busy? result)))))

(run-tests thinking-persistence-suite)
