#lang racket

;; @speed fast
;; @suite default

;; q/tests/test-gui-event-coverage.rkt — W3: thinking, retry, compaction, exploration
;;
;; Tests that new event subscriptions produce typed gui-messages
;; with correct kinds and text formatting.

(require rackunit
         rackunit/text-ui
         racket/list
         "../gui/state-sync.rkt"
         "../gui/gui-types.rkt"
         "../gui/components/rich-transcript-view.rkt"
         "../ui-core/theme-protocol.rkt"
         "../util/event/event.rkt")

(define (mk-event tag payload)
  (make-event tag (current-inexact-milliseconds) #f #f payload))

(define (fresh-box)
  (box (make-gui-state)))

(define-test-suite
 test-gui-event-coverage

 ;; ─── Thinking flush on stream completed ───

 (test-case "thinking accumulated then flushed as kind=thinking on stream completed"
   (define sb (fresh-box))
   (define sub (make-gui-event-subscriber sb))
   (sub (mk-event "model.stream.thinking" (hash 'delta "Let me think...")))
   (sub (mk-event "model.stream.thinking" (hash 'delta " about this problem.")))
   (sub (mk-event "model.stream.completed" (hash)))
   (define msgs (gui-state-messages (unbox sb)))
   ;; Should have one thinking entry
   (define thinking-msgs (filter (lambda (m) (eq? (gui-message-kind m) 'thinking)) msgs))
   (check-equal? (length thinking-msgs) 1)
   (check-true (string-contains? (gui-message-text (car thinking-msgs)) "Let me think...")))

 (test-case "long thinking text truncated to 200 chars"
   (define sb (fresh-box))
   (define sub (make-gui-event-subscriber sb))
   (define long-thinking (make-string 300 #\A))
   (sub (mk-event "model.stream.thinking" (hash 'delta long-thinking)))
   (sub (mk-event "model.stream.completed" (hash)))
   (define msgs (gui-state-messages (unbox sb)))
   (define thinking-msgs (filter (lambda (m) (eq? (gui-message-kind m) 'thinking)) msgs))
   (check-equal? (length thinking-msgs) 1)
   (check-true (<= (string-length (gui-message-text (car thinking-msgs))) 200)))

 (test-case "no thinking entry when no thinking delta"
   (define sb (fresh-box))
   (define sub (make-gui-event-subscriber sb))
   (sub (mk-event "model.stream.delta" (hash 'delta "Hello")))
   (sub (mk-event "model.stream.completed" (hash)))
   (define msgs (gui-state-messages (unbox sb)))
   (define thinking-msgs (filter (lambda (m) (eq? (gui-message-kind m) 'thinking)) msgs))
   (check-equal? (length thinking-msgs) 0))

 ;; ─── Compaction events ───

 (test-case "compaction.warning produces system entry"
   (define sb (fresh-box))
   (define sub (make-gui-event-subscriber sb))
   (sub (mk-event "compaction.warning" (hash 'tokenCount 50000)))
   (define msgs (gui-state-messages (unbox sb)))
   (check-equal? (length msgs) 1)
   (check-equal? (gui-message-kind (car msgs)) 'system)
   (check-true (string-contains? (gui-message-text (car msgs)) "50000 tokens")))

 (test-case "compaction.started produces system entry"
   (define sb (fresh-box))
   (define sub (make-gui-event-subscriber sb))
   (sub (mk-event "compaction.started" (hash)))
   (define msgs (gui-state-messages (unbox sb)))
   (check-equal? (length msgs) 1)
   (check-equal? (gui-message-kind (car msgs)) 'system)
   (check-true (string-contains? (gui-message-text (car msgs)) "compacting")))

 (test-case "compaction.completed produces system entry"
   (define sb (fresh-box))
   (define sub (make-gui-event-subscriber sb))
   (sub (mk-event "compaction.completed" (hash 'reduction "50% smaller")))
   (define msgs (gui-state-messages (unbox sb)))
   (check-equal? (length msgs) 1)
   (check-true (string-contains? (gui-message-text (car msgs)) "50% smaller")))

 ;; ─── Retry events ───

 (test-case "auto-retry.start produces system entry with attempt count"
   (define sb (fresh-box))
   (define sub (make-gui-event-subscriber sb))
   (sub (mk-event "auto-retry.start" (hash 'reason "rate limited" 'attempt 2 'maxAttempts 3)))
   (define msgs (gui-state-messages (unbox sb)))
   (check-equal? (length msgs) 1)
   (check-equal? (gui-message-kind (car msgs)) 'system)
   (check-true (string-contains? (gui-message-text (car msgs)) "rate limited"))
   (check-true (string-contains? (gui-message-text (car msgs)) "2/3")))

 ;; ─── Iteration events ───

 (test-case "iteration.soft-warning produces system entry"
   (define sb (fresh-box))
   (define sub (make-gui-event-subscriber sb))
   (sub (mk-event "iteration.soft-warning" (hash 'iteration 5 'remaining 3)))
   (define msgs (gui-state-messages (unbox sb)))
   (check-equal? (length msgs) 1)
   (check-true (string-contains? (gui-message-text (car msgs)) "iteration 5"))
   (check-true (string-contains? (gui-message-text (car msgs)) "3 remaining")))

 ;; ─── Context pressure ───

 (test-case "context.pressure updates context-info in gui-state"
   (define sb (fresh-box))
   (define sub (make-gui-event-subscriber sb))
   (sub (mk-event "context.pressure" (hash 'level "medium" 'usagePercent 72)))
   (define state (unbox sb))
   (define info (gui-state-context-info state))
   (check-not-false info)
   (check-equal? (hash-ref info 'level) 'medium)
   (check-equal? (hash-ref info 'percent) 72))

 (test-case "context.mid-turn-over-budget produces system entry"
   (define sb (fresh-box))
   (define sub (make-gui-event-subscriber sb))
   (sub (mk-event "context.mid-turn-over-budget" (hash 'tokensUsed 8000 'tokenBudget 6000)))
   (define msgs (gui-state-messages (unbox sb)))
   (check-equal? (length msgs) 1)
   (check-true (string-contains? (gui-message-text (car msgs)) "8000/6000 tokens")))

 ;; ─── Session events ───

 (test-case "session.started produces system entry"
   (define sb (fresh-box))
   (define sub (make-gui-event-subscriber sb))
   (sub (mk-event "session.started" (hash)))
   (define msgs (gui-state-messages (unbox sb)))
   (check-equal? (length msgs) 1)
   (check-true (string-contains? (gui-message-text (car msgs)) "session started")))

 (test-case "session.forked produces system entry with id"
   (define sb (fresh-box))
   (define sub (make-gui-event-subscriber sb))
   (sub (mk-event "session.forked" (hash 'sessionId "abc-123")))
   (define msgs (gui-state-messages (unbox sb)))
   (check-equal? (length msgs) 1)
   (check-true (string-contains? (gui-message-text (car msgs)) "abc-123")))

 ;; ─── Goal events ───

 (test-case "goal.started sets active-goal"
   (define sb (fresh-box))
   (define sub (make-gui-event-subscriber sb))
   (sub (mk-event "goal.started" (hash 'description "Fix the bug")))
   (check-equal? (gui-state-active-goal (unbox sb)) "Fix the bug"))

 (test-case "goal.achieved clears active-goal"
   (define sb (fresh-box))
   (define sub (make-gui-event-subscriber sb))
   (sub (mk-event "goal.started" (hash 'description "Fix the bug")))
   (sub (mk-event "goal.achieved" (hash)))
   (check-false (gui-state-active-goal (unbox sb))))

 (test-case "goal.failed clears active-goal"
   (define sb (fresh-box))
   (define sub (make-gui-event-subscriber sb))
   (sub (mk-event "goal.started" (hash 'description "Fix the bug")))
   (sub (mk-event "goal.failed" (hash)))
   (check-false (gui-state-active-goal (unbox sb))))

 (test-case "goal.started with empty description does not set active-goal"
   (define sb (fresh-box))
   (define sub (make-gui-event-subscriber sb))
   (sub (mk-event "goal.started" (hash 'description "")))
   (check-false (gui-state-active-goal (unbox sb))))

 ;; ─── Tool blocked ───

 (test-case "tool.call.blocked produces system entry"
   (define sb (fresh-box))
   (define sub (make-gui-event-subscriber sb))
   (sub (mk-event "tool.call.blocked" (hash 'name "bash" 'reason "requires permission")))
   (define msgs (gui-state-messages (unbox sb)))
   (check-equal? (length msgs) 1)
   (check-equal? (gui-message-kind (car msgs)) 'system)
   (check-true (string-contains? (gui-message-text (car msgs)) "bash"))
   (check-true (string-contains? (gui-message-text (car msgs)) "permission")))

 ;; ─── Kind->color for thinking ───

 (test-case "kind->color returns muted for thinking"
   (define theme (default-theme))
   (check-equal? (kind->color 'thinking theme) (theme-ref theme 'muted))))

(run-tests test-gui-event-coverage)
