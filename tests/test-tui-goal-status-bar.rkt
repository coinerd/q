#lang racket/base

;; BOUNDARY: unit
;; @suite tui
;; @speed fast
;; @boundary unit
;; @mutates none
;; tests/test-tui-goal-status-bar.rkt — TUI goal status-bar reducer contracts

(require rackunit
         racket/string
         "../util/event.rkt"
         "../util/cost-tracker.rkt"
         "../tui/state.rkt"
         "../tui/state-events.rkt"
         "../tui/render/status-line.rkt"
         "../tui/render/message-layout.rkt")

(define (evt type payload)
  (event 1 type 1000.0 "S" #f payload))

(define (status-text st)
  (styled-segment-text (car (styled-line-segments (render-status-bar st 100)))))

(test-case "goal.status appends transcript entry and sets status-message"
  (define st (initial-ui-state #:session-id "S" #:model-name "glm-5.1"))
  (define st1 (apply-event-to-state st (evt "goal.status" (hasheq 'message "Goal turn 1/8: working..."))))
  (check-equal? (ui-state-status-message st1) "Goal turn 1/8: working...")
  (check-true (string-contains? (transcript-entry-text (car (ui-state-transcript st1)))
                                "[goal] Goal turn 1/8: working..."))
  (check-true (string-contains? (status-text st1) "Goal turn 1/8")))

(test-case "goal.turn.started sets busy marker while preserving goal badge"
  (define st (initial-ui-state #:session-id "S" #:model-name "glm-5.1"))
  (define st1 (apply-event-to-state st (evt "goal.started" (hasheq 'goal-text "build website" 'max-turns 8))))
  (define st2 (apply-event-to-state st1 (evt "goal.turn.started" (hasheq 'turn-number 1))))
  (check-true (ui-state-busy? st2))
  (check-not-false (ui-state-active-goal st2))
  (check-true (string-contains? (status-text st2) "*"))
  (check-true (string-contains? (status-text st2) "goal 1/8")))

(test-case "stream.turn.completed clears busy for normal chat"
  (define st (set-busy (initial-ui-state #:session-id "S" #:model-name "glm-5.1") #t))
  (define st1 (apply-event-to-state st (evt "stream.turn.completed" (hasheq))))
  (check-false (ui-state-busy? st1))
  (check-false (ui-state-active-goal st1)))

(test-case "stream.turn.completed preserves active-goal visual working state"
  (define st (initial-ui-state #:session-id "S" #:model-name "glm-5.1"))
  (define st1 (apply-event-to-state st (evt "goal.started" (hasheq 'goal-text "build website" 'max-turns 8))))
  (define st2 (apply-event-to-state st1 (evt "goal.turn.started" (hasheq 'turn-number 1))))
  (define st3 (apply-event-to-state st2 (evt "stream.turn.completed" (hasheq))))
  (check-true (ui-state-busy? st3))
  (check-not-false (ui-state-active-goal st3))
  (check-true (string-contains? (or (ui-state-status-message st3) "") "evaluating"))
  (check-true (string-contains? (status-text st3) "*"))
  (check-true (string-contains? (status-text st3) "evaluating")))

(test-case "context.built accepts camelCase and kebab-case token keys"
  (define st (initial-ui-state #:session-id "S" #:model-name "glm-5.1"))
  (define st1 (apply-event-to-state st (evt "context.built" (hasheq 'tokenCount 5000))))
  (check-equal? (ui-state-context-tokens st1) 5000)
  (define st2 (apply-event-to-state st1 (evt "context.built" (hasheq 'token-count 6000))))
  (check-equal? (ui-state-context-tokens st2) 6000))

(test-case "model.stream.completed with missing or malformed usage does not throw"
  (define st (initial-ui-state #:session-id "S" #:model-name "glm-5.1"))
  (check-not-exn (lambda () (apply-event-to-state st (evt "model.stream.completed" (hasheq)))))
  (check-not-exn (lambda () (apply-event-to-state st (evt "model.stream.completed" (hasheq 'usage #f))))))

(test-case "model.stream.completed valid estimated usage updates cost tracker and renders cost"
  (define trk (make-cost-tracker "glm-5.1"))
  (define st (struct-copy ui-state
                          (initial-ui-state #:session-id "S" #:model-name "glm-5.1")
                          [cost-tracker trk]))
  (define st1 (apply-event-to-state st (evt "model.stream.completed"
                                            (hasheq 'usage (hasheq 'prompt_tokens 5000
                                                                   'completion_tokens 250
                                                                   'estimated? #t)))))
  (check-true (positive? (cost-tracker-input-tokens-total (ui-state-cost-tracker st1))))
  (check-true (string-contains? (status-text st1) "$")))
