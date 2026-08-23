#lang racket

;; @speed fast
;; @suite integration
;; @boundary integration

(require rackunit
         rackunit/text-ui
         "../tui/state-types.rkt"
         "../tui/state-events.rkt"
         "../ui-core/conversation-artifact.rkt"
         "../ui-core/conversation-reducer.rkt"
         "../ui-core/feature-flags.rkt"
         "../util/cost-tracker.rkt"
         "../util/event/event.rkt")

(define (evt type session-id turn-id payload)
  (make-event type 100 session-id turn-id payload))

(define (apply-events state events)
  (for/fold ([current state]) ([event (in-list events)])
    (apply-event-to-state current event)))

(define (entries-of-kind state kind)
  (filter (lambda (entry) (eq? (transcript-entry-kind entry) kind)) (ui-state-transcript state)))

(define tests
  (test-suite "TUI reasoning artifact integration"

    (test-case "event envelope identity keeps equal turn ids in different sessions distinct"
      (define result-a
        (apply-events (initial-ui-state)
                      (list (evt "model.stream.thinking" "session-a" "turn-1" (hasheq 'delta "A"))
                            (evt "model.stream.completed" "session-a" "turn-1" (hasheq)))))
      (define result-b
        (apply-events (initial-ui-state)
                      (list (evt "model.stream.thinking" "session-b" "turn-1" (hasheq 'delta "B"))
                            (evt "model.stream.completed" "session-b" "turn-1" (hasheq)))))
      (define reducer-a (ui-state-conversation-reducer result-a))
      (define reducer-b (ui-state-conversation-reducer result-b))
      (define artifact-a (reducer-thinking-artifact reducer-a "session-a" "turn-1"))
      (define artifact-b (reducer-thinking-artifact reducer-b "session-b" "turn-1"))
      (check-equal? (conversation-artifact-body artifact-a) "A")
      (check-equal? (conversation-artifact-body artifact-b) "B")
      (check-not-equal? (conversation-artifact-id artifact-a) (conversation-artifact-id artifact-b))
      (check-not-equal? (make-artifact-id "a:b" "c" 'thinking) (make-artifact-id "a" "b:c" 'thinking))
      (check-true (reducer-turn-completed? reducer-a "session-a" "turn-1"))
      (check-true (reducer-turn-completed? reducer-b "session-b" "turn-1")))

    (test-case "missing canonical identity cannot create a collision artifact"
      (define result
        (apply-event-to-state (initial-ui-state)
                              (evt "model.stream.thinking" #f #f (hasheq 'delta "orphan"))))
      (check-equal? (hash-count (reducer-state-artifacts (ui-state-conversation-reducer result))) 0))

    (test-case "completion events are idempotent and thinking-only stays singular"
      (define events
        (list (evt "model.stream.thinking" "session-a" "turn-2" (hasheq 'delta "reason only"))
              (evt "assistant.message.completed" "session-a" "turn-2" (hasheq 'content ""))
              (evt "assistant.message.completed" "session-a" "turn-2" (hasheq 'content ""))
              (evt "model.stream.completed" "session-a" "turn-2" (hasheq))
              (evt "model.stream.completed" "session-a" "turn-2" (hasheq))))
      (define result (apply-events (initial-ui-state) events))
      (define thinking (entries-of-kind result 'thinking))
      (check-equal? (length thinking) 1)
      (check-equal? (transcript-entry-text (car thinking)) "reason only")
      (check-equal? (entries-of-kind result 'assistant) '())
      (define artifact (hash-ref (transcript-entry-meta (car thinking)) 'artifact))
      (check-true (conversation-artifact? artifact))
      (check-equal? (conversation-artifact-session-id artifact) "session-a")
      (check-equal? (conversation-artifact-turn-id artifact) "turn-2")
      (check-false (ui-state-streaming-thinking result))
      (check-false (ui-state-streaming-text result)))

    (test-case "usage is counted once per canonical completion and malformed values are zero"
      (define tracker (make-cost-tracker "test-model"))
      (define start (struct-copy ui-state (initial-ui-state) [cost-tracker tracker]))
      (define duplicate
        (evt "model.stream.completed"
             "session-usage"
             "turn-usage"
             (hasheq 'usage (hasheq 'prompt_tokens 21 'completion_tokens 8))))
      (define once (apply-event-to-state start duplicate))
      (define twice (apply-event-to-state once duplicate))
      (check-equal? (cost-tracker-input-tokens-total (ui-state-cost-tracker twice)) 21)
      (check-equal? (cost-tracker-output-tokens-total (ui-state-cost-tracker twice)) 8)
      (define malformed
        (apply-event-to-state twice
                              (evt "model.stream.completed"
                                   "session-usage"
                                   "turn-malformed"
                                   (hasheq 'usage
                                           (hasheq 'prompt_tokens -2 'completion_tokens "many")))))
      (check-equal? (cost-tracker-input-tokens-total (ui-state-cost-tracker malformed)) 21)
      (check-equal? (cost-tracker-output-tokens-total (ui-state-cost-tracker malformed)) 8))

    (test-case "empty canonical identity cannot activate transient streaming state"
      (define result
        (apply-events (initial-ui-state)
                      (list (evt "turn.started" "" "" (hasheq))
                            (evt "model.request.started" "" "" (hasheq))
                            (evt "model.stream.delta" "" "" (hasheq 'delta "orphan"))
                            (evt "model.stream.thinking" "" "" (hasheq 'delta "reason"))
                            (evt "turn.completed" "" "" (hasheq)))))
      (check-false (ui-state-busy? result))
      (check-false (ui-state-streaming-text result))
      (check-false (ui-state-streaming-thinking result)))

    (test-case "empty canonical identity never accounts duplicate usage"
      (define tracker (make-cost-tracker "test-model"))
      (define start (struct-copy ui-state (initial-ui-state) [cost-tracker tracker]))
      (define orphan
        (evt "model.stream.completed"
             ""
             ""
             (hasheq 'usage (hasheq 'prompt_tokens 21 'completion_tokens 8))))
      (define result (apply-events start (list orphan orphan)))
      (check-equal? (cost-tracker-input-tokens-total (ui-state-cost-tracker result)) 0)
      (check-equal? (cost-tracker-output-tokens-total (ui-state-cost-tracker result)) 0))

    (test-case "cross-session deltas cannot contaminate the active stream"
      (define started
        (apply-events
         (initial-ui-state #:session-id "session-a")
         (list (evt "turn.started" "session-a" "active-turn" (hasheq 'scope "prompt"))
               (evt "model.stream.delta" "session-a" "active-turn" (hasheq 'delta "owned")))))
      (define after-foreign
        (apply-events
         started
         (list
          (evt "model.stream.delta" "session-b" "foreign-turn" (hasheq 'delta "-foreign"))
          (evt "model.stream.thinking" "session-b" "foreign-turn" (hasheq 'delta "foreign reason")))))
      (check-equal? (ui-state-streaming-text after-foreign) "owned")
      (check-false (ui-state-streaming-thinking after-foreign))
      (check-equal? (ui-state-session-id after-foreign) "session-a"))

    (test-case "stale terminals retain the unrelated active stream and status"
      (define started
        (apply-events
         (initial-ui-state #:session-id "session-a")
         (list
          (evt "turn.started" "session-a" "active-turn" (hasheq 'scope "prompt"))
          (evt "model.stream.thinking" "session-a" "active-turn" (hasheq 'delta "still working")))))
      (define stale-model
        (apply-event-to-state started
                              (evt "model.stream.completed" "session-a" "stale-turn" (hasheq))))
      (define stale-assistant
        (apply-event-to-state
         stale-model
         (evt "assistant.message.completed" "session-a" "stale-turn" (hasheq 'content "old"))))
      (check-true (ui-state-busy? stale-assistant))
      (check-equal? (ui-state-active-turn-id stale-assistant) "active-turn")
      (check-equal? (ui-state-streaming-thinking stale-assistant) "still working"))

    (test-case "reasoning is UTF-8 byte bounded at the persistence boundary"
      (parameterize ([ui-reasoning-artifacts-max-bytes 17])
        (define result
          (apply-events (initial-ui-state)
                        (list (evt "model.stream.thinking"
                                   "session-a"
                                   "turn-large"
                                   (hasheq 'delta (make-string 20 #\λ)))
                              (evt "model.stream.completed" "session-a" "turn-large" (hasheq)))))
        (define art
          (reducer-thinking-artifact (ui-state-conversation-reducer result) "session-a" "turn-large"))
        (check-true (<= (bytes-length (string->bytes/utf-8 (conversation-artifact-body art))) 17))))

    (test-case "live reducer and transcript retention are capped at scrollback size"
      (define result
        (for/fold ([state (initial-ui-state)]) ([i (in-range 510)])
          (apply-event-to-state state
                                (evt "assistant.message.completed"
                                     "session-a"
                                     (format "turn-~a" i)
                                     (hasheq 'content (format "answer-~a" i))))))
      (check-equal? (length (ui-state-transcript result)) 500)
      (check-equal? (hash-count (reducer-state-artifacts (ui-state-conversation-reducer result)))
                    500))))

(module+ test
  (run-tests tests))
