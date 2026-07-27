#lang racket/base

;; @speed fast
;; @suite default
;; BOUNDARY: pure

;; tests/test-effect-types.rkt — Effect descriptor type tests (F1)

(require rackunit
         rackunit/text-ui
         "../agent/effect-types.rkt"
         "../agent/loop-fsm.rkt"
         (only-in "../util/fsm/fsm.rkt" fsm-state-name fsm-event-name))

(define effect-types-tests
  (test-suite "effect-types"

    (test-case "effect:none construction"
      (check-true (effect:none? (effect:none))))

    (test-case "effect:emit-event construction"
      (let ([e (effect:emit-event 'turn-start (hasheq 'test #t))])
        (check-equal? (effect:emit-event-type e) 'turn-start)
        (check-true (hash? (effect:emit-event-payload e)))))

    (test-case "effect:update-fsm construction with FSM structs"
      (let ([e (effect:update-fsm turn-state-emit-start turn-event-start)])
        (check-true (effect:update-fsm? e))
        (check-equal? (fsm-state-name (effect:update-fsm-from-state e)) 'emit-start)
        (check-equal? (fsm-event-name (effect:update-fsm-event e)) 'start)))

    (test-case "effect:dispatch-hook construction"
      (let ([e (effect:dispatch-hook 'agent-start (hasheq))])
        (check-equal? (effect:dispatch-hook-hook-point e) 'agent-start)))

    (test-case "effect predicates"
      (check-true (effect:none? (effect:none)))
      (check-true (effect:emit-event? (effect:emit-event 'test #f)))
      (check-true (effect:update-fsm? (effect:update-fsm turn-state-emit-start turn-event-start)))
      (check-true (effect:dispatch-hook? (effect:dispatch-hook 'test #f))))

    (test-case "effect? contract accepts all variants"
      (for ([e (list (effect:none)
                     (effect:emit-event 'x #f)
                     (effect:update-fsm turn-state-emit-start turn-event-start)
                     (effect:dispatch-hook 'c #f))])
        (check-true (effect? e))))

    ;; ── v0.99.70 W0: New effect descriptors ──

    (test-case "effect:build-result construction"
      (let ([e (effect:build-result 'state 'completed #f)])
        (check-true (effect:build-result? e))
        (check-true (effect? e))
        (check-true (effect-base? e))
        (check-equal? (effect:build-result-result-type e) 'completed)))

    (test-case "effect:cancel construction"
      (let ([e (effect:cancel "turn-1" "session-1" "user")])
        (check-true (effect:cancel? e))
        (check-true (effect? e))
        (check-equal? (effect:cancel-turn-id e) "turn-1")
        (check-equal? (effect:cancel-session-id e) "session-1")
        (check-equal? (effect:cancel-reason e) "user")))

    (test-case "effect:log construction"
      (let ([e (effect:log 'warning "test message" '(1 2 3))])
        (check-true (effect:log? e))
        (check-true (effect? e))
        (check-equal? (effect:log-level e) 'warning)
        (check-equal? (effect:log-message e) "test message")
        (check-equal? (effect:log-data e) '(1 2 3))))

    (test-case "effect:log with null data"
      (let ([e (effect:log 'debug "no extra" #f)])
        (check-true (effect:log? e))
        (check-false (effect:log-data e))))

    (test-case "effect:validate-messages construction"
      (let ([msgs (list (hasheq 'role "user" 'content "hi")
                        (hasheq 'role "system" 'content "hello"))])
        (let ([e (effect:validate-messages msgs)])
          (check-true (effect:validate-messages? e))
          (check-true (effect? e))
          (check-equal? (length (effect:validate-messages-messages e)) 2))))

    (test-case "effect:stream construction"
      (let ([mock-provider (list 'mock-provider)]
            [mock-req (list 'mock-request)])
        (let ([e (effect:stream mock-provider mock-req #f "s1" "t1" "state" '("msg") #f #f #f)])
          (check-true (effect:stream? e))
          (check-true (effect? e))
          (check-equal? (effect:stream-provider e) mock-provider)
          (check-equal? (effect:stream-request e) mock-req)
          (check-equal? (effect:stream-session-id e) "s1")
          (check-equal? (effect:stream-turn-id e) "t1"))))

    (test-case "all new effects inherit from effect-base"
      (check-true (effect-base? (effect:build-result 's 'completed #f)))
      (check-true (effect-base? (effect:cancel "t" "s" "u")))
      (check-true (effect-base? (effect:log 'info "m" #f)))
      (check-true (effect-base? (effect:validate-messages '())))
      (check-true (effect-base? (effect:stream 'p 'r #f "s" "t" "st" '() #f #f #f))))))

(run-tests effect-types-tests)
