#lang racket

;; @speed slow  ;; @suite runtime

;; BOUNDARY: integration

;; tests/test-turn-reducer-integration.rkt -- Turn-reducer integration tests (v0.43.0)
;;
;; Verifies that the reducer-driven dispatch in loop.rkt produces correct
;; decisions at all 6 branch points of the turn lifecycle.

(require rackunit
         rackunit/text-ui
         "../agent/turn-reducer.rkt"
         "../agent/turn-model.rkt"
         (only-in "../util/hook-types.rkt" hook-result? hook-block hook-pass))

(define integration-suite
  (test-suite "turn-reducer integration with loop"

    ;; S1: Validate reducer alignment with hook results

    (test-case "reducer blocks on hook-block result"
      (define block-result (hook-block "test-reason"))
      (define d-pre (decide-after-pre-hook block-result))
      (check-equal? (turn-decision-tag d-pre) 'blocked)
      (define d-msg (decide-after-msg-hook block-result))
      (check-equal? (turn-decision-tag d-msg) 'blocked))

    (test-case "reducer passes on hook-pass result"
      (define pass-result (hook-pass 'some-data))
      (define d-pre (decide-after-pre-hook pass-result))
      (check-equal? (turn-decision-tag d-pre) 'check-msg-hook)
      (define d-msg (decide-after-msg-hook pass-result))
      (check-equal? (turn-decision-tag d-msg) 'begin-stream))

    (test-case "reducer passes on #f (no hook)"
      (define d-pre (decide-after-pre-hook #f))
      (check-equal? (turn-decision-tag d-pre) 'check-msg-hook)
      (define d-msg (decide-after-msg-hook #f))
      (check-equal? (turn-decision-tag d-msg) 'begin-stream))

    ;; Full lifecycle via decide-turn-step
    (test-case "full turn lifecycle via decide-turn-step: happy path"
      (define ctx (turn-context "s1" 0 '() '() (hasheq) (hasheq) #f))
      ;; Phase 1: start
      (define d1 (decide-turn-step (make-turn-start ctx)))
      (check-equal? (turn-decision-tag d1) 'build-context)
      ;; Phase 2: context (always continues)
      (define d2 (decide-after-context ctx))
      (check-equal? (turn-decision-tag d2) 'check-pre-hook)
      ;; Phase 3: pre-hook pass
      (define d3 (decide-turn-step (make-turn-hook-result
                                     (hook-stage-payload 'pre (hook-pass)))))
      (check-equal? (turn-decision-tag d3) 'check-msg-hook)
      ;; Phase 4: msg-hook pass
      (define d4 (decide-turn-step (make-turn-hook-result
                                     (hook-stage-payload 'msg (hook-pass)))))
      (check-equal? (turn-decision-tag d4) 'begin-stream)
      ;; Phase 5: stream complete
      (define sc (make-stream-completion
                  #:cancelled? #f
                  #:text "hello"
                  #:tool-calls '()))
      (define d5 (decide-turn-step (make-turn-stream-complete sc)))
      (check-equal? (turn-decision-tag d5) 'complete))

    (test-case "full turn lifecycle: pre-hook blocked"
      (define ctx (turn-context "s1" 0 '() '() (hasheq) (hasheq) #f))
      (define d1 (decide-turn-step (make-turn-start ctx)))
      (check-equal? (turn-decision-tag d1) 'build-context)
      ;; Pre-hook blocks
      (define d3 (decide-turn-step (make-turn-hook-result
                                     (hook-stage-payload 'pre (hook-block "denied")))))
      (check-equal? (turn-decision-tag d3) 'blocked))

    (test-case "full turn lifecycle: msg-hook blocked"
      ;; Pre-hook passes
      (define d3 (decide-turn-step
                  (make-turn-hook-result
                   (hook-stage-payload 'pre (hook-pass)))))
      (check-equal? (turn-decision-tag d3) 'check-msg-hook)
      ;; Msg-hook blocks
      (define d4 (decide-turn-step
                  (make-turn-hook-result
                   (hook-stage-payload 'msg (hook-block "nope")))))
      (check-equal? (turn-decision-tag d4) 'blocked))

    (test-case "full turn lifecycle: stream cancelled"
      (define sc (make-stream-completion
                  #:cancelled? #t
                  #:cancel-reason "user"
                  #:text ""
                  #:tool-calls '()))
      (define d5 (decide-turn-step (make-turn-stream-complete sc)))
      (check-equal? (turn-decision-tag d5) 'cancelled))

    (test-case "decide-turn-step handles cancel command"
      (define d (decide-turn-step (make-turn-cancel "timeout")))
      (check-equal? (turn-decision-tag d) 'cancelled))))

(run-tests integration-suite)
