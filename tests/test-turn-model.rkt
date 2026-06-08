#lang racket

;; @speed fast  ;; @suite runtime
;; BOUNDARY: pure

;; BOUNDARY: unit

;; tests/test-turn-model.rkt — Turn-model struct tests (R1-P1)

(require rackunit
         rackunit/text-ui
         "../agent/turn-model.rkt")

(define turn-model-suite
  (test-suite "turn-model structs"

    (test-case "turn-context construction"
      (define tc (turn-context "sid" 0 '(msg) '() (hasheq) (hasheq) #f))
      (check-equal? (turn-context-session-id tc) "sid")
      (check-equal? (turn-context-iteration tc) 0))

    (test-case "turn-command discriminated union"
      (define cmd-start (make-turn-start (hasheq 'user "hi")))
      (check-true (turn-start? cmd-start))
      (check-false (turn-cancel? cmd-start))
      (define cmd-cancel (make-turn-cancel (hasheq 'reason "abort")))
      (check-true (turn-cancel? cmd-cancel)))

    (test-case "turn-decision blocked/complete"
      (define blocked (make-decision-blocked "hook"))
      (check-true (decision-blocked? blocked))
      (check-false (decision-complete? blocked))
      (define complete (make-decision-complete (hasheq 'result "ok")))
      (check-true (decision-complete? complete))
      (check-false (decision-blocked? complete)))

    (test-case "decision-emit-start constructor and predicate"
      (define d (make-decision-emit-start (hasheq 'session-id "s1")))
      (check-true (decision-emit-start? d))
      (check-false (decision-blocked? d))
      (check-equal? (turn-decision-tag d) 'emit-start))

    (test-case "decision-build-context constructor and predicate"
      (define d (make-decision-build-context (hasheq 'messages '())))
      (check-true (decision-build-context? d))
      (check-false (decision-emit-start? d))
      (check-equal? (turn-decision-tag d) 'build-context))

    (test-case "decision-check-pre-hook and decision-check-msg-hook"
      (define d1 (make-decision-check-pre-hook (hasheq)))
      (check-true (decision-check-pre-hook? d1))
      (define d2 (make-decision-check-msg-hook (hasheq)))
      (check-true (decision-check-msg-hook? d2))
      (check-false (decision-check-pre-hook? d2)))

    (test-case "decision-begin-stream constructor and predicate"
      (define d (make-decision-begin-stream (hasheq 'model "test")))
      (check-true (decision-begin-stream? d))
      (check-false (decision-cancelled? d))
      (check-equal? (turn-decision-tag d) 'begin-stream))

    (test-case "decision-cancelled constructor and predicate"
      (define d (make-decision-cancelled "user-abort"))
      (check-true (decision-cancelled? d))
      (check-false (decision-complete? d))
      (check-equal? (turn-decision-tag d) 'cancelled))

    (test-case "turn-command hook-result and stream-complete constructors"
      (define hr (make-turn-hook-result (hasheq 'stage 'pre)))
      (check-true (turn-hook-result? hr))
      (check-false (turn-start? hr))
      (define sc (make-turn-stream-complete (hasheq 'text "done")))
      (check-true (turn-stream-complete? sc))
      (check-false (turn-cancel? sc)))

    (test-case "stream-completion construction"
      (define sc
        (make-stream-completion #:cancelled? #t
                                #:cancel-reason "timeout"
                                #:text "hello"
                                #:tool-calls '()
                                #:usage (hasheq 'tokens 10)
                                #:model "gpt-4"))
      (check-true (stream-completion? sc))
      (check-true (stream-completion-cancelled? sc))
      (check-equal? (stream-completion-cancel-reason sc) "timeout")
      (check-equal? (stream-completion-text sc) "hello")
      (check-equal? (stream-completion-model sc) "gpt-4"))

    (test-case "hook-stage-payload construction"
      (define hsp (hook-stage-payload 'pre (hasheq 'result "ok")))
      (check-true (hook-stage-payload? hsp))
      (check-equal? (hook-stage-payload-stage hsp) 'pre)
      (check-equal? (hash-ref (hook-stage-payload-result hsp) 'result) "ok"))))

(run-tests turn-model-suite 'verbose)
