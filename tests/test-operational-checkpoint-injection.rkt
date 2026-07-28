#lang racket/base

;; @speed fast
;; @suite default
;; @boundary integration

;; test-operational-checkpoint-injection.rkt — W9: Checkpoint injection integration test
;;
;; Tests that the operational checkpoint is injected into assembled messages
;; via the turn-context assembly pipeline.

(require rackunit
         rackunit/text-ui
         racket/string
         "../runtime/context-assembly/operational-checkpoint.rkt"
         "../runtime/context-assembly/turn-context.rkt"
         "../runtime/working-set.rkt"
         "../util/message/message.rkt"
         "../util/event/event-bus.rkt")

(define injection-tests
  (test-suite "Operational Checkpoint Injection (W9)"

    (test-case "checkpoint parameter stores and retrieves"
      (current-operational-checkpoint (make-empty-checkpoint))
      (define cp (make-empty-checkpoint))
      (define cp1 (checkpoint-set-repo-root cp "/test-area/q"))
      (define cp2 (checkpoint-set-planning-root cp1 "/test-area/.planning"))
      (define cp3 (checkpoint-set-milestone cp2 "v0.99.73"))
      (define cp4 (checkpoint-set-wave cp3 "W9"))
      (current-operational-checkpoint cp4)
      (check-equal? (operational-checkpoint-repo-root (current-operational-checkpoint))
                    "/test-area/q")
      (check-equal? (operational-checkpoint-active-milestone (current-operational-checkpoint))
                    "v0.99.73"))

    (test-case "inject-checkpoint-message prepends when checkpoint is valid"
      (define cp (make-empty-checkpoint))
      (define cp1 (checkpoint-set-repo-root cp "/test-area/q"))
      (define messages
        (list (hash 'role "user" 'content "hello") (hash 'role "assistant" 'content "world")))
      (define injected (inject-checkpoint-message cp1 messages))
      (check-equal? (length injected) 3)
      (check-equal? (hash-ref (car injected) 'role) "system")
      (check-equal? (hash-ref (car injected) 'kind) "checkpoint")
      (define content-parts (hash-ref (car injected) 'content))
      (check-true (list? content-parts))
      (check-true (regexp-match? #px"/test-area/q" (format "~a" content-parts))))

    (test-case "inject-checkpoint-message returns unchanged for empty checkpoint"
      (define messages (list (hash 'role "user" 'content "hello")))
      (define injected (inject-checkpoint-message (make-empty-checkpoint) messages))
      (check-equal? (length injected) 1)
      (check-equal? (hash-ref (car injected) 'role) "user"))

    (test-case "checkpoint supersession detection"
      (check-true (supercedes-generic-planning? "STATE-v0.99.73.md"))
      (check-true (supercedes-generic-planning? "PLAN-v0.99.73-ZERO-FAILING-TESTS.md"))
      (check-false (supercedes-generic-planning? "STATE.md"))
      (check-false (supercedes-generic-planning? "VALIDATION.md"))
      (check-true (contradicts-generic-planning? "STATE.md" "STATE-v0.99.73-ZERO-FAILING-TESTS.md"))
      (check-false (contradicts-generic-planning? "STATE-v0.99.73.md" "STATE.md")))))

(module+ test
  (require rackunit/text-ui)
  (run-tests injection-tests))

(module+ main
  (require rackunit/text-ui)
  (run-tests injection-tests))
