#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; tests/test-context-assembly-serialization.rkt — tests for context-assembly/serialization

(require rackunit
         rackunit/text-ui
         "../runtime/context-assembly/serialization.rkt")

(define serialization-tests
  (test-suite "Context Assembly Serialization"

    (test-case "tiered-context construction"
      (define tc (tiered-context '(a) '(b) '(c)))
      (check-true (tiered-context? tc))
      (check-equal? (tiered-context-tier-a tc) '(a))
      (check-equal? (tiered-context-tier-b tc) '(b))
      (check-equal? (tiered-context-tier-c tc) '(c)))

    (test-case "tiered-context->message-list concatenates"
      (define tc (tiered-context '(1) '(2) '(3)))
      (check-equal? (tiered-context->message-list tc) '(1 2 3)))

    (test-case "payload->tiered-context round-trip"
      (define payload (context-assembly-payload '(a) '(b) '(c) 8192 (hasheq)))
      (define tc (payload->tiered-context payload))
      (check-equal? (tiered-context-tier-a tc) '(a))
      (check-equal? (tiered-context-tier-b tc) '(b))
      (check-equal? (tiered-context-tier-c tc) '(c)))

    (test-case "build-tiered-context: empty messages"
      (define tc (build-tiered-context '()))
      (check-equal? (tiered-context-tier-a tc) '())
      (check-equal? (tiered-context-tier-b tc) '())
      (check-equal? (tiered-context-tier-c tc) '()))))

(module+ main
  (run-tests serialization-tests))
