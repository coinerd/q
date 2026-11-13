#lang racket
;; @speed fast
;; @suite testing
;; @boundary integration
;; @covers scripts/run-tests/inventory.rkt

(require rackunit
         (only-in "../scripts/run-tests/inventory.rkt" gate-membership))

(module+ test
  (test-case "gate names are looked up by string content"
    (define membership (gate-membership))
    (for ([name '("fast" "platform" "security" "workflows" "unit-fast" "slow/L4")])
      (check-true (hash-has-key? membership (string-copy name)) name)
      (check-equal? (hash-ref membership (string-copy name)) (hash-ref membership name)))))
