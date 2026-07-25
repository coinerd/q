#lang racket/base

;; @speed fast
;; @suite default

(require rackunit
         "../util/capability.rkt")

(test-case "capability-authorized?: concrete requirements need matching or any grant"
  (check-true (capability-authorized? 'read-only '(read-only)))
  (check-true (capability-authorized? 'read-only '(any)))
  (check-false (capability-authorized? 'read-only '(shell-exec))))

(test-case "capability-authorized?: required any needs explicit any grant"
  (check-true (capability-authorized? 'any '(any)))
  (check-false (capability-authorized? 'any '(read-only)))
  (check-false (capability-authorized? 'any '())))

(test-case "capability-authorized?: malformed requirements and grants fail closed"
  (check-false (capability-authorized? 'bogus '(any)))
  (check-false (capability-authorized? "read-only" '(any)))
  (check-false (capability-authorized? 'read-only 'not-a-list))
  (check-false (capability-authorized? 'read-only '(read-only bogus)))
  (check-false (capability-authorized? 'read-only '("read-only"))))

(test-case "canonical capability snapshots are immutable, valid, and duplicate-free"
  (define source (list 'read-only 'shell-exec 'read-only))
  (define snapshot (canonical-capabilities-snapshot source))
  (check-equal? snapshot '(read-only shell-exec))
  (check-false (eq? snapshot source))
  ;; Racket pairs are immutable by construction; `immutable?` is not a
  ;; meaningful predicate for proper lists.
  (check-true (list? snapshot))
  (check-equal? (canonical-capabilities-snapshot '(read-only bogus)) '())
  (check-equal? (canonical-capabilities-snapshot 'malformed) '()))
