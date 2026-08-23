;; @suite unit
;; @speed fast
;; @boundary unit
;; Fixture: ordinary fully-tagged test file (discovery parity fixture, W0 of
;; docs/planning/PLAN-v1.00.11-TDD-CI-INTEGRITY-BASELINES.md).
;; Contract: MUST be discovered by collect-test-files in every invocation
;; mode (repo-root direct and clean-copy temp root).
(module+ test
  (require rackunit)
  (check-true #t))
