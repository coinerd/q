#lang racket/base

;; @speed fast
;; @suite default
;; @boundary unit

;; test-operational-checkpoint.rkt — W9: Operational checkpoint and supersession tests

(require rackunit
         rackunit/text-ui
         racket/string
         "../runtime/context-assembly/operational-checkpoint.rkt")

(define op-checkpoint-tests
  (test-suite "Operational Checkpoint (W9 R1/R2)"

    ;; ============================================================
    ;; R1: Operational checkpoint
    ;; ============================================================

    (test-case "empty checkpoint has empty roots"
      (define cp (make-empty-checkpoint))
      (check-equal? (operational-checkpoint-repo-root cp) "")
      (check-equal? (operational-checkpoint-planning-root cp) "")
      (check-false (operational-checkpoint-active-milestone cp))
      (check-false (operational-checkpoint-active-wave cp))
      (check-equal? (checkpoint->text cp) ""))

    (test-case "checkpoint->text produces compact output"
      (define cp (make-empty-checkpoint))
      (define cp2 (checkpoint-set-repo-root cp "/fake/repo/q"))
      (define cp3 (checkpoint-set-planning-root cp2 "/fake/repo/.planning"))
      (define cp4 (checkpoint-set-milestone cp3 "v0.99.73"))
      (define cp5 (checkpoint-set-wave cp4 "W9"))
      (define cp6 (checkpoint-set-dirty-files cp5 '("README.md")))
      (define text (checkpoint->text cp6))
      (check-true (string-contains? text "/fake/repo/q") "contains repo root")
      (check-true (string-contains? text "/fake/repo/.planning") "contains planning root")
      (check-true (string-contains? text "v0.99.73") "contains milestone")
      (check-true (string-contains? text "W9") "contains wave")
      ;; Should be compact (~200-400 chars)
      (check-true (< (string-length text) 1000) "checkpoint text is compact")
      (check-false (string-contains? text "last_error") "no error in fresh checkpoint"))

    (test-case "checkpoint error tracking"
      (define cp (make-empty-checkpoint))
      (define cp1 (checkpoint-set-error cp "not-a-git-repo"))
      (check-equal? (operational-checkpoint-last-error cp1) "not-a-git-repo")
      (check-equal? (operational-checkpoint-error-count cp1) 1)
      (define cp2 (checkpoint-set-error cp1 "file-not-found"))
      (check-equal? (operational-checkpoint-last-error cp2) "file-not-found")
      (check-equal? (operational-checkpoint-error-count cp2) 2)
      (define cp3 (checkpoint-set-repo-root cp2 "/fixed/path"))
      (check-false (operational-checkpoint-last-error cp3) "repo-root reset clears error")
      (check-equal? (operational-checkpoint-error-count cp3) 0))

    (test-case "checkpoint estimated tokens"
      (define cp (make-empty-checkpoint))
      (define cp1 (checkpoint-set-repo-root cp "/a/b"))
      (check-true (>= (checkpoint-estimated-tokens cp1) 0))
      (check-true (< (checkpoint-estimated-tokens cp1) 100) "estimated tokens < 100"))

    (test-case "checkpoint rendering enforces the frozen 512-token bound"
      (define huge-root (string-append "/" (make-string 10000 #\x)))
      (define cp (checkpoint-set-repo-root (make-empty-checkpoint) huge-root))
      (check-true (<= (checkpoint-estimated-tokens cp) 512))
      (check-true (<= (string-length (checkpoint->text cp)) 2048)))

    ;; ============================================================
    ;; R2: Supersession
    ;; ============================================================

    (test-case "named planning detection includes anchored VALIDATION names"
      (for ([path '("STATE-v0.99.73-ZERO-FAILING-TESTS.md"
                    "PLAN-v0.99.73.md"
                    "/tmp/.planning/VALIDATION-v0.99.73-W13.md")])
        (check-true (supercedes-generic-planning? path) path))
      (for ([path '("STATE.md" "VALIDATION.md"
                               "NOT-VALIDATION-v0.99.73-W13.md"
                               "notes-STATE-v0.99.73-W13.md"
                               "VALIDATION-v0.99.73-W13.md.bak")])
        (check-false (supercedes-generic-planning? path) path)))

    (test-case "supersession requires generic and named artifacts from the same family"
      (check-true (contradicts-generic-planning? "STATE.md" "STATE-v0.99.73-ZERO-FAILING-TESTS.md"))
      (check-true (contradicts-generic-planning? "/p/VALIDATION.md" "/p/VALIDATION-v0.99.73-W13.md"))
      (check-false (contradicts-generic-planning? "STATE.md" "PLAN-v0.99.73.md"))
      (check-false (contradicts-generic-planning? "VALIDATION.md" "STATE-v0.99.73-W13.md"))
      (check-false (contradicts-generic-planning? "STATE.md" #f))
      (check-false (contradicts-generic-planning? #f "STATE-v0.99.73.md")))))

(module+ test
  (require rackunit/text-ui)
  (run-tests op-checkpoint-tests))

(module+ main
  (run-tests op-checkpoint-tests))
