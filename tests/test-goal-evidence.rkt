#lang racket/base

;; tests/test-goal-evidence.rkt — Evidence prompt + no-progress detection tests

(require rackunit
         racket/string
         (only-in "../runtime/goal/goal-state.rkt"
                  make-goal-state
                  make-evaluation-result
                  evaluation-result?
                  evaluation-result-achieved?
                  evaluation-result-reason
                  goal-state-goal-text
                  goal-state-turns-used
                  goal-state-max-turns
                  goal-state?)
         "../runtime/goal/goal-evidence.rkt")

;; ============================================================
;; GOAL-EVIDENCE-SYSTEM-PROMPT is a non-empty string
;; ============================================================

(check-true (string? GOAL-EVIDENCE-SYSTEM-PROMPT))
(check-true (> (string-length GOAL-EVIDENCE-SYSTEM-PROMPT) 50))
(check-not-false (string-contains? GOAL-EVIDENCE-SYSTEM-PROMPT "GOAL ACHIEVED"))

;; ============================================================
;; goal-system-instructions returns list of 2 strings
;; ============================================================

(let ()
  (define gs (make-goal-state #:goal-text "tests pass" #:max-turns 8))
  (define instrs (goal-system-instructions gs))
  (check-equal? (length instrs) 2)
  (check-true (string-contains? (cadr instrs) "tests pass"))
  (check-true (string-contains? (cadr instrs) "turn 0/8")))

;; ============================================================
;; evidence-prompt-for-goal without evaluation
;; ============================================================

(let ()
  (define prompt (evidence-prompt-for-goal "fix the bug" #f))
  (check-true (string-contains? prompt "fix the bug"))
  (check-true (string-contains? prompt "evidence")))

;; ============================================================
;; evidence-prompt-for-goal with evaluation
;; ============================================================

(let ()
  (define eval (make-evaluation-result #:achieved? #f #:reason "still 2 test failures"))
  (define prompt (evidence-prompt-for-goal "tests pass" eval))
  (check-true (string-contains? prompt "tests pass"))
  (check-true (string-contains? prompt "still 2 test failures"))
  (check-true (string-contains? prompt "evidence")))

;; ============================================================
;; evidence-prompt-for-goal achieved includes evidence reminder
;; ============================================================

(let ()
  (define eval (make-evaluation-result #:achieved? #t #:reason "all green"))
  (define prompt (evidence-prompt-for-goal "tests pass" eval))
  (check-true (string-contains? prompt "tests pass")))

;; ============================================================
;; consecutive-same-reason? — fewer than threshold → #f
;; ============================================================

(check-false (consecutive-same-reason? '()))
(check-false (consecutive-same-reason? (list (make-evaluation-result #:achieved? #f
                                                                     #:reason "nope"))))

;; ============================================================
;; consecutive-same-reason? — same reason 3 times → #t
;; ============================================================

(check-true (consecutive-same-reason?
             (list (make-evaluation-result #:achieved? #f #:reason "no progress")
                   (make-evaluation-result #:achieved? #f #:reason "no progress")
                   (make-evaluation-result #:achieved? #f #:reason "no progress"))))

;; ============================================================
;; consecutive-same-reason? — mixed reasons → #f
;; ============================================================

(check-false (consecutive-same-reason?
              (list (make-evaluation-result #:achieved? #f #:reason "no progress")
                    (make-evaluation-result #:achieved? #f #:reason "different")
                    (make-evaluation-result #:achieved? #f #:reason "no progress"))))

;; ============================================================
;; consecutive-same-reason? — one achieved → #f
;; ============================================================

(check-false (consecutive-same-reason? (list (make-evaluation-result #:achieved? #t #:reason "done")
                                             (make-evaluation-result #:achieved? #t #:reason "done")
                                             (make-evaluation-result #:achieved? #t
                                                                     #:reason "done"))))

;; ============================================================
;; detect-no-progress — same as consecutive-same-reason?
;; ============================================================

(check-false (detect-no-progress '()))

(check-true (detect-no-progress (list (make-evaluation-result #:achieved? #f #:reason "stuck")
                                      (make-evaluation-result #:achieved? #f #:reason "stuck")
                                      (make-evaluation-result #:achieved? #f #:reason "stuck"))))

;; ============================================================
;; detect-no-progress — 5 results with last 3 same → #t
;; ============================================================

(check-true (detect-no-progress (list (make-evaluation-result #:achieved? #f #:reason "first")
                                      (make-evaluation-result #:achieved? #f #:reason "second")
                                      (make-evaluation-result #:achieved? #f #:reason "stuck")
                                      (make-evaluation-result #:achieved? #f #:reason "stuck")
                                      (make-evaluation-result #:achieved? #f #:reason "stuck"))))

(displayln "All goal-evidence tests passed.")
