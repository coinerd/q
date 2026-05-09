#lang racket/base

;; tests/test-gsd-facade-sm-assertion.rkt — GSD facade split & state machine assertion (W-17, I-16, I-18, I-24)

(require rackunit
         "../extensions/gsd/state-machine.rkt"
         "../extensions/gsd/plan-types.rkt")

;; ============================================================
;; I-16: State machine assertion (TRANSITIONS states ⊆ GSD-STATES)
;; ============================================================

(test-case "all TRANSITIONS states are valid GSD-STATES"
  ;; If the assertion in state-machine.rkt didn't fire at module load time,
  ;; all transition states are valid. This test verifies programmatically.
  (for ([t TRANSITIONS])
    (check-true (gsm-state? (car t)) (format "from-state ~a not in GSD-STATES" (car t)))
    (check-true (gsm-state? (cdr t)) (format "to-state ~a not in GSD-STATES" (cdr t)))))

(test-case "GSD-STATES contains expected states"
  (for ([s GSD-STATES])
    (check-true (symbol? s)))
  (check-not-false (member 'idle GSD-STATES))
  (check-not-false (member 'exploring GSD-STATES))
  (check-not-false (member 'plan-written GSD-STATES))
  (check-not-false (member 'executing GSD-STATES))
  (check-not-false (member 'verifying GSD-STATES)))

(test-case "TRANSITIONS has expected from-idle entries"
  (define idle-targets (map cdr (filter (lambda (t) (eq? (car t) 'idle)) TRANSITIONS)))
  (check-not-false (member 'exploring idle-targets)))

(test-case "TRANSITIONS has expected from-executing entries"
  (define exec-targets (map cdr (filter (lambda (t) (eq? (car t) 'executing)) TRANSITIONS)))
  (check-not-false (member 'verifying exec-targets))
  (check-not-false (member 'idle exec-targets)))

(test-case "no transitions from bogus state"
  (define bogus-targets (filter (lambda (t) (eq? (car t) 'bogus)) TRANSITIONS))
  (check-equal? bogus-targets '()))

;; ============================================================
;; I-18: Validated plan parsing (replaces cast)
;; ============================================================

(test-case "expect-natural accepts natural numbers"
  (check-equal? (expect-natural 0) 0)
  (check-equal? (expect-natural 42) 42))

(test-case "expect-natural rejects non-naturals"
  (check-exn exn:fail? (lambda () (expect-natural -1)))
  (check-exn exn:fail? (lambda () (expect-natural "not a number"))))

(test-case "expect-string accepts strings"
  (check-equal? (expect-string "hello") "hello")
  (check-equal? (expect-string "") ""))

(test-case "expect-string rejects non-strings"
  (check-exn exn:fail? (lambda () (expect-string 42))))

(test-case "expect-string-list accepts string lists"
  (check-equal? (expect-string-list '("a" "b")) '("a" "b"))
  (check-equal? (expect-string-list '()) '()))

(test-case "expect-string-list rejects mixed lists"
  (check-exn exn:fail? (lambda () (expect-string-list '(1 "a")))))
