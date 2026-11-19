#lang racket

;; tests/test-qt-quick-run-classification.rkt
;; W5 (#9953) bounded pure pilot, candidate 2: direct table-driven coverage of
;; classify-quick-child-run — the ONE pure trust classification every ./qt
;; quick and ./qt full evidence record passes through (scripts/testing/
;; quick-runner.rkt run-one). Before this owner the decision was two
;; copy-pasted inline precedence ladders plus two exit-code mappings,
;; exercised only through effectful subprocess fixtures:
;; tests/test-qt-output-limits.rkt pins the truncated/timeout arms via real
;; children, while the identity, no-evidence and exit arms and the exit-code
;; mapping had no direct coverage at all.

;; @speed fast
;; @suite testing
;; @boundary unit
;; @covers scripts/testing/quick-runner.rkt

(require rackunit
         "../scripts/testing/quick-runner.rkt")

(module+ test
  (define (row evidence timed-out? truncated? exit)
    (call-with-values (lambda () (classify-quick-child-run evidence timed-out? truncated? exit))
                      list))

  (test-case "well-formed matched evidence: clean runs stay valid, failures exit"
    ;; A clean matched run reports the child's own (zero) exit and no reason.
    (check-equal? (row 'matched #f #f 0) (list 0 #f))
    ;; A nonzero exit from a complete evidence record is the child's own
    ;; assertion failure: the real code rides through, 'exit invalidates.
    (check-equal? (row 'matched #f #f 3) (list 3 'exit)))

  (test-case "child plane precedence: timeout > truncated > exit"
    (check-equal? (row 'matched #t #f 0) (list 1 'timeout))
    (check-equal? (row 'matched #f #t 0) (list 1 'truncated))
    ;; A timeout/truncated child that ALSO exited nonzero is still the
    ;; child-plane reason, never the exit reason — in BOTH evidence shapes.
    (check-equal? (row 'matched #t #f 2) (list 1 'timeout))
    (check-equal? (row 'matched #f #t 2) (list 1 'truncated))
    (check-equal? (row 'absent #t #f 2) (list 1 'timeout))
    (check-equal? (row 'absent #f #t 2) (list 1 'truncated)))

  (test-case "mismatched evidence identity wins the precedence head"
    (check-equal? (row 'mismatched #f #f 0) (list 1 'identity))
    (check-equal? (row 'mismatched #f #f 5) (list 1 'identity))
    (check-equal? (row 'mismatched #t #f 0) (list 1 'identity))
    (check-equal? (row 'mismatched #f #t 0) (list 1 'identity)))

  (test-case "absent evidence: no-evidence fails closed, exit still truthful"
    (check-equal? (row 'absent #f #f 0) (list 1 'no-evidence))
    (check-equal? (row 'absent #f #f 7) (list 7 'exit)))

  (test-case "only 'exit reports the child's own exit code"
    (check-equal? (row 'matched #f #t 4) (list 1 'truncated))
    (check-equal? (row 'mismatched #f #f 4) (list 1 'identity)))

  (test-case "deterministic over its immutable inputs"
    (check-equal? (row 'matched #t #t 9) (list 1 'timeout))
    (check-equal? (row 'matched #t #t 9) (row 'matched #t #t 9))
    (check-equal? (row 'absent #f #f 1) (row 'absent #f #f 1))))
