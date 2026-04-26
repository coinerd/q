#lang racket

;; tests/test-gsd-plan-types.rkt — Structured Plan/Task/Wave type tests
;;
;; Wave 0 of v0.21.0: Tests for structured plan types.
;; Covers: wave construction, task construction, plan construction,
;; parsing waves from markdown, validation, serialization.

(require rackunit
         "../extensions/gsd/plan-types.rkt")

;; ============================================================
;; gsd-task construction
;; ============================================================

(test-case "gsd-task construction"
  (define t
    (gsd-task "Fix the bug"
              '("src/fix.rkt")
              "Replace the broken function"
              "raco test tests/test-fix.rkt"
              "Tests pass"
              'pending))
  (check-equal? (gsd-task-name t) "Fix the bug")
  (check-equal? (gsd-task-files t) '("src/fix.rkt"))
  (check-equal? (gsd-task-action t) "Replace the broken function")
  (check-equal? (gsd-task-verify t) "raco test tests/test-fix.rkt")
  (check-equal? (gsd-task-done t) "Tests pass")
  (check-eq? (gsd-task-status t) 'pending))

(test-case "gsd-task status transitions"
  (define t (gsd-task "Fix" '() "" "" "" 'pending))
  (check-eq? (gsd-task-status t) 'pending)
  (define t2 (gsd-task-set-status t 'in-progress))
  (check-eq? (gsd-task-status t2) 'in-progress)
  ;; Original is immutable
  (check-eq? (gsd-task-status t) 'pending))

(test-case "gsd-task with default status"
  (define t (make-gsd-task "Task name" '("file.rkt") "Do the thing" "raco test tests/"))
  (check-eq? (gsd-task-status t) 'pending)
  (check-equal? (gsd-task-done t) ""))

;; ============================================================
;; gsd-wave construction
;; ============================================================

(test-case "gsd-wave construction"
  (define t (make-gsd-task "Fix" '("f.rkt") "fix it" "test"))
  (define w
    (gsd-wave 0
              "Foundation"
              'pending
              "Root cause is X"
              '("extensions/gsd/state-machine.rkt")
              (list t)
              "raco test tests/"
              '("All tests pass")))
  (check-equal? (gsd-wave-index w) 0)
  (check-equal? (gsd-wave-title w) "Foundation")
  (check-eq? (gsd-wave-status w) 'pending)
  (check-equal? (gsd-wave-root-cause w) "Root cause is X")
  (check-equal? (gsd-wave-files w) '("extensions/gsd/state-machine.rkt"))
  (check-equal? (length (gsd-wave-tasks w)) 1)
  (check-equal? (gsd-wave-verify w) "raco test tests/")
  (check-equal? (gsd-wave-done-criteria w) '("All tests pass")))

(test-case "gsd-wave status transitions"
  (define w (make-gsd-wave 0 "Test" "RC" '("f") '() "test" '("done")))
  (check-eq? (gsd-wave-status w) 'pending)
  (define w2 (gsd-wave-set-status w 'in-progress))
  (check-eq? (gsd-wave-status w2) 'in-progress))

(test-case "gsd-wave-set-status: all valid statuses"
  (define w (make-gsd-wave 0 "T" "" '() '() "" '()))
  (for ([s '(pending in-progress completed skipped failed)])
    (define w2 (gsd-wave-set-status w s))
    (check-eq? (gsd-wave-status w2) s)))

;; ============================================================
;; gsd-plan construction
;; ============================================================

(test-case "gsd-plan construction"
  (define w1 (make-gsd-wave 0 "W0" "RC" '("f1") '() "test" '("done")))
  (define w2 (make-gsd-wave 1 "W1" "RC" '("f2") '() "test" '("done")))
  (define p
    (gsd-plan (list w1 w2) "PROJECT.md, STATE.md" '("Do not touch foo.rkt") '("REQ-001" "REQ-002")))
  (check-equal? (length (gsd-plan-waves p)) 2)
  (check-equal? (gsd-plan-context-bundle p) "PROJECT.md, STATE.md")
  (check-equal? (gsd-plan-constraints p) '("Do not touch foo.rkt"))
  (check-equal? (gsd-plan-must-haves p) '("REQ-001" "REQ-002")))

(test-case "gsd-plan with no waves"
  (define p (gsd-plan '() "" '() '()))
  (check-equal? (length (gsd-plan-waves p)) 0))

;; ============================================================
;; Wave parsing from markdown
;; ============================================================

(test-case "parse-waves-from-markdown: single wave"
  (define md "## Wave 0: Foundation\n- Root cause: X\n- File: f.rkt\n- Verify: test\n")
  (define waves (parse-waves-from-markdown md))
  (check-equal? (length waves) 1)
  (check-equal? (gsd-wave-index (car waves)) 0)
  (check-equal? (gsd-wave-title (car waves)) "Foundation"))

(test-case "parse-waves-from-markdown: multiple waves"
  (define md
    (string-append "## Wave 0: Foundation\nSome content\n\n"
                   "## Wave 1: Core Rewrite\nMore content\n\n"
                   "## Wave 2: Tests\nFinal content\n"))
  (define waves (parse-waves-from-markdown md))
  (check-equal? (length waves) 3)
  (check-equal? (gsd-wave-title (first waves)) "Foundation")
  (check-equal? (gsd-wave-title (second waves)) "Core Rewrite")
  (check-equal? (gsd-wave-title (third waves)) "Tests"))

(test-case "parse-waves-from-markdown: extracts verify command"
  (define md "## Wave 0: Test Wave\n- Verify: raco test tests/test.rkt\n- Root cause: bug\n")
  (define waves (parse-waves-from-markdown md))
  (check-equal? (length waves) 1)
  (check-equal? (gsd-wave-verify (car waves)) "raco test tests/test.rkt"))

(test-case "parse-waves-from-markdown: extracts files"
  (define md "## Wave 0: Test Wave\n- File: src/fix.rkt\n- File: tests/test.rkt\n- Verify: test\n")
  (define waves (parse-waves-from-markdown md))
  (check-equal? (length waves) 1)
  ;; Files are extracted from "- File: ..." lines
  (check-equal? (length (gsd-wave-files (car waves))) 2))

(test-case "parse-waves-from-markdown: no waves returns empty"
  (define md "This is just text.\nNo wave headers here.\n")
  (define waves (parse-waves-from-markdown md))
  (check-equal? waves '()))

(test-case "parse-waves-from-markdown: extracts root cause"
  (define md "## Wave 0: Bug Fix\n- Root cause: off-by-one error in loop\n- Verify: test\n")
  (define waves (parse-waves-from-markdown md))
  (check-equal? (gsd-wave-root-cause (car waves)) "off-by-one error in loop"))

(test-case "parse-waves-from-markdown: extracts done criteria"
  (define md "## Wave 0: Bug Fix\n- Done: All tests pass\n- Done: No regressions\n- Verify: test\n")
  (define waves (parse-waves-from-markdown md))
  (check-equal? (gsd-wave-done-criteria (car waves)) '("All tests pass" "No regressions")))

;; ============================================================
;; Plan validation
;; ============================================================

(test-case "validate-plan: valid plan returns empty errors"
  (define w (make-gsd-wave 0 "Fix Bug" "RC" '("f.rkt") '() "raco test" '("Tests pass")))
  (define p (gsd-plan (list w) "" '() '()))
  (define result (validate-plan p))
  (check-equal? (validation-errors result) '()))

(test-case "validate-plan: empty plan has error"
  (define p (gsd-plan '() "" '() '()))
  (define result (validate-plan p))
  (check-not-equal? (validation-errors result) '()))

(test-case "validate-plan: wave with no files has warning"
  (define w (make-gsd-wave 0 "No Files" "RC" '() '() "test" '("done")))
  (define p (gsd-plan (list w) "" '() '()))
  (define result (validate-plan p))
  (check-equal? (validation-errors result) '())
  (check-not-equal? (validation-warnings result) '()))

(test-case "validate-plan: wave with no title has error"
  (define w (make-gsd-wave 0 "" "RC" '("f.rkt") '() "test" '("done")))
  (define p (gsd-plan (list w) "" '() '()))
  (define result (validate-plan p))
  (check-not-equal? (validation-errors result) '()))

(test-case "validate-plan: wave with no verify command has warning"
  (define w (make-gsd-wave 0 "Wave" "RC" '("f.rkt") '() "" '("done")))
  (define p (gsd-plan (list w) "" '() '()))
  (define result (validate-plan p))
  (check-equal? (validation-errors result) '())
  (check-not-equal? (validation-warnings result) '()))

(test-case "validation-result: is-valid?"
  (define vr (validation-result '() '()))
  (check-true (validation-valid? vr))
  (define vr2 (validation-result '("error") '()))
  (check-false (validation-valid? vr2)))

;; ============================================================
;; Utility: wave index lookup
;; ============================================================

(test-case "plan-wave-ref: find wave by index"
  (define w0 (make-gsd-wave 0 "First" "" '("a") '() "t" '()))
  (define w1 (make-gsd-wave 1 "Second" "" '("b") '() "t" '()))
  (define p (gsd-plan (list w0 w1) "" '() '()))
  (check-equal? (gsd-wave-title (plan-wave-ref p 1)) "Second"))

(test-case "plan-wave-ref: missing index returns #f"
  (define w0 (make-gsd-wave 0 "Only" "" '("a") '() "t" '()))
  (define p (gsd-plan (list w0) "" '() '()))
  (check-false (plan-wave-ref p 99)))

(test-case "plan-pending-waves: returns waves with pending status"
  (define w0 (gsd-wave 0 "A" 'pending "" '("a") '() "t" '()))
  (define w1 (gsd-wave 1 "B" 'completed "" '("b") '() "t" '()))
  (define w2 (gsd-wave 2 "C" 'pending "" '("c") '() "t" '()))
  (define p (gsd-plan (list w0 w1 w2) "" '() '()))
  (define pending (plan-pending-waves p))
  (check-equal? (length pending) 2)
  (check-equal? (gsd-wave-index (first pending)) 0)
  (check-equal? (gsd-wave-index (second pending)) 2))

(test-case "plan-next-pending-wave: returns first pending"
  (define w0 (gsd-wave 0 "A" 'completed "" '("a") '() "t" '()))
  (define w1 (gsd-wave 1 "B" 'pending "" '("b") '() "t" '()))
  (define p (gsd-plan (list w0 w1) "" '() '()))
  (define next (plan-next-pending-wave p))
  (check-not-false next)
  (check-equal? (gsd-wave-index next) 1))

(test-case "plan-next-pending-wave: returns #f when all complete"
  (define w0 (gsd-wave 0 "A" 'completed "" '("a") '() "t" '()))
  (define p (gsd-plan (list w0) "" '() '()))
  (check-false (plan-next-pending-wave p)))
