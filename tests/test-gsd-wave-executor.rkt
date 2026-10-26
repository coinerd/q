#lang racket
;; @covers extensions/gsd/wave-executor.rkt

;; @speed fast  ;; @suite extensions
;; @boundary integration

;; BOUNDARY: integration

;; tests/test-gsd-wave-executor.rkt — Wave execution engine tests
;;
;; Wave 2b: Status lifecycle and error recovery.

(require rackunit
         "../extensions/gsd/wave-executor.rkt"
         "../extensions/gsd/plan-types.rkt"
         "../extensions/gsd/wave-docs.rkt")

;; ============================================================
;; Helper
;; ============================================================

(define (make-test-plan n)
  (wave-gate-clear!)
  (gsd-plan (for/list ([i (in-range n)])
              (gsd-wave i (format "Wave ~a" i) 'pending "" '("file.rkt") '() "test" '()))
            ""
            '()
            '()))

;; ============================================================
;; Lifecycle: pending → in-progress → completed
;; ============================================================

(test-case "status lifecycle: pending → in-progress → completed"
  (define exec (make-wave-executor (make-test-plan 3)))
  (check-equal? (next-pending-wave exec) 0)
  (wave-start! exec 0)
  (define s0 (car (wave-executor-statuses exec)))
  (check-eq? (wave-status-state s0) 'in-progress)
  (check-equal? (wave-status-attempt-count s0) 1)
  (wave-complete! exec 0)
  (define s0-done (car (wave-executor-statuses exec)))
  (check-eq? (wave-status-state s0-done) 'completed))

;; ============================================================
;; Failed wave recovery
;; ============================================================

(test-case "failed wave: pending → in-progress → failed"
  (define exec (make-wave-executor (make-test-plan 3)))
  (wave-start! exec 0)
  (wave-fail! exec 0 "compile error in foo.rkt")
  (define s0 (car (wave-executor-statuses exec)))
  (check-eq? (wave-status-state s0) 'failed)
  (check-equal? (wave-status-error-message s0) "compile error in foo.rkt"))

(test-case "failed wave does not block next wave"
  (define exec (make-wave-executor (make-test-plan 3)))
  (wave-start! exec 0)
  (wave-fail! exec 0 "error")
  ;; Next pending wave should be 1
  (check-equal? (next-pending-wave exec) 1)
  ;; And we can execute it
  (wave-start! exec 1)
  (wave-complete! exec 1)
  (define statuses (wave-executor-statuses exec))
  (check-eq? (wave-status-state (list-ref statuses 0)) 'failed)
  (check-eq? (wave-status-state (list-ref statuses 1)) 'completed))

;; ============================================================
;; Skip wave
;; ============================================================

(test-case "skip wave: marks as skipped"
  (define exec (make-wave-executor (make-test-plan 3)))
  (wave-skip! exec 1)
  (define statuses (wave-executor-statuses exec))
  (check-eq? (wave-status-state (list-ref statuses 1)) 'skipped)
  ;; Next pending should skip over wave 1
  (check-equal? (next-pending-wave exec) 0))

(test-case "skip advances to next pending wave"
  (define exec (make-wave-executor (make-test-plan 3)))
  (wave-start! exec 0)
  (wave-complete! exec 0)
  (wave-skip! exec 1)
  (check-equal? (next-pending-wave exec) 2))

;; ============================================================
;; All done check
;; ============================================================

(test-case "all-waves-done? returns #f when pending remain"
  (define exec (make-wave-executor (make-test-plan 3)))
  (wave-complete! exec 0)
  (check-false (all-waves-done? exec)))

(test-case "all-waves-done? returns #t when all completed"
  (define exec (make-wave-executor (make-test-plan 2)))
  (wave-complete! exec 0)
  (wave-complete! exec 1)
  (check-true (all-waves-done? exec)))

(test-case "all-waves-done? returns #t when mix of completed/failed/skipped"
  (define exec (make-wave-executor (make-test-plan 3)))
  (wave-complete! exec 0)
  (wave-fail! exec 1 "error")
  (wave-skip! exec 2)
  (check-true (all-waves-done? exec)))

;; ============================================================
;; Summary
;; ============================================================

(test-case "wave-summary shows correct counts"
  (define exec (make-wave-executor (make-test-plan 3)))
  (wave-complete! exec 0)
  (wave-fail! exec 1 "error")
  (wave-skip! exec 2)
  (define summary (wave-summary exec))
  (check-true (string-contains? summary "Total: 3"))
  (check-true (string-contains? summary "Completed: 1"))
  (check-true (string-contains? summary "Failed: 1"))
  (check-true (string-contains? summary "Skipped: 1")))

(test-case "wave-summary with all pending"
  (define exec (make-wave-executor (make-test-plan 2)))
  (define summary (wave-summary exec))
  (check-true (string-contains? summary "Total: 2"))
  (check-true (string-contains? summary "Pending: 2")))

;; ============================================================
;; next-pending-wave
;; ============================================================

(test-case "next-pending-wave returns #f when no pending"
  (define exec (make-wave-executor (make-test-plan 2)))
  (wave-complete! exec 0)
  (wave-complete! exec 1)
  (check-false (next-pending-wave exec)))

(test-case "attempt count increments on wave-start!"
  (define exec (make-wave-executor (make-test-plan 1)))
  (wave-start! exec 0)
  (wave-start! exec 0) ;; restart
  (define s0 (car (wave-executor-statuses exec)))
  (check-equal? (wave-status-attempt-count s0) 2))

;; ============================================================
;; load-plan-from-index
;; ============================================================

(test-case "load-plan-from-index returns #f when no PLAN.md"
  (check-false (load-plan-from-index "/tmp/nonexistent-dir-for-test")))

(test-case "load-plan-from-index loads waves from disk"
  (define dir (make-temporary-file "gsd-index-test-~a" 'directory))
  (define planning-dir (build-path dir ".planning"))
  (make-directory planning-dir)
  (make-directory (build-path planning-dir "waves"))
  ;; Write PLAN.md index
  (define plan-md
    (string-append "# Plan: Test Plan\n\n"
                   "## Waves\n"
                   "- [Inbox] W0: Fix bug → waves/W0-fix-bug.md\n"
                   "- [Inbox] W1: Add tests → waves/W1-add-tests.md\n"))
  (call-with-output-file (build-path planning-dir "PLAN.md")
                         (lambda (out) (display plan-md out))
                         #:exists 'truncate)
  ;; Write wave docs
  (write-wave-doc! dir 0 "fix-bug" "- File: foo.rkt\n- Verify: raco test\n" "Inbox")
  (write-wave-doc! dir 1 "add-tests" "- File: test-foo.rkt\n- Verify: raco test\n" "Inbox")
  ;; Load and verify
  (define plan (load-plan-from-index dir))
  (check-not-false plan)
  (check-equal? (length (gsd-plan-waves plan)) 2)
  (define w0 (list-ref (gsd-plan-waves plan) 0))
  (check-equal? (gsd-wave-index w0) 0)
  (check-true (string-contains? (gsd-wave-title w0) "Fix bug"))
  ;; Cleanup
  (delete-directory/files dir #:must-exist? #f))

(test-case "load-plan-from-index preserves completed status"
  (define dir (make-temporary-file "gsd-index-status-~a" 'directory))
  (define planning-dir (build-path dir ".planning"))
  (make-directory planning-dir)
  (make-directory (build-path planning-dir "waves"))
  (define plan-md
    "# Plan: Status Test\n\n## Waves\n- [DONE] W0: Done wave → waves/W0-done-wave.md\n- [Inbox] W1: Todo wave → waves/W1-todo-wave.md\n")
  (call-with-output-file (build-path planning-dir "PLAN.md")
                         (lambda (out) (display plan-md out))
                         #:exists 'truncate)
  (write-wave-doc! dir 0 "done-wave" "Content" "DONE")
  (write-wave-doc! dir 1 "todo-wave" "Content" "Inbox")
  (define plan (load-plan-from-index dir))
  (check-not-false plan)
  (define w0 (list-ref (gsd-plan-waves plan) 0))
  (check-eq? (gsd-wave-status w0) 'completed)
  (define w1 (list-ref (gsd-plan-waves plan) 1))
  (check-eq? (gsd-wave-status w1) 'pending)
  (delete-directory/files dir #:must-exist? #f))

(test-case "BUG-0082: loader content comes from the DECLARED doc, never the stale twin"
  (define dir (make-temporary-file "gsd-index-declared-~a" 'directory))
  (define planning-dir (build-path dir ".planning"))
  (make-directory planning-dir)
  (make-directory (build-path planning-dir "waves"))
  (define plan-md "# Plan: Declared\n\n## Waves\n- [Inbox] W2: Canary → waves/W0-carrier-eight.md\n")
  (call-with-output-file (build-path planning-dir "PLAN.md")
                         (lambda (out) (display plan-md out))
                         #:exists 'truncate)
  ;; the DECLARED carrier
  (call-with-output-file
   (build-path planning-dir "waves" "W0-carrier-eight.md")
   (lambda (out) (display "# Wave 0\nStatus: Inbox\n\n## Verify\nstrong-carrier-verify-9f2\n" out))
   #:exists 'truncate)
  ;; a stale synthesized twin — different bytes, never authoritative
  (call-with-output-file (build-path planning-dir "waves" "W2-canary.md")
                         (lambda (out)
                           (display "# Wave 2\nStatus: Inbox\n\nweak-stale-verify-3b7\n" out))
                         #:exists 'truncate)
  (define plan (load-plan-from-index dir))
  (check-not-false plan)
  (define w2 (list-ref (gsd-plan-waves plan) 0))
  (check-equal? (gsd-wave-index w2) 2)
  (check-true (string-contains? (gsd-wave-verify w2) "strong-carrier-verify-9f2"))
  (check-false (string-contains? (gsd-wave-verify w2) "weak-stale-verify-3b7"))
  ;; missing-declared: deleting the carrier refuses BY NAME despite the twin
  (delete-file (build-path planning-dir "waves" "W0-carrier-eight.md"))
  (check-exn (lambda (e)
               (and (exn:fail? e)
                    (string-contains? (exn-message e) ".planning/waves/W0-carrier-eight.md")))
             (lambda () (load-plan-from-index dir)))
  (delete-directory/files dir #:must-exist? #f))

(test-case "BUG-0082: shared-carrier validation refuses the ambiguous mapping"
  (define dir (make-temporary-file "gsd-index-shared-~a" 'directory))
  (define planning-dir (build-path dir ".planning"))
  (make-directory planning-dir)
  (make-directory (build-path planning-dir "waves"))
  (call-with-output-file
   (build-path planning-dir "PLAN.md")
   (lambda (out)
     (display (string-append "# Plan\n\n## Waves\n"
                             "- [Inbox] W0: carrier → waves/W0-carrier-nine.md\n"
                             "- [Inbox] W2: twin → waves/W0-carrier-nine.md\n")
              out))
   #:exists 'truncate)
  (call-with-output-file (build-path planning-dir "waves" "W0-carrier-nine.md")
                         (lambda (out) (display "# Wave 0\nStatus: Inbox\n\nBody.\n" out))
                         #:exists 'truncate)
  (define result (validate-plan-artifacts dir))
  (check-false (hash-ref result 'ok?))
  (check-true (ormap (lambda (f)
                       (string-contains? (plan-validation-finding-message f)
                                         ".planning/waves/W0-carrier-nine.md"))
                     (plan-validation-error-findings (hash-ref result 'findings))))
  (delete-directory/files dir #:must-exist? #f))
