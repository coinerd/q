#lang racket/base

;; tests/test-gsd-wave-docs.rkt — Tests for wave document I/O and PLAN.md index
;;
;; v0.21.1 W0: Tests for the wave-docs module covering:
;; - Slug generation
;; - Wave document write/read roundtrip
;; - PLAN.md index parsing and updating
;; - Status transitions with dual-write
;; - Wave queries (next-inbox, overall status)

(require rackunit
         racket/file
         racket/string
         racket/path
         racket/port
         "../extensions/gsd/wave-docs.rkt")

;; ============================================================
;; Test helpers
;; ============================================================

(define (make-temp-planning-dir)
  (define dir (make-temporary-file "wave-docs-test-~a" 'directory))
  (make-directory* (build-path dir ".planning"))
  dir)

(define (cleanup-dir dir)
  (delete-directory/files dir #:must-exist? #f))

(define test-dir #f)

(define (setup)
  (set! test-dir (make-temp-planning-dir)))

(define (teardown)
  (when test-dir
    (cleanup-dir test-dir)
    (set! test-dir #f)))

;; ============================================================
;; Slug generation
;; ============================================================

(test-case "slugify: basic title"
  (setup)
  (check-equal? (slugify "Fix the bug") "fix-the-bug")
  (teardown))

(test-case "slugify: special characters"
  (check-equal? (slugify "Read/write (v2) & more!") "readwrite-v2-more")
  (teardown))

(test-case "slugify: truncation to 40 chars"
  (define long-title (make-string 100 #\a))
  (check-true (<= (string-length (slugify long-title)) 40)))

(test-case "slugify: empty string"
  (check-equal? (slugify "") "wave"))

(test-case "slugify: consecutive hyphens collapsed"
  (check-equal? (slugify "a---b---c") "a-b-c"))

(test-case "slugify: leading/trailing hyphens trimmed"
  (check-equal? (slugify " hello world ") "hello-world"))

(test-case "slugify: numeric preserved"
  (check-equal? (slugify "Fix bug #123") "fix-bug-123"))

;; ============================================================
;; Wave document path
;; ============================================================

(test-case "wave-doc-path: correct structure"
  (setup)
  (define p (wave-doc-path test-dir 0 "fix-bug"))
  (check-true (string? (path->string p)))
  (check-true (string-contains? (path->string p) ".planning/waves/W0-fix-bug.md"))
  (teardown))

(test-case "wave-doc-path: different indices"
  (setup)
  (define p0 (wave-doc-path test-dir 0 "a"))
  (define p3 (wave-doc-path test-dir 3 "b"))
  (check-true (string-contains? (path->string p0) "W0-a.md"))
  (check-true (string-contains? (path->string p3) "W3-b.md"))
  (teardown))

;; ============================================================
;; Wave document write + read roundtrip
;; ============================================================

(test-case "write-wave-doc!: creates file"
  (setup)
  (define path (write-wave-doc! test-dir 0 "fix-bug" "## Tasks\nDo stuff" "Inbox"))
  (check-true (file-exists? path))
  (teardown))

(test-case "write-wave-doc!: creates waves directory"
  (setup)
  (write-wave-doc! test-dir 0 "fix-bug" "content" "Inbox")
  (check-true (directory-exists? (build-path test-dir ".planning" "waves")))
  (teardown))

(test-case "read-wave-doc: roundtrip"
  (setup)
  (write-wave-doc! test-dir 0 "fix-bug" "## Tasks\nDo stuff" "Inbox")
  (define doc (read-wave-doc test-dir 0 "fix-bug"))
  (check-not-false doc)
  (check-equal? (hash-ref doc 'status) "Inbox")
  (check-equal? (hash-ref doc 'index) 0)
  (check-equal? (hash-ref doc 'slug) "fix-bug")
  (check-true (string-contains? (hash-ref doc 'content) "## Tasks"))
  (teardown))

(test-case "read-wave-doc: nonexistent returns #f"
  (setup)
  (check-false (read-wave-doc test-dir 99 "nope"))
  (teardown))

(test-case "wave-exists?: true after write"
  (setup)
  (write-wave-doc! test-dir 0 "fix-bug" "content" "Inbox")
  (check-true (wave-exists? test-dir 0 "fix-bug"))
  (check-false (wave-exists? test-dir 1 "nope"))
  (teardown))

(test-case "write-wave-doc!: overwrite updates content"
  (setup)
  (write-wave-doc! test-dir 0 "fix-bug" "v1" "Inbox")
  (write-wave-doc! test-dir 0 "fix-bug" "v2" "DONE")
  (define doc (read-wave-doc test-dir 0 "fix-bug"))
  (check-equal? (hash-ref doc 'status) "DONE")
  (check-true (string-contains? (hash-ref doc 'content) "v2"))
  (check-false (string-contains? (hash-ref doc 'content) "v1"))
  (teardown))

;; ============================================================
;; PLAN.md index parsing
;; ============================================================

(define sample-plan-index
  "# Plan: Fix all the bugs\n\n## Waves\n\n- [Inbox] W0: Fix the bug → waves/W0-fix-the-bug.md\n- [Inbox] W1: Add tests → waves/W1-add-tests.md\n- [Inbox] W2: Update docs → waves/W2-update-docs.md\n")

(test-case "parse-plan-index: extracts all waves"
  (define entries (parse-plan-index sample-plan-index))
  (check-equal? (length entries) 3)
  (check-equal? (wave-index-entry-idx (car entries)) 0)
  (check-equal? (wave-index-entry-status (car entries)) "Inbox")
  (check-equal? (wave-index-entry-title (car entries)) "Fix the bug"))

(test-case "parse-plan-index: mixed statuses"
  (define mixed-plan
    (string-append "- [DONE] W0: Fix the bug → waves/W0-fix-the-bug.md\n"
                   "- [In-Progress] W1: Add tests → waves/W1-add-tests.md\n"
                   "- [FAILED] W2: Bad wave → waves/W2-bad-wave.md\n"
                   "- [DEFERRED] W3: Later → waves/W3-later.md\n"))
  (define entries (parse-plan-index mixed-plan))
  (check-equal? (length entries) 4)
  (check-equal? (wave-index-entry-status (list-ref entries 0)) "DONE")
  (check-equal? (wave-index-entry-status (list-ref entries 1)) "In-Progress")
  (check-equal? (wave-index-entry-status (list-ref entries 2)) "FAILED")
  (check-equal? (wave-index-entry-status (list-ref entries 3)) "DEFERRED"))

(test-case "parse-plan-index: no waves returns empty"
  (define entries (parse-plan-index "# Just a doc\nNo waves here\n"))
  (check-equal? entries '()))

(test-case "parse-plan-index: slug from target path"
  (define entries (parse-plan-index "- [Inbox] W0: Title → waves/W0-my-slug.md\n"))
  (check-equal? (length entries) 1)
  (check-equal? (wave-index-entry-slug (car entries)) "my-slug"))

;; ============================================================
;; PLAN.md index status update
;; ============================================================

(test-case "update-wave-in-index!: changes status marker"
  (setup)
  (define plan-path (build-path test-dir ".planning" "PLAN.md"))
  (call-with-output-file plan-path (lambda (out) (display sample-plan-index out)) #:exists 'truncate)
  (update-wave-in-index! test-dir 0 "DONE")
  (define new-text (call-with-input-file plan-path port->string))
  (check-true (string-contains? new-text "[DONE] W0:"))
  (check-true (string-contains? new-text "[Inbox] W1:"))
  (teardown))

(test-case "update-wave-in-index!: nonexistent plan returns #f"
  (setup)
  (check-false (update-wave-in-index! test-dir 0 "DONE"))
  (teardown))

(test-case "update-wave-in-index!: nonexistent wave returns #t"
  ;; Should still succeed — just no line matches
  (setup)
  (define plan-path (build-path test-dir ".planning" "PLAN.md"))
  (call-with-output-file plan-path (lambda (out) (display sample-plan-index out)) #:exists 'truncate)
  (check-true (update-wave-in-index! test-dir 99 "DONE"))
  (teardown))

;; ============================================================
;; Wave queries
;; ============================================================

(test-case "next-inbox-wave: returns first Inbox wave"
  (setup)
  (define plan-path (build-path test-dir ".planning" "PLAN.md"))
  (call-with-output-file plan-path (lambda (out) (display sample-plan-index out)) #:exists 'truncate)
  (define next (next-inbox-wave test-dir))
  (check-not-false next)
  (check-equal? (wave-index-entry-idx next) 0)
  (teardown))

(test-case "next-inbox-wave: skips DONE waves"
  (setup)
  (define plan-path (build-path test-dir ".planning" "PLAN.md"))
  (call-with-output-file plan-path (lambda (out) (display sample-plan-index out)) #:exists 'truncate)
  (update-wave-in-index! test-dir 0 "DONE")
  (define next (next-inbox-wave test-dir))
  (check-not-false next)
  (check-equal? (wave-index-entry-idx next) 1)
  (teardown))

(test-case "next-inbox-wave: returns FAILED waves for retry"
  (setup)
  (define plan-path (build-path test-dir ".planning" "PLAN.md"))
  (define plan-text
    "- [DONE] W0: First → waves/W0-first.md\n- [FAILED] W1: Bad → waves/W1-bad.md\n- [Inbox] W2: Next → waves/W2-next.md\n")
  (call-with-output-file plan-path (lambda (out) (display plan-text out)) #:exists 'truncate)
  ;; FAILED wave (W1) comes before Inbox (W2)
  (define next (next-inbox-wave test-dir))
  (check-not-false next)
  (check-equal? (wave-index-entry-idx next) 1)
  (teardown))

(test-case "next-inbox-wave: #f when all done"
  (setup)
  (define plan-path (build-path test-dir ".planning" "PLAN.md"))
  (define plan-text
    "- [DONE] W0: First → waves/W0-first.md\n- [DEFERRED] W1: Skip → waves/W1-skip.md\n")
  (call-with-output-file plan-path (lambda (out) (display plan-text out)) #:exists 'truncate)
  (check-false (next-inbox-wave test-dir))
  (teardown))

(test-case "next-inbox-wave: #f when no plan file"
  (setup)
  (check-false (next-inbox-wave test-dir))
  (teardown))

;; ============================================================
;; Dual-write status transition
;; ============================================================

(test-case "mark-wave-status!: dual write updates both files"
  (setup)
  ;; Create wave docs
  (write-wave-doc! test-dir 0 "fix-bug" "## Tasks\nFix it" "Inbox")
  (write-wave-doc! test-dir 1 "add-tests" "## Tasks\nTest it" "Inbox")
  ;; Create PLAN.md index
  (define plan-path (build-path test-dir ".planning" "PLAN.md"))
  (call-with-output-file plan-path
                         (lambda (out)
                           (display "- [Inbox] W0: Fix the bug → waves/W0-fix-bug.md\n" out)
                           (display "- [Inbox] W1: Add tests → waves/W1-add-tests.md\n" out))
                         #:exists 'truncate)
  ;; Mark W0 as DONE
  (mark-wave-status! test-dir 0 "DONE")
  ;; Check wave doc updated
  (define doc (read-wave-doc test-dir 0 "fix-bug"))
  (check-equal? (hash-ref doc 'status) "DONE")
  ;; Check PLAN.md index updated
  (define new-plan (call-with-input-file plan-path port->string))
  (check-true (string-contains? new-plan "[DONE] W0:"))
  (check-true (string-contains? new-plan "[Inbox] W1:"))
  (teardown))

(test-case "mark-wave-status!: #f for nonexistent wave"
  (setup)
  (define plan-path (build-path test-dir ".planning" "PLAN.md"))
  (call-with-output-file plan-path
                         (lambda (out) (display "- [Inbox] W0: Fix → waves/W0-fix.md\n" out))
                         #:exists 'truncate)
  (check-false (mark-wave-status! test-dir 99 "DONE"))
  (teardown))

;; ============================================================
;; Plan overall status
;; ============================================================

(test-case "plan-overall-status: not-started when no plan"
  (setup)
  (check-equal? (plan-overall-status test-dir) 'not-started)
  (teardown))

(test-case "plan-overall-status: not-started when empty plan"
  (setup)
  (define plan-path (build-path test-dir ".planning" "PLAN.md"))
  (call-with-output-file plan-path (lambda (out) (display "# No waves yet\n" out)) #:exists 'truncate)
  (check-equal? (plan-overall-status test-dir) 'not-started)
  (teardown))

(test-case "plan-overall-status: in-progress when all Inbox"
  (setup)
  (define plan-path (build-path test-dir ".planning" "PLAN.md"))
  (call-with-output-file plan-path (lambda (out) (display sample-plan-index out)) #:exists 'truncate)
  (check-equal? (plan-overall-status test-dir) 'in-progress)
  (teardown))

(test-case "plan-overall-status: partly-done when some done"
  (setup)
  (define plan-path (build-path test-dir ".planning" "PLAN.md"))
  (call-with-output-file plan-path (lambda (out) (display sample-plan-index out)) #:exists 'truncate)
  (update-wave-in-index! test-dir 0 "DONE")
  (check-equal? (plan-overall-status test-dir) 'partly-done)
  (teardown))

(test-case "plan-overall-status: all-done when all DONE or DEFERRED"
  (setup)
  (define plan-path (build-path test-dir ".planning" "PLAN.md"))
  (call-with-output-file plan-path (lambda (out) (display sample-plan-index out)) #:exists 'truncate)
  (update-wave-in-index! test-dir 0 "DONE")
  (update-wave-in-index! test-dir 1 "DONE")
  (update-wave-in-index! test-dir 2 "DEFERRED")
  (check-equal? (plan-overall-status test-dir) 'all-done)
  (teardown))

(test-case "plan-overall-status: partly-done with FAILED"
  (setup)
  (define plan-path (build-path test-dir ".planning" "PLAN.md"))
  (call-with-output-file plan-path (lambda (out) (display sample-plan-index out)) #:exists 'truncate)
  (update-wave-in-index! test-dir 0 "DONE")
  (update-wave-in-index! test-dir 1 "FAILED")
  (check-equal? (plan-overall-status test-dir) 'partly-done)
  (teardown))
