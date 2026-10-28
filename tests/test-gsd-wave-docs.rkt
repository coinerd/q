#lang racket/base
;; @covers extensions/gsd/wave-docs.rkt

;; @speed fast  ;; @suite extensions
;; @boundary integration

;; BOUNDARY: integration

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
         racket/list
         racket/string
         racket/path
         racket/port
         "../extensions/gsd/wave-docs.rkt"
         "../extensions/gsd/prompts.rkt"
         ;; W5: the lenient declared-wave-reference lexical grammar is owned by
         ;; planning-paths.rkt; wave-docs only re-exports the same binding.
         (only-in "../extensions/gsd/planning-paths.rkt"
                  declared-wave-doc-reference)
         ;; W5/R-019: the typed unknown-status rejection lives in wave-status.
         (only-in "../extensions/gsd/wave-status.rkt"
                  exn:fail:wave-status?
                  exn:fail:wave-status-value
                  wave-status-comparison-key))

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
  (string-append "# Plan: Fix all the bugs\n\n## Waves\n\n"
                 "- [Inbox] W0: Fix the bug → waves/W0-fix-the-bug.md\n"
                 "- [Inbox] W1: Add tests → waves/W1-add-tests.md\n"
                 "- [Inbox] W2: Update docs → waves/W2-update-docs.md\n"))

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

;; BUG-0081: a wave entry may deliberately route through another campaign's
;; W-numbered carrier doc; the declared arrow path is identity, not the
;; entry's own W number.
(test-case "parse-plan-index: declared arrow path is captured verbatim (BUG-0081)"
  (define plan
    (string-append
     "- [Inbox] W0: Tracker reconciliation → waves/W0-tracker-reconciliation-wiring.md\n"
     "- [Inbox] W2: Canary seven-link delivery → waves/W0-canary-seven-link-delivery.md\n"))
  (define entries (parse-plan-index plan))
  (check-equal? (wave-index-entry-ref-path (list-ref entries 0))
                "waves/W0-tracker-reconciliation-wiring.md")
  ;; Foreign numbering preserved: the carrier doc keeps its own W0 number.
  (check-equal? (wave-index-entry-ref-path (list-ref entries 1))
                "waves/W0-canary-seven-link-delivery.md")
  ;; The slug still follows the target so synthesized fallbacks agree.
  (check-equal? (wave-index-entry-slug (list-ref entries 1)) "canary-seven-link-delivery"))

(test-case "parse-plan-index: no arrow means no declared ref-path"
  (define entries (parse-plan-index "- [Inbox] W0: No arrow here\n"))
  (check-false (wave-index-entry-ref-path (car entries))))

(test-case "wave-entry-doc-path: declared path wins; synthesis is the fallback (BUG-0081)"
  (define entries
    (parse-plan-index (string-append "- [Inbox] W0: Tracker → waves/W0-tracker.md\n"
                                     "- [Inbox] W2: Carrier → waves/W0-carrier.md\n"
                                     "- [Inbox] W3: Arrowless wave\n")))
  (check-equal? (wave-entry-doc-path entries 0) "waves/W0-tracker.md")
  (check-equal? (wave-entry-doc-path entries 2) "waves/W0-carrier.md")
  (check-equal? (wave-entry-doc-path entries 3) "waves/W3-arrowless-wave.md")
  (check-false (wave-entry-doc-path entries 9)))

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
;; W5 (R-019): unknown statuses are typed rejections
;; ============================================================

(test-case "W5: next-inbox-wave rejects an unknown status instead of selecting it"
  (setup)
  (define plan-path (build-path test-dir ".planning" "PLAN.md"))
  (define plan-text
    (string-append "- [REWORK] W0: Unknown → waves/W0-unknown.md\n"
                   "- [Inbox] W1: Next → waves/W1-next.md\n"))
  (call-with-output-file plan-path (lambda (out) (display plan-text out)) #:exists 'truncate)
  ;; The historical `pending` fallback made W0 selectable (and dispatched).
  (check-exn exn:fail:wave-status?
             (lambda () (next-inbox-wave test-dir)))
  (define ex (with-handlers ([exn:fail:wave-status? values])
               (next-inbox-wave test-dir)))
  (check-equal? (exn:fail:wave-status-value ex) "REWORK")
  (check-true (string-contains? (exn-message ex) "wave selection refused"))
  (check-true (string-contains? (exn-message ex) "W0"))
  (teardown))

(test-case "W5: documented legacy aliases still select normally"
  (setup)
  (define plan-path (build-path test-dir ".planning" "PLAN.md"))
  ;; `[COMPLETED]` is DONE, `[PENDING]` is pending (documented legacy aliases).
  (define plan-text
    (string-append "- [COMPLETED] W0: Done → waves/W0-done.md\n"
                   "- [PENDING] W1: Next → waves/W1-next.md\n"))
  (call-with-output-file plan-path (lambda (out) (display plan-text out)) #:exists 'truncate)
  (define next (next-inbox-wave test-dir))
  (check-not-false next)
  (check-equal? (wave-index-entry-idx next) 1)
  (teardown))

(test-case "W5: unknown status is described, not coerced, by status precedence"
  ;; Precedence is a comparison: an unknown PLAN row status must NOT be
  ;; treated as `deferred` (the only row that wins), so the doc header stands.
  (check-equal? (resolve-status-precedence "DEFERRED" "DONE") "DEFERRED")
  (check-equal? (resolve-status-precedence "CANCELLED" "DONE") "DONE")
  (check-equal? (resolve-status-precedence "CANCELLED" "COMPLETED") "COMPLETED")
  ;; Comparison keys give an unknown spelling its own identity, so two
  ;; different typos never compare equal (the old `pending` fallback did).
  (check-not-equal? (wave-status-comparison-key "CANCELLED")
                    (wave-status-comparison-key "PENDING"))
  (check-equal? (wave-status-comparison-key "CANCELLED")
                (wave-status-comparison-key "cancelled"))
  (check-eq? (wave-status-comparison-key "COMPLETED") 'done))

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

;; ============================================================
;; T-03: parse-wave-doc-from-string pure extraction
;; ============================================================

(test-case "parse-wave-doc-from-string parses valid DONE wave"
  (define text "# Wave 0
Status: DONE
Some content here.")
  (define result (parse-wave-doc-from-string text 0 "W0" (string->path "/tmp/W0.md")))
  (check-not-false result)
  (check-equal? (hash-ref result 'status) "DONE")
  (check-equal? (hash-ref result 'slug) "W0")
  (check-equal? (hash-ref result 'index) 0)
  (check-equal? (hash-ref result 'path) "/tmp/W0.md"))

(test-case "parse-wave-doc-from-string parses wave without status"
  (define text "# Wave 1
Some description without status header.")
  (define result (parse-wave-doc-from-string text 1 "W1"))
  (check-not-false result)
  (check-equal? (hash-ref result 'status) "Inbox")
  (check-equal? (hash-ref result 'slug) "W1"))

(test-case "parse-wave-doc-from-string returns hash for empty input"
  (define result (parse-wave-doc-from-string "" 0 "W0"))
  (check-not-false result)
  ;; Empty input -> status defaults to Inbox
  (check-equal? (hash-ref result 'status) "Inbox"))

;; ============================================================
;; BUG-0095 (#9854): prompt-authored wave docs on disk. planning-write
;; stores content VERBATIM, so the exact mandatory template emitted by
;; planning-prompt is written RAW (no writer header prepended) and must
;; pass the file-backed campaign lint surfaces cleanly — for W0 and a
;; substituted nonzero wave (W2).
;; ============================================================

(define (bug-0095-string-index-of s sub [from 0])
  (define len (string-length sub))
  (let loop ([i from])
    (cond
      [(> (+ i len) (string-length s)) #f]
      [(string=? (substring s i (+ i len)) sub) i]
      [else (loop (add1 i))])))

;; The exact fenced mandatory template emitted by planning-prompt —
;; byte-for-byte, NO test-added repairs.
(define (bug-0095-template)
  (define p (planning-prompt "test"))
  (define m (regexp-match-positions #rx"### Wave Doc Format[^\n]*\n```\n" p))
  (unless m
    (error "wave-doc template fence missing from planning-prompt"))
  (define body-start (cdr (car m)))
  (define close (bug-0095-string-index-of p "\n```" body-start))
  (unless close
    (error "wave-doc template closing fence missing"))
  (substring p body-start close))

(define (bug-0095-count-lines s rx)
  (count (lambda (line) (regexp-match? rx line)) (string-split s "\n")))

;; Write one wave doc's exact bytes RAW (as planning-write would store
;; them) plus a PLAN.md index row for it, in an isolated root.
(define (bug-0095-author-doc! root idx slug raw)
  (define waves-dir (build-path root ".planning" "waves"))
  (unless (directory-exists? waves-dir)
    (make-directory* waves-dir))
  (call-with-output-file (build-path waves-dir (format "W~a-~a.md" idx slug))
                         (lambda (out) (display raw out))
                         #:exists 'truncate)
  (call-with-output-file
   (build-path root ".planning" "PLAN.md")
   (lambda (out) (fprintf out "- [Inbox] W~a: Short title → waves/W~a-~a.md\n" idx idx slug))
   #:exists 'append)
  (build-path waves-dir (format "W~a-~a.md" idx slug)))

(test-case "BUG-0095: verbatim W0 doc on disk passes campaign lint surfaces"
  (define root (make-temporary-file "wave-docs-0095-~a" 'directory))
  (define raw (bug-0095-template))
  (define path (bug-0095-author-doc! root 0 "short-title" raw))
  ;; Byte-exact storage: no writer header was prepended.
  (define on-disk (call-with-input-file path port->string))
  (check-equal? on-disk raw "doc bytes stored verbatim")
  (check-equal? (bug-0095-count-lines on-disk #rx"^# Wave [0-9]+$") 1 "single H1 on disk")
  (check-equal? (bug-0095-count-lines on-disk #px"^Status:") 1 "single Status line on disk")
  ;; File-backed canonical lint: zero violations, zero warnings.
  (check-equal? (lint-campaign-wave-docs root) '())
  (check-equal? (wave-doc-lint-warning-lines root) '())
  ;; Index row and doc header agree; no second body Status line.
  (check-equal? (check-status-consistency root) '())
  (delete-directory/files root))

(test-case "BUG-0095: verbatim W2 (substituted) doc on disk passes campaign lint surfaces"
  (define root (make-temporary-file "wave-docs-0095-~a" 'directory))
  (define raw (string-replace (bug-0095-template) "# Wave 0" "# Wave 2"))
  (define path (bug-0095-author-doc! root 2 "short-title" raw))
  (define on-disk (call-with-input-file path port->string))
  (check-equal? on-disk raw)
  (check-equal? (bug-0095-count-lines on-disk #rx"^# Wave [0-9]+$") 1 "single H1 on disk")
  (check-equal? (bug-0095-count-lines on-disk #px"^Status:") 1 "single Status line on disk")
  (check-equal? (lint-campaign-wave-docs root) '())
  (check-equal? (wave-doc-lint-warning-lines root) '())
  (check-equal? (check-status-consistency root) '())
  ;; And the read-back doc itself lints clean with a recognized Inbox header.
  (define doc (read-wave-doc root 2 "short-title"))
  (check-not-false doc)
  (check-true (hash-ref doc 'status-header?))
  (check-equal? (hash-ref doc 'status) "Inbox")
  (check-equal? (map wave-doc-violation-section (lint-wave-doc doc)) '())
  (delete-directory/files root))

;; ============================================================
;; BUG-0082: declared wave-path identity (status/consistency transitions)
;; ============================================================

(test-case "BUG-0082: mark-wave-status! writes the DECLARED doc, carrier header identity preserved"
  (define root (make-temp-planning-dir))
  (make-directory (build-path root ".planning" "waves"))
  (call-with-output-file
   (build-path root ".planning" "PLAN.md")
   (lambda (out)
     (display (string-append "# Plan\n\n## Waves\n"
                             "- [Inbox] W2: Carrier wave → waves/W0-identity-carrier.md\n")
              out))
   #:exists 'truncate)
  (call-with-output-file (build-path root ".planning" "waves" "W0-identity-carrier.md")
                         (lambda (out) (display "# Wave 0\nStatus: Inbox\n\ncarrier body\n" out))
                         #:exists 'truncate)
  (check-true (mark-wave-status! root 2 "DONE"))
  (define text
    (call-with-input-file (build-path root ".planning" "waves" "W0-identity-carrier.md")
                          port->string))
  ;; carrier path preserved, carrier header identity preserved (never
  ;; rewritten to the selected W-number), status dual-written
  (check-true (string-contains? text "# Wave 0\nStatus: DONE"))
  (check-false (string-contains? text "# Wave 2"))
  (check-true (string-contains? (call-with-input-file (build-path root ".planning" "PLAN.md")
                                                      port->string)
                                "[DONE] W2:"))
  ;; consistency reads the declared doc: row and header agree
  (check-equal? (check-status-consistency root) '())
  ;; effective status of the W2 row resolves through the carrier header
  (define entries
    (parse-plan-index (call-with-input-file (build-path root ".planning" "PLAN.md") port->string)))
  (check-equal? (effective-wave-status root (car entries)) "DONE")
  (delete-directory/files root #:must-exist? #f))

(test-case "BUG-0082: shared multi-entry carrier mutation refuses with zero file changes"
  (define root (make-temp-planning-dir))
  (make-directory (build-path root ".planning" "waves"))
  (call-with-output-file
   (build-path root ".planning" "PLAN.md")
   (lambda (out)
     (display (string-append "# Plan\n\n## Waves\n"
                             "- [Inbox] W0: carrier → waves/W0-shared-carrier.md\n"
                             "- [Inbox] W2: twin → waves/W0-shared-carrier.md\n")
              out))
   #:exists 'truncate)
  (call-with-output-file (build-path root ".planning" "waves" "W0-shared-carrier.md")
                         (lambda (out) (display "# Wave 0\nStatus: Inbox\n\ncarrier body\n" out))
                         #:exists 'truncate)
  (define (snapshot)
    (sort (for/list ([p (in-directory root)]
                     #:when (file-exists? p))
            (cons (path->string (find-relative-path root p)) (file->bytes p)))
          string<?
          #:key car))
  (define before (snapshot))
  (check-false (mark-wave-status! root 2 "DONE"))
  (check-false (mark-wave-status! root 0 "DONE"))
  (check-equal? (snapshot) before)
  ;; selection refuses the same mapping by name
  (check-exn (lambda (e)
               (and (exn:fail? e)
                    (string-contains? (exn-message e) ".planning/waves/W0-shared-carrier.md")))
             (lambda () (next-inbox-wave root)))
  (delete-directory/files root #:must-exist? #f))

(test-case "BUG-0082: unsafe declared arrow refuses by name; never synthesizes"
  (define root (make-temp-planning-dir))
  (make-directory (build-path root ".planning" "waves"))
  (call-with-output-file (build-path root ".planning" "PLAN.md")
                         (lambda (out)
                           (display (string-append "# Plan\n\n## Waves\n"
                                                   "- [Inbox] W0: escape → waves/../../evil.md\n")
                                    out))
                         #:exists 'truncate)
  (check-exn (lambda (e)
               (and (exn:fail? e)
                    (string-contains? (exn-message e) "waves/../../evil.md")
                    (string-contains? (exn-message e) "escapes")))
             (lambda () (read-declared-wave-doc root 0)))
  (delete-directory/files root #:must-exist? #f))

;; GSD-SIMPLIFICATION W1: the marker vocabulary is owned by wave-status.rkt;
;; wave-docs keeps only its historical accessor shape (no second table).
(test-case "W1: wave-status-markers delegates to the single shared vocabulary"
  (define markers (wave-status-markers))
  (check-equal? (assoc "Inbox" markers) (cons "Inbox" "[Inbox]"))
  (check-equal? (assoc "In-Progress" markers) (cons "In-Progress" "[In-Progress]"))
  (check-equal? (assoc "DONE" markers) (cons "DONE" "[DONE]"))
  (check-equal? (assoc "DEFERRED" markers) (cons "DEFERRED" "[DEFERRED]"))
  (check-equal? (assoc "FAILED" markers) (cons "FAILED" "[FAILED]"))
  ;; Legacy marker spellings are retained verbatim, not rewritten.
  (check-equal? (cdr (assoc "COMPLETED" markers)) "[COMPLETED]")
  (check-equal? (cdr (assoc "CANCELLED" markers)) "[CANCELLED]")
  (check-equal? (length markers) 7))

;; W5: the lenient lexical grammar for author-declared arrow targets moved to
;; planning-paths.rkt. wave-docs re-exports that ONE binding (no second
;; regexp/parser) and keeps the historical lenient behavior for nested names.
(test-case "W5: wave-ref-path-from-target is the planning-paths declared grammar"
  (check-eq? wave-ref-path-from-target declared-wave-doc-reference)
  (check-equal? (wave-ref-path-from-target "→ waves/custom/nested-legal.md")
                "waves/custom/nested-legal.md")
  (check-equal? (wave-ref-path-from-target "see waves/W0-fine.md for details")
                "waves/W0-fine.md"))
