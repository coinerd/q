#lang racket/base

;; tests/test-gsd-plan-format-characterization.rkt — GSD plan-format
;; characterization pins (v1.00.18 campaign d079a35e successor; BUG-0023,
;; BUG-0025).
;;
;; W0 pinned CURRENT behavior so later waves flip explicit reviewed pins:
;;   - BUG-0025 pin (clean-file-path kept "[NEW]" annotations) — FLIPPED
;;     by W1: annotated declarations now parse to clean paths.
;;   - BUG-0023 pins — FLIPPED by W2: strict index validation raises
;;     naming the expected wave-doc path (no more silent empty waves);
;;     zero-waves /go rejection reports BOTH accepted formats.
;;
;; Table-format plans still parse to 0 index entries (parser unchanged —
;; the fix is diagnostics, not format tolerance).
;;
;; Zero-behavior-change guarantee for unannotated declarations is pinned
;; here too: every clean-file-path case without annotation prose must be
;; byte-identical to pre-W1 results.

(require racket/file
         racket/format
         racket/string
         rackunit
         rackunit/text-ui
         (only-in "../extensions/gsd/plan-types-parser.rkt" clean-file-path)
         (only-in "../extensions/gsd/plan-types.rkt"
                  gsd-plan
                  gsd-plan-waves
                  plan-wave-ref
                  gsd-wave-files
                  validation-result-errors)
         (only-in "../extensions/gsd/plan-validator.rkt" validate-plan-strict)
         (only-in "../extensions/gsd/wave-docs.rkt"
                  parse-plan-index
                  count-inline-wave-sections
                  wave-index-entry-idx
                  wave-index-entry-slug)
         (only-in "../extensions/gsd/wave-executor.rkt" load-plan-from-index))

;; errors containing needle — for asserting actionable diagnostics
(define (errors-containing errors needle)
  (filter (lambda (e) (string-contains? e needle)) errors))

;; ============================================================
;; shared fixtures & helpers (module level — a `define` inside the
;; all-tests `list` expression is a syntax error)
;; ============================================================

(define index-format-plan
  "# Plan: Test

## Waves

- [Inbox] W0: setup → waves/W0-setup.md
- [Inbox] W1: implement → waves/W1-implement.md
")

(define (make-tmp-planning plan-text wave-docs)
  ;; wave-docs: (listof (list filename content)) written under
  ;; <tmp>/.planning/waves/.
  (define tmp (make-temporary-file "gsd-char-~a" 'directory))
  (make-directory* (build-path tmp ".planning" "waves"))
  (call-with-output-file (build-path tmp ".planning" "PLAN.md") (lambda (o) (display plan-text o)))
  (for ([d (in-list wave-docs)])
    (call-with-output-file (build-path tmp ".planning" "waves" (car d))
                           (lambda (o) (display (cadr d) o))))
  tmp)

(define (rm-tmp-planning tmp)
  (delete-directory/files tmp #:must-exist? #f))

(define wave-doc-with-annotated-file
  "# Wave 0: from the index

Status: Inbox

## Files

- File: q/from-index.rkt  [NEW]

## Action

Do the thing.
")

;; ============================================================
;; clean-file-path: BUG-0025 (flipped by W1)
;; ============================================================

(define (characterization-suite)
  (test-suite "plan-format-characterization"

    (test-case "clean-file-path strips trailing [NEW] annotations (BUG-0025, W1 flip)"
      (check-equal? (clean-file-path "q/tests/foo.rkt  [NEW]") "q/tests/foo.rkt")
      (check-equal? (clean-file-path "q/tests/foo.rkt [NEW]") "q/tests/foo.rkt"))

    (test-case "clean-file-path strips [NEW, design record] style annotations"
      (check-equal? (clean-file-path "q/docs/design.md [NEW, design record]") "q/docs/design.md")
      (check-equal? (clean-file-path "q/docs/design.md  [design record]") "q/docs/design.md"))

    (test-case "clean-file-path handles combined backtick + annotation forms (W1)"
      (check-equal? (clean-file-path "`q/foo.rkt` [NEW]") "q/foo.rkt")
      (check-equal? (clean-file-path "```q/foo.rkt``` [NEW]") "q/foo.rkt")
      (check-equal? (clean-file-path "`q/foo.rkt  [NEW]`") "q/foo.rkt"))

    (test-case "clean-file-path strips trailing parenthetical annotations (pre-existing)"
      (check-equal? (clean-file-path "q/docs/x.md (new: evidence log)") "q/docs/x.md")
      (check-equal? (clean-file-path "q/x.md (new: a, b) [NEW]") "q/x.md"))

    (test-case "clean-file-path keeps interior brackets and parens intact"
      (check-equal? (clean-file-path "q/src/a[b]c.rkt") "q/src/a[b]c.rkt")
      (check-equal? (clean-file-path "q/foo (bar)/x.rkt") "q/foo (bar)/x.rkt"))

    (test-case "clean-file-path: unannotated declarations behave exactly as before W1"
      (check-equal? (clean-file-path "q/tests/foo.rkt") "q/tests/foo.rkt")
      (check-equal? (clean-file-path " q/tests/foo.rkt ") "q/tests/foo.rkt")
      (check-equal? (clean-file-path "`q/tests/foo.rkt`") "q/tests/foo.rkt")
      (check-equal? (clean-file-path "```q/tests/foo.rkt```") "q/tests/foo.rkt"))

    ;; ============================================================
    ;; parse-plan-index format characterization
    ;; ============================================================

    (test-case "parse-plan-index extracts bullet-index entries with idx/slug (live format)"
      (define entries (parse-plan-index index-format-plan))
      (check-equal? (length entries) 2)
      (check-equal? (wave-index-entry-idx (car entries)) 0)
      (check-equal? (wave-index-entry-slug (car entries)) "setup")
      (check-equal? (wave-index-entry-idx (cadr entries)) 1)
      (check-equal? (wave-index-entry-slug (cadr entries)) "implement"))

    (test-case "parse-plan-index returns 0 entries for a table-format plan (BUG-0023, W2 pin)"
      (define table-plan
        "# Plan: Test

## Waves

| Wave | Status | Doc |
|------|--------|-----|
| 0 | Inbox | waves/W0-setup.md |
| 1 | Inbox | waves/W1-implement.md |
")
      (check-equal? (parse-plan-index table-plan) '()))

    (test-case "parse-plan-index returns 0 entries when the Waves section has no bullets"
      (check-equal? (parse-plan-index "# Plan: T\n\n## Waves\n\ntext only\n") '()))

    ;; ============================================================
    ;; load-plan-from-index precedence
    ;; ============================================================

    (test-case "load-plan-from-index prefers index entries over inline ## Wave sections"
      (define plan-text
        "# Plan: Precedence

## Waves

- [Inbox] W0: idxslug → waves/W0-idxslug.md

## Wave 0: inline section that must NOT win

## Files

- File: q/from-inline.rkt
")
      (define tmp
        (make-tmp-planning plan-text (list (list "W0-idxslug.md" wave-doc-with-annotated-file))))
      (dynamic-wind void
                    (lambda ()
                      (define plan (load-plan-from-index tmp))
                      (check-not-false plan)
                      (check-equal? (length (gsd-plan-waves plan)) 1)
                      ;; INDEX wins: the file comes from waves/W0-idxslug.md, not the inline
                      ;; section; and the annotated declaration parses CLEAN (W1 flip).
                      (check-equal? (gsd-wave-files (plan-wave-ref plan 0)) '("q/from-index.rkt")))
                    (lambda () (rm-tmp-planning tmp))))

    (test-case "load-plan-from-index returns #f when the index is empty (inline-fallback trigger)"
      (define plan-text
        "# Plan: Inline Only

## Wave 0: inline

## Files

- File: q/from-inline.rkt
")
      (define tmp (make-tmp-planning plan-text '()))
      (dynamic-wind void
                    (lambda ()
                      ;; Precedence pin: with an empty index the loader does not silently
                      ;; fall back to inline sections here — it signals #f and the caller
                      ;; decides. (Inline fallback lives above this layer.)
                      (check-false (load-plan-from-index tmp)))
                    (lambda () (rm-tmp-planning tmp))))

    (test-case "non-W<n>-slug wave-doc target now raises, naming the expected path (BUG-0023, W2 flip)"
      (define plan-text "# Plan: Odd Name

## Waves

- [Inbox] W0: setup → waves/notes.md
")
      (define tmp
        (make-tmp-planning plan-text
                           ;; The on-disk name does not follow W<n>-slug; the loader expects
                           ;; W0-setup.md, so strict index validation must ERROR naming that
                           ;; path (previously: silent empty content).
                           (list (list "notes.md" wave-doc-with-annotated-file))))
      (dynamic-wind void
                    (lambda ()
                      (define msg
                        (with-handlers ([exn:fail? exn-message])
                          (load-plan-from-index tmp)
                          "no error raised — BUG-0023 regression: silent empty wave"))
                      (check-true (string-contains? msg ".planning/waves/W0-setup.md")
                                  "error must name the expected wave-doc path")
                      (check-true (string-contains? msg "W<idx>-<slug>.md")
                                  "error must state the filename convention"))
                    (lambda () (rm-tmp-planning tmp))))

    (test-case "index entry whose target doc does not exist is a validation error (BUG-0023, W2)"
      (define plan-text
        "# Plan: Missing Target

## Waves

- [Inbox] W0: gone → waves/W0-gone.md
- [Inbox] W1: present → waves/W1-present.md
")
      (define tmp
        (make-tmp-planning plan-text
                           ;; Only W1's doc exists; W0's is missing.
                           (list (list "W1-present.md" wave-doc-with-annotated-file))))
      (dynamic-wind void
                    (lambda ()
                      (define msg
                        (with-handlers ([exn:fail? exn-message])
                          (load-plan-from-index tmp)
                          "no error raised — BUG-0023 regression: missing target silently empty"))
                      (check-true (string-contains? msg ".planning/waves/W0-gone.md"))
                      ;; The existing doc must NOT be reported missing.
                      (check-false (string-contains? msg "W1-present.md is missing")))
                    (lambda () (rm-tmp-planning tmp))))
    (test-case "table-format plan yields an actionable zero-waves /go diagnostic (BUG-0023, W2)"
      (define table-plan
        "# Plan: Test

## Waves

| Wave | Status | Doc |
|------|--------|-----|
| 0 | Inbox | waves/W0-setup.md |
| 1 | Inbox | waves/W1-implement.md |
")
      ;; Same path /go takes: 0 index entries ⇒ loader #f ⇒ inline parse ⇒ 0 waves.
      (check-equal? (parse-plan-index table-plan) '())
      (check-equal? (count-inline-wave-sections table-plan) 0)
      (define result (validate-plan-strict (gsd-plan '() #f '() '())))
      (define errs (validation-result-errors result))
      ;; Legacy first error kept verbatim (compat), then the actionable companion.
      (check-not-false (member "Plan has no waves" errs))
      (define diag (errors-containing errs "found 0 index entries"))
      (check-equal? (length diag) 1)
      (check-true (string-contains? (car diag) "`- [Inbox] W0: Title → waves/W0-slug.md`")
                  "must skeleton the index format")
      (check-true (string-contains? (car diag) "0 inline `## Wave N:` sections")
                  "must report the inline-format count")
      (check-true (string-contains? (car diag) "`## Wave 0: Title`")
                  "must skeleton the inline format"))

    (test-case "parser-provenance counts: hybrid and inline-only texts (BUG-0023, W2)"
      (check-equal? (count-inline-wave-sections index-format-plan) 0)
      (check-equal? (count-inline-wave-sections "## Wave 0: a\nbody\n## Wave 1: b\n") 2)
      (check-equal? (count-inline-wave-sections "## wave 2: lower\n") 1)
      (check-equal? (count-inline-wave-sections "### Wave 3: h3 not counted\n") 0))

    (test-case "valid index + inline hybrid keeps precedence and loads all docs (W2 regression)"
      (define plan-text
        "# Plan: Hybrid

## Waves

- [Inbox] W0: idxslug → waves/W0-idxslug.md
- [Inbox] W1: second → waves/W1-second.md

## Wave 0: inline section that must NOT win
")
      (define tmp
        (make-tmp-planning
         plan-text
         (list
          (list "W0-idxslug.md" wave-doc-with-annotated-file)
          (list
           "W1-second.md"
           "# Wave 1: from the index

Status: Inbox

## Files

- File: q/from-w1.rkt

## Action

Second.
"))))
      (dynamic-wind void
                    (lambda ()
                      (define plan (load-plan-from-index tmp))
                      (check-not-false plan)
                      (check-equal? (length (gsd-plan-waves plan)) 2)
                      ;; INDEX wins for W0 even though an inline W0 section exists.
                      (check-equal? (gsd-wave-files (plan-wave-ref plan 0)) '("q/from-index.rkt"))
                      (check-equal? (gsd-wave-files (plan-wave-ref plan 1)) '("q/from-w1.rkt")))
                    (lambda () (rm-tmp-planning tmp))))

    ;; ============================================================
    ;; run
    ;; ============================================================
    )) ;; end test-suite body

(module+ main
  (exit (run-tests (characterization-suite))))
