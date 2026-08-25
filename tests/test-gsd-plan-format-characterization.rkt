#lang racket/base

;; tests/test-gsd-plan-format-characterization.rkt — GSD plan-format
;; characterization pins (v1.00.18 campaign d079a35e successor; BUG-0023,
;; BUG-0025).
;;
;; W0 pinned CURRENT behavior so later waves flip explicit reviewed pins:
;;   - BUG-0025 pin (clean-file-path kept "[NEW]" annotations) — FLIPPED
;;     by W1: annotated declarations now parse to clean paths.
;;   - BUG-0023 pins (table-format plan index parses to 0 entries;
;;     non-`W<n>-slug` wave docs load empty with no error) — still red,
;;     flipped by W2.
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
         (only-in "../extensions/gsd/plan-types.rkt" gsd-plan-waves plan-wave-ref gsd-wave-files)
         (only-in "../extensions/gsd/wave-docs.rkt"
                  parse-plan-index
                  wave-index-entry-idx
                  wave-index-entry-slug)
         (only-in "../extensions/gsd/wave-executor.rkt" load-plan-from-index))

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

    (test-case "wave doc referenced by a non-W<n>-slug name loads empty, no error (BUG-0023, W2 pin)"
      (define plan-text "# Plan: Odd Name

## Waves

- [Inbox] W0: setup → waves/notes.md
")
      (define tmp
        (make-tmp-planning plan-text
                           ;; The on-disk name does not follow W<n>-slug; read-wave-doc looks for
                           ;; W0-setup.md, misses, and the wave loads with empty content.
                           (list (list "notes.md" wave-doc-with-annotated-file))))
      (dynamic-wind void
                    (lambda ()
                      (define plan (load-plan-from-index tmp))
                      (check-not-false plan)
                      (define w (plan-wave-ref plan 0))
                      (check-not-false w)
                      ;; Empty content ⇒ no extracted files (BUG-0023 pin, flipped by W2).
                      (check-equal? (gsd-wave-files w) '()))
                    (lambda () (rm-tmp-planning tmp))))

    ;; ============================================================
    ;; run
    ;; ============================================================
    )) ;; end test-suite body

(module+ main
  (exit (run-tests (characterization-suite))))
