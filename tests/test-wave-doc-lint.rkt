#lang racket/base

;; @suite gsd
;; @speed fast
;; @boundary pure
;; tests/test-wave-doc-lint.rkt
;; BUG-0041 (v1.00.21 W4): wave-doc lint at /go entry with recorded
;; verdicts. W0 pinned the ABSENCE of the seam; this suite verifies the
;; FLIP: lint-wave-doc names missing sections, /go warns per doc,
;; the verdict is durable campaign evidence, and slug mismatches ride
;; the v1.00.20 W2 consistency surface.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string
         racket/runtime-path
         "../extensions/gsd/wave-docs.rkt"
         "../extensions/gsd/campaign-repository.rkt")

(define-runtime-path wave-docs-src "../extensions/gsd/wave-docs.rkt")
(define-runtime-path command-handlers-src "../extensions/gsd/command-handlers.rkt")

;; ── Fixtures ──────────────────────────────────────────────────

;; Base .planning skeleton; doc-body writes W0-no-sections.md.
(define (make-planning-with-doc doc-body)
  (define dir (make-temporary-file "w4-lint~a" 'directory))
  (make-directory* (build-path dir ".planning" "waves"))
  (with-output-to-file (build-path dir ".planning" "PLAN.md")
                       (lambda ()
                         (displayln "# Plan: Lint characterization")
                         (newline)
                         (displayln "- [Inbox] W0: Doc with no sections → waves/W0-no-sections.md"))
                       #:exists 'replace)
  (with-output-to-file (build-path dir ".planning" "waves" "W0-no-sections.md")
                       doc-body
                       #:exists 'replace)
  dir)

;; Doc with a canonical status but NO Files/Verify/Done sections.
(define (make-temp-planning)
  (make-planning-with-doc (lambda ()
                            (displayln "# Wave 0")
                            (displayln "Status: Inbox")
                            (newline)
                            (displayln "## Goal")
                            (newline)
                            (displayln "Missing ## Files, ## Verify, and ## Done entirely."))))

;; Doc with sections intact but a non-canonical status header.
(define (make-temp-planning-bad-status)
  (make-planning-with-doc (lambda ()
                            (displayln "# Wave 0")
                            (displayln "Status: Bogus")
                            (newline)
                            (displayln "## Files")
                            (displayln "- File: q/src/a.rkt")
                            (newline)
                            (displayln "## Verify")
                            (displayln "cd q && raco test tests/a-test.rkt")
                            (newline)
                            (displayln "## Done")
                            (displayln "Change is merged and reviewed."))))

;; Doc satisfying the entire executor contract.
(define (make-temp-planning-clean)
  (make-planning-with-doc (lambda ()
                            (displayln "# Wave 0")
                            (displayln "Status: PENDING")
                            (newline)
                            (displayln "## Files")
                            (displayln "- File: q/src/a.rkt")
                            (newline)
                            (displayln "## Verify")
                            (displayln "cd q && raco test tests/a-test.rkt")
                            (newline)
                            (displayln "## Done")
                            (displayln "Change is merged and reviewed."))))

;; PLAN.md arrow slug disagrees with the on-disk filename slug.
(define (make-temp-planning-slug-mismatch)
  (define dir (make-temporary-file "w4-slug~a" 'directory))
  (make-directory* (build-path dir ".planning" "waves"))
  (with-output-to-file (build-path dir ".planning" "PLAN.md")
                       (lambda ()
                         (displayln "# Plan: Slug mismatch")
                         (newline)
                         (displayln "- [Inbox] W0: Index name → waves/W0-index-slug.md"))
                       #:exists 'replace)
  (with-output-to-file (build-path dir ".planning" "waves" "W0-disk-slug.md")
                       (lambda ()
                         (displayln "# Wave 0")
                         (displayln "Status: Inbox")
                         (newline)
                         (displayln "## Goal")
                         (displayln "Slug disagrees with the arrow target."))
                       #:exists 'replace)
  dir)

(define (cleanup! dir)
  (delete-directory/files dir #:must-exist? #f))

;; Pull a lint-verdict.rktd apart. Entry shape:
;; (wave <idx> doc-path <path> violations (<section> ...)).
(define (verdict-tag v)
  (car v))
(define (verdict-waves v)
  (caddr v))
(define (verdict-violations-for wave-idx verdict)
  (define entry
    (for/or ([w (in-list (verdict-waves verdict))]
             #:when (= (cadr w) wave-idx))
      w))
  (and entry (list-ref entry 5)))

(define (test-doc-lint dir) ; lint warnings per doc
  (wave-doc-lint-warning-lines dir))

(define w4-lint-suite
  (test-suite "BUG-0041 W4: wave-doc lint at /go entry with recorded verdicts"

    (test-case "doc missing Files/Verify/Done yields three named violations"
      (define dir (make-temp-planning))
      (dynamic-wind
       void
       (lambda ()
         (define doc (read-wave-doc dir 0 "no-sections"))
         (define vs (lint-wave-doc doc))
         (check-equal? (map wave-doc-violation-section vs)
                       '(files verify done)
                       "each missing section is named")
         (for ([v (in-list vs)])
           (check-equal? (wave-doc-violation-wave-idx v) 0 "violation names the wave index")
           (check-pred string? (wave-doc-violation-doc-path v))
           (check-true (string-contains? (wave-doc-violation-doc-path v) "W0-no-sections.md")
                       "violation names the doc path")))
       (lambda () (cleanup! dir))))

    (test-case "non-canonical status header is its own named violation"
      (define dir (make-temp-planning-bad-status))
      (dynamic-wind void
                    (lambda ()
                      (define doc (read-wave-doc dir 0 "no-sections"))
                      (check-equal? (map wave-doc-violation-section (lint-wave-doc doc))
                                    '(status-header)
                                    "sections are intact; only the header is faulted"))
                    (lambda () (cleanup! dir))))

    (test-case "clean docs are silent"
      (define dir (make-temp-planning-clean))
      (dynamic-wind void
                    (lambda ()
                      (check-equal? (lint-wave-doc (read-wave-doc dir 0 "no-sections")) '())
                      (check-equal? (lint-campaign-wave-docs dir) '())
                      (check-equal? (test-doc-lint dir) '()))
                    (lambda () (cleanup! dir))))

    (test-case "one named warning per doc at /go entry; warnings never block"
      (define dir (make-temp-planning))
      (dynamic-wind
       void
       (lambda ()
         (define lines (test-doc-lint dir))
         (check-equal? (length lines) 1 "exactly one warning per doc")
         (check-true (string-contains? (car lines) "W0") "warning names the wave")
         (check-true (string-contains? (car lines) "Files") "warning names the missing section")
         ;; advisory only: the /go load path still succeeds
         (define vals (call-with-values (lambda () (load-or-migrate-campaign! dir)) list))
         (check-equal? (length vals) 1 "single result value")
         (check-true (struct? (car vals)) "migrate + load succeeds despite lint warnings"))
       (lambda () (cleanup! dir))))

    (test-case "lint verdict is stored as durable campaign evidence at creation"
      (define dir (make-temp-planning))
      (dynamic-wind
       void
       (lambda ()
         (check-true (store-wave-doc-lint-verdict! dir "testplan") "first store succeeds")
         (define dest (build-path dir ".planning" "campaigns" "testplan" "lint-verdict.rktd"))
         (check-equal? (file-exists? dest) #t "verdict file is on disk")
         (define verdict (call-with-input-file dest read))
         (check-equal? (verdict-tag verdict) 'wave-doc-lint-verdict)
         (check-equal? (length (verdict-waves verdict)) 1 "one entry per referenced wave doc")
         (check-equal? (verdict-violations-for 0 verdict)
                       '(files verify done)
                       "verdict records the named violations")
         (check-false (store-wave-doc-lint-verdict! dir "testplan")
                      "write-once: a second store does not overwrite")
         (check-equal? (call-with-input-file dest read)
                       verdict
                       "verdict content is unchanged by the retry"))
       (lambda () (cleanup! dir))))

    (test-case "clean campaign verdict records zero violations"
      (define dir (make-temp-planning-clean))
      (dynamic-wind void
                    (lambda ()
                      (check-true (store-wave-doc-lint-verdict! dir "testplan"))
                      (define verdict
                        (call-with-input-file
                         (build-path dir ".planning" "campaigns" "testplan" "lint-verdict.rktd")
                         read))
                      (check-equal? (verdict-violations-for 0 verdict) '()))
                    (lambda () (cleanup! dir))))

    (test-case "slug mismatch is reported through the consistency surface"
      (define dir (make-temp-planning-slug-mismatch))
      (dynamic-wind void
                    (lambda ()
                      (define ms (check-slug-consistency dir))
                      (check-equal? (length ms) 1)
                      (define m (car ms))
                      (check-equal? (slug-mismatch-wave-idx m) 0)
                      (check-equal? (slug-mismatch-arrow-slug m) "index-slug")
                      (check-equal? (slug-mismatch-disk-slug m) "disk-slug")
                      (define lines (slug-mismatch-warning-lines dir))
                      (check-equal? (length lines) 1)
                      (check-true (and (string-contains? (car lines) "index-slug")
                                       (string-contains? (car lines) "disk-slug"))
                                  "warning names both spellings"))
                    (lambda () (cleanup! dir))))

    (test-case "the lint seam now exists in the plan tooling (W0 pin flipped)"
      (check-equal? (file-exists? wave-docs-src) #t)
      (check-true (string-contains? (file->string wave-docs-src) "lint-wave-doc")
                  "wave-docs.rkt carries lint-wave-doc next to the consistency checker"))

    (test-case "executor prompt carries the lint verdict (/go wiring)"
      (check-equal? (file-exists? command-handlers-src) #t)
      (define src (file->string command-handlers-src))
      (check-true (string-contains? src "wave-doc-lint-warning-lines")
                  "/go surfaces one named lint warning per doc")
      (check-true (string-contains? src "store-wave-doc-lint-verdict!")
                  "/go persists the lint verdict at campaign creation")
      (check-true (string-contains? src "slug-mismatch-warning-lines")
                  "slug mismatches ride the same advisory surface"))))

(module+ test
  (run-tests w4-lint-suite))

(module+ main
  (unless (zero? (run-tests w4-lint-suite))
    (exit 1)))
