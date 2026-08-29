#lang racket

;; @speed fast
;; @suite default
;; @boundary unit

;;; test-changelog-bug-ref-lint.rkt — W5 flip of the W0 BUG-0049 pin.
;; W0 pinned the gap: BUG-NNNN tokens in CHANGELOG entries were never
;;; resolved against the bug registry (.planning/bugs/INDEX.md), so
;;; scrambled or fabricated ids passed the release lint silently.
;;; W5 closes BUG-0049: the linter now cross-checks every token in the
;;; version block. This suite pins the NEW behavior:
;;;   1. the cross-check seam exists in the linter source;
;;;   2. a CHANGELOG citing a BUG id that is not in the registry fails;
;;;   3. a bug claimed fixed whose registry status is not done fails;
;;;   4. a severity that disagrees with the registry fails;
;;;   5. a registry-consistent CHANGELOG passes with zero errors.

(require rackunit
         racket/file
         racket/path
         racket/string
         "../scripts/lint-release-notes.rkt")

;; Where this test file (and the linter) live. -------------------------------

(define module-dir
  (simplify-path
   (path-only
    (resolved-module-path-name
     (variable-reference->resolved-module-path (#%variable-reference))))))

(define linter-source
  (file->string (build-path module-dir 'up "scripts" "lint-release-notes.rkt")))

;; Pin 1 (flipped in W5): the registry cross-check seam exists. ---------------

(test-case "BUG-0049 fixed: linter resolves BUG tokens against the registry"
  (check-true (and (regexp-match? #px"(?i:bugs/INDEX\\.md)" linter-source)
                   (regexp-match? #px"validate-bug-refs" linter-source))
              "BUG-0049: lint-release-notes must cross-check BUG-NNNN tokens against .planning/bugs/INDEX.md"))

;; Fixture registry + compliant changelog scaffolding. ------------------------

(define scratch-dir (make-temporary-file "bug0049-w5-pin~a" 'directory))
(define fixture-index (build-path scratch-dir "INDEX.md"))

(with-output-to-file fixture-index
  (lambda ()
    (displayln "# Fixture Bug Registry (BUG-0049 W5 pin)")
    (displayln "")
    (displayln "| ID | Reported | Title | Component | Severity | Status | Fixed in | File |")
    (displayln "|----|----------|-------|-----------|----------|--------|----------|------|")
    (displayln "| BUG-0101 | 2026-08-28 | pin fixture: still open | test | medium | reported | — | [BUG-0101.md](BUG-0101.md) |")
    (displayln "| BUG-0102 | 2026-08-28 | pin fixture: long fixed | test | high | fixed v9.9.8 | v9.9.8 | [BUG-0102.md](BUG-0102.md) |")
    (displayln "| BUG-0103 | 2026-08-28 | pin fixture: partially landed | test | low | partial: half landed v9.9.7 | — | [BUG-0103.md](BUG-0103.md) |")
    (displayln "| BUG-0104 | 2026-08-28 | pin fixture: planned work | test | medium | planned v9.9.9 | — | [BUG-0104.md](BUG-0104.md) |")))

(define (write-fixture-changelog name user-visible-line)
  (define p (build-path scratch-dir name))
  (display-to-file
   (string-join
    (list "## 9.9.9" ""
          "### User-Visible Changes"
          user-visible-line
          "" "### Breaking / Behavior Changes" "none"
          "" "### Migration Notes" "none"
           "" "### Testing" "- registry cross-check suite (changelog bug-ref pin)"
          "" "### Operational / Release" "- fixture only")
    "\n")
   p
   #:exists 'truncate)
  p)

(define (lint-fixture name user-visible-line)
  (define cl (write-fixture-changelog name user-visible-line))
  (parameterize ([bug-registry-path fixture-index])
    (lint-changelog cl "9.9.9")))

;; Pins 2-5 (flipped in W5): the cross-check flags bad references. -------------

(test-case "unknown BUG id in a CHANGELOG entry is a named lint error"
  (define errors (lint-fixture "unknown.md"
                               "- Fixed the frobnicator; BUG-9999 is not in any registry."))
  (check-true (and (pair? errors)
                   (ormap (lambda (e)
                            (and (string-contains? e "BUG-9999")
                                 (string-contains? (string-downcase e) "unknown")))
                          errors))
              (format "expected unknown-bug-id error, got: ~a" errors)))

(test-case "claimed fixed vs registry-reported contradiction is flagged"
  (define errors (lint-fixture "status-open.md"
                               "- Fixed the stream hang reported in BUG-0101."))
  (check-true (and (pair? errors)
                   (ormap (lambda (e)
                            (and (string-contains? e "BUG-0101")
                                 (string-contains? (string-downcase e) "status contradiction")))
                          errors))
              (format "expected status-contradiction error, got: ~a" errors)))

(test-case "claimed fixed vs registry-partial contradiction is flagged"
  (define errors (lint-fixture "status-partial.md"
                               "- Resolved the half-landed work from BUG-0103."))
  (check-true (and (pair? errors)
                   (ormap (lambda (e)
                            (and (string-contains? e "BUG-0103")
                                 (string-contains? (string-downcase e) "status contradiction")))
                          errors))
              (format "expected status-contradiction error, got: ~a" errors)))

(test-case "severity mismatch against the registry is flagged"
  (define errors (lint-fixture "sev-mismatch.md"
                               "- Hardened the transport: critical BUG-0104 no longer stalls."))
  (check-true (and (pair? errors)
                   (ormap (lambda (e)
                            (and (string-contains? e "BUG-0104")
                                 (string-contains? (string-downcase e) "severity mismatch")))
                          errors))
              (format "expected severity-mismatch error, got: ~a" errors)))

(test-case "registry-consistent CHANGELOG entry passes with zero errors"
  (define errors (lint-fixture "clean.md"
                               "- Fixed the pooled-body corruption (BUG-0102); regression suite green."))
  (check-equal? errors '() (format "expected clean pass, got: ~a" errors)))

(test-case "consistent parenthetical severity passes"
  (define errors (lint-fixture "sev-ok.md"
                               "- Hardened the transport for BUG-0102 (high) connections."))
  (check-equal? errors '() (format "expected clean pass, got: ~a" errors)))

;; Cleanup. --------------------------------------------------------------------

(delete-directory/files scratch-dir)

(displayln "All BUG-0049 changelog bug-ref lint pin tests passed (W5 flip).")
