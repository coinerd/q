#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;;; test-lint-release-notes.rkt — Tests for lint-release-notes script.

(require rackunit
         racket/string
         "../scripts/lint-release-notes.rkt")

;; ===========================================================================
;; Helpers
;; ===========================================================================

(define (changelog . entries)
  (string-join entries "\n"))

(define valid-entry
  (changelog "## 0.5.0"
             ""
             "### User-Visible Changes"
             "- Added new foo command"
             ""
             "### Breaking / Behavior Changes"
             ""
             "### Migration Notes"
             ""
             "### Testing"
             "- test-foo.rkt: 12 new tests"
             ""
             "### Operational / Release"
             "- Bumped version to 0.5.0"))

(define valid-entry-alt-headers
  (changelog "## 0.3.0"
             ""
             "### Features"
             "- Cool feature"
             ""
             "### Bug Fixes"
             "- Fixed crash"
             ""
             "### Breaking / Behavior Changes"
             ""
             "### Migration Notes"
             ""
             "### Testing"
             "- test-bar.rkt added"
             ""
             "### Operational / Release"
             "- CI tweak"))

;; ===========================================================================
;; Test 1: Valid release notes pass
;; ===========================================================================

(test-case "test-lint-release-notes: checks block 11"
  (check-equal? (validate-release-notes (extract-version-block valid-entry "0.5.0"))
                '()
                "valid release notes should produce zero errors"))

;; ===========================================================================
;; Test 2: Missing required section fails
;; ===========================================================================

(test-case "test-lint-release-notes: checks block 10"
  (check-not-false
   (let ([errors (validate-release-notes (changelog "## 0.1.0" "" "### Features" "- stuff"))])
     (and (> (length errors) 0)
          (ormap (λ (e) (string-contains? e "Missing required section")) errors)))
   "missing required sections should produce errors"))

;; ===========================================================================
;; Test 3: Empty sections are ok (header present, no content)
;; ===========================================================================

(test-case "test-lint-release-notes: checks block 9"
  (check-equal? (validate-release-notes (extract-version-block valid-entry "0.5.0"))
                '()
                "empty section bodies are fine as long as headers exist"))

;; ===========================================================================
;; Test 4: Version not found in changelog fails
;; ===========================================================================

(test-case "test-lint-release-notes: checks block 8"
  (check-false (extract-version-block valid-entry "99.99.99") "version not present returns #f")
  (check-not-false (let ([block (extract-version-block valid-entry "99.99.99")]) (not block))
                   "non-existent version block returns #f"))

;; ===========================================================================
;; Test 5: Multiple version entries — only check the requested one
;; ===========================================================================

(define multi-version-changelog
  (string-append (changelog "## 0.4.0"
                            ""
                            "### Features"
                            "- Old feature"
                            ""
                            "### Breaking / Behavior Changes"
                            ""
                            "### Migration Notes"
                            ""
                            "### Testing"
                            "- test-old.rkt"
                            ""
                            "### Operational / Release"
                            "- v0.4.0")
                 "\n"
                 valid-entry))

(test-case "test-lint-release-notes: checks block 7"
  (check-equal? (validate-release-notes (extract-version-block multi-version-changelog "0.5.0"))
                '()
                "only the requested version is validated — 0.5.0 is valid"))

;; The 0.4.0 entry is also valid; verify it separately
(test-case "test-lint-release-notes: checks block 6"
  (check-equal? (validate-release-notes (extract-version-block multi-version-changelog "0.4.0"))
                '()
                "version 0.4.0 is also valid in multi-version changelog"))

;; ===========================================================================
;; Test 6: Case-insensitive section headers
;; ===========================================================================

(define uppercase-entry
  (changelog "## 1.0.0"
             ""
             "### USER-VISIBLE CHANGES"
             "- Big release"
             ""
             "### BREAKING / BEHAVIOR CHANGES"
             ""
             "### MIGRATION NOTES"
             ""
             "### TESTING"
             "- Full suite"
             ""
             "### OPERATIONAL / RELEASE"
             "- Tagged v1.0.0"))

(test-case "test-lint-release-notes: checks block 5"
  (check-equal? (validate-release-notes (extract-version-block uppercase-entry "1.0.0"))
                '()
                "case-insensitive header matching"))

;; Mixed case
(define mixed-case-entry
  (changelog "## 1.1.0"
             ""
             "### user-visible changes"
             "- Mixed case"
             ""
             "### Breaking / Behavior Changes"
             ""
             "### migration notes"
             ""
             "### testing"
             "- mixed"
             ""
             "### Operational / Release"
             "- misc"))

(test-case "test-lint-release-notes: checks block 4"
  (check-equal? (validate-release-notes (extract-version-block mixed-case-entry "1.1.0"))
                '()
                "mixed case headers accepted"))

;; ===========================================================================
;; Test 7: Extra sections are fine
;; ===========================================================================

(define extra-sections-entry
  (changelog "## 0.6.0"
             ""
             "### User-Visible Changes"
             "- Something"
             ""
             "### Breaking / Behavior Changes"
             ""
             "### Migration Notes"
             ""
             "### Testing"
             "- 5 tests"
             ""
             "### Operational / Release"
             "- bump"
             ""
             "### Contributors"
             "- Thanks everyone!"
             ""
             "### Internal Refactoring"
             "- Cleaned up bar module"))

(test-case "test-lint-release-notes: checks block 3"
  (check-equal? (validate-release-notes (extract-version-block extra-sections-entry "0.6.0"))
                '()
                "extra sections beyond the required ones are fine"))

;; ===========================================================================
;; Test 8: --check mode / exit code behavior
;; ===========================================================================

;; A bad block should produce errors that would trigger exit 1
(let ([errors (validate-release-notes (changelog "### Features" "- only features, nothing else"))])
  (check-true (> (length errors) 0) "bad block produces errors (would cause exit 1 with --check)"))

;; Good block produces no errors (would NOT exit 1)
(let ([errors (validate-release-notes (extract-version-block valid-entry "0.5.0"))])
  (check-equal? errors '() "good block produces no errors (exit 0)"))

;; ===========================================================================
;; Test: "v" prefix accepted in version
;; ===========================================================================

(define v-prefix-entry
  (changelog "## v2.0.0"
             ""
             "### Features"
             "- v-prefix"
             ""
             "### Breaking / Behavior Changes"
             ""
             "### Migration Notes"
             ""
             "### Testing"
             "- 1 test"
             ""
             "### Operational / Release"
             "- tagged"))

(test-case "test-lint-release-notes: checks block 2"
  (check-equal? (validate-release-notes (extract-version-block v-prefix-entry "2.0.0"))
                '()
                "version with 'v' prefix in heading matched by plain number"))

;; ===========================================================================
;; Test: Features + Bug Fixes together is fine
;; ===========================================================================

(test-case "test-lint-release-notes: checks block 1"
  (check-equal? (validate-release-notes (extract-version-block valid-entry-alt-headers "0.3.0"))
                '()
                "Features + Bug Fixes (without User-Visible Changes) is acceptable"))

;; ===========================================================================
;; v0.99.75 W3: exact release-entry parsing
;; ===========================================================================

(test-case "release truth rejects former cwd and count terminology"
  (define base
    (string-append "### User-Visible Changes\n"
                   "### Breaking / Behavior Changes\n"
                   "### Migration Notes\n"
                   "### Testing\n"
                   "### Operational / Release\n"))
  (check-not-false (member "False cwd contract: 'working directory is always canonical'"
                           (validate-release-notes
                            (string-append base "working directory is always canonical\n"))))
  (check-not-false (member "Unreconciled test-count terminology"
                           (validate-release-notes
                            (string-append base "All 45 of 45 failing test files pass.\n")))))

(test-case "release truth requires TS7 and ledger for defined count populations"
  (define base
    (string-append
     "### User-Visible Changes\nchange\n"
     "### Breaking / Behavior Changes\nnone\n"
     "### Migration Notes\nnone\n"
     "### Testing\nauthoritative 53-file baseline; release-tracked 45-file set; 44 passing.\n"
     "### Operational / Release\n"))
  (check-not-false (member "Unreconciled test-count terminology" (validate-release-notes base)))
  (check-not-false (member "Unreconciled test-count terminology"
                           (validate-release-notes
                            (string-append "### User-Visible Changes\nchange\n"
                                           "### Breaking / Behavior Changes\nnone\n"
                                           "### Migration Notes\nnone\n"
                                           "### Testing\nB53, T45, and P44.\n"
                                           "### Operational / Release\n"))))
  (check-not-false (member "Unreconciled test-count terminology"
                           (validate-release-notes
                            (string-append base
                                           "See docs/reports/v0.99.75-W0-EVIDENCE-FREEZE.md.\n"))))
  (check-not-false (member "Unreconciled test-count terminology"
                           (validate-release-notes (string-append base "remaining TS7.\n")))))

(test-case "release truth accepts defined B53/T45/P44/TS7 terminology"
  (define block
    (string-append
     "### User-Visible Changes\nexplicit invocation cwd takes precedence; execution-context cwd is the fallback; failures surface visibly.\n"
     "### Breaking / Behavior Changes\nnone\n"
     "### Migration Notes\nnone\n"
     "### Testing\nauthoritative 53-file baseline; later release-tracked 45-file set; 44 passing; remaining TS7.\n"
     "### Operational / Release\nSee docs/reports/v0.99.75-W0-EVIDENCE-FREEZE.md.\n"))
  (check-equal? (validate-release-notes block) '()))

(test-case "release-note extraction accepts canonical release metadata"
  (define text (changelog "## v1.2.3 — 2026-07-29" "body" "## 1.2.2" "historical"))
  (check-equal? (string-trim (extract-version-block text "1.2.3")) "body"))

(test-case "release-note extraction rejects partial and duplicate targets"
  (check-false (extract-version-block "## 1.2.30\nbody" "1.2.3"))
  (check-false (extract-version-block "## 1.2.3\nfirst\n## v1.2.3 — 2026-07-29\nsecond" "1.2.3")))

(test-case "historical release entries delimit the selected block"
  (define text
    (changelog "## 1.2.3 (Released 2026-07-29)" "current" "## v1.2.2 — Unreleased" "historical"))
  (check-equal? (string-trim (extract-version-block text "1.2.3")) "current"))

;; ===========================================================================
;; Summary
;; ===========================================================================

(printf "\nAll lint-release-notes tests completed.\n")

;; ===========================================================================
;; Test: pre-release version headings (e.g. 1.00.00-PRE1) are supported
;; ===========================================================================

(test-case "test-lint-release-notes: pre-release version heading validates"
  (define prerelease-entry
    (changelog "## 1.00.00-PRE1"
               ""
               "### Bug Fixes"
               "- fixed thing"
               ""
               "### Breaking / Behavior Changes"
               ""
               "### Migration Notes"
               ""
               "### Testing"
               "- tests"
               ""
               "### Operational / Release"
               "- stamp"))
  (check-equal? (validate-release-notes (extract-version-block prerelease-entry "1.00.00-PRE1"))
                '()
                "pre-release version heading should validate cleanly")
  ;; The pre-release heading must not be captured as its bare base version.
  (check-false (extract-version-block prerelease-entry "1.00.00")
               "base version alone must not match the pre-release heading"))
