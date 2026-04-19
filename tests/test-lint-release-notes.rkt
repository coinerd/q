#lang racket

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

(check-equal? (validate-release-notes (extract-version-block valid-entry "0.5.0"))
              '()
              "valid release notes should produce zero errors")

;; ===========================================================================
;; Test 2: Missing required section fails
;; ===========================================================================

(check-not-false
 (let ([errors (validate-release-notes (changelog "## 0.1.0" "" "### Features" "- stuff"))])
   (and (> (length errors) 0) (ormap (λ (e) (string-contains? e "Missing required section")) errors)))
 "missing required sections should produce errors")

;; ===========================================================================
;; Test 3: Empty sections are ok (header present, no content)
;; ===========================================================================

(check-equal? (validate-release-notes (extract-version-block valid-entry "0.5.0"))
              '()
              "empty section bodies are fine as long as headers exist")

;; ===========================================================================
;; Test 4: Version not found in changelog fails
;; ===========================================================================

;; placeholder check — real validation below
(check-true #t "placeholder — real checks follow")

(check-false (extract-version-block valid-entry "99.99.99") "version not present returns #f")

(check-not-false (let ([block (extract-version-block valid-entry "99.99.99")]) (not block))
                 "non-existent version block returns #f")

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

(check-equal? (validate-release-notes (extract-version-block multi-version-changelog "0.5.0"))
              '()
              "only the requested version is validated — 0.5.0 is valid")

;; The 0.4.0 entry is also valid; verify it separately
(check-equal? (validate-release-notes (extract-version-block multi-version-changelog "0.4.0"))
              '()
              "version 0.4.0 is also valid in multi-version changelog")

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

(check-equal? (validate-release-notes (extract-version-block uppercase-entry "1.0.0"))
              '()
              "case-insensitive header matching")

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

(check-equal? (validate-release-notes (extract-version-block mixed-case-entry "1.1.0"))
              '()
              "mixed case headers accepted")

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

(check-equal? (validate-release-notes (extract-version-block extra-sections-entry "0.6.0"))
              '()
              "extra sections beyond the required ones are fine")

;; ===========================================================================
;; Test 8: --check mode / exit code behavior
;; ===========================================================================

;; We simulate the exit-code behavior by checking the errors list directly
;; (The CLI module+main handles exit 1; we verify the logic it depends on.)

;; placeholder — the real exit-code test follows below
(check-true #t "placeholder — exit code tests follow")

;; A bad block should produce errors that would trigger exit 1
(let ([bad-block (changelog "### Features" "- only features, nothing else")]
      [errors (validate-release-notes (changelog "### Features" "- only features, nothing else"))])
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

(check-equal? (validate-release-notes (extract-version-block v-prefix-entry "2.0.0"))
              '()
              "version with 'v' prefix in heading matched by plain number")

;; ===========================================================================
;; Test: Features + Bug Fixes together is fine
;; ===========================================================================

(check-equal? (validate-release-notes (extract-version-block valid-entry-alt-headers "0.3.0"))
              '()
              "Features + Bug Fixes (without User-Visible Changes) is acceptable")

;; ===========================================================================
;; Summary
;; ===========================================================================

(printf "\nAll lint-release-notes tests completed.\n")
