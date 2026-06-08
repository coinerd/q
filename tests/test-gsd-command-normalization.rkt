#lang racket

;; @speed fast  ;; @suite extensions

;; BOUNDARY: integration

;; tests/test-gsd-command-normalization.rkt — tests for command parsing and artifact validation

(require rackunit
         rackunit/text-ui
         (combine-in "../extensions/gsd-planning/command-normalization.rkt"
                 (only-in "../util/command-helpers.rkt" extract-cmd-args)))

(define cmd-norm-tests
  (test-suite "GSD Command Normalization"

    (test-case "extract-cmd-args: /go 5"
      (check-equal? (extract-cmd-args "/go 5") "5"))

    (test-case "extract-cmd-args: /plan fix the bug"
      (check-equal? (extract-cmd-args "/plan fix the bug") "fix the bug"))

    (test-case "extract-cmd-args: /go (no args)"
      (check-equal? (extract-cmd-args "/go") ""))

    (test-case "extract-cmd-args: empty string"
      (check-equal? (extract-cmd-args "") ""))

    (test-case "extract-cmd-args: non-command text"
      (check-equal? (extract-cmd-args "hello world") ""))

    (test-case "parse-wave-headers: extracts wave numbers"
      (define plan "## Wave 0: Setup\n...\n## Wave 1: Implement\n...\n## Wave 2: Test\n")
      (check-equal? (parse-wave-headers plan) '(0 1 2)))

    (test-case "parse-wave-headers: no waves"
      (check-equal? (parse-wave-headers "No waves here") '()))

    (test-case "parse-wave-headers: lowercase wave"
      (check-equal? (parse-wave-headers "## wave 3: misc\n") '(3)))

    (test-case "valid-artifact-name?: PLAN"
      (check-true (valid-artifact-name? "PLAN")))

    (test-case "valid-artifact-name?: STATE"
      (check-true (valid-artifact-name? "STATE")))

    (test-case "valid-artifact-name?: custom.md"
      (check-true (valid-artifact-name? "custom.md")))

    (test-case "valid-artifact-name?: waves/W0-test.md"
      (check-true (valid-artifact-name? "waves/W0-test.md")))

    (test-case "valid-artifact-name?: rejects path traversal"
      (check-false (valid-artifact-name? "../etc/passwd")))

    (test-case "valid-artifact-name?: rejects null byte"
      (check-false (valid-artifact-name? "PLAN\x00.md")))

    (test-case "valid-artifact-name?: rejects nested paths"
      (check-false (valid-artifact-name? "sub/dir/file.md")))

    (test-case "json-artifact?: HANDOFF"
      (check-true (json-artifact? "HANDOFF")))

    (test-case "json-artifact?: PLAN"
      (check-false (json-artifact? "PLAN")))

    (test-case "json-artifact?: file.json"
      (check-true (json-artifact? "file.json")))))

(module+ main
  (run-tests cmd-norm-tests))
