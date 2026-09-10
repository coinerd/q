#lang racket

;; @speed slow
;; @suite default
;; @isolation process

;; BOUNDARY: integration

;; tests/test-run-tests-script.rkt — Tests for scripts/run-tests.rkt
;;
;; NOTE: Tests script metadata (exists, compiles, help) AND the documented
;; one-command L0–L3 workflow forms from docs/TDD-TEST-STRATEGY-PLAN.md
;; (W6): a documented flag the runner rejects is a red test here.
;; Does NOT invoke full test suite (that would be recursive and slow).
;; @boundary unit  ;; @mutates fs

(require rackunit
         rackunit/text-ui
         racket/runtime-path)

;; Resolve to the directory containing this test file
(define-runtime-path here ".")
(define project-root (simplify-path (build-path here ".."))) ;; q/ root

(define script-path (build-path project-root "scripts" "run-tests.rkt"))

(define (q-system/cmd cmd)
  (parameterize ([current-directory project-root])
    (system/exit-code cmd)))

(define run-tests-script-tests
  (test-suite "run-tests script"

    (test-case "script file exists"
      (check-true (file-exists? script-path)))

    (test-case "script compiles without error"
      (define exit-code (q-system/cmd "raco make scripts/run-tests.rkt 2>&1"))
      (check-equal? exit-code 0))

    (test-case "--help exits successfully"
      (define exit-code (q-system/cmd "racket scripts/run-tests.rkt --help 2>&1"))
      (check-equal? exit-code 0))

    (test-case "--suite fast help accepted"
      (define exit-code (q-system/cmd "racket scripts/run-tests.rkt --suite fast --help 2>&1"))
      (check-equal? exit-code 0))

    (test-case "--sequential flag accepted"
      (define exit-code (q-system/cmd "racket scripts/run-tests.rkt --sequential --help 2>&1"))
      (check-equal? exit-code 0))

    (test-case "script uses racket/base"
      (define content (file->string script-path))
      (check-not-false (string-contains? content "#lang racket/base")))

    ;; BUG-0033 (W5): the canonical runner is
    ;; q/scripts/run-tests.rkt; it must at least LOAD from an arbitrary
    ;; cwd (the wave-doc convention `cd <project-base>/q && racket
    ;; scripts/run-tests.rkt` additionally holds, but loading must not
    ;; silently depend on the invocation directory).
    (test-case "documented entry point loads from an arbitrary cwd (BUG-0033)"
      (parameterize ([current-directory (find-system-path 'temp-dir)])
        (define exit-code
          (system*/exit-code (find-executable-path "racket") (path->string script-path) "--help"))
        (check-equal? exit-code 0)))

    ;; BUG-0033 (W5): every tracked test file must be invocable
    ;; via `racket <file>` from ANY cwd (spot-check from the system temp
    ;; dir; broader battery in tests/test-cwd-independence.rkt).
    (test-case "tracked tests invocable from arbitrary cwd - spot-check (BUG-0033)"
      (parameterize ([current-directory (find-system-path 'temp-dir)])
        (define exit-code
          (system*/exit-code
           (find-executable-path "racket")
           (path->string (build-path project-root "tests" "test-ui-action-adapters.rkt"))))
        (check-equal? exit-code 0)))

    ;; ── W6: documented L0–L3 one-command workflows ──
    ;; Every command form documented in docs/TDD-TEST-STRATEGY-PLAN.md must
    ;; execute against the real CLI. Using an undocumented/invented flag or
    ;; dropping a documented one makes one of these tests red.

    ;; L0 — current test: racket scripts/run-tests.rkt <file>
    (test-case "L0 documented form: direct file argument executes (W6)"
      (define exit-code (q-system/cmd "racket scripts/run-tests.rkt tests/test-version.rkt 2>&1"))
      (check-equal? exit-code 0))

    ;; L1 — direct impact:
    ;;   racket scripts/run-tests.rkt --changed-base <base> --changed-head HEAD
    ;; Exercised with --impact-dry-run --explain so this unit test performs
    ;; selection only (no selected-test execution); the flags themselves are
    ;; the documented surface and must be accepted by the real parser.
    (test-case "L1 documented form: --changed-base/--changed-head accepted (W6)"
      (define exit-code
        (q-system/cmd
         "racket scripts/run-tests.rkt --changed-base origin/main --changed-head HEAD --impact-dry-run --explain 2>&1"))
      (check-equal? exit-code 0))

    ;; L2 — transitive impact: the implemented impact selector already walks
    ;; transitive dependents and escalates on graph uncertainty, so the
    ;; documented L2 command uses the same selection flags; they must be
    ;; accepted identically (level difference is workflow timing, not syntax).
    ;; --explain (selection-only) keeps the form diff-state independent: a
    ;; bare --impact-dry-run on an empty selection is refused with exit 3.
    (test-case "L2 documented form: same selection flags accepted (W6)"
      (define exit-code
        (q-system/cmd
         "racket scripts/run-tests.rkt --changed-base origin/main --changed-head HEAD --impact-dry-run --explain 2>&1"))
      (check-equal? exit-code 0))

    ;; L3 — broad fast: racket scripts/run-tests.rkt --suite fast.
    ;; The full fast suite is out of scope for this unit test; the documented
    ;; flag pair must still be accepted by the real parser.
    (test-case "L3 documented form: --suite fast accepted (W6)"
      (define exit-code (q-system/cmd "racket scripts/run-tests.rkt --suite fast --help 2>&1"))
      (check-equal? exit-code 0))))

(run-tests run-tests-script-tests)
