#lang racket/base

;; @speed fast
;; @suite release-smoke testing
;; Tests for the release-smoke suite contract.

(require racket/string
         rackunit
         rackunit/text-ui
         "../scripts/run-tests/classify.rkt")

(define suite
  (test-suite "release-smoke contract"
    (test-case "release-smoke-included? accepts curated files"
      (check-true (release-smoke-included? "tests/test-version.rkt"))
      (check-true (release-smoke-included? "tests/test-safe-mode.rkt"))
      (check-true (release-smoke-included? "tests/test-verifier-gate.rkt"))
      (check-true (release-smoke-included? "tests/test-cli-flags.rkt")))

    (test-case "release-smoke-included? rejects non-curated files"
      (check-false (release-smoke-included? "tests/test-browser-playwright-sidecar.rkt"))
      (check-false (release-smoke-included? "tests/test-run-tests.rkt"))
      (check-false (release-smoke-included? "tests/test-protocol-types-source-migration.rkt")))

    (test-case "release-smoke-curated-files is non-empty and bounded"
      (check-true (> (length release-smoke-curated-files) 5))
      (check-true (<= (length release-smoke-curated-files) 20))
      (for ([f (in-list release-smoke-curated-files)])
        (check-pred string? f)
        (check-true (string-prefix? f "tests/"))))

    (test-case "release-smoke excludes browser tests"
      (check-false (release-smoke-included? "tests/test-browser-adapter.rkt"))
      (check-false (release-smoke-included? "tests/test-browser-playwright-adapter.rkt"))
      (check-false (release-smoke-included? "tests/test-browser-integration.rkt")))

    (test-case "release-smoke excludes slow/integration tests"
      (check-false (release-smoke-included? "tests/test-sandbox.rkt"))
      (check-false (release-smoke-included? "tests/test-subprocess.rkt"))
      (check-false (release-smoke-included? "tests/test-integration.rkt")))

    (test-case "release-smoke does not include #581 failing files"
      (check-false (release-smoke-included? "tests/test-browser-playwright-sidecar.rkt"))
      (check-false (release-smoke-included? "tests/test-protocol-types-source-migration.rkt"))
      (check-false (release-smoke-included? "tests/test-run-tests.rkt")))

    (test-case "release-smoke supports @suite release-smoke tag"
      (check-pred values (release-smoke-included? "tests/test-release-smoke-contract.rkt")))))

(run-tests suite)
