#lang racket

;; @speed fast
;; @suite fast

;; W9 (#8483): Verify that design-fact comments added in v0.99.38 W9
;; are present in the source files. These comments document concrete
;; audit findings (hidden assumptions, known bugs, invariant provenance).
;; Each test ties a comment to a specific audit wave.

(require rackunit
         rackunit/text-ui
         racket/file)

;; Paths are relative to the project root (q/), not to this test file.
;; raco test runs from the directory containing the test file (tests/),
;; so we prepend "../" to reach source modules.
(define root "../")

;; Helper: read file as string, check for substring
(define (has-comment? path fragment)
  (and (file-exists? path) (string-contains? (file->string path) fragment)))

;; ============================================================
;; Design-fact comment presence tests
;; ============================================================

(define-test-suite
 design-fact-comment-tests
 (test-case "parse.rkt: W3 F1 regex bug design-fact comment"
   (check-true (has-comment? (string-append root "scripts/run-tests/parse.rkt") "DESIGN FACT (W3 F1")
               "FAILURE-END regex bug documented"))
 (test-case "config-paths.rkt: W6 global-config-dir consolidation comment"
   (check-true (has-comment? (string-append root "util/config-paths.rkt") "DESIGN FACT (W6")
               "global-config-dir consolidation rationale documented"))
 (test-case "abstraction-audit.rkt: W8 signal provenance comment"
   (check-true (has-comment? (string-append root "scripts/abstraction-audit.rkt")
                             "DESIGN FACT: Each signal encodes")
               "W8 scanner signal audit provenance documented"))
 (test-case "metrics-helpers.rkt: W5 pure/shell boundary comment"
   (check-true (has-comment? (string-append root "scripts/metrics-helpers.rkt") "DESIGN FACT (W5")
               "pure-helpers + thin-I/O-shell pattern provenance documented"))
 (test-case "subprocess.rkt: W6 env sanitization boundary comment"
   (check-true (has-comment? (string-append root "sandbox/subprocess.rkt") "DESIGN FACT (W6")
               "sanitize-env mutation boundary documented"))
 (test-case "gui/main.rkt: W6 GUI adapter environment gate comment"
   (check-true (has-comment? (string-append root "gui/main.rkt") "DESIGN FACT (W6")
               "gui-available? environment gate documented")))

;; ============================================================
;; Design-fact content verification tests
;; ============================================================

(define-test-suite
 design-fact-content-tests
 (test-case "parse.rkt: comment references #rx vs #px"
   (check-true (has-comment? (string-append root "scripts/run-tests/parse.rkt") "#px")
               "regex bug comment mentions #px fix"))
 (test-case "config-paths.rkt: comment references provider-factory consolidation"
   (check-true (has-comment? (string-append root "util/config-paths.rkt") "provider-factory")
               "consolidation comment references first migrated module"))
 (test-case "metrics-helpers.rkt: comment references --check-only flag"
   (check-true (has-comment? (string-append root "scripts/metrics-helpers.rkt") "--check-only")
               "pure/shell comment references the dry-run flag that depends on it"))
 (test-case "abstraction-audit.rkt: comment references all 4 signal origins"
   (define content (file->string (string-append root "scripts/abstraction-audit.rkt")))
   (check-true (string-contains? content "W2 audit") "references W2")
   (check-true (string-contains? content "W5 mutation") "references W5")
   (check-true (string-contains? content "W1 scorecard P5") "references W1")
   (check-true (string-contains? content "W6 adapter") "references W6")))

(define-test-suite all-design-fact-tests design-fact-comment-tests design-fact-content-tests)

(module+ test
  (run-tests all-design-fact-tests))

(module+ main
  (run-tests all-design-fact-tests))
