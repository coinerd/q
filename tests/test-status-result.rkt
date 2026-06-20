#lang racket

;; test-status-result.rkt — Tests for result-type boundary pattern (W5 pilot)
;;
;; Tests the status-check-result types and check-readme-status function.

(require rackunit
         racket/match
         "../scripts/status-result.rkt")

;; ---------------------------------------------------------------------------
;; Result type construction and predicates
;; ---------------------------------------------------------------------------

(test-case "status-ok: constructor and accessors"
  (define r (status-ok "1.2.3" "Feature release"))
  (check-equal? (status-ok-version r) "1.2.3")
  (check-equal? (status-ok-description r) "Feature release")
  (check-true (status-ok? r))
  (check-true (status-check-result? r)))

(test-case "status-version-mismatch: constructor and accessors"
  (define r (status-version-mismatch "0.9.0" "1.0.0" "README.md"))
  (check-equal? (status-version-mismatch-found r) "0.9.0")
  (check-equal? (status-version-mismatch-expected r) "1.0.0")
  (check-true (status-version-mismatch? r))
  (check-true (status-check-result? r)))

(test-case "status-description-mismatch: constructor and accessors"
  (define r
    (status-description-mismatch "**v1.0.0** — Old text" "**v1.0.0** — New text" "1.0.0" "README.md"))
  (check-equal? (status-description-mismatch-version r) "1.0.0")
  (check-true (status-description-mismatch? r)))

(test-case "status-missing-section: constructor and accessors"
  (define r (status-missing-section "README.md"))
  (check-equal? (status-missing-section-path r) "README.md")
  (check-true (status-missing-section? r)))

(test-case "status-file-not-found: constructor and accessors"
  (define r (status-file-not-found "MISSING.md"))
  (check-equal? (status-file-not-found-path r) "MISSING.md")
  (check-true (status-file-not-found? r)))

;; ---------------------------------------------------------------------------
;; Union predicate and discriminator
;; ---------------------------------------------------------------------------

(test-case "status-check-result?: false for non-results"
  (check-false (status-check-result? 42))
  (check-false (status-check-result? "string"))
  (check-false (status-check-result? '()))
  (check-false (status-check-result? #f))
  (check-false (status-check-result? (list "a" "b"))))

(test-case "status-result-kind: correct symbol for each variant"
  (check-equal? (status-result-kind (status-ok "1.0.0" "desc")) 'ok)
  (check-equal? (status-result-kind (status-version-mismatch "0.9" "1.0" "p")) 'version-mismatch)
  (check-equal? (status-result-kind (status-description-mismatch "a" "b" "1.0" "p"))
                'description-mismatch)
  (check-equal? (status-result-kind (status-missing-section "p")) 'missing-section)
  (check-equal? (status-result-kind (status-file-not-found "p")) 'file-not-found))

;; ---------------------------------------------------------------------------
;; Formatter
;; ---------------------------------------------------------------------------

(test-case "format-status-result: ok message"
  (define r (status-ok "1.0.0" "desc"))
  (define msg (format-status-result r))
  (check-true (string-contains? msg "OK"))
  (check-true (string-contains? msg "1.0.0")))

(test-case "format-status-result: version-mismatch message"
  (define r (status-version-mismatch "0.9.0" "1.0.0" "README.md"))
  (define msg (format-status-result r))
  (check-true (string-contains? msg "MISMATCH"))
  (check-true (string-contains? msg "0.9.0"))
  (check-true (string-contains? msg "1.0.0")))

(test-case "format-status-result: missing-section message"
  (define r (status-missing-section "README.md"))
  (define msg (format-status-result r))
  (check-true (string-contains? msg "MISSING"))
  (check-true (string-contains? msg "README.md")))

(test-case "format-status-result: file-not-found message"
  (define r (status-file-not-found "MISSING.md"))
  (define msg (format-status-result r))
  (check-true (string-contains? msg "ERROR"))
  (check-true (string-contains? msg "MISSING.md")))

;; ---------------------------------------------------------------------------
;; check-readme-status: pure check function
;; ---------------------------------------------------------------------------

(test-case "check-readme-status: returns status-ok when all matches"
  (define lines '("# q" "" "## Status" "" "**v1.0.0** — Release notes" "" "## License" "MIT"))
  (define r (check-readme-status lines "1.0.0" "1.0.0" "Release notes" "README.md"))
  (check-true (status-ok? r) "should be status-ok"))

(test-case "check-readme-status: returns version-mismatch when versions differ"
  (define lines '("# q" "" "## Status" "" "**v0.9.0** — Old" "" "## License" "MIT"))
  (define r (check-readme-status lines "1.0.0" "1.0.0" "Release notes" "README.md"))
  (check-true (status-version-mismatch? r))
  (check-equal? (status-version-mismatch-found r) "0.9.0")
  (check-equal? (status-version-mismatch-expected r) "1.0.0"))

(test-case "check-readme-status: returns description-mismatch when desc differs"
  (define lines '("# q" "" "## Status" "" "**v1.0.0** — Wrong desc" "" "## License" "MIT"))
  (define r (check-readme-status lines "1.0.0" "1.0.0" "Correct desc" "README.md"))
  (check-true (status-description-mismatch? r)))

(test-case "check-readme-status: returns missing-section when no Status section"
  (define lines '("# q" "" "Some text" "" "## License" "MIT"))
  (define r (check-readme-status lines "1.0.0" "1.0.0" "Release notes" "README.md"))
  (check-true (status-missing-section? r)))

(test-case "check-readme-status: returns ok when changelog version is #f (skip desc check)"
  (define lines '("# q" "" "## Status" "" "**v1.0.0** — Old desc" "" "## License" "MIT"))
  (define r (check-readme-status lines "1.0.0" #f #f "README.md"))
  (check-true (status-ok? r)))

;; ---------------------------------------------------------------------------
;; Exit code extraction
;; ---------------------------------------------------------------------------

(test-case "status-result-exit-code: 0 for ok, 1 for failures"
  (check-equal? (status-result-exit-code (status-ok "1.0.0" "desc")) 0)
  (check-equal? (status-result-exit-code (status-version-mismatch "a" "b" "p")) 1)
  (check-equal? (status-result-exit-code (status-description-mismatch "a" "b" "v" "p")) 1)
  (check-equal? (status-result-exit-code (status-missing-section "p")) 1)
  (check-equal? (status-result-exit-code (status-file-not-found "p")) 1))
