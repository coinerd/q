#lang racket/base

;; tests/test-milestone-close-gate.rkt — Unit tests for milestone-close-gate.rkt
;;
;; W2 (#8684): Pre-closure milestone gate (GAP-4).
;;
;; Tests the gate-result struct, aggregation predicates, individual gate
;; functions (with injected mock data), and CLI argument parsing.

(require rackunit
         racket/string
         (only-in "../scripts/claim-verifier.rkt" claim-result)
         "../scripts/milestone-close-gate.rkt")

;; ---------------------------------------------------------------------------
;; gate-result struct
;; ---------------------------------------------------------------------------

(test-case "gate-result constructs with name/passed?/details"
  (define g (gate-result 'release #t "Release v0.99.47 exists"))
  (check-equal? (gate-result-name g) 'release)
  (check-true (gate-result-passed? g))
  (check-equal? (gate-result-details g) "Release v0.99.47 exists"))

(test-case "gate-result is transparent for debugging"
  (define g (gate-result 'ci #f "CI job 'test' failed"))
  (check-pred gate-result? g))

;; ---------------------------------------------------------------------------
;; all-gates-passed?
;; ---------------------------------------------------------------------------

(test-case "all-gates-passed? returns #t when all gates pass"
  (define results
    (list (gate-result 'claims #t "All claims verified")
          (gate-result 'ci #t "CI green")
          (gate-result 'release #t "Release exists")))
  (check-true (all-gates-passed? results)))

(test-case "all-gates-passed? returns #f when any gate fails"
  (define results
    (list (gate-result 'claims #t "All claims verified")
          (gate-result 'ci #f "CI job 'test' failed")
          (gate-result 'release #t "Release exists")))
  (check-false (all-gates-passed? results)))

(test-case "all-gates-passed? returns #t for empty list"
  (check-true (all-gates-passed? '())))

(test-case "all-gates-passed? returns #f when all gates fail"
  (define results (list (gate-result 'claims #f "Claims mismatch") (gate-result 'ci #f "CI failed")))
  (check-false (all-gates-passed? results)))

;; ---------------------------------------------------------------------------
;; gate-summary
;; ---------------------------------------------------------------------------

(test-case "gate-summary includes PASS/FAIL markers"
  (define results
    (list (gate-result 'claims #t "All claims verified") (gate-result 'ci #f "CI job 'test' failed")))
  (define summary (gate-summary results))
  (check-pred string? summary)
  (check-true (string-contains? summary "claims"))
  (check-true (string-contains? summary "PASS") (format "summary: ~a" summary))
  (check-true (string-contains? summary "FAIL") (format "summary: ~a" summary)))

(test-case "gate-summary reports verdict for all-pass"
  (define results (list (gate-result 'claims #t "ok") (gate-result 'ci #t "ok")))
  (define summary (gate-summary results))
  (check-true (string-contains? summary "ALL PASS")))

(test-case "gate-summary reports verdict for failures"
  (define results (list (gate-result 'claims #t "ok") (gate-result 'ci #f "bad")))
  (define summary (gate-summary results))
  (check-true (string-contains? summary "FAIL")))

;; ---------------------------------------------------------------------------
;; check-claims-gate
;; ---------------------------------------------------------------------------

(test-case "check-claims-gate passes when all claims match"
  (define verified (list (claim-result 'tests 100 100 #t) (claim-result 'check-assertions 50 50 #t)))
  (define g (check-claims-gate verified))
  (check-true (gate-result-passed? g))
  (check-equal? (gate-result-name g) 'claims))

(test-case "check-claims-gate fails when any claim mismatches"
  (define verified (list (claim-result 'tests 100 100 #t) (claim-result 'check-assertions 50 49 #f)))
  (define g (check-claims-gate verified))
  (check-false (gate-result-passed? g))
  (check-equal? (gate-result-name g) 'claims))

(test-case "check-claims-gate passes for empty claim list"
  (define g (check-claims-gate '()))
  (check-true (gate-result-passed? g)))

;; ---------------------------------------------------------------------------
;; check-release-gate
;; ---------------------------------------------------------------------------

(test-case "check-release-gate passes when release exists with assets"
  (define release-data
    (hasheq 'tag_name
            "v0.99.47"
            'draft
            #f
            'assets
            (list (hasheq 'name "q-0.99.47.tar.gz") (hasheq 'name "release-manifest.json"))))
  (define g (check-release-gate release-data "0.99.47"))
  (check-true (gate-result-passed? g))
  (check-equal? (gate-result-name g) 'release))

(test-case "check-release-gate fails when release is nil"
  (define g (check-release-gate #f "0.99.47"))
  (check-false (gate-result-passed? g)))

(test-case "check-release-gate fails when release is draft"
  (define release-data (hasheq 'tag_name "v0.99.47" 'draft #t 'assets '()))
  (define g (check-release-gate release-data "0.99.47"))
  (check-false (gate-result-passed? g)))

(test-case "check-release-gate fails when tarball asset missing"
  (define release-data
    (hasheq 'tag_name "v0.99.47" 'draft #f 'assets (list (hasheq 'name "release-manifest.json"))))
  (define g (check-release-gate release-data "0.99.47"))
  (check-false (gate-result-passed? g)))

;; ---------------------------------------------------------------------------
;; check-issues-gate
;; ---------------------------------------------------------------------------

(test-case "check-issues-gate passes when all issues closed"
  (define issues (list (hasheq 'number 100 'state "closed") (hasheq 'number 101 'state "closed")))
  (define g (check-issues-gate issues))
  (check-true (gate-result-passed? g))
  (check-equal? (gate-result-name g) 'issues))

(test-case "check-issues-gate fails when any issue open"
  (define issues (list (hasheq 'number 100 'state "closed") (hasheq 'number 101 'state "open")))
  (define g (check-issues-gate issues))
  (check-false (gate-result-passed? g)))

(test-case "check-issues-gate passes for empty issue list"
  (define g (check-issues-gate '()))
  (check-true (gate-result-passed? g)))

;; ---------------------------------------------------------------------------
;; check-changelog-gate
;; ---------------------------------------------------------------------------

(test-case "check-changelog-gate passes when version entry exists"
  (define changelog "## v0.99.47\n- Fixed stuff\n\n## v0.99.46\n- Old stuff")
  (define g (check-changelog-gate changelog "0.99.47"))
  (check-true (gate-result-passed? g))
  (check-equal? (gate-result-name g) 'changelog))

(test-case "check-changelog-gate fails when version entry missing"
  (define changelog "## v0.99.46\n- Old stuff")
  (define g (check-changelog-gate changelog "0.99.99"))
  (check-false (gate-result-passed? g)))

;; ---------------------------------------------------------------------------
;; parse-close-gate-args
;; ---------------------------------------------------------------------------

(test-case "parse-close-gate-args parses milestone number"
  (define opts (parse-close-gate-args '("834")))
  (check-equal? (hash-ref opts 'milestone-number) 834)
  (check-false (hash-ref opts 'json #f)))

(test-case "parse-close-gate-args parses --json flag"
  (define opts (parse-close-gate-args '("834" "--json")))
  (check-equal? (hash-ref opts 'milestone-number) 834)
  (check-true (hash-ref opts 'json)))

(test-case "parse-close-gate-args with no args returns help mode"
  (define opts (parse-close-gate-args '()))
  (check-equal? (hash-ref opts 'mode) 'help))
