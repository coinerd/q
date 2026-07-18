#lang racket/base

;; @suite all
;; @speed fast
;; tests/test-milestone-close-gate.rkt — Unit tests for milestone-close-gate.rkt
;;
;; W0: authoritative milestone truth integrated into the closure gate.
;;
;; Tests the gate-result struct, aggregation predicates, individual gate
;; functions (with injected mock data), and CLI argument parsing.

(require rackunit
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         (only-in "../scripts/claim-verifier.rkt" claim-result)
         "../scripts/gsd-milestone-truth.rkt"
         "../scripts/milestone-close-gate.rkt")

(define-runtime-path close-gate-script "../scripts/milestone-close-gate.rkt")
(define full-digest (make-string 64 #\a))

(define (run-close-cli . arguments)
  (define-values (process stdout stdin stderr)
    (apply subprocess
           #f
           #f
           #f
           (find-executable-path "racket")
           (path->string close-gate-script)
           arguments))
  (close-output-port stdin)
  (define output (port->string stdout))
  (define errors (port->string stderr))
  (subprocess-wait process)
  (values (subprocess-status process) output errors))

(define (make-truth #:milestone [milestone "v0.99.47"]
                    #:release [release "published"]
                    #:substantive [substantive "accepted"]
                    #:accepted? [accepted? #t]
                    #:projection-digest [projection-digest full-digest]
                    #:digest-matches? [digest-matches? #t])
  (milestone-truth milestone
                   release
                   release
                   release
                   substantive
                   substantive
                   substantive
                   accepted?
                   '()
                   projection-digest
                   digest-matches?))

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

(test-case "all-gates-passed? fails closed for empty list"
  (check-false (all-gates-passed? '())))

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

(test-case "check-claims-gate fails closed for empty claim list"
  (define g (check-claims-gate '()))
  (check-false (gate-result-passed? g))
  (check-true (string-contains? (gate-result-details g) "No claims")))

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

(test-case "check-release-gate fails closed for malformed or uncertain API data"
  (define malformed-releases
    (list "not-an-object"
          (hasheq 'tag_name "v0.99.47" 'assets '())
          (hasheq 'tag_name "v0.99.47" 'draft #f 'assets "not-an-array")
          (hasheq 'tag_name
                  "wrong-tag"
                  'draft
                  #f
                  'assets
                  (list (hasheq 'name "q-0.99.47.tar.gz") (hasheq 'name "release-manifest.json")))))
  (for ([release-data (in-list malformed-releases)])
    (define g (check-release-gate release-data "0.99.47"))
    (check-false (gate-result-passed? g))))

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

(test-case "check-issues-gate fails closed for empty issue list"
  (define g (check-issues-gate '()))
  (check-false (gate-result-passed? g))
  (check-true (string-contains? (gate-result-details g) "No issue")))

(test-case "check-issues-gate fails closed for malformed issue data"
  (define g (check-issues-gate (list (hasheq 'number 100))))
  (check-false (gate-result-passed? g))
  (check-true (string-contains? (gate-result-details g) "malformed")))

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

(test-case "check-changelog-gate requires an exact Markdown heading without prefix collisions"
  (for ([changelog (in-list '("## v0.99.470\n- Different release" "Text mentioning ## v0.99.47 inline"
                                                                  "## v0.99.47 release notes"))])
    (check-false (gate-result-passed? (check-changelog-gate changelog "0.99.47")))))

;; ---------------------------------------------------------------------------
;; milestone truth gate
;; ---------------------------------------------------------------------------

(test-case "milestone-truth gate passes only authoritative published acceptance"
  (define g (check-milestone-truth-gate (make-truth) "v0.99.47"))
  (check-equal? (gate-result-name g) 'milestone-truth)
  (check-true (gate-result-passed? g))
  (check-true (string-contains? (gate-result-details g) "published")))

(test-case "milestone-truth gate fails closed for every non-authoritative state"
  (for ([truth (in-list (list (make-truth #:digest-matches? #f)
                              (make-truth #:release "draft-verified")
                              (make-truth #:substantive "rejected" #:accepted? #f)
                              (make-truth #:substantive "in-progress" #:accepted? #f)
                              (make-truth #:accepted? #f)
                              (make-truth #:projection-digest "not-a-digest")
                              (make-truth #:digest-matches? 'not-a-boolean)
                              (make-truth #:accepted? 'not-a-boolean)
                              #f))])
    (define g (check-milestone-truth-gate truth "v0.99.47"))
    (check-equal? (gate-result-name g) 'milestone-truth)
    (check-false (gate-result-passed? g))))

(test-case "milestone-truth gate rejects the wrong milestone and fabricated inconsistent structs"
  (check-false (gate-result-passed? (check-milestone-truth-gate (make-truth #:milestone "v0.99.470")
                                                                "v0.99.47")))
  (define fabricated
    (milestone-truth "v0.99.47"
                     "planned"
                     "published"
                     "published"
                     "rejected"
                     "accepted"
                     "accepted"
                     #t
                     '()
                     full-digest
                     #t))
  (check-false (gate-result-passed? (check-milestone-truth-gate fabricated "v0.99.47"))))

(test-case "milestone-truth file/API uncertainty fails closed with injectable evaluator"
  (define observed #f)
  (define injected
    (evaluate-milestone-truth-gate "independent.json"
                                   full-digest
                                   "v0.99.47"
                                   #:evaluate-file (lambda (path digest)
                                                     (set! observed (list path digest))
                                                     (make-truth))))
  (check-true (gate-result-passed? injected))
  (check-equal? observed (list "independent.json" full-digest))
  (define uncertain
    (evaluate-milestone-truth-gate "missing.json"
                                   full-digest
                                   "v0.99.47"
                                   #:evaluate-file (lambda (_path _digest)
                                                     (error 'test "unavailable"))))
  (check-false (gate-result-passed? uncertain))
  (check-true (string-contains? (gate-result-details uncertain) "uncertainty")))

(test-case "gate summary includes milestone-truth result"
  (define summary (gate-summary (list (check-milestone-truth-gate (make-truth) "v0.99.47"))))
  (check-true (string-contains? summary "milestone-truth")))

;; ---------------------------------------------------------------------------
;; parse-close-gate-args and negative CLI behavior
;; ---------------------------------------------------------------------------

(test-case "parse-close-gate-args requires independent truth file and full digest"
  (define opts
    (parse-close-gate-args (list "834" "--truth-file" "truth.json" "--truth-digest" full-digest)))
  (check-equal? (hash-ref opts 'mode) 'run)
  (check-equal? (hash-ref opts 'milestone-number) 834)
  (check-equal? (hash-ref opts 'truth-file) "truth.json")
  (check-equal? (hash-ref opts 'truth-digest) full-digest)
  (check-false (hash-ref opts 'json #f)))

(test-case "parse-close-gate-args parses --json with required truth inputs"
  (define opts
    (parse-close-gate-args
     (list "--json" "834" "--truth-digest" full-digest "--truth-file" "truth.json")))
  (check-equal? (hash-ref opts 'milestone-number) 834)
  (check-true (hash-ref opts 'json)))

(test-case "bare, missing, malformed, and nonnumeric closure invocations are invalid"
  (for ([args (in-list
               (list '("834")
                     '("834" "--truth-file" "truth.json")
                     (list "834" "--truth-file" "truth.json" "--truth-digest" "short")
                     (list "nope" "--truth-file" "truth.json" "--truth-digest" full-digest)))])
    (check-equal? (hash-ref (parse-close-gate-args args) 'mode) 'invalid)))

(test-case "parse-close-gate-args reserves help mode for explicit --help"
  (check-equal? (hash-ref (parse-close-gate-args '()) 'mode) 'invalid)
  (check-equal? (hash-ref (parse-close-gate-args '("--help")) 'mode) 'help))

(test-case "no-argument CLI exits 2 while explicit help exits 0"
  (define-values (missing-status _missing-output missing-errors) (run-close-cli))
  (check-equal? missing-status 2 missing-errors)
  (define-values (help-status help-output help-errors) (run-close-cli "--help"))
  (check-equal? help-status 0 help-errors)
  (check-true (string-contains? help-output "USAGE:")))

(test-case "old bare, missing-digest, and malformed-digest CLIs fail before closure"
  (for ([arguments
         (in-list (list '("834")
                        '("834" "--truth-file" "truth.json")
                        (list "834" "--truth-file" "truth.json" "--truth-digest" "short")))])
    (define-values (status output errors) (apply run-close-cli arguments))
    (check-not-equal? status 0 (string-append output errors))
    (check-true (string-contains? (string-append output errors) "--truth-file"))))
