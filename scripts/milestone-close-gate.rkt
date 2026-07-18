#!/usr/bin/env racket
#lang racket

;; scripts/milestone-close-gate.rkt — Pre-closure milestone gate (GAP-4).
;;
;; W0 milestone truth is an authoritative closure prerequisite. The command
;; requires an independently supplied truth projection and SHA-256 digest,
;; evaluates it through gsd-milestone-truth.rkt, and combines that result with
;; the existing release-truth checks before a milestone can be closed.
;;
;; This tool exists because v0.99.45 closed with inaccurate test claims.
;; Release-truth enforcement was post-hoc (audit caught it later). This
;; gate makes enforcement PROACTIVE — run BEFORE closing the milestone.
;;
;; Layering:
;;   ├── gate-result struct
;;   ├── Pure gate functions (testable without I/O)
;;   │   ├── check-milestone-truth-gate — authoritative W0 truth
;;   │   ├── check-claims-gate           — claim verification
;;   │   ├── check-release-gate          — release + assets present
;;   │   ├── check-issues-gate           — all milestone issues closed
;;   │   └── check-changelog-gate        — CHANGELOG entry exists
;;   ├── Aggregation
;;   │   ├── all-gates-passed?
;;   │   └── gate-summary
;;   ├── Orchestrator (does I/O)
;;   │   └── run-all-gates
;;   ├── CLI
;;   │   ├── parse-close-gate-args
;;   │   └── main
;;   └── Tests in tests/test-milestone-close-gate.rkt
;;
;; Usage:
;;   cd q/ && racket scripts/milestone-close-gate.rkt <milestone-number> \
;;     --truth-file PATH --truth-digest FULL64
;;   cd q/ && racket scripts/milestone-close-gate.rkt 834 \
;;     --truth-file PATH --truth-digest FULL64 --json
;;
;; Exit codes:
;;   0 — all gates pass, milestone may be closed
;;   1 — one or more gates fail, milestone must NOT be closed
;;   2 — invalid invocation; closure checks did not run

(require racket/file
         racket/match
         racket/system
         json
         "claim-verifier.rkt"
         "gsd-milestone-truth.rkt"
         "milestone-gate.rkt")

;; ---------------------------------------------------------------------------
;; gate-result struct
;; ---------------------------------------------------------------------------

;; A gate-result records the outcome of one gate check.
;;   name    — gate identifier symbol ('claims, 'release, 'issues, etc.)
;;   passed? — #t if the gate passed
;;   details — human-readable explanation string
(struct gate-result (name passed? details) #:transparent)

;; ---------------------------------------------------------------------------
;; Provides
;; ---------------------------------------------------------------------------

(provide gate-result
         gate-result?
         gate-result-name
         gate-result-passed?
         gate-result-details
         all-gates-passed?
         gate-summary
         gate-counts
         check-milestone-truth-gate
         evaluate-milestone-truth-gate
         check-claims-gate
         check-release-gate
         check-issues-gate
         check-changelog-gate
         run-all-gates
         parse-close-gate-args
         main)

;; ---------------------------------------------------------------------------
;; Aggregation (pure)
;; ---------------------------------------------------------------------------

;; Returns #t iff every gate in results passed.
(define (all-gates-passed? results)
  (and (pair? results) (andmap gate-result-passed? results)))

;; Count passed and total gates.
;; Returns (values passed-count total-count).
(define (gate-counts results)
  (define total (length results))
  (define passed (count gate-result-passed? results))
  (values passed total))

;; Format a human-readable summary of all gate results.
(define (gate-summary results)
  (define-values (passed total) (gate-counts results))
  (string-append (string-join (for/list ([g (in-list results)])
                                (format "  [~a] ~a: ~a"
                                        (if (gate-result-passed? g) "✓ PASS" "✗ FAIL")
                                        (gate-result-name g)
                                        (gate-result-details g)))
                              "\n")
                 "\n"
                 (format "---\n~a/~a gates passed — ~a"
                         passed
                         total
                         (if (= passed total) "ALL PASS ✓" "HAS FAILURES ✗"))))

;; ---------------------------------------------------------------------------
;; Individual gate functions (pure — accept injected data)
;; ---------------------------------------------------------------------------

;; Milestone-truth gate: closure is allowed only for independently digest-bound,
;; effectively published, effectively accepted truth whose accepted? verdict is
;; true and whose validated milestone identity exactly matches the current
;; version. Reuse the canonical predicate so transparent synthetic structs must
;; satisfy every declaration/derived/effective consistency condition.
(define (check-milestone-truth-gate truth expected-milestone)
  (cond
    [(not (milestone-truth? truth))
     (gate-result 'milestone-truth #f "Milestone truth is unavailable or malformed")]
    [else
     (define actual-milestone (milestone-truth-milestone truth))
     (define digest-matches? (milestone-truth-digest-matches? truth))
     (define effective-release (milestone-truth-effective-release-mechanics truth))
     (define effective-substantive (milestone-truth-effective-substantive-acceptance truth))
     (define accepted? (milestone-truth-accepted? truth))
     (define passed?
       (and (string? expected-milestone)
            (equal? actual-milestone expected-milestone)
            (milestone-truth-gate-passed? truth)
            (equal? effective-release "published")))
     (gate-result
      'milestone-truth
      passed?
      (format
       "milestone=~a (expected ~a), digest-match=~a, effective release=~a, effective substantive=~a, accepted?=~a"
       actual-milestone
       expected-milestone
       digest-matches?
       effective-release
       effective-substantive
       accepted?))]))

;; Evaluate a truth file through the authoritative module. The evaluator is
;; injectable so tests can remain pure and no file/API uncertainty can be
;; mistaken for acceptance.
(define (evaluate-milestone-truth-gate path
                                       external-digest
                                       expected-milestone
                                       #:evaluate-file [evaluate-file evaluate-milestone-file])
  (with-handlers ([exn:fail? (lambda (failure)
                               (gate-result 'milestone-truth
                                            #f
                                            (format "Milestone truth file/API uncertainty: ~a"
                                                    (exn-message failure))))])
    (check-milestone-truth-gate (evaluate-file path external-digest) expected-milestone)))

;; Claims gate: verify that all claim-results have matched? = #t.
;;   verified-claims — list of claim-result structs from claim-verifier
;; Returns a gate-result.
(define (check-claims-gate verified-claims)
  (define mismatches (filter (λ (c) (not (claim-result-matched? c))) verified-claims))
  (cond
    [(null? verified-claims) (gate-result 'claims #f "No claims were available to verify")]
    [(null? mismatches)
     (gate-result 'claims #t (format "~a claim(s) verified, all matched" (length verified-claims)))]
    [else
     (gate-result 'claims
                  #f
                  (format "~a/~a claim(s) mismatched: ~a"
                          (length mismatches)
                          (length verified-claims)
                          (string-join (for/list ([m (in-list mismatches)])
                                         (format "~a (claimed ~a, actual ~a)"
                                                 (claim-result-pattern m)
                                                 (claim-result-claimed m)
                                                 (claim-result-actual m)))
                                       "; ")))]))

;; Release gate: verify a GitHub release exists with required assets.
;;   release-data — jsexpr from GitHub API or #f
;;   version      — version string like "0.99.47"
;; Returns a gate-result.
(define (check-release-gate release-data version)
  (define expected-tag (format "v~a" version))
  (define expected-tarball (format "q-~a.tar.gz" version))
  (define release-assets (and (hash? release-data) (hash-ref release-data 'assets #f)))
  (define valid-api-record?
    (and (hash? release-data)
         (equal? (hash-ref release-data 'tag_name #f) expected-tag)
         (boolean? (hash-ref release-data 'draft 'missing))
         (list? release-assets)
         (andmap (lambda (asset) (and (hash? asset) (string? (hash-ref asset 'name #f))))
                 release-assets)))
  (cond
    [(not release-data) (gate-result 'release #f (format "No release for ~a" expected-tag))]
    [(not valid-api-record?)
     (gate-result 'release #f (format "Malformed or uncertain release API data for ~a" expected-tag))]
    [(hash-ref release-data 'draft)
     (gate-result 'release #f (format "Release ~a is draft" expected-tag))]
    [(not (release-has-asset? release-data expected-tarball))
     (gate-result 'release #f (format "Missing tarball asset ~a" expected-tarball))]
    [(not (release-has-asset? release-data "release-manifest.json"))
     (gate-result 'release #f "Missing release-manifest.json asset")]
    [else (gate-result 'release #t (format "Release ~a with tarball + manifest" expected-tag))]))

;; Issues gate: verify all milestone issues are closed.
;;   issues — list of issue hashes with 'state key ("open" or "closed")
;; Returns a gate-result.
(define (check-issues-gate issues)
  (define malformed
    (filter (λ (i)
              (or (not (hash? i))
                  (not (exact-positive-integer? (hash-ref i 'number #f)))
                  (not (member (hash-ref i 'state #f) '("open" "closed")))))
            issues))
  (define open-issues (filter (λ (i) (and (hash? i) (equal? (hash-ref i 'state #f) "open"))) issues))
  (cond
    [(null? issues) (gate-result 'issues #f "No issue evidence was returned")]
    [(pair? malformed)
     (gate-result 'issues #f (format "~a malformed issue record(s)" (length malformed)))]
    [(null? open-issues) (gate-result 'issues #t (format "~a issue(s) all closed" (length issues)))]
    [else
     (gate-result
      'issues
      #f
      (format "~a open issue(s): ~a"
              (length open-issues)
              (string-join (map (λ (i) (format "#~a" (hash-ref i 'number #f))) open-issues) ", ")))]))

;; Changelog gate: verify CHANGELOG has an entry for the version.
;;   changelog-text — string contents of CHANGELOG.md
;;   version        — version string like "0.99.47"
;; Returns a gate-result.
(define (check-changelog-gate changelog-text version)
  ;; Compare complete level-two heading lines so 0.99.47 cannot match
  ;; 0.99.470 or prose. Accept CRLF without accepting padded headings.
  (define expected-headings (list (format "## ~a" version) (format "## v~a" version)))
  (define (without-terminal-return line)
    (if (and (positive? (string-length line))
             (char=? (string-ref line (sub1 (string-length line))) #\return))
        (substring line 0 (sub1 (string-length line)))
        line))
  (define found?
    (for/or ([line (in-list (string-split changelog-text "\n" #:trim? #f))])
      (member (without-terminal-return line) expected-headings)))
  (if found?
      (gate-result 'changelog #t (format "CHANGELOG entry for v~a found" version))
      (gate-result 'changelog #f (format "No CHANGELOG entry for v~a" version))))

;; ---------------------------------------------------------------------------
;; Orchestrator (does I/O — calls GitHub API, reads files)
;; ---------------------------------------------------------------------------

;; Helper: get current version from util/version.rkt.
(define (get-current-version)
  (define version-path "util/version.rkt")
  (if (file-exists? version-path)
      (let ([text (file->string version-path)])
        (define m (regexp-match #rx"\"([0-9]+\\.[0-9]+\\.[0-9]+)\"" text))
        (and m (cadr m)))
      #f))

;; Helper: run milestone-gate.rkt as a subprocess and collect its exit code.
;; This delegates the CI, traceability, and metrics checks to the existing
;; battle-tested tool.
(define (run-milestone-gate-subprocess milestone-number)
  (define exit-code
    (system/exit-code (format "racket scripts/milestone-gate.rkt ~a" milestone-number)))
  exit-code)

;; Run all gates for a milestone. Does I/O (GitHub API, file reads).
;; The truth evaluator remains injectable for deterministic boundary tests.
;; Returns a list of gate-result structs.
(define (run-all-gates milestone-number
                       truth-file
                       truth-digest
                       #:truth-evaluator [truth-evaluator evaluate-milestone-file])
  (define version (get-current-version))
  (unless version
    (printf "ERROR: Could not determine current version from util/version.rkt~n")
    (exit 1))

  ;; Gate 1: authoritative milestone truth supplied independently of this gate.
  (define milestone-truth-gate
    (evaluate-milestone-truth-gate truth-file
                                   truth-digest
                                   (format "v~a" version)
                                   #:evaluate-file truth-evaluator))

  ;; Gate 2: Claims — scan docs/reports/ for claims mentioning this version
  (define reports-dir "docs/reports")
  (define all-claims '())
  (when (directory-exists? reports-dir)
    (for ([f (in-directory reports-dir)])
      (define f-str (path->string f))
      (when (and (string-suffix? f-str ".md") (string-contains? f-str version))
        (define claims (scan-report-file f-str))
        (set! all-claims (append all-claims claims)))))

  ;; Get actual test counts for verification
  (define actual-counts
    (let ([test-files (filter (λ (p) (string-suffix? (path->string p) ".rkt"))
                              (sequence->list (in-directory "tests")))])
      (list (cons 'test-cases (for/sum ([f (in-list test-files)]) (count-test-cases-in-file f)))
            (cons 'test-files (length test-files)))))

  (define claims-gate (check-claims-gate (verify-claims all-claims actual-counts)))

  ;; Gate 3: Release — query GitHub API
  (define release-data
    (with-handlers ([exn:fail? (λ (_) #f)])
      (define in
        (process (format "curl -s \"https://api.github.com/repos/~a/~a/releases/tags/v~a\""
                         "coinerd"
                         "q"
                         version)))
      (define out (car in))
      (define json-text (port->string out))
      (close-input-port out)
      (string->jsexpr json-text)))
  (define release-gate (check-release-gate release-data version))

  ;; Gate 4: Issues — query GitHub API for milestone issues
  (define issues-data
    (with-handlers ([exn:fail? (λ (_) '())])
      (define in
        (process
         (format "curl -s \"https://api.github.com/repos/~a/~a/issues?milestone=~a&state=all\""
                 "coinerd"
                 "q"
                 milestone-number)))
      (define out (car in))
      (define json-text (port->string out))
      (close-input-port out)
      (define parsed (string->jsexpr json-text))
      (if (list? parsed)
          parsed
          '())))
  (define issues-gate (check-issues-gate issues-data))

  ;; Gate 5: Changelog — read CHANGELOG.md
  (define changelog-text
    (if (file-exists? "CHANGELOG.md")
        (file->string "CHANGELOG.md")
        ""))
  (define changelog-gate (check-changelog-gate changelog-text version))

  ;; Gate 6: Delegate CI + traceability + metrics to milestone-gate.rkt
  (define mg-exit (run-milestone-gate-subprocess milestone-number))
  (define mg-gate
    (gate-result 'ci-traceability-metrics
                 (= mg-exit 0)
                 (format "milestone-gate.rkt exit code ~a" mg-exit)))

  (list milestone-truth-gate claims-gate release-gate issues-gate changelog-gate mg-gate))

;; ---------------------------------------------------------------------------
;; CLI layer
;; ---------------------------------------------------------------------------

(define full-digest-pattern #px"^[0-9a-f]{64}$")

(define (parse-close-gate-args args)
  (define (invalid reason)
    (hash 'mode 'invalid 'reason reason))
  (cond
    [(null? args) (invalid "arguments are required; use --help for usage")]
    [(equal? args '("--help")) (hash 'mode 'help)]
    [else
     (let loop ([remaining args]
                [milestone-number #f]
                [truth-file #f]
                [truth-digest #f]
                [json? #f])
       (match remaining
         ['()
          (cond
            [(not milestone-number) (invalid "a positive milestone number is required")]
            [(or (not truth-file) (string=? truth-file "")) (invalid "--truth-file PATH is required")]
            [(not truth-digest) (invalid "--truth-digest FULL64 is required")]
            [(not (regexp-match? full-digest-pattern truth-digest))
             (invalid "--truth-digest must be exactly 64 lowercase hexadecimal characters")]
            [else
             (hash 'mode
                   'run
                   'milestone-number
                   milestone-number
                   'truth-file
                   truth-file
                   'truth-digest
                   truth-digest
                   'json
                   json?)])]
         [(list* "--json" tail)
          (if json?
              (invalid "--json may be supplied only once")
              (loop tail milestone-number truth-file truth-digest #t))]
         [(list* "--truth-file" value tail)
          (if (or truth-file (string-prefix? value "--"))
              (invalid "--truth-file requires exactly one PATH")
              (loop tail milestone-number value truth-digest json?))]
         [(list* "--truth-digest" value tail)
          (if (or truth-digest (string-prefix? value "--"))
              (invalid "--truth-digest requires exactly one FULL64 value")
              (loop tail milestone-number truth-file value json?))]
         [(list* option tail)
          (define parsed-number (string->number option))
          (if (and (not milestone-number) (exact-positive-integer? parsed-number))
              (loop tail parsed-number truth-file truth-digest json?)
              (invalid (format "unknown or invalid argument: ~a" option)))]))]))

(define (print-usage)
  (displayln "USAGE:")
  (displayln
   "  racket scripts/milestone-close-gate.rkt <milestone-number> --truth-file PATH --truth-digest FULL64 [--json]")
  (displayln "")
  (displayln "FULL64 is an independently supplied 64-character lowercase SHA-256 digest.")
  (displayln "A bare milestone number is invalid and never starts closure checks.")
  (displayln "")
  (displayln "EXIT CODES:")
  (displayln "  0 — all gates pass, milestone may be closed")
  (displayln "  1 — one or more gates fail, milestone must NOT be closed")
  (displayln "  2 — invalid command-line invocation"))

(define (main args)
  (define opts (parse-close-gate-args args))
  (case (hash-ref opts 'mode)
    [(help) (print-usage)]
    [(invalid)
     (eprintf "ERROR: ~a~n~n" (hash-ref opts 'reason))
     (print-usage)
     (exit 2)]
    [else
     (define milestone-number (hash-ref opts 'milestone-number))
     (unless (file-exists? "main.rkt")
       (printf "ERROR: Run from the q/ directory~n")
       (exit 1))
     (define results
       (run-all-gates milestone-number (hash-ref opts 'truth-file) (hash-ref opts 'truth-digest)))
     (cond
       [(hash-ref opts 'json)
        (define-values (passed total) (gate-counts results))
        (printf "~a~n"
                (jsexpr->string (hasheq 'milestone
                                        milestone-number
                                        'all_passed
                                        (all-gates-passed? results)
                                        'passed
                                        passed
                                        'total
                                        total
                                        'gates
                                        (for/list ([g (in-list results)])
                                          (hasheq 'name
                                                  (symbol->string (gate-result-name g))
                                                  'passed
                                                  (gate-result-passed? g)
                                                  'details
                                                  (gate-result-details g))))))]
       [else
        (printf "=== Milestone #~a Close Gate ===~n~n" milestone-number)
        (displayln (gate-summary results))])
     (exit (if (all-gates-passed? results) 0 1))]))

(module+ main
  (main (vector->list (current-command-line-arguments))))
