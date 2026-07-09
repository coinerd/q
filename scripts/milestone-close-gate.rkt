#!/usr/bin/env racket
#lang racket

;; scripts/milestone-close-gate.rkt — Pre-closure milestone gate (GAP-4).
;;
;; W2 (#8684): A single-command pre-closure gate that enforces ALL
;; release-truth checks before a milestone can be closed. This wraps the
;; existing milestone-gate.rkt and adds the claim-verifier.rkt logic into
;; one mandatory gate.
;;
;; This tool exists because v0.99.45 closed with inaccurate test claims.
;; Release-truth enforcement was post-hoc (audit caught it later). This
;; gate makes enforcement PROACTIVE — run BEFORE closing the milestone.
;;
;; Layering:
;;   ├── gate-result struct
;;   ├── Pure gate functions (testable without I/O)
;;   │   ├── check-claims-gate     — claim verification (delegates to claim-verifier)
;;   │   ├── check-release-gate    — release + assets present
;;   │   ├── check-issues-gate     — all milestone issues closed
;;   │   └── check-changelog-gate  — CHANGELOG entry exists
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
;;   cd q/ && racket scripts/milestone-close-gate.rkt <milestone-number>
;;   cd q/ && racket scripts/milestone-close-gate.rkt 834 --json
;;
;; Exit codes:
;;   0 — all gates pass, milestone may be closed
;;   1 — one or more gates fail, milestone must NOT be closed

(require racket/file
         racket/match
         racket/system
         json
         "claim-verifier.rkt"
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
  (andmap gate-result-passed? results))

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

;; Claims gate: verify that all claim-results have matched? = #t.
;;   verified-claims — list of claim-result structs from claim-verifier
;; Returns a gate-result.
(define (check-claims-gate verified-claims)
  (define mismatches (filter (λ (c) (not (claim-result-matched? c))) verified-claims))
  (if (null? mismatches)
      (gate-result 'claims #t (format "~a claim(s) verified, all matched" (length verified-claims)))
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
                                        "; ")))))

;; Release gate: verify a GitHub release exists with required assets.
;;   release-data — jsexpr from GitHub API or #f
;;   version      — version string like "0.99.47"
;; Returns a gate-result.
(define (check-release-gate release-data version)
  (define expected-tag (format "v~a" version))
  (define expected-tarball (format "q-~a.tar.gz" version))
  (cond
    [(not release-data) (gate-result 'release #f (format "No release for ~a" expected-tag))]
    [(hash-ref release-data 'draft #f)
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
  (define open-issues (filter (λ (i) (equal? (hash-ref i 'state #f) "open")) issues))
  (if (null? open-issues)
      (gate-result 'issues #t (format "~a issue(s) all closed" (length issues)))
      (gate-result
       'issues
       #f
       (format "~a open issue(s): ~a"
               (length open-issues)
               (string-join (map (λ (i) (format "#~a" (hash-ref i 'number #f))) open-issues) ", ")))))

;; Changelog gate: verify CHANGELOG has an entry for the version.
;;   changelog-text — string contents of CHANGELOG.md
;;   version        — version string like "0.99.47"
;; Returns a gate-result.
(define (check-changelog-gate changelog-text version)
  (define version-pattern (format "## v~a" version))
  (if (string-contains? changelog-text version-pattern)
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
;; Returns a list of gate-result structs.
(define (run-all-gates milestone-number)
  (define version (get-current-version))
  (unless version
    (printf "ERROR: Could not determine current version from util/version.rkt~n")
    (exit 1))

  ;; Gate 1: Claims — scan docs/reports/ for claims mentioning this version
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

  ;; Gate 2: Release — query GitHub API
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

  ;; Gate 3: Issues — query GitHub API for milestone issues
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

  ;; Gate 4: Changelog — read CHANGELOG.md
  (define changelog-text
    (if (file-exists? "CHANGELOG.md")
        (file->string "CHANGELOG.md")
        ""))
  (define changelog-gate (check-changelog-gate changelog-text version))

  ;; Gate 5: Delegate CI + traceability + metrics to milestone-gate.rkt
  (define mg-exit (run-milestone-gate-subprocess milestone-number))
  (define mg-gate
    (gate-result 'ci-traceability-metrics
                 (= mg-exit 0)
                 (format "milestone-gate.rkt exit code ~a" mg-exit)))

  (list claims-gate release-gate issues-gate changelog-gate mg-gate))

;; ---------------------------------------------------------------------------
;; CLI layer
;; ---------------------------------------------------------------------------

(define (parse-close-gate-args args)
  (match args
    ['() (hash 'mode 'help)]
    [(list "--help") (hash 'mode 'help)]
    [(list n) (hash 'mode 'run 'milestone-number (string->number n) 'json #f)]
    [(list n "--json") (hash 'mode 'run 'milestone-number (string->number n) 'json #t)]
    [(list "--json" n) (hash 'mode 'run 'milestone-number (string->number n) 'json #t)]
    [_ (hash 'mode 'help)]))

(define (print-usage)
  (displayln "USAGE:")
  (displayln "  racket scripts/milestone-close-gate.rkt <milestone-number>")
  (displayln "  racket scripts/milestone-close-gate.rkt <milestone-number> --json")
  (displayln "")
  (displayln "EXIT CODES:")
  (displayln "  0 — all gates pass, milestone may be closed")
  (displayln "  1 — one or more gates fail, milestone must NOT be closed"))

(define (main args)
  (define opts (parse-close-gate-args args))
  (cond
    [(equal? (hash-ref opts 'mode) 'help) (print-usage)]
    [else
     (define milestone-number (hash-ref opts 'milestone-number))
     (unless (file-exists? "main.rkt")
       (printf "ERROR: Run from the q/ directory~n")
       (exit 1))
     (define results (run-all-gates milestone-number))
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
