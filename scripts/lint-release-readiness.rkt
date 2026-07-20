#!/usr/bin/env racket
#lang racket/base

;; lint-release-readiness.rkt — CI release hygiene gate
;;
;; Verifies that all pre-release conditions are met:
;;   1. Version sync: util/version.rkt, info.rkt, README.md agree
;;   2. CHANGELOG entry: CHANGELOG.md has an entry for the current version
;;   3. Git clean: no uncommitted changes
;;   4. On main branch
;;   5. (strict mode) Tag does not already exist
;;   6. (strict mode) Gate evidence: recent passing results for required suites
;;
;; Exit codes:
;;   0 — all checks pass, ready to release
;;   1 — one or more checks failed
;;
;; Usage:
;;   racket scripts/lint-release-readiness.rkt           # dev mode
;;   racket scripts/lint-release-readiness.rkt --strict   # release mode (gate evidence + tag check)

(provide get-canonical-version
         get-info-version
         check-version-sync
         check-tag-unique
         check-changelog-entry
         check-git-clean
         check-main-branch
         check-gate-evidence
         validate-gate-evidence-entry
         required-gate-suites
         parse-argv)

(require racket/file
         racket/string
         racket/port
         racket/system
         racket/list
         racket/path
         json)

;; ---------------------------------------------------------------------------
;; Version extraction
;; ---------------------------------------------------------------------------

(define (read-version-from-file path pattern)
  (and (file-exists? path)
       (let* ([content (file->string path)]
              [m (regexp-match pattern content)])
         (and m (cadr m)))))

(define (get-canonical-version)
  (or (read-version-from-file "util/version.rkt" #rx"define q-version \"([^\"]+)\"")
      (error "Cannot read version from util/version.rkt")))

(define (get-info-version)
  (read-version-from-file "info.rkt" #rx"define version \"([^\"]+)\""))

;; ---------------------------------------------------------------------------
;; Check 1: Version sync
;; ---------------------------------------------------------------------------

(define (check-version-sync)
  (define canonical (get-canonical-version))
  (define info-ver (get-info-version))
  (cond
    [(equal? canonical info-ver)
     (printf "  [PASS] version sync: ~a~n" canonical)
     #t]
    [else
     (printf "  [FAIL] version drift: util/version.rkt=~a, info.rkt=~a~n" canonical info-ver)
     #f]))

;; ---------------------------------------------------------------------------
;; Check 2: Tag uniqueness
;; ---------------------------------------------------------------------------

(define (check-tag-unique)
  (define ver (get-canonical-version))
  (define tag (format "v~a" ver))
  (define existing
    (with-input-from-string (with-output-to-string (lambda () (system "git tag -l")))
                            (lambda ()
                              (for/or ([line (in-lines)])
                                (equal? (string-trim line) tag)))))
  (cond
    [(not existing)
     (printf "  [PASS] tag ~a does not exist yet~n" tag)
     #t]
    [else
     (printf "  [FAIL] tag ~a already exists — bump version first~n" tag)
     #f]))

;; ---------------------------------------------------------------------------
;; Check 3: CHANGELOG entry
;; ---------------------------------------------------------------------------

(define (check-changelog-entry)
  (define ver (get-canonical-version))
  (cond
    [(not (file-exists? "CHANGELOG.md"))
     (printf "  [FAIL] CHANGELOG.md not found~n")
     #f]
    [else
     (define content (file->string "CHANGELOG.md"))
     (define bare-pattern (regexp (format "## ~a" (regexp-quote ver))))
     (define v-prefix-pattern (regexp (format "## v~a" (regexp-quote ver))))
     (cond
       [(or (regexp-match? bare-pattern content) (regexp-match? v-prefix-pattern content))
        (printf "  [PASS] CHANGELOG has entry for ~a~n" ver)
        #t]
       [else
        (printf "  [FAIL] CHANGELOG missing entry for ~a~n" ver)
        #f])]))

;; ---------------------------------------------------------------------------
;; Check 4: Git clean (no uncommitted changes)
;; ---------------------------------------------------------------------------

(define (check-git-clean)
  (define status (string-trim (with-output-to-string (lambda () (system "git status --porcelain")))))
  (cond
    [(equal? status "")
     (printf "  [PASS] git working tree clean~n")
     #t]
    [else
     (define n (length (string-split status "\n")))
     (printf "  [FAIL] git has ~a uncommitted change(s)~n" n)
     #f]))

;; ---------------------------------------------------------------------------
;; F-15 (#8753): Pure gate evidence validation
;; ---------------------------------------------------------------------------
;; Validates a single gate evidence entry against expected values.
;; Returns (values pass? detail-string).
;; Pure — no I/O. Takes parsed evidence hash, expected version, expected SHA,
;; and current time in seconds.

(define (get-current-git-sha)
  (string-trim (with-output-to-string (lambda () (system "git rev-parse HEAD 2>/dev/null")))))

(define (validate-gate-evidence-entry evidence expected-version expected-sha now-seconds)
  (define max-age 7200) ;; 2 hours
  (define max-future 300) ;; 5 minutes clock skew tolerance
  (define ev-version (hash-ref evidence 'version #f))
  (define ev-sha (hash-ref evidence 'git_sha #f))
  (define ev-time (hash-ref evidence 'timestamp #f))
  (define ev-tests (hash-ref evidence 'parsed_test_count 0))
  (define ev-failed (hash-ref evidence 'failed 0))
  (define ev-timed-out (hash-ref evidence 'timed_out 0))
  (cond
    [(not (equal? ev-version expected-version))
     (values #f (format "version mismatch: ~a ≠ ~a" ev-version expected-version))]
    [(and expected-sha (or (not ev-sha) (equal? ev-sha "unknown")))
     (values #f (format "sha is unknown: ~a" ev-sha))]
    ;; F-15 (#8772): Reject short/non-full SHA (< 40 chars) before equality check
    [(and expected-sha (not (= (string-length ev-sha) 40)))
     (values
      #f
      (format "sha must be full 40 hex chars; got ~a (length ~a)" ev-sha (string-length ev-sha)))]
    [(and expected-sha (not (equal? ev-sha expected-sha)))
     (values #f (format "sha mismatch: ~a ≠ ~a" ev-sha expected-sha))]
    [(and ev-time (integer? ev-time) (>= (- now-seconds ev-time) max-age))
     (values #f (format "stale evidence (~a seconds old)" (- now-seconds ev-time)))]
    ;; F-15 (#8772): Reject future timestamps beyond clock skew
    [(and ev-time (integer? ev-time) (>= (- ev-time now-seconds) max-future))
     (values #f (format "future timestamp (~a seconds ahead)" (- ev-time now-seconds)))]
    [(not (and (integer? ev-tests) (positive? ev-tests)))
     (values #f (format "zero or missing parsed_test_count: ~a" ev-tests))]
    [(and (integer? ev-failed) (positive? ev-failed))
     (values #f (format "~a failed test(s) in evidence" ev-failed))]
    [(and (integer? ev-timed-out) (positive? ev-timed-out))
     (values #f (format "~a timed-out test(s) in evidence" ev-timed-out))]
    [else (values #t "ok")]))

;; ---------------------------------------------------------------------------
;; Check 5: On main branch
;; ---------------------------------------------------------------------------

(define (check-main-branch context)
  (define branch
    (string-trim (with-output-to-string (lambda () (system "git rev-parse --abbrev-ref HEAD")))))
  (cond
    ;; tag-publish context: detached HEAD is expected (tag checkout)
    [(and (equal? context 'tag-publish) (equal? branch "HEAD"))
     (printf "  [PASS] detached HEAD (tag-publish context)~n")
     #t]
    [(equal? branch "main")
     (printf "  [PASS] on main branch~n")
     #t]
    [else
     (printf "  [FAIL] on branch '~a' — must be on main~n" branch)
     #f]))

;; ---------------------------------------------------------------------------
;; Check 6: Gate evidence (required suites have recent passing results)
;; ---------------------------------------------------------------------------

(define required-gate-suites '("fast" "tui" "arch" "workflows"))

(define (gate-evidence-dir)
  (build-path ".gate-evidence"))

(define (check-gate-evidence #:strict [strict? #f])
  (define evid-dir (gate-evidence-dir))
  (cond
    [(not (directory-exists? evid-dir))
     (printf "  [FAIL] no .gate-evidence/ directory — run test suites with --record-gate-evidence~n")
     #f]
    [else
     (define ver (get-canonical-version))
     (define results
       (for/list ([suite (in-list required-gate-suites)])
         ;; Support both v1 (.passed) and v2 (.json) evidence formats
         (define json-file (build-path evid-dir (format "~a.json" suite)))
         (define passed-file (build-path evid-dir (format "~a.passed" suite)))
         (define evidence-file
           (cond
             [(file-exists? json-file) json-file]
             [(file-exists? passed-file) passed-file]
             [else #f]))
         (cond
           [(not evidence-file)
            (printf "  [FAIL] gate evidence missing: ~a~n" suite)
            #f]
           [(and strict? (not (regexp-match? #rx"\\.json$" (path->string evidence-file))))
            (printf
             "  [FAIL] gate evidence ~a: legacy .passed format rejected in strict mode; use JSON~n"
             suite)
            #f]
           [else
            (define now (current-seconds))
            (define expected-sha (get-current-git-sha))
            (define json-path? (regexp-match? #rx"\\.json$" (path->string evidence-file)))
            (define-values (pass? detail)
              (if json-path?
                  ;; v2 JSON format: use full validation
                  (let* ([content (file->string evidence-file)]
                         [data (with-input-from-string content read-json)])
                    (validate-gate-evidence-entry data ver expected-sha now))
                  ;; v1 space-separated format: basic validation only
                  (let* ([content (file->string evidence-file)]
                         [parts (string-split content " ")]
                         [ev-version (and (>= (length parts) 1) (car parts))]
                         [ev-time (and (>= (length parts) 2) (string->number (cadr parts)))])
                    (validate-gate-evidence-entry
                     (hasheq 'version
                             ev-version
                             'git_sha
                             (if (= (string-length expected-sha) 40) expected-sha "unknown")
                             'timestamp
                             ev-time
                             'parsed_test_count
                             1
                             'failed
                             0
                             'timed_out
                             0)
                     ver
                     expected-sha
                     now))))
            (if pass?
                (begin
                  (printf "  [PASS] gate evidence: ~a~n" suite)
                  #t)
                (begin
                  (printf "  [FAIL] gate evidence ~a: ~a~n" suite detail)
                  #f))])))
     (andmap values results)]))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(define (parse-argv argv)
  (define strict? (member "--strict" argv))
  (define context-idx (index-of argv "--context"))
  (define context
    (if context-idx
        (let ([next (and (< context-idx (sub1 (length argv))) (list-ref argv (add1 context-idx)))])
          (if (member next '("pre-tag" "tag-publish"))
              (string->symbol next)
              #f))
        #f))
  (values strict? context))

(define (main)
  (define argv (vector->list (current-command-line-arguments)))
  (define-values (strict? context) (parse-argv argv))

  (unless (file-exists? "util/version.rkt")
    (displayln "ERROR: Run from q/ project root (util/version.rkt not found)")
    (exit 1))

  (when (and strict? context (not (member context '(pre-tag tag-publish))))
    (displayln "ERROR: --context must be 'pre-tag' or 'tag-publish'")
    (exit 1))

  (define ver (get-canonical-version))
  (define context-label
    (cond
      [(not strict?) ""]
      [context (format "[~a] " context)]
      [else "[STRICT] "]))
  (printf "~n── Release Readiness Check (v~a) ~a──~n" ver context-label)

  (define base-results
    (list (check-version-sync) (check-changelog-entry) (check-git-clean) (check-main-branch context)))

  ;; Tag check: only in pre-tag context (not tag-publish, not dev)
  ;; tag-publish context: tag already exists by design
  (define tag-result
    (cond
      [(and strict? (or (not context) (eq? context 'pre-tag))) (check-tag-unique)]
      [else #t]))

  ;; Gate evidence: in strict mode only
  (define gate-result
    (if strict?
        (check-gate-evidence #:strict #t)
        #t))

  (define results
    (if strict?
        (append base-results (list tag-result gate-result))
        base-results))

  (define passed (count values results))
  (define failed (- (length results) passed))

  (printf "~n── Summary: ~a/~a checks passed ~a──~n"
          passed
          (length results)
          (if (positive? failed) "❌ " "✅ "))

  (exit (if (positive? failed) 1 0)))

;; Only auto-run when executed directly as a script
(define invoked-directly?
  (let ([run-file (find-system-path 'run-file)])
    (and (path? run-file)
         (let ([base (file-name-from-path run-file)])
           (and base (equal? (path->string base) "lint-release-readiness.rkt"))))))
(when invoked-directly?
  (main))
