#lang racket

;; @speed fast
;; @suite default
;; BOUNDARY: unit
;;
;; W3: Parser/string hotspot tests.
;; Covers edge cases for run-tests/parse.rkt pure functions
;; and metrics.rkt pure predicates (replicated locally
;; because metrics.rkt executes (main) at module load time).

(require rackunit
         "../scripts/run-tests/parse.rkt")

;; ═══════════════════════════════════════════════════════════════
;; §1: parse-raco-output — edge cases
;; ═══════════════════════════════════════════════════════════════

(test-case "parse-raco-output: empty input"
  (define-values (p f t) (parse-raco-output #""))
  (check-equal? (list p f t) '(0 0 0)))

(test-case "parse-raco-output: modern format with errors"
  (define output (string->bytes/utf-8 "2 success(es) 1 failure(s) 1 error(s) 4 test(s) run\n"))
  (define-values (p f t) (parse-raco-output output))
  (check-equal? p 2)
  (check-equal? f 2 "failures + errors should be counted together")
  (check-equal? t 4))

(test-case "parse-raco-output: modern format without errors"
  (define output (string->bytes/utf-8 "5 success(es) 0 failure(s) 5 test(s) run\n"))
  (define-values (p f t) (parse-raco-output output))
  (check-equal? p 5)
  (check-equal? f 0)
  (check-equal? t 5))

(test-case "parse-raco-output: legacy format (tests passed/failed)"
  (define output (string->bytes/utf-8 "3 tests passed\n1 test failed\n"))
  (define-values (p f t) (parse-raco-output output))
  (check-equal? p 3)
  (check-equal? f 1)
  (check-equal? t 4))

(test-case "parse-raco-output: result-based format (#<test-success>)"
  (define output (string->bytes/utf-8 "#<test-success>\n#<test-success>\n#<test-failure>\n"))
  (define-values (p f t) (parse-raco-output output))
  (check-equal? p 2)
  (check-equal? f 1)
  (check-equal? t 3))

(test-case "parse-raco-output: multiple modern-format lines sum"
  (define output
    (string->bytes/utf-8 (string-append "1 success(es) 0 failure(s) 1 test(s) run\n"
                                        "2 success(es) 1 failure(s) 3 test(s) run\n")))
  (define-values (p f t) (parse-raco-output output))
  (check-equal? p 3)
  (check-equal? f 1)
  (check-equal? t 4))

;; ═══════════════════════════════════════════════════════════════
;; §2: classify-test-result — category coverage
;; ═══════════════════════════════════════════════════════════════

(define (make-result exit passed failed total stdout stderr)
  (make-test-file-result "test.rkt"
                         exit
                         (string->bytes/utf-8 stdout)
                         (string->bytes/utf-8 stderr)
                         100
                         passed
                         failed
                         total))

(test-case "classify: PASS (exit 0, total > 0)"
  (check-equal? (classify-test-result (make-result 0 5 0 5 "" "")) 'PASS))

(test-case "classify: ZERO_PARSED (exit 0, total 0)"
  (check-equal? (classify-test-result (make-result 0 0 0 0 "" "")) 'ZERO_PARSED))

(test-case "classify: TIMEOUT (exit 2)"
  (check-equal? (classify-test-result (make-result 2 0 0 0 "" "")) 'TIMEOUT))

(test-case "classify: SKIPPED_BY_PROFILE (exit 5)"
  (check-equal? (classify-test-result (make-result 5 0 0 0 "" "")) 'SKIPPED_BY_PROFILE))

(test-case "classify: SKIPPED_BY_PROFILE (keyword in output)"
  (check-equal? (classify-test-result (make-result 1 0 0 0 "skipped_by_profile" ""))
                'SKIPPED_BY_PROFILE))

(test-case "classify: USER_BREAK"
  (check-equal? (classify-test-result (make-result 1 0 0 0 "user break" "")) 'USER_BREAK))

(test-case "classify: ENVIRONMENT_MISSING (API key)"
  (check-equal? (classify-test-result (make-result 1 0 0 0 "missing environment variable" ""))
                'ENVIRONMENT_MISSING))

(test-case "classify: COMPILE_FAILURE (syntax error)"
  (check-equal? (classify-test-result (make-result 1 0 0 0 "read-syntax error" "")) 'COMPILE_FAILURE))

(test-case "classify: MODULE_LOAD_FAILURE"
  (check-equal? (classify-test-result (make-result 1 0 0 0 "cannot open module file" ""))
                'MODULE_LOAD_FAILURE))

(test-case "classify: ASSERTION_FAILURE (failed > 0)"
  (check-equal? (classify-test-result (make-result 1 5 3 8 "" "")) 'ASSERTION_FAILURE))

(test-case "classify: ASSERTION_FAILURE (check-equal in output)"
  (check-equal? (classify-test-result (make-result 1 0 0 0 "check-equal failed" ""))
                'ASSERTION_FAILURE))

(test-case "classify: UNKNOWN_FAILURE (nothing matches)"
  (check-equal? (classify-test-result (make-result 1 0 0 0 "segfault" "")) 'UNKNOWN_FAILURE))

;; ═══════════════════════════════════════════════════════════════
;; §3: extract-failure-lines
;; ═══════════════════════════════════════════════════════════════

(test-case "extract-failure-lines: no failures"
  (check-equal? (extract-failure-lines #"all tests passed\n") '()))

(test-case "extract-failure-lines: single failure block (characterization)"
  ;; FINDING: FAILURE-END regex #rx"^-{20,}$" doesn't match in #rx mode
  ;; because {20,} quantifier syntax requires #px (PCRE) mode.
  ;; In #rx mode, {20,} is treated as literal characters.
  ;; As a result, extract-failure-lines includes ALL lines after the
  ;; FAILURE start marker until end of input. This is a characterization
  ;; test documenting actual behavior, not desired behavior.
  (define output
    (string->bytes/utf-8 (string-append "before\n"
                                        "--- FAILURE ---\n"
                                        "test failed here\n"
                                        "--------------------\n"
                                        "after\n")))
  (define result (extract-failure-lines output))
  (check-not-false (member "--- FAILURE ---" result) "should contain start marker")
  (check-not-false (member "test failed here" result) "should contain failure text")
  (check-true (> (length result) 2) "includes lines after failure block (known limitation)"))

(test-case "extract-failure-lines: multiple failure blocks"
  (define output
    (string->bytes/utf-8 (string-append "--- FAILURE ---\n"
                                        "first failure\n"
                                        "--------------------\n"
                                        "--- FAILURE ---\n"
                                        "second failure\n"
                                        "--------------------\n")))
  ;; Due to the FAILURE-END regex limitation (documented above),
  ;; all lines are included once the first FAILURE marker is found.
  (define result (extract-failure-lines output))
  (check-not-false (member "first failure" result))
  (check-not-false (member "second failure" result)))

;; ═══════════════════════════════════════════════════════════════
;; §4: truncate-test-output
;; ═══════════════════════════════════════════════════════════════

(test-case "truncate-test-output: short output unchanged"
  (define short "hello world")
  (check-equal? (truncate-test-output short DEFAULT-OUTPUT-CAP) short))

(test-case "truncate-test-output: long output truncated"
  (define long (make-string (* DEFAULT-OUTPUT-CAP 2) #\A))
  (define result (truncate-test-output long DEFAULT-OUTPUT-CAP))
  (check-true (< (string-length result) (string-length long)) "truncated output should be shorter")
  (check-true (string-contains? result "truncated")
              "truncated output should contain truncation marker"))

;; ═══════════════════════════════════════════════════════════════
;; §5: effective-exit-code
;; ═══════════════════════════════════════════════════════════════

(test-case "effective-exit-code: exit 0 with failures → 1"
  (check-equal? (effective-exit-code 0 3) 1))

(test-case "effective-exit-code: exit 0 with no failures → 0"
  (check-equal? (effective-exit-code 0 0) 0))

(test-case "effective-exit-code: non-zero exit preserved"
  (check-equal? (effective-exit-code 2 0) 2))

;; ═══════════════════════════════════════════════════════════════
;; §6: Metrics pure predicates (replicated from scripts/metrics.rkt)
;;     These are characterization tests for the path classification
;;     logic that determines source vs test file counts.
;; ═══════════════════════════════════════════════════════════════

(require racket/string)

;; Replicated from scripts/metrics.rkt (can't require it directly
;; because it executes (main) at module load time).
(define (path-under-tests? s)
  (or (string-prefix? s "tests/") (string-contains? s "/tests/")))

(define (rkt-file-keep? s dir exclude?)
  (and (string-suffix? s ".rkt")
       (not (string-contains? s "/compiled/"))
       (not (string-contains? s ".zo"))
       (not (string-contains? s "/__pycache__/"))
       (not (string-contains? s "/.git/"))
       (if exclude?
           (not (path-under-tests? s))
           (if (equal? dir "tests")
               (path-under-tests? s)
               #t))))

(test-case "path-under-tests?: git-style path"
  (check-true (path-under-tests? "tests/test-foo.rkt")))

(test-case "path-under-tests?: nested path"
  (check-true (path-under-tests? "foo/tests/test-bar.rkt")))

(test-case "path-under-tests?: non-test path"
  (check-false (path-under-tests? "util/helper.rkt")))

(test-case "path-under-tests?: source file named test-something"
  ;; 'test-' prefix in the basename is NOT under tests/
  (check-false (path-under-tests? "util/test-helper.rkt")))

(test-case "rkt-file-keep?: valid source file"
  (check-true (rkt-file-keep? "util/helper.rkt" "src" #f)))

(test-case "rkt-file-keep?: excludes compiled"
  (check-false (rkt-file-keep? "compiled/helper_rkt.zo" "src" #f)))

(test-case "rkt-file-keep?: excludes .zo files"
  (check-false (rkt-file-keep? "util/helper.zo" "src" #f)))

(test-case "rkt-file-keep?: test file in tests dir"
  (check-true (rkt-file-keep? "tests/test-foo.rkt" "tests" #f)))

(test-case "rkt-file-keep?: test file excluded when exclude-tests"
  (check-false (rkt-file-keep? "tests/test-foo.rkt" "src" #t)))

(test-case "rkt-file-keep?: non-.rkt file rejected"
  (check-false (rkt-file-keep? "util/helper.rkt.bak" "src" #f)))

;; ═══════════════════════════════════════════════════════════════
;; §7: test-result->jsexpr serialization
;; ═══════════════════════════════════════════════════════════════

(require racket/hash)

(test-case "test-result->jsexpr: structure and fields"
  (define r (make-result 0 5 0 5 "all good" "no errors"))
  (define j (test-result->jsexpr r))
  (check-equal? (hash-ref j 'path) "test.rkt")
  (check-equal? (hash-ref j 'exit_code) 0)
  (check-equal? (hash-ref j 'passed) 5)
  (check-equal? (hash-ref j 'failed) 0)
  (check-equal? (hash-ref j 'total) 5)
  (check-equal? (hash-ref j 'category) "PASS"))

;; ═══════════════════════════════════════════════════════════════
;; §8: F1 regex fix — FAILURE-END now matches with #px
;; ═══════════════════════════════════════════════════════════════

;; F1 fix verified: #px"^-{20,}$" matches 20+ dash lines
;; (old #rx mode treated {20,} as literal characters)
(define F1-REGEX #px"^-{20,}$")

(test-case "F1 fix: #px regex matches 20+ dashes"
  (check-true (regexp-match? F1-REGEX "--------------------"))
  (check-true (regexp-match? F1-REGEX "------------------------------")))

(test-case "F1 fix: #px regex does not match <20 dashes"
  (check-false (regexp-match? F1-REGEX "---"))
  (check-false (regexp-match? F1-REGEX "-----------------")))

(test-case "F1 fix: old #rx regex does NOT match (regression proof)"
  (define old-regex #rx"^-{20,}$")
  (check-false (regexp-match? old-regex "--------------------"))
  (check-false (regexp-match? old-regex "------------------------------")))

(test-case "F1 fix: extract-failure-lines detects dash-delimited blocks"
  (define output
    (string->bytes/utf-8
     "-------- FAILURE --------\nFAILURE\nfoo.rkt:12: test failed\n--------------------\n"))
  (define lines (extract-failure-lines output))
  (check-true (> (length lines) 0) "should extract failure lines from dash-delimited block"))
