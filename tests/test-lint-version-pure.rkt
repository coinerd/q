#lang racket

;; @speed fast
;; @suite default

;; test-lint-version-pure.rkt — Tests for pure check functions extracted from
;; lint-version.rkt via the I/O abstraction (W6 #8419).
;;
;; These tests exercise the pure check functions directly with content strings,
;; AND test the I/O wrappers with mock file systems via lint-version-io.rkt.

(require rackunit
         rackunit/text-ui
         racket/runtime-path)

(define-runtime-path script-path "../scripts/lint-version.rkt")
(define-runtime-path io-path "../scripts/lint-version-io.rkt")

(define (lv-ref sym)
  (dynamic-require script-path sym))

(define (io-ref sym)
  (dynamic-require io-path sym))

;; ---------------------------------------------------------------------------
;; Pure function tests (no I/O)
;; ---------------------------------------------------------------------------

(define-test-suite
 lint-version-pure-tests
 ;; --- check-info-content ---
 (test-case "check-info-content: matching version returns no errors"
   (define check (lv-ref 'check-info-content))
   (define content "(define version \"1.0.0\")")
   (check-equal? (check content "1.0.0") '()))
 (test-case "check-info-content: mismatched version returns error"
   (define check (lv-ref 'check-info-content))
   (define content "(define version \"0.9.0\")")
   (define errors (check content "1.0.0"))
   (check-equal? (length errors) 1))
 (test-case "check-info-content: unparseable content returns error"
   (define check (lv-ref 'check-info-content))
   (define content "(define something-else \"1.0.0\")")
   (define errors (check content "1.0.0"))
   (check-equal? (length errors) 1))
 ;; --- check-readme-content ---
 (test-case "check-readme-content: matching badge and verify"
   (define check (lv-ref 'check-readme-content))
   (define content "badge/version-1.0.0-blue and q version 1.0.0-blue")
   (check-equal? (check content "1.0.0") '()))
 (test-case "check-readme-content: mismatched badge"
   (define check (lv-ref 'check-readme-content))
   (define content "badge/version-0.9.0-blue and q version 1.0.0-blue")
   (define errors (check content "1.0.0"))
   (check-equal? (length errors) 1)
   (check-equal? (first (first errors)) 'README.md))
 (test-case "check-readme-content: mismatched verify snippet"
   (define check (lv-ref 'check-readme-content))
   (define content "badge/version-1.0.0-blue and q version 0.9.0-blue")
   (define errors (check content "1.0.0"))
   (check-equal? (length errors) 1))
 (test-case "check-readme-content: no version strings in content"
   (define check (lv-ref 'check-readme-content))
   (define content "# Just a readme with no versions")
   (check-equal? (check content "1.0.0") '()))
 ;; --- check-changelog-content ---
 (test-case "check-changelog-content: sufficient versions → no errors"
   (define check (lv-ref 'check-changelog-content))
   ;; Generate 50 version headers
   (define content
     (string-join (for/list ([i (in-range 1 51)])
                    (format "## v0.~a.0" i))
                  "\n"))
   (check-equal? (check content) '()))
 (test-case "check-changelog-content: too few versions → error"
   (define check (lv-ref 'check-changelog-content))
   (define content "## v1.0.0\n## v1.0.1\n## v1.0.2")
   (define errors (check content))
   (check-equal? (length errors) 1)
   (check-equal? (first (first errors)) 'CHANGELOG.md))
 (test-case "check-changelog-content: duplicate release entries fail"
   (define check (lv-ref 'check-changelog-content))
   (define content
     (string-join (append (for/list ([i (in-range 1 50)])
                            (format "## v0.~a.0" i))
                          '("## 1.2.3" "## v1.2.3 — 2026-07-29"))
                  "\n"))
   (define errors (check content))
   (check-true (ormap (lambda (e) (regexp-match? #rx"duplicate release entries" (third e))) errors)))
 (test-case "check-changelog-content: canonical dated and unreleased headings count"
   (define check (lv-ref 'check-changelog-content))
   (define content
     (string-join (append '("## v1.2.3 — 2026-07-29" "## 1.2.2 — Unreleased")
                          (for/list ([i (in-range 1 49)])
                            (format "## 0.~a.0" i)))
                  "\n"))
   (check-equal? (check content) '())))

;; ---------------------------------------------------------------------------
;; I/O wrapper tests with mock file system
;; ---------------------------------------------------------------------------

(define-test-suite
 lint-version-mock-io-tests
 (test-case "check-info-rkt with mock fs: matching version"
   (define check-info-rkt (lv-ref 'check-info-rkt))
   (define make-mock-fs (io-ref 'make-mock-fs))
   (define make-mock-exists (io-ref 'make-mock-exists))
   (define make-mock-read (io-ref 'make-mock-read-string))
   (define exists-param (io-ref 'current-lint-file-exists?))
   (define read-param (io-ref 'current-lint-file->string))
   (define fs
     (make-mock-fs (list (cons (path->string (build-path (current-directory) "info.rkt"))
                               "(define version \"1.0.0\")"))))
   (parameterize ([exists-param (make-mock-exists fs)]
                  [read-param (make-mock-read fs)])
     ;; The check-info-rkt function prints and returns; check return value
     (define result (with-output-to-string (λ () (check-info-rkt "1.0.0"))))
     (check-true (string-contains? result "OK"))))
 (test-case "check-info-rkt with mock fs: mismatched version"
   (define check-info-rkt (lv-ref 'check-info-rkt))
   (define make-mock-fs (io-ref 'make-mock-fs))
   (define make-mock-exists (io-ref 'make-mock-exists))
   (define make-mock-read (io-ref 'make-mock-read-string))
   (define exists-param (io-ref 'current-lint-file-exists?))
   (define read-param (io-ref 'current-lint-file->string))
   (define fs
     (make-mock-fs (list (cons (path->string (build-path (current-directory) "info.rkt"))
                               "(define version \"0.9.0\")"))))
   (parameterize ([exists-param (make-mock-exists fs)]
                  [read-param (make-mock-read fs)])
     ;; Capture output to suppress printing
     (define result (with-output-to-string (λ () (check-info-rkt "1.0.0"))))
     (check-true (string-contains? result "ERROR")))))

(define-test-suite
 lint-version-skip-path-tests
 (test-case "skip-path? excludes docs/planning scaffolds"
   (define skip-path? (lv-ref 'skip-path?))
   (check-true (skip-path? (build-path (current-directory)
                                       "docs"
                                       "planning"
                                       "PLAN-v0.99.92-SESSION-LIFECYCLE-SERIES-CLOSURE.md")))
   (check-true (skip-path? (build-path (current-directory) "docs" "planning" "v0.99.91.json")))
   (check-true (skip-path? (build-path (current-directory) ".planning" "PLAN.md"))))
 (test-case "skip-path? still scans normal docs"
   (define skip-path? (lv-ref 'skip-path?))
   (check-false (skip-path? (build-path (current-directory) "docs" "system-overview.md")))
   (check-false (skip-path? (build-path (current-directory) "docs" "architecture" "overview.md")))))

(run-tests lint-version-skip-path-tests)

(run-tests lint-version-pure-tests)
(run-tests lint-version-mock-io-tests)
