#!/usr/bin/env racket
#lang racket

;; scripts/claim-verifier.rkt — Claim verification tool (GAP-1).
;;
;; W1 (#8683): Auto-counts test-cases and check assertions, then compares
;; them against claims in markdown report files. Prevents inaccurate test
;; counts from being published.
;;
;; This tool exists because v0.99.45 published a "663 tests" claim that did
;; not match reality, and the discrepancy was only caught post-hoc by an
;; in-depth audit. Claim verification should be PROACTIVE, not reactive.
;;
;; Layering (matches status-result.rkt pattern):
;;   ├── Pure functions (testable without file I/O)
;;   │   ├── count-test-cases-in-text
;;   │   ├── count-check-assertions-in-text
;;   │   ├── extract-claimed-counts
;;   │   ├── extract-test-file-refs
;;   │   └── verify-claims
;;   ├── File I/O layer
;;   │   ├── count-test-cases-in-file
;;   │   └── scan-report-file
;;   ├── CLI layer
;;   │   ├── parse-claim-verifier-args
;;   │   └── main
;;   └── Tests in tests/test-claim-verifier.rkt
;;
;; Usage:
;;   cd q/ && racket scripts/claim-verifier.rkt                    # count tests
;;   cd q/ && racket scripts/claim-verifier.rkt --check report.md  # verify claims
;;   cd q/ && racket scripts/claim-verifier.rkt --milestone 834    # scan all reports

(require racket/file
         racket/match)

;; ---------------------------------------------------------------------------
;; Result types
;; ---------------------------------------------------------------------------

;; A claim-result records one verified claim.
;;   pattern  — claim type symbol ('tests, 'check-assertions, etc.)
;;   claimed  — number stated in the report
;;   actual   — number found by recounting
;;   matched? — #t if claimed equals actual
(struct claim-result
        (pattern claimed
          actual
          matched?)
  #:transparent)

;; ---------------------------------------------------------------------------
;; Pure functions
;; ---------------------------------------------------------------------------

(provide claim-result
         claim-result?
         claim-result-pattern
         claim-result-claimed
         claim-result-actual
         claim-result-matched?
         count-test-cases-in-text
         count-check-assertions-in-text
         extract-claimed-counts
         extract-test-file-refs
         verify-claims
         count-test-cases-in-file
         scan-report-file
         parse-claim-verifier-args
         main)

;; Count (test-case forms in text. Only matches the open paren form, not
;; the word "test-case" appearing inside a string. The literal open paren
;; must be escaped (\\() so it is not treated as a regex group.
(define (count-test-cases-in-text text)
  (length (regexp-match-positions* #rx"\\(test-case" text)))

;; Count (check- assertion forms in text. Matches check-equal?, check-true,
;; check-not-false, etc. Only matches the open paren form.
(define (count-check-assertions-in-text text)
  (length (regexp-match-positions* #rx"\\(check-[-a-zA-Z]+" text)))

;; Strip integer commas: "1,626" → 1626.
(define (parse-count-number s)
  (string->number (string-replace s "," "")))

;; The claim-count regex patterns. Each is a (key . pattern) pair.
;; Patterns are compiled pregexp (PCRE) so ? and + quantifiers work.
(define claim-count-patterns
  (list (cons 'tests (pregexp "([0-9][0-9,]*)[ ]+tests?"))
        (cons 'test-files (pregexp "([0-9][0-9,]*)[ ]+test[ ]+files?"))
        (cons 'test-cases (pregexp "([0-9][0-9,]*)[ ]+test-cases"))
        (cons 'check-assertions (pregexp "([0-9][0-9,]*)[ ]+check[ ]+assertions?"))
        (cons 'check-assertions (pregexp "([0-9][0-9,]*)[ ]+test[ ]+assertions?"))))

;; Extract claimed counts from report text.
;; Returns an association list: '((tests . 286) (test-files . 11) ...)
(define (extract-claimed-counts text)
  (define claims '())
  (define (add! key n)
    (unless (assoc key claims)
      (set! claims (cons (cons key n) claims))))
  (for ([kp (in-list claim-count-patterns)])
    (define key (car kp))
    (define pat (cdr kp))
    (define m (regexp-match pat text))
    (when m
      (define n (parse-count-number (cadr m)))
      (when n
        (add! key n))))
  (reverse claims))

;; Extract test file references (test-*.rkt) from text.
(define (extract-test-file-refs text)
  (remove-duplicates (regexp-match* #rx"test-[-a-zA-Z0-9_./]*\\.rkt" text)))

;; Verify that claimed counts match actual counts.
;;   claims — assoc list from extract-claimed-counts
;;   actual — optional assoc list of actual counts (defaults to empty)
;; Returns a list of claim-result structs.
(define (verify-claims claims [actual '()])
  (for/list ([claim (in-list claims)])
    (define key (car claim))
    (define claimed (cdr claim))
    (define actual-n
      (cond
        [(assoc key actual)
         =>
         cdr]
        [else 0]))
    (claim-result key claimed actual-n (= claimed actual-n))))

;; ---------------------------------------------------------------------------
;; File I/O layer
;; ---------------------------------------------------------------------------

;; Count (test-case forms in a file.
(define (count-test-cases-in-file path)
  (count-test-cases-in-text (file->string path)))

;; Scan a report file for claims and verify them.
;; Returns a list of claim-result structs.
(define (scan-report-file path)
  (define text (file->string path))
  (extract-claimed-counts text))

;; ---------------------------------------------------------------------------
;; CLI layer
;; ---------------------------------------------------------------------------

(define (parse-claim-verifier-args args)
  (match args
    ['() (hash 'mode 'count)]
    [(list "--check" file) (hash 'mode 'check 'file file)]
    [(list "--milestone" n) (hash 'mode 'milestone 'milestone-number (string->number n))]
    [(list "--help") (hash 'mode 'help)]
    [_ (hash 'mode 'help)]))

(define (print-usage)
  (displayln "USAGE:")
  (displayln "  racket scripts/claim-verifier.rkt                    # count tests in tests/")
  (displayln "  racket scripts/claim-verifier.rkt --check <file.md>  # verify claims in report")
  (displayln
   "  racket scripts/claim-verifier.rkt --milestone <N>    # scan all reports for milestone"))

(define (count-mode)
  (define test-dir "tests")
  (define all-paths (sequence->list (in-directory test-dir)))
  (define test-files (filter (λ (p) (string-suffix? (path->string p) ".rkt")) all-paths))
  (define total-test-cases (for/sum ([f (in-list test-files)]) (count-test-cases-in-file f)))
  (printf "~a test-cases across ~a test files~n" total-test-cases (length test-files)))

(define (check-mode file)
  (unless (file-exists? file)
    (printf "ERROR: file not found: ~a~n" file)
    (exit 1))
  (define claims (scan-report-file file))
  (if (null? claims)
      (printf "~a: no claims found~n" file)
      (begin
        (printf "~a:~n" file)
        (for ([c (in-list claims)])
          (printf "  ~a: ~a~n" (car c) (cdr c))))))

(define (milestone-mode n)
  (printf "Scanning docs/reports/ for milestone ~a...~n" n)
  (define reports-dir "docs/reports")
  (define files (sequence->list (in-directory reports-dir)))
  (for ([f (in-list files)])
    (define f-str (path->string f))
    (when (and (string-suffix? f-str ".md") (file-exists? f))
      (define claims (scan-report-file f-str))
      (unless (null? claims)
        (printf "~a:~n" f-str)
        (for ([c (in-list claims)])
          (printf "  ~a: ~a~n" (car c) (cdr c)))))))

(define (main args)
  (define opts (parse-claim-verifier-args args))
  (match (hash-ref opts 'mode)
    ['count (count-mode)]
    ['check (check-mode (hash-ref opts 'file))]
    ['milestone (milestone-mode (hash-ref opts 'milestone-number))]
    ['help (print-usage)]))

(module+ main
  (main (vector->list (current-command-line-arguments))))
