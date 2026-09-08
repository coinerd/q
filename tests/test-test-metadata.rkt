#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: unit
;; Tests for test-metadata.rkt metadata parser (v0.83.2 W0)

(require rackunit
         racket/file
         racket/port
         racket/runtime-path)

(define-runtime-path meta-path "../scripts/test-metadata.rkt")

(define meta-loaded? (box #f))
(define meta-cache (make-hash))

(define (meta-ref sym)
  (unless (unbox meta-loaded?)
    (dynamic-require meta-path #f)
    (set-box! meta-loaded? #t))
  (hash-ref! meta-cache sym (lambda () (dynamic-require meta-path sym))))

;; ---------------------------------------------------------------------------
;; parse-test-metadata — synthetic files
;; ---------------------------------------------------------------------------

(define (with-temp-test-file content proc)
  (define tmp (make-temporary-file "test-meta-~a.rkt"))
  (call-with-output-file tmp (lambda (out) (display content out)) #:exists 'truncate)
  (define result (proc tmp))
  (delete-file tmp)
  result)

(test-case "parse-test-metadata: no annotations returns all-#f"
  (define parse (meta-ref 'parse-test-metadata))
  (with-temp-test-file "#lang racket\n(require rackunit)\n(check-true #t)\n"
                       (lambda (f)
                         (define m (parse f))
                         (check-false ((meta-ref 'metadata-suite) m))
                         (check-false ((meta-ref 'metadata-boundary) m))
                         (check-false ((meta-ref 'metadata-speed) m))
                         (check-false ((meta-ref 'metadata-timeout) m))
                         (check-equal? ((meta-ref 'metadata-warnings) m) '()))))

(test-case "parse-test-metadata: parses @suite annotation"
  (define parse (meta-ref 'parse-test-metadata))
  (with-temp-test-file "#lang racket\n;; @suite runtime\n(require rackunit)\n"
                       ;; @boundary integration
                       (lambda (f)
                         (define m (parse f))
                         (check-equal? ((meta-ref 'metadata-suite) m) "runtime")
                         (check-equal? ((meta-ref 'metadata-warnings) m) '()))))

(test-case "parse-test-metadata: parses all annotations"
  (define parse (meta-ref 'parse-test-metadata))
  (with-temp-test-file (string-append "#lang racket\n"
                                      ";; @suite tui\n"
                                      ";; @boundary integration\n"
                                      ";; @speed slow\n"
                                      ";; @mutates cwd\n"
                                      ";; @isolation temp-dir\n"
                                      ";; @timeout 30\n"
                                      "(require rackunit)\n")
                       (lambda (f)
                         (define m (parse f))
                         (check-equal? ((meta-ref 'metadata-suite) m) "tui")
                         (check-equal? ((meta-ref 'metadata-boundary) m) "integration")
                         (check-equal? ((meta-ref 'metadata-speed) m) "slow")
                         (check-equal? ((meta-ref 'metadata-mutates) m) "cwd")
                         (check-equal? ((meta-ref 'metadata-isolation) m) "temp-dir")
                         (check-equal? ((meta-ref 'metadata-timeout) m) 30)
                         (check-equal? ((meta-ref 'metadata-warnings) m) '()))))

(test-case "parse-test-metadata: warns on invalid @suite value"
  (define parse (meta-ref 'parse-test-metadata))
  (with-temp-test-file "#lang racket\n;; @suite invalid-suite\n(require rackunit)\n"
                       (lambda (f)
                         (define m (parse f))
                         (check-equal? ((meta-ref 'metadata-suite) m) "invalid-suite")
                         (define warnings ((meta-ref 'metadata-warnings) m))
                         (check-not-false (ormap (lambda (w) (regexp-match? #rx"invalid value" w))
                                                 warnings)))))

(test-case "parse-test-metadata: warns on invalid @timeout value"
  (define parse (meta-ref 'parse-test-metadata))
  (with-temp-test-file "#lang racket\n;; @timeout abc\n(require rackunit)\n"
                       (lambda (f)
                         (define m (parse f))
                         (define warnings ((meta-ref 'metadata-warnings) m))
                         (check-not-false (ormap (lambda (w) (regexp-match? #rx"invalid value" w))
                                                 warnings)))))

(test-case "parse-test-metadata: handles missing file gracefully"
  (define parse (meta-ref 'parse-test-metadata))
  (define m (parse "/nonexistent/path/test.rkt"))
  (check-false ((meta-ref 'metadata-suite) m)))

(test-case "parse-test-metadata: only scans first 30 lines"
  (define parse (meta-ref 'parse-test-metadata))
  ;; Generate 40 lines, annotation on line 35 — should NOT be picked up
  (define content
    (string-append "#lang racket\n"
                   (apply string-append
                          (for/list ([i (in-range 33)])
                            (format ";; line ~a\n" i)))
                   ";; @suite security\n"
                   "(require rackunit)\n"))
  (with-temp-test-file content
                       (lambda (f)
                         (define m (parse f))
                         (check-false ((meta-ref 'metadata-suite) m)))))

;; ---------------------------------------------------------------------------
;; scan-files-metadata
;; ---------------------------------------------------------------------------

(test-case "scan-files-metadata: returns metadata per file"
  (define scan (meta-ref 'scan-files-metadata))
  (define parse (meta-ref 'parse-test-metadata))
  (define tmp1 (make-temporary-file "scan-~a.rkt"))
  (define tmp2 (make-temporary-file "scan-~a.rkt"))
  (call-with-output-file tmp1 (lambda (out) (display ";; @suite fast\n" out)) #:exists 'truncate)
  (call-with-output-file tmp2 (lambda (out) (display "#lang racket\n" out)) #:exists 'truncate)
  (define results (scan (list tmp1 tmp2)))
  (check-equal? (length results) 2)
  (check-equal? ((meta-ref 'metadata-file) (first results)) tmp1)
  (delete-file tmp1)
  (delete-file tmp2))

;; ---------------------------------------------------------------------------
;; metadata-report
;; ---------------------------------------------------------------------------

(test-case "metadata-report: outputs scan summary"
  (define report (meta-ref 'metadata-report))
  (define parse (meta-ref 'parse-test-metadata))
  (define tmp (make-temporary-file "report-~a.rkt"))
  (call-with-output-file tmp
                         (lambda (out) (display ";; @suite all\n;; @boundary unit\n" out))
                         #:exists 'truncate)
  (define m (parse tmp))
  (define output (with-output-to-string (lambda () (report (list m)))))
  (check-not-false (regexp-match? #rx"1 files scanned, 1 tagged, 0 untagged" output))
  (delete-file tmp))

(test-case "metadata-report: reports untagged count"
  (define report (meta-ref 'metadata-report))
  (define parse (meta-ref 'parse-test-metadata))
  (define tmp (make-temporary-file "report-~a.rkt"))
  (call-with-output-file tmp (lambda (out) (display "#lang racket\n" out)) #:exists 'truncate)
  (define m (parse tmp))
  (define output (with-output-to-string (lambda () (report (list m)))))
  (check-not-false (regexp-match? #rx"0 tagged, 1 untagged" output))
  (delete-file tmp))

;; ---------------------------------------------------------------------------
;; tier vocabulary consistency (W1)
;; ---------------------------------------------------------------------------

;; The documented tier vocabulary must match the runner CLI: `fast` (the
;; broad PR regression tier, what CI runs per PR) and `unit-fast` (the
;; developer iteration tier with a declared local p90 SLO, measured in W4)
;; exist as runner suites, and every suite named via `--suite X` in the
;; docs' Suites and Tier semantics tables is a real CLI suite. Docs and
;; naming only — no runner behavior is asserted here.

(require racket/string
         rackunit/text-ui
         (only-in "../scripts/run-tests/cli.rkt" known-suites))

(define-runtime-path cli-path "../scripts/run-tests/cli.rkt")
(define (cli-ref sym)
  (dynamic-require cli-path sym))

;; Extract a "## <heading>" section body: from the heading line to the next
;; top-level "## " heading (or end of file).
(define (doc-section doc heading-rx)
  (define m (regexp-match-positions heading-rx doc))
  (unless m
    (raise-user-error 'tier-semantics "docs section missing: ~a" heading-rx))
  (define start (cdr (car m)))
  (define rest (substring doc start))
  (define next (regexp-match-positions #rx"\n## " rest))
  (if next
      (substring rest 0 (caar next))
      rest))

;; Every `--suite <name>` selector named in a docs section.
(define (suite-names-in-section doc heading-rx)
  (map string->symbol
       (regexp-match* #rx"`--suite ([a-z0-9-]+)`" (doc-section doc heading-rx) #:match-select cadr)))

(define tier-vocabulary-suite
  (test-suite "tier vocabulary consistency (W1)"

    (test-case "the two tier names of record exist as runner suites"
      (define suites (cli-ref 'known-suites))
      (check-not-false (memq 'fast suites) "fast is not a known suite")
      (check-not-false (memq 'unit-fast suites) "unit-fast is not a known suite"))

    (test-case "every Suites-table command names a real CLI suite"
      (define doc (file->string "docs/TEST_CONVENTIONS.md"))
      (define suites (cli-ref 'known-suites))
      (define names (suite-names-in-section doc #rx"(?m:^## Suites)"))
      (check-true (pair? names) "no --suite selectors found in the Suites table")
      (for ([n (in-list names)])
        (check-not-false (memq n suites) (format "~a is not a known suite" n))))

    (test-case "tier semantics table names both tiers as --suite selectors"
      (define doc (file->string "docs/TEST_CONVENTIONS.md"))
      (define names (suite-names-in-section doc #rx"(?m:^## Tier semantics)"))
      (check-not-false (memq 'fast names) "tier table does not name the fast tier")
      (check-not-false (memq 'unit-fast names) "tier table does not name the unit-fast tier")
      (for ([n (in-list names)])
        (check-not-false (memq n (cli-ref 'known-suites)) (format "~a is not a known suite" n))))))

(run-tests tier-vocabulary-suite)
