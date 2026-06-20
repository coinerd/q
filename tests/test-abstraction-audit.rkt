#lang racket

;; @speed fast
;; @suite fast

;; Test for scripts/abstraction-audit.rkt — W1 v0.99.35
;; Verifies the abstraction audit tool works on fixture trees,
;; produces expected categories, and does not mutate the repo.

(require rackunit
         rackunit/text-ui
         racket/port
         racket/file
         racket/list
         racket/string
         racket/path
         racket/runtime-path
         json)

(define-runtime-path audit-path "../scripts/abstraction-audit.rkt")

;; Load module dynamically
(define audit-cache (make-hash))

(define (audit-ref sym)
  (hash-ref! audit-cache sym (lambda () (dynamic-require audit-path sym))))

;; ============================================================
;; Fixture helpers
;; ============================================================

(define (setup-fixture-tree base-dir)
  (define (write-file path content)
    (make-parent-directory* path)
    (call-with-output-file* path (lambda (out) (display content out)) #:exists 'truncate))

  ;; Small clean module — no I/O, no parameters, no macros
  (write-file (build-path base-dir "clean.rkt")
              #<<EOF
#lang racket/base
(provide greet)
(define (greet name)
  (string-append "Hello, " name "!"))
EOF
              )

  ;; Big module with parameters, macros, struct-out, I/O mixed with logic,
  ;; error/raise, and provide shapes
  (write-file (build-path base-dir "subdir" "big-module.rkt")
              #<<EOF
#lang racket/base

(require racket/match)

(provide (struct-out widget)
         make-widget
         widget-name
         process-widget
         current-widget-cache)

(struct widget (name value))

(define current-widget-cache (make-parameter #f))

(define-syntax-rule (with-widget-cache body ...)
  (parameterize ([current-widget-cache (make-hash)])
    body ...))

(define (make-widget name value)
  (widget name value))

(define (widget-name w)
  (widget-name w))

(define (process-widget w)
  (if (widget? w)
      (begin
        (define name (widget-name w))
        (call-with-output-file "/tmp/widget.log"
          (lambda (out) (display name out)))
        (widget-name w))
      (error "not a widget")))
EOF
              )

  ;; Skipped directories: default audit is for production/source modules only.
  (write-file (build-path base-dir "tests" "test-skipped.rkt")
              #<<EOF
#lang racket/base
(define skipped-test #t)
EOF
              )
  (write-file (build-path base-dir "compiled" "skipped-compiled.rkt")
              #<<EOF
#lang racket/base
(define skipped-compiled #t)
EOF
              )
  ;; Module with all-defined-out
  (write-file (build-path base-dir "subdir" "ado-module.rkt")
              #<<EOF
#lang racket/base
(provide (all-defined-out))
(define (helper x) x)
(define (other y) y)
EOF
              ))

(define (cleanup-fixture-tree base-dir)
  (delete-directory/files base-dir #:must-exist? #f))

;; ============================================================
;; Test suites
;; ============================================================

(define-test-suite
 api-existence-tests
 (test-case "Core API procedures exist"
   (check-true (procedure? (audit-ref 'audit-module)) "audit-module is a procedure")
   (check-true (procedure? (audit-ref 'audit-directory)) "audit-directory is a procedure")
   (check-true (procedure? (audit-ref 'report->jsexpr)) "report->jsexpr is a procedure")
   (check-true (procedure? (audit-ref 'jsexpr->json-string)) "jsexpr->json-string is a procedure")
   (check-true (procedure? (audit-ref 'find-rkt-files)) "find-rkt-files is a procedure")
   (check-true (procedure? (audit-ref 'count-exports)) "count-exports is a procedure")
   (check-true (procedure? (audit-ref 'has-io-effects?)) "has-io-effects? is a procedure")
   (check-true (procedure? (audit-ref 'count-io-effects)) "count-io-effects is a procedure")
   (check-true (procedure? (audit-ref 'detect-provide-shapes)) "detect-provide-shapes is a procedure")
   (check-true (procedure? (audit-ref 'count-struct-outs)) "count-struct-outs is a procedure")))

(define-test-suite
 audit-module-tests
 (test-case "audit-module returns finding hash for valid file"
   (define tmpdir (make-temporary-file "abstraction-audit-~a" 'directory))
   (setup-fixture-tree tmpdir)
   (define big-path (build-path tmpdir "subdir" "big-module.rkt"))
   (define finding ((audit-ref 'audit-module) (path->string big-path)))

   (check-true (hash? finding) "finding is a hash")
   (check-true (hash-has-key? finding 'path) "finding has path")
   (check-true (hash-has-key? finding 'line-count) "finding has line-count")
   (check-true (hash-has-key? finding 'export-count) "finding has export-count")
   (check-true (hash-has-key? finding 'parameter-count) "finding has parameter-count")
   (check-true (hash-has-key? finding 'macro-count) "finding has macro-count")
   (check-true (hash-has-key? finding 'has-struct-out?) "finding has has-struct-out?")
   (check-true (hash-has-key? finding 'io-mixed-with-logic?) "finding has io-mixed-with-logic?")
   (check-true (hash-has-key? finding 'struct-out-count) "finding has struct-out-count")
   (check-true (hash-has-key? finding 'io-count) "finding has io-count")
   (check-true (hash-has-key? finding 'error-count) "finding has error-count")
   (check-true (hash-has-key? finding 'provide-shapes) "finding has provide-shapes")
   (check-true (hash-has-key? finding 'handler-count) "finding has handler-count")

   ;; Verify the big fixture module has expected properties
   (check-pred positive? (hash-ref finding 'line-count) "big-module has positive line count")
   (check-pred positive? (hash-ref finding 'export-count) "big-module has positive export count")
   (check-pred positive? (hash-ref finding 'parameter-count) "big-module has parameter usage")
   (check-pred positive? (hash-ref finding 'macro-count) "big-module has macro usage")
   (check-true (hash-ref finding 'has-struct-out?) "big-module has struct-out")
   (check-true (hash-ref finding 'io-mixed-with-logic?) "big-module mixes I/O with logic")
   (check-pred positive?
               (hash-ref finding 'struct-out-count)
               "big-module has positive struct-out-count")
   (check-pred positive? (hash-ref finding 'io-count) "big-module has positive io-count")
   (check-pred positive? (hash-ref finding 'error-count) "big-module has positive error-count")
   (check-true (and (member 'struct-out (hash-ref finding 'provide-shapes)) #t)
               "big-module provide-shapes includes struct-out")
   (check-true (and (member 'explicit (hash-ref finding 'provide-shapes)) #t)
               "big-module provide-shapes includes explicit")
   (check-false (and (member 'all-defined-out (hash-ref finding 'provide-shapes)) #t)
                "big-module does NOT use all-defined-out")

   (cleanup-fixture-tree tmpdir))
 (test-case "audit-module returns #f for nonexistent file"
   (define finding ((audit-ref 'audit-module) "/nonexistent/path/file.rkt"))
   (check-false finding "nonexistent file returns #f"))
 (test-case "audit-module on clean file: no I/O, no params"
   (define tmpdir (make-temporary-file "abstraction-audit-~a" 'directory))
   (setup-fixture-tree tmpdir)
   (define clean-path (build-path tmpdir "clean.rkt"))
   (define finding ((audit-ref 'audit-module) (path->string clean-path)))

   (check-true (hash? finding) "clean finding is a hash")
   (check-false (hash-ref finding 'has-struct-out?) "clean module has no struct-out")
   (check-false (hash-ref finding 'io-mixed-with-logic?) "clean module has no I/O")
   (check-equal? (hash-ref finding 'parameter-count) 0 "clean module has no parameters")
   (check-equal? (hash-ref finding 'macro-count) 0 "clean module has no macros")

   (cleanup-fixture-tree tmpdir)))

(define-test-suite
 audit-directory-tests
 (test-case "audit-directory returns report with modules and summary"
   (define tmpdir (make-temporary-file "abstraction-audit-~a" 'directory))
   (setup-fixture-tree tmpdir)

   (define report ((audit-ref 'audit-directory) (path->string tmpdir)))

   (check-true (hash? report) "report is a hash")
   (check-true (hash-has-key? report 'modules) "report has modules key")
   (check-true (list? (hash-ref report 'modules)) "modules is a list")
   (check-true (hash-has-key? report 'summary) "report has summary key")
   (check-true (hash? (hash-ref report 'summary)) "summary is a hash")

   ;; Summary should contain expected dimension keys
   (define summary (hash-ref report 'summary))
   (check-true (hash-has-key? summary 'largest-modules) "summary has largest-modules")
   (check-true (hash-has-key? summary 'parameter-usage) "summary has parameter-usage")
   (check-true (hash-has-key? summary 'macro-usage) "summary has macro-usage")
   (check-true (hash-has-key? summary 'struct-out-exports) "summary has struct-out-exports")
   (check-true (hash-has-key? summary 'io-mixed-with-logic) "summary has io-mixed-with-logic")
   (check-true (hash-has-key? summary 'serialization-hotspots) "summary has serialization-hotspots")
   (check-true (hash-has-key? summary 'total-modules) "summary has total-modules")
   (check-true (hash-has-key? summary 'error-density) "summary has error-density")
   (check-true (hash-has-key? summary 'handler-density) "summary has handler-density")
   (check-true (hash-has-key? summary 'all-defined-out-modules) "summary has all-defined-out-modules")

   ;; Should find exactly 3 production/source modules in fixture (clean, big-module, ado-module).
   (check-equal? (hash-ref summary 'total-modules) 3 "fixture has 3 production modules")

   ;; Should find struct-out in fixture
   (check-true (and (hash-ref summary 'struct-out-exports) #t) "struct-out-exports is non-empty")

   ;; Should find all-defined-out in ado-module
   (check-true (and (hash-ref summary 'all-defined-out-modules) #t)
               "all-defined-out-modules is non-empty")

   ;; Should find error density in big-module
   (check-true (and (hash-ref summary 'error-density) #t) "error-density is non-empty")

   (cleanup-fixture-tree tmpdir))
 (test-case "find-rkt-files excludes tests and compiled directories by default"
   (define tmpdir (make-temporary-file "abstraction-audit-~a" 'directory))
   (setup-fixture-tree tmpdir)

   (define files (map path->string ((audit-ref 'find-rkt-files) (path->string tmpdir))))
   (check-equal? (length files) 3 "fixture scan excludes skipped directories")
   (check-false (ormap (lambda (p) (string-contains? p "/tests/")) files)
                "default scan excludes tests/")
   (check-false (ormap (lambda (p) (string-contains? p "/compiled/")) files)
                "default scan excludes compiled/")

   (cleanup-fixture-tree tmpdir)))

(define-test-suite json-serialization-tests
                   (test-case "report->jsexpr + jsexpr->json-string produces valid JSON"
                     (define tmpdir (make-temporary-file "abstraction-audit-~a" 'directory))
                     (setup-fixture-tree tmpdir)

                     (define report ((audit-ref 'audit-directory) (path->string tmpdir)))
                     (define js ((audit-ref 'report->jsexpr) report))
                     (define json-str ((audit-ref 'jsexpr->json-string) js))

                     (check-true (string? json-str) "JSON output is a string")
                     (check-true (> (string-length json-str) 10) "JSON output is non-trivial")

                     ;; Should be parseable as JSON
                     (define parsed (with-input-from-string json-str read-json))
                     (check-true (hash? parsed) "JSON round-trips to hash")

                     (cleanup-fixture-tree tmpdir)))

(define-test-suite no-mutation-tests
                   (test-case "audit-directory does not mutate files"
                     (define tmpdir (make-temporary-file "abstraction-audit-~a" 'directory))
                     (setup-fixture-tree tmpdir)
                     (define big-path (build-path tmpdir "subdir" "big-module.rkt"))

                     (define before (file->string big-path))
                     (define _ ((audit-ref 'audit-directory) (path->string tmpdir)))
                     (define after (file->string big-path))

                     (check-equal? before after "audit does not mutate files")

                     (cleanup-fixture-tree tmpdir)))

(define-test-suite count-exports-tests
                   (test-case "count-exports on simple provide"
                     (define text "(provide greet)\n(define (greet x) x)")
                     (check-equal? ((audit-ref 'count-exports) text) 1))
                   (test-case "count-exports on struct-out provide"
                     (define text "(provide (struct-out widget) make-widget)")
                     (check-true (>= ((audit-ref 'count-exports) text) 2)
                                 "struct-out counts as at least 2")))

(define-test-suite
 red-flag-signal-tests
 (test-case "count-struct-outs counts struct-out forms"
   (define text "(provide (struct-out a) (struct-out b))")
   (check-equal? ((audit-ref 'count-struct-outs) text) 2 "counts 2 struct-out forms"))
 (test-case "count-struct-outs returns 0 for no struct-out"
   (define text "(provide greet)")
   (check-equal? ((audit-ref 'count-struct-outs) text) 0 "no struct-out returns 0"))
 (test-case "count-io-effects counts I/O pattern occurrences"
   (define text "(call-with-output-file \"a\" f)\n(file->string \"b\")")
   (check-true (>= ((audit-ref 'count-io-effects) text) 2) "counts >= 2 I/O occurrences"))
 (test-case "count-io-effects returns 0 for clean text"
   (define text "(define (f x) x)")
   (check-equal? ((audit-ref 'count-io-effects) text) 0 "no I/O returns 0"))
 (test-case "detect-provide-shapes detects all-defined-out"
   (define text "(provide (all-defined-out))")
   (define shapes ((audit-ref 'detect-provide-shapes) text))
   (check-true (and (member 'all-defined-out shapes) #t) "detects all-defined-out"))
 (test-case "detect-provide-shapes detects struct-out"
   (define text "(provide (struct-out widget))")
   (define shapes ((audit-ref 'detect-provide-shapes) text))
   (check-true (and (member 'struct-out shapes) #t) "detects struct-out"))
 (test-case "detect-provide-shapes detects contract-out"
   (define text "(provide (contract-out [f (-> number? number?)]))")
   (define shapes ((audit-ref 'detect-provide-shapes) text))
   (check-true (and (member 'contract-out shapes) #t) "detects contract-out"))
 (test-case "detect-provide-shapes detects explicit IDs"
   (define text "(provide greet farewell)")
   (define shapes ((audit-ref 'detect-provide-shapes) text))
   (check-true (and (member 'explicit shapes) #t) "detects explicit"))
 (test-case "detect-provide-shapes on empty returns empty list"
   (define text "(define x 1)")
   (check-equal? ((audit-ref 'detect-provide-shapes) text) '() "no provide returns empty list"))
 (test-case "audit-module detects error-count in big fixture"
   (define tmpdir (make-temporary-file "abstraction-audit-~a" 'directory))
   (setup-fixture-tree tmpdir)
   (define big-path (build-path tmpdir "subdir" "big-module.rkt"))
   (define finding ((audit-ref 'audit-module) (path->string big-path)))
   (check-pred positive? (hash-ref finding 'error-count) "big-module has error calls")
   (cleanup-fixture-tree tmpdir))
 (test-case "audit-module detects all-defined-out in ado fixture"
   (define tmpdir (make-temporary-file "abstraction-audit-~a" 'directory))
   (setup-fixture-tree tmpdir)
   (define ado-path (build-path tmpdir "subdir" "ado-module.rkt"))
   (define finding ((audit-ref 'audit-module) (path->string ado-path)))
   (define shapes (hash-ref finding 'provide-shapes))
   (check-true (and (member 'all-defined-out shapes) #t) "ado-module has all-defined-out"))
 (test-case "audit-module on clean file has zero error count"
   (define tmpdir (make-temporary-file "abstraction-audit-~a" 'directory))
   (setup-fixture-tree tmpdir)
   (define clean-path (build-path tmpdir "clean.rkt"))
   (define finding ((audit-ref 'audit-module) (path->string clean-path)))
   (check-equal? (hash-ref finding 'error-count) 0 "clean module has 0 error calls")
   (check-equal? (hash-ref finding 'io-count) 0 "clean module has 0 io-count")
   (check-equal? (hash-ref finding 'struct-out-count) 0 "clean module has 0 struct-out-count")
   (cleanup-fixture-tree tmpdir)))

;; ============================================================
;; Run all tests
;; ============================================================

(define-test-suite all-abstraction-audit-tests
                   api-existence-tests
                   audit-module-tests
                   audit-directory-tests
                   json-serialization-tests
                   no-mutation-tests
                   count-exports-tests
                   red-flag-signal-tests)

(module+ test
  (run-tests all-abstraction-audit-tests))

(module+ main
  (run-tests all-abstraction-audit-tests))
