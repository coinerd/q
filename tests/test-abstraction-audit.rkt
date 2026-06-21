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

  ;; Module with mutable cache, ad-hoc parsing, and event handler (v0.99.37 W1)
  (write-file (build-path base-dir "subdir" "cache-module.rkt")
              #<<EOF
#lang racket/base

(provide handle-event
         get-cache-entry)

(define cache (make-hash))

(define (handle-event evt)
  (hash-set! cache 'last evt)
  (string-split (format "~a" evt) ":"))

(define (get-cache-entry key)
  (hash-ref cache key #f))
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
   (check-true (procedure? (audit-ref 'count-struct-outs)) "count-struct-outs is a procedure")
   (check-true (procedure? (audit-ref 'count-mutable-caches)) "count-mutable-caches is a procedure")
   (check-true (procedure? (audit-ref 'count-bench-timing)) "count-bench-timing is a procedure")
   (check-true (procedure? (audit-ref 'count-ad-hoc-parsing)) "count-ad-hoc-parsing is a procedure")
   (check-true (procedure? (audit-ref 'count-event-handlers)) "count-event-handlers is a procedure")))

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
   (check-true (hash-has-key? summary 'mutable-cache-modules) "summary has mutable-cache-modules")
   (check-true (hash-has-key? summary 'bench-timing-modules) "summary has bench-timing-modules")
   (check-true (hash-has-key? summary 'ad-hoc-parse-modules) "summary has ad-hoc-parse-modules")
   (check-true (hash-has-key? summary 'event-handler-modules) "summary has event-handler-modules")

   ;; Should find exactly 4 production/source modules in fixture (clean, big-module, ado-module, cache-module).
   (check-equal? (hash-ref summary 'total-modules) 4 "fixture has 4 production modules")

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
   (check-equal? (length files) 4 "fixture scan excludes skipped directories")
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
;; v0.99.37 W1: New signal tests
;; ============================================================

(define-test-suite
 v3-signal-tests
 (test-case "count-mutable-caches detects make-hash and hash-set!"
   (define text "(define h (make-hash))\n(hash-set! h 'k 1)\n(hash-remove! h 'k)")
   (check-equal? ((audit-ref 'count-mutable-caches) text) 3 "counts 3 mutable cache ops"))
 (test-case "count-mutable-caches returns 0 for clean text"
   (define text "(define (f x) x)")
   (check-equal? ((audit-ref 'count-mutable-caches) text) 0 "no mutable cache returns 0"))
 (test-case "count-bench-timing detects current-inexact-milliseconds"
   (define text
     "(let ([start (current-inexact-milliseconds)]) (- (current-inexact-milliseconds) start))")
   (check-equal? ((audit-ref 'count-bench-timing) text) 2 "counts 2 timing calls"))
 (test-case "count-bench-timing returns 0 for clean text"
   (define text "(define (f x) x)")
   (check-equal? ((audit-ref 'count-bench-timing) text) 0 "no timing returns 0"))
 (test-case "count-ad-hoc-parsing detects regexp-match and string-split"
   (define text "(regexp-match #rx\"foo\" s)\n(string-split s \" \")\n(string-trim s)")
   (check-equal? ((audit-ref 'count-ad-hoc-parsing) text) 3 "counts 3 parsing ops"))
 (test-case "count-ad-hoc-parsing returns 0 for clean text"
   (define text "(define (f x) x)")
   (check-equal? ((audit-ref 'count-ad-hoc-parsing) text) 0 "no parsing returns 0"))
 (test-case "count-event-handlers detects handler definitions"
   (define text "(define (handle-event e) e)\n(define (handle-key k) k)")
   (check-equal? ((audit-ref 'count-event-handlers) text) 2 "counts 2 handler defs"))
 (test-case "count-event-handlers returns 0 for clean text"
   (define text "(define (f x) x)")
   (check-equal? ((audit-ref 'count-event-handlers) text) 0 "no handlers returns 0"))
 (test-case "audit-module detects mutable-cache-count in cache fixture"
   (define tmpdir (make-temporary-file "abstraction-audit-~a" 'directory))
   (setup-fixture-tree tmpdir)
   (define cache-path (build-path tmpdir "subdir" "cache-module.rkt"))
   (define finding ((audit-ref 'audit-module) (path->string cache-path)))
   (check-pred positive? (hash-ref finding 'mutable-cache-count) "cache-module has mutable cache ops")
   (check-pred positive? (hash-ref finding 'ad-hoc-parse-count) "cache-module has ad-hoc parsing")
   (check-pred positive? (hash-ref finding 'event-handler-count) "cache-module has event handlers")
   (cleanup-fixture-tree tmpdir))
 (test-case "audit-module on clean file has zero v3 signals"
   (define tmpdir (make-temporary-file "abstraction-audit-~a" 'directory))
   (setup-fixture-tree tmpdir)
   (define clean-path (build-path tmpdir "clean.rkt"))
   (define finding ((audit-ref 'audit-module) (path->string clean-path)))
   (check-equal? (hash-ref finding 'mutable-cache-count) 0 "clean module has 0 mutable cache")
   (check-equal? (hash-ref finding 'bench-timing-count) 0 "clean module has 0 bench timing")
   (check-equal? (hash-ref finding 'ad-hoc-parse-count) 0 "clean module has 0 ad-hoc parsing")
   (check-equal? (hash-ref finding 'event-handler-count) 0 "clean module has 0 event handlers")
   (cleanup-fixture-tree tmpdir)))

;; ============================================================
;; W6 v0.99.37: Pure audit-content tests (no filesystem I/O)
;; ============================================================

(define-test-suite
 pure-audit-content-tests
 (test-case "audit-content analyzes text without I/O"
   (define text
     "#lang racket/base\n(provide greet)\n(define (greet name)\n  (string-append \"Hello, \" name \"!\"))")
   (define finding ((audit-ref 'audit-content) "test.rkt" text))
   (check-true (hash? finding) "returns a hash")
   (check-equal? (hash-ref finding 'path) "test.rkt" "path preserved")
   (check-true (> (hash-ref finding 'line-count) 0) "positive line count"))
 (test-case "audit-content detects struct-out without I/O"
   (define text "#lang racket/base\n(provide (struct-out widget))\n(struct widget (name))")
   (define finding ((audit-ref 'audit-content) "w.rkt" text))
   (check-true (hash-ref finding 'has-struct-out?) "detects struct-out")
   (check-pred positive? (hash-ref finding 'struct-out-count) "struct-out count > 0"))
 (test-case "audit-content detects I/O mixed with logic"
   (define text
     "#lang racket/base\n(define (f x)\n  (call-with-output-file \"a\" (lambda (o) (display x o))))")
   (define finding ((audit-ref 'audit-content) "io.rkt" text))
   (check-true (hash-ref finding 'io-mixed-with-logic?) "detects I/O mixed with logic")
   (check-pred positive? (hash-ref finding 'io-count) "io count > 0"))
 (test-case "audit-content detects mutable-cache without I/O"
   (define text
     "#lang racket/base\n(define cache (make-hash))\n(define (handle-event e)\n  (hash-set! cache 'last e))")
   (define finding ((audit-ref 'audit-content) "cache.rkt" text))
   (check-pred positive? (hash-ref finding 'mutable-cache-count) "mutable cache count > 0")
   (check-pred positive? (hash-ref finding 'event-handler-count) "event handler count > 0"))
 (test-case "audit-content on empty text returns hash with 0 lines"
   (define finding ((audit-ref 'audit-content) "empty.rkt" ""))
   (check-true (hash? finding) "returns hash for empty text")
   (check-equal? (hash-ref finding 'line-count) 0 "empty string splits to 0 lines"))
 (test-case "audit-content: path string preserved as-is"
   (define finding ((audit-ref 'audit-content) "foo/bar.rkt" "(define x 1)"))
   (check-equal? (hash-ref finding 'path) "foo/bar.rkt"))
 (test-case "audit-module with mocked I/O parameter"
   (define mock-text "#lang racket/base\n(provide f)\n(define (f x) x)")
   (define mock-reader (lambda (path) (string-split mock-text "\n")))
   ;; Use eval to parameterize since audit-ref uses dynamic-require
   (define orig-param (audit-ref 'current-audit-file->lines))
   (parameterize ([orig-param mock-reader])
     (define finding ((audit-ref 'audit-module) "mock.rkt"))
     (check-true (hash? finding) "mocked audit-module returns hash")
     (check-true (> (hash-ref finding 'line-count) 0) "mocked module has positive lines"))))

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
                   red-flag-signal-tests
                   v3-signal-tests
                   pure-audit-content-tests)

(module+ test
  (run-tests all-abstraction-audit-tests))

(module+ main
  (run-tests all-abstraction-audit-tests))
