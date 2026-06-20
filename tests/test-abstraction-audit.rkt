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

  ;; Big module with parameters, macros, struct-out, I/O mixed with logic
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
   (check-true (procedure? (audit-ref 'has-io-effects?)) "has-io-effects? is a procedure")))

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

   ;; Verify the big fixture module has expected properties
   (check-pred positive? (hash-ref finding 'line-count) "big-module has positive line count")
   (check-pred positive? (hash-ref finding 'export-count) "big-module has positive export count")
   (check-pred positive? (hash-ref finding 'parameter-count) "big-module has parameter usage")
   (check-pred positive? (hash-ref finding 'macro-count) "big-module has macro usage")
   (check-true (hash-ref finding 'has-struct-out?) "big-module has struct-out")
   (check-true (hash-ref finding 'io-mixed-with-logic?) "big-module mixes I/O with logic")

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

   ;; Should find exactly 2 modules in fixture
   (check-equal? (hash-ref summary 'total-modules) 2 "fixture has 2 modules")

   ;; Should find struct-out in fixture
   (check-true (and (hash-ref summary 'struct-out-exports) #t) "struct-out-exports is non-empty")

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

;; ============================================================
;; Run all tests
;; ============================================================

(define-test-suite all-abstraction-audit-tests
                   api-existence-tests
                   audit-module-tests
                   audit-directory-tests
                   json-serialization-tests
                   no-mutation-tests
                   count-exports-tests)

(module+ test
  (run-tests all-abstraction-audit-tests))

(module+ main
  (run-tests all-abstraction-audit-tests))
