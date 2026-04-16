#lang racket/base

(require racket/string
         racket/match
         racket/path
         racket/file
         racket/system
         racket/future)

;; ---------------------------------------------------------------------------
;; Parallel test runner wrapping `raco test`
;; ---------------------------------------------------------------------------

(define (usage)
  (displayln "Usage: racket scripts/run-tests.rkt [OPTIONS] [TEST-FILES ...]")
  (newline)
  (displayln "Options:")
  (displayln "  --jobs N          Number of parallel jobs (default: processor-count)")
  (displayln "  --sequential      Run tests sequentially (no --jobs, no --process)")
  (displayln "  --timeout SECS    Set raco test timeout in seconds")
  (displayln "  --suite <name>    Run test suite: all (default), fast, slow, tui, smoke")
  (displayln "  --help            Show this help message")
  (newline)
  (displayln "Suites:")
  (displayln "  all     Entire tests/ directory")
  (displayln "  fast    All tests except slow patterns")
  (displayln "  slow    Only sandbox/subprocess tests")
  (displayln "  tui     Files in tests/tui/")
  (displayln "  smoke   Fast minus workflows/, interfaces/, and provider tests"))

;; ---------------------------------------------------------------------------
;; Suite matching helpers
;; ---------------------------------------------------------------------------

(define slow-patterns '("sandbox" "subprocess" "integration" "benchmark" "workflow-" "e2e-"))

(define (slow-file? f)
  (define base (file-name-from-path f))
  (for/or ([p (in-list slow-patterns)])
    (and base (string-contains? (path->string base) p))))

(define (tui-file? f)
  (string-contains? f "/tui/"))

(define (smoke-excluded? f)
  (or (slow-file? f)
      (string-contains? f "/workflows/")
      (string-contains? f "/interfaces/")
      (let ([base (file-name-from-path f)])
        (and base (string-contains? (path->string base) "provider")))))

;; ---------------------------------------------------------------------------
;; Test file collection
;; ---------------------------------------------------------------------------

(define (collect-test-files suite)
  (define all-files
    (for/list ([f (in-directory "tests")]
               #:when (and (file-exists? f)
                           (let ([s (path->string f)])
                             (and (string-suffix? s ".rkt")
                                  (not (string-contains? s "/compiled/"))))))
      (path->string f)))
  (case suite
    [(all) '("tests/")]
    [(fast) (filter (lambda (f) (not (slow-file? f))) all-files)]
    [(slow) (filter slow-file? all-files)]
    [(tui) (filter tui-file? all-files)]
    [(smoke) (filter (lambda (f) (not (smoke-excluded? f))) all-files)]
    [else '("tests/")]))

;; ---------------------------------------------------------------------------
;; Build raco test command
;; ---------------------------------------------------------------------------

(define (build-raco-args files jobs timeout sequential?)
  (append (if sequential?
              '()
              '("--process"))
          (if sequential?
              '()
              (list "--jobs" (number->string jobs)))
          (if timeout
              (list "--timeout" (number->string timeout))
              '())
          files))

;; ---------------------------------------------------------------------------
;; CLI argument parsing
;; ---------------------------------------------------------------------------

(define (parse-args args)
  (let loop ([rest args]
             [jobs (processor-count)]
             [sequential? #f]
             [timeout #f]
             [suite 'all]
             [extra '()])
    (match rest
      ['() (values jobs sequential? timeout suite (reverse extra))]
      [(list "--help" _ ...)
       (usage)
       (exit 0)]
      [(list "--jobs" n rest ...) (loop rest (string->number n) sequential? timeout suite extra)]
      [(list "--sequential" rest ...) (loop rest jobs #t timeout suite extra)]
      [(list "--timeout" secs rest ...)
       (loop rest jobs sequential? (string->number secs) suite extra)]
      [(list "--suite" name rest ...)
       (loop rest jobs sequential? timeout (string->symbol name) extra)]
      [(list arg rest ...) (loop rest jobs sequential? timeout suite (cons arg extra))])))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(define (main args)
  (define-values (jobs sequential? timeout suite extra-files) (parse-args args))

  ;; Resolve test targets
  (define suite-files
    (if (pair? extra-files)
        extra-files
        (collect-test-files suite)))

  (unless (pair? suite-files)
    (displayln "No test files matched the selected suite.")
    (exit 1))

  ;; Build command
  (define raco-args (build-raco-args suite-files jobs timeout sequential?))
  (define cmd (string-append "raco test " (string-join raco-args " ")))

  (define suite-label (symbol->string suite))
  (define n-files (length suite-files))
  (printf ";; run-tests: suite=~a files=~a jobs=~a sequential=~a~n"
          suite-label
          n-files
          jobs
          sequential?)
  (printf ";; command: ~a~n" cmd)
  (newline)

  ;; Run with timing
  (define t0 (current-inexact-milliseconds))
  (define exit-code (system/exit-code cmd))
  (define elapsed (/ (- (current-inexact-milliseconds) t0) 1000.0))

  (newline)
  (printf ";; run-tests: done suite=~a files=~a elapsed=~as exit=~a~n"
          suite-label
          n-files
          elapsed
          exit-code)

  (exit exit-code))

;; Entry point — skip script name from command-line-arguments is already correct
(main (vector->list (current-command-line-arguments)))
