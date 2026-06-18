#lang racket/base

;; q/scripts/run-tests/overhead.rkt — test-runner overhead diagnostics
;;
;; W0 of v0.99.30: measure fixed-cost process/test startup overhead so
;; local/VPS/CI broad-suite behavior can be reported with evidence instead
;; of conjecture.

(require racket/file
         racket/format
         racket/list
         racket/match
         racket/math
         racket/path
         racket/port
         racket/string
         racket/system)

(provide (struct-out overhead-result)
         make-overhead-result
         run-overhead-command
         collect-overhead-diagnostics
         format-overhead-result
         print-overhead-diagnostics)

(struct overhead-result (label command exit-code elapsed-ms stdout stderr) #:transparent)

(define (make-overhead-result label command exit-code elapsed-ms stdout stderr)
  (overhead-result label command exit-code elapsed-ms stdout stderr))

(define (now-ms)
  (current-inexact-milliseconds))

(define (elapsed-since t0)
  (exact-round (- (now-ms) t0)))

(define (run-overhead-command label executable args #:cwd [cwd (current-directory)])
  (define exe-path (find-executable-path executable))
  (unless exe-path
    (error 'run-overhead-command "executable not found: ~a" executable))
  (define stdout-out (open-output-string))
  (define stderr-out (open-output-string))
  (define command-text (string-join (cons executable args) " "))
  (define t0 (now-ms))
  (define-values (_proc stdin _pid _stderr ctrl)
    (parameterize ([current-directory cwd])
      (apply values (apply process*/ports stdout-out #f stderr-out exe-path args))))
  (when stdin
    (close-output-port stdin))
  (ctrl 'wait)
  (define exit-code (ctrl 'exit-code))
  (make-overhead-result label
                        command-text
                        exit-code
                        (elapsed-since t0)
                        (get-output-string stdout-out)
                        (get-output-string stderr-out)))

(define (write-file path content)
  (call-with-output-file path #:exists 'replace (lambda (out) (display content out))))

(define (collect-overhead-diagnostics #:base-dir [base-dir (current-directory)])
  (define temp-dir (make-temporary-file "q-run-tests-overhead-~a" 'directory))
  (define empty-file (build-path temp-dir "empty.rkt"))
  (define rackunit-file (build-path temp-dir "rackunit-empty.rkt"))
  (write-file empty-file "#lang racket/base\n(void)\n")
  (write-file rackunit-file
              (string-append
               "#lang racket\n"
               "(require rackunit rackunit/text-ui)\n"
               "(run-tests (test-suite \"empty\" (test-case \"ok\" (check-true #t))))\n"))
  (define representative (build-path base-dir "tests" "test-version.rkt"))
  (define commands
    (append
     (list (list "racket-noop" "racket" (list "-e" "(void)"))
           (list "racket-empty" "racket" (list (path->string empty-file)))
           (list "raco-empty" "raco" (list "test" (path->string empty-file)))
           (list "raco-rackunit-empty" "raco" (list "test" (path->string rackunit-file))))
     (if (file-exists? representative)
         (list (list "raco-representative-test" "raco" (list "test" (path->string representative))))
         '())))
  (dynamic-wind void
                (lambda ()
                  (for/list ([cmd (in-list commands)])
                    (match cmd
                      [(list label exe args) (run-overhead-command label exe args #:cwd base-dir)])))
                (lambda ()
                  (when (directory-exists? temp-dir)
                    (delete-directory/files temp-dir)))))

(define (format-overhead-result r)
  (format "  ~a: ~ams exit=~a cmd=~a"
          (overhead-result-label r)
          (overhead-result-elapsed-ms r)
          (overhead-result-exit-code r)
          (overhead-result-command r)))

(define (print-overhead-diagnostics #:base-dir [base-dir (current-directory)])
  (displayln "═══════════════════════════════════════════════════════════")
  (displayln "              TEST RUNNER OVERHEAD DIAGNOSTIC")
  (displayln "═══════════════════════════════════════════════════════════")
  (printf "  Base dir:   ~a~n" (path->string (simplify-path base-dir)))
  (printf "  Racket:     ~a~n" (or (find-executable-path "racket") "not found"))
  (printf "  raco:       ~a~n" (or (find-executable-path "raco") "not found"))
  (newline)
  (define results (collect-overhead-diagnostics #:base-dir base-dir))
  (for ([r (in-list results)])
    (displayln (format-overhead-result r)))
  (newline)
  (displayln "Interpretation:")
  (displayln "  - racket-noop approximates raw Racket process startup cost.")
  (displayln "  - raco-empty approximates per-file raco test harness overhead.")
  (displayln "  - raco-rackunit-empty approximates parser-visible rackunit overhead.")
  (displayln "  - raco-representative-test samples one normal project test when available.")
  (displayln "═══════════════════════════════════════════════════════════")
  results)
