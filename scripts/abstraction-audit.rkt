#lang racket/base

;; scripts/abstraction-audit.rkt — Lightweight abstraction fitness scanner (coordinator)
;;
;; Refactored: analysis logic in abstraction-analysis.rkt,
;; report formatting in abstraction-report.rkt.
;; This module coordinates and provides the CLI interface.
;;
;; Usage:
;;   racket scripts/abstraction-audit.rkt [--root DIR] [--out FILE] [--json-out FILE] [--strict]

(require racket/list
         racket/string
         racket/port
         racket/file
         racket/cmdline
         json
         "abstraction-analysis.rkt"
         "abstraction-report.rkt")

(provide (all-from-out "abstraction-analysis.rkt")
         (all-from-out "abstraction-report.rkt"))

;; ============================================================

(module+ main
  (define root (make-parameter "."))
  (define out-file (make-parameter #f))
  (define json-out-file (make-parameter #f))
  (define strict? (make-parameter #f))

  (command-line #:program "abstraction-audit"
                #:once-each [("--root" "-r") dir "Root directory to scan" (root dir)]
                [("--out" "-o") file "Write report to file (default: stdout)" (out-file file)]
                [("--json-out" "-j") file "Write JSON report to file" (json-out-file file)]
                [("--strict") "Fail if any module exceeds thresholds" (strict? #t)])

  (define report (audit-directory (root)))

  ;; Human-readable output
  (if (out-file)
      (call-with-output-file* (out-file) (lambda (out) (format-report report out)) #:exists 'truncate)
      (format-report report (current-output-port)))

  ;; JSON output
  (when (json-out-file)
    (call-with-output-file* (json-out-file)
                            (lambda (out)
                              (write-string (jsexpr->json-string (report->jsexpr report)) out))
                            #:exists 'truncate))

  ;; Strict mode check
  (when (strict?)
    (define violations (strict-violations report))
    (unless (null? violations)
      (fprintf (current-error-port) "\nStrict mode violations:\n")
      (for ([v violations])
        (fprintf (current-error-port) "  ~a\n" v))
      (exit 1))))

;; ============================================================
;; Test submodule
;; ============================================================

(module+ test
  (require rackunit
           rackunit/text-ui)

  (define-test-suite abstraction-audit-internal-tests
                     ;; Smoke test: audit-module on self
                     (let ([finding (audit-module (build-path "abstraction-audit.rkt"))])
                       (check-true (hash? finding) "self-audit returns hash")
                       (check-true (> (hash-ref finding 'line-count) 0) "has positive line count")))

  (run-tests abstraction-audit-internal-tests))
