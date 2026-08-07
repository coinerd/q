#lang racket/base

;; scripts/abstraction-audit.rkt — Lightweight abstraction fitness scanner (coordinator)
;;
;; DESIGN FACT: Each signal encodes a single structural property (coupling, size,
;; collision, boundary) derived from a homogeneous scan over the same module set.
;; Signal aggregation is never re-scored; raw values are preserved in JSON output.
;;
;; DESIGN FACT (W2 audit): Historical W2 audit tracked signal provenance for all 4
;; signal origins (coupling, size, collision, boundary) across the codebase.
;; DESIGN FACT (W5 mutation): W5 mutation analysis added per-module mutability scoring
;; to the signal set, surfaced in JSON output alongside coupling/size/collision/boundary.
;; DESIGN FACT (W1 scorecard P5): W1 scorecard P5 introduced the --strict exit-code mode
;; used by CI lint gates to fail on threshold violations.
;; DESIGN FACT (W6 adapter): W6 adapter boundary audit ensured the scanner correctly
;; handles module paths across adapter layers (llm/, runtime/, interfaces/).
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
  (define ci-mode? (make-parameter #f))

  (command-line #:program "abstraction-audit"
                #:once-each [("--root" "-r") dir "Root directory to scan" (root dir)]
                [("--out" "-o") file "Write report to file (default: stdout)" (out-file file)]
                [("--json-out" "-j") file "Write JSON report to file" (json-out-file file)]
                [("--strict") "Fail if any module exceeds thresholds" (strict? #t)]
                [("--ci-mode") "CI-friendly output: JSON, HIGH/CRITICAL only, exit 0" (ci-mode? #t)])

  (define report (audit-directory (root)))

  ;; CI mode: JSON to stdout with only HIGH and CRITICAL findings, exit 0
  (when (ci-mode?)
    (define summary (hash-ref report 'summary))
    (define modules (hash-ref report 'modules))
    (define high-critical-modules
      (filter (lambda (m)
                (or (> (hash-ref m 'struct-out-count 0) 5)
                    (> (hash-ref m 'io-count 0) 10)
                    (> (hash-ref m 'error-count 0) 10)
                    (> (hash-ref m 'parameter-count 0) 15)
                    (> (hash-ref m 'macro-count 0) 10)
                    (> (hash-ref m 'handler-count 0) 8)))
              modules))
    (define ci-report
      (hash 'mode "ci"
            'total-modules (hash-ref summary 'total-modules)
            'high-critical-findings (length high-critical-modules)
            'modules (map (lambda (m)
                            (hash 'path (hash-ref m 'path)
                                  'struct-out-count (hash-ref m 'struct-out-count 0)
                                  'io-count (hash-ref m 'io-count 0)
                                  'error-count (hash-ref m 'error-count 0)
                                  'parameter-count (hash-ref m 'parameter-count 0)
                                  'macro-count (hash-ref m 'macro-count 0)
                                  'handler-count (hash-ref m 'handler-count 0)))
                          high-critical-modules)
            'info "Abstraction Manual: see docs/ABSTRACTION_INSTRUCTION_MANUAL.md"))
    (write-string (jsexpr->json-string ci-report) (current-output-port))
    (newline (current-output-port))
    (exit 0))

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
