#lang racket/base

;; scripts/abstraction-report.rkt — Markdown report generation
;;
;; Extracted from abstraction-audit.rkt. Contains report formatting
;; and serialization functions.

(require racket/list
         racket/string
         racket/port
         racket/file
         json
         "abstraction-analysis.rkt")

(provide report->jsexpr
         jsexpr->json-string
         format-report
         strict-violations
         strict-thresholds)

;; ============================================================
;; Serialization
;; ============================================================

(define (report->jsexpr report)
  "Convert audit-report to a JSON-serializable jsexpr.
   Converts symbol values in provide-shapes to strings for JSON compatibility."
  (define modules (hash-ref report 'modules))
  (define json-modules
    (map (lambda (m)
           (define shapes (hash-ref m 'provide-shapes '()))
           (hash-set m 'provide-shapes (map (lambda (s) (symbol->string s)) shapes)))
         modules))
  (hash 'modules json-modules 'summary (hash-ref report 'summary)))

(define (jsexpr->json-string js)
  "Convert jsexpr to a JSON string."
  (jsexpr->string js))

;; ============================================================
;; Human-readable report
;; ============================================================

(define (format-report report out)
  "Print a human-readable report to port `out`."
  (define summary (hash-ref report 'summary))
  (define modules (hash-ref report 'modules))

  (fprintf out "# Abstraction Fitness Report\n\n")
  (fprintf out "**Total modules scanned:** ~a\n\n" (hash-ref summary 'total-modules))

  ;; Largest modules
  (fprintf out "## Largest Modules (top 20)\n\n")
  (fprintf out "| Lines | Exports | Path |\n")
  (fprintf out "|-------|---------|------|\n")
  (for ([m (hash-ref summary 'largest-modules)])
    (fprintf out "| ~a | ~a | ~a |\n" (hash-ref m 'lines) (hash-ref m 'exports) (hash-ref m 'path)))
  (fprintf out "\n")

  ;; Parameter usage
  (fprintf out "## Parameter Usage\n\n")
  (fprintf out "| Count | Path |\n")
  (fprintf out "|-------|------|\n")
  (for ([p (hash-ref summary 'parameter-usage)])
    (fprintf out "| ~a | ~a |\n" (hash-ref p 'count) (hash-ref p 'path)))
  (fprintf out "\n")

  ;; Macro usage
  (fprintf out "## Macro Usage\n\n")
  (fprintf out "| Count | Path |\n")
  (fprintf out "|-------|------|\n")
  (for ([m (hash-ref summary 'macro-usage)])
    (fprintf out "| ~a | ~a |\n" (hash-ref m 'count) (hash-ref m 'path)))
  (fprintf out "\n")

  ;; struct-out exports
  (fprintf out "## struct-out Exports\n\n")
  (fprintf out "| Count | Path |\n")
  (fprintf out "|-------|------|\n")
  (for ([s (hash-ref summary 'struct-out-exports)])
    (fprintf out "| ~a | ~a |\n" (hash-ref s 'count) (hash-ref s 'path)))
  (fprintf out "\n")

  ;; I/O mixed with logic
  (fprintf out "## I/O Mixed With Pure Logic\n\n")
  (fprintf out "| Count | Path |\n")
  (fprintf out "|-------|------|\n")
  (for ([p (hash-ref summary 'io-mixed-with-logic)])
    (fprintf out "| ~a | ~a |\n" (hash-ref p 'count) (hash-ref p 'path)))
  (fprintf out "\n")

  ;; Serialization hotspots
  (fprintf out "## Serialization Hotspots\n\n")
  (fprintf out "| Count | Path |\n")
  (fprintf out "|-------|------|\n")
  (for ([s (hash-ref summary 'serialization-hotspots)])
    (fprintf out "| ~a | ~a |\n" (hash-ref s 'count) (hash-ref s 'path)))
  (fprintf out "\n")

  ;; Error/raise density
  (fprintf out "## Error/Raise Density (top 20)\n\n")
  (fprintf out "| Count | Path |\n")
  (fprintf out "|-------|------|\n")
  (for ([e (take-at-most (hash-ref summary 'error-density) 20)])
    (fprintf out "| ~a | ~a |\n" (hash-ref e 'count) (hash-ref e 'path)))
  (fprintf out "\n")

  ;; Handler density
  (fprintf out "## Handler Density (with-handlers, top 20)\n\n")
  (fprintf out "| Count | Path |\n")
  (fprintf out "|-------|------|\n")
  (for ([h (take-at-most (hash-ref summary 'handler-density) 20)])
    (fprintf out "| ~a | ~a |\n" (hash-ref h 'count) (hash-ref h 'path)))
  (fprintf out "\n")

  ;; all-defined-out modules
  (fprintf out "## all-defined-out Usage\n\n")
  (define ado (hash-ref summary 'all-defined-out-modules '()))
  (if (null? ado)
      (fprintf out "None found.\n\n")
      (begin
        (for ([p ado])
          (fprintf out "- ~a\n" p))
        (fprintf out "\n")))

  ;; Mutable cache usage (v0.99.37 W1)
  (fprintf out "## Mutable Cache Usage (make-hash / hash-set!)\n\n")
  (define mc (hash-ref summary 'mutable-cache-modules '()))
  (if (null? mc)
      (fprintf out "None found.\n\n")
      (begin
        (fprintf out "| Count | Path |\n")
        (fprintf out "|-------|------|\n")
        (for ([p mc])
          (fprintf out "| ~a | ~a |\n" (hash-ref p 'count) (hash-ref p 'path)))
        (fprintf out "\n")))

  ;; Benchmark timing assertions (v0.99.37 W1)
  (fprintf out "## Benchmark Timing Assertions (current-inexact-milliseconds)\n\n")
  (define bt (hash-ref summary 'bench-timing-modules '()))
  (if (null? bt)
      (fprintf out "None found.\n\n")
      (begin
        (fprintf out "| Count | Path |\n")
        (fprintf out "|-------|------|\n")
        (for ([p bt])
          (fprintf out "| ~a | ~a |\n" (hash-ref p 'count) (hash-ref p 'path)))
        (fprintf out "\n")))

  ;; Ad-hoc string parsing (v0.99.37 W1)
  (fprintf out "## Ad-Hoc String Parsing (regexp-match / string-split)\n\n")
  (define ap (hash-ref summary 'ad-hoc-parse-modules '()))
  (if (null? ap)
      (fprintf out "None found.\n\n")
      (begin
        (fprintf out "| Count | Path |\n")
        (fprintf out "|-------|------|\n")
        (for ([p (take-at-most ap 20)])
          (fprintf out "| ~a | ~a |\n" (hash-ref p 'count) (hash-ref p 'path)))
        (fprintf out "\n")))

  ;; Event handler modules (v0.99.37 W1)
  (fprintf out "## Event Handler Modules (handler/model boundary risk)\n\n")
  (define eh (hash-ref summary 'event-handler-modules '()))
  (if (null? eh)
      (fprintf out "None found.\n\n")
      (begin
        (fprintf out "| Count | Path |\n")
        (fprintf out "|-------|------|\n")
        (for ([p eh])
          (fprintf out "| ~a | ~a |\n" (hash-ref p 'count) (hash-ref p 'path)))
        (fprintf out "\n")))

  ;; v0.99.38 W8: Change-locality signals
  (fprintf out "## CWD-Sensitive Path Patterns (dynamic-require / file->* with relative paths)\n\n")
  (define csp (hash-ref summary 'cwd-sensitive-path-modules '()))
  (if (null? csp)
      (fprintf out "None found.\n\n")
      (begin
        (fprintf out "| Count | Path |\n")
        (fprintf out "|-------|------|\n")
        (for ([p csp])
          (fprintf out "| ~a | ~a |\n" (hash-ref p 'count) (hash-ref p 'path)))
        (fprintf out "\n")))

  (fprintf out "## File Mutation Sites (call-with-output-file / write-to-file / display-to-file)\n\n")
  (define ms (hash-ref summary 'mutation-site-modules '()))
  (if (null? ms)
      (fprintf out "None found.\n\n")
      (begin
        (fprintf out "| Count | Path |\n")
        (fprintf out "|-------|------|\n")
        (for ([p (take-at-most ms 20)])
          (fprintf out "| ~a | ~a |\n" (hash-ref p 'count) (hash-ref p 'path)))
        (fprintf out "\n")))

  (fprintf out "## Hash-Ref Chains (nested hash-ref — common-case API pain point)\n\n")
  (define hrc (hash-ref summary 'hash-ref-chain-modules '()))
  (if (null? hrc)
      (fprintf out "None found.\n\n")
      (begin
        (fprintf out "| Count | Path |\n")
        (fprintf out "|-------|------|\n")
        (for ([p (take-at-most hrc 20)])
          (fprintf out "| ~a | ~a |\n" (hash-ref p 'count) (hash-ref p 'path)))
        (fprintf out "\n")))

  (fprintf out "## Inline Config Paths ((build-path (find-system-path 'home-dir) ...))\n\n")
  (define icp (hash-ref summary 'inline-config-path-modules '()))
  (if (null? icp)
      (fprintf out "None found.\n\n")
      (begin
        (fprintf out "| Count | Path |\n")
        (fprintf out "|-------|------|\n")
        (for ([p icp])
          (fprintf out "| ~a | ~a |\n" (hash-ref p 'count) (hash-ref p 'path)))
        (fprintf out "\n")))

  ;; v0.99.42 W1: Manual red-flag signals
  (fprintf out "## Stringly Verdict Constants (string status/verdict returns)\n\n")
  (define sv (hash-ref summary 'stringly-verdict-modules '()))
  (if (null? sv)
      (fprintf out "None found.\n\n")
      (begin
        (fprintf out "| Count | Path |\n")
        (fprintf out "|-------|------|\n")
        (for ([p (take-at-most sv 20)])
          (fprintf out "| ~a | ~a |\n" (hash-ref p 'count) (hash-ref p 'path)))
        (fprintf out "\n")))

  (fprintf out "## Effectful Classifier Functions (classify/verify/gate with potential I/O)\n\n")
  (define ec (hash-ref summary 'effectful-classifier-modules '()))
  (if (null? ec)
      (fprintf out "None found.\n\n")
      (begin
        (fprintf out "| Count | Path |\n")
        (fprintf out "|-------|------|\n")
        (for ([p (take-at-most ec 20)])
          (fprintf out "| ~a | ~a |\n" (hash-ref p 'count) (hash-ref p 'path)))
        (fprintf out "\n")))

  (fprintf out "## Optional-Mandatory Asset Wording (optional near manifest/asset/tarball)\n\n")
  (define om (hash-ref summary 'optional-mandatory-modules '()))
  (if (null? om)
      (fprintf out "None found.\n\n")
      (begin
        (fprintf out "| Count | Path |\n")
        (fprintf out "|-------|------|\n")
        (for ([p om])
          (fprintf out "| ~a | ~a |\n" (hash-ref p 'count) (hash-ref p 'path)))
        (fprintf out "\n")))

  ;; Advisory
  (fprintf out "## Advisory\n\n")
  (fprintf out "This report is advisory. Use with the Abstraction Instruction Manual\n")
  (fprintf out "to identify candidates for boundary clarification.\n"))

;; ============================================================
;; Strict mode thresholds

;; ============================================================

(define strict-thresholds
  (hash 'max-lines
        800
        'max-exports
        40
        'max-require-count
        30
        'max-handler-count
        12
        'max-error-count
        20))

(define (strict-violations report)
  "Return list of strict-mode violations. Empty list = pass."
  (define modules (hash-ref report 'modules))
  (append* (for/list ([m modules])
             (define path (hash-ref m 'path))
             (define lines (hash-ref m 'line-count))
             (define exports (hash-ref m 'export-count))
             (define reqs (hash-ref m 'require-count))
             (define handlers (hash-ref m 'handler-count 0))
             (define errors (hash-ref m 'error-count 0))
             (append (if (> lines (hash-ref strict-thresholds 'max-lines))
                         (list (format "~a: ~a lines exceeds max ~a"
                                       path
                                       lines
                                       (hash-ref strict-thresholds 'max-lines)))
                         '())
                     (if (> exports (hash-ref strict-thresholds 'max-exports))
                         (list (format "~a: ~a exports exceeds max ~a"
                                       path
                                       exports
                                       (hash-ref strict-thresholds 'max-exports)))
                         '())
                     (if (> reqs (hash-ref strict-thresholds 'max-require-count))
                         (list (format "~a: ~a requires exceeds max ~a"
                                       path
                                       reqs
                                       (hash-ref strict-thresholds 'max-require-count)))
                         '())
                     (if (> handlers (hash-ref strict-thresholds 'max-handler-count))
                         (list (format "~a: ~a handlers exceeds max ~a"
                                       path
                                       handlers
                                       (hash-ref strict-thresholds 'max-handler-count)))
                         '())
                     (if (> errors (hash-ref strict-thresholds 'max-error-count))
                         (list (format "~a: ~a error/raise calls exceeds max ~a"
                                       path
                                       errors
                                       (hash-ref strict-thresholds 'max-error-count)))
                         '())))))
