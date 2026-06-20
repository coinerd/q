#lang racket/base

;; scripts/abstraction-audit.rkt — Lightweight abstraction fitness scanner
;;
;; W1 v0.99.35: Convert the Racket Abstraction Instruction Manual into
;; repeatable, evidence-based signals. Advisory by default; does NOT fail
;; the build unless invoked with --strict.
;;
;; Usage:
;;   racket scripts/abstraction-audit.rkt [--root DIR] [--out FILE] [--json-out FILE] [--strict]
;;
;; Signals collected per module:
;;   - Line count
;;   - Public export count
;;   - Parameter usage (make-parameter / parameterize)
;;   - Macro usage (define-syntax / syntax-parse / syntax-case / define-syntax-rule)
;;   - struct-out exports
;;   - I/O effects mixed with pure logic definitions
;;   - Serialization hotspots (->jsexpr / jsexpr-> / hash-> / ->hash)
;;   - Exception handler density (with-handlers)
;;   - Dependency fan-out (require forms)
;;
;; Output:
;;   Human-readable report to stdout (or --out FILE)
;;   Machine-readable JSON to --json-out FILE

(require racket/list
         racket/string
         racket/port
         racket/file
         racket/path
         racket/match
         racket/cmdline
         json)

(provide audit-module
         audit-directory
         report->jsexpr
         jsexpr->json-string
         format-report
         strict-violations
         find-rkt-files
         count-exports
         count-requires
         count-matches
         has-io-effects?)

;; ============================================================
;; Core data structures
;; ============================================================

;; A module-finding is a hash with keys:
;;   path, line-count, export-count, parameter-count, macro-count,
;;   has-struct-out?, io-mixed-with-logic?, serialization-count,
;;   handler-count, require-count

;; An audit-report is a hash with keys:
;;   modules   — list of module-finding hashes
;;   summary   — hash with aggregate statistics

;; ============================================================
;; Per-file analysis
;; ============================================================

(define (audit-module path)
  "Analyze a single .rkt file and return a module-finding hash.
   Returns #f if the file cannot be read."
  (define lines
    (with-handlers ([exn:fail:filesystem? (lambda (_) #f)])
      (file->lines path)))
  (if (not lines)
      #f
      (let* ([text (string-join lines "\n")]
             [line-count (length lines)]
             [export-count (count-exports text)]
             [parameter-count (count-matches text parameter-rx)]
             [macro-count (count-matches text macro-rx)]
             [has-struct-out? (regexp-match? struct-out-rx text)]
             [io? (has-io-effects? text)]
             [serialization-count (count-matches text serialization-rx)]
             [handler-count (count-matches text handler-rx)]
             [require-count (count-requires text)])
        (hash 'path
              (if (path? path)
                  (path->string path)
                  path)
              'line-count
              line-count
              'export-count
              export-count
              'parameter-count
              parameter-count
              'macro-count
              macro-count
              'has-struct-out?
              has-struct-out?
              'io-mixed-with-logic?
              io?
              'serialization-count
              serialization-count
              'handler-count
              handler-count
              'require-count
              require-count))))

;; Count exported identifiers in (provide ...) forms.
;; Handles: provide id, provide (contract-out ...), provide (struct-out ...)
(define provide-rx #px"\\(provide\\s")

(define (count-exports text)
  "Approximate export count: count identifiers in provide forms."
  (define matches (regexp-match* #px"\\(provide\\s" text))
  ;; Each provide form contributes at least 1; we count provide forms
  ;; rather than individual identifiers for a stable, fast signal.
  ;; For more accuracy, count struct-out as +1 and contract-out entries.
  (define struct-outs (regexp-match* #rx"struct-out " text))
  (define contract-outs (regexp-match* #rx"contract-out" text))
  (+ (length matches) (length struct-outs) (length contract-outs)))

(define (count-requires text)
  "Count distinct require imports."
  (define matches (regexp-match* #px"\\(require\\s" text))
  (length matches))

(define parameter-rx #px"make-parameter|parameterize")
(define macro-rx #px"define-syntax|syntax-parse|syntax-case|define-syntax-rule")
(define struct-out-rx #px"struct-out\\s")
(define serialization-rx #px"->jsexpr|jsexpr->|->hash|hash->")
(define handler-rx #rx"with-handlers")

(define (count-matches text rx)
  (length (regexp-match* rx text)))

(define io-patterns
  '("call-with-output-file" "call-with-input-file"
                            "with-output-to-file"
                            "with-input-from-file"
                            "open-output-file"
                            "open-input-file"
                            "subprocess"
                            "system "
                            "process "
                            "file->string"
                            "file->bytes"
                            "file->lines"
                            "file->list"
                            "display-to-file"
                            "write-to-file"))

(define (has-io-effects? text)
  "Check if the file contains I/O effects mixed with function definitions."
  (and (regexp-match? #rx"\\(define " text)
       (ormap (lambda (pat) (string-contains? text pat)) io-patterns)))

;; ============================================================
;; Directory analysis
;; ============================================================

(define (find-rkt-files root)
  "Find all .rkt files under root, excluding compiled/ and tests/."
  (define (skip-path? p)
    (define str (path->string p))
    (or (string-contains? str "/compiled/")
        (string-contains? str "/.git/")
        (string-contains? str "compiled/")))
  (filter (lambda (f) (not (skip-path? f)))
          (find-files (lambda (p)
                        (and (file-exists? p)
                             (let ([ext (filename-extension p)])
                               (and ext (string=? (bytes->string/utf-8 ext) "rkt")))))
                      root)))

(define (audit-directory root)
  "Analyze all .rkt files under root and return an audit-report hash."
  (define files (find-rkt-files root))
  (define findings
    (filter (lambda (x) x)
            (map (lambda (f)
                   (define path-str
                     (if (path? f)
                         (path->string f)
                         f))
                   (define finding (audit-module path-str))
                   finding)
                 files)))
  (hash 'modules findings 'summary (compute-summary findings)))

;; ============================================================
;; Summary computation
;; ============================================================

(define (compute-summary findings)
  "Compute aggregate statistics from module findings."
  (define sorted-by-size (sort findings > #:key (lambda (f) (hash-ref f 'line-count 0))))
  (define largest (take-at-most sorted-by-size 20))

  (define all-params (filter (lambda (f) (> (hash-ref f 'parameter-count 0) 0)) findings))
  (define all-macros (filter (lambda (f) (> (hash-ref f 'macro-count 0) 0)) findings))
  (define all-struct-out (filter (lambda (f) (hash-ref f 'has-struct-out? #f)) findings))
  (define all-io-mixed (filter (lambda (f) (hash-ref f 'io-mixed-with-logic? #f)) findings))
  (define all-serialization (filter (lambda (f) (> (hash-ref f 'serialization-count 0) 0)) findings))

  (hash 'total-modules
        (length findings)
        'largest-modules
        (map (lambda (f)
               (hash 'path
                     (hash-ref f 'path)
                     'lines
                     (hash-ref f 'line-count)
                     'exports
                     (hash-ref f 'export-count)))
             largest)
        'parameter-usage
        (map (lambda (f) (hash 'path (hash-ref f 'path) 'count (hash-ref f 'parameter-count)))
             (sort all-params > #:key (lambda (f) (hash-ref f 'parameter-count 0))))
        'macro-usage
        (map (lambda (f) (hash 'path (hash-ref f 'path) 'count (hash-ref f 'macro-count)))
             (sort all-macros > #:key (lambda (f) (hash-ref f 'macro-count 0))))
        'struct-out-exports
        (map (lambda (f) (hash-ref f 'path)) all-struct-out)
        'io-mixed-with-logic
        (map (lambda (f) (hash-ref f 'path)) all-io-mixed)
        'serialization-hotspots
        (map (lambda (f) (hash 'path (hash-ref f 'path) 'count (hash-ref f 'serialization-count)))
             (sort all-serialization > #:key (lambda (f) (hash-ref f 'serialization-count 0))))))

(define (take-at-most lst n)
  (if (> (length lst) n)
      (take lst n)
      lst))

;; ============================================================
;; Serialization
;; ============================================================

(define (report->jsexpr report)
  "Convert audit-report to a JSON-serializable jsexpr."
  report) ; Already hash-based jsexpr-compatible

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
  (for ([s (hash-ref summary 'struct-out-exports)])
    (fprintf out "- ~a\n" s))
  (fprintf out "\n")

  ;; I/O mixed with logic
  (fprintf out "## I/O Mixed With Pure Logic\n\n")
  (for ([p (hash-ref summary 'io-mixed-with-logic)])
    (fprintf out "- ~a\n" p))
  (fprintf out "\n")

  ;; Serialization hotspots
  (fprintf out "## Serialization Hotspots\n\n")
  (fprintf out "| Count | Path |\n")
  (fprintf out "|-------|------|\n")
  (for ([s (hash-ref summary 'serialization-hotspots)])
    (fprintf out "| ~a | ~a |\n" (hash-ref s 'count) (hash-ref s 'path)))
  (fprintf out "\n")

  ;; Advisory
  (fprintf out "## Advisory\n\n")
  (fprintf out "This report is advisory. Use with the Abstraction Instruction Manual\n")
  (fprintf out "to identify candidates for boundary clarification.\n"))

;; ============================================================
;; Strict mode thresholds
;; ============================================================

(define strict-thresholds (hash 'max-lines 800 'max-exports 40 'max-require-count 30))

(define (strict-violations report)
  "Return list of strict-mode violations. Empty list = pass."
  (define modules (hash-ref report 'modules))
  (append* (for/list ([m modules])
             (define path (hash-ref m 'path))
             (define lines (hash-ref m 'line-count))
             (define exports (hash-ref m 'export-count))
             (define reqs (hash-ref m 'require-count))
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
                         '())))))

;; ============================================================
;; Main entry point
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
