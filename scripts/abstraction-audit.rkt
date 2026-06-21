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
;; By default this scanner reports production/source modules and skips tests/,
;; compiled/, .git/, .planning/, and .pi/ trees.
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
;;   - Error/raise density (error, raise)
;;   - Provide form shapes (all-defined-out, struct-out, contract-out, explicit)
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
         has-io-effects?
         count-io-effects
         detect-provide-shapes
         count-struct-outs
         count-mutable-caches
         count-bench-timing
         count-ad-hoc-parsing
         count-event-handlers
         count-cwd-sensitive-paths
         count-mutation-sites
         count-hash-ref-chains
         count-inline-config-paths
         audit-content
         current-audit-file->lines)

;; ============================================================
;; I/O parameter (W6 v0.99.37)
;; ============================================================

(define current-audit-file->lines (make-parameter file->lines))

;; ============================================================
;; Core data structures
;; ============================================================

;; A module-finding is a hash with keys:
;;   path, line-count, export-count, parameter-count, macro-count,
;;   struct-out-count, has-struct-out?, io-count, io-mixed-with-logic?,
;;   serialization-count, handler-count, error-count, provide-shapes,
;;   require-count

;; An audit-report is a hash with keys:
;;   modules   — list of module-finding hashes
;;   summary   — hash with aggregate statistics

;; ============================================================
;; Per-file analysis
;; ============================================================

(define (audit-module path)
  "Analyze a single .rkt file and return a module-finding hash.
   Returns #f if the file cannot be read.
   Uses current-audit-file->lines parameter for I/O (W6 v0.99.37)."
  (define reader (current-audit-file->lines))
  (define lines
    (with-handlers ([exn:fail:filesystem? (lambda (_) #f)])
      (reader path)))
  (if (not lines)
      #f
      (audit-content path (string-join lines "\n"))))

(define (audit-content path text)
  "Analyze module text content and return a module-finding hash.
   Pure function: no I/O. W6 v0.99.37."
  (let* ([lines (string-split text "\n")]
         [line-count (length lines)]
         [export-count (count-exports text)]
         [parameter-count (count-matches text parameter-rx)]
         [macro-count (count-matches text macro-rx)]
         [struct-out-count (count-struct-outs text)]
         [has-struct-out? (> struct-out-count 0)]
         [io-count (count-io-effects text)]
         [io? (> io-count 0)]
         [serialization-count (count-matches text serialization-rx)]
         [handler-count (count-matches text handler-rx)]
         [error-count (count-matches text error-rx)]
         [provide-shapes (detect-provide-shapes text)]
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
          'struct-out-count
          struct-out-count
          'has-struct-out?
          has-struct-out?
          'io-count
          io-count
          'io-mixed-with-logic?
          io?
          'serialization-count
          serialization-count
          'handler-count
          handler-count
          'error-count
          error-count
          'provide-shapes
          provide-shapes
          'require-count
          require-count
          'mutable-cache-count
          (count-mutable-caches text)
          'bench-timing-count
          (count-bench-timing text)
          'ad-hoc-parse-count
          (count-ad-hoc-parsing text)
          'event-handler-count
          (count-event-handlers text)
          'cwd-sensitive-path-count
          (count-cwd-sensitive-paths text)
          'mutation-site-count
          (count-mutation-sites text)
          'hash-ref-chain-count
          (count-hash-ref-chains text)
          'inline-config-path-count
          (count-inline-config-paths text))))

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
(define error-rx #px"\\(error\\b|\\(raise\\b")
(define all-defined-out-rx #px"all-defined-out")
(define contract-out-rx #px"contract-out")

;; v0.99.37 W1: New boundary-semantics signals
(define mutable-cache-rx #px"make-hash|hash-set!|hash-remove!|hash-update!")
(define bench-timing-rx #px"current-inexact-milliseconds")
(define ad-hoc-parse-rx #px"regexp-match|string-split|string-trim|regexp-replace")
;; Handler/model risk: defines functions AND dispatches on event/state types
(define event-handler-rx #px"define.*handler|handle-event|handle-key|dispatch")

;; v0.99.38 W8: Change-locality signals
;; CWD-sensitive: dynamic-require with relative-path string, file->* without build-path
(define cwd-sensitive-rx
  #px"dynamic-require\\s+\"[^/]|file->string\\s+\"[^/]|file->lines\\s+\"[^/]|file->bytes\\s+\"[^/]|file->list\\s+\"[^/]")
;; Mutation sites: file-writing calls that modify the filesystem
(define mutation-site-rx
  #px"call-with-output-file|with-output-to-file|display-to-file|write-to-file|open-output-file")
;; Hash-ref chains: nested hash-ref calls (common-case API pain point P5)
(define hash-ref-chain-rx #px"hash-ref\\s+\\(hash-ref")
;; Inline config paths: (build-path (find-system-path 'home-dir) ...) pattern
(define inline-config-path-rx #px"build-path\\s+\\(find-system-path\\s+'home-dir")

(define (count-matches text rx)
  (length (regexp-match* rx text)))

(define (count-struct-outs text)
  "Count the number of struct-out forms in the text."
  (length (regexp-match* struct-out-rx text)))

(define (count-mutable-caches text)
  "Count mutable cache operations in text."
  (count-matches text mutable-cache-rx))

(define (count-bench-timing text)
  "Count benchmark timing assertions in text."
  (count-matches text bench-timing-rx))

(define (count-ad-hoc-parsing text)
  "Count ad-hoc string parsing operations in text."
  (count-matches text ad-hoc-parse-rx))

(define (count-event-handlers text)
  "Count event handler definitions in text."
  (count-matches text event-handler-rx))

;; v0.99.38 W8: Change-locality signal counters
(define (count-cwd-sensitive-paths text)
  "Count CWD-sensitive dynamic-require / file->* patterns in text.
   These are relative-path string arguments that break if CWD changes."
  (count-matches text cwd-sensitive-rx))

(define (count-mutation-sites text)
  "Count file-writing mutation sites (call-with-output-file, etc.) in text."
  (count-matches text mutation-site-rx))

(define (count-hash-ref-chains text)
  "Count nested hash-ref chains in text (common-case API pain point P5)."
  (count-matches text hash-ref-chain-rx))

(define (count-inline-config-paths text)
  "Count inline (build-path (find-system-path 'home-dir) ...) patterns in text.
   These should use the shared global-config-dir helper instead."
  (count-matches text inline-config-path-rx))

(define (count-io-effects text)
  "Count I/O effect occurrences in text."
  (apply +
         (map (lambda (pat) (length (regexp-match* (regexp (regexp-quote pat)) text))) io-patterns)))

(define (detect-provide-shapes text)
  "Detect which provide form shapes are present. Returns a list of symbols."
  (define shapes '())
  (when (regexp-match? all-defined-out-rx text)
    (set! shapes (cons 'all-defined-out shapes)))
  (when (regexp-match? struct-out-rx text)
    (set! shapes (cons 'struct-out shapes)))
  (when (regexp-match? contract-out-rx text)
    (set! shapes (cons 'contract-out shapes)))
  ;; Explicit IDs: detect bare identifiers in provide forms.
  ;; Flatten newlines so multi-line provides can be checked.
  ;; Pattern 1: (provide bare-id ...) — first token is a bare identifier.
  ;; Pattern 2: (provide (sub-form) bare-id ...) — bare ID after a sub-form.
  (define flat (regexp-replace* #rx"\n" text " "))
  (when (or (regexp-match? #px"\\(provide\\s+[a-zA-Z!_@%-]" flat)
            (regexp-match? #px"\\(provide\\s.*?\\)\\s+[a-zA-Z!_@%-]" flat))
    (set! shapes (cons 'explicit shapes)))
  (reverse shapes) ; preserve discovery order
  )

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
  "Find production .rkt files under root, excluding tests/, compiled/, .git/, .planning/, and .pi/."
  (define (skip-path? p)
    (define str (path->string (simple-form-path p)))
    (define normalized (regexp-replace* #rx"\\\\" str "/"))
    (define (has-path-segment? segment)
      (regexp-match? (pregexp (format "(^|/)~a(/|$)" (regexp-quote segment))) normalized))
    (or (has-path-segment? "compiled")
        (has-path-segment? "tests")
        (has-path-segment? ".git")
        (has-path-segment? ".planning")
        (has-path-segment? ".pi")))
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
  (define all-errors (filter (lambda (f) (> (hash-ref f 'error-count 0) 0)) findings))
  (define all-handlers (filter (lambda (f) (> (hash-ref f 'handler-count 0) 0)) findings))
  (define all-all-defined-out
    (filter (lambda (f)
              (and (list? (hash-ref f 'provide-shapes '()))
                   (member 'all-defined-out (hash-ref f 'provide-shapes '()))))
            findings))
  (define all-mutable-cache (filter (lambda (f) (> (hash-ref f 'mutable-cache-count 0) 0)) findings))
  (define all-bench-timing (filter (lambda (f) (> (hash-ref f 'bench-timing-count 0) 0)) findings))
  (define all-ad-hoc-parse (filter (lambda (f) (> (hash-ref f 'ad-hoc-parse-count 0) 0)) findings))
  (define all-event-handlers (filter (lambda (f) (> (hash-ref f 'event-handler-count 0) 0)) findings))

  ;; v0.99.38 W8: Change-locality signals
  (define all-cwd-sensitive
    (filter (lambda (f) (> (hash-ref f 'cwd-sensitive-path-count 0) 0)) findings))
  (define all-mutation-sites (filter (lambda (f) (> (hash-ref f 'mutation-site-count 0) 0)) findings))
  (define all-hash-ref-chains
    (filter (lambda (f) (> (hash-ref f 'hash-ref-chain-count 0) 0)) findings))
  (define all-inline-config-paths
    (filter (lambda (f) (> (hash-ref f 'inline-config-path-count 0) 0)) findings))

  (hash
   'total-modules
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
   (map (lambda (f) (hash 'path (hash-ref f 'path) 'count (hash-ref f 'struct-out-count 0)))
        (sort all-struct-out > #:key (lambda (f) (hash-ref f 'struct-out-count 0))))
   'io-mixed-with-logic
   (map (lambda (f) (hash 'path (hash-ref f 'path) 'count (hash-ref f 'io-count 0)))
        (sort all-io-mixed > #:key (lambda (f) (hash-ref f 'io-count 0))))
   'serialization-hotspots
   (map (lambda (f) (hash 'path (hash-ref f 'path) 'count (hash-ref f 'serialization-count)))
        (sort all-serialization > #:key (lambda (f) (hash-ref f 'serialization-count 0))))
   'error-density
   (map (lambda (f) (hash 'path (hash-ref f 'path) 'count (hash-ref f 'error-count 0)))
        (sort all-errors > #:key (lambda (f) (hash-ref f 'error-count 0))))
   'handler-density
   (map (lambda (f) (hash 'path (hash-ref f 'path) 'count (hash-ref f 'handler-count 0)))
        (sort all-handlers > #:key (lambda (f) (hash-ref f 'handler-count 0))))
   'all-defined-out-modules
   (map (lambda (f) (hash-ref f 'path)) all-all-defined-out)
   'mutable-cache-modules
   (map (lambda (f) (hash 'path (hash-ref f 'path) 'count (hash-ref f 'mutable-cache-count 0)))
        (sort all-mutable-cache > #:key (lambda (f) (hash-ref f 'mutable-cache-count 0))))
   'bench-timing-modules
   (map (lambda (f) (hash 'path (hash-ref f 'path) 'count (hash-ref f 'bench-timing-count 0)))
        (sort all-bench-timing > #:key (lambda (f) (hash-ref f 'bench-timing-count 0))))
   'ad-hoc-parse-modules
   (map (lambda (f) (hash 'path (hash-ref f 'path) 'count (hash-ref f 'ad-hoc-parse-count 0)))
        (sort all-ad-hoc-parse > #:key (lambda (f) (hash-ref f 'ad-hoc-parse-count 0))))
   'event-handler-modules
   (map (lambda (f) (hash 'path (hash-ref f 'path) 'count (hash-ref f 'event-handler-count 0)))
        (sort all-event-handlers > #:key (lambda (f) (hash-ref f 'event-handler-count 0))))
   'cwd-sensitive-path-modules
   (map (lambda (f) (hash 'path (hash-ref f 'path) 'count (hash-ref f 'cwd-sensitive-path-count 0)))
        (sort all-cwd-sensitive > #:key (lambda (f) (hash-ref f 'cwd-sensitive-path-count 0))))
   'mutation-site-modules
   (map (lambda (f) (hash 'path (hash-ref f 'path) 'count (hash-ref f 'mutation-site-count 0)))
        (sort all-mutation-sites > #:key (lambda (f) (hash-ref f 'mutation-site-count 0))))
   'hash-ref-chain-modules
   (map (lambda (f) (hash 'path (hash-ref f 'path) 'count (hash-ref f 'hash-ref-chain-count 0)))
        (sort all-hash-ref-chains > #:key (lambda (f) (hash-ref f 'hash-ref-chain-count 0))))
   'inline-config-path-modules
   (map
    (lambda (f) (hash 'path (hash-ref f 'path) 'count (hash-ref f 'inline-config-path-count 0)))
    (sort all-inline-config-paths > #:key (lambda (f) (hash-ref f 'inline-config-path-count 0))))))

(define (take-at-most lst n)
  (if (> (length lst) n)
      (take lst n)
      lst))

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
