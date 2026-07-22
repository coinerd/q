#lang racket/base

;; scripts/abstraction-analysis.rkt — AST analysis, dependency detection, provide counting
;;
;; Extracted from abstraction-audit.rkt. Contains pure analysis functions
;; for scanning Racket source files and computing module metadata.

(require racket/list
         racket/string
         racket/port
         racket/file
         racket/path
         racket/match
         json)

(provide audit-module
         audit-content
         audit-directory
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
         count-stringly-verdicts
         count-effectful-classifiers
         count-optional-mandatory-wording
         current-audit-file->lines
         compute-summary
         take-at-most
         io-patterns
         parameter-rx
         macro-rx
         struct-out-rx
         serialization-rx
         handler-rx
         error-rx
         all-defined-out-rx
         contract-out-rx
         mutable-cache-rx
         bench-timing-rx
         ad-hoc-parse-rx
         event-handler-rx
         cwd-sensitive-rx
         mutation-site-rx
         hash-ref-chain-rx
         inline-config-path-rx
         stringly-verdict-rx
         effectful-classifier-rx
         optional-before-asset-rx
         asset-before-optional-rx)

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
          (count-inline-config-paths text)
          'stringly-verdict-count
          (count-stringly-verdicts text)
          'effectful-classifier-count
          (count-effectful-classifiers text)
          'optional-mandatory-count
          (count-optional-mandatory-wording text))))

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
;; DESIGN FACT: Each signal encodes a concrete audit finding from W0–W7:
;;   cwd-sensitive-rx  ← W2 audit: all dynamic-require sites checked for CWD safety
;;   mutation-site-rx  ← W5 mutation boundary: file-writing ops should use write-or-check
;;   hash-ref-chain-rx ← W1 scorecard P5: nested hash-ref chains are fragile
;;   inline-config-path-rx ← W6 adapter audit: ~19 modules inline home-dir ".q" path
;; All signals are advisory only (no --strict thresholds).
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

;; v0.99.42 W1: Manual red-flag signals
;; DESIGN FACT: Each signal encodes a concrete v0.99.40/v0.99.41 incident:
;;   stringly-verdict-rx <- #581: verdict strings scattered across modules
;;   effectful-classifier-rx <- classify functions mixing I/O with classification
;;   optional-mandatory-rx <- #581: optional wording for mandatory assets
(define stringly-verdict-rx
  (pregexp (string-append "\"(?:success|failure|failed?|pending|cancelled?|skipped?|in_progress|"
                          "completed?|blocked?|red|green|pass|fail|unknown|error|warning|"
                          "timeout|neutral|action_required|publication_succeeded_smoke_failed|"
                          "current_blocking_red_[a-zA-Z_0-9]+)\"")))
(define effectful-classifier-rx
  #px"\\(define\\s+\\((?:classify|check|verify|gate|make-[a-zA-Z_0-9]*-verdict|make-[a-zA-Z_0-9]*-result)[-_a-zA-Z]*\\s")
(define optional-before-asset-rx
  #px"(?:optional|may[ ]+(?:be[ ]+)?(?:missing|absent|skipped))[^\n]{0,80}(?:manifest|asset|tarball)")
(define asset-before-optional-rx
  #px"(?:manifest|asset|tarball)[^\n]{0,80}(?:optional|may[ ]+(?:be[ ]+)?(?:missing|absent|skipped))")

(define (count-stringly-verdicts text)
  "Count string literals used as status/verdict values (failure-design smell)."
  (count-matches text stringly-verdict-rx))

(define (count-effectful-classifiers text)
  "Count classify/verdict/check/verify function definitions that may mix effects."
  (count-matches text effectful-classifier-rx))

(define (count-optional-mandatory-wording text)
  "Count optional language near mandatory release assets (manifest/tarball/asset)."
  (+ (count-matches text optional-before-asset-rx) (count-matches text asset-before-optional-rx)))

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

  ;; v0.99.42 W1: Manual red-flag signals
  (define all-stringly-verdicts
    (filter (lambda (f) (> (hash-ref f 'stringly-verdict-count 0) 0)) findings))
  (define all-effectful-classifiers
    (filter (lambda (f) (> (hash-ref f 'effectful-classifier-count 0) 0)) findings))
  (define all-optional-mandatory
    (filter (lambda (f) (> (hash-ref f 'optional-mandatory-count 0) 0)) findings))

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
   (map (lambda (f) (hash 'path (hash-ref f 'path) 'count (hash-ref f 'inline-config-path-count 0)))
        (sort all-inline-config-paths > #:key (lambda (f) (hash-ref f 'inline-config-path-count 0))))
   'stringly-verdict-modules
   (map (lambda (f) (hash 'path (hash-ref f 'path) 'count (hash-ref f 'stringly-verdict-count 0)))
        (sort all-stringly-verdicts > #:key (lambda (f) (hash-ref f 'stringly-verdict-count 0))))
   'effectful-classifier-modules
   (map
    (lambda (f) (hash 'path (hash-ref f 'path) 'count (hash-ref f 'effectful-classifier-count 0)))
    (sort all-effectful-classifiers > #:key (lambda (f) (hash-ref f 'effectful-classifier-count 0))))
   'optional-mandatory-modules
   (map (lambda (f) (hash 'path (hash-ref f 'path) 'count (hash-ref f 'optional-mandatory-count 0)))
        (sort all-optional-mandatory > #:key (lambda (f) (hash-ref f 'optional-mandatory-count 0))))))

(define (take-at-most lst n)
  (if (> (length lst) n)
      (take lst n)
      lst))
