#lang racket/base

;; q/scripts/run-tests/classify-metadata.rkt — File metadata parsing + base-dir resolution
;;
;; Extracted from classify.rkt in v0.99.58 W3-1 (P3-CL).
;; Shared infrastructure used by both classify.rkt (file collection)
;; and classify-filters.rkt (classification predicates).
;; STABILITY: internal (test runner infrastructure)

(require racket/string
         racket/list
         racket/file)

(provide base-dir
          q-root-candidate?
          resolve-base-dir
          metadata-cache
          clear-metadata-cache!
          metadata-tokens
          metadata-bool
          metadata-line-match
          get-file-metadata
          ;; Schema v1 + report-only lint (W1)
          metadata-schema-version
          schema-known-tags
          schema-reserved-tags
          schema-required-tags
          schema-suite-values
          schema-speed-values
          schema-boundary-values
          schema-mutates-values
          schema-isolation-values
          canonical-isolation
          deprecated-isolation-alias?
          validate-file
          validate-files
          summarize-findings
          findings->jsexpr
          lint-summary->jsexpr
          print-lint-report)

;; ============================================================
;; Base directory resolution
;; ============================================================

(define (q-root-candidate? p)
  (and (directory-exists? (build-path p "tests"))
       (file-exists? (build-path p "scripts" "run-tests.rkt"))))

(define (resolve-base-dir orig)
  (define parent (simplify-path (build-path orig "..")))
  (define candidates
    (list (simplify-path (build-path orig "q")) (simplify-path (build-path parent "q")) orig parent))
  (or (for/first ([candidate (in-list candidates)]
                  #:when (q-root-candidate? candidate))
        candidate)
      orig))

(define base-dir (resolve-base-dir (find-system-path 'orig-dir)))

;; ============================================================
;; Metadata parser
;; ============================================================

(define metadata-cache (make-hash))

(define (clear-metadata-cache!)
  (hash-clear! metadata-cache))

(define (metadata-tokens raw)
  (filter (lambda (s) (not (string=? s ""))) (regexp-split #rx"[ ,\t]+" (string-trim raw))))

(define (metadata-bool raw default)
  (define normalized (string-downcase (string-trim raw)))
  (cond
    [(string=? normalized "") default]
    [(member normalized '("true" "yes" "1" "on")) #t]
    [(member normalized '("false" "no" "0" "off")) #f]
    [else default]))

(define (metadata-line-match line tag)
  (define pattern (pregexp (format "@~a(?:[[:space:]]+([^;]*))?" (regexp-quote tag))))
  (define m (regexp-match pattern line))
  (and m (list line (string-trim (or (cadr m) "")))))

(define (get-file-metadata f)
  (hash-ref!
   metadata-cache
   f
   (lambda ()
     (define full-path
       (if (absolute-path? f)
           f
           (build-path base-dir f)))
     (cond
       [(not (file-exists? full-path)) (hash)]
       [else
        (define speed #f)
        (define suite #f)
        (define suites '())
        (define requires '())
        (define not-test? #f)
        (define mutates #f)
        (define boundary #f)
         (define isolation #f)
         (define isolation-raw #f)
         (define timeout #f)
        (with-handlers ([exn:fail? (lambda (_) (void))])
          (call-with-input-file full-path
                                (lambda (port)
                                  (for ([_ (in-range 50)]
                                        #:break (eof-object? (peek-byte port)))
                                    (define line (read-line port))
                                    (when (string? line)
                                      (define speed-match (metadata-line-match line "speed"))
                                      (when speed-match
                                        (define toks (metadata-tokens (cadr speed-match)))
                                        (when (pair? toks)
                                          (set! speed (string->symbol (car toks)))))
                                      (define suite-match (metadata-line-match line "suite"))
                                      (when suite-match
                                        (set! suites (metadata-tokens (cadr suite-match)))
                                        (set! suite (and (pair? suites) (car suites))))
                                      (define requires-match (metadata-line-match line "requires"))
                                      (when requires-match
                                        (set! requires (metadata-tokens (cadr requires-match))))
                                      (define not-test-match (metadata-line-match line "not-test"))
                                      (when not-test-match
                                        (set! not-test? (metadata-bool (cadr not-test-match) #t)))
                                      (define mutates-match (metadata-line-match line "mutates"))
                                      (when mutates-match
                                        (set! mutates (string-trim (cadr mutates-match))))
                                      (define boundary-match (metadata-line-match line "boundary"))
                                      (when boundary-match
                                        (set! boundary (string-trim (cadr boundary-match))))
                                      (define isolation-match (metadata-line-match line "isolation"))
                                      (when isolation-match
                                        (set! isolation (string-trim (cadr isolation-match))))
                                      (define timeout-match
                                        (regexp-match #rx";+[ \t]*@timeout[ \t]+([0-9]+)" line))
                                      (when timeout-match
                                        (set! timeout (string->number (cadr timeout-match)))))))))
         ;; Schema v1 normalization (W1): `subprocess` is a deprecated alias
         ;; for the canonical `process` isolation value. Normalize on parse so
         ;; every consumer sees the canonical spelling; retain the raw value
         ;; under 'isolation-raw so the lint can flag it for migration.
         (define canonical-iso (and isolation (canonical-isolation isolation)))
         (when (and isolation canonical-iso (not (string=? isolation canonical-iso)))
           (set! isolation-raw isolation)
           (set! isolation canonical-iso))
         (hash 'speed
               speed
               'suite
               suite
               'suites
               suites
               'requires
               requires
               'not-test?
               not-test?
               'mutates
               mutates
               'boundary
               boundary
               'isolation
               isolation
               'isolation-raw
               isolation-raw
               'timeout
               timeout
               ;; Classification provenance: 'explicit when the file carries
               ;; @suite/@speed metadata; 'heuristic when selection relies on
               ;; filename/path heuristics.
               'classification
               (if (or suite speed) 'explicit 'heuristic))]))))

;; ============================================================
;; Metadata schema (v1) and report-only lint (W1)
;; ============================================================
;;
;; The declarative metadata contract was previously implicit and unenforced.
;; Schema v1 makes it explicit: a fixed tag vocabulary, allowed values per
;; tag, required tags, and a single canonical @isolation vocabulary. The lint
;; is REPORT-ONLY in W1 (the CLI mode always exits 0); enforcement is
;; deferred to W3. Documented in docs/TEST_CONVENTIONS.md.

(define metadata-schema-version 1)

;; Full tag vocabulary. `covers` is forward-reserved: recognized by the
;; schema and accepted by the parser, but its value is not yet validated
;; or consumed anywhere.
(define schema-known-tags
  '("suite" "speed" "boundary" "mutates" "isolation" "timeout" "requires" "covers"))
(define schema-reserved-tags '("covers"))
(define schema-required-tags '("suite" "speed"))

;; Allowed values per tag (strings exactly as they appear in the header).
(define schema-suite-values
  '("all" "broad" "fast" "unit" "unit-fast" "slow" "smoke" "release-smoke" "tui"
    "tui-tmux" "security" "arch" "runtime" "extensions" "workflows" "platform"
    "mutating" "skills" "ci" "testing" "integration" "tools" "provider" "gsd"
    "verifier" "harness" "default"))
(define schema-speed-values '("fast" "slow"))
(define schema-boundary-values '("unit" "integration" "e2e"))
(define schema-mutates-values '("none" "env" "cwd" "fs" "repo" "temp" "home"))
(define schema-requires-values '("terminal" "browser" "network" "provider-key" "git" "fs"))
(define schema-timeout-value-pattern #rx"^[0-9]+$")

;; Canonical @isolation vocabulary. `process` is canonical. `subprocess` is a
;; deprecated alias that normalizes to `process` and is flagged by the lint
;; until migration completes. `temp-dir` marks tests that only need a fresh
;; temporary directory (weaker than process isolation).
(define schema-isolation-values '("process" "temp-dir" "subprocess"))
(define schema-isolation-deprecated-aliases '(("subprocess" . "process")))

(define (canonical-isolation v)
  (cond
    [(assoc v schema-isolation-deprecated-aliases) => cdr]
    [else v]))

(define (deprecated-isolation-alias? v)
  (and (assoc v schema-isolation-deprecated-aliases) #t))

;; Raw header tag extraction: `@tag value` pairs from the first 50 lines.
;; Only comment lines are considered, so racket-level `@` forms are ignored.
;; Multiple tags may appear on one comment line, either in separate `;;`
;; segments (`;; @speed fast  ;; @suite unit`) or space-separated
;; (`;; @speed fast @suite default`). Every `@tag [value]` occurrence in a
;; comment line is scanned; a value ends at the next `@` or `;`.
(define header-comment-line-pattern #rx"^[ \t]*;+")
(define header-chunk-tag-pattern
  (pregexp "^([A-Za-z][A-Za-z0-9_-]*)(?:[ \t]+(.*))?"))

(define (clean-tag-value v)
  (string-trim (regexp-replace* #rx";.*$" (or v "") "")))

(define (extract-header-tags f)
  (define full-path (if (absolute-path? f) f (build-path base-dir f)))
  (cond
    [(not (file-exists? full-path)) '()]
    [else
     (define acc '())
     (with-handlers ([exn:fail? (lambda (_) (void))])
       (call-with-input-file full-path
         (lambda (port)
           (for ([_ (in-range 50)]
                 #:break (eof-object? (peek-byte port)))
             (define line (read-line port))
             (when (and (string? line)
                        (regexp-match? header-comment-line-pattern line))
               (define chunks (regexp-split #rx"@" line))
               (for ([chunk (in-list (cdr chunks))])
                 (define m (regexp-match header-chunk-tag-pattern chunk))
                 (when (and m (cadr m))
                   (set! acc
                         (cons (cons (string-downcase (cadr m))
                                     (clean-tag-value (caddr m)))
                               acc)))))))))
     (reverse acc)]))

(define (raw-tag-value tags tag)
  (for/first ([tv (in-list tags)]
              #:when (string=? (car tv) tag))
    (cdr tv)))

;; ---- Findings ----

(define (make-finding kind code tag message)
  (hasheq 'kind kind 'code code 'tag tag 'message message))

(define (lint-area f)
  (define p (if (path? f) (path->string f) f))
  (define m (regexp-match #rx"^tests/([^/]+)/" p))
  (cond
    [(and m (cadr m)) (string-append "(" (cadr m) ")")]
    [(string-prefix? p "tests/") "(root)"]
    [else "(other)"]))

;; validate-file : path-string? -> hash?
;; Structured result:
;;   'file           the path as given
;;   'area           module-area bucket (tests/<area>/)
;;   'classification  'explicit | 'heuristic
;;   'findings       list of finding hashes ('kind 'error|'warning|'info,
;;                    'code, 'tag, 'message)
;;   'normalized     metadata with canonical isolation values
(define (validate-file f)
  (define meta (get-file-metadata f))
  (define tags (extract-header-tags f))
  (define findings '())
  (define (add! kind code tag message)
    (set! findings (cons (make-finding kind code tag message) findings)))
  ;; 1. Tag vocabulary: unknown tags are errors; reserved tags get an info note.
  (for ([tv (in-list tags)])
    (define tag (car tv))
    (cond
      [(member tag schema-known-tags)
       (when (member tag schema-reserved-tags)
         (add! 'info 'reserved-tag tag
               "forward-reserved tag: accepted by schema v1, value not yet enforced"))]
      [else
       (add! 'error 'unknown-tag tag
             (format "unknown tag @~a (schema v~a vocabulary: ~a)"
                     tag metadata-schema-version (string-join schema-known-tags " ")))]))
  (define (raw tag) (raw-tag-value tags tag))
  ;; 2. Enum validations. These use the RAW header value so malformed
  ;;    spellings that the lenient parser silently drops are still visible
  ;;    to the lint.
  (define suite-raw (raw "suite"))
  (when suite-raw
    (for ([tok (in-list (metadata-tokens suite-raw))])
      (unless (member tok schema-suite-values)
        (add! 'error 'invalid-suite "suite"
               (format "unknown suite value `~a` (valid: ~a)"
                       tok (string-join schema-suite-values " "))))))
  (define speed-raw (raw "speed"))
  (when speed-raw
    (for ([tok (in-list (metadata-tokens speed-raw))])
      (unless (member tok schema-speed-values)
        (add! 'error 'invalid-speed "speed"
               (format "invalid speed `~a` (valid: fast slow)" tok)))))
  (define boundary-raw (raw "boundary"))
  (when boundary-raw
    (unless (member boundary-raw schema-boundary-values)
      (add! 'error 'invalid-boundary "boundary"
             (format "invalid boundary `~a` (valid: ~a)"
                     boundary-raw (string-join schema-boundary-values " ")))))
  (define mutates-raw (raw "mutates"))
  (when mutates-raw
    (for ([tok (in-list (metadata-tokens mutates-raw))])
      (unless (member tok schema-mutates-values)
        (add! 'error 'invalid-mutates "mutates"
               (format "invalid mutates token `~a` (valid: ~a)"
                       tok (string-join schema-mutates-values " "))))))
  (define isolation-header-raw (raw "isolation"))
  (cond
    [(not isolation-header-raw) (void)]
    [(deprecated-isolation-alias? isolation-header-raw)
     (add! 'warning 'deprecated-isolation-alias "isolation"
           (format "deprecated alias `~a` normalizes to `~a`"
                   isolation-header-raw (canonical-isolation isolation-header-raw)))]
    [(not (member isolation-header-raw schema-isolation-values))
     (add! 'error 'invalid-isolation "isolation"
           (format "invalid isolation `~a` (canonical vocabulary: ~a)"
                   isolation-header-raw (string-join schema-isolation-values " ")))])
  (define timeout-raw (raw "timeout"))
  (when (and timeout-raw
             (not (or (string=? timeout-raw "")
                      (regexp-match? schema-timeout-value-pattern timeout-raw))))
    (add! 'error 'malformed-timeout "timeout"
          (format "malformed timeout `~a` (expected positive integer seconds)"
                  timeout-raw)))
  (define requires-raw (raw "requires"))
  (when requires-raw
    (for ([tok (in-list (metadata-tokens requires-raw))])
      (unless (member tok schema-requires-values)
        (add! 'error 'unknown-requires-token "requires"
               (format "unknown requirement `~a` (valid: ~a)"
                       tok (string-join schema-requires-values " "))))))
  ;; 3. Required tags.
  (for ([tag (in-list schema-required-tags)])
    (unless (raw tag)
      (add! 'warning 'missing-required tag
            (format "missing required tag @~a (schema v~a)"
                    tag metadata-schema-version))))
  ;; 4. Provenance: heuristic-only files are selected by filename/path.
  (define classification (hash-ref meta 'classification 'heuristic))
  (when (eq? classification 'heuristic)
    (add! 'info 'heuristic-classification "suite"
          "no @suite/@speed: classification relies on filename/path heuristics"))
  (hasheq 'file
          (if (path? f) (path->string f) f)
          'area (lint-area f)
          'classification classification
          'findings (reverse findings)
          'normalized
          (hasheq 'isolation (canonical-isolation (hash-ref meta 'isolation #f))
                  'isolation-raw (hash-ref meta 'isolation-raw #f)
                  'suite (hash-ref meta 'suite #f)
                  'speed (hash-ref meta 'speed #f))))

;; validate-files : (listof path-string?) -> (listof hash?)
(define (validate-files files) (map validate-file files))

;; ---- Aggregation ----

(define (summarize-findings results)
  (define invalid 0)
  (define deprecated 0)
  (define missing-required 0)
  (define explicit 0)
  (define heuristic 0)
  (define per-area (make-hash))
  (for ([r (in-list results)])
    (define area (hash-ref r 'area "(other)"))
    (define a
      (hash-ref! per-area area
                 (lambda ()
                   (make-hash (list (cons 'invalid 0)
                                    (cons 'deprecated_alias 0)
                                    (cons 'missing_required 0)
                                    (cons 'files 0))))))
    (hash-update! a 'files add1)
    (for ([f (in-list (hash-ref r 'findings '()))])
      (define code (hash-ref f 'code))
      (cond
        [(eq? code 'deprecated-isolation-alias)
         (set! deprecated (add1 deprecated))
         (hash-update! a 'deprecated_alias add1)]
        [(eq? code 'missing-required)
         (set! missing-required (add1 missing-required))
         (hash-update! a 'missing_required add1)]
        [(eq? (hash-ref f 'kind) 'error)
         (set! invalid (add1 invalid))
         (hash-update! a 'invalid add1)]))
    (if (eq? (hash-ref r 'classification) 'explicit)
        (set! explicit (add1 explicit))
        (set! heuristic (add1 heuristic))))
  (hasheq 'schema_version metadata-schema-version
          'file_count (length results)
          'invalid_count invalid
          'deprecated_alias_count deprecated
          'missing_required_count missing-required
          'classification (hasheq 'explicit explicit 'heuristic heuristic)
          'per_area (for/hasheq ([(area a) (in-hash per-area)])
                      (values (string->symbol area)
                              (make-immutable-hasheq (hash->list a))))))

;; ---- jsexpr helpers (for --json-out payloads) ----

(define (finding->jsexpr f)
  (hasheq 'kind (symbol->string (hash-ref f 'kind))
          'code (symbol->string (hash-ref f 'code))
          'tag (hash-ref f 'tag)
          'message (hash-ref f 'message)))

(define (file-result->jsexpr r)
  (hasheq 'file (hash-ref r 'file)
          'area (hash-ref r 'area)
          'classification (symbol->string (hash-ref r 'classification))
          'findings (map finding->jsexpr (hash-ref r 'findings))))

(define (findings->jsexpr results) (map file-result->jsexpr results))

(define (lint-summary->jsexpr s)
  (hasheq 'schema_version (hash-ref s 'schema_version)
          'file_count (hash-ref s 'file_count)
          'invalid_count (hash-ref s 'invalid_count)
          'deprecated_alias_count (hash-ref s 'deprecated_alias_count)
          'missing_required_count (hash-ref s 'missing_required_count)
          'classification (hash-ref s 'classification)
          'per_area (hash-ref s 'per_area)))

;; ---- Report-only CLI lint (W1) ----
;; Prints per-file findings (errors and warnings; info findings stay in the
;; structured result) and an aggregate with per-area counts. ALWAYS exits 0
;; in this wave; enforcement is deferred to W3.

(define (print-lint-report files)
  (define results (validate-files files))
  (printf ";; METADATA LINT — schema v~a — REPORT-ONLY (enforcement deferred to W3)~n"
          metadata-schema-version)
  (printf ";; ════════════════════════════════════════════════════════════~n")
  (for ([r (in-list results)])
    (define fs
      (filter (lambda (f) (memq (hash-ref f 'kind) '(error warning)))
              (hash-ref r 'findings '())))
    (when (pair? fs)
      (printf "~a  [~a]~n" (hash-ref r 'file) (hash-ref r 'classification))
      (for ([f (in-list fs)])
        (printf "    ~a @~a [~a]: ~a~n"
                (hash-ref f 'kind)
                (hash-ref f 'tag)
                (hash-ref f 'code)
                (hash-ref f 'message)))))
  (define s (summarize-findings results))
  (newline)
  (printf ";; aggregate: files=~a invalid=~a deprecated-alias=~a missing-required=~a explicit=~a heuristic-only=~a~n"
          (hash-ref s 'file_count)
          (hash-ref s 'invalid_count)
          (hash-ref s 'deprecated_alias_count)
          (hash-ref s 'missing_required_count)
          (hash-ref (hash-ref s 'classification) 'explicit)
          (hash-ref (hash-ref s 'classification) 'heuristic))
  (define areas
    (sort (for/list ([(k v) (in-hash (hash-ref s 'per_area))]) k)
          (lambda (x y) (string<? (symbol->string x) (symbol->string y)))))
  (for ([area (in-list areas)])
    (define a (hash-ref (hash-ref s 'per_area) area))
    (printf ";; [~a] invalid:~a deprecated:~a missing-required:~a files:~a~n"
            area
            (hash-ref a 'invalid)
            (hash-ref a 'deprecated_alias)
            (hash-ref a 'missing_required)
            (hash-ref a 'files)))
  s)
