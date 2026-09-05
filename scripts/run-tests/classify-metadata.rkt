#lang racket/base

;; q/scripts/run-tests/classify-metadata.rkt — File metadata parsing + base-dir resolution
;;
;; Extracted from classify.rkt in v0.99.58 W3-1 (P3-CL).
;; Shared infrastructure used by both classify.rkt (file collection)
;; and classify-filters.rkt (classification predicates).
;; STABILITY: internal (test runner infrastructure)

(require racket/string
         racket/list
         racket/file
         racket/path)

;; NOTE (W2): deliberately no `racket/json` require. This script must run on
;; bare installations (the repo's CI base image ships a minimal Racket whose
;; collection tree has no racket/json.rkt), so JSON emission is implemented
;; locally below. Keep it that way unless the CI image is pinned to a full
;; distribution — a missing collection here is a discovery tool outage.

(provide base-dir
         q-root-candidate?
         resolve-base-dir
         resolve-repository-root
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
         print-lint-report
         ;; Canonical test-file discovery + deterministic inventory (W2)
         sha256-hex
         discovery-ignored-directory-names
         discovery-ignored-path-prefixes
         discovery-support-module-names
         discover-metadata-files
         results-with-finding-code
         build-metadata-inventory
         emit-metadata-inventory-json)

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
  (hash-ref! metadata-cache
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
                  (define covers '())
                  (define not-test? #f)
                  (define mutates #f)
                  (define boundary #f)
                  (define isolation #f)
                  (define isolation-raw #f)
                  (define timeout #f)
                  (with-handlers ([exn:fail? (lambda (_) (void))])
                    (call-with-input-file
                     full-path
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
                           ;; @covers (W4): production modules/contracts a test
                           ;; directly validates, repo-root-relative paths.
                           ;; Multiple @covers lines accumulate in order.
                           (define covers-match (metadata-line-match line "covers"))
                           (when covers-match
                             (set! covers (append covers (metadata-tokens (cadr covers-match)))))
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
                        'covers
                        covers
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

;; Full tag vocabulary. `covers` (W4): repo-root-relative production
;; modules/contracts the test directly validates; parsed by the metadata
;; parser, validated by manifest generation (generate-covers-manifest /
;; write-covers-manifest!), and consumed by impact selection.
(define schema-known-tags
  '("suite" "speed" "boundary" "mutates" "isolation" "timeout" "requires" "covers"))
(define schema-reserved-tags '())
(define schema-required-tags '("suite" "speed"))

;; Allowed values per tag (strings exactly as they appear in the header).
(define schema-suite-values
  '("all" "broad"
          "fast"
          "unit"
          "unit-fast"
          "slow"
          "smoke"
          "release-smoke"
          "tui"
          "tui-tmux"
          "security"
          "arch"
          "runtime"
          "extensions"
          "workflows"
          "platform"
          "mutating"
          "skills"
          "ci"
          "testing"
          "integration"
          "tools"
          "provider"
          "session"
          "gsd"
          "verifier"
          "harness"
          "default"))
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
    [(assoc v schema-isolation-deprecated-aliases)
     =>
     cdr]
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
(define header-chunk-tag-pattern (pregexp "^([A-Za-z][A-Za-z0-9_-]*)(?:[ \t]+(.*))?"))

(define (clean-tag-value v)
  (string-trim (regexp-replace* #rx";.*$" (or v "") "")))

(define (extract-header-tags f)
  (define full-path
    (if (absolute-path? f)
        f
        (build-path base-dir f)))
  (cond
    [(not (file-exists? full-path)) '()]
    [else
     (define acc '())
     (with-handlers ([exn:fail? (lambda (_) (void))])
       (call-with-input-file
        full-path
        (lambda (port)
          (for ([_ (in-range 50)]
                #:break (eof-object? (peek-byte port)))
            (define line (read-line port))
            (when (and (string? line) (regexp-match? header-comment-line-pattern line))
              (define chunks (regexp-split #rx"@" line))
              (for ([chunk (in-list (cdr chunks))])
                (define m (regexp-match header-chunk-tag-pattern chunk))
                (when (and m (cadr m))
                  (set! acc
                        (cons (cons (string-downcase (cadr m)) (clean-tag-value (caddr m)))
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
  (define p
    (if (path? f)
        (path->string f)
        f))
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
         (add! 'info
               'reserved-tag
               tag
               "forward-reserved tag: accepted by schema v1, value not yet enforced"))]
      [else
       (add! 'error
             'unknown-tag
             tag
             (format "unknown tag @~a (schema v~a vocabulary: ~a)"
                     tag
                     metadata-schema-version
                     (string-join schema-known-tags " ")))]))
  (define (raw tag)
    (raw-tag-value tags tag))
  ;; 2. Enum validations. These use the RAW header value so malformed
  ;;    spellings that the lenient parser silently drops are still visible
  ;;    to the lint.
  (define suite-raw (raw "suite"))
  (when suite-raw
    (for ([tok (in-list (metadata-tokens suite-raw))])
      (unless (member tok schema-suite-values)
        (add!
         'error
         'invalid-suite
         "suite"
         (format "unknown suite value `~a` (valid: ~a)" tok (string-join schema-suite-values " "))))))
  (define speed-raw (raw "speed"))
  (when speed-raw
    (for ([tok (in-list (metadata-tokens speed-raw))])
      (unless (member tok schema-speed-values)
        (add! 'error 'invalid-speed "speed" (format "invalid speed `~a` (valid: fast slow)" tok)))))
  (define boundary-raw (raw "boundary"))
  (when boundary-raw
    (unless (member boundary-raw schema-boundary-values)
      (add! 'error
            'invalid-boundary
            "boundary"
            (format "invalid boundary `~a` (valid: ~a)"
                    boundary-raw
                    (string-join schema-boundary-values " ")))))
  (define mutates-raw (raw "mutates"))
  (when mutates-raw
    (for ([tok (in-list (metadata-tokens mutates-raw))])
      (unless (member tok schema-mutates-values)
        (add! 'error
              'invalid-mutates
              "mutates"
              (format "invalid mutates token `~a` (valid: ~a)"
                      tok
                      (string-join schema-mutates-values " "))))))
  (define isolation-header-raw (raw "isolation"))
  (cond
    [(not isolation-header-raw) (void)]
    [(deprecated-isolation-alias? isolation-header-raw)
     (add! 'warning
           'deprecated-isolation-alias
           "isolation"
           (format "deprecated alias `~a` normalizes to `~a`"
                   isolation-header-raw
                   (canonical-isolation isolation-header-raw)))]
    [(not (member isolation-header-raw schema-isolation-values))
     (add! 'error
           'invalid-isolation
           "isolation"
           (format "invalid isolation `~a` (canonical vocabulary: ~a)"
                   isolation-header-raw
                   (string-join schema-isolation-values " ")))])
  (define timeout-raw (raw "timeout"))
  (when (and timeout-raw
             (not (or (string=? timeout-raw "")
                      (regexp-match? schema-timeout-value-pattern timeout-raw))))
    (add! 'error
          'malformed-timeout
          "timeout"
          (format "malformed timeout `~a` (expected positive integer seconds)" timeout-raw)))
  (define requires-raw (raw "requires"))
  (when requires-raw
    (for ([tok (in-list (metadata-tokens requires-raw))])
      (unless (member tok schema-requires-values)
        (add! 'error
              'unknown-requires-token
              "requires"
              (format "unknown requirement `~a` (valid: ~a)"
                      tok
                      (string-join schema-requires-values " "))))))
  ;; 3. Required tags.
  (for ([tag (in-list schema-required-tags)])
    (unless (raw tag)
      (add! 'warning
            'missing-required
            tag
            (format "missing required tag @~a (schema v~a)" tag metadata-schema-version))))
  ;; 4. Provenance: heuristic-only files are selected by filename/path.
  (define classification (hash-ref meta 'classification 'heuristic))
  (when (eq? classification 'heuristic)
    (add! 'info
          'heuristic-classification
          "suite"
          "no @suite/@speed: classification relies on filename/path heuristics"))
  (hasheq 'file
          (if (path? f)
              (path->string f)
              f)
          'area
          (lint-area f)
          'classification
          classification
          'findings
          (reverse findings)
          'normalized
          (hasheq 'isolation
                  (canonical-isolation (hash-ref meta 'isolation #f))
                  'isolation-raw
                  (hash-ref meta 'isolation-raw #f)
                  'suite
                  (hash-ref meta 'suite #f)
                  'speed
                  (hash-ref meta 'speed #f))))

;; validate-files : (listof path-string?) -> (listof hash?)
(define (validate-files files)
  (map validate-file files))

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
      (hash-ref! per-area
                 area
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
  (hasheq 'schema_version
          metadata-schema-version
          'file_count
          (length results)
          'invalid_count
          invalid
          'deprecated_alias_count
          deprecated
          'missing_required_count
          missing-required
          'classification
          (hasheq 'explicit explicit 'heuristic heuristic)
          'per_area
          (for/hasheq ([(area a) (in-hash per-area)])
            (values (string->symbol area) (make-immutable-hasheq (hash->list a))))))

;; ---- jsexpr helpers (for --json-out payloads) ----

;; ---- minimal local JSON emitter (no racket/json dependency) ----
;; Supports the jsexpr subset this script produces: immutable hasheq with
;; symbol keys, lists, strings, exact integers, booleans, #f-as-null is NOT
;; used (we emit explicit 'null), and symbols (rendered as strings).

(define (json-escape-string! s out)
  (write-char #\" out)
  (for ([c (in-string s)])
    (case c
      [(#\") (write-string "\\\"" out)]
      [(#\\) (write-string "\\\\" out)]
      [(#\newline) (write-string "\\n" out)]
      [(#\return) (write-string "\\r" out)]
      [(#\tab) (write-string "\\t" out)]
      [(#\backspace) (write-string "\\b" out)]
      [(#\u000c) (write-string "\\f" out)]
      [else
       ;; escape only control chars and non-ASCII; printables go through raw
       (if (or (char<? c #\space) (char>? c #\u007f))
           (fprintf out
                    "\\u~a"
                    (let ([hex (number->string (char->integer c) 16)])
                      (string-append (make-string (max 0 (- 4 (string-length hex))) #\0)
                                     (string-upcase hex))))
           (write-char c out))]))
  (write-char #\" out))

(define (json-write v [out (current-output-port)])
  (cond
    [(hash? v)
     (write-char #\{ out)
     (define pairs (hash->list v))
     (for ([i (in-naturals)]
           [kv (in-list pairs)])
       (unless (zero? i)
         (write-char #\, out))
       (json-escape-string! (if (symbol? (car kv))
                                (symbol->string (car kv))
                                (car kv))
                            out)
       (write-char #\: out)
       (json-write (cdr kv) out))
     (write-char #\} out)]
    [(list? v)
     (write-char #\[ out)
     (for ([i (in-naturals)]
           [e (in-list v)])
       (unless (zero? i)
         (write-char #\, out))
       (json-write e out))
     (write-char #\] out)]
    [(string? v) (json-escape-string! v out)]
    [(symbol? v) (json-escape-string! (symbol->string v) out)]
    [(boolean? v) (write-string (if v "true" "false") out)]
    [(eq? v 'null) (write-string "null" out)]
    [(integer? v) (fprintf out "~a" v)]
    [(real? v) (fprintf out "~a" v)]
    [else (json-escape-string! (format "~a" v) out)]))

(define (json->string v)
  (define o (open-output-string))
  (json-write v o)
  (get-output-string o))

(define (finding->jsexpr f)
  (hasheq 'kind
          (symbol->string (hash-ref f 'kind))
          'code
          (symbol->string (hash-ref f 'code))
          'tag
          (hash-ref f 'tag)
          'message
          (hash-ref f 'message)))

(define (file-result->jsexpr r)
  (hasheq 'file
          (hash-ref r 'file)
          'area
          (hash-ref r 'area)
          'classification
          (symbol->string (hash-ref r 'classification))
          'findings
          (map finding->jsexpr (hash-ref r 'findings))))

(define (findings->jsexpr results)
  (map file-result->jsexpr results))

(define (lint-summary->jsexpr s)
  (hasheq 'schema_version
          (hash-ref s 'schema_version)
          'file_count
          (hash-ref s 'file_count)
          'invalid_count
          (hash-ref s 'invalid_count)
          'deprecated_alias_count
          (hash-ref s 'deprecated_alias_count)
          'missing_required_count
          (hash-ref s 'missing_required_count)
          'classification
          (hash-ref s 'classification)
          'per_area
          (hash-ref s 'per_area)))

;; ---- Report-only CLI lint (W1) ----
;; Prints per-file findings (errors and warnings; info findings stay in the
;; structured result) and an aggregate with per-area counts. ALWAYS exits 0
;; in this wave; enforcement is deferred to W3.

(define (print-lint-report files)
  (define results (validate-files files))
  (printf ";; METADATA LINT — schema v~a — ENFORCED (invalid tags fail; missing tags warn)~n"
          metadata-schema-version)
  (printf ";; ════════════════════════════════════════════════════════════~n")
  (for ([r (in-list results)])
    (define fs
      (filter (lambda (f) (memq (hash-ref f 'kind) '(error warning))) (hash-ref r 'findings '())))
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
  (printf
   ";; aggregate: files=~a invalid=~a deprecated-alias=~a missing-required=~a explicit=~a heuristic-only=~a~n"
   (hash-ref s 'file_count)
   (hash-ref s 'invalid_count)
   (hash-ref s 'deprecated_alias_count)
   (hash-ref s 'missing_required_count)
   (hash-ref (hash-ref s 'classification) 'explicit)
   (hash-ref (hash-ref s 'classification) 'heuristic))
  (define areas
    (sort (for/list ([(k v) (in-hash (hash-ref s 'per_area))])
            k)
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

;; ============================================================
;; Canonical test-file discovery (W2) — the ONE repository-owned
;; inventory used by BOTH metadata consumers (local --lint-metadata
;; and the CI metadata step).
;; ============================================================
;;
;; CONTRACT (any change here is a behavior change for BOTH modes and
;; must be pinned by q/tests/ci/metadata-discovery-test.rkt):
;;
;;   1. INPUT ROOT — explicit repository root (a directory containing
;;      tests/ and scripts/run-tests.rkt). Only <root>/tests is scanned;
;;      nothing outside the discovery root can enter the inventory.
;;
;;   2. PATH NORMALIZATION — every discovered path is reported
;;      repo-relative with forward slashes ("tests/foo/bar-test.rkt"),
;;      so the inventory is independent of the invocation root's
;;      absolute location.
;;
;;   3. IGNORED DIRECTORIES — directory names pruned anywhere under
;;      tests/: "compiled" (bytecode), ".git", "generated"
;;      (documented generated trees).
;;
;;   4. IGNORED PATH PREFIXES — documented non-canonical subtrees:
;;      "tests/metadata-discovery/fixture/" is the frozen W0/W2
;;      discovery-parity fixture tree. Its files are fixture data, not
;;      repository tests, so they are excluded from the canonical
;;      inventory (they are still discovered when the fixture tree is
;;      copied to a temp root and scanned as its own root).
;;
;;   5. SUPPORT MODULES — fixed helper-module name list that is not part
;;      of the test inventory (same list as classify-filters.rkt
;;      `support-test-module?`).
;;
;;   6. SYMLINK POLICY — file symlinks are treated like ordinary files
;;      (they appear at their link path); directory symlinks are NOT
;;      descended into (cycle safety + mode independence, since checkout
;;      copies materialize symlinks as files). Applied identically in
;;      local and CI invocation modes because there is only this one
;;      function.
;;
;;   7. TEST-FILE PREDICATE (which files REQUIRE metadata) — a file
;;      enters the inventory iff it is under <root>/tests, has the .rkt
;;      suffix, is not in an ignored directory/prefix, is not a support
;;      module, and does not carry `@not-test`. Everything else
;;      (including non-*-test.rkt names like beta-plain.rkt) requires
;;      metadata.
;;
;; DETERMINISM: the returned list is sorted lexicographically, so the
;; same tree always yields the same list, digest, and counts regardless
;; of the invoking process's working directory.

;; Walk up from `from` to the nearest repository root. Falls back to the
;; legacy resolve-base-dir candidate list when nothing matches (bounded
;; climb, 12 levels).
(define (resolve-repository-root [from (find-system-path 'orig-dir)])
  (let climb ([dir (simplify-path (path->complete-path from))]
              [fuel 12])
    (cond
      [(<= fuel 0) (resolve-base-dir from)]
      [(q-root-candidate? dir) dir]
      [(and (directory-exists? (build-path dir "q")) (q-root-candidate? (build-path dir "q")))
       (simplify-path (build-path dir "q"))]
      [else (climb (simplify-path (build-path dir 'up)) (sub1 fuel))])))

(define discovery-ignored-directory-names '("compiled" ".git" "generated"))
(define discovery-ignored-path-prefixes '("tests/metadata-discovery/fixture/"))
(define discovery-support-module-names
  '("event-simulator.rkt" "mock-tui-session.rkt" "state-assertions.rkt" "workflow-harness.rkt"))

;; repo-relative, forward-slash path string of p under root
(define (repo-relative-path-string root p)
  (define rel (find-relative-path (simple-form-path root) (simple-form-path p)))
  (apply string-append
         (add-between (map (lambda (seg)
                             (if (symbol? seg)
                                 (symbol->string seg)
                                 (path->string seg)))
                           (explode-path rel))
                      "/")))

(define (discovery-ignored-path? rel)
  (for/or ([prefix (in-list discovery-ignored-path-prefixes)])
    (string-prefix? rel prefix)))

;; discover-metadata-files : [#:root path?] -> (listof string?)
;; The single repository-owned discovery function (see contract above).
(define (discover-metadata-files #:root [root base-dir])
  (define root* (simplify-path (path->complete-path root)))
  (define tests-dir (build-path root* "tests"))
  (define found '())
  (define (walk! dir)
    (for ([entry (in-list (directory-list dir #:build? #t))])
      (define name
        (let ([n (file-name-from-path entry)])
          (and n
               (if (path? n)
                   (path->string n)
                   (symbol->string n)))))
      (cond
        [(directory-exists? entry)
         (when (and (not (link-exists? entry))
                    name
                    (not (member name discovery-ignored-directory-names)))
           (walk! entry))]
        [else
         (when (and (file-exists? entry)
                    name
                    (string-suffix? name ".rkt")
                    (not (member name discovery-support-module-names)))
           (define rel (repo-relative-path-string root* entry))
           (when (and (not (discovery-ignored-path? rel))
                      (not (hash-ref (get-file-metadata entry) 'not-test? #f)))
             (set! found (cons rel found))))])))
  (when (directory-exists? tests-dir)
    (walk! tests-dir))
  (sort found string<?))

;; ============================================================
;; Pure-Racket SHA-256 (FIPS 180-4)
;; ============================================================
;; The openssl collection is not part of this Racket installation, so the
;; digest is implemented here. Correctness is pinned by known-answer tests
;; ("abc" and the empty string) in q/tests/ci/metadata-discovery-test.rkt.

(define sha256-k
  (list->vector (list #x428a2f98
                      #x71374491
                      #xb5c0fbcf
                      #xe9b5dba5
                      #x3956c25b
                      #x59f111f1
                      #x923f82a4
                      #xab1c5ed5
                      #xd807aa98
                      #x12835b01
                      #x243185be
                      #x550c7dc3
                      #x72be5d74
                      #x80deb1fe
                      #x9bdc06a7
                      #xc19bf174
                      #xe49b69c1
                      #xefbe4786
                      #x0fc19dc6
                      #x240ca1cc
                      #x2de92c6f
                      #x4a7484aa
                      #x5cb0a9dc
                      #x76f988da
                      #x983e5152
                      #xa831c66d
                      #xb00327c8
                      #xbf597fc7
                      #xc6e00bf3
                      #xd5a79147
                      #x06ca6351
                      #x14292967
                      #x27b70a85
                      #x2e1b2138
                      #x4d2c6dfc
                      #x53380d13
                      #x650a7354
                      #x766a0abb
                      #x81c2c92e
                      #x92722c85
                      #xa2bfe8a1
                      #xa81a664b
                      #xc24b8b70
                      #xc76c51a3
                      #xd192e819
                      #xd6990624
                      #xf40e3585
                      #x106aa070
                      #x19a4c116
                      #x1e376c08
                      #x2748774c
                      #x34b0bcb5
                      #x391c0cb3
                      #x4ed8aa4a
                      #x5b9cca4f
                      #x682e6ff3
                      #x748f82ee
                      #x78a5636f
                      #x84c87814
                      #x8cc70208
                      #x90befffa
                      #xa4506ceb
                      #xbef9a3f7
                      #xc67178f2)))

(define (sha-rotr x n)
  (bitwise-and #xffffffff (bitwise-ior (arithmetic-shift x (- n)) (arithmetic-shift x (- 32 n)))))

(define (sha-hex8 x)
  (define s (number->string x 16))
  (string-append (make-string (max 0 (- 8 (string-length s))) #\0) s))

;; sha256-hex : string? -> string?
;; Lowercase hex SHA-256 of the UTF-8 encoding of s.
(define (sha256-hex s)
  (define msg (string->bytes/utf-8 s))
  (define len (bytes-length msg))
  (define padded
    (let* ([k0 (- 56 (modulo (+ len 1) 64))]
           [k (if (< k0 0)
                  (+ k0 64)
                  k0)]
           [out (make-bytes (+ len 1 k 8))])
      (bytes-copy! out 0 msg)
      (bytes-set! out len #x80)
      (define bitlen (* len 8))
      (for ([i (in-range 8)])
        (bytes-set! out (+ len 1 k i) (bitwise-and (arithmetic-shift bitlen (* -8 (- 7 i))) #xff)))
      out))
  (define h0 #x6a09e667)
  (define h1 #xbb67ae85)
  (define h2 #x3c6ef372)
  (define h3 #xa54ff53a)
  (define h4 #x510e527f)
  (define h5 #x9b05688c)
  (define h6 #x1f83d9ab)
  (define h7 #x5be0cd19)
  (define-values (final0 final1 final2 final3 final4 final5 final6 final7)
    (for/fold ([a0 h0]
               [a1 h1]
               [a2 h2]
               [a3 h3]
               [a4 h4]
               [a5 h5]
               [a6 h6]
               [a7 h7])
              ([off (in-range 0 (bytes-length padded) 64)])
      (define w (make-vector 64))
      (for ([i (in-range 16)])
        (vector-set! w
                     i
                     (bitwise-ior (arithmetic-shift (bytes-ref padded (+ off (* 4 i))) 24)
                                  (arithmetic-shift (bytes-ref padded (+ off (* 4 i) 1)) 16)
                                  (arithmetic-shift (bytes-ref padded (+ off (* 4 i) 2)) 8)
                                  (bytes-ref padded (+ off (* 4 i) 3)))))
      (for ([i (in-range 16 64)])
        (define x15 (vector-ref w (- i 15)))
        (define x2 (vector-ref w (- i 2)))
        (define s0 (bitwise-xor (sha-rotr x15 7) (sha-rotr x15 18) (arithmetic-shift x15 -3)))
        (define s1 (bitwise-xor (sha-rotr x2 17) (sha-rotr x2 19) (arithmetic-shift x2 -10)))
        (vector-set! w
                     i
                     (bitwise-and #xffffffff
                                  (+ (vector-ref w (- i 16)) s0 (vector-ref w (- i 7)) s1))))
      (let loop ([i 0]
                 [ha a0]
                 [hb a1]
                 [hc a2]
                 [hd a3]
                 [he a4]
                 [hf a5]
                 [hg a6]
                 [hh a7])
        (if (= i 64)
            (values (bitwise-and #xffffffff (+ a0 ha))
                    (bitwise-and #xffffffff (+ a1 hb))
                    (bitwise-and #xffffffff (+ a2 hc))
                    (bitwise-and #xffffffff (+ a3 hd))
                    (bitwise-and #xffffffff (+ a4 he))
                    (bitwise-and #xffffffff (+ a5 hf))
                    (bitwise-and #xffffffff (+ a6 hg))
                    (bitwise-and #xffffffff (+ a7 hh)))
            (let* ([S1 (bitwise-xor (sha-rotr he 6) (sha-rotr he 11) (sha-rotr he 25))]
                   [ch (bitwise-and #xffffffff
                                    (bitwise-xor (bitwise-and he hf)
                                                 (bitwise-and (bitwise-not he) hg)))]
                   [temp1 (+ hh S1 ch (vector-ref sha256-k i) (vector-ref w i))]
                   [S0 (bitwise-xor (sha-rotr ha 2) (sha-rotr ha 13) (sha-rotr ha 22))]
                   [maj (bitwise-and
                         #xffffffff
                         (bitwise-xor (bitwise-and ha hb) (bitwise-and ha hc) (bitwise-and hb hc)))]
                   [temp2 (+ S0 maj)])
              (loop (add1 i)
                    (bitwise-and #xffffffff (+ temp1 temp2))
                    ha
                    hb
                    hc
                    (bitwise-and #xffffffff (+ hd temp1))
                    he
                    hf
                    hg))))))
  (apply string-append (map sha-hex8 (list final0 final1 final2 final3 final4 final5 final6 final7))))

;; ============================================================
;; Deterministic metadata inventory (W2)
;; ============================================================
;; `--metadata-inventory-json` payload: schema version, invocation root,
;; normalized file-list digest (SHA-256 over the sorted normalized
;; repo-relative paths, newline-joined), file count, per-area counts, and
;; the full invalid / deprecated-alias / missing-required details. The
;; same tree always yields the same digest — independent of the absolute
;; location of the checkout, which is exactly the local-vs-CI parity
;; contract pinned by q/tests/ci/metadata-discovery-test.rkt.

(define (results-with-finding-code results code)
  (for/list ([r (in-list results)]
             #:when (for/or ([fnd (in-list (hash-ref r 'findings '()))])
                      (eq? (hash-ref fnd 'code) code)))
    (file-result->jsexpr r)))

;; build-metadata-inventory : [#:root path?] -> jsexpr?
(define (build-metadata-inventory #:root [root base-dir])
  (define root* (simplify-path (path->complete-path root)))
  (define files (discover-metadata-files #:root root*))
  (define results-abs (validate-files (map (lambda (rel) (build-path root* rel)) files)))
  ;; Re-key every result with its normalized repo-relative path so the
  ;; payload (and per-area bucketing) is invocation-root independent.
  (define results (map (lambda (r rel) (hash-set r 'file rel)) results-abs files))
  (define summary (summarize-findings results))
  (hasheq 'tool
          "classify-metadata"
          'schema_version
          metadata-schema-version
          'inventory_schema_version
          1
          'invocation_root
          (path->string root*)
          'file_count
          (length files)
          'file_list_digest
          (sha256-hex (string-join files "\n"))
          'files
          files
          'counts
          (lint-summary->jsexpr summary)
          'violations
          (hasheq 'invalid
                  (results-with-finding-code results 'invalid-speed)
                  'deprecated_alias
                  (results-with-finding-code results 'deprecated-isolation-alias)
                  'missing_required
                  (results-with-finding-code results 'missing-required))))

;; emit-metadata-inventory-json : [#:root path?] -> void?
;; Writes the deterministic inventory JSON to the current output port.
;; Single repository entry point:
;; scripts/run-tests/classify-metadata.rkt --metadata-inventory-json.
(define (emit-metadata-inventory-json #:root [root base-dir])
  (json-write (build-metadata-inventory #:root root) (current-output-port))
  (newline (current-output-port)))

;; ---- Direct CLI entry (repository command, same code path as the
;; run-tests.rkt facade): both consumers invoke this one function. ----
(module+ main
  (define raw-args (vector->list (current-command-line-arguments)))
  ;; Optional explicit root override: --root <dir>. Default: repository
  ;; root resolved by climbing from the process directory.
  (define root-override
    (let loop ([rest raw-args])
      (cond
        [(and (pair? rest) (pair? (cdr rest)) (string=? (car rest) "--root")) (cadr rest)]
        [(pair? rest) (loop (cdr rest))]
        [else #f])))
  (define (flag? name)
    (and (member name raw-args) #t))
  (define inventory? (flag? "--metadata-inventory-json"))
  (define lint? (flag? "--lint-metadata"))
  (define root
    (if root-override
        (path->complete-path root-override)
        (resolve-repository-root)))
  (cond
    [inventory? (emit-metadata-inventory-json #:root root)]
    [lint? (print-lint-report (discover-metadata-files #:root root))]
    [else
     (eprintf
      "usage: racket scripts/run-tests/classify-metadata.rkt [--metadata-inventory-json | --lint-metadata] [--root <dir>]\n")
     (exit 2)]))
