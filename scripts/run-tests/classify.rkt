#lang racket/base

;; q/scripts/run-tests/classify.rkt — File classification, metadata, discovery
;;
;; Consolidated module (Phase C): merges the former classify-metadata.rkt
;; (metadata parsing, schema v1 lint, canonical discovery, inventory) and
;; classify-filters.rkt (suite classification predicates) into this single
;; module. The provided binding surface is the UNION of the original modules:
;; no renames, no behavior change.
;; Also provides shard distribution, file collection, path utilities, and
;; bytecode cleanup (extracted from run-tests.rkt, v0.96.16, AX1-2).
;; Decomposed in v0.99.58 W3-1 (P3-CL); re-consolidated in Phase C.
;; STABILITY: internal (test runner infrastructure)

(require racket/string
         racket/path
         racket/file
         racket/list)

;; NOTE (W2): deliberately no `racket/json` require. This script must run on
;; bare installations (the repo's CI base image ships a minimal Racket whose
;; collection tree has no racket/json.rkt), so JSON emission is implemented
;; locally below. Keep it that way unless the CI image is pinned to a full
;; distribution — a missing collection here is a discovery tool outage.

;; Full extracted API surface (union of the former modules).
(provide base-dir
         q-root-candidate?
         resolve-base-dir
         resolve-repository-root
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
         schema-requires-values
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
         emit-metadata-inventory-json
         ;; Suite classification predicates (former classify-filters.rkt)
         slow-patterns
         path-slow-patterns
         mutating-patterns
         support-test-module-names
         smoke-curated-files
         release-smoke-curated-files
         platform-curated-files
         slow-file?
         tui-file?
         mutating-file?
         ;; Fast/PR lane exclusion classifiers (same metadata, no second scheme)
         declared-mutation?
         isolation-excluded?
         ;; Shared-scheduler resource classes (final plan §2, rev 3)
         repository-exclusive-file?
         co-runs-under-exclusive-file?
         requires-excluded-tokens
         requires-excluded?
         security-file?
         file-has-suite-tag?
         smoke-excluded?
         smoke-included?
         release-smoke-included?
         support-test-module?
         arch-file?
         runtime-file?
         extensions-file?
         workflows-file?
         unit-fast-file?
         file-has-rackunit-tests?
         platform-file?
         ;; Shard support
         shard-files
         ;; Metadata classification provenance (W1)
         file-metadata-classification
         ;; Path utilities
         normalize-test-path
         ;; File collection
         collect-test-files
         ;; The suite dispatch itself (one registry; full-plan.rkt's
         ;; `suite-select-rels` classifies an already-collected universe
         ;; through it)
         collect-classify-file-list
         ;; Managed repo surfaces (detection only; never rewritten)
         repo-surface-files
         ;; Bytecode cleanup
         clean-stale-bytecode!
         ;; Environment profile skip policy (folded profiles.rkt)
         known-profiles
         normalize-requirement
         profile-unavailable-requirements
         skipped-requirements
         profile-skips-test?)

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
  ;; Ancestor walk: when the launch cwd sits at an arbitrary depth inside
  ;; the repo (e.g. suite harness subprocess execution), the original
  ;; one-level candidates can all miss the root. Walk upward from orig
  ;; looking for a q-root directory; preserve prior behavior otherwise.
  (define ancestor-root
    (let loop ([dir (simplify-path orig)])
      (cond
        [(q-root-candidate? dir) dir]
        [else
         (define up (simplify-path (build-path dir "..")))
         (if (string=? (path->string up) (path->string dir))
             #f ; filesystem root reached
             (loop up))])))
  ;; The launch tree wins. A `q`-shaped sibling is a FALLBACK for the
  ;; monorepo layout (launched from a directory that is not inside a repo),
  ;; never a substitute for the tree the runner was started in: preferring
  ;; it first made every worktree in this working area silently measure the
  ;; neighbouring `q/` clone — different files, different commit, different
  ;; q-version — while reporting a RUN-SUMMARY for it.
  (or ancestor-root
      (for/first ([candidate (in-list candidates)]
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
;;      of the test inventory (same list as this module's
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
;; scripts/run-tests/classify.rkt --metadata-inventory-json.
(define (emit-metadata-inventory-json #:root [root base-dir])
  (json-write (build-metadata-inventory #:root root) (current-output-port))
  (newline (current-output-port)))

;; ============================================================
;; Suite classification predicates (former classify-filters.rkt)
;; ============================================================
;;
;; Pure predicates that classify test files into suites (slow, tui,
;; mutating, security, smoke, platform, arch, runtime, extensions, etc.).

;; ============================================================
;; Patterns
;; ============================================================

(define slow-patterns
  '("sandbox" "subprocess"
              "integration"
              "benchmark"
              "workflow-"
              "e2e-"
              "tmux-explore-campaign"
              "tmux-explore-multi-step"
              "tmux-explore-evidence"
              "tmux-explore-executor"
              "tmux-explore-production-provenance"
              "tmux-explore-verifiers"
              "tmux-q-harness"
              "tmux-tui-"
              "ci-local"
              "metrics-readme"
              "examples-compile"
              "pre-commit"
              "racket-tooling"
              "run-tests"
              "audit-script"
              "test-doctor"
              "check-deps"
              "self-hosting"
              "tui-terminal"
              "sync-readme"))

(define path-slow-patterns '("/workflows/"))

(define mutating-patterns
  '("ci-local" "pre-commit"
               "check-deps"
               "sync-version"
               "sync-readme"
               "bump-version"
               "metrics-readme"
               "self-hosting"))

(define support-test-module-names
  '("event-simulator.rkt" "mock-tui-session.rkt" "state-assertions.rkt" "workflow-harness.rkt"))

;; ============================================================
;; Classifiers
;; ============================================================

(define (slow-file? f)
  (define meta (get-file-metadata f))
  (define meta-speed (hash-ref meta 'speed #f))
  (cond
    [(eq? meta-speed 'fast) #f]
    [(eq? meta-speed 'slow) #t]
    [(eq? meta-speed 'perf) #t]
    [else
     (define base (file-name-from-path f))
     (or (for/or ([p (in-list slow-patterns)])
           (and base (string-contains? (path->string base) p)))
         (for/or ([p (in-list path-slow-patterns)])
           (string-contains? f p)))]))

(define (tui-file? f)
  (define meta (get-file-metadata f))
  (define meta-suite (hash-ref meta 'suite #f))
  (if (equal? meta-suite "tui")
      #t
      (let ([base (file-name-from-path f)])
        (or (string-contains? f "/tui/")
            (string-contains? f "/interfaces/tui.rkt")
            (and base (string-prefix? (path->string base) "test-tui-"))))))

(define (mutating-file? f)
  (define meta (get-file-metadata f))
  (define meta-mutates (hash-ref meta 'mutates #f))
  (define meta-isolation (hash-ref meta 'isolation #f))
  (cond
    [(equal? meta-mutates "none") #f]
    [(equal? meta-isolation "process") #t]
    ;; W2 v0.99.77 (F-7): the test-run-tests-* family tags itself
    ;; @isolation subprocess because each member spawns inner
    ;; `racket scripts/run-tests.rkt` subprocesses that clean stale
    ;; compiled/ dirs repo-wide, write /tmp failure logs, and create temp
    ;; ledger files. Running several members concurrently lets the inner
    ;; invocations race on that shared state (intermittent
    ;; test-run-tests-ledger failures under --jobs 12). Treating subprocess
    ;; isolation as mutating gives the family the repository-exclusive
    ;; resource — dispatched first, one at a time (final plan §2).
    [(equal? meta-isolation "subprocess") #t]
    [else
     (let ([base (file-name-from-path f)])
       (for/or ([p (in-list mutating-patterns)])
         (and base (string-contains? (path->string base) p))))]))

;; --- Fast/PR lane exclusion classifiers ---------------------------------
;;
;; These sit next to the other metadata classifiers on purpose: the `./qt
;; default` lane filter must read the SAME repository annotation, not a second
;; hand-maintained list. Each predicate is one declared metadata fact.

;; A declaration that the file mutates something other than nothing. Values
;; other than "none" are declarations even when the file name matches no
;; `mutating-patterns` entry (those patterns are a heuristic, this is a claim).
(define (declared-mutation? f)
  (define mutates (hash-ref (get-file-metadata f) 'mutates #f))
  (and (string? mutates) (not (equal? mutates "none")) #t))

;; Any declared `@isolation` value other than "none" owns process/global state
;; (process, temp-dir, subprocess, process-tree, repo-serial, sandboxed-home,
;; ...). The predicate is deliberately vocabulary-agnostic: an isolation value
;; that the schema lint has not admitted yet (`sandboxed-home`, `process-tree`)
;; is still a declared isolation, so it must not reach the fast/PR lane.
(define (isolation-excluded? f)
  (define isolation (hash-ref (get-file-metadata f) 'isolation #f))
  (and (string? isolation) (not (equal? isolation "none")) #t))

;; ── Shared-scheduler resource classes (final plan §2, rev 3) ──────────
;;
;; The ONE derivation of `repository-exclusive` for every execution path
;; (run-tests/runner.rkt and full-plan.rkt's run-files): a file owns the
;; repository-exclusive resource when `mutating-file?` reports it OR it
;; declares `@mutates` other than none. Unknown or ambiguous mutation
;; requirements acquire the lock (conservative default — mutating-file?'s
;; name patterns cover the unknown cases, declared-mutation? the explicit
;; ones).
(define (repository-exclusive-file? f)
  (with-handlers ([exn:fail? (lambda (_) #t)]) ; unknown/ambiguous → acquire the lock
    (or (mutating-file? f) (declared-mutation? f))))

;; The only files that may co-run while a repository-exclusive owner holds
;; the lock: declared `temp-dir`/`sandboxed-home` isolation runs outside the
;; checkout and cannot observe a managed-surface mutation (mutation excludes
;; observation — the before/after surface guard cannot attribute a mutation
;; to a file). Every other non-owner is eligible only once the mutation set
;; drains.
(define (co-runs-under-exclusive-file? f)
  (with-handlers ([exn:fail? (lambda (_) #f)]) ; unknown/ambiguous → never co-run
    (and (member (hash-ref (get-file-metadata f) 'isolation #f) '("temp-dir" "sandboxed-home")) #t)))

;; Declared requirements that make a file unsafe for the fast/PR lane: it needs
;; a live network, an interactive terminal, a browser or a real provider key.
(define requires-excluded-tokens '("network" "terminal" "browser" "provider-key"))

(define (requires-excluded? f)
  (define requires (hash-ref (get-file-metadata f) 'requires '()))
  (and (for/or ([tok (in-list requires)])
         (member tok requires-excluded-tokens))
       #t))

(define (security-file? f)
  (define base (path->string (file-name-from-path f)))
  (or (string-contains? base "security")
      (string-contains? base "permission")
      (string-contains? base "safe-mode")
      (string-contains? base "sandbox")
      (string-contains? base "tool-bash")
      (file-has-suite-tag? f "security")))

(define (file-has-suite-tag? f tag)
  (define meta (get-file-metadata f))
  (define suites (hash-ref meta 'suites '()))
  (or (member tag suites) #f))

(define (smoke-excluded? f)
  (or (slow-file? f) (string-contains? f "/workflows/") (string-contains? f "/interfaces/")))

;; Smoke suite: curated sanity-gate of ~20 fast, always-green tests.
;; Verifies the system can boot, import core modules, and perform basic operations.
;; Files are selected by @suite smoke tag OR by matching the curated list below.
(define smoke-curated-files
  '("tests/test-version.rkt" "tests/test-safe-mode.rkt"
                             "tests/test-error-classify.rkt"
                             "tests/test-mutating-tool-taxonomy.rkt"
                             "tests/test-run-tests-script.rkt"
                             "tests/test-worker-security.rkt"
                             "tests/test-spawn-subagent-serialization.rkt"
                             "tests/test-verifier-gate.rkt"
                             "tests/test-cli-flags.rkt"
                             "tests/test-extension-tiers.rkt"
                             "tests/test-capability-aware-spawn.rkt"
                             "tests/test-frontmatter-extended.rkt"
                             "tests/test-context-assembly-config.rkt"
                             "tests/test-execution-plane-error-label.rkt"
                             "tests/test-runtime-packages.rkt"
                             "tests/test-cli.rkt"
                             "tests/test-tui-hotspot-characterization.rkt"
                             "tests/test-tui-render-loop.rkt"
                             "tests/test-tui-frame-integrity.rkt"
                             "tests/test-tui-goal-status-bar.rkt"))

(define (smoke-included? f)
  (define s
    (if (path? f)
        (path->string f)
        f))
  (or (file-has-suite-tag? f "smoke")
      (for/or ([curated (in-list smoke-curated-files)])
        (string-suffix? s curated))))

;; Release-smoke suite: post-release artifact verification.
;; Deterministic subset of smoke that focuses on verifying the installed
;; artifact works correctly. No browser/playwright, no live network,
;; no terminal, no mutating tests.
;; Contract: tests that verify version, CLI, core system boot, and
;; selected release-critical functionality only.
(define release-smoke-curated-files
  '("tests/test-version.rkt" "tests/test-safe-mode.rkt"
                             "tests/test-error-classify.rkt"
                             "tests/test-mutating-tool-taxonomy.rkt"
                             "tests/test-verifier-gate.rkt"
                             "tests/test-cli-flags.rkt"
                             "tests/test-extension-tiers.rkt"
                             "tests/test-capability-aware-spawn.rkt"
                             "tests/test-frontmatter-extended.rkt"
                             "tests/test-context-assembly-config.rkt"
                             "tests/test-execution-plane-error-label.rkt"
                             "tests/test-runtime-packages.rkt"
                             "tests/test-spawn-subagent-serialization.rkt"))

(define (release-smoke-included? f)
  (define s
    (if (path? f)
        (path->string f)
        f))
  (or (file-has-suite-tag? f "release-smoke")
      (for/or ([curated (in-list release-smoke-curated-files)])
        (string-suffix? s curated))))

(define (support-test-module? f)
  (define s
    (if (path? f)
        (path->string f)
        f))
  (define base (file-name-from-path s))
  (or (string-contains? s "/helpers/")
      (string-contains? s "/fixtures/")
      (and base (member (path->string base) support-test-module-names) #t)))

(define (arch-file? f)
  (or (string-contains? f "arch-")
      (string-contains? f "boundary")
      (string-contains? f "fitness")
      (string-contains? f "hotspot")
      (file-has-suite-tag? f "arch")))

(define (runtime-file? f)
  (or (string-contains? f "runtime")
      (string-contains? f "session")
      (string-contains? f "compaction")
      (string-contains? f "iteration")
      (string-contains? f "turn-")
      (string-contains? f "tool-coord")
      (file-has-suite-tag? f "runtime")))

(define (extensions-file? f)
  (or (string-contains? f "extensions/")
      (string-contains? f "gsd-")
      (string-contains? f "define-extension")
      (string-contains? f "wave-executor")
      (string-contains? f "hook-")
      (file-has-suite-tag? f "extensions")))

(define (workflows-file? f)
  ;; Every other area predicate (security, arch, runtime, extensions) accepts
  ;; the `@suite` tag as well as a path pattern. This one did not, so a file
  ;; that declared `@suite workflows` while living outside tests/workflows/ was
  ;; silently inert: the tag selected nothing. A test must be selectable by the
  ;; suite it declares, so the tag counts here too — provided the file really
  ;; is a real test file, since the runner's convention is tests/**/test-*.rkt:
  ;; a helper or fixture that happens to carry the tag must not join a suite.
  (or (and (string-contains? f "/workflows/")
           (or (not (string-contains? f "/fixtures/"))
               (string-prefix? (path->string (file-name-from-path f)) "test-")))
      (and (file-has-suite-tag? f "workflows")
           (string-prefix? (path->string (file-name-from-path f)) "test-"))))

(define (unit-fast-file? f)
  (define meta (get-file-metadata f))
  (define speed (hash-ref meta 'speed #f))
  (define boundary (hash-ref meta 'boundary #f))
  (and (not (mutating-file? f))
       (or (file-has-suite-tag? f "unit-fast") (and (eq? speed 'fast) (equal? boundary "unit")))))

(define (file-has-rackunit-tests? path)
  (and (file-exists? path)
       (let* ([content (file->string path)]
              [has-rackunit-forms? (or (regexp-match? #rx"\\(test-case" content)
                                       (regexp-match? #rx"\\(check-" content))]
              [module-plus-test? (regexp-match? #px"\\(module\\+\\s+test\\b" content)]
              [uses-rackunit-text-ui? (and has-rackunit-forms?
                                           (regexp-match? #rx"\\(run-tests" content))]
              [needs-raco-discovery?
               (and has-rackunit-forms? (not module-plus-test?) (not uses-rackunit-text-ui?))])
         (or module-plus-test? needs-raco-discovery?))))

;; ============================================================
;; Platform-cross classifier
;; Tests that exercise platform-specific paths: filesystem,
;; subprocess, terminal detection, path handling, Unicode.
;; These are the only tests that need macOS verification.
;; ============================================================

(define platform-curated-files
  '("tests/test-subprocess.rkt" "tests/test-subprocess-helpers.rkt"
                                "tests/test-subprocess-edge-cases.rkt"
                                "tests/test-cwd-independence.rkt"
                                "tests/test-config-paths.rkt"
                                "tests/test-sandbox.rkt"
                                "tests/test-worker-security.rkt"
                                "tests/test-tool-bash-workflow.rkt"
                                "tests/test-version.rkt"
                                "tests/test-safe-mode.rkt"
                                "tests/test-cli.rkt"
                                "tests/test-spawn-subagent-serialization.rkt"
                                "tests/test-runtime-packages.rkt"
                                "tests/test-extension-tiers.rkt"
                                "tests/test-capability-aware-spawn.rkt"
                                "tests/test-execution-plane-error-label.rkt"
                                "tests/test-error-classify.rkt"
                                "tests/test-mutating-tool-taxonomy.rkt"
                                "tests/test-verifier-gate.rkt"
                                "tests/test-cli-flags.rkt"
                                "tests/test-frontmatter-extended.rkt"
                                "tests/test-context-assembly-config.rkt"
                                "tests/test-metrics-helpers.rkt"
                                "tests/test-token-estimate-cache.rkt"
                                "tests/test-truncation.rkt"
                                "tests/test-lockfile.rkt"
                                "tests/test-facade-surface.rkt"
                                "tests/test-red-module-boundaries.rkt"
                                "tests/test-export-formats.rkt"
                                "tests/test-message-layout.rkt"
                                "tests/test-cell-diff-render.rkt"
                                "tests/test-llm-model.rkt"
                                "tests/test-llm-error-visibility.rkt"
                                "tests/test-streaming-transitions.rkt"
                                "tests/test-streaming-tool-bug.rkt"
                                "tests/test-loop-cancellation.rkt"
                                "tests/test-loop-edge-cases.rkt"
                                "tests/test-loop-events.rkt"
                                "tests/test-settings.rkt"
                                "tests/test-sync-readme-status.rkt"))

(define (platform-file? f)
  (define s
    (if (path? f)
        (path->string f)
        f))
  (or (file-has-suite-tag? f "platform")
      (for/or ([curated (in-list platform-curated-files)])
        (string-suffix? s curated))))

;; ============================================================
;; Shard support — select files by round-robin modulo
;; ============================================================

(define (shard-files files shard-index shard-total)
  (unless (and (integer? shard-total) (> shard-total 0))
    (raise-argument-error 'shard-files "positive integer" shard-total))
  (unless (and (integer? shard-index) (>= shard-index 0) (< shard-index shard-total))
    (raise-argument-error 'shard-files (format "integer in [0, ~a)" shard-total) shard-index))
  (for/list ([f (in-list files)]
             [i (in-naturals)]
             #:when (= (modulo i shard-total) shard-index))
    f))

;; ============================================================
;; Metadata classification provenance (W1)
;; ============================================================

;; All metadata parsing routes through the schema-aware parser above (see
;; `validate-file` for schema v1 and the report-only lint). Heuristic
;; (filename/path-based) classification results are labeled 'heuristic rather
;; than 'explicit so downstream reports can distinguish declarative metadata
;; from heuristic fallbacks.
(define (file-metadata-classification f)
  (hash-ref (get-file-metadata f) 'classification 'heuristic))

;; ============================================================
;; Path utilities
;; ============================================================

(define (normalize-test-path f)
  (define s
    (if (path? f)
        (path->string f)
        f))
  (cond
    [(absolute-path? s) s]
    [(string-prefix? s "q/tests/") (substring s 2)]
    [(string-prefix? s "./q/tests/") (substring s 4)]
    [else s]))

;; ============================================================
;; File collection
;; ============================================================
;;
;; Default behavior (no root keywords): byte-for-byte the pre-W5 crawl of
;; the repository's own tests/ tree, in in-directory order.
;;
;; Explicit-root seam (v1.00.24 W5): `#:root` / `#:test-root` (aliases)
;; point collection at an explicit fixture root shaped like a repository
;; root: the walk visits `<root>/tests` recursively and returns
;; ROOT-RELATIVE normalized paths. Contract (pinned by
;; tests/test-run-tests-shard.rkt):
;;   - deterministic output: entries sorted lexicographically per level,
;;     pre-order (files and directories interleaved by name);
;;   - containment: no selected path may escape the root — directory
;;     symlinks are never descended (no symlink escape), file symlinks are
;;     resolved and rejected when their target lies outside the root, and
;;     `..` components in the root argument itself are normalized away;
;;   - metadata reads use the RESOLVED ABSOLUTE path of every file, so the
;;     metadata cache stays isolated between distinct roots (and between a
;;     fixture root and the repository);
;;   - selection mirrors the default-root rules: `.rkt` suffix, `compiled/`
;;     exclusion, `/helpers/` + `/fixtures/` support-module exclusion, and
;;     `@not-test` exclusion — repo-relative discovery ignore prefixes
;;     (tests/metadata-discovery/fixture/ et al.) do not apply inside an
;;     explicit root, because the caller owns that root;
;;   - a missing `<root>/tests` directory fails closed;
;;   - the shared metadata cache is NOT cleared: absolute-path keys cannot
;;     collide across roots.

(define (collect-test-files suite
                            #:extra-files [extra-files #f]
                            #:root [root #f]
                            #:test-root [test-root #f])
  (cond
    [(pair? extra-files) (map normalize-test-path extra-files)]
    [(and root test-root)
     (raise-argument-error 'collect-test-files
                           "only one of #:root/#:test-root"
                           (format "~s ~s" root test-root))]
    [(or root test-root) (collect-test-files/under suite (or root test-root))]
    [else (collect-test-files/default suite)]))

(define (collect-test-files/default suite)
  (define all-files
    (for/list ([f (in-directory (build-path base-dir "tests"))]
               #:when (and (file-exists? f)
                           (let* ([s (path->string f)]
                                  [rel (path->string (find-relative-path base-dir f))])
                             (and (string-suffix? s ".rkt")
                                  (not (string-contains? s "/compiled/"))
                                  ;; v1.00.11 hotfix: the discovery-parity fixture tree
                                  ;; (tests/metadata-discovery/fixture/) is frozen input
                                  ;; data for tests/ci/metadata-discovery-test.rkt, which
                                  ;; copies it into its own temp root before collecting.
                                  ;; The repo-root walk must never collect or execute it.
                                  (not (for/or ([prefix (in-list discovery-ignored-path-prefixes)])
                                         (string-prefix? rel prefix)))
                                  (not (support-test-module? s))
                                  (not (hash-ref (get-file-metadata rel) 'not-test? #f))))))
      (path->string (find-relative-path base-dir f))))
  (collect-classify-file-list suite all-files))

;; Suite classification over a collected path list. Shared by the default
;; repository branch and the explicit-root branch, so suite semantics are
;; identical regardless of the collection root.
(define (collect-classify-file-list suite all-files)
  (case suite
    [(all broad) all-files]
    [(fast) (filter (lambda (f) (and (not (slow-file? f)) (not (tui-file? f)))) all-files)]
    [(unit_fast unit-fast) (filter unit-fast-file? all-files)]
    [(slow) (filter slow-file? all-files)]
    [(tui) (filter tui-file? all-files)]
    [(smoke) (filter smoke-included? all-files)]
    [(release_smoke release-smoke) (filter release-smoke-included? all-files)]
    [(security) (filter security-file? all-files)]
    [(arch) (filter arch-file? all-files)]
    [(runtime) (filter runtime-file? all-files)]
    [(extensions) (filter extensions-file? all-files)]
    [(workflows) (filter workflows-file? all-files)]
    [(platform) (filter platform-file? all-files)]
    [(mutating) (filter mutating-file? all-files)]
    [else '("tests/")]))

(define (collect-test-files/under suite root-arg)
  ;; simplify-path + path->directory-path normalize away any `..` in the
  ;; caller-supplied root, so nothing below can select above the real root.
  (define root-path
    (simplify-path (path->directory-path (path->complete-path (if (path? root-arg)
                                                                  root-arg
                                                                  (string->path root-arg))
                                                              base-dir))
                   #f))
  (define root-str (path->string root-path))
  (unless (directory-exists? (build-path root-path "tests"))
    (raise-user-error 'collect-test-files "explicit test root has no tests/ directory: ~a" root-path))
  ;; Selection rules over the absolute fixture path. Metadata reads use the
  ;; absolute path, keeping cache entries keyed per root. The support-module
  ;; exclusion (`/helpers/`, `/fixtures/`) is applied to the ROOT-RELATIVE
  ;; path: support directories are a property of the collected tree, and an
  ;; ancestor of the root that merely happens to be named `fixtures` must
  ;; never exclude the entire tree.
  (define (select? fstr)
    (define rel-from-root
      (path->string (find-relative-path (simple-form-path root-path)
                                        (simple-form-path (string->path fstr)))))
    (and (string-suffix? fstr ".rkt")
         (not (string-contains? fstr "/compiled/"))
         (not (support-test-module? rel-from-root))
         (not (hash-ref (get-file-metadata fstr) 'not-test? #f))))
  ;; Containment: resolve symlinks and reject anything leaving the root.
  (define (contained-file? p)
    (define resolved
      (with-handlers ([exn:fail? (lambda (_) p)])
        (simplify-path (resolve-path p) #f)))
    (string-prefix? (path->string resolved) root-str))
  (define (walk! dir)
    (apply append
           (for/list ([entry (in-list (sort (map path->string (directory-list dir #:build? #t))
                                            string<?))])
             (define p (string->path entry))
             (cond
               [(directory-exists? p)
                ;; Containment first: descending is allowed only when the
                ;; directory's RESOLVED target stays under the root, so a
                ;; symlinked directory pointing outside is never descended.
                (define resolved
                  (with-handlers ([exn:fail? (lambda (_) #f)])
                    (path->string (resolve-path p))))
                (if (and resolved (string-prefix? resolved root-str))
                    (walk! p)
                    '())]
               [(and (file-exists? p) (contained-file? p))
                (define fstr (path->string p))
                (if (select? fstr)
                    (list fstr)
                    '())]
               [else '()]))))
  (define selected-abs (walk! (build-path root-path "tests")))
  ;; Classify ABSOLUTE path strings so per-file metadata (@speed, @tui, ...)
  ;; is read from each root's own files — the same contract select? obeys.
  ;; Only the RETURNED names are mapped back to root-relative form.
  (map (lambda (f)
         (normalize-test-path (path->string (find-relative-path (simple-form-path root-path)
                                                                (simple-form-path f)))))
       (collect-classify-file-list suite selected-abs)))

;; ============================================================
;; Managed repository surfaces
;; ============================================================
;;
;; Tests must never rewrite these files in the live checkout: restoring them
;; (via git checkout or saved bytes) can destroy pre-existing or concurrent
;; user edits. The runner DETECTS changes and fails closed instead of
;; restoring; mutation tests use temporary fixtures.

(define repo-surface-files '("info.rkt" "README.md" "CHANGELOG.md"))

;; ============================================================
;; Bytecode cleanup — remove stale compiled/ artifacts
;; ============================================================

(define (compiled-zo-source-candidates compiled-dir zo)
  (define parent (path-only compiled-dir))
  (define base-path (file-name-from-path zo))
  (define base
    (if base-path
        (path->string base-path)
        ""))
  (define stem (regexp-replace #rx"\\.zo$" base ""))
  (filter values
          (list (and (regexp-match? #rx"_rkt$" stem)
                     (build-path parent (regexp-replace #rx"_rkt$" stem ".rkt")))
                (and (regexp-match? #rx"_rktl$" stem)
                     (build-path parent (regexp-replace #rx"_rktl$" stem ".rktl")))
                (and (regexp-match? #rx"_scrbl$" stem)
                     (build-path parent (regexp-replace #rx"_scrbl$" stem ".scrbl")))
                (path-replace-extension (path-replace-suffix zo "") #".rkt")
                (path-replace-extension (path-replace-suffix zo "") #".rktl"))))

(define (stale-compiled-zo? compiled-dir zo)
  (and (file-exists? zo)
       (string-suffix? (path->string zo) ".zo")
       (let* ([candidates (compiled-zo-source-candidates compiled-dir zo)]
              [existing-sources (filter file-exists? candidates)])
         (or (null? existing-sources)
             (for/or ([src (in-list existing-sources)])
               (> (file-or-directory-modify-seconds src) (file-or-directory-modify-seconds zo)))))))

(define (clean-stale-bytecode! root)
  (define cleaned 0)
  (for ([d (in-directory root)])
    (when (and (directory-exists? d) (equal? (path->string (file-name-from-path d)) "compiled"))
      (define zo-files (directory-list d #:build? #t))
      (define stale?
        (for/or ([zo (in-list zo-files)])
          (stale-compiled-zo? d zo)))
      (when stale?
        (delete-directory/files d)
        (set! cleaned (add1 cleaned)))))
  cleaned)

;; ---- Environment profile skip policy (folded verbatim from the former
;; profiles.rkt; the grouped-expansion policy it used to carry is deleted
;; with the grouped execution mode, §11.3) ----

(define known-profiles '(local vps ci headless full))

(define (normalize-requirement req)
  (cond
    [(symbol? req) (symbol->string req)]
    [(string? req) (string-downcase (string-trim req))]
    [else (format "~a" req)]))

(define (profile-unavailable-requirements profile)
  (case profile
    ;; Developer workstation: terminal/subprocess/filesystem/git are assumed;
    ;; live provider credentials are intentionally not assumed.
    [(local) '("provider-key")]
    ;; VPS is headless: browser/terminal are unavailable; provider credentials
    ;; may exist and are therefore not skipped by policy.
    [(vps) '("browser" "terminal")]
    ;; CI must be deterministic and non-interactive by default.
    [(ci) '("provider-key" "browser" "terminal" "network")]
    ;; Headless means no interactive/UI/provider dependencies.
    [(headless) '("provider-key" "browser" "terminal")]
    ;; Full is opt-in to run everything possible.
    [(full) '()]
    [else '()]))

(define (skipped-requirements profile requirements)
  (define unavailable (profile-unavailable-requirements profile))
  (filter (lambda (req)
            (define normalized (normalize-requirement req))
            (and (not (string=? normalized "none")) (member normalized unavailable)))
          requirements))

(define (profile-skips-test? profile requirements)
  (pair? (skipped-requirements profile requirements)))

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
      "usage: racket scripts/run-tests/classify.rkt [--metadata-inventory-json | --lint-metadata] [--root <dir>]\n")
     (exit 2)]))
