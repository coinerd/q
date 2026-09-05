#lang racket/base

;; q/scripts/run-tests/inventory.rkt — Inventory report mode
;;
;; Prints selected/excluded files with classifier hits and risk flags.
;; Extracted from run-tests.rkt (v0.96.16, AX1-2).
;; STABILITY: internal

(require racket/string
         racket/path
         (only-in "classify.rkt"
                  base-dir
                  slow-file?
                  tui-file?
                  security-file?
                  mutating-file?
                  arch-file?
                  runtime-file?
                  extensions-file?
                  workflows-file?
                  support-test-module?
                  get-file-metadata)
         (only-in "reporting.rkt" metadata-completeness))

(provide print-inventory
         classify-exclusion-reason
         detect-high-risk-flags
         compute-inventory-hash
         run-metadata-quality-report
         run-unit-fast-audit
         run-ownership-map
         gate-membership
         v124-behavior-table
         gate-ownership-rows
         gate-ownership-errors
         gate-ownership-markdown
         gate-ownership-json
         gate-ownership-ledger-text
         selected-paths-digest
         run-gate-ownership-map)

(define (classify-exclusion-reason f)
  (cond
    [(string-contains? f "/compiled/") 'compiled]
    [(not (string-suffix? f ".rkt")) 'non-rkt]
    [(support-test-module? f) 'support-module]
    [(hash-ref (get-file-metadata f) 'not-test? #f) 'metadata-not-test]
    [else 'unknown]))

(define (detect-high-risk-flags f)
  (with-handlers ([exn:fail? (lambda (_) '())])
    (define resolved
      (if (absolute-path? f)
          f
          (build-path base-dir f)))
    (define content (file->string resolved))
    (define flags '())
    (when (regexp-match? #rx"current-directory" content)
      (set! flags (cons 'cwd flags)))
    (when (regexp-match? #rx"getenv" content)
      (set! flags (cons 'env flags)))
    (when (regexp-match? #rx"make-temporary" content)
      (set! flags (cons 'temp-file flags)))
    (when (regexp-match? #rx"subprocess" content)
      (set! flags (cons 'subprocess flags)))
    (when (or (regexp-match? #rx"benchmark" content) (regexp-match? #rx"perf" content))
      (set! flags (cons 'perf flags)))
    (when (regexp-match? #rx"terminal" content)
      (set! flags (cons 'terminal flags)))
    (reverse flags)))

;; v1.00.24 W3: stable selected-inventory identity. `equal-hash-code` is
;; randomized per Racket process, so two runs over the identical selection
;; produced different digests and recorded evidence could not be re-derived
;; or compared after the fact. Use the existing SHA-256 selected-path digest
;; (canonical form: sorted, de-duplicated, newline-joined paths).
(define (compute-inventory-hash files)
  (selected-paths-digest files))

(define (list->set lst)
  (for/hash ([x (in-list lst)])
    (values x #t)))

(define (set-member? st k)
  (hash-has-key? st k))

(define (print-inventory suite suite-files)
  (define all-rkt-files
    (for/list ([f (in-directory (build-path base-dir "tests"))]
               #:when (and (file-exists? f)
                           (string-suffix? (path->string f) ".rkt")
                           (not (string-contains? (path->string f) "/compiled/"))))
      (path->string (find-relative-path base-dir f))))
  (define suite-set (list->set suite-files))
  (define excluded (filter (lambda (f) (not (set-member? suite-set f))) all-rkt-files))
  (printf ";; INVENTORY REPORT — suite: ~a~n" suite)
  (printf ";; ═══════════════════════════════════════~n")
  (printf ";; Selected files: ~a~n" (length suite-files))
  (printf ";; Excluded files: ~a~n" (length excluded))
  (printf ";; Inventory hash: ~a~n~n" (compute-inventory-hash suite-files))
  (printf ";; SELECTED FILES:~n")
  (for ([f (in-list (sort suite-files string<?))])
    (define resolved
      (if (absolute-path? f)
          f
          (build-path base-dir f)))
    (define flags
      (if (file-exists? resolved)
          (detect-high-risk-flags f)
          '()))
    (define suite-hits
      (filter values
              (list (and (slow-file? f) 'slow)
                    (and (tui-file? f) 'tui)
                    (and (security-file? f) 'security)
                    (and (mutating-file? f) 'mutating)
                    (and (arch-file? f) 'arch)
                    (and (runtime-file? f) 'runtime)
                    (and (extensions-file? f) 'extensions)
                    (and (workflows-file? f) 'workflows))))
    (when (pair? flags)
      (printf "  ~a  [high-risk: ~a]~n" f (string-join (map symbol->string flags) ",")))
    (when (pair? suite-hits)
      (printf "    classifiers: ~a~n" (string-join (map symbol->string suite-hits) ","))))
  (when (pair? excluded)
    (newline)
    (printf ";; EXCLUDED FILES:~n")
    (for ([f (in-list (sort excluded string<?))])
      (define reason (classify-exclusion-reason f))
      (printf "  ~a  [reason: ~a]~n" f reason))))

(require racket/file
         racket/list
         racket/match
         json)

;; ============================================================
;; Metadata-quality report (W0 baseline measurement)
;; ============================================================
;; Walks every tests/**/test-*.rkt file and reports, per file and per
;; module area, which metadata tags are ok / invalid / missing:
;; @suite @speed @boundary @mutates @isolation @timeout @requires.
;; Also flags files whose classification is heuristic-only
;; (no @suite/@speed; selected purely by path/filename heuristics).

(define metadata-fields '(suite speed boundary mutates isolation timeout requires))

(define (header-lines path)
  (with-handlers ([exn:fail? (lambda (_) '())])
    (call-with-input-file* (if (absolute-path? path)
                               path
                               (build-path base-dir path))
                           (lambda (port)
                             (for/list ([_ (in-range 50)]
                                        #:break (eof-object? (peek-byte port)))
                               (define line (read-line port))
                               (if (string? line) line ""))))))

(define (raw-tag-present? header field)
  (define pattern (pregexp (format "@~a(?:[[:space:]]|$)" (regexp-quote (symbol->string field)))))
  (for/or ([line (in-list header)])
    (and (regexp-match? pattern line) #t)))

(define (field-status path header field)
  (define m (get-file-metadata path))
  (define parsed
    (case field
      [(suite) (hash-ref m 'suite #f)]
      [(speed) (hash-ref m 'speed #f)]
      [(boundary) (hash-ref m 'boundary #f)]
      [(mutates) (hash-ref m 'mutates #f)]
      [(isolation) (hash-ref m 'isolation #f)]
      [(timeout) (hash-ref m 'timeout #f)]
      [(requires) (and (pair? (hash-ref m 'requires '())) #t)]
      [else #f]))
  (cond
    [parsed 'ok]
    [(raw-tag-present? header field) 'invalid]
    [else 'missing]))

(define (collect-test-star-files)
  (sort (for/list ([f (in-directory (build-path base-dir "tests"))]
                   #:when (and (file-exists? f)
                               (regexp-match? #rx"(^|/)test-[^/]*\\.rkt$" (path->string f))
                               (not (string-contains? (path->string f) "/compiled/"))))
          (path->string (find-relative-path base-dir f)))
        string<?))

(define (file-area f)
  (define parts
    (string-split (if (path? f)
                      (path->string f)
                      f)
                  "/"))
  (cond
    [(and (>= (length parts) 3) (string=? (car parts) "tests")) (cadr parts)]
    [(and (>= (length parts) 2) (string=? (car parts) "tests")) "(root)"]
    [else "(other)"]))

(define (file-metadata-quality f)
  (define header (header-lines f))
  (define statuses
    (for/hasheq ([field (in-list metadata-fields)])
      (values field (field-status f header field))))
  (hasheq 'file
          f
          'area
          (file-area f)
          'metadata_completeness
          (symbol->string (metadata-completeness f))
          'fields
          statuses))

(define (status->flag field status)
  (case status
    [(missing) (format "missing:@~a" field)]
    [(invalid) (format "invalid:@~a" field)]
    [else #f]))

(define (summarize-quality files)
  (define records (map file-metadata-quality files))
  (define per-field
    (for/hasheq ([field (in-list metadata-fields)])
      (values field
              (hasheq 'missing
                      (count (lambda (rec) (eq? 'missing (hash-ref (hash-ref rec 'fields) field)))
                             records)
                      'invalid
                      (count (lambda (rec) (eq? 'invalid (hash-ref (hash-ref rec 'fields) field)))
                             records)))))
  (define per-area
    (for/hash ([area (in-list (remove-duplicates (map (lambda (rec) (hash-ref rec 'area)) records)))])
      (values area (count (lambda (rec) (string=? (hash-ref rec 'area) area)) records))))
  (define-values (explicit heuristic missing)
    (for/fold ([explicit 0]
               [heuristic 0]
               [missing 0])
              ([rec (in-list records)])
      (case (string->symbol (hash-ref rec 'metadata_completeness))
        [(explicit) (values (add1 explicit) heuristic missing)]
        [(heuristic) (values explicit (add1 heuristic) missing)]
        [else (values explicit heuristic (add1 missing))])))
  (hasheq 'file_count
          (length records)
          'fields
          metadata-fields
          'per_field
          per-field
          'per_area
          (for/hasheq ([(area n) (in-hash per-area)])
            (values (string->symbol area) n))
          'metadata_completeness
          (hasheq 'explicit explicit 'heuristic heuristic 'missing missing)
          'files
          records))

(define (run-metadata-quality-report #:json-out [json-out #f])
  (define files (collect-test-star-files))
  (define summary (summarize-quality files))
  (define records (hash-ref summary 'files))
  (define completeness (hash-ref summary 'metadata_completeness))
  (printf ";; METADATA QUALITY REPORT~n")
  (printf ";; ═════════════════════════~n")
  (printf ";; Files scanned (tests/**/test-*.rkt, excluding compiled/): ~a~n"
          (hash-ref summary 'file_count))
  (for ([field (in-list metadata-fields)])
    (define counts (hash-ref (hash-ref summary 'per_field) field))
    (printf ";; @~a: ~a missing, ~a invalid~n"
            field
            (hash-ref counts 'missing)
            (hash-ref counts 'invalid)))
  (printf ";; metadata-completeness: explicit=~a heuristic-only=~a missing=~a~n"
          (hash-ref completeness 'explicit)
          (hash-ref completeness 'heuristic)
          (hash-ref completeness 'missing))
  (printf ";; heuristic-only classification (no @suite/@speed): ~a files~n~n"
          (+ (hash-ref completeness 'heuristic) (hash-ref completeness 'missing)))
  ;; Per-module-area listing
  (define areas (sort (remove-duplicates (map (lambda (rec) (hash-ref rec 'area)) records)) string<?))
  (for ([area (in-list areas)])
    (define area-records (filter (lambda (rec) (string=? (hash-ref rec 'area) area)) records))
    (printf ";; [area: ~a] ~a file~a~n"
            area
            (length area-records)
            (if (= (length area-records) 1) "" "s"))
    (for ([rec (in-list area-records)])
      (define flags
        (filter values
                (for/list ([field (in-list metadata-fields)])
                  (status->flag field (hash-ref (hash-ref rec 'fields) field)))))
      (cond
        [(pair? flags) (printf "  ~a  [~a]~n" (hash-ref rec 'file) (string-join flags " "))]
        [(string=? (hash-ref rec 'metadata_completeness) "explicit")
         (printf "  ~a  [complete]~n" (hash-ref rec 'file))]
        [else
         (printf "  ~a  [complete, ~a classification]~n"
                 (hash-ref rec 'file)
                 (hash-ref rec 'metadata_completeness))]))
    (newline))
  (when json-out
    (define payload
      (hasheq 'generator
              "inventory.rkt --metadata-quality"
              'file_count
              (hash-ref summary 'file_count)
              'per_field
              (hash-ref summary 'per_field)
              'per_area
              (hash-ref summary 'per_area)
              'metadata_completeness
              completeness
              'files
              records))
    (call-with-output-file json-out
                           #:exists 'truncate/replace
                           (lambda (out) (write-json payload out)))
    (printf ";; JSON report written to ~a~n" json-out))
  summary)

(require racket/port
         racket/file
         (only-in "classify.rkt" collect-test-files)
         (only-in "classify-filters.rkt" unit-fast-file?))

;; ============================================================
;; unit-fast eligibility audit (W3)
;; ============================================================
;; Lists unit-fast candidates and flags files that must NOT join the
;; grouped in-process execution mode: declared mutations, declared or
;; lexically-detected env/filesystem/network/process side effects, or
;; a missing `module+ test` form (no RackUnit discovery point for
;; grouped execution).

(define (file-content f)
  (with-handlers ([exn:fail? (lambda (_) "")])
    (file->string (if (absolute-path? f)
                      f
                      (build-path base-dir f)))))

(define (has-module-plus-test? f)
  (regexp-match? #px"\\(module\\+\\s+test\\b" (file-content f)))

(define (metadata-value->string v)
  (if v
      (format "~a" v)
      ""))

(define (metadata-suite=? f suite-name)
  (define v (metadata-value->string (hash-ref (get-file-metadata f) 'suite #f)))
  (string=? v suite-name))

(define (declared-mutation f)
  (define v (hash-ref (get-file-metadata f) 'mutates #f))
  (and v (not (member (metadata-value->string v) '("none" "#f" "false"))) (metadata-value->string v)))

(define (audit-unsafe-reasons f)
  (define content (file-content f))
  (define reasons '())
  (define (flag! r)
    (set! reasons (cons r reasons)))
  (cond
    [(declared-mutation f)
     =>
     (lambda (v) (flag! (format "declared-mutation:@mutates=~a" v)))])
  (when (regexp-match? #rx"getenv|putenv" content)
    (flag! "side-effect:env"))
  (when (regexp-match? #rx"make-temporary-file|make-temporary-directory" content)
    (flag! "side-effect:temp-file"))
  (when (regexp-match? #rx"delete-file|delete-directory|copy-file|rename-file-or-directory" content)
    (flag! "side-effect:filesystem-write"))
  (when (regexp-match? #rx"current-directory" content)
    (flag! "side-effect:cwd"))
  (when (regexp-match? #rx"subprocess|process\\*? " content)
    (flag! "side-effect:subprocess"))
  (when (regexp-match? #rx"tcp-connect|ssl-connect|get-pure-port|post-pure-port|http-send" content)
    (flag! "side-effect:network"))
  (unless (has-module-plus-test? f)
    (flag! "missing:module+test-form"))
  (reverse reasons))

(define (unit-fast-candidates)
  (sort (remove-duplicates (append (collect-test-files 'unit-fast)
                                   (filter unit-fast-file? (collect-test-files 'all))))
        string<?))

(define (audit-unit-fast-records)
  (for/list ([f (in-list (unit-fast-candidates))])
    (define reasons (audit-unsafe-reasons f))
    (hasheq 'file f 'eligible (null? reasons) 'reasons reasons)))

(define (run-unit-fast-audit #:json-out [json-out #f])
  (define records (audit-unit-fast-records))
  (define eligible (filter (lambda (r) (hash-ref r 'eligible)) records))
  (define flagged (filter (lambda (r) (not (hash-ref r 'eligible))) records))
  (define missing-form
    (filter (lambda (r) (member "missing:module+test-form" (hash-ref r 'reasons))) records))
  (printf ";; UNIT-FAST ELIGIBILITY AUDIT (W3)~n")
  (printf ";; ═════════════════════════════════════~n")
  (printf ";; Candidates (metadata @suite unit-fast + unit-fast classifier): ~a~n" (length records))
  (printf ";; Grouped/in-process eligible: ~a~n" (length eligible))
  (printf ";; Flagged unsafe (excluded from grouped mode): ~a~n" (length flagged))
  (printf ";; Of which missing `module+ test` form: ~a~n~n" (length missing-form))
  (for ([r (in-list (sort flagged string<? #:key (lambda (r) (hash-ref r 'file))))])
    (printf "  ~a  [~a]~n" (hash-ref r 'file) (string-join (hash-ref r 'reasons) " ")))
  (when (null? flagged)
    (displayln "  (no unsafe candidates — all candidates eligible for grouped mode)"))
  (when json-out
    (define payload
      (hasheq 'generator
              "inventory.rkt --unit-fast-audit"
              'candidates
              (length records)
              'eligible
              (length eligible)
              'flagged
              (length flagged)
              'missing_module_plus_test
              (length missing-form)
              'files
              (sort records string<? #:key (lambda (r) (hash-ref r 'file)))))
    (ensure-parent-dir! json-out)
    (call-with-output-file json-out
                           #:exists 'truncate/replace
                           (lambda (out) (write-json payload out)))
    (printf ";; JSON report written to ~a~n" json-out))
  (hasheq 'candidates
          (length records)
          'eligible
          (length eligible)
          'flagged
          (length flagged)
          'missing_module_plus_test
          (length missing-form)))

;; ============================================================
;; Test ownership map (W3)
;; ============================================================
;; Maps each production area to its accountable test destination
;; (suite tag, boundary tags, owning path). A production area with no
;; test destination is reported as a GAP. Output is deterministic.

(define (session-file? f)
  ;; Agent-session behaviour tests currently live flat in tests/ under
  ;; @suite runtime; they are the session area's accountable destination.
  (or (regexp-match? #rx"^tests/test-agent-(session|loop-fsm|iteration|queue|registry|state)" f)
      (string-prefix? f "tests/agent/")
      (string-prefix? f "tests/session/")))

(define ownership-area-definitions
  (list (hasheq 'area
                "runtime"
                'source
                "runtime/"
                'suite
                "runtime"
                'predicate
                'runtime-file?
                'dirs
                '("tests/runtime"))
        (hasheq 'area
                "provider"
                'source
                "llm/ (provider adapters)"
                'suite
                "provider"
                'predicate
                #f
                'dirs
                '("tests/llm" "tests/provider"))
        (hasheq 'area
                "session"
                'source
                "agent/ (agent session)"
                'suite
                "runtime"
                'predicate
                'session-file?
                'dirs
                '("tests/agent" "tests/session"))
        (hasheq 'area
                "tools"
                'source
                "tools/ + agent/roles/tool-gateway"
                'suite
                "tools"
                'predicate
                #f
                'dirs
                '("tests/tools"))
        (hasheq 'area
                "extensions"
                'source
                "extensions/"
                'suite
                "extensions"
                'predicate
                'extensions-file?
                'dirs
                '("tests/extensions"))
        (hasheq 'area "tui" 'source "tui/" 'suite "tui" 'predicate 'tui-file? 'dirs '("tests/tui"))
        (hasheq 'area
                "workflows"
                'source
                "scripts/run-tests/workflows/ + GSD"
                'suite
                "workflows"
                'predicate
                'workflows-file?
                'dirs
                '("tests/workflows" "tests/gsd"))))

(define (area-predicate-match? pred-sym f)
  (case pred-sym
    [(runtime-file?) (runtime-file? f)]
    [(session-file?) (session-file? f)]
    [(extensions-file?) (extensions-file? f)]
    [(tui-file?) (tui-file? f)]
    [(workflows-file?) (workflows-file? f)]
    [else #f]))

(define (parent-dir s)
  (define m (regexp-match-positions #rx"/[^/]*$" s))
  (if m
      (substring s 0 (car (car m)))
      "."))

(define (ensure-parent-dir! p)
  (define parent (path-only p))
  (when parent
    (make-directory* parent)))

(define (ownership-records)
  (define all (collect-test-files 'all))
  (for/list ([def (in-list ownership-area-definitions)])
    (define suite-name (hash-ref def 'suite))
    (define dirs (hash-ref def 'dirs))
    (define tests
      (sort (remove-duplicates (filter (lambda (f)
                                         (or (metadata-suite=? f suite-name)
                                             (area-predicate-match? (hash-ref def 'predicate) f)
                                             (for/or ([d (in-list dirs)])
                                               (string-prefix? f d))))
                                       all))
            string<?))
    (define boundaries
      (sort (remove-duplicates (filter (lambda (s) (non-empty-string? s))
                                       (for/list ([f (in-list tests)])
                                         (metadata-value->string
                                          (hash-ref (get-file-metadata f) 'boundary #f)))))
            string<?))
    (define owning-path
      (and (pair? tests)
           (let* ([counts (for/fold ([acc (hash)]) ([f (in-list tests)])
                            (hash-update acc (parent-dir f) add1 0))]
                  [keys (for/list ([(k v) (in-hash counts)])
                          k)])
             (first (sort keys
                          (lambda (a b)
                            (or (> (hash-ref counts a) (hash-ref counts b))
                                (and (= (hash-ref counts a) (hash-ref counts b))
                                     (string<? a b)))))))))
    (hasheq 'area
            (hash-ref def 'area)
            'production_source
            (hash-ref def 'source)
            'suite
            suite-name
            'test_count
            (length tests)
            'boundary_tags
            boundaries
            'owning_path
            owning-path
            'gap?
            (null? tests))))

(define (ownership-markdown records)
  (string-append "# Test Ownership Map (generated — do not edit)\n\n"
                 "Generated by `scripts/run-tests/inventory.rkt --ownership-map` (W3).\n"
                 "Every production area must have an accountable test destination.\n\n"
                 "| Area | Production source | Suite | Tests | Boundary tags | Owning path |\n"
                 "|---|---|---|---|---|---|\n"
                 (apply string-append
                        (for/list ([r (in-list records)])
                          (format "| ~a | ~a | ~a | ~a | ~a | ~a |\n"
                                  (hash-ref r 'area)
                                  (hash-ref r 'production_source)
                                  (hash-ref r 'suite)
                                  (hash-ref r 'test_count)
                                  (string-join (hash-ref r 'boundary_tags) ", ")
                                  (or (hash-ref r 'owning_path) "**none**"))))
                 "\n"
                 (if (for/or ([r (in-list records)])
                       (hash-ref r 'gap?))
                     "**GAPS:** areas above with `**none**` have no test destination.\n"
                     "No gaps: every production area has a test destination.\n")))

(define (run-ownership-map #:md-out [md-out #f] #:json-out [json-out #f])
  (define records (ownership-records))
  (define gaps (filter (lambda (r) (hash-ref r 'gap?)) records))
  (printf ";; TEST OWNERSHIP MAP (W3)~n")
  (printf ";; ═══════════════════════~n")
  (for ([r (in-list records)])
    (printf ";; ~a (source: ~a) -> suite ~a: ~a test(s), boundaries [~a], owner: ~a~a~n"
            (hash-ref r 'area)
            (hash-ref r 'production_source)
            (hash-ref r 'suite)
            (hash-ref r 'test_count)
            (string-join (hash-ref r 'boundary_tags) ",")
            (or (hash-ref r 'owning_path) "<none>")
            (if (hash-ref r 'gap?) "  ** GAP: no test destination **" "")))
  (printf ";; areas: ~a, gaps: ~a~n~n" (length records) (length gaps))
  (define md-path (or md-out (build-path base-dir "reports" "test-ownership-map.md")))
  (define json-path (or json-out (build-path base-dir "reports" "test-ownership-map.json")))
  (ensure-parent-dir! md-path)
  (call-with-output-file md-path
                         #:exists 'truncate/replace
                         (lambda (out) (display (ownership-markdown records) out)))
  (ensure-parent-dir! json-path)
  (call-with-output-file
   json-path
   #:exists 'truncate/replace
   (lambda (out) (write-json (hasheq 'generator "inventory.rkt --ownership-map" 'areas records) out)))
  (printf ";; markdown report written to ~a~n" md-path)
  (printf ";; json report written to ~a~n" json-path)
  (hasheq 'areas (length records) 'gaps (length gaps)))

;; ============================================================
;; v1.00.24 W0 cross-gate ownership map
;; ============================================================
;; Freezes, for the eight v1.00.24 candidate behaviors, where each
;; behavior's evidence lives today (source tier == destination tier;
;; retained-in-place for W0) and validates that declared membership
;; matches what the actual suite classifiers select. Deterministic;
;; missing evidence is reported, never imputed.

(require "sha256.rkt")

(define v124-gate-names '("fast" "platform" "security" "workflows" "unit-fast" "slow/L4"))

;; Gate name -> sorted list of selected test files (repository walk).
(define (gate-membership)
  (hasheq "fast"
          (collect-test-files 'fast)
          "platform"
          (collect-test-files 'platform)
          "security"
          (collect-test-files 'security)
          "workflows"
          (collect-test-files 'workflows)
          "unit-fast"
          (collect-test-files 'unit-fast)
          "slow/L4"
          (collect-test-files 'slow)))

;; Frozen v1.00.24 W0 candidate behavior table. Members are real test
;; files at freeze time; `--check` re-derives gate membership and
;; reports drift instead of silently accepting moves.
(define v124-behavior-table
  (list
   (hasheq
    'behavior-id
    "RETRY-LOGICAL-SEMANTICS-FAST"
    'source-gate
    "fast"
    'destination-gate
    "fast"
    'members
    '("tests/test-partial-result-preservation.rkt" "tests/test-agent-session-basic.rkt")
    'owner
    "agent-session"
    'status
    "retained-in-place"
    'wave
    "W2"
    'rationale
    (string-append
     "W2 re-tier: unit/component retry assertions run under with-deterministic-retries (sleep-scale 0.0 seam) and assert the LOGICAL retry "
     "semantics — computed per-attempt delays via auto-retry.start events, attempt counts, continuation-prompt injection, retry-exhausted "
     "payload, and total-retry-delay-ms — identical to the production scale-1.0 computation, while paying no real production backoff. "
     "Production current-auto-retry-sleep-scale default (1.0) is unchanged; the seam is test-scoped only."))
   (hasheq
    'behavior-id
    "RETRY-REAL-TIMER-CANARY"
    'source-gate
    "slow/L4"
    'destination-gate
    "slow/L4"
    'members
    '("tests/test-auto-retry-timer-canary.rkt")
    'owner
    "agent-session"
    'status
    "re-tiered"
    'wave
    "W2"
    'rationale
    (string-append
     "W2 re-tier: the real (wall-clock) auto-retry sleep path is owned by exactly one bounded slow/L4 integration canary. The logical retry "
     "behavior moved out of the pre-W2 fast files into the deterministic fast row above; source-gate slow/L4 records classifier reality (the "
     "canary carries @speed slow), matching the W1 convention of declaring source gates per the classifier rather than tier intent. The "
     "canary uses a deliberately tiny nonzero delay with a generous bounded assertion window so it proves the sleep is actually connected — "
     "distinguishing too-early from timeout — without depending on exact scheduler timing or ever paying production-sized seconds. This is "
     "the sole executable destination for real-timer retry integration coverage."))
   (hasheq
    'behavior-id
    "CWD-INVOCATION-AUDIT-CANARY"
    'source-gate
    "fast"
    'destination-gate
    "fast"
    'members
    '("tests/test-cwd-independence.rkt")
    'owner
    "test-runtime"
    'status
    "retained-in-place"
    'wave
    "W1"
    'rationale
    (string-append
     "W1 re-tier: the three recursive fast subprocess spot-checks (full nested test runs) were removed; the fast canary is now the minimal "
     "absolute-path probe invocation in tests/test-cwd-independence.rkt. Real audit-script CWD behavior is owned by "
     "tests/test-audit-script.rkt in slow/L4, which invokes the absolute audit script from an arbitrary cwd without self-recursion."))
   (hasheq 'behavior-id
           "PARTIAL-RESULT-AGENT-SESSION-RETRY-CHAIN"
           'source-gate
           "fast"
           'destination-gate
           "fast"
           'members
           '("tests/test-agent-session.rkt" "tests/test-turn-retry.rkt"
                                            "tests/test-iteration-retry.rkt")
           'owner
           "agent-session"
           'status
           "retained-in-place"
           'wave
           "W0"
           'rationale
           "Retry-chain partial-result semantics stay in fast unit coverage this milestone.")
   (hasheq
    'behavior-id
    "GSD-WAVE-TIMEOUT-CANCELLATION"
    'source-gate
    "fast"
    'destination-gate
    "fast"
    'members
    '("tests/test-goal-runner-timeout.rkt" "tests/test-run-tests-timeout-cleanup.rkt")
    'owner
    "gsd-delivery"
    'status
    "retained-in-place"
    'wave
    "W0"
    'rationale
    (string-append
     "Timeout/cancellation unit tests are fast-classified (suite default/runtime); W1 corrected the gate declaration from slow/L4 to fast to "
     "match classifier reality (the classifier, not tier intent, selects these files)."))
   (hasheq
    'behavior-id
    "RUNNER-REPOSITORY-DISCOVERY"
    'source-gate
    "slow/L4"
    'destination-gate
    "slow/L4"
    'members
    '("tests/test-run-tests.rkt")
    'owner
    "test-runtime"
    'status
    "retained-in-place"
    'wave
    "W0"
    'rationale
    (string-append
     "Runner repository-discovery behavior is exercised by the slow/L4 runner test (W1 gate correction: the live walker is slow-classified). "
     "The metadata-discovery driver tests/test-run-tests-metadata-discovery.rkt is @not-test frozen fixture input selected by no executable "
     "gate and was dropped from membership in W1."))
   (hasheq 'behavior-id
           "GOLDEN-SESSION-LIFECYCLE"
           'source-gate
           "fast"
           'destination-gate
           "fast"
           'members
           '("tests/test-golden-flows.rkt" "tests/test-session-lifecycle-characterization.rkt")
           'owner
           "agent-session"
           'status
           "retained-in-place"
           'wave
           "W0"
           'rationale
           "Golden lifecycle characterizations remain the accountable fast-tier destination.")
   (hasheq
    'behavior-id
    "GSD-DELIVERY-VERIFIER-GIT-SANDBOXES"
    'source-gate
    "fast"
    'destination-gate
    "fast"
    'members
    '("tests/ci/verify-lock-selection-test.rkt")
    'owner
    "gsd-delivery"
    'status
    "retained-in-place"
    'wave
    "W0"
    'rationale
    (string-append
     "Lock-selection verifier sandbox test is fast-classified (suite ci, no slow tags); W1 corrected the gate declaration from slow/L4 to "
     "fast to match classifier reality."))
   (hasheq
    'behavior-id
    "GSD-WAVE-WORKTREE-SANDBOXES"
    'source-gate
    "fast"
    'destination-gate
    "fast"
    'members
    '("tests/test-gsd-wave-worktree.rkt")
    'owner
    "gsd-delivery"
    'status
    "retained-in-place"
    'wave
    "W0"
    'rationale
    (string-append
     "Wave worktree sandbox test has no explicit metadata tags and is selected by the fast classifier default; W1 corrected the gate "
     "declaration from slow/L4 to fast to match classifier reality."))
   (hasheq
    'behavior-id
    "GROUPED-MODE-CHARACTERIZATION"
    'source-gate
    "fast"
    'destination-gate
    "fast"
    'members
    '("tests/test-run-tests-in-process-mode.rkt" "tests/test-execution-plane-characterization.rkt")
    'owner
    "test-runtime"
    'status
    "retained-in-place"
    'wave
    "W0"
    'rationale
    "Grouped in-process execution characterization stays hermetic and fast-tier owned.")
   (hasheq
    'behavior-id
    "GSD-TIMEOUT-DETERMINISTIC-SEAM-FAST"
    'source-gate
    "fast"
    'destination-gate
    "fast"
    'members
    '("tests/test-gsd-system-adapters-timeout.rkt" "tests/test-gsd-wave-executor-isolation.rkt")
    'owner
    "gsd-delivery"
    'status
    "retained-in-place"
    'wave
    "W4"
    'rationale
    (string-append
     "W4 re-tier: deterministic GSD wave timeout semantics remain fast — deadline expiry, external cancellation, exactly-once cancel/outcome "
     "emission, cooperative grace, and force-kill-after-grace assertions run against run-wave-with-timeout behind the injected deterministic "
     "timeout clock/wait seam (with-deterministic-timeout stages) and pay no wall-clock deadline or grace. Production defaults "
     "(current-inexact-milliseconds, sync/timeout, the two-second grace) are unchanged; the seam is test-scoped parameters only. "
     "Pre-W4 executor-isolation timeout cases that slept past real one-second deadlines now use the same seam with identical assertions "
     "and preserved campaign persistence (DONE never recorded on timeout), retry-count isolation, and durable cancellation coverage."))
   (hasheq
    'behavior-id
    "GSD-TIMEOUT-REAL-CLOCK-CANARY"
    'source-gate
    "slow/L4"
    'destination-gate
    "slow/L4"
    'members
    '("tests/test-gsd-wave-timeout-canary.rkt")
    'owner
    "gsd-delivery"
    'status
    "re-tiered"
    'wave
    "W4"
    'rationale
    (string-append
     "W4 re-tier: real clock/thread integration for the GSD wave timeout adapter is owned by exactly one bounded slow/L4 canary (the file "
     "carries @speed slow), mirroring the W2 RETRY-REAL-TIMER-CANARY convention. The canary proves the production adapter completes inside "
     "a real deadline, requests cancellation, and force-reaps a stubborn never-finishing worker after the real two-second grace, with "
     "jitter-tolerant assertions and a hard 12-second ceiling. It is the sole executable destination for real-clock timeout wiring."))
   (hasheq
    'behavior-id
    "RUNNER-DISCOVERY-UNIT-FIXTURE-ROOT"
    'source-gate
    "unit-fast"
    'destination-gate
    "unit-fast"
    'members
    '("tests/test-run-tests-shard.rkt")
    'owner
    "test-runtime"
    'status
    "re-tiered"
    'wave
    "W5"
    'rationale
    (string-append
     "W5 re-tier: runner classifier/sharding unit assertions collect from the hermetic fixture tree tests/fixtures/run-tests-discovery/ "
     "through the new #:root seam on collect-test-files instead of crawling the production repository. The fixture tree owns fast, slow, "
     "platform, TUI, helper, malformed/edge-metadata, named/unnamed, symlink, and nested cases; unit assertions cover deterministic path "
     "sort, metadata/heuristic selection, platform inclusion, shard partition, helper/fixture exclusion, missing-root failure, and no "
     "escape through symlinks or .. without depending on the number of files in the live checkout. Omitted-root calls preserve the pre-W5 "
     "q-root discovery byte-for-byte, asserted from both directions against the ignored-prefix contract. The test process now propagates "
     "rackunit failure/error counts to its exit code so runner exit-code verdicts observe real failures."))
   (hasheq
    'behavior-id
    "RUNNER-REPOSITORY-DISCOVERY-L4"
    'source-gate
    "slow/L4"
    'destination-gate
    "slow/L4"
    'members
    '("tests/test-run-tests-repository-discovery.rkt")
    'owner
    "test-runtime"
    'status
    "re-tiered"
    'wave
    "W5"
    'rationale
     (string-append
      "W5 re-tier: exactly one scheduled slow/L4 smoke owns REAL repository-scale discovery. It asserts invariant properties only — "
      "nonempty normalized default discovery, suite containment/exclusion, a nonempty real platform inventory (responsibility moved from "
      "the fixture-root unit tests), deterministic 64-hex selected-path digests sensitive to the selected set, and default-call "
      "compatibility of the #:root seam — never a brittle exact file count. Classifier semantics for fixture-root units remain fast; "
      "live repository discovery remains executable in L4."))
   (hasheq
    'behavior-id
    "PRIVATE-FIXTURE-TEMPLATE-CONTRACT"
    'source-gate
    "fast"
    'destination-gate
    "fast"
    'members
    '("tests/test-private-fixture-templates.rkt")
    'owner
    "test-design"
    'status
    "retained-in-place"
    'wave
    "W6"
    'rationale
    (string-append
     "W6 retained: new fast contract/stress destination owning the copy-on-test fixture-template invariants — distinct private canonical "
     "roots for concurrent instances, immutable template source, independent ref/history/CWD/env mutation, cross-instance and "
     "template-safety proof, arbitrary-order destruction with idempotent cleanup, and explicit git-unavailable skip semantics (never a "
     "silent pass). No product behavior is owned here."))
   (hasheq
    'behavior-id
    "GOLDEN-SESSION-PRIVATE-TEMPLATE"
    'source-gate
    "fast"
    'destination-gate
    "fast"
    'members
    '("tests/test-golden-flows.rkt")
    'owner
    "test-design"
    'status
    "retained-in-place"
    'wave
    "W6"
    'rationale
    (string-append
     "W6 retained-in-place (no re-tier): golden-session lifecycle family keeps its fast tier and every behavioral assertion. Repeated "
     "baseline session construction is centralized behind the private copy-on-test session template (immutable "
     "tests/fixtures/session-template/ copied to a fresh private temp root per test with fresh session IDs/event buses/registries); "
     "scratch-build and meaningful multi-turn canary cases remain in the owner file."))
   (hasheq
    'behavior-id
    "GSD-DELIVERY-VERIFIER-PRIVATE-TEMPLATE"
    'source-gate
    "fast"
    'destination-gate
    "fast"
    'members
    '("tests/test-gsd-delivery-verifier.rkt")
    'owner
    "test-design"
    'status
    "retained-in-place"
    'wave
    "W6"
    'rationale
    (string-append
     "W6 retained-in-place (no re-tier): delivery-verifier Git sandbox family keeps its fast tier and assertions. Duplicated "
     "init/config/baseline-commit scaffolding is centralized in the lazy per-process Git template and cloned privately per test with "
     "no shared refs, index, worktree metadata, config, or hooks; hermetic user identity and the offline origin/main stand-in are "
     "preserved."))
   (hasheq
    'behavior-id
    "GSD-WAVE-WORKTREE-PRIVATE-TEMPLATE"
    'source-gate
    "fast"
    'destination-gate
    "fast"
    'members
    '("tests/test-gsd-wave-worktree.rkt")
    'owner
    "test-design"
    'status
    "retained-in-place"
    'wave
    "W6"
    'rationale
    (string-append
     "W6 retained-in-place (no re-tier): GSD wave-worktree family keeps its fast tier and assertions. Duplicated baseline-repository "
     "construction is centralized in the shared lazy Git template; real Git/filesystem behavior, family-specific branch/commit/"
     "dirty-tree/delivery/cleanup/orphan assertions, and offline origin/main stand-ins remain in the owner file."))
   (hasheq
    'behavior-id
    "GSD-BRANCH-DELIVERY-PRIVATE-TEMPLATE"
    'source-gate
    "fast"
    'destination-gate
    "fast"
    'members
    '("tests/test-gsd-branch-delivery-verification.rkt")
    'owner
    "test-design"
    'status
    "retained-in-place"
    'wave
    "W6"
    'rationale
    (string-append
     "W6 retained-in-place (no re-tier): branch-delivery verification family adopts the shared private Git sandbox builder for its "
     "baseline repository only; branch, commit, dirty-tree, delivery, cleanup, and orphan assertions stay unchanged in the owner "
     "file."))))

;; Rows actually owned by this milestone (frozen table).
(define (gate-ownership-rows [memberships (gate-membership)])
  v124-behavior-table)

;; Pure validator: declared table vs a membership hash
;; (gate-name -> list of files). Reports:
;;  - duplicate behavior IDs
;;  - members selected by no gate at all (missing destination)
;;  - members not selected by the declared source tier (membership drift)
;;  - rows with no member selected by the declared destination tier
(define (gate-ownership-errors rows [memberships (gate-membership)])
  (define errors '())
  (define seen (make-hash))
  (for ([r (in-list rows)])
    (define id (hash-ref r 'behavior-id))
    (when (hash-ref seen id #f)
      (set! errors (cons (format "~a: duplicate behavior ID" id) errors)))
    (hash-set! seen id #t))
  (for ([r (in-list rows)])
    (define id (hash-ref r 'behavior-id))
    (define src (hash-ref r 'source-gate))
    (define dest (hash-ref r 'destination-gate))
    (define members (hash-ref r 'members '()))
    (define (gate-has? g f)
      (and (member f (hash-ref memberships g '())) #t))
    (for ([m (in-list members)])
      (unless (for/or ([g (in-list v124-gate-names)])
                (gate-has? g m))
        (set! errors
              (cons (format "~a: missing destination: member ~a is selected by no gate" id m)
                    errors)))
      (unless (gate-has? src m)
        (set!
         errors
         (cons (format "~a: membership drift: member ~a is not selected by source gate ~a" id m src)
               errors))))
    (unless (for/or ([m (in-list members)])
              (gate-has? dest m))
      (set! errors
            (cons (format "~a: destination not selected: no member is selected by destination gate ~a"
                          id
                          dest)
                  errors))))
  (reverse errors))

(define (gate-ownership-markdown
         rows
         [gate-stats (v124-gate-stats (gate-membership))]
         [selected-digest
          (selected-paths-digest (remove-duplicates (append-map (lambda (g)
                                                                  (hash-ref (gate-membership) g '()))
                                                                v124-gate-names)))]
         [members-meta (v124-members-meta rows)])
  (string-append
   "# TEST-GATE-OWNERSHIP v1.00.24 (generated — do not edit)\n\n"
   "Generated by `scripts/run-tests/inventory.rkt --gate-ownership-map` (W0).\n"
   "W0 retains every candidate behavior in its source tier (retained-in-place).\n\n"
   "## Selected-path digest (union of all gates)\n\n`"
   selected-digest
   "`\n\n"
   "## Gate membership (deterministic repository walk)\n\n"
   "| Gate | Selected files | Selected-path digest |\n"
   "|---|---|---|\n"
   (apply
    string-append
    (for/list ([s (in-list gate-stats)])
      (format "| ~a | ~a | ~a |\n" (hash-ref s 'gate) (hash-ref s 'count) (hash-ref s 'digest))))
   "\n"
   "## Behavior ownership (frozen v1.00.24 W0 candidate rows)\n\n"
   "| Behavior | Source tier | Destination tier | Members | Owner | Status | Wave | Rationale |\n"
   "|---|---|---|---|---|---|---|---|\n"
   (apply string-append
          (for/list ([r (in-list rows)])
            (format "| ~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a |\n"
                    (hash-ref r 'behavior-id)
                    (hash-ref r 'source-gate)
                    (hash-ref r 'destination-gate)
                    (string-join (hash-ref r 'members) "<br>")
                    (hash-ref r 'owner)
                    (hash-ref r 'status)
                    (hash-ref r 'wave)
                    (hash-ref r 'rationale))))
   "\n"
   "## Metadata boundary and declared side effects (per member)\n\n"
   "| Behavior | File | Suite | Speed | Boundary | Mutates | Isolation | Requires | Timeout |\n"
   "|---|---|---|---|---|---|---|---|---|\n"
   (apply string-append
          (for/list ([bm (in-list members-meta)]
                     [r (in-list rows)])
            (apply string-append
                   (for/list ([m (in-list (hash-ref bm 'members))])
                     (format "| ~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a |\n"
                             (hash-ref r 'behavior-id)
                             (hash-ref m 'file)
                             (hash-ref m 'suite)
                             (hash-ref m 'speed)
                             (hash-ref m 'boundary)
                             (hash-ref m 'mutates)
                             (hash-ref m 'isolation)
                             (hash-ref m 'requires)
                             (hash-ref m 'timeout))))))))

(define (gate-ownership-json
         rows
         [gate-stats (v124-gate-stats (gate-membership))]
         [selected-digest
          (selected-paths-digest (remove-duplicates (append-map (lambda (g)
                                                                  (hash-ref (gate-membership) g '()))
                                                                v124-gate-names)))]
         [members-meta (v124-members-meta rows)])
  (define payload
    (hasheq 'generator
            "inventory.rkt --gate-ownership-map"
            'milestone
            "v1.00.24"
            'wave
            "W0"
            'selected_paths_digest
            selected-digest
            'gates
            (for/list ([s (in-list gate-stats)])
              (for/hash ([(k v) (in-hash s)])
                (values k v)))
            'behaviors
            (for/list ([r (in-list rows)])
              (for/hash ([(k v) (in-hash r)])
                (values k v)))
            'members_meta
            (for/list ([bm (in-list members-meta)])
              (hasheq 'behavior_id
                      (hash-ref bm 'behavior-id)
                      'members
                      (for/list ([m (in-list (hash-ref bm 'members))])
                        (for/hash ([(k v) (in-hash m)])
                          (values k v)))))))
  (with-output-to-string (lambda () (write-json payload))))

(define (gate-ownership-ledger-text rows)
  (string-append ";; TEST-RETIER-LEDGER v1.00.24 — W0 retained-in-place snapshot\n"
                 ";; Readable Racket datum: (gate-ownership-ledger (<row> ...)).\n"
                 (with-output-to-string (lambda () (write (list 'gate-ownership-ledger rows))))
                 "\n"))

;; Canonical digest over the sorted, de-duplicated selected paths.
(define (selected-paths-digest paths)
  (define canonical (string-join (remove-duplicates (sort paths string<?)) "\n"))
  (bytes->hex-string (sha256 (string->bytes/utf-8 canonical))))

(define (v124-report-paths)
  (values (build-path base-dir "docs" "reports" "TEST-GATE-OWNERSHIP-v1.00.24.md")
          (build-path base-dir "docs" "reports" "TEST-GATE-OWNERSHIP-v1.00.24.json")
          (build-path base-dir "docs" "reports" "TEST-RETIER-LEDGER-v1.00.24.rktd")))

(define (v124-meta-str v)
  (cond
    [(list? v) (string-join (map (lambda (x) (format "~a" x)) v) ",")]
    [(vector? v) (string-join (map (lambda (x) (format "~a" x)) (vector->list v)) ",")]
    [else (format "~a" v)]))

(define (v124-member-meta f)
  (define m
    (with-handlers ([exn:fail? (lambda (_) (hash))])
      (get-file-metadata f)))
  (hasheq 'file
          f
          'suite
          (v124-meta-str (hash-ref m 'suite 'unset))
          'speed
          (v124-meta-str (hash-ref m 'speed 'unset))
          'boundary
          (v124-meta-str (hash-ref m 'boundary '()))
          'mutates
          (v124-meta-str (hash-ref m 'mutates #f))
          'isolation
          (v124-meta-str (hash-ref m 'isolation 'unset))
          'requires
          (v124-meta-str (hash-ref m 'requires '()))
          'timeout
          (v124-meta-str (hash-ref m 'timeout 'unset))))

;; Enumerate, per behavior, the metadata boundary and declared side effects
;; of every member file (read from test metadata; no collection change).
(define (v124-members-meta rows)
  (for/list ([r (in-list rows)])
    (hasheq 'behavior-id
            (hash-ref r 'behavior-id)
            'members
            (map v124-member-meta (hash-ref r 'members '())))))

;; Per-gate membership counts + selected-path digests (deterministic walk).
(define (v124-gate-stats membership)
  (for/list ([g (in-list v124-gate-names)])
    (define fs (sort (remove-duplicates (hash-ref membership g '())) string<?))
    (hasheq 'gate g 'count (length fs) 'digest (selected-paths-digest fs))))

(define (run-gate-ownership-map #:md-out [md-out #f]
                                #:json-out [json-out #f]
                                #:ledger-out [ledger-out #f]
                                #:check? [check? #f])
  (define rows (gate-ownership-rows))
  (define membership (gate-membership))
  (define errors (gate-ownership-errors rows membership))
  (define gate-stats (v124-gate-stats membership))
  (define selected-digest
    (selected-paths-digest (remove-duplicates (append-map (lambda (g) (hash-ref membership g '()))
                                                          v124-gate-names))))
  (define members-meta (v124-members-meta rows))
  (define-values (default-md default-json default-ledger) (v124-report-paths))
  (define md-path (or md-out default-md))
  (define json-path (or json-out default-json))
  (define ledger-path (or ledger-out default-ledger))
  (define md-bytes
    (string->bytes/utf-8 (gate-ownership-markdown rows gate-stats selected-digest members-meta)))
  (define json-bytes
    (string->bytes/utf-8 (gate-ownership-json rows gate-stats selected-digest members-meta)))
  (define ledger-bytes (string->bytes/utf-8 (gate-ownership-ledger-text rows)))
  (printf ";; TEST-GATE-OWNERSHIP MAP (v1.00.24 W0)~n")
  (printf ";; ═════════════════════════════════════════~n")
  (for ([r (in-list rows)])
    (printf ";; ~a: ~a -> ~a (~a member(s), owner ~a)~n"
            (hash-ref r 'behavior-id)
            (hash-ref r 'source-gate)
            (hash-ref r 'destination-gate)
            (length (hash-ref r 'members))
            (hash-ref r 'owner)))
  (printf ";; behaviors: ~a, validation errors: ~a~n" (length rows) (length errors))
  (for ([e (in-list errors)])
    (printf ";; ERROR: ~a~n" e))
  (define check-errors
    (if (not check?)
        '()
        (for/list ([pair (in-list (list (cons md-path md-bytes)
                                        (cons json-path json-bytes)
                                        (cons ledger-path ledger-bytes)))]
                   #:when (let ([on-disk (with-handlers ([exn:fail? (lambda (_) #f)])
                                           (file->bytes (car pair)))])
                            (or (not on-disk) (not (equal? on-disk (cdr pair))))))
          (format "~a: byte drift (or missing) vs regeneration" (car pair)))))
  (when check?
    (for ([e (in-list check-errors)])
      (printf ";; CHECK: ~a~n" e)))
  (unless check?
    (for ([pair (in-list (list (cons md-path md-bytes)
                               (cons json-path json-bytes)
                               (cons ledger-path ledger-bytes)))])
      (ensure-parent-dir! (car pair))
      (call-with-output-file (car pair)
                             (lambda (out) (write-bytes (cdr pair) out))
                             #:exists 'truncate/replace)
      (printf ";; wrote ~a~n" (car pair))))
  (hasheq 'behaviors
          (length rows)
          'errors
          errors
          'check-errors
          check-errors
          'md
          md-path
          'json
          json-path
          'ledger
          ledger-path))

(define (inventory-usage)
  (displayln "usage: racket scripts/run-tests/inventory.rkt MODE [--json-out PATH] [--md-out PATH]")
  (displayln "  MODE is one of:")
  (displayln "    --metadata-quality   metadata tag quality report (missing/invalid/explicit)")
  (displayln "    --unit-fast-audit    unit-fast grouped-execution eligibility audit")
  (displayln "    --ownership-map      production-area test ownership map (md + json)")
  (displayln "    --gate-ownership-map v1.00.24 W0 behavior ownership map + retier ledger")
  (displayln "      [--ledger PATH]    retier ledger output path (default docs/reports)")
  (displayln "      [--check]          verify existing artifacts byte-identically, no rewrite"))

(define (inventory-main argv)
  (define json-out #f)
  (define md-out #f)
  (define ledger-out #f)
  (define check? #f)
  (define mode #f)
  (let loop ([rest argv])
    (match rest
      ['() (void)]
      [(or (list "--help" _) (list "-h" _))
       (inventory-usage)
       (exit 0)]
      [(list "--metadata-quality" rest ...)
       (set! mode 'metadata-quality)
       (loop rest)]
      [(list "--unit-fast-audit" rest ...)
       (set! mode 'unit-fast-audit)
       (loop rest)]
      [(list "--ownership-map" rest ...)
       (set! mode 'ownership-map)
       (loop rest)]
      [(list "--gate-ownership-map" rest ...)
       (set! mode 'gate-ownership-map)
       (loop rest)]
      [(list "--ledger" p rest ...)
       (set! ledger-out p)
       (loop rest)]
      [(list "--check" rest ...)
       (set! check? #t)
       (loop rest)]
      [(list "--json-out" p rest ...)
       (set! json-out p)
       (loop rest)]
      [(list "--md-out" p rest ...)
       (set! md-out p)
       (loop rest)]
      [(list other rest ...)
       (printf "unknown argument: ~a~n" other)
       (inventory-usage)
       (exit 2)]))
  (case mode
    [(metadata-quality) (run-metadata-quality-report #:json-out json-out)]
    [(unit-fast-audit) (run-unit-fast-audit #:json-out json-out)]
    [(ownership-map) (run-ownership-map #:md-out md-out #:json-out json-out)]
    [(gate-ownership-map)
     (define result
       (run-gate-ownership-map #:md-out md-out
                               #:json-out json-out
                               #:ledger-out ledger-out
                               #:check? check?))
     (exit (if (or (pair? (hash-ref result 'errors '())) (pair? (hash-ref result 'check-errors '())))
               1
               0))]
    [else
     (inventory-usage)
     (exit 2)]))

(define invoked-directly?
  (let ([run-file (find-system-path 'run-file)])
    (and (path? run-file)
         (let ([base (file-name-from-path run-file)])
           (and base (equal? (path->string base) "inventory.rkt"))))))

(when invoked-directly?
  (inventory-main (vector->list (current-command-line-arguments))))
