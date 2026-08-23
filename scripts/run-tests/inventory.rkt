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
         run-ownership-map)

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

(define (compute-inventory-hash files)
  (define content (string-join (sort files string<?)))
  (define bytes (string->bytes/utf-8 content))
  (format "~x" (equal-hash-code bytes)))

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

(define (inventory-usage)
  (displayln "usage: racket scripts/run-tests/inventory.rkt MODE [--json-out PATH] [--md-out PATH]")
  (displayln "  MODE is one of:")
  (displayln "    --metadata-quality   metadata tag quality report (missing/invalid/explicit)")
  (displayln "    --unit-fast-audit    unit-fast grouped-execution eligibility audit")
  (displayln "    --ownership-map      production-area test ownership map (md + json)"))

(define (inventory-main argv)
  (define json-out #f)
  (define md-out #f)
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
