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
         run-metadata-quality-report)

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

(define (inventory-main argv)
  (define json-out #f)
  (let loop ([rest argv])
    (match rest
      ['() (void)]
      [(or (list "--help" _) (list "-h" _))
       (displayln
        "usage: racket scripts/run-tests/inventory.rkt --metadata-quality [--json-out PATH]")
       (exit 0)]
      [(list "--metadata-quality" rest ...) (loop rest)]
      [(list "--json-out" p rest ...)
       (set! json-out p)
       (loop rest)]
      [(list other rest ...)
       (printf "unknown argument: ~a~n" other)
       (displayln
        "usage: racket scripts/run-tests/inventory.rkt --metadata-quality [--json-out PATH]")
       (exit 2)
       (loop rest)]))
  (when (member "--metadata-quality" argv)
    (run-metadata-quality-report #:json-out json-out))
  (unless (member "--metadata-quality" argv)
    (displayln "usage: racket scripts/run-tests/inventory.rkt --metadata-quality [--json-out PATH]")
    (exit 2)))

(define invoked-directly?
  (let ([run-file (find-system-path 'run-file)])
    (and (path? run-file)
         (let ([base (file-name-from-path run-file)])
           (and base (equal? (path->string base) "inventory.rkt"))))))

(when invoked-directly?
  (inventory-main (vector->list (current-command-line-arguments))))
