#lang racket/base

;; scripts/benchmark/task.rkt — Benchmark task definition, loading, and validation
;;
;; Defines the enhanced task format for the live benchmark suite.
;; Each task specifies a prompt, scoring criteria, setup/teardown,
;; and constraints (time limit, iteration limit, tool expectations).

(require racket/file
         racket/format
         racket/list
         racket/match
         racket/path
         racket/port
         racket/string
         json)

(provide (struct-out benchmark-task)
         (struct-out scoring-spec)
         (struct-out task-setup-spec)
         (struct-out file-check)
         load-benchmark-task
         validate-benchmark-task
         task-setup
         task-teardown
         task-fixture-paths
         all-benchmark-tasks)

;; ============================================================
;; Structs
;; ============================================================

(struct file-check (path must-contain must-not-contain) #:transparent)

(struct scoring-spec
        (files-created ; (listof file-check?)
         files-not-modified ; (listof string?)
         tests-pass ; (listof string?) — paths to raco test
         tools-used ; (listof string?) — tool names that must appear
         tools-not-used ; (listof string?) — tool names that must NOT appear
         max-iterations ; (or/c exact-positive-integer? #f)
         custom-scorer) ; (or/c string? #f) — name of custom scorer module
  #:transparent)

(struct task-setup-spec
        (files ; (hash/c string? string?) — path -> content
         extensions ; (listof string?) — extension names to load
         skills) ; (listof string?) — skill names to inject
  #:transparent)

(struct benchmark-task
        (name category ; 'implementation | 'bug-fix | 'planning | 'full-workflow
              difficulty ; 1-3
              description
              prompt
              max-iterations
              max-tokens
              time-limit-seconds
              setup ; task-setup-spec?
              scoring ; scoring-spec?
              teardown ; (listof string?) — paths to clean up
              fixtures-dir) ; (or/c string? #f) — relative path to fixture dir
  #:transparent)

;; ============================================================
;; Loading
;; ============================================================

;; load-benchmark-task : path-string? -> benchmark-task?
(define (load-benchmark-task path)
  (define js (call-with-input-file path read-json))
  (validate-benchmark-task js))

;; normalize-keys : hash? -> hash?
;; Converts string keys to symbols. Leaves 'files' values as-is
;; (string keys are file paths).
(define (normalize-keys h)
  (for/hasheq ([(k v) (in-hash h)])
    (define sk
      (if (string? k)
          (string->symbol k)
          k))
    (define sv
      (cond
        [(and (eq? sk 'files) (hash? v)) v]
        [(and (eq? sk 'files_created) (list? v))
         (for/list ([item (in-list v)])
           (if (hash? item)
               (normalize-keys item)
               item))]
        [(hash? v) (normalize-keys v)]
        [(and (list? v) (andmap hash? v)) (map normalize-keys v)]
        [else v]))
    (values sk sv)))

;; validate-benchmark-task : hash? -> benchmark-task?
(define (validate-benchmark-task js)
  (unless (hash? js)
    (error 'validate-benchmark-task "Task must be a JSON object, got: ~a" js))

  ;; Normalize string keys from read-json to symbols
  (define j (normalize-keys js))

  ;; Required fields
  (define name (hash-ref j 'name #f))
  (define prompt (hash-ref j 'prompt #f))
  (unless (and name (string? name) (not (string=? name "")))
    (error 'validate-benchmark-task "Task must have a non-empty string 'name' field"))
  (unless (and prompt (string? prompt) (not (string=? prompt "")))
    (error 'validate-benchmark-task "Task must have a non-empty string 'prompt' field"))

  ;; Category
  (define raw-category (hash-ref j 'category "implementation"))
  (define category
    (if (symbol? raw-category)
        raw-category
        (string->symbol raw-category)))
  (unless (member category '(implementation bug-fix planning full-workflow))
    (error 'validate-benchmark-task
           "Category must be one of: implementation, bug-fix, planning, full-workflow, got: ~a"
           category))

  ;; Difficulty
  (define difficulty (hash-ref j 'difficulty 1))
  (unless (and (exact-positive-integer? difficulty) (<= difficulty 3))
    (error 'validate-benchmark-task "Difficulty must be 1, 2, or 3, got: ~a" difficulty))

  ;; Parse setup
  (define raw-setup (hash-ref j 'setup (hasheq)))
  (define setup-spec
    (task-setup-spec (hash-ref raw-setup 'files (hasheq))
                     (hash-ref raw-setup 'extensions '())
                     (hash-ref raw-setup 'skills '())))

  ;; Parse scoring
  (define raw-scoring (hash-ref j 'scoring (hasheq)))
  (define scoring-spec-val
    (scoring-spec (for/list ([fc (in-list (hash-ref raw-scoring 'files_created '()))])
                    (file-check (hash-ref fc 'path "")
                                (hash-ref fc 'must_contain '())
                                (hash-ref fc 'must_not_contain '())))
                  (hash-ref raw-scoring 'files_not_modified '())
                  (hash-ref raw-scoring 'tests_pass '())
                  (hash-ref raw-scoring 'tools_used '())
                  (hash-ref raw-scoring 'tools_not_used '())
                  (hash-ref raw-scoring 'max_iterations #f)
                  (hash-ref raw-scoring 'custom_scorer #f)))

  ;; Parse teardown
  (define teardown (hash-ref j 'teardown '()))
  (unless (and (list? teardown) (andmap string? teardown))
    (error 'validate-benchmark-task "teardown must be a list of strings, got: ~a" teardown))

  (benchmark-task name
                  category
                  difficulty
                  (hash-ref j 'description "")
                  prompt
                  (hash-ref j 'max_iterations 15)
                  (hash-ref j 'max_tokens 100000)
                  (hash-ref j 'time_limit_seconds 120)
                  setup-spec
                  scoring-spec-val
                  teardown
                  (hash-ref j 'fixtures_dir #f)))

;; ============================================================
;; Task setup / teardown
;; ============================================================

;; task-setup : benchmark-task? path-string? -> path?
;; Creates a temp project directory with fixture files and extension symlinks.
;; Returns the temp project directory path.
(define (task-setup task [base-dir #f])
  (define tmp-dir
    (if base-dir
        (make-temporary-file "benchmark-~a" 'directory base-dir)
        (make-temporary-file "benchmark-~a" 'directory)))
  (make-directory* tmp-dir)

  ;; Copy fixtures if fixtures-dir is specified
  (define fixtures-dir (benchmark-task-fixtures-dir task))
  (when fixtures-dir
    (define fixtures-path
      (if (absolute-path? fixtures-dir)
          (string->path fixtures-dir)
          (build-path (task-fixture-paths) fixtures-dir)))
    (cond
      [(directory-exists? fixtures-path)
       ;; Copy contents of fixtures dir into tmp-dir (which already exists)
       (for ([child (in-list (directory-list fixtures-path))])
         (define src (build-path fixtures-path child))
         (define dst (build-path tmp-dir child))
         (cond
           [(directory-exists? src) (copy-directory/files src dst)]
           [(file-exists? src) (copy-file src dst)]))]
      [(file-exists? fixtures-path)
       ;; Single fixture file — copy into tmp-dir preserving name
       (copy-file fixtures-path (build-path tmp-dir (file-name-from-path fixtures-path)))]))

  ;; Create setup files (overrides fixtures if overlapping)
  (define files (task-setup-spec-files (benchmark-task-setup task)))
  (for ([(rel-path content) (in-hash files)])
    (define rel-str
      (if (symbol? rel-path)
          (symbol->string rel-path)
          rel-path))
    (define full-path (build-path tmp-dir rel-str))
    (make-directory* (path-only full-path))
    (call-with-output-file full-path
                           (lambda (out)
                             (display (if (string? content)
                                          content
                                          (~a content))
                                      out))
                           #:exists 'truncate))

  ;; Create .q directory for project config
  (make-directory* (build-path tmp-dir ".q"))

  tmp-dir)

;; task-teardown : benchmark-task? path? boolean? -> void?
;; Removes the temp project directory. If keep? is true, does nothing.
(define (task-teardown task tmp-dir [keep? #f])
  (unless keep?
    (when (and tmp-dir (directory-exists? tmp-dir))
      (delete-directory/files tmp-dir))))

;; task-fixture-paths : -> path?
;; Returns the directory containing benchmark fixture files.
(define (task-fixture-paths)
  (build-path (path-only (find-system-path 'run-file)) "fixtures"))

;; ============================================================
;; Loading all tasks from a directory
;; ============================================================

;; all-benchmark-tasks : path-string? -> (listof benchmark-task?)
(define (all-benchmark-tasks task-dir)
  (for/list ([f (in-directory task-dir)]
             #:when (regexp-match? #rx"\\.json$" (path->string f)))
    (load-benchmark-task f)))
