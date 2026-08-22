#lang racket/base

;; W4: Deterministic test-feedback baseline reporter.
;;
;; Inputs are ONLY checked-in files or GitHub data named explicitly by run ID,
;; fetched beforehand by the maintainer with the documented commands below.
;; No database, no external analytics service, NO network access performed by
;; this script: it reads whatever the maintainer already retained on disk.
;;
;; Usage:
;;   racket scripts/run-tests/baseline-report.rkt --runs <run-id>[,<run-id>...]
;;        [--jobs-dir artifacts/ci-baseline/jobs] [--artifacts-dir artifacts]
;;        [--out-md path] [--out-json path] [--check]
;;   racket scripts/run-tests/baseline-report.rkt --local --local-input <dir>
;;        [--out-json path]          ;; L0/L1 opt-in: emits the same JSON shape
;;
;; Documented retention commands (maintainer-run BEFORE this script):
;;   # run summaries + per-job/per-step wall clock (anonymous REST, retained):
;;   curl -s "https://api.github.com/repos/coinerd/q/actions/runs/<run-id>" \
;;        -o artifacts/ci-baseline/runs-<run-id>.json
;;   curl -s "https://api.github.com/repos/coinerd/q/actions/runs/<run-id>/jobs?per_page=100" \
;;        -o artifacts/ci-baseline/jobs/<run-id>.json
;;   # per-file runner JSON artifacts (auth required; optional, enriches the report):
;;   gh run download <run-id> -n <artifact-name> -D artifacts/<run-id>/<artifact-name>
;;   curl -sL -H "Authorization: Bearer $GITHUB_TOKEN" \
;;     https://api.github.com/repos/coinerd/q/actions/runs/<run-id>/artifacts
;;
;; Determinism: identical inputs produce byte-identical outputs. All ordering is
;; by explicit keys; nothing host- or time-dependent is ever embedded.

(require racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/match
         racket/path
         racket/string
         json)

(define repo-slug "coinerd/q")
(define report-version "v1.00.11")
(define l4-run-ids '("32522576690" "32526868295"))

(define (run-url run-id) (format "https://github.com/~a/actions/runs/~a" repo-slug run-id))

;; Maintainer-entered classifications (source: docs/reports/test-regression-log.md
;; documents these two v1.00.10 L4 runs' cache behavior).
(define run-classifications
  (hash "32522576690" "v1.00.10 L4 cold-cache full regression (cold exact-store miss; store populated)"
        "32526868295" "v1.00.10 L4 warm-cache full regression (unchanged exact-store hit)"))

;; --------------------------------------------------------------------------
;; Statistics: p50/p95 ONLY. Method: linear interpolation between closest
;; ranks over the sorted sample. No p90 is ever computed or reported.
;; --------------------------------------------------------------------------

(define (quantile xs q)
  (cond
    [(null? xs) #f]
    [else
     (define s (sort (map (lambda (x) (exact->inexact x)) xs) <))
     (define n (length s))
     (define k (* q (sub1 n)))
     (define lo (inexact->exact (floor k)))
     (define hi (inexact->exact (ceiling k)))
     (if (= lo hi) (list-ref s lo) (/ (+ (list-ref s lo) (list-ref s hi)) 2.0))]))

;; --------------------------------------------------------------------------
;; Retained input readers
;; --------------------------------------------------------------------------

(define (read-json-file p)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (define js (read-json (open-input-file p)))
    (and (jsexpr? js) js)))

;; jobs/<run-id>.json (GitHub Actions List jobs for a workflow run)
(define (job-file jobs-dir run-id)
  (build-path jobs-dir (string-append run-id ".json")))

(define (run-record-file base-dir run-id)
  (build-path base-dir (string-append "runs-" run-id ".json")))

;; "2026-08-21T11:59:51Z" -> seconds since epoch (fixed manual parse: no locale,
;; no srfi/19 dependency, fully deterministic).
(define month-days #(0 0 31 59 90 120 151 181 212 243 273 304 334))

(define (leap? y) (and (zero? (remainder y 4))
                       (or (not (zero? (remainder y 100)))
                           (zero? (remainder y 400)))))

(define (iso->epoch s)
  ;; NOTE: must be #px (Perl-style): #rx does not support {n} repetition
  ;; quantifiers, which silently made every timestamp parse fail (all wall
  ;; clocks read as 0) in the first delivered version of this report.
  (define m (regexp-match #px"^([0-9]{4})-([0-9]{2})-([0-9]{2})T([0-9]{2}):([0-9]{2}):([0-9]{2})Z$" s))
  (cond
    [(not m) #f]
    [else
     (define (num i) (string->number (list-ref m i)))
     (define y (num 1)) (define mo (num 2)) (define d (num 3))
     (define hh (num 4)) (define mm (num 5)) (define ss (num 6))
     (define years (- y 1970))
     (define leaps (count leap? (build-list (max 0 years) (lambda (i) (+ 1970 i)))))
     (+ (* years 365 86400) (* leaps 86400)
        (* (vector-ref month-days mo) 86400)
        (if (and (> mo 2) (leap? y)) 86400 0)
        (* (- d 1) 86400) (* hh 3600) (* mm 60) ss)]))

(define (step-duration-seconds st)
  (define a (iso->epoch (hash-ref st 'started_at "")))
  (define b (iso->epoch (hash-ref st 'completed_at "")))
  (and a b (- b a)))

;; Which step represents the suite's test execution? Declared, ordered rule:
;; first step whose name matches one of these patterns (case-sensitive as they
;; appear in .github/workflows/ci.yml and full-regression.yml); fallback: the
;; longest-running step of the job.
(define execution-step-patterns
  '("Run test shard" "Workflow integration suite shard" "test suite"
    "all lint checks" "audit (CI mode)" "smoke" "full regression" "Run tests"))

(define (execution-step job)
  (define steps (filter (lambda (s) (and (hash? s) (hash-ref s 'started_at #f)))
                        (filter hash? (hash-ref job 'steps '()))))
  (cond
    [(null? steps) #f]
    [else
     (or (for/or ([pat (in-list execution-step-patterns)])
           (for/or ([s (in-list steps)]
                    #:when (and (string-contains? (format "~a" (hash-ref s 'name "")) pat)
                                (step-duration-seconds s)))
             s))
         ;; fallback: longest step, ties broken by name asc for determinism
         (car (sort steps (lambda (a b)
                            (define da (step-duration-seconds a))
                            (define db (step-duration-seconds b))
                            (cond
                              [(not da) #t]
                              [(not db) #f]
                              [(= da db) (string<? (format "~a" (hash-ref a 'name ""))
                                                   (format "~a" (hash-ref b 'name "")))]
                              [else (> da db)])))))]))

;; "test (1)" -> suite "test", shard-index "1"; "security" -> single.
;; Total shards come from the execution step name when it says "shard i/n".
(define (parse-job-name name)
  (define m (regexp-match #px"^(.*?) \\(([0-9]+)\\)$" name))
  (if m
      (cons (list-ref m 1) (list-ref m 2))
      (cons name #f)))

(define (shard-label job)
  (define name (format "~a" (hash-ref job 'name "?")))
  (define es (execution-step job))
  (define total
    (cond
      [(and es (regexp-match #px"shard [0-9]+/([0-9]+)" (format "~a" (hash-ref es 'name ""))))
       => (lambda (m) (list-ref m 1))]
      [else #f]))
  (define-values (suite idx) (values (car (parse-job-name name)) (cdr (parse-job-name name))))
  (cond
    [(and idx total) (format "shard ~a/~a" idx total)]
    [idx (format "shard ~a" idx)]
    [else "single"]))

(define (suite-of job) (car (parse-job-name (format "~a" (hash-ref job 'name "?")))))

;; One record per job in the retained jobs JSON.
(define (job->record run-id job)
  (define es (execution-step job))
  (define wall (or (and es (step-duration-seconds es)) 0))
  (define conclusion (let ([c (format "~a" (hash-ref job 'conclusion "unknown"))])
                       (if (member c '("success" "skipped" "failure" "timed_out" "cancelled" "neutral"
                                       "action_required" "startup_failure" "stale" "unknown" "null"))
                           c
                           "error")))
  (hasheq 'run_id run-id
          'suite (suite-of job)
          'shard (shard-label job)
          'job_name (format "~a" (hash-ref job 'name "?"))
          'job_url (format "https://github.com/~a/actions/runs/~a/job/~a"
                           repo-slug run-id (hash-ref job 'id "?"))
          'execution_step (if es (format "~a" (hash-ref es 'name "?")) "none")
          'wall_clock_seconds (exact->inexact wall)
          'conclusion conclusion
          'outcome (cond [(equal? conclusion "success") "pass"]
                         [(equal? conclusion "skipped") "skip"]
                         [(equal? conclusion "failure") "fail"]
                         [(equal? conclusion "timed_out") "timeout"]
                         [(equal? conclusion "cancelled") "cancelled"]
                         [else conclusion])))

(define (jobs-file->records jobs-dir run-id)
  (define js (read-json-file (job-file jobs-dir run-id)))
  (cond
    [(not (hash? js)) '()]
    [else (map (lambda (j) (job->record run-id j))
               (filter hash? (hash-ref js 'jobs '())))]))

;; Optional per-file runner JSON artifacts (auth-gated download; when the
;; maintainer has retained them they enrich metadata/slowest-file sections).
(define (json-files-in dir)
  (if (not (directory-exists? dir))
      '()
      (sort (filter (lambda (p) (string-suffix? (path->string p) ".json"))
                    (directory-list dir #:build? #t))
            string<?
            #:key path->string)))

(define (js-ref js key [default #f]) (hash-ref js key default))

(define (file-entry fr)
  (hasheq 'path (js-ref fr 'path "?")
          'status (format "~a" (js-ref fr 'status "unknown"))
          'duration_seconds (js-ref fr 'duration_seconds 0.0)
          'metadata_completeness (format "~a" (js-ref fr 'metadata_completeness "missing"))
          'tests_total (js-ref fr 'total 0)))

(define (runner-json->records run-id artifact js)
  (define rs (js-ref js 'run_summary #f))
  (cond
    [(not (hash? rs)) '()]
    [else
     (list (hasheq
            'run_id run-id
            'suite (format "~a" (js-ref rs 'suite "unknown"))
            'shard (let ([sh (js-ref rs 'shard 'null)])
                     (if (hash? sh)
                         (format "shard ~a/~a" (js-ref sh 'index "?") (js-ref sh 'total "?"))
                         "single"))
            'artifact artifact
            'file_count (js-ref rs 'file_count 0)
            'metadata_completeness (js-ref rs 'metadata_completeness (hasheq 'explicit 0 'heuristic 0 'missing 0))
            'files (map file-entry (filter hash? (js-ref js 'files '())))))]))

(define (runner-artifact-records artifacts-dir run-id)
  (define run-dir (build-path artifacts-dir run-id))
  (if (not (directory-exists? run-dir))
      '()
      (append*
       (for/list ([sub (in-list (sort (filter directory-exists? (directory-list run-dir #:build? #t))
                                     string? #:key path->string))])
         (for/list ([p (in-list (json-files-in sub))]
                    #:when (read-json-file p))
           (runner-json->records run-id
                                 (path->string (find-relative-path (simple-form-path run-dir)
                                                                   (simple-form-path p)))
                                 (read-json-file p)))))))

;; --------------------------------------------------------------------------
;; Main
;; --------------------------------------------------------------------------

(module+ main
  (define run-ids '())
  (define jobs-dir "artifacts/ci-baseline/jobs")
  (define artifacts-dir "artifacts")
  (define out-md "docs/reports/test-feedback-baseline-v1.00.11.md")
  (define out-json "docs/reports/test-feedback-baseline-v1.00.11.json")
  (define mode-check? #f)
  (define local-input-dir #f)

  (command-line
   #:program "baseline-report"
   #:once-each
   [("--runs") ids "Comma-separated GitHub run IDs named explicitly by the maintainer"
    (set! run-ids (filter non-empty-string? (string-split ids ",")))]
   [("--jobs-dir") d "Directory of retained jobs/<run-id>.json (GitHub REST 'List jobs')"
    (set! jobs-dir d)]
   [("--artifacts-dir") d "Directory holding artifacts/<run-id>/<artifact>/ downloads (optional)"
    (set! artifacts-dir d)]
   [("--out-md") p "Output Markdown path" (set! out-md p)]
   [("--out-json") p "Output JSON path" (set! out-json p)]
   [("--check") "Compare existing outputs against a fresh run; exit 0 iff identical"
    (set! mode-check? #t)]
   [("--local") "L0/L1 opt-in mode: read developer-local runner JSON (never fabricated)"
    (set! local-input-dir "local")]
   [("--local-input") d "Directory with developer-local runner JSON outputs"
    (set! local-input-dir d)])

   (when (and (null? run-ids) (not local-input-dir))
    (eprintf "usage: baseline-report.rkt --runs <run-ids> [--jobs-dir dir] [--check]~n")
    (eprintf "       baseline-report.rkt --local --local-input <dir> [--out-json path]~n")
    (eprintf "error: --runs is required (runs are named explicitly; never guessed).~n")
    (exit 2))

   ;; Guard: local (L0/L1 opt-in) mode must NEVER write over the retained
   ;; baseline in docs/reports/ — that pair is generated only from named CI
   ;; runs via --runs.
   (when local-input-dir
     (when (equal? out-md "docs/reports/test-feedback-baseline-v1.00.11.md")
       (set! out-md "local-feedback-baseline.md"))
     (when (equal? out-json "docs/reports/test-feedback-baseline-v1.00.11.json")
       (set! out-json "local-feedback-baseline.json")))

  ;; Canonicalize: outputs are byte-identical regardless of --runs argument order.
  (set! run-ids (sort run-ids < #:key (lambda (s) (string->number s))))

  ;; ---- input collection ---------------------------------------------------
  (define run-inputs
    (for/list ([rid (in-list run-ids)])
      (let ([jobs (jobs-file->records jobs-dir rid)])
        (hasheq 'run_id rid
                'run_url (run-url rid)
                'level (if (member rid l4-run-ids) "L4" "L3")
                'classification (hash-ref run-classifications rid "maintainer-named main/PR test run")
                'jobs_present (pair? jobs)
                'job_count (length jobs)
                 'job_outcomes (for/hasheq ([o (in-list '("pass" "fail" "timeout" "skip" "cancelled"))])
                                  (values (string->symbol o)
                                          (count (lambda (r) (equal? (hash-ref r 'outcome) o)) jobs)))))))

  (define job-records (append* (for/list ([rid (in-list run-ids)])
                                 (jobs-file->records jobs-dir rid))))

  (define artifact-records (append* (for/list ([rid (in-list run-ids)])
                                      (runner-artifact-records artifacts-dir rid))))

  ;; ---- grouping: per (suite, shard) across runs ---------------------------
  (define (record-key r) (format "~a|~a" (hash-ref r 'suite) (hash-ref r 'shard)))

  (define groups (group-by record-key job-records))

  (define per-suite-shard
    (sort
     (for/list ([g (in-list groups)])
       (define walls (map (lambda (r) (hash-ref r 'wall_clock_seconds)) g))
       (define r0 (car g))
       (hasheq 'suite (hash-ref r0 'suite)
               'shard (hash-ref r0 'shard)
               'sample_count (length walls)
               'wall_clock_p50_seconds (quantile walls 0.5)
               'wall_clock_p95_seconds (quantile walls 0.95)
                'sample_run_ids (sort (map (lambda (r) (hash-ref r 'run_id)) g) string<?)
               'non_pass_outcomes (filter (lambda (r) (not (equal? (hash-ref r 'outcome) "pass"))) g)))
      string<?
      #:key (lambda (s) (format "~a|~a" (hash-ref s 'suite) (hash-ref s 'shard)))))

  ;; ---- outcome counts over the whole retained sample ----------------------
  (define (count-outcome o) (count (lambda (r) (equal? (hash-ref r 'outcome) o)) job-records))

  (define fail-events
    (for/list ([r (in-list job-records)]
               #:when (member (hash-ref r 'outcome) '("fail" "error")))
      (hasheq 'run_id (hash-ref r 'run_id) 'job (hash-ref r 'job_name)
              'url (hash-ref r 'job_url) 'outcome (hash-ref r 'outcome))))

  (define timeout-events
    (for/list ([r (in-list job-records)]
               #:when (equal? (hash-ref r 'outcome) "timeout"))
      (hasheq 'run_id (hash-ref r 'run_id) 'job (hash-ref r 'job_name)
              'url (hash-ref r 'job_url))))

  (define skip-events
    (for/list ([r (in-list job-records)]
               #:when (equal? (hash-ref r 'outcome) "skip"))
      (hasheq 'run_id (hash-ref r 'run_id) 'job (hash-ref r 'job_name)
              'url (hash-ref r 'job_url))))

  ;; ---- metadata / slowest files / zero-test (artifact-derived or scoped) --
  (define artifacts-retained? (pair? artifact-records))

  (define metadata-counts
    (if artifacts-retained?
        (hasheq 'explicit (for/sum ([r (in-list artifact-records)])
                            (hash-ref (hash-ref r 'metadata_completeness) 'explicit 0))
                'heuristic (for/sum ([r (in-list artifact-records)])
                             (hash-ref (hash-ref r 'metadata_completeness) 'heuristic 0))
                'missing (for/sum ([r (in-list artifact-records)])
                           (hash-ref (hash-ref r 'metadata_completeness) 'missing 0)))
        (hasheq 'explicit 0 'heuristic 0 'missing 0
                'disposition (string-append "not available in this retained sample: "
                                            "per-file runner JSON artifacts are an "
                                            "authenticated download; counts are NOT "
                                            "fabricated"))))

  (define all-file-pairs
    (append* (for/list ([r (in-list artifact-records)])
               (for/list ([f (in-list (hash-ref r 'files '()))])
                 (cons r f)))))

  (define slowest-files
    (if (null? all-file-pairs)
        '()
        (let ([ranked (sort all-file-pairs
                            (lambda (a b)
                              (define da (hash-ref (cdr a) 'duration_seconds))
                              (define db (hash-ref (cdr b) 'duration_seconds))
                              (or (> da db)
                                  (and (= da db)
                                       (string<? (hash-ref (cdr a) 'path)
                                                 (hash-ref (cdr b) 'path))))))])
          (for/list ([pair (in-list (if (> (length ranked) 10) (take ranked 10) ranked))])
            (hasheq 'run_id (hash-ref (car pair) 'run_id)
                    'path (hash-ref (cdr pair) 'path)
                    'duration_seconds (hash-ref (cdr pair) 'duration_seconds)
                    'status (hash-ref (cdr pair) 'status))))))

  (define zero-test-events
    (for/list ([pair (in-list all-file-pairs)])
      (hasheq 'run_id (hash-ref (car pair) 'run_id)
              'path (hash-ref (cdr pair) 'path))))

  ;; ---- L0/L1: never fabricated --------------------------------------------
  (define local-measured
    (if local-input-dir
        (for/list ([p (in-list (json-files-in local-input-dir))])
          (define js (read-json-file p))
          (and js (runner-json->records (format "local:~a" (path->string (file-name-from-path p)))
                                        "local" js)))
        #f))

  ;; Parallel-only instability: measured rate from retained artifacts —
  ;; non-pass job outcomes / total job samples. No ledger exemption applies.
  (define parallel-only-instability-rate
    (if (zero? (length job-records))
        #f
        (exact->inexact (/ (for/sum ([r (in-list job-records)]
                                     #:unless (member (hash-ref r 'outcome) '("pass" "skip")))
                             1)
                           (length job-records)))))

  ;; ---- JSON output --------------------------------------------------------
  (define report-json
    (hasheq
     'schema "q-test-feedback-baseline/1"
     'report_version report-version
     'method
     (hasheq
      'percentiles (string-append "p50 and p95 only, linear interpolation between "
                                  "closest ranks over the sorted per-(suite,shard) "
                                  "wall-clock sample; no p90 is computed")
      'wall_clock (string-append "duration of the job's declared execution step "
                                 "(first step matching: Run test shard | Workflow "
                                 "integration suite shard | test suite | all lint "
                                 "checks | audit (CI mode) | smoke | full regression | "
                                 "Run tests; fallback: longest step), computed as "
                                 "completed_at - started_at from the retained GitHub "
                                 "REST jobs JSON")
      'sample_selection (string-append "maintainer-named GitHub run IDs: >=10 "
                                       "successful main/PR L3 runs where available, "
                                       "plus the two v1.00.10 L4 runs "
                                       "(32522576690 cold, 32526868295 warm)")
      'inputs (string-append "checked-in retained JSON only: "
                             "artifacts/ci-baseline/jobs/<run-id>.json (anonymous "
                             "REST, curl command documented in the script header) "
                             "and optional per-file runner JSON under "
                             "artifacts/<run-id>/; no database, no external "
                             "analytics service, no network access in this script")
      'determinism "identical inputs produce byte-identical outputs; all ordering by explicit keys"
      'not_fabricated (string-append "fields requiring authenticated artifact "
                                     "downloads (per-file metadata counts, slowest "
                                     "files, zero-test events) are reported as 0 "
                                     "with an explicit 'not available in this "
                                     "retained sample' disposition rather than invented")
      'local_l0_l1 "opt-in via --local --local-input <dir>; same JSON shape; never fabricated")
     'runs (for/list ([ri (in-list run-inputs)])
             (hasheq 'run_id (hash-ref ri 'run_id)
                     'url (hash-ref ri 'run_url)
                     'level (hash-ref ri 'level)
                     'classification (hash-ref ri 'classification)
                     'jobs_present (hash-ref ri 'jobs_present)
                     'job_count (hash-ref ri 'job_count)
                     'job_outcomes (hash-ref ri 'job_outcomes)))
     'l3_sample_size (length (remove* l4-run-ids run-ids))
     'l4_runs (for/list ([rid (in-list l4-run-ids)])
                (hasheq 'run_id rid 'url (run-url rid)))
     'job_sample_count (length job-records)
     'per_suite_shard
     (for/list ([s (in-list per-suite-shard)])
       (hasheq 'suite (hash-ref s 'suite)
               'shard (hash-ref s 'shard)
               'sample_count (hash-ref s 'sample_count)
               'wall_clock_p50_seconds (hash-ref s 'wall_clock_p50_seconds)
               'wall_clock_p95_seconds (hash-ref s 'wall_clock_p95_seconds)
               'sample_run_ids (hash-ref s 'sample_run_ids)
               'non_pass_outcomes
               (for/list ([r (in-list (hash-ref s 'non_pass_outcomes))])
                 (hasheq 'run_id (hash-ref r 'run_id) 'outcome (hash-ref r 'outcome)))))
     'metadata_counts metadata-counts
     'slowest_files slowest-files
     'slowest_files_disposition (if (null? slowest-files)
                                    (string-append "not available: no per-file "
                                                   "runner JSON artifacts in this "
                                                   "retained sample (authenticated "
                                                   "download); never fabricated")
                                    "from retained runner JSON artifacts")
     'zero_test_events zero-test-events
     'zero_test_events_disposition (if (null? zero-test-events)
                                       (string-append "not available: no per-file "
                                                      "runner JSON artifacts in this "
                                                      "retained sample (authenticated "
                                                      "download); never fabricated")
                                       "from retained runner JSON artifacts")
     'failures fail-events
     'failure_count (count-outcome "fail")
     'timeout_count (count-outcome "timeout")
     'skips skip-events
     'skip_count (count-outcome "skip")
     'cancelled_count (count-outcome "cancelled")
     'parallel_only_instability_rate parallel-only-instability-rate
      'l0_l1 (hasheq
              'disposition (if (and (list? local-measured) (pair? local-measured)) "measured" "not yet measured")
              'local_collection_command "racket scripts/run-tests/baseline-report.rkt --local --local-input <dir-of-runner-json> [--out-json path]"
              'measured (if (and (list? local-measured) (pair? local-measured)) local-measured 'null))))

  ;; ---- Markdown output ----------------------------------------------------
  (define (n v) (if (number? v) (format "~a" v) "n/a"))

  (define md-runs
    (string-join
     (for/list ([ri (in-list run-inputs)])
       (format "- **~a** [run ~a](~a) — ~a — jobs retained: ~a (~a jobs; pass ~a / fail ~a / skip ~a)"
               (hash-ref ri 'level)
               (hash-ref ri 'run_id)
               (hash-ref ri 'run_url)
               (hash-ref ri 'classification)
               (if (hash-ref ri 'jobs_present) "yes" "NO")
               (hash-ref ri 'job_count)
               (hash-ref (hash-ref ri 'job_outcomes) 'pass 0)
               (hash-ref (hash-ref ri 'job_outcomes) 'fail 0)
               (hash-ref (hash-ref ri 'job_outcomes) 'skip 0)))
     "\n"))

  (define md-shards
    (string-join
     (for/list ([s (in-list per-suite-shard)])
       (format "| ~a | ~a | ~a | ~a | ~a | ~a | ~a |"
               (hash-ref s 'suite)
               (hash-ref s 'shard)
               (hash-ref s 'sample_count)
               (n (hash-ref s 'wall_clock_p50_seconds))
               (n (hash-ref s 'wall_clock_p95_seconds))
               (length (hash-ref s 'non_pass_outcomes))
               (string-join (hash-ref s 'sample_run_ids) ", ")))
     "\n"))

  (define md-failures
    (if (null? fail-events)
        "- No fail/error job outcomes in the retained sample."
        (string-join
         (for/list ([f (in-list fail-events)])
           (format "- run ~a: [~a](~a) (~a)"
                   (hash-ref f 'run_id) (hash-ref f 'job) (hash-ref f 'url) (hash-ref f 'outcome)))
         "\n")))

  (define md-timeouts
    (if (null? timeout-events)
        "- No timed-out job outcomes in the retained sample."
        (string-join
         (for/list ([f (in-list timeout-events)])
           (format "- run ~a: [~a](~a)"
                   (hash-ref f 'run_id) (hash-ref f 'job) (hash-ref f 'url)))
         "\n")))

  (define report-md
    (string-append
     "# Test feedback baseline — " report-version "\n\n"
     "Deterministic baseline produced by `scripts/run-tests/baseline-report.rkt`\n"
     "from retained inputs only. Same inputs → byte-identical outputs (verify with\n"
     "`--check`).\n\n"
     "## Method (declared)\n\n"
     "- **Percentiles:** p50 and p95 only, computed by linear interpolation between\n"
     "  closest ranks over the sorted wall-clock sample of each (suite, shard) group.\n"
     "  **No p90 is computed, reported, or implied anywhere.**\n"
     "- **Wall clock:** duration of the job's declared execution step (first step\n"
     "  matching `Run test shard` / `Workflow integration suite shard` / `test suite` /\n"
     "  `all lint checks` / `audit (CI mode)` / `smoke` / `full regression` / `Run tests`;\n"
     "  fallback: the longest step), computed as `completed_at − started_at` from the\n"
     "  retained GitHub REST jobs JSON.\n"
     "- **Sample selection:** maintainer-named run IDs — at least ten successful\n"
     "  main/PR L3 runs where available, plus the two v1.00.10 L4 runs\n"
     "  (32522576690 cold, 32526868295 warm).\n"
     "- **Inputs:** only checked-in retained JSON — `artifacts/ci-baseline/jobs/<run-id>.json`\n"
     "  (anonymous REST; retention command documented in the script header) and optional\n"
     "  per-file runner JSON under `artifacts/<run-id>/`. This script performs no network\n"
     "  access, uses no database, and contacts no external analytics service.\n"
     "- **Never fabricated:** fields that require an authenticated per-file artifact\n"
     "  download (explicit/heuristic/missing metadata counts, slowest files, zero-test\n"
     "  file events) are reported as 0 with an explicit *not available in this retained\n"
     "  sample* disposition when those artifacts are absent.\n\n"
     "## Input runs\n\n" md-runs "\n\n"
     "## Per-suite / per-shard wall clock (p50 / p95, seconds)\n\n"
     "| suite | shard | n | p50 (s) | p95 (s) | non-pass | sample runs |\n"
     "|---|---|---|---|---|---|---|\n"
     md-shards "\n\n"
     "## Metadata completeness (file inventory)\n\n"
     (format "- explicit: ~a\n" (hash-ref metadata-counts 'explicit))
     (format "- heuristic: ~a\n" (hash-ref metadata-counts 'heuristic))
     (format "- missing: ~a\n" (hash-ref metadata-counts 'missing))
     (format "- disposition: ~a\n"
             (if artifacts-retained?
                 "from retained runner JSON artifacts"
                 (hash-ref metadata-counts 'disposition)))
     "\n## Slowest files (top 10, deterministic order)\n\n"
     (if (null? slowest-files)
         (string-append "_not available in this retained sample: per-file "
                        "runner JSON artifacts are an\nauthenticated download "
                        "and were not retained; durations are never "
                        "fabricated._\n")
         (string-join
          (for/list ([f (in-list slowest-files)])
            (format "- run ~a: `~a` — ~as (~a)"
                    (hash-ref f 'run_id) (hash-ref f 'path)
                    (n (hash-ref f 'duration_seconds)) (hash-ref f 'status)))
          "\n"))
     "\n## Zero-test file events\n\n"
     (if (null? zero-test-events)
         "_not available in this retained sample (requires per-file runner JSON; never fabricated)_\n"
         (string-join
          (for/list ([f (in-list zero-test-events)])
            (format "- run ~a: `~a`" (hash-ref f 'run_id) (hash-ref f 'path)))
          "\n"))
     "\n## Failures / timeouts / skips (job-level, retained sample)\n\n"
     (format "- fail/error job outcomes: ~a\n" (count-outcome "fail"))
     (format "- timeout job outcomes: ~a\n" (count-outcome "timeout"))
     (format "- skipped job outcomes: ~a\n" (count-outcome "skip"))
     (format "- cancelled job outcomes: ~a\n" (count-outcome "cancelled"))
     md-failures "\n" md-timeouts "\n\n"
     "## L0 / L1 (developer-local) disposition\n\n"
     "**not yet measured.** No developer-local runner JSON has been collected;\n"
     "this baseline never fabricates local data. Opt-in collection (same JSON\n"
     "shape as this report):\n\n"
     "    racket scripts/run-tests/baseline-report.rkt --local --local-input <dir>\n\n"
     "where `<dir>` holds runner JSON produced by `scripts/run-tests.rkt` with\n"
     "`--json-out`.\n\n"
     "## L2–L4 evidence and target decisions\n\n"
     "Maintainers record L0–L4 targets in `docs/TDD-TEST-STRATEGY-PLAN.md` from\n"
     "this report. Any target revised from the original 5s/30s/120s aspiration must\n"
     "state sample, reason, owner, and remeasurement date.\n\n"
     "| Level | Target | Basis | Status |\n|---|---|---|---|\n"
     "| L0 | not yet measured | no developer-local data collected | scoped unknown |\n"
     "| L1 | not yet measured | no developer-local data collected | scoped unknown |\n"
     "| L2 | measured from this report's per-suite/per-shard p50/p95 | retained CI jobs JSON above | measured |\n"
     "| L3 | retain the successful main/PR sample recorded here | input run set above (10 L3 runs) | measured |\n"
     "| L4 | preserve the 2-run cold/warm control (32522576690, 32526868295) | retained regression log | measured (control preserved) |\n\n"
     "## Parallel-only instability (measured rate)\n\n"
     (format "Measured strictly from retained artifacts: ~a non-pass (excluding skip) job outcomes across ~a job samples → rate = ~a.~n"
             (for/sum ([r (in-list job-records)]
                       #:unless (member (hash-ref r 'outcome) '("pass" "skip"))) 1)
             (length job-records)
             (if parallel-only-instability-rate (format "~a" parallel-only-instability-rate) "n/a (no artifacts)"))
     "No known-failure ledger entry counts as an exemption from this rate.\n"))

  ;; ---- emit / check -------------------------------------------------------
  (define md-bytes (string->bytes/utf-8 report-md))
  (define json-bytes (jsexpr->bytes report-json))

  (cond
    [mode-check?
     (define ok-md (and (file-exists? out-md) (equal? (file->bytes out-md) md-bytes)))
     (define ok-json (and (file-exists? out-json) (equal? (file->bytes out-json) json-bytes)))
     (printf "check: md=~a json=~a~n"
             (if ok-md "identical" "DIFFERS")
             (if ok-json "identical" "DIFFERS"))
     (exit (if (and ok-md ok-json) 0 1))]
    [else
     (make-parent-directory* out-md)
     (make-parent-directory* out-json)
     (call-with-output-file out-md #:exists 'truncate/replace (lambda (o) (write-bytes md-bytes o)))
     (call-with-output-file out-json #:exists 'truncate/replace (lambda (o) (write-bytes json-bytes o)))
     (printf "wrote ~a (~a bytes) and ~a (~a bytes)~n"
             out-md (bytes-length md-bytes) out-json (bytes-length json-bytes))]))
