#lang racket/base

;; baseline.rkt — CL5 W3 F2: verified comprehensive baseline (evaluation only).
;;
;; Purpose. W3 needs a comprehensive reference that is PROVEN to have executed,
;; at an exact revision, with every required test producer and shard present and
;; every executed test file reconstructed and matched against the producer's own
;; plan. A green aggregate is not evidence on its own: it is derived from shard
;; artifacts and says nothing about which files ran.
;;
;; This module reads a real CI run (jobs + artifacts), enumerates the producers'
;; plans with the shipped tools at the pinned revision, reconstructs the executed
;; file sets from each producer's `test-results.json` artifact, and reports
;; `complete` only when every check holds. Missing jobs, missed shards, expired
;; or absent artifacts, count/summary mismatches, a different head SHA or any
;; setup-failed producer make the baseline `unavailable` — never an assumed pass.
;;
;; Producer registry is pre-registered below and pinned against ci.yml by a test:
;;   test (0..2)        ./qt --lane fast --shard-index i --shard-total 3
;;                      artifact test-results-fast-i, step "Run test shard i/3"
;;   test-platform       ./qt --lane platform
;;                      artifact test-results-platform, step "Run platform-cross suite"
;;   security            ./qt --lane security
;;                      artifact test-results-security, step "Run security test suite (parallel)"
;;   workflows (0..1)    ./qt --lane workflows --shard-index i --shard-total 2
;;                      artifact test-results-workflows-i, step "Workflow integration suite shard i/2"
;;   smoke               raco test tests/test-provider-smoke.rkt   (declared command, no artifact)
;;                      step "Provider compilation smoke tests"
;;
;; The smoke job does not upload a test-results artifact; its executed set is the
;; file named by the workflow's own command, which is a declaration, not an
;; inference from a filename pattern. The workflow text is pinned by a test.
;;
;; No production CI, workflow, retry, scheduler, admission or selection change.

(require json
         racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/match
         racket/path
         racket/string
         racket/system
         "ci-evidence.rkt"
         "selection-experiment.rkt"
         "w3-dry-run.rkt")

(provide baseline-producers
         producer-plan-command
         artifact-record
         baseline-checks
         complete?
         run-baseline
         verify-baseline-record)

;; ---- pre-registered producer registry -----------------------------------------------

(define (lane-shard name lane index total artifact step)
  (hash 'name
        name
        'kind
        'lane-shard
        'lane
        lane
        'shard-index
        index
        'shard-total
        total
        'artifact
        artifact
        'step-pattern
        step))

(define (lane-single name lane artifact step)
  (hash 'name
        name
        'kind
        'lane-single
        'lane
        lane
        'shard-index
        0
        'shard-total
        1
        'artifact
        artifact
        'step-pattern
        step))

(define smoke-command "raco test tests/test-provider-smoke.rkt")
(define smoke-files (list "tests/test-provider-smoke.rkt"))

(define baseline-producers
  (list
   (lane-shard "test (0)" "fast" 0 3 "test-results-fast-0" #px"^Run test shard 0/3$")
   (lane-shard "test (1)" "fast" 1 3 "test-results-fast-1" #px"^Run test shard 1/3$")
   (lane-shard "test (2)" "fast" 2 3 "test-results-fast-2" #px"^Run test shard 2/3$")
   (lane-single "test-platform" "platform" "test-results-platform" #px"^Run platform-cross suite$")
   (lane-single "security"
                "security"
                "test-results-security"
                #px"^Run security test suite \\(parallel\\)$")
   (lane-shard "workflows (0)"
               "workflows"
               0
               2
               "test-results-workflows-0"
               #px"^Workflow integration suite shard 0/2$")
   (lane-shard "workflows (1)"
               "workflows"
               1
               2
               "test-results-workflows-1"
               #px"^Workflow integration suite shard 1/2$")
   (hash 'name
         "smoke (ubuntu-latest)"
         'kind
         'declared-command
         'command
         smoke-command
         'files
         smoke-files
         'step-pattern
         #px"^Provider compilation smoke tests$")))

(define (lane-producer? spec)
  (memq (hash-ref spec 'kind) '(lane-shard lane-single)))

;; ---- plans (shipped tool output, never inferred) ------------------------------------

(define (producer-plan-command spec)
  (list "./qt"
        "--lane"
        (hash-ref spec 'lane)
        "--shard-index"
        (number->string (hash-ref spec 'shard-index))
        "--shard-total"
        (number->string (hash-ref spec 'shard-total))
        "--explain"))

;; ---- artifact reconstruction --------------------------------------------------------

;; A `test-results.json` artifact record as the runner writes it. Only the fields
;; this baseline reasons about are read; a missing field is a refusal, not a 0.
(define (artifact-record spec json)
  (define problems '())
  (define (problem! message)
    (set! problems (cons message problems)))
  (define files (hash-ref json 'files '()))
  (define paths
    (for/list ([entry (in-list files)])
      (or (hash-ref entry 'path #f) (hash-ref entry 'file #f))))
  (unless (and (pair? files) (andmap string? paths))
    (problem! "artifact has no usable per-file results"))
  (define unique (remove-duplicates (filter string? paths)))
  (when (> (length paths) (length unique))
    (problem! "artifact lists a duplicate executed file"))
  (define summary (hash-ref json 'run_summary #f))
  (define file-count (and summary (hash-ref summary 'file_count #f)))
  (unless (and (exact-nonnegative-integer? file-count) (= file-count (length paths)))
    (problem! (format "run_summary.file_count ~a does not match ~a per-file results"
                      file-count
                      (length paths))))
  (define qt (hash-ref (hash-ref json 'extra '()) 'qt '()))
  (when (lane-producer? spec)
    (unless (equal? (hash-ref qt 'suite #f) (hash-ref spec 'lane))
      (problem! (format "artifact suite ~a is not ~a" (hash-ref qt 'suite #f) (hash-ref spec 'lane))))
    (unless (= (or (hash-ref qt 'shard_index 0) 0) (hash-ref spec 'shard-index))
      (problem! (format "artifact shard_index ~a is not ~a"
                        (hash-ref qt 'shard_index 0)
                        (hash-ref spec 'shard-index))))
    (unless (= (or (hash-ref qt 'shard_total 1) 1) (hash-ref spec 'shard-total))
      (problem! (format "artifact shard_total ~a is not ~a"
                        (hash-ref qt 'shard_total 1)
                        (hash-ref spec 'shard-total)))))
  (define raw-verdict (hash-ref json 'verdict #f))
  (hash 'files
        (sort unique string<?)
        'verdict
        (if (string? raw-verdict)
            (string->symbol raw-verdict)
            raw-verdict)
        'file-count
        file-count
        'problems
        (reverse problems)))

;; ---- completeness checks ------------------------------------------------------------

;; Every check is recorded, not just the first failure: the baseline report must
;; say what is missing, and no check may default to success.
(define (check name ok? detail)
  (hash 'check name 'ok? (and ok? #t) 'detail detail))

(define (job-by-name jobs name)
  (for/first ([job (in-list jobs)]
              #:when (equal? (hash-ref job 'name "") name))
    job))

(define (step-conclusions job)
  (for/list ([step (in-list (hash-ref job 'steps '()))])
    (hash 'name (hash-ref step 'name "") 'conclusion (hash-ref step 'conclusion ""))))

(define (lane-step-ok? spec job)
  (for/or ([step (in-list (step-conclusions job))])
    (and (regexp-match? (hash-ref spec 'step-pattern) (hash-ref step 'name))
         (equal? (hash-ref step 'conclusion) "success")
         #t)))

(define (artifact-for artifacts name)
  (filter (lambda (artifact) (equal? (hash-ref artifact 'name "") name)) artifacts))

(define (lane-names)
  (remove-duplicates (for/list ([spec (in-list baseline-producers)]
                                #:when (lane-producer? spec))
                       (hash-ref spec 'lane))))

(define (union-of lists)
  (sort (remove-duplicates (append* lists)) string<?))

;; jobs/artifacts/plans/artifacts-json are keyed by producer name.
;; plans: name -> (listof string)
(define (producer-checks spec jobs artifacts plans artifact-records)
  (define name (hash-ref spec 'name))
  (define checks '())
  (define (add! label ok? detail)
    (set! checks (cons (check (string->symbol (format "~a-~a" name label)) ok? detail) checks)))
  (define job (job-by-name jobs name))
  (add! 'job-present (and job #t) (and job "present"))
  (when job
    (add! 'job-success (equal? (hash-ref job 'conclusion "") "success") (hash-ref job 'conclusion ""))
    (add! 'lane-step-executed
          (lane-step-ok? spec job)
          (for/list ([step (in-list (step-conclusions job))]
                     #:when (regexp-match? (hash-ref spec 'step-pattern) (hash-ref step 'name)))
            (format "~a=~a" (hash-ref step 'name) (hash-ref step 'conclusion)))))
  (cond
    [(lane-producer? spec)
     (define matching (artifact-for artifacts (hash-ref spec 'artifact)))
     (add! 'artifact-single (= 1 (length matching)) (length matching))
     (when (= 1 (length matching))
       (add! 'artifact-not-expired
             (not (hash-ref (car matching) 'expired #f))
             (hash-ref (car matching) 'expired #f)))
     (define executed (hash-ref artifact-records name #f))
     (add! 'artifact-parse
           (and executed (null? (hash-ref executed 'problems)))
           (and executed (hash-ref executed 'problems)))
     (when executed
       (add! 'artifact-verdict-pass
             (eq? (hash-ref executed 'verdict) 'pass)
             (hash-ref executed 'verdict))
       (add! 'executed-equals-plan
             (equal? (hash-ref executed 'files) (hash-ref plans name '()))
             (format "executed ~a, planned ~a"
                     (length (hash-ref executed 'files))
                     (length (hash-ref plans name '())))))]
    [else
     ;; declared-command producer: the executed set is the command's file, and it
     ;; must match the recorded plan (the same list by construction, re-checked).
     (define executed (hash-ref spec 'files))
     (add! 'executed-equals-plan
           (equal? executed (hash-ref plans name '()))
           (format "executed ~a, planned ~a" executed (hash-ref plans name '())))])
  (reverse checks))

(define (lane-checks lane artifact-records)
  (define specs
    (filter (lambda (spec) (and (lane-producer? spec) (equal? (hash-ref spec 'lane) lane)))
            baseline-producers))
  (define executed-sets
    (for/list ([spec (in-list specs)])
      (define record (hash-ref artifact-records (hash-ref spec 'name) #f))
      (and record (hash-ref record 'files))))
  (define present (filter values executed-sets))
  (cond
    [(= (length present) (length specs))
     (define concatenated (append* present))
     (list (check (string->symbol (format "~a-shards-disjoint" lane))
                  (= (length concatenated) (length (remove-duplicates concatenated)))
                  (format "~a executed entries" (length concatenated)))
           (check (string->symbol (format "~a-shard-union-nonempty" lane))
                  (pair? concatenated)
                  (format "~a executed entries" (length concatenated))))]
    [else
     (list (check (string->symbol (format "~a-shards-complete" lane)) #f "an artifact is missing"))]))

(define (baseline-checks revision run jobs artifacts plans artifact-records)
  (define run-checks
    (list
     (check 'run-completed (equal? (hash-ref run 'status "") "completed") (hash-ref run 'status ""))
     (check 'run-success
            (equal? (hash-ref run 'conclusion "") "success")
            (hash-ref run 'conclusion ""))
     (check 'head-sha-equals-revision
            (equal? (hash-ref run 'head_sha "") revision)
            (format "run head ~a, revision ~a" (hash-ref run 'head_sha "") revision))))
  ;; Only TEST-PRODUCER jobs participate in the exact-set check: lint, security
  ;; (a producer) and the other checks are covered by run-success, and adding a
  ;; new check job must not be mistaken for an unexpected test shard.
  (define (test-producer-job? name)
    (or (regexp-match? #px"^test \\([0-9]+\\)$" name)
        (regexp-match? #px"^workflows \\([0-9]+\\)$" name)
        (member name '("test-platform" "security" "smoke (ubuntu-latest)"))))
  (define job-names (filter test-producer-job? (map (lambda (job) (hash-ref job 'name "")) jobs)))
  (define expected-names (map (lambda (spec) (hash-ref spec 'name)) baseline-producers))
  ;; The API pages at 100; a full page means the payload may be truncated, so a
  ;; baseline is refused rather than read partially.
  (define pagination-check
    (check 'no-pagination-truncation
           (and (< (length jobs) 100) (< (length artifacts) 100))
           (format "jobs ~a, artifacts ~a" (length jobs) (length artifacts))))
  (define set-check
    (check 'producer-set-exact
           (and (null? (filter (lambda (name) (not (member name job-names))) expected-names))
                (null? (filter (lambda (name) (not (member name expected-names))) job-names)))
           (format "test-producer jobs ~a" job-names)))
  (define per-producer
    (append* (for/list ([spec (in-list baseline-producers)])
               (producer-checks spec jobs artifacts plans artifact-records))))
  (define per-lane
    (append* (for/list ([lane (in-list (lane-names))])
               (lane-checks lane artifact-records))))
  (define executed-union
    (union-of (for/list ([spec (in-list baseline-producers)])
                (define record (hash-ref artifact-records (hash-ref spec 'name) #f))
                (if record
                    (hash-ref record 'files)
                    (hash-ref spec 'files '())))))
  (define planned-union
    (union-of (for/list ([spec (in-list baseline-producers)])
                (hash-ref plans (hash-ref spec 'name) '()))))
  (define union-check
    (check
     'union-executed-equals-planned
     (equal? executed-union planned-union)
     (format "executed union ~a, planned union ~a" (length executed-union) (length planned-union))))
  (define all-checks
    (append run-checks (list pagination-check set-check) per-producer per-lane (list union-check)))
  (hash 'checks
        all-checks
        'complete?
        (andmap (lambda (c) (hash-ref c 'ok?)) all-checks)
        'executed-union
        executed-union
        'planned-union
        planned-union))

(define (complete? report)
  (hash-ref report 'complete? #f))

;; ---- effects ------------------------------------------------------------------------

(define (download-artifact-once! repo-root artifact-id dest-file)
  (define code
    (string-append "import sys;sys.path.insert(0,'scripts');import gh_helpers as h;"
                   "print(h.download_artifact(int(sys.argv[1]),sys.argv[2]))"))
  (define-values (status stdout stderr)
    (capture repo-root
             (or (find-executable-path "python3") "python3")
             (list "-c" code (number->string artifact-id) (path->string dest-file))))
  (unless (equal? status 0)
    (raise-arguments-error 'download-artifact-once!
                           "artifact download failed"
                           "artifact-id"
                           artifact-id
                           "stdout"
                           stdout
                           "stderr"
                           stderr))
  (string-trim stdout))

(define (artifact-json! repo-root artifact-id zip-file)
  (download-artifact-once! repo-root artifact-id zip-file)
  (define code
    (string-append
     "import sys,zipfile;z=zipfile.ZipFile(sys.argv[1]);"
     "name='test-results.json' if 'test-results.json' in z.namelist() else z.namelist()[0];"
     "sys.stdout.write(z.read(name).decode('utf-8'))"))
  (define-values (status stdout stderr)
    (capture repo-root
             (or (find-executable-path "python3") "python3")
             (list "-c" code (path->string zip-file))))
  (unless (equal? status 0)
    (raise-arguments-error 'artifact-json! "artifact unzip failed" "stderr" stderr))
  (string->jsexpr stdout))

(define (enumerate! worktree command)
  (define-values (status _elapsed stdout stderr timed-out?)
    (run-command command #:cwd worktree #:timeout-seconds 900))
  (when (or timed-out? (not (equal? status 0)))
    (raise-arguments-error 'enumerate! "plan enumeration failed" "command" command "stderr" stderr))
  (hash-ref (parse-explain-files stdout) 'files))

(define (preflight! worktree revision)
  (define head (string-trim (git! worktree "rev-parse" "HEAD")))
  (unless (equal? head revision)
    (raise-arguments-error 'run-baseline
                           "worktree HEAD is not the intended revision"
                           "head"
                           head
                           "revision"
                           revision))
  (define status (string-trim (git! worktree "status" "--porcelain")))
  (unless (equal? status "")
    (raise-arguments-error 'run-baseline "worktree is not clean" "status" status)))

(define (producer-row spec jobs artifacts plans artifact-records)
  (define name (hash-ref spec 'name))
  (define job (job-by-name jobs name))
  (define record (hash-ref artifact-records name #f))
  (define planned (hash-ref plans name '()))
  (define executed
    (if record
        (hash-ref record 'files)
        (hash-ref spec 'files '())))
  (hash
   'name
   name
   'kind
   (hash-ref spec 'kind)
   'plan-command
   (if (lane-producer? spec)
       (producer-plan-command spec)
       (hash-ref spec 'command))
   'job
   (hash 'conclusion (and job (hash-ref job 'conclusion #f)) 'steps (and job (step-conclusions job)))
   'artifact
   (and (lane-producer? spec)
        (for/first ([a (in-list artifacts)]
                    #:when (equal? (hash-ref a 'name "") (hash-ref spec 'artifact "")))
          (hash 'name (hash-ref a 'name) 'id (hash-ref a 'id) 'expired (hash-ref a 'expired #f))))
   'planned
   (hash 'files planned 'count (length planned) 'digest (enumeration-digest planned))
   'executed
   (hash 'files executed 'count (length executed) 'digest (enumeration-digest executed))))

(define (run-row run)
  (hash 'id
        (hash-ref run 'id)
        'attempt
        (hash-ref run 'run_attempt 1)
        'head-sha
        (hash-ref run 'head_sha #f)
        'event
        (hash-ref run 'event #f)
        'status
        (hash-ref run 'status #f)
        'conclusion
        (hash-ref run 'conclusion #f)))

;; run-baseline : workspace-root worktree revision [run-id] -> record
(define (run-baseline repo-root worktree revision #:run-id [run-id #f] #:out-file [out-file #f])
  (preflight! worktree revision)
  (define run
    (if run-id
        (gh-json repo-root "GET" (format "actions/runs/~a" run-id))
        (or (find-run-for-sha repo-root revision)
            (raise-arguments-error 'run-baseline
                                   "no CI run found for the intended revision"
                                   "revision"
                                   revision))))
  (define id (hash-ref run 'id))
  (define jobs
    (hash-ref (gh-json repo-root "GET" (format "actions/runs/~a/jobs?per_page=100" id)) 'jobs '()))
  (define artifacts
    (hash-ref (gh-json repo-root "GET" (format "actions/runs/~a/artifacts?per_page=100" id))
              'artifacts
              '()))
  (define plans
    (for/hash ([spec (in-list baseline-producers)])
      (values (hash-ref spec 'name)
              (if (lane-producer? spec)
                  (enumerate! worktree (producer-plan-command spec))
                  (hash-ref spec 'files)))))
  (define temp-dir (make-temporary-file "cl5-f2-artifacts-~a" 'directory))
  (define artifact-records
    (dynamic-wind
     void
     (lambda ()
       (for/hash ([spec (in-list baseline-producers)]
                  #:when (lane-producer? spec))
         (define name (hash-ref spec 'name))
         (define matching (artifact-for artifacts (hash-ref spec 'artifact)))
         (cond
           [(= 1 (length matching))
            (define artifact (car matching))
            (define zip (build-path temp-dir (format "~a.zip" (hash-ref spec 'artifact))))
            (values name
                    (artifact-record spec (artifact-json! repo-root (hash-ref artifact 'id) zip)))]
           [else (values name #f)])))
     (lambda ()
       (with-handlers ([exn:fail? (lambda (_) (void))])
         (delete-directory/files temp-dir)))))
  (define result (baseline-checks revision run jobs artifacts plans artifact-records))
  (define tree (string-trim (git! worktree "rev-parse" (format "~a^{tree}" revision))))
  (define record
    (hash 'experiment
          "CL5 W3 F2 verified comprehensive baseline (#9982)"
          'generated-by
          "scripts/cl5/baseline.rkt"
          'revision
          revision
          'tree
          tree
          'run
          (run-row run)
          'producers
          (for/list ([spec (in-list baseline-producers)])
            (producer-row spec jobs artifacts plans artifact-records))
          'totals
          (hash 'planned-union-count
                (length (hash-ref result 'planned-union))
                'executed-union-count
                (length (hash-ref result 'executed-union))
                'planned-union-digest
                (enumeration-digest (hash-ref result 'planned-union))
                'executed-union-digest
                (enumeration-digest (hash-ref result 'executed-union)))
          'checks
          (hash-ref result 'checks)
          'completeness
          (if (hash-ref result 'complete?) 'complete 'unavailable)
          'reasons
          (for/list ([c (in-list (hash-ref result 'checks))]
                     #:unless (hash-ref c 'ok?))
            (format "~a: ~a" (hash-ref c 'check) (hash-ref c 'detail)))))
  (when out-file
    (make-directory* (path-only out-file))
    (define tmp (string->path (string-append (path->string out-file) ".tmp")))
    (call-with-output-file tmp
                           (lambda (out) (write-string (jsexpr->string (->jsexpr record)) out))
                           #:exists 'truncate/replace)
    (rename-file-or-directory tmp out-file #t))
  record)

;; ---- record verification (independent reproduction) ---------------------------------

;; Re-derive every claim that the record alone can carry: plan/executed digests
;; from the stored file lists, per-producer set equality, and the completeness
;; verdict from the recorded checks. The external anchors remain the run id and
;; artifact ids; a verifier MUST re-fetch those to confirm the lists are the
;; artifacts' contents.
(define (recorded-completeness record)
  (define value (hash-ref record 'completeness))
  (if (string? value)
      (string->symbol value)
      value))

(define (verify-baseline-record record)
  (define problems '())
  (define (problem! message)
    (set! problems (cons message problems)))
  (for ([producer (in-list (hash-ref record 'producers))])
    (define name (hash-ref producer 'name))
    (define planned (hash-ref (hash-ref producer 'planned) 'files))
    (define executed (hash-ref (hash-ref producer 'executed) 'files))
    (for ([side (in-list (list (cons "planned" planned) (cons "executed" executed)))])
      (define label (car side))
      (define files (cdr side))
      (define entry (hash-ref producer (string->symbol label)))
      (unless (equal? (hash-ref entry 'digest) (enumeration-digest files))
        (problem! (format "~a ~a digest does not match its list" name label)))
      (unless (= (hash-ref entry 'count) (length files))
        (problem! (format "~a ~a count is wrong" name label))))
    (unless (equal? planned executed)
      ;; Divergence is exactly what an unavailable baseline records; it is only a
      ;; contradiction when the record claims `complete`.
      (unless (eq? (recorded-completeness record) 'unavailable)
        (problem! (format "~a executed set differs from its plan" name)))))
  (define all-checks (hash-ref record 'checks))
  (define expected (if (andmap (lambda (c) (hash-ref c 'ok?)) all-checks) 'complete 'unavailable))
  (unless (eq? (recorded-completeness record) expected)
    (problem! (format "completeness does not follow from the checks (expected ~a)" expected)))
  ;; Union totals must be re-derivable from the stored producer lists.
  (define recorded-totals (hash-ref record 'totals))
  (define executed-union
    (sort (remove-duplicates (append* (for/list ([p (in-list (hash-ref record 'producers))])
                                        (hash-ref (hash-ref p 'executed) 'files))))
          string<?))
  (define planned-union
    (sort (remove-duplicates (append* (for/list ([p (in-list (hash-ref record 'producers))])
                                        (hash-ref (hash-ref p 'planned) 'files))))
          string<?))
  (unless (and (= (hash-ref recorded-totals 'executed-union-count) (length executed-union))
               (equal? (hash-ref recorded-totals 'executed-union-digest)
                       (enumeration-digest executed-union)))
    (problem! "recorded executed-union totals do not match the producer lists"))
  (unless (and (= (hash-ref recorded-totals 'planned-union-count) (length planned-union))
               (equal? (hash-ref recorded-totals 'planned-union-digest)
                       (enumeration-digest planned-union)))
    (problem! "recorded planned-union totals do not match the producer lists"))
  (if (null? problems)
      #t
      (raise-arguments-error 'verify-baseline-record
                             "baseline record verification failed"
                             "problems"
                             (reverse problems))))

(module+ main
  (define repo-root #f)
  (define worktree #f)
  (define revision #f)
  (define out-file #f)
  (define run-id #f)
  (define verify-file #f)
  (command-line
   #:program "baseline.rkt"
   #:once-each
   ["--repo-root" path "workspace root holding scripts/gh_helpers.py" (set! repo-root path)]
   ["--worktree" path "clean disposable worktree at the revision" (set! worktree path)]
   ["--revision" sha "full SHA of the intended revision" (set! revision sha)]
   ["--out" path "JSON baseline record" (set! out-file path)]
   ["--run-id"
    id
    "explicit run id (default: latest run at the revision)"
    (set! run-id (string->number id))]
   ["--verify" path "re-verify a recorded baseline; launch nothing" (set! verify-file path)]
   #:args ()
   (cond
     [verify-file
      (define record (with-input-from-file verify-file (lambda () (read-json))))
      (verify-baseline-record record)
      (printf "verified ~a (~a)\n" verify-file (hash-ref record 'completeness))]
     [else
      (unless (and repo-root worktree revision out-file)
        (raise-user-error 'baseline "--repo-root, --worktree, --revision and --out are required"))
      (define record
        (run-baseline (string->path repo-root)
                      (string->path worktree)
                      revision
                      #:run-id run-id
                      #:out-file (string->path out-file)))
      (printf "wrote ~a: ~a\n" out-file (hash-ref record 'completeness))
      (for ([reason (in-list (hash-ref record 'reasons))])
        (printf "  ~a\n" reason))])))
