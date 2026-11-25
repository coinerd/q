#lang racket/base

;; ci-producers.rkt — the pre-registered CI test-producer registry, shared by the
;; F2 baseline verifier and the W3 evaluation driver (#9982).
;;
;; One authority for "what CI runs": fast shards 0–2, platform, security, workflows
;; shards 0–1, and smoke. Smoke uploads no test-results artifact; its executed set
;; is the file named by the workflow's own command (a declaration, pinned against
;; ci.yml by tests/test-cl5-baseline.rkt), never an inferred filename set.
;;
;; `failed-tests-in-run` reads a run's own artifacts to obtain the per-file
;; failures for the tree that run produced (F1/F2 machinery: real behavioural
;; evidence, not job names). Evaluation-only; nothing here selects tests.

(require json
         racket/file
         racket/list
         racket/path
         racket/string
         "ci-evidence.rkt")

(provide baseline-producers
         lane-producer?
         producer-plan-command
         smoke-command
         smoke-files
         download-artifact-once!
         artifact-json!
         failed-tests-in-run)

;; ---- pre-registered producer registry ----------------------------------------------

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

(define (producer-plan-command spec)
  (list "./qt"
        "--lane"
        (hash-ref spec 'lane)
        "--shard-index"
        (number->string (hash-ref spec 'shard-index))
        "--shard-total"
        (number->string (hash-ref spec 'shard-total))
        "--explain"))

;; ---- artifact access ---------------------------------------------------------------

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

(define (failed-file? entry)
  (or (eq? (hash-ref entry 'failed #f) #t)
      (member (string-downcase (format "~a" (hash-ref entry 'status "")))
              '("fail" "failed" "failure"))
      (pair? (hash-ref entry 'failures '()))))

;; Per-file failures from a run's own artifacts, for the tree that run produced.
;; A missing/expired artifact is recorded as such (no failed files, no guess).
(define (failed-tests-in-run repo-root run-id)
  (define run (gh-json repo-root "GET" (format "actions/runs/~a" run-id)))
  (define artifacts
    (hash-ref (gh-json repo-root "GET" (format "actions/runs/~a/artifacts?per_page=100" run-id))
              'artifacts
              '()))
  (define temp-dir (make-temporary-file "cl5-run-artifacts-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define rows
       (for/list ([spec (in-list baseline-producers)]
                  #:when (lane-producer? spec))
         (define matching
           (filter (lambda (artifact) (equal? (hash-ref artifact 'name "") (hash-ref spec 'artifact)))
                   artifacts))
         (cond
           [(= 1 (length matching))
            (define artifact (car matching))
            (define zip (build-path temp-dir (format "~a.zip" (hash-ref spec 'artifact))))
            (define payload (artifact-json! repo-root (hash-ref artifact 'id) zip))
            (define files (hash-ref payload 'files '()))
            (define failed
              (sort (for/list ([entry (in-list files)]
                               #:when (failed-file? entry))
                      (hash-ref entry 'path ""))
                    string<?))
            (hash 'name
                  (hash-ref spec 'name)
                  'artifact-id
                  (hash-ref artifact 'id)
                  'expired
                  (hash-ref artifact 'expired #f)
                  'verdict
                  (hash-ref payload 'verdict #f)
                  'executed
                  (length files)
                  'failed
                  (filter (lambda (p) (and (string? p) (not (equal? p "")))) failed))]
           [else
            (hash 'name
                  (hash-ref spec 'name)
                  'artifact-id
                  #f
                  'expired
                  #f
                  'verdict
                  #f
                  'executed
                  0
                  'failed
                  '())])))
     (hash 'run-id
           run-id
           'head-sha
           (hash-ref run 'head_sha #f)
           'conclusion
           (hash-ref run 'conclusion #f)
           'producers
           rows
           'failed-files
           (sort (remove-duplicates (append* (map (lambda (row) (hash-ref row 'failed)) rows)))
                 string<?)))
   (lambda ()
     (with-handlers ([exn:fail? (lambda (_) (void))])
       (delete-directory/files temp-dir)))))
