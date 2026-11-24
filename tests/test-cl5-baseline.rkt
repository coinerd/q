#lang racket

;; Tests for scripts/cl5/baseline.rkt (CL5 W3 F2 verified comprehensive baseline).
;; Pure: artifact reconstruction, the completeness check set, and record
;; verification. No network, no CI, no file writes.
;;
;; @suite default
;; @speed fast
;; @boundary unit

(require rackunit
         racket/file
         racket/list
         racket/runtime-path
         racket/string
         "../scripts/cl5/baseline.rkt"
         "../scripts/cl5/w3-dry-run.rkt")

(define-runtime-path workflow-path "../.github/workflows/ci.yml")

;; ---- registry pinned against the workflow ------------------------------------------

(test-case "the producer registry matches ci.yml's smoke command and shard defaults"
  (define text (file->string workflow-path))
  (check-true (regexp-match? #rx"raco test tests/test-provider-smoke\\.rkt" text))
  ;; fast shards default to [0, 1, 2]; workflows to two shards
  (check-true (regexp-match? #rx"\\[0, 1, 2\\]" text))
  (check-equal? (length (filter (lambda (spec) (equal? (hash-ref spec 'name) "test (0)"))
                                baseline-producers))
                1))

;; ---- artifact reconstruction -------------------------------------------------------

(define (artifact-json paths
                       #:suite [suite "fast"]
                       #:index [index 0]
                       #:total [total 3]
                       #:count [count (length paths)]
                       #:verdict [verdict 'pass])
  (hash 'files
        (for/list ([path (in-list paths)])
          (hash 'path path 'exit_code 0))
        'run_summary
        (hash 'file_count count 'pass count 'fail 0 'timeout 0 'skip 0)
        'verdict
        verdict
        'extra
        (hash 'qt (hash 'suite suite 'shard_index index 'shard_total total))))

(define fast-spec (first baseline-producers))

(test-case "a well-formed artifact yields its executed set"
  (define record (artifact-record fast-spec (artifact-json (list "tests/b.rkt" "tests/a.rkt"))))
  (check-equal? (hash-ref record 'files) (list "tests/a.rkt" "tests/b.rkt"))
  (check-equal? (hash-ref record 'problems) '()))

(test-case "artifact shape defects are refused, never defaulted"
  ;; missing per-file path
  (check-true (pair? (hash-ref (artifact-record
                                fast-spec
                                (hash 'files
                                      (list (hash 'foo 1))
                                      'run_summary
                                      (hash 'file_count 1)
                                      'verdict
                                      'pass
                                      'extra
                                      (hash 'qt (hash 'suite "fast" 'shard-index 0 'shard-total 3))))
                               'problems)))
  ;; duplicate executed file
  (check-true (pair? (hash-ref (artifact-record fast-spec
                                                (artifact-json (list "tests/a.rkt" "tests/a.rkt")))
                               'problems)))
  ;; summary count mismatch
  (check-true (pair? (hash-ref (artifact-record fast-spec
                                                (artifact-json (list "tests/a.rkt") #:count 9))
                               'problems)))
  ;; wrong shard metadata
  (check-true (pair? (hash-ref (artifact-record fast-spec
                                                (artifact-json (list "tests/a.rkt") #:index 2))
                               'problems))))

;; ---- synthetic complete run --------------------------------------------------------

(define fast-shards
  (hash 0
        (list "tests/f0.rkt" "tests/f1.rkt")
        1
        (list "tests/f2.rkt" "tests/f3.rkt")
        2
        (list "tests/f4.rkt" "tests/f5.rkt")))

(define plans
  (hash "test (0)"
        (hash-ref fast-shards 0)
        "test (1)"
        (hash-ref fast-shards 1)
        "test (2)"
        (hash-ref fast-shards 2)
        "test-platform"
        (list "tests/p0.rkt")
        "security"
        (list "tests/sec0.rkt")
        "workflows (0)"
        (list "tests/w0.rkt")
        "workflows (1)"
        (list "tests/w1.rkt")
        "smoke (ubuntu-latest)"
        (list "tests/test-provider-smoke.rkt")))

(define lane-plans
  (hash "fast"
        (append (hash-ref fast-shards 0) (hash-ref fast-shards 1) (hash-ref fast-shards 2))
        "platform"
        (list "tests/p0.rkt")
        "security"
        (list "tests/sec0.rkt")
        "workflows"
        (list "tests/w0.rkt" "tests/w1.rkt")))

(define (step name conclusion)
  (hash 'name name 'conclusion conclusion))

(define (job name steps)
  (hash 'name name 'status "completed" 'conclusion "success" 'steps steps))

(define (jobs)
  (list (job "test (0)" (list (step "Run test shard 0/3" "success")))
        (job "test (1)" (list (step "Run test shard 1/3" "success")))
        (job "test (2)" (list (step "Run test shard 2/3" "success")))
        (job "test-platform" (list (step "Run platform-cross suite" "success")))
        (job "security" (list (step "Run security test suite (parallel)" "success")))
        (job "workflows (0)" (list (step "Workflow integration suite shard 0/2" "success")))
        (job "workflows (1)" (list (step "Workflow integration suite shard 1/2" "success")))
        (job "smoke (ubuntu-latest)" (list (step "Provider compilation smoke tests" "success")))))

(define (artifact name id)
  (hash 'id id 'name name 'expired #f))

(define (artifacts)
  (list (artifact "test-results-fast-0" 1)
        (artifact "test-results-fast-1" 2)
        (artifact "test-results-fast-2" 3)
        (artifact "test-results-platform" 4)
        (artifact "test-results-security" 5)
        (artifact "test-results-workflows-0" 6)
        (artifact "test-results-workflows-1" 7)))

(define (artifact-records #:override [overrides (hash)])
  (for/hash ([(name files) (in-hash plans)]
             #:unless (equal? name "smoke (ubuntu-latest)"))
    (values name
            (hash 'files
                  (hash-ref overrides name files)
                  'verdict
                  'pass
                  'file-count
                  (length (hash-ref overrides name files))
                  'problems
                  '()))))

(define (run #:head [head "abc123"])
  (hash 'id 1 'run_attempt 1 'head_sha head 'event "push" 'status "completed" 'conclusion "success"))

(define (report #:head [head "abc123"]
                #:jobs [jobs* (jobs)]
                #:artifacts [arts (artifacts)]
                #:overrides [overrides (hash)])
  (baseline-checks "abc123"
                   (run #:head head)
                   jobs*
                   arts
                   plans
                   (artifact-records #:override overrides)))

(test-case "a fully executed, fully matching run is complete"
  (define result (report))
  (check-true (hash-ref result 'complete?)
              (format "~a" (filter (lambda (c) (not (hash-ref c 'ok?))) (hash-ref result 'checks))))
  (check-equal? (hash-ref result 'executed-union) (hash-ref result 'planned-union)))

(test-case "a missing shard job makes the baseline unavailable"
  (define result
    (report #:jobs (filter (lambda (j) (not (equal? (hash-ref j 'name) "test (2)"))) (jobs))))
  (check-false (hash-ref result 'complete?))
  (check-true (for/or ([c (in-list (hash-ref result 'checks))])
                (and (eq? (hash-ref c 'check) 'producer-set-exact) (not (hash-ref c 'ok?))))))

(test-case "a missing artifact makes the baseline unavailable"
  (define result
    (report #:artifacts (filter (lambda (a) (not (equal? (hash-ref a 'name) "test-results-fast-1")))
                                (artifacts))))
  (check-false (hash-ref result 'complete?)))

(test-case "an executed set that differs from the plan is caught"
  (define result (report #:overrides (hash "test (1)" (list "tests/f2.rkt" "tests/extra.rkt"))))
  (check-false (hash-ref result 'complete?))
  (check-true (hash-ref (report) 'complete?) "sanity: the unmodified run is complete"))

(test-case "overlapping shard sets are caught even when totals coincidentally match"
  ;; shard 1 claims a shard 0 file, shard 0 loses it: union stays the same size
  (define result
    (report #:overrides (hash "test (0)"
                              (list "tests/f0.rkt" "tests/f2.rkt")
                              "test (1)"
                              (list "tests/f1.rkt" "tests/f3.rkt"))))
  (check-false (hash-ref result 'complete?)))

(test-case "a run at a different head SHA is not the intended revision"
  (define result (report #:head "different"))
  (check-false (hash-ref result 'complete?)))

(test-case "a skipped lane step is caught even if the job says success"
  (define jobs*
    (for/list ([j (in-list (jobs))])
      (if (equal? (hash-ref j 'name) "security")
          (job "security" (list (step "Run security test suite (parallel)" "skipped")))
          j)))
  (define result (report #:jobs jobs*))
  (check-false (hash-ref result 'complete?)))

;; ---- record verification -----------------------------------------------------------

(define (check-row name ok?)
  (hash 'check name 'ok? ok? 'detail "test"))

(define (record #:complete? [complete? #t] #:files [files-override (hash)])
  (define prods
    (for/list ([spec (in-list baseline-producers)])
      (define name (hash-ref spec 'name))
      (define planned (hash-ref plans name))
      (define executed (hash-ref files-override name planned))
      (hash 'name
            name
            'planned
            (hash 'files planned 'count (length planned) 'digest (enumeration-digest planned))
            'executed
            (hash 'files executed 'count (length executed) 'digest (enumeration-digest executed)))))
  (define planned-union
    (sort (remove-duplicates (append* (map (lambda (p) (hash-ref (hash-ref p 'planned) 'files))
                                           prods)))
          string<?))
  (define executed-union
    (sort (remove-duplicates (append* (map (lambda (p) (hash-ref (hash-ref p 'executed) 'files))
                                           prods)))
          string<?))
  (hash 'producers
        prods
        'checks
        (list (check-row 'run-completed #t) (check-row 'union-executed-equals-planned complete?))
        'completeness
        (if complete? 'complete 'unavailable)
        'totals
        (hash 'planned-union-count
              (length planned-union)
              'planned-union-digest
              (enumeration-digest planned-union)
              'executed-union-count
              (length executed-union)
              'executed-union-digest
              (enumeration-digest executed-union))))

(test-case "a faithful baseline record re-derives"
  (check-true (verify-baseline-record (record))))

(test-case "a baseline record with a mismatched executed set is refused"
  (check-exn exn:fail?
             (lambda ()
               (verify-baseline-record (record #:files (hash "test (0)" (list "tests/f0.rkt")))))))

(test-case "a baseline record whose completeness contradicts its checks is refused"
  (define r (record #:complete? #f))
  (check-exn exn:fail? (lambda () (verify-baseline-record (hash-set r 'completeness 'complete)))))

(test-case "a baseline record with a tampered digest is refused"
  (define r (record))
  (define prods (hash-ref r 'producers))
  (define first*
    (hash-set (first prods) 'planned (hash-set (hash-ref (first prods) 'planned) 'digest "0000")))
  (check-exn exn:fail?
             (lambda () (verify-baseline-record (hash-set r 'producers (cons first* (rest prods)))))))

(test-case "an unavailable record with a divergent producer verifies as unavailable"
  (check-true (verify-baseline-record (record #:complete? #f
                                              #:files (hash "test (0)" (list "tests/f0.rkt"))))))

(test-case "a record whose union totals do not match its lists is refused"
  (define r (record))
  (define totals (hash-set (hash-ref r 'totals) 'executed-union-count 999))
  (check-exn exn:fail? (lambda () (verify-baseline-record (hash-set r 'totals totals)))))
