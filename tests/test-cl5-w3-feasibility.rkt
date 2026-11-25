#lang racket

;; @suite default  ;; @speed fast
;; @boundary unit

;; tests/test-cl5-w3-feasibility.rkt — W3.2-A feasibility audit.
;;
;; The audit must be able to conclude "this population cannot support the
;; measurement" from corpus + tree contents alone, without running a trial and
;; without editing the corpus.

(require rackunit
         racket/list
         (file "../scripts/cl5/w3-feasibility.rkt"))

;; ---- case classification -----------------------------------------------------------

(define (case sha
          stratum
          modules
          cones)
  (hash 'sha
        sha
        'stratum
        stratum
        'subject
        (format "case ~a" sha)
        'production-modules
        modules
        'cone-keys
        cones))

(define (presence-for cases #:engine [engine '()] #:manifest? [manifest? #f])
  (for*/hash ([c (in-list cases)]
              [path (in-list (append engine-paths
                                     (list "tests/.coverage-manifest.json"
                                           "tests/.quick-admission.rktd")))])
    (values (cons (hash-ref c 'sha) path) (or (member path engine) manifest?))))

(test-case "a version-bump-only case is derived metadata, not behaviour"
  (check-true (version-bump-only? (case "a"
                                    "S3"
                                    [list "info.rkt"]
                                    '("self:info.rkt"))))
  (check-true (version-bump-only? (case "a"
                                    "S3"
                                    [list
                                     "info.rkt"
                                     "util/version.rkt"]
                                    '())))
  (check-false (version-bump-only? (case "a"
                                     "S3"
                                     [list
                                      "info.rkt"
                                      "runtime/x.rkt"]
                                     '())))
  (check-false (version-bump-only? (case "a"
                                     "S3"
                                     '()
                                     '()))))

(test-case "the engine is present only when all three tools exist at the tree"
  (define c
    (case "a"
      "S3"
      [list "runtime/x.rkt"]
      '()))
  (define all (presence-for (list c) #:engine engine-paths))
  (check-true (hash-ref (classify-case c all) 'engine-present?))
  (define partial (presence-for (list c) #:engine (list "qt")))
  (define row (classify-case c partial))
  (check-false (hash-ref row 'engine-present?))
  (check-equal? (sort (hash-ref row 'engine-missing) string<?)
                (sort (list "scripts/testing/full-plan.rkt" "scripts/testing/quick-audit.rkt")
                      string<?)))

(test-case "classification records the manifest and admission record"
  (define c
    (case "a"
      "S3"
      [list "runtime/x.rkt"]
      '()))
  (define row (classify-case c (presence-for (list c) #:engine engine-paths #:manifest? #t)))
  (check-true (hash-ref row 'manifest-present?))
  (check-true (hash-ref row 'admission-record-present?)))

;; ---- strata and the population gate ------------------------------------------------

(define (classified sha stratum modules engine? bump?)
  (hash 'sha
        sha
        'stratum
        stratum
        'subject
        "s"
        'production-modules
        modules
        'cone-keys
        (list (format "self:~a" sha))
        'version-bump-only?
        bump?
        'engine-present?
        engine?
        'engine-missing
        '()
        'manifest-present?
        #t
        'admission-record-present?
        #t))

(test-case "strata summary counts measurable cases per stratum"
  (define summary
    (strata-summary (list (classified "a" "S1" '("m") #t #f)
                          (classified "b" "S2" '("m") #t #f)
                          (classified "c" "S2" '("m") #t #t)
                          (classified "d" "S2" '("m") #f #f)
                          (classified "e" "S3" '("m") #t #f))))
  (check-equal? (hash-ref (hash-ref summary 'S2) 'total) 3)
  (check-equal? (hash-ref (hash-ref summary 'S2) 'measurable) 1)
  (check-equal? (hash-ref (hash-ref summary 'S2) 'version-bump-only) 1)
  (check-equal? (hash-ref (hash-ref summary 'S2) 'engine-present) 2)
  (check-equal? (hash-ref (hash-ref summary 'S3) 'measurable) 1))

(test-case "the population gate needs the pre-registered minimum in S2 and S3"
  (define (summary-with s2 s3)
    (hash 'S1 (hash 'measurable 5) 'S2 (hash 'measurable s2) 'S3 (hash 'measurable s3)))
  (check-true (feasible? (summary-with 20 20)))
  (check-false (feasible? (summary-with 19 20)))
  (check-false (feasible? (summary-with 20 19)))
  (check-false (feasible? (summary-with 0 100))))

(test-case "measurable cases exclude version-bump-only and engine-absent cases"
  (define cases
    (list (classified "a" "S1" '("m") #t #f)
          (classified "b" "S2" '("m") #t #t)
          (classified "c" "S3" '("m") #f #f)))
  (check-equal? (map (lambda (c) (hash-ref c 'sha)) (measurable-cases cases)) (list "a")))

;; ---- the audit over a synthetic corpus ---------------------------------------------

(test-case "an audit of a corpus without the engine reports infeasibility and a candidate list"
  (define cases
    (list (case "a"
            "S2"
            [list "runtime/x.rkt"]
            '("self:a"))
          (case "b"
            "S2"
            [list "runtime/y.rkt"]
            '("self:b"))
          (case "c"
            "S3"
            [list "runtime/z.rkt"]
            '("self:c"))))
  (define corpus (hash 'revision "deadbeef" 'cases cases))
  (define record
    (audit-corpus corpus
                  "."
                  #:presence (presence-for cases
                                           #:engine (list "qt"
                                                          "scripts/testing/quick-audit.rkt"
                                                          "scripts/testing/full-plan.rkt")
                                           #:manifest? #t)))
  (check-false (hash-ref record 'population-feasible?))
  (check-equal? (hash-ref (hash-ref record 'totals) 'measurable) 3)
  (check-equal? (hash-ref (hash-ref record 'totals) 'engine-present) 3)
  (check-true (pair? (hash-ref record 'reasons)))
  (check-equal? (length (hash-ref record 'measurable-cases)) 3)
  ;; the audit never rewrites the corpus
  (check-equal? (hash-ref record 'corpus-sha) "deadbeef"))

;; ---- CI run lookup ---------------------------------------------------------------

(test-case "CI lookup pages past a full first page of other workflows"
  ;; A busy commit can have more workflow_run entries than one page; the push CI
  ;; run may be on page 2. Dropping it would misreport eligibility.
  (define pages
    (hash "page-1"
          (hash 'total_count
                12
                'workflow_runs
                (for/list ([i (in-range 10)])
                  (hash 'name "Shard plan telemetry" 'id i 'conclusion "success")))
          "page-2"
          (hash 'total_count
                12
                'workflow_runs
                (list (hash 'name "pr-latency-guard" 'id 10 'conclusion "failure")
                      (hash 'name "CI" 'id 11 'conclusion "success" 'event "push")))))
  (define (fake-fetch path)
    (define page (if (regexp-match? #rx"[?&]page=1$" path) "page-1" "page-2"))
    (hash-ref pages page))
  (define rows (ci-rows "." (list (hash 'sha "abc")) #:fetch fake-fetch))
  (check-equal? (length rows) 1)
  (check-equal? (hash-ref (car rows) 'ci-run-id) 11)
  (check-true (hash-ref (car rows) 'eligible?)))

(test-case "a commit with no CI workflow run is not eligible"
  (define (fake-fetch path)
    (hash 'total_count
          1
          'workflow_runs
          (list (hash 'name "pr-latency-guard" 'id 1 'conclusion "failure"))))
  (define rows (ci-rows "." (list (hash 'sha "abc")) #:fetch fake-fetch))
  (check-false (hash-ref (car rows) 'eligible?)))
