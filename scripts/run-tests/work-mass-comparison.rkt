#lang racket/base

;; scripts/run-tests/work-mass-comparison.rkt — v1.00.28 W7
;;
;; Compares the W0 baseline fast-runtime census against the W7
;; post-remediation census (identical measurement method, PLAN-v1.00.28
;; §W7 + Measurement Contract §3).
;;
;; Contracts (see tests/test-work-mass-comparison.rkt):
;; - inventory equality: every W0 record path must exist in the post census;
;;   a silently absent file is a red error (exn:fail:work-mass-comparison).
;;   New post files get explicit inventory rows (never silent additions).
;; - percentage math is computed from the stored per-record medians and
;;   cross-checked against the stored aggregates.work_mass_ms; a stored
;;   aggregate that disagrees with its own medians is a red error.
;; - required row families: fast work mass; top 1/10/25/50/100 contribution;
;;   bucket counts; total process launches; Git command launches; fixture
;;   construction counts; total requested real sleep; grouped-safe share (Q7).
;; - unknown (null) counters stay null ("unknown" rows), never coerced to 0.
;; - verdict guide: <10% insufficient; 10-25% partial; 25-40% meaningful;
;;   >=40% strong (improvement of fast work mass).

(require json
         racket/contract
         racket/date
         racket/file
         racket/format
         racket/hash
         racket/list
         racket/match
         racket/path
         racket/string)

(provide (contract-out (struct exn:fail:work-mass-comparison
                               ([message string?] [continuation-marks continuation-mark-set?]))
                       [work-mass-verdict (-> real? string?)]
                       [work-mass-compare (->* (hash? hash?) (#:removed-since-baseline hash?) hash?)]
                       [compare-files (-> path-string? path-string? hash?)]
                       [main (-> (listof string?) any)]))

;; ------------------------------------------------------------------ errors

(struct exn:fail:work-mass-comparison exn:fail ())

(define (comparison-error fmt . args)
  (raise (exn:fail:work-mass-comparison (apply format fmt args) (current-continuation-marks))))

;; ------------------------------------------------------------------ helpers

(define (round2 x)
  (/ (round (* x 100.0)) 100.0))

;; Percentage change from baseline to post, in percent, 2dp.
;; A zero baseline has no meaningful percentage -> null.
(define (pct-change baseline post)
  (cond
    [(equal? baseline 'null) 'null]
    [(equal? post 'null) 'null]
    [(zero? baseline) 'null]
    [else (round2 (* 100 (/ (- post baseline) baseline)))]))

(define (num?/null v)
  (or (null? v) (real? v)))

;; Null-aware sum: any null component makes the total unknown (never 0).
(define (null-aware-sum . vs)
  (if (ormap (lambda (v) (equal? v 'null)) vs)
      'null
      (apply + vs)))

;; --------------------------------------------------------------- census io

(define (read-census path)
  (define doc (with-input-from-file path read-json))
  (unless (and (hash? doc) (hash? (hash-ref doc 'aggregates #f)) (list? (hash-ref doc 'records #f)))
    (comparison-error "census ~a does not have the expected shape" path))
  doc)

(define (record-path rec)
  (hash-ref rec 'path))
(define (pass-record? rec)
  (equal? (hash-ref rec 'status) "pass"))
(define (record-median rec)
  (hash-ref rec 'median_ms 0))

(define (work-type-total census key)
  (define totals (hash-ref (hash-ref census 'aggregates) 'work_type_totals #f))
  (cond
    [(not totals) 'null]
    [(hash-has-key? totals key) (hash-ref totals key)]
    [else 'null]))

;; ------------------------------------------------------------------- rows

(define (make-row metric baseline post)
  (define delta
    (if (or (equal? baseline 'null) (equal? post 'null))
        'null
        (- post baseline)))
  (hasheq 'metric
          metric
          'baseline
          baseline
          'post
          post
          'delta
          delta
          'delta_pct
          (pct-change baseline post)
          'status
          (cond
            [(or (equal? baseline 'null) (equal? post 'null)) "unknown"]
            [(equal? baseline post) "equal"]
            [(< post baseline) "improved"]
            [else "regressed"])))

;; Sum of the top-N largest per-file medians (desc, tie: path asc) — the
;; method used for the census Pareto rows.
(define (top-n-mass records n)
  (define passing
    (sort (filter pass-record? records)
          (lambda (a b)
            (let ([ma (record-median a)]
                  [mb (record-median b)])
              (or (> ma mb) (and (= ma mb) (string<? (record-path a) (record-path b))))))))
  (for/sum ([rec (in-list (take passing (min n (length passing))))]) (record-median rec)))

;; Grouped-safe share from the census Q7 queue: Q7 lists member paths;
;; mass/files are computed over the passing records with those paths.
(define (q7-share census)
  (define queues (hash-ref (hash-ref census 'aggregates) 'queues #f))
  (define q7 (and queues (hash-ref queues 'q7 #f)))
  (cond
    [(not (list? q7)) (hasheq 'pct 'null 'files 0 'mass 'null)]
    [else
     (define by-path
       (for/hash ([rec (in-list (hash-ref census 'records))]
                  #:when (pass-record? rec))
         (values (record-path rec) rec)))
     (define members
       (for/list ([p (in-list q7)]
                  #:when (hash-has-key? by-path p))
         (hash-ref by-path p)))
     (define mass (for/sum ([rec (in-list members)]) (record-median rec)))
     (define total
       (for/sum ([rec (in-list (hash-ref census 'records))] #:when (pass-record? rec))
                (record-median rec)))
     (hasheq 'pct
             (if (zero? total)
                 'null
                 (round2 (* 100 (/ mass total))))
             'files
             (length members)
             'mass
             mass)]))

(define (bucket-rows baseline post)
  (define (bucket-table census)
    (for/hash ([b (in-list (hash-ref (hash-ref census 'aggregates) 'buckets '()))])
      (values (hash-ref b 'label) (hash-ref b 'file_count))))
  (define bt (bucket-table baseline))
  (define pt (bucket-table post))
  (define labels
    (append (hash-keys bt)
            (for/list ([l (in-hash-keys pt)]
                       #:unless (hash-has-key? bt l))
              l)))
  (for/list ([label (in-list labels)])
    (define b (hash-ref bt label 'null))
    (define p (hash-ref pt label 'null))
    (hasheq 'metric
            "bucket count"
            'label
            label
            'baseline
            b
            'post
            p
            'delta
            (if (or (equal? b 'null) (equal? p 'null))
                'null
                (- p b))
            'status
            (cond
              [(or (equal? b 'null) (equal? p 'null)) "unknown"]
              [(equal? b p) "equal"]
              [(< p b) "improved"]
              [else "regressed"]))))

;; ------------------------------------------------------------ verification

;; The stored aggregate work mass must equal the sum of the stored medians —
;; the comparison's percentages are only trustworthy against medians.
(define (check-work-mass-aggregate census label)
  (define stored (hash-ref (hash-ref census 'aggregates) 'work_mass_ms #f))
  (define computed
    (for/sum ([rec (in-list (hash-ref census 'records))] #:when (pass-record? rec))
             (record-median rec)))
  (unless (equal? stored computed)
    (comparison-error
     "~a census stored work_mass_ms (~a) disagrees with the sum of stored medians (~a)"
     label
     stored
     computed))
  computed)

;; ----------------------------------------------------------------- compare

(define (work-mass-compare baseline post #:removed-since-baseline [removal-manifest #hash()])
  ;; §W7.2 inventory equality first: fail loudly on silent removals.
  ;; Removals declared in the manifest (path -> reason) become explicit
  ;; inventory rows; anything else absent from the post census stays red.
  (define baseline-by-path
    (for/hash ([rec (in-list (hash-ref baseline 'records))])
      (values (record-path rec) rec)))
  (define post-by-path
    (for/hash ([rec (in-list (hash-ref post 'records))])
      (values (record-path rec) rec)))
  (define removed
    (sort (for/list ([p (in-hash-keys baseline-by-path)]
                     #:unless (hash-has-key? post-by-path p))
            p)
          string<?))
  (define explicit-removal-rows
    (for/list ([p (in-list removed)]
               #:when (hash-has-key? removal-manifest p))
      (hasheq 'path
              p
              'reason
              (hash-ref removal-manifest p)
              'baseline_median_ms
              (record-median (hash-ref baseline-by-path p)))))
  (define silent
    (for/list ([p (in-list removed)]
               #:unless (hash-has-key? removal-manifest p))
      p))
  (unless (null? silent)
    (comparison-error
     "inventory mismatch: ~a file(s) present in the baseline census are silently absent from the post census; first: ~a"
     (length silent)
     (car silent)))

  (define added
    (for/list ([p (in-list (sort (for/list ([pth (in-hash-keys post-by-path)]
                                            #:unless (hash-has-key? baseline-by-path pth))
                                   pth)
                                 string<?))])
      (define rec (hash-ref post-by-path p))
      (hasheq 'path
              p
              'status
              (hash-ref rec 'status)
              'median_ms
              (if (pass-record? rec)
                  (hash-ref rec 'median_ms 'null)
                  'null)
              'reason
              (hash-ref rec 'reason 'null))))

  ;; Integrity of the stored aggregates against the stored medians.
  (define baseline-mass (check-work-mass-aggregate baseline "baseline"))
  (define post-mass (check-work-mass-aggregate post "post"))

  (define rows
    (append (list (make-row "fast work mass" baseline-mass post-mass)
                  (make-row "top 1 contribution"
                            (top-n-mass (hash-ref baseline 'records) 1)
                            (top-n-mass (hash-ref post 'records) 1))
                  (make-row "top 10 contribution"
                            (top-n-mass (hash-ref baseline 'records) 10)
                            (top-n-mass (hash-ref post 'records) 10))
                  (make-row "top 25 contribution"
                            (top-n-mass (hash-ref baseline 'records) 25)
                            (top-n-mass (hash-ref post 'records) 25))
                  (make-row "top 50 contribution"
                            (top-n-mass (hash-ref baseline 'records) 50)
                            (top-n-mass (hash-ref post 'records) 50))
                  (make-row "top 100 contribution"
                            (top-n-mass (hash-ref baseline 'records) 100)
                            (top-n-mass (hash-ref post 'records) 100)))
            (bucket-rows baseline post)
            (list (make-row "total process launches"
                            (null-aware-sum (work-type-total baseline 'subprocesses)
                                            (work-type-total baseline 'racket_subprocesses))
                            (null-aware-sum (work-type-total post 'subprocesses)
                                            (work-type-total post 'racket_subprocesses)))
                  (make-row "git command launches"
                            (work-type-total baseline 'git_commands)
                            (work-type-total post 'git_commands))
                  (make-row "fixture constructions"
                            (null-aware-sum (work-type-total baseline 'git_fixtures)
                                            (work-type-total baseline 'session_fixtures))
                            (null-aware-sum (work-type-total post 'git_fixtures)
                                            (work-type-total post 'session_fixtures)))
                  (make-row "total requested real sleep (ms)"
                            (work-type-total baseline 'sleep_requested_ms)
                            (work-type-total post 'sleep_requested_ms)))))
  (define baseline-q7 (q7-share baseline))
  (define post-q7 (q7-share post))
  (define q7-row
    (hasheq 'metric
            "grouped-safe share"
            'baseline
            (hash-ref baseline-q7 'mass)
            'post
            (hash-ref post-q7 'mass)
            'baseline_pct
            (hash-ref baseline-q7 'pct)
            'post_pct
            (hash-ref post-q7 'pct)
            'baseline_files
            (hash-ref baseline-q7 'files)
            'post_files
            (hash-ref post-q7 'files)))
  (set! rows (append rows (list q7-row)))

  (define improvement-pct
    (let ([dp (pct-change baseline-mass post-mass)])
      (if (equal? dp 'null)
          'null
          (- dp))))
  (hasheq 'rows
          rows
          'inventory
          (hasheq 'removed explicit-removal-rows 'added added)
          'summary
          (hasheq 'work_mass_baseline
                  baseline-mass
                  'work_mass_post
                  post-mass
                  'work_mass_delta
                  (- post-mass baseline-mass)
                  'work_mass_delta_pct
                  (pct-change baseline-mass post-mass)
                  'work_mass_improvement_pct
                  improvement-pct
                  'verdict
                  (work-mass-verdict (if (equal? improvement-pct 'null) -1 improvement-pct))
                  'grouped_safe_baseline
                  (hash-ref baseline-q7 'pct)
                  'grouped_safe_post
                  (hash-ref post-q7 'pct)
                  'generated_at_utc
                  (parameterize ([date-display-format 'iso-8601])
                    (define d (current-date))
                    (format "~a-~a-~aT~a:~a:~aZ"
                            (date-year d)
                            (~r (date-month d) #:min-width 2 #:pad-string "0")
                            (~r (date-day d) #:min-width 2 #:pad-string "0")
                            (~r (date-hour d) #:min-width 2 #:pad-string "0")
                            (~r (date-minute d) #:min-width 2 #:pad-string "0")
                            (~r (date-second d) #:min-width 2 #:pad-string "0"))))))

(define (work-mass-verdict improvement-pct)
  (cond
    [(< improvement-pct 10) "insufficient"]
    [(< improvement-pct 25) "partial"]
    [(< improvement-pct 40) "meaningful"]
    [else "strong"]))

(define (compare-files baseline-path post-path [removals #hash()])
  (work-mass-compare (read-census baseline-path)
                     (read-census post-path)
                     #:removed-since-baseline removals))

;; Removals sidecar: JSON object path -> reason, sibling of the post census
;; by default. Absent file => empty manifest (silent removals stay red).
(define (load-removals post-path removals-path)
  (define path
    (cond
      [removals-path removals-path]
      [else
       (define sibling
         (build-path (path-only (simplify-path post-path)) "removed-since-baseline.json"))
       (if (file-exists? sibling) sibling #f)]))
  (cond
    [(not path) #hash()]
    [else
     (define doc (with-input-from-file path read-json))
     (unless (hash? doc)
       (comparison-error "removals manifest ~a must be a JSON object" path))
     (for/hash ([(k v) (in-hash doc)])
       (values (format "~a" k)
               (cond
                 [(string? v) v]
                 [else (format "~a" v)])))]))

;; -------------------------------------------------------------------- main

(define (usage)
  (eprintf
   "usage: racket scripts/run-tests/work-mass-comparison.rkt --baseline <census.json> --post <census.json> --out <comparison.json> [--removals <removed-since-baseline.json>]\n")
  2)

(define (main args)
  (define baseline-path #f)
  (define post-path #f)
  (define out-path #f)
  (define removals-path #f)
  (let loop ([args args])
    (match args
      ['() (void)]
      [(list "--baseline" p rest ...)
       (set! baseline-path p)
       (loop rest)]
      [(list "--post" p rest ...)
       (set! post-path p)
       (loop rest)]
      [(list "--out" p rest ...)
       (set! out-path p)
       (loop rest)]
      [(list "--removals" p rest ...)
       (set! removals-path p)
       (loop rest)]
      [else (comparison-error "unexpected argument: ~a" (car args))]))
  (unless (and baseline-path post-path out-path)
    (dynamic-wind void usage (lambda () (exit 2))))
  (define doc (compare-files baseline-path post-path (load-removals post-path removals-path)))
  (define full
    (hash-union doc
                (hasheq 'schema
                        "work-mass-comparison/v1"
                        'generated_from
                        (hasheq 'baseline_census
                                (path->string (simplify-path baseline-path))
                                'post_census
                                (path->string (simplify-path post-path))))))
  (with-output-to-file out-path (lambda () (write-json full)) #:exists 'replace)
  (define summary (hash-ref doc 'summary))
  (printf "work mass ~a -> ~a ms (~a%); verdict: ~a\n"
          (hash-ref summary 'work_mass_baseline)
          (hash-ref summary 'work_mass_post)
          (hash-ref summary 'work_mass_delta_pct)
          (hash-ref summary 'verdict))
  0)

(module+ main
  (exit (main (vector->list (current-command-line-arguments)))))
