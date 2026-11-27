#lang racket/base

;; timeout-margin.rkt — derive concurrency-aware per-file timeouts (#9982, P2-D).
;;
;; Policy (owner-approved 2026-11-26, applied only through this registry):
;;   T = max(T_existing, ceil_30s(1.5 * p95_n))
;; where p95_n is the 95th percentile wall time measured AT THE MEASUREMENT'S JOB
;; COUNT (n), not standalone. Existing declarations are floors: nothing is lowered.
;;
;; Scope guard: the registry carries an explicit allow-list. This module refuses a
;; registry containing a path outside it, so the policy cannot quietly become a
;; repository-wide timeout raise. `--verify` re-derives every number from the
;; recorded samples and refuses anything that does not reproduce.
;;
;; Fail-closed behaviour is untouched: this module computes limits and never runs a
;; verdict. A timeout still fails a file, an incomplete producer still makes the
;; comprehensive record `degraded`, and a censored run still cannot support a GO.
;;
;; Evaluation-only.

(require json
         "../testing/full-plan.rkt"
         racket/cmdline
         racket/file
         racket/list
         racket/match
         racket/path
         racket/port
         racket/string
         racket/system)

(provide probe-copy-source
         censored-samples
         default-allowed-paths
         percentile
         ceil-to-30
         derive-limit
         registry-entry
         registry-entry?
         registry-entry->jsexpr
         verify-registry
         build-registry
         measure-samples)

(define schema "cl5-slow-tests/1")

;; The authoritative allow-list. A registry may not widen or narrow it: `--verify`
;; compares the registry's own list against THIS one, so a registry cannot authorise
;; itself to cover another file.
(define default-allowed-paths
  (list "tests/test-ci-recovery-trial.rkt"
        "tests/test-golden-flows.rkt"
        "tests/test-gsd-branch-delivery-verification.rkt"
        "tests/test-gsd-delivery-coordinator.rkt"
        "tests/test-gsd-delivery-verifier-git-contract.rkt"
        "tests/test-gsd-evidence-discovery.rkt"
        "tests/test-gsd-evidence-repair-production-contract.rkt"
        "tests/test-gsd-executor-ergonomics-prompt.rkt"
        "tests/test-milestone-gate.rkt"
        "tests/test-qt-lanes.rkt"
        "tests/test-qt-quick.rkt"))

;; ---- pure policy arithmetic --------------------------------------------------------

;; Nearest-rank percentile over an unsorted list of numbers.
(define (percentile values p)
  (unless (pair? values)
    (error 'percentile "no samples"))
  (define sorted (sort (map exact->inexact values) <))
  (define n (length sorted))
  (define rank (max 1 (min n (inexact->exact (ceiling (* p n))))))
  (list-ref sorted (sub1 rank)))

(define (ceil-to-30 seconds)
  (* 30 (inexact->exact (ceiling (/ (exact->inexact seconds) 30)))))

;; T = max(existing, ceil30(1.5 * p95_n)); existing is a floor, never lowered.
(define (derive-limit existing-seconds p95-ms)
  (max (inexact->exact existing-seconds) (ceil-to-30 (* 1.5 (/ p95-ms 1000.0)))))

;; ---- registry ----------------------------------------------------------------------

(struct registry-entry
        (path ; test file path
         existing-limit-s ; declared/effective limit before the policy
         job-count ; n at which the samples were taken
         samples-ms
         p95-ms
         limit-s
         uncertainty) ; honest statement about sample sparsity
  #:transparent)

(define (registry-entry->jsexpr e)
  (hash 'path
        (registry-entry-path e)
        'existing_limit_s
        (registry-entry-existing-limit-s e)
        'job_count
        (registry-entry-job-count e)
        'samples_ms
        (registry-entry-samples-ms e)
        'sample_count
        (length (registry-entry-samples-ms e))
        'p95_ms
        (inexact->exact (round (registry-entry-p95-ms e)))
        'limit_s
        (registry-entry-limit-s e)
        'margin
        (/ (registry-entry-limit-s e) (/ (registry-entry-p95-ms e) 1000.0))
        'uncertainty
        (registry-entry-uncertainty e)))

(define (build-registry entries #:allowed allowed)
  (hasheq 'schema
          schema
          'policy
          (hasheq 'formula
                  "max(existing_limit_s, ceil_30s(1.5 * p95_job_count))"
                  'confidence
                  0.95
                  'rounding_s
                  30
                  'min_samples
                  5
                  'applied_by
                  "test-file @timeout declarations")
          'allowed_paths
          allowed
          'entries
          (for/list ([e (in-list entries)])
            (registry-entry->jsexpr e))))

(define (refuse fmt . args)
  (error 'verify-registry (string-append "registry refused: " (apply format fmt args))))

(define (verify-registry reg #:allowed [authoritative default-allowed-paths])
  (unless (equal? (hash-ref reg 'schema #f) schema)
    (refuse "unknown schema ~s" (hash-ref reg 'schema #f)))
  (define policy (hash-ref reg 'policy (hash)))
  (define min-samples (hash-ref policy 'min_samples 5))
  (define allowed (hash-ref reg 'allowed_paths '()))
  (unless (equal? (sort (map (lambda (x) (format "~a" x)) allowed) string<?)
                  (sort (map (lambda (x) (format "~a" x)) authoritative) string<?))
    (refuse "the registry's allow-list ~s differs from the authoritative one ~s"
            allowed
            authoritative))
  (define entries (hash-ref reg 'entries '()))
  (when (null? entries)
    (refuse "no entries"))
  (for ([e (in-list entries)])
    (define path (hash-ref e 'path #f))
    (unless (and (string? path) (member path allowed equal?))
      (refuse "entry ~s is not in the registry's allow-list (the policy is not repository-wide)"
              path))
    (define samples (hash-ref e 'samples_ms '()))
    (unless (>= (length samples) min-samples)
      (refuse "entry ~s has ~a samples (< ~a): too sparse to derive a limit"
              path
              (length samples)
              min-samples))
    (unless (= (hash-ref e 'sample_count -1) (length samples))
      (refuse "entry ~s declares sample_count ~s but records ~s samples"
              path
              (hash-ref e 'sample_count -1)
              (length samples)))
    (define p95 (percentile samples 0.95))
    (unless (= (hash-ref e 'p95_ms #f) p95)
      (refuse "entry ~s declares p95 ~s but re-derives ~s" path (hash-ref e 'p95_ms #f) p95))
    (define derived (derive-limit (hash-ref e 'existing_limit_s) p95))
    (unless (= (hash-ref e 'limit_s #f) derived)
      (refuse "entry ~s declares limit ~s but the policy derives ~s"
              path
              (hash-ref e 'limit_s #f)
              derived))
    (define margin (hash-ref e 'margin #f))
    (unless (and (real? margin) (>= margin 1.5))
      (refuse "entry ~s margin ~s is below 1.5x p95" path margin))
    ;; The margin must also be the one the numbers imply, not a self-declaration.
    (define derived-margin (/ derived (/ p95 1000.0)))
    (unless (<= (abs (- margin derived-margin)) 1e-6)
      (refuse "entry ~s margin ~s does not match the derived ~s" path margin derived-margin))
    (unless (and (string? (hash-ref e 'uncertainty #f))
                 (positive? (string-length (hash-ref e 'uncertainty))))
      (refuse "entry ~s does not state its measurement uncertainty" path))
    (unless (exact-positive-integer? (hash-ref e 'job_count #f))
      (refuse "entry ~s does not record the job count at which it was measured" path)))
  (hash 'schema
        schema
        'entries
        (length entries)
        'paths
        (map (lambda (e) (hash-ref e 'path)) entries)))

;; ---- measurement -------------------------------------------------------------------

;; CPU load generator: `n` busy Racket children, so the measured file runs under the
;; same number of competing workers as the surface's job count.
(define (start-load n)
  (for/list ([_ (in-range n)])
    (call-with-values (lambda ()
                        (subprocess #f
                                    #f
                                    #f
                                    (find-executable-path "racket")
                                    "-e"
                                    "(let loop ([i 0]) (when (< i 100000000) (loop (+ i 1))))"))
                      list)))

(define (stop-load procs)
  (for ([p (in-list procs)])
    (match-define (list proc out in err) p)
    (close-input-port out)
    (close-output-port in)
    (close-input-port err)
    (subprocess-kill proc #t)
    (subprocess-wait proc)))

;; One sample: the file through the production adapter with censoring DISABLED, so the
;; measurement can see the real wall time. The production limit stays whatever the
;; declaration says until the registry is applied.
(define (measure-one file uncensored-timeout-s)
  (define start (current-inexact-milliseconds))
  (define status
    (parameterize ([current-output-port (open-output-nowhere)])
      (system*/exit-code (path->string (find-executable-path "racket"))
                         "scripts/run-tests/runner.rkt"
                         "--timeout"
                         (number->string uncensored-timeout-s)
                         file)))
  ;; The runner's exit code is the only signal this path has: 0 finished, 2 was killed at
  ;; the deadline, anything else failed. Map it to the same vocabulary the surface path
  ;; reads from the artifact, so `censored-samples` means the same thing in both places.
  (hash 'file
        file
        'run_status
        (cond
          [(eqv? status 0) "pass"]
          [(eqv? status 2) "timeout"]
          [else "fail"])
        'elapsed_ms
        (inexact->exact (round (- (current-inexact-milliseconds) start)))))

;; P2-B rev 3: a measurement session begins with one recorded warm-up. The warm-up is
;; not a sample (a cold cache is a different workload), but it is recorded so the
;; session can be audited.
(define (measure-samples file #:load [load 3] #:repeats [repeats 5] #:timeout [timeout 900])
  (define warm-procs (start-load load))
  (define warm
    (dynamic-wind void (lambda () (measure-one file timeout)) (lambda () (stop-load warm-procs))))
  (eprintf "  warm-up: ~a ms (recorded, not a sample)~n" (hash-ref warm 'elapsed_ms))
  (define samples
    (for/list ([i (in-range repeats)])
      (define procs (start-load load))
      (define result
        (dynamic-wind void (lambda () (measure-one file timeout)) (lambda () (stop-load procs))))
      (eprintf "  sample ~a: ~a ms~n" (add1 i) (hash-ref result 'elapsed_ms))
      result))
  (define censored (censored-samples samples))
  (when (pair? censored)
    (error 'timeout-margin
           (string-append "~a of ~a samples for ~a were censored: the declaration cut the"
                          " run short, so they are not durations.")
           (length censored)
           (length samples)
           file))
  (hash 'warmup_ms (hash-ref warm 'elapsed_ms) 'samples samples))

;; A faithful concurrency sample: the target runs in a REAL surface invocation with
;; `fillers` alongside it at the surface's own job count. The target's per-file
;; elapsed_ms is read from the run's own artifact, so the number is the adapter's,
;; not a shell timer's. Censoring is disabled (--timeout) so the sample is uncensored.
;; A file whose own `@timeout` is at or below what it needs cannot be measured: the
;; adapter honours the declaration over `--timeout`, so every sample would be the
;; moment the harness killed it (a censored observation, not a duration). When that
;; is the case we measure a copy whose ONLY difference is the declaration, and say so.
(define (probe-copy-source target timeout-s)
  (define text (file->string target))
  (unless (regexp-match? #rx"^#lang racket" text)
    (error 'timeout-margin "cannot build a probe copy of ~a (no #lang line)" target))
  (define copy
    (build-path (or (path-only (path->complete-path target)) (current-directory))
                (format ".cl5-probe-~a" (file-name-from-path target))))
  ;; The metadata parser keeps the LAST match in the first 50 lines, so merely inserting a
  ;; line would be shadowed by the file's own declaration further down: replace it if present.
  (define raised
    (format ";; @timeout ~a ;; CL5 probe copy (declaration raised for measurement only)" timeout-s))
  (define with-declaration
    (cond
      [(regexp-match? #rx"[ \t]*;+[ \t]*@timeout[ \t]+[0-9]+" text)
       ;; Every declaration is replaced, not just the first: the metadata parser keeps the
       ;; LAST match in the first 50 lines, so replacing one of two would leave the old
       ;; limit in force.
       (regexp-replace* #rx"[ \t]*;+[ \t]*@timeout[ \t]+[0-9]+" text raised)]
      [else
       (regexp-replace #rx"^(#lang racket[^\n]*\n)"
                       text
                       (lambda (_whole lang) (string-append lang "\n" raised "\n")))]))
  (call-with-output-file copy #:exists 'replace (lambda (o) (write-string with-declaration o)))
  copy)

;; A sample is censored when the run did not finish on its own terms: it was killed
;; at the declaration, so its number is the kill moment, not a duration.
(define (censored-samples samples)
  (filter (lambda (s) (not (equal? (hash-ref s 'run_status 'fail) "pass"))) samples))

(define (measure-surface-samples target
                                 fillers
                                 #:job-count [job-count 4]
                                 #:repeats [repeats 5]
                                 #:timeout [timeout 900]
                                 #:allow-probe-copy? [allow-probe-copy? #t])
  (define declared (full-file-timeout target))
  ;; A copy is only needed when the declaration is TIGHTER than the probe budget. At
  ;; equality the target already runs uncensored, and a copy would change the file's
  ;; path, which path-sensitive tests can fail on.
  (define copy? (and allow-probe-copy? (not (zero? declared)) (< declared timeout)))
  (define run-target
    (if copy?
        (path->string (probe-copy-source target timeout))
        (if (path? target)
            (path->string target)
            target)))
  (define (one-run)
    (define out (make-temporary-file "cl5-margin-~a.json"))
    (define status
      (parameterize ([current-output-port (open-output-nowhere)])
        (apply system*
               (append (list (path->string (find-executable-path "racket"))
                             "scripts/run-tests/runner.rkt"
                             "--timeout"
                             (number->string timeout)
                             "--jobs"
                             (number->string job-count)
                             "--json-out"
                             (path->string out)
                             run-target)
                       fillers))))
    (define artifact (with-input-from-file out read-json))
    (delete-file out)
    (define entry
      (for/first ([f (in-list (hash-ref artifact 'files '()))]
                  #:when (equal? run-target (hash-ref f 'path "")))
        f))
    (unless entry
      (error 'timeout-margin "the run's artifact has no entry for ~a" run-target))
    (hash 'file
          target
          'status
          status
          'elapsed_ms
          (hash-ref entry 'elapsed_ms)
          'run_status
          (hash-ref entry 'status)))
  (define warm (one-run))
  (eprintf "  warm-up: ~a ms (recorded, not a sample)~n" (hash-ref warm 'elapsed_ms))
  (define samples
    (for/list ([i (in-range repeats)])
      (define s (one-run))
      (eprintf "  sample ~a: ~a ms~n" (add1 i) (hash-ref s 'elapsed_ms))
      s))
  ;; Censored samples are not durations: a sample taken at the moment the harness
  ;; killed the file is a lower bound, and building a p95 from it would understate
  ;; the true p95. Refuse rather than silently derive a limit from killed runs.
  (when (and copy? (file-exists? run-target))
    (delete-file run-target))
  (define censored (censored-samples samples))
  (when (pair? censored)
    (error
     'timeout-margin
     (string-append "~a of ~a samples for ~a were censored (run_status ~s at ~a ms): the declaration"
                    " (~a s) cut the run short, so these are not durations."
                    " Re-measure with an uncensored probe.")
     (length censored)
     (length samples)
     target
     (map (lambda (s) (hash-ref s 'run_status)) censored)
     (map (lambda (s) (hash-ref s 'elapsed_ms)) censored)
     declared))
  (hash 'probe_copy
        copy?
        'declared_limit_s
        declared
        'warmup_ms
        (hash-ref warm 'elapsed_ms)
        'samples
        samples
        'job_count
        job-count
        'fillers
        fillers))

(define (uncertainty-text sample-count job-count)
  (string-append (format "p95 = nearest-rank of ~a samples at job count ~a; " sample-count job-count)
                 "with that sample count the p95 equals the maximum sample, so it is a lower bound "
                 "on the true p95 and the derived limit inherits that uncertainty"))

;; ---- CLI ---------------------------------------------------------------------------

(define measure-path (make-parameter #f))
(define fillers (make-parameter #f))
(define load (make-parameter 3))
(define repeats (make-parameter 5))
(define uncensored (make-parameter 900))
(define evidence-out (make-parameter #f))
(define registry-out (make-parameter #f))
(define verify-path (make-parameter #f))
(define existing-limit (make-parameter #f))
(define job-count (make-parameter 4))
(define allowed-paths (make-parameter default-allowed-paths))

(define (policy-hash)
  (hasheq 'formula
          "max(existing_limit_s, ceil_30s(1.5 * p95_job_count))"
          'confidence
          0.95
          'rounding_s
          30
          'min_samples
          5
          'applied_by
          "test-file @timeout declarations"))

(define (usage-line)
  (string-append "usage: timeout-margin.rkt --measure FILE --existing-limit S [--load N] "
                 "[--repeats K] [--evidence OUT] [--to-registry OUT] | --verify REGISTRY~n"))

(module+ main
  (define probe-copy? #t)
  (command-line
   #:program "timeout-margin.rkt"
   #:once-each [("--measure") v "Test file to measure" (measure-path v)]
   [("--load") v "Competing CPU workers (job count - 1)" (load (string->number v))]
   [("--job-count") v "Job count the samples represent" (job-count (string->number v))]
   [("--repeats") v "Samples to take" (repeats (string->number v))]
   [("--timeout")
    v
    "Uncensored per-file timeout for the measurement run"
    (uncensored (string->number v))]
   [("--existing-limit")
    v
    "Current declared/effective limit in seconds"
    (existing-limit (string->number v))]
   [("--evidence") v "Write raw samples here" (evidence-out v)]
   [("--to-registry") v "Merge this measurement into a registry here" (registry-out v)]
   [("--at-real-path")
    "Measure the file itself (no probe copy) -- for path-sensitive tests whose declaration you raised yourself"
    (set! probe-copy? #f)]
   [("--fillers")
    v
    "Comma-separated files run alongside the target (real surface sample)"
    (fillers (string-split v ","))]
   [("--verify") v "Verify an existing registry" (verify-path v)]
   #:args ()
   (cond
     [(verify-path)
      (define summary (verify-registry (with-input-from-file (verify-path) read-json)))
      (printf "registry verified: ~a entr(y/ies)~n" (hash-ref summary 'entries))
      (for ([p (in-list (hash-ref summary 'paths))])
        (printf "  ~a~n" p))]
     [(measure-path)
      (unless (existing-limit)
        (error 'timeout-margin "--existing-limit is required (existing declarations are floors)"))
      (define run
        (if (fillers)
            (measure-surface-samples (measure-path)
                                     (fillers)
                                     #:job-count (job-count)
                                     #:repeats (repeats)
                                     #:timeout (uncensored)
                                     #:allow-probe-copy? probe-copy?)
            (measure-samples (measure-path)
                             #:load (load)
                             #:repeats (repeats)
                             #:timeout (uncensored))))
      (define samples (hash-ref run 'samples))
      (define warmup-ms (hash-ref run 'warmup_ms))
      (define ms (map (lambda (s) (hash-ref s 'elapsed_ms)) samples))
      (define p95 (percentile ms 0.95))
      (define e
        (registry-entry (measure-path)
                        (existing-limit)
                        (job-count)
                        ms
                        p95
                        (derive-limit (existing-limit) p95)
                        (uncertainty-text (length ms) (job-count))))
      (define e-json (hash-set (registry-entry->jsexpr e) 'probe_copy (hash-ref run 'probe_copy #f)))
      (define e*
        (if (hash-ref run 'probe_copy #f)
            (hash-set e-json
                      'uncertainty
                      (string-append
                       (hash-ref e-json 'uncertainty)
                       "; samples taken from a sibling probe copy (only"
                       " the @timeout line differs), because the declared limit would have"
                       " censored every run"))
            e-json))
      (when (evidence-out)
        (call-with-output-file
         (evidence-out)
         #:exists 'replace
         (lambda (o)
           (write-json
            (hash 'schema schema 'probe_copy (hash-ref run 'probe_copy #f) 'entry e* 'samples samples)
            o)
           (newline o))))
      (when (registry-out)
        (define previous
          (if (file-exists? (registry-out))
              (hash-ref (with-input-from-file (registry-out) read-json) 'entries '())
              '()))
        (define merged
          (cons e*
                (filter (lambda (x) (not (equal? (hash-ref x 'path #f) (measure-path)))) previous)))
        (define reg
          (hasheq 'schema
                  schema
                  'policy
                  (policy-hash)
                  'allowed_paths
                  (allowed-paths)
                  'entries
                  (sort merged string<? #:key (lambda (x) (hash-ref x 'path)))))
        (verify-registry reg)
        (call-with-output-file (registry-out)
                               #:exists 'replace
                               (lambda (o)
                                 (write-json reg o)
                                 (newline o))))
      (printf "~a: p95 ~a ms -> limit ~a s (existing ~a s)~n"
              (measure-path)
              p95
              (registry-entry-limit-s e)
              (existing-limit))]
     [else
      (eprintf "~a" (usage-line))
      (exit 1)])))
