#lang racket/base

;; W4: Reproducible 20-PR cohort evidence tooling.
;;
;; Turns expiring GitHub workflow artifacts into a deterministic, reviewable
;; activation record with pre-registered sampling and statistics.
;;
;; A "cohort" is 20 consecutive eligible unique PR head SHAs.  Each SHA has
;; exactly one final successful timing sample (the timing datum) plus zero or
;; more failed/cancelled/rerun attempts (reliability evidence).  The cohort
;; manifest is a checked-in JSON file that names every SHA, every attempt,
;; every exclusion, and every retained artifact digest.
;;
;; This script validates the manifest, computes linear-interpolation p50/p95,
;; and emits a report whose `--check` mode reproduces byte-identically from
;; the manifest alone — no network, no database, no external service.
;;
;; Usage:
;;   racket scripts/run-tests/cohort-report.rkt --manifest <path>
;;        [--out-json path] [--out-md path] [--check]
;;
;; Determinism: identical manifest inputs produce byte-identical report
;; outputs.  All ordering is by explicit keys; nothing host- or
;; time-dependent is ever embedded.

(require racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/match
         racket/string
         json)

;; manifest loading + validation
(provide load-cohort-manifest
         cohort-manifest?
         validate-cohort
         validation-ok?
         validation-errors
         validation-warnings
         ;; cohort analysis
         cohort-timing-samples
         cohort-attempts-summary
         cohort-quantile
         ;; report generation
         cohort-report-jsexpr
         cohort-report-json-string
         cohort-report-md-string
         ;; check mode
         cohort-check
         ;; constants
         expected-cohort-size
         cohort-schema-version
         known-exclusion-reasons
         known-config-lanes
         known-config-schedulers
         known-config-orderings
         known-cohort-statuses
         ;; helpers exposed for testing
         sha-eligible?
         sha-has-timing-sample?
         sha-final-success-attempt
         manifest-digest
         decision-lane-verdict
         decision-report-jsexpr
         cohort-decision-md-string
         fast-queue-gate-text
         fast-p50-max-seconds
         fast-p95-max-seconds)

;; ============================================================
;; Constants
;; ============================================================

(define cohort-schema-version 1)
(define expected-cohort-size 20)

;; Named mechanical exclusion reasons.  Every exclusion MUST use one of these
;; exact strings — no free-text rejection is accepted.
(define known-exclusion-reasons
  '("missing-lane-artifact" "incompatible-scheduler"
                            "incompatible-config"
                            "inventory-mismatch"
                            "artifact-corrupt"
                            "artifact-expired"
                            "non-unique-sha"))

;; Paired configuration schema (v1.00.25 W0: C1 shadow cohort start).
;; Lanes and orderings known to the configuration schema; schedulers extend
;; the flat-row set with the work-conserving "queue" scheduler, which is
;; shadow-only: required CI keeps running batch/serial and nothing in this
;; module activates a queue default.
(define known-config-lanes '("fast" "security"))
(define known-config-schedulers '("batch" "queue" "serial"))
(define known-config-orderings '("fifo" "lpt"))
(define known-cohort-statuses '("started" "open" "closed" "cancelled"))
;; Cohort statuses in which shadow configurations may still carry empty
;; attempt lists (the paired runs have not landed yet).
(define open-cohort-statuses '("started" "open"))
(define sha40-regexp #px"^[0-9a-f]{40}$")
(define (sha40? s)
  (and (string? s) (regexp-match? sha40-regexp s)))

;; ============================================================
;; Manifest loading
;; ============================================================

;; A cohort manifest is a JSON object (parsed to a hasheq with symbol keys):
;;   {"cohort-id": "...", "milestone": "...", "schema-version": 1,
;;    "expected-count": 20,
;;    "shas": [ {sha, pr, scheduler, ordering, attempts: [...],
;;               inventory-digest, file-count, test-count,
;;               pass, fail, timeout, skip, zero-test,
;;               flakes, parallel-only-failures, prepared-env,
;;               queue-wait-seconds, queue-depth, runner-minutes} ... ],
;;    "exclusions": [ {sha, reason, detail} ... ] }

(define (cohort-manifest? v)
  (and (hash? v)
       (hash-has-key? v 'cohort-id)
       (hash-has-key? v 'milestone)
       (hash-has-key? v 'schema-version)
       (hash-has-key? v 'shas)))

(define (load-cohort-manifest path)
  (cond
    [(not (file-exists? path)) (error 'load-cohort-manifest "manifest file not found: ~a" path)]
    [else
     (define v
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (call-with-input-file path read-json)))
     (cond
       [(not v) (error 'load-cohort-manifest "manifest is not valid JSON: ~a" path)]
       [(not (cohort-manifest? v))
        (error 'load-cohort-manifest "manifest does not match cohort schema: ~a" path)]
       [else v])]))

;; ============================================================
;; SHA-level helpers
;; ============================================================

(define (sha-eligible? sha-entry)
  ;; A SHA is eligible if it has at least one attempt and exactly one of them
  ;; is marked as the timing sample (final successful attempt).
  (and (hash? sha-entry)
       (hash-has-key? sha-entry 'sha)
       (hash-has-key? sha-entry 'attempts)
       (let ([attempts (hash-ref sha-entry 'attempts '())])
         (and (list? attempts)
              (not (null? attempts))
              (= 1 (count (lambda (a) (hash-ref a 'timing-sample #f)) attempts))))))

(define (sha-has-timing-sample? sha-entry)
  (and (hash? sha-entry)
       (hash-has-key? sha-entry 'attempts)
       (let ([attempts (hash-ref sha-entry 'attempts '())])
         (and (list? attempts)
              (positive? (count (lambda (a) (hash-ref a 'timing-sample #f)) attempts))))))

(define (sha-final-success-attempt sha-entry)
  ;; Returns the timing-sample attempt (the final successful one), or #f.
  (define attempts (hash-ref sha-entry 'attempts '()))
  (findf (lambda (a) (hash-ref a 'timing-sample #f)) attempts))

(define (sha-timing-seconds sha-entry)
  (define ts (sha-final-success-attempt sha-entry))
  (and ts (hash-ref ts 'elapsed-seconds #f)))

;; ============================================================
;; Validation
;; ============================================================

(struct validation-result (ok errors warnings) #:transparent)

(define (validation-ok? vr)
  (validation-result-ok vr))
(define (validation-errors vr)
  (validation-result-errors vr))
(define (validation-warnings vr)
  (validation-result-warnings vr))

;; Paired-configuration validation (v1.00.25 W0).  When the manifest carries
;; a `configurations` block, every configuration row must name its scheduler,
;; ordering, lane, start SHA, eligible-SHA list, per-SHA attempts, and the
;; inventory digest for every SHA.  The required configuration must be the
;; required-lane baseline (fast/batch/fifo); all paired configurations must
;; run over the identical ordered eligible-SHA list; and every selected-file
;; inventory digest must equal the required-lane inventory for the same SHA —
;; any mismatch is a cohort error, never silently ignored.
(define (validate-configurations manifest err!)
  (define configs (hash-ref manifest 'configurations #f))
  (unless (not configs)
    (define cohort-status (hash-ref manifest 'cohort-status #f))
    (unless (member cohort-status known-cohort-statuses)
      (err! (format
             "manifest has configurations but unknown/missing cohort-status: ~a (must be one of ~a)"
             cohort-status
             known-cohort-statuses)))
    (unless (and (list? configs) (not (null? configs)))
      (err! "configurations must be a non-empty list of configuration objects"))
    (when (and (list? configs) (not (null? configs)))
      (define required-configs (filter (lambda (c) (hash-ref c 'required #f)) configs))
      (unless (= 1 (length required-configs))
        (err!
         (format
          "configurations must declare exactly one required configuration (the required-lane baseline); found ~a"
          (length required-configs))))
      (define baseline (and (= 1 (length required-configs)) (first required-configs)))
      (when baseline
        (unless (and (equal? (hash-ref baseline 'lane #f) "fast")
                     (equal? (hash-ref baseline 'scheduler #f) "batch")
                     (equal? (hash-ref baseline 'ordering #f) "fifo"))
          (err! "required configuration must be the required-lane baseline fast/batch/fifo")))
      (define exclusions (hash-ref manifest 'exclusions '()))
      (define expected (hash-ref manifest 'expected-count expected-cohort-size))
      (define allowed-lengths (list expected (+ expected (length exclusions))))
      (define baseline-eligible (and baseline (hash-ref baseline 'eligible-shas #f)))
      (define baseline-digests
        (and baseline
             (for/hash ([r (in-list (hash-ref baseline 'shas '()))])
               (values (hash-ref r 'sha "") (hash-ref r 'inventory-digest "")))))
      (define cohort-open? (and (member cohort-status open-cohort-statuses) #t))
      (for ([c (in-list configs)]
            [ci (in-naturals)])
        (define config-id (hash-ref c 'config-id (format "#~a" ci)))
        ;; required fields
        (for ([field (in-list '(config-id lane scheduler ordering start-sha eligible-shas shas))])
          (unless (hash-has-key? c field)
            (err!
             (format "configuration ~a (index ~a) missing required field: ~a" config-id ci field))))
        (define lane (hash-ref c 'lane #f))
        (define scheduler (hash-ref c 'scheduler #f))
        (define ordering (hash-ref c 'ordering #f))
        (define start-sha (hash-ref c 'start-sha #f))
        (unless (member lane known-config-lanes)
          (err! (format "configuration ~a has unknown lane: ~a" config-id lane)))
        (unless (member scheduler known-config-schedulers)
          (err! (format "configuration ~a has incompatible configuration scheduler: ~a"
                        config-id
                        scheduler)))
        (unless (member ordering known-config-orderings)
          (err! (format "configuration ~a has unknown ordering: ~a" config-id ordering)))
        (unless (and config-id
                     lane
                     scheduler
                     ordering
                     (equal? config-id (string-append lane "/" scheduler "/" ordering)))
          (err! (format "configuration config-id ~a does not match lane/scheduler/ordering (~a/~a/~a)"
                        config-id
                        lane
                        scheduler
                        ordering)))
        (unless (sha40? start-sha)
          (err! (format "configuration ~a start-sha must be 40 lowercase hex chars: ~a"
                        config-id
                        start-sha)))
        (define manifest-start (hash-ref manifest 'start-sha #f))
        (when (and (sha40? start-sha) manifest-start (not (equal? start-sha manifest-start)))
          (err! (format "configuration ~a start-sha ~a does not match cohort start-sha ~a"
                        config-id
                        start-sha
                        manifest-start)))
        ;; eligible-SHA list
        (define eligible (hash-ref c 'eligible-shas #f))
        (unless (list? eligible)
          (err! (format "configuration ~a eligible-shas must be a list" config-id)))
        (when (list? eligible)
          (unless (member (length eligible) allowed-lengths)
            (err! (format "configuration ~a eligible-shas has ~a entries; expected one of ~a"
                          config-id
                          (length eligible)
                          allowed-lengths)))
          (define seen (make-hash))
          (for ([s (in-list eligible)])
            (unless (sha40? s)
              (err! (format "configuration ~a eligible SHA is not 40 lowercase hex: ~a" config-id s)))
            (when (hash-has-key? seen s)
              (err! (format "duplicate SHA in configuration ~a: ~a" config-id s)))
            (hash-set! seen s #t))
          (when (and baseline-eligible (not (equal? eligible baseline-eligible)))
            (err!
             (format
              "configuration ~a eligible-SHA list mismatch: paired configurations must run over the identical ordered eligible-SHA list"
              config-id))))
        ;; per-SHA rows
        (define rows (hash-ref c 'shas #f))
        (unless (list? rows)
          (err! (format "configuration ~a shas must be a list of per-SHA rows" config-id)))
        (when (list? rows)
          (when (list? eligible)
            (unless (= (length rows) (length eligible))
              (err! (format "configuration ~a has ~a shas rows but ~a eligible SHAs"
                            config-id
                            (length rows)
                            (length eligible))))
            (for ([r (in-list rows)]
                  [s (in-list eligible)]
                  [ri (in-naturals)])
              (unless (equal? (hash-ref r 'sha #f) s)
                (err! (format "configuration ~a shas row ~a does not match eligible-SHA ~a in order"
                              config-id
                              ri
                              s)))))
          (for ([r (in-list rows)]
                [ri (in-naturals)])
            (define rs (hash-ref r 'sha (format "#~a" ri)))
            (unless (hash-has-key? r 'attempts)
              (err! (format "configuration ~a SHA ~a missing required field: attempts" config-id rs)))
            (define attempts (hash-ref r 'attempts #f))
            (when (list? attempts)
              (for ([a (in-list attempts)]
                    [ai (in-naturals)])
                (unless (and (hash? a)
                             (hash-has-key? a 'result)
                             (string? (hash-ref a 'result))
                             (hash-has-key? a 'timing-sample)
                             (boolean? (hash-ref a 'timing-sample)))
                  (err! (format "configuration ~a SHA ~a attempt ~a missing result/timing-sample flag"
                                config-id
                                rs
                                ai)))))
            (define d (hash-ref r 'inventory-digest #f))
            (when (or (not d) (equal? d ""))
              (err!
               (format "configuration ~a SHA ~a has missing/empty inventory-digest" config-id rs)))
            (when (and d baseline-digests (hash-has-key? baseline-digests rs))
              (define baseline-digest (hash-ref baseline-digests rs))
              (unless (equal? d baseline-digest)
                (err! (format (string-append "inventory mismatch for configuration ~a SHA ~a: "
                                             "expected required-lane baseline digest ~a, got ~a "
                                             "— cohort error, never silently ignored")
                              config-id
                              rs
                              baseline-digest
                              d))))
            (when (and baseline (equal? c baseline))
              (unless (sha-eligible? r)
                (err!
                 (format
                  "required-lane baseline configuration SHA ~a is not eligible: must have exactly one timing-sample attempt (final successful)"
                  rs))))
            (when (and (not cohort-open?) baseline (not (equal? c baseline)))
              (when (and attempts (null? attempts))
                (err! (format "closed cohort configuration ~a SHA ~a has no attempts collected"
                              config-id
                              rs)))
              (unless (sha-eligible? r)
                (err!
                 (format
                  "closed cohort configuration SHA ~a is not eligible: must have exactly one timing-sample attempt (final successful)"
                  rs))))))
        (void))
      ;; flat view cross-check
      (when baseline-eligible
        (define flat (map (lambda (s) (hash-ref s 'sha #f)) (hash-ref manifest 'shas '())))
        (unless (equal? flat baseline-eligible)
          (err!
           "flat shas do not match the required-lane configuration rows (order must be identical)"))))))

(define (validate-cohort manifest)
  (define errors '())
  (define warnings '())
  (define (err! msg)
    (set! errors (append errors (list msg))))
  (define (warn! msg)
    (set! warnings (append warnings (list msg))))

  (define shas (hash-ref manifest 'shas '()))
  (define exclusions (hash-ref manifest 'exclusions '()))
  (define expected (hash-ref manifest 'expected-count expected-cohort-size))

  ;; 1. Exactly 20 SHAs (or expected-count) — reject silently truncated cohorts.
  (define n-shas (length shas))
  (cond
    [(= n-shas expected)
     ;; ok — but warn if the expected count differs from 20
     (unless (= expected expected-cohort-size)
       (warn! (format "expected-count ~a differs from canonical ~a" expected expected-cohort-size)))]
    [(< n-shas expected)
     ;; Fewer than expected: only valid if every missing SHA has a named
     ;; mechanical exclusion.  Silently truncated cohorts are rejected.
     (define n-exclusions (length exclusions))
     (cond
       ;; All gaps accounted for by named exclusions — acceptable.
       [(= (+ n-shas n-exclusions) expected) (void)]
       [else
        (err!
         (format
          "cohort has ~a SHAs but expected ~a; only ~a exclusions named — \
                       silently truncated cohort rejected"
          n-shas
          expected
          n-exclusions))])]
    [else (err! (format "cohort has ~a SHAs but expected ~a — too many" n-shas expected))])

  ;; 2. No duplicate SHAs.
  (define sha-list (map (lambda (s) (hash-ref s 'sha #f)) shas))
  (define seen (make-hash))
  (for ([sha (in-list sha-list)])
    (when (and sha (hash-has-key? seen sha))
      (err! (format "duplicate SHA in cohort: ~a" sha)))
    (when sha
      (hash-set! seen sha #t)))

  ;; 3. Every SHA must be eligible (one timing sample = final success).
  (for ([s (in-list shas)]
        [i (in-naturals)])
    (cond
      [(not (sha-eligible? s))
       (err!
        (format
         "SHA ~a (index ~a) is not eligible: must have exactly one \
                      timing-sample attempt (final successful)"
         (hash-ref s 'sha "?")
         i))]))

  ;; 4. Every SHA must have required fields.
  (for ([s (in-list shas)]
        [i (in-naturals)])
    (for ([field (in-list '(sha scheduler
                                ordering
                                inventory-digest
                                file-count
                                test-count
                                pass
                                fail
                                timeout
                                skip
                                flakes
                                parallel-only-failures
                                prepared-env
                                queue-wait-seconds
                                queue-depth
                                runner-minutes))])
      (unless (hash-has-key? s field)
        (err!
         (format "SHA ~a (index ~a) missing required field: ~a" (hash-ref s 'sha "?") i field)))))

  ;; 5. Zero-test detection: a SHA with test-count 0 must be flagged.
  (for ([s (in-list shas)])
    (when (and (hash-has-key? s 'test-count) (zero? (hash-ref s 'test-count 0)))
      (unless (hash-ref s 'zero-test #f)
        (err! (format "SHA ~a has test-count 0 but zero-test flag not set" (hash-ref s 'sha "?"))))))

  ;; 6. Inventory digest must be present and non-empty for every SHA.
  (for ([s (in-list shas)])
    (define d (hash-ref s 'inventory-digest #f))
    (when (or (not d) (equal? d ""))
      (err! (format "SHA ~a has missing/empty inventory-digest" (hash-ref s 'sha "?")))))

  ;; 7. Scheduler/config must be one of the known compatible values.
  (for ([s (in-list shas)])
    (define sched (hash-ref s 'scheduler #f))
    (unless (member sched '("batch" "serial"))
      (err! (format "SHA ~a has incompatible scheduler: ~a" (hash-ref s 'sha "?") sched))))

  ;; 8. Exclusions must use named mechanical reasons.
  (for ([e (in-list exclusions)]
        [i (in-naturals)])
    (define reason (hash-ref e 'reason #f))
    (unless (member reason known-exclusion-reasons)
      (err! (format "exclusion ~a has unnamed reason: ~a (must be one of ~a)"
                    i
                    reason
                    known-exclusion-reasons))))

  ;; 9. Exclusion SHAs must not also appear in the cohort SHAs.
  (for ([e (in-list exclusions)])
    (define esha (hash-ref e 'sha #f))
    (when (and esha (member esha sha-list))
      (err! (format "exclusion SHA ~a also appears in cohort — contradiction" esha))))

  ;; 10. prepared-env must be a known value.
  (for ([s (in-list shas)])
    (define pe (hash-ref s 'prepared-env #f))
    (unless (member pe '("match" "rebuild" "cached"))
      (err! (format "SHA ~a has unknown prepared-env: ~a" (hash-ref s 'sha "?") pe))))

  ;; 11. Paired configurations (when present): schema, pairing, inventory
  ;; equality, and lifecycle rules.
  (validate-configurations manifest err!)

  (validation-result (null? errors) errors warnings))

;; ============================================================
;; Statistics: linear-interpolation percentile estimator
;; (adopted from baseline-report.rkt W0)
;; ============================================================

(define (cohort-quantile xs q)
  (cond
    [(null? xs) #f]
    [else
     (define s (sort (map (lambda (x) (exact->inexact x)) xs) <))
     (define n (length s))
     (define k (* q (sub1 n)))
     (define lo (inexact->exact (floor k)))
     (define hi (inexact->exact (ceiling k)))
     (if (= lo hi)
         (list-ref s lo)
         (/ (+ (list-ref s lo) (list-ref s hi)) 2.0))]))

;; ============================================================
;; Cohort analysis
;; ============================================================

(define (cohort-timing-samples manifest)
  ;; Extract the final successful timing sample (elapsed-seconds) for each
  ;; eligible SHA in manifest order.
  (define shas (hash-ref manifest 'shas '()))
  (filter values
          (map (lambda (s)
                 (cond
                   [(sha-eligible? s) (sha-timing-seconds s)]
                   [else #f]))
               shas)))

(define (cohort-attempts-summary manifest)
  ;; Summarize all attempts across the cohort for reliability evidence.
  (define shas (hash-ref manifest 'shas '()))
  (define all-attempts (append* (map (lambda (s) (hash-ref s 'attempts '())) shas)))
  (hasheq 'total-attempts
          (length all-attempts)
          'failures
          (count (lambda (a) (equal? (hash-ref a 'result #f) "failure")) all-attempts)
          'cancelled
          (count (lambda (a) (equal? (hash-ref a 'result #f) "cancelled")) all-attempts)
          'successes
          (count (lambda (a) (equal? (hash-ref a 'result #f) "success")) all-attempts)
          'reruns
          (count (lambda (a) (equal? (hash-ref a 'result #f) "rerun")) all-attempts)))

;; ============================================================
;; Manifest digest (for checksum verification)
;; ============================================================

(define (manifest-digest manifest)
  ;; Produce a deterministic digest of the manifest's normalized form.
  ;; The digest is computed over the canonical JSON string so that any
  ;; byte-level difference in inputs is detected.
  (define json-str (jsexpr->string (normalize-manifest manifest)))
  (define bytes (string->bytes/utf-8 json-str))
  ;; Simple deterministic digest: sum of byte values as hex.  This is NOT
  ;; cryptographic — it's a change-detection checksum sufficient for
  ;; `--check` regeneration verification.  For cryptographic integrity the
  ;; per-SHA inventory-digest fields are the authoritative digests.
  (define sum (for/sum ([b (in-bytes bytes)]) b))
  (format "check:~x:~a" sum (bytes-length bytes)))

(define (normalize-manifest manifest)
  ;; Produce a canonical jsexpr with sorted keys (hasheq already sorts in
  ;; jsexpr->string, but we also normalize the structure).
  manifest)

;; ============================================================
;; Report generation
;; ============================================================

;; Paired-configurations report section.  Returns #f when the manifest does
;; not declare configurations (legacy shape is preserved byte-for-byte).
(define (report-configurations-section manifest)
  (define configs (hash-ref manifest 'configurations #f))
  (and configs
       (let ([baseline-c (findf (lambda (c) (hash-ref c 'required #f)) configs)])
         (define baseline-digests
           (if baseline-c
               (for/hash ([r (in-list (hash-ref baseline-c 'shas '()))])
                 (values (hash-ref r 'sha "") (hash-ref r 'inventory-digest "")))
               (hash)))
         (for/list ([c (in-list configs)])
           (define rows (hash-ref c 'shas '()))
           (hasheq 'config-id
                   (hash-ref c 'config-id "?")
                   'lane
                   (hash-ref c 'lane "?")
                   'scheduler
                   (hash-ref c 'scheduler "?")
                   'ordering
                   (hash-ref c 'ordering "?")
                   'start-sha
                   (hash-ref c 'start-sha "?")
                   'required
                   (hash-ref c 'required #f)
                   'eligible-count
                   (length (hash-ref c 'eligible-shas '()))
                   'attempts-recorded
                   (count (lambda (r) (positive? (length (hash-ref r 'attempts '())))) rows)
                   'inventory-digest
                   (string-join (sort (map (lambda (r) (hash-ref r 'inventory-digest "")) rows)
                                      string<?)
                                "|")
                   'inventory-equal-to-baseline
                   (andmap (lambda (r)
                             (equal? (hash-ref r 'inventory-digest #f)
                                     (hash-ref baseline-digests (hash-ref r 'sha "") #f)))
                           rows))))))

;; ============================================================
;; W1 promotion decision (paired lanes)
;;
;; The decision is explicit and fail-closed: a lane promotes only when its
;; paired evidence is complete and every gate passes.  A missed gate produces
;; hold, never a revised target; missing evidence produces hold with named
;; reasons.  Stale or missing LPT duration evidence falls back to FIFO with a
;; named reason.
;; ============================================================

(define fast-p50-max-seconds 130.0)
(define fast-p95-max-seconds 145.0)

(define fast-queue-gate-text
  (format
   (string-append
    "Initial fast threshold: queue fast execution p50 ≤ ~a s and p95 ≤ ~a s with no reliability"
    " regression versus the paired batch baseline on the same SHAs (roadmap v1.00.25 §6, W1)."
    " A missed gate produces hold, never a revised target.")
   (exact->inexact fast-p50-max-seconds)
   (exact->inexact fast-p95-max-seconds)))

(define decision-lanes
  (list (hasheq 'lane "fast-queue" 'config-id "fast/queue/fifo" 'timing-gate #t 'ordering-proof #f)
        (hasheq 'lane
                "fast-LPT"
                'config-id
                "fast/queue/lpt"
                'timing-gate
                #t
                'ordering-proof
                "fast/queue/fifo")
        (hasheq 'lane
                "security-queue"
                'config-id
                "security/queue/fifo"
                'timing-gate
                #f
                'ordering-proof
                #f)))

(define (configuration-by-config-id manifest config-id)
  (findf (lambda (c) (equal? (hash-ref c 'config-id "?") config-id))
         (hash-ref manifest 'configurations '())))

(define (configuration-timing-samples config)
  ;; Final successful timing sample per eligible SHA row.
  (filter values
          (map (lambda (s) (and (sha-eligible? s) (sha-timing-seconds s)))
               (hash-ref config 'shas '()))))

(define (configuration-complete? config)
  (define rows (hash-ref config 'shas '()))
  (and (pair? rows)
       (= (length rows) (length (hash-ref config 'eligible-shas '())))
       (andmap sha-eligible? rows)))

(define (configuration-attempts-summary config)
  ;; Per-configuration reliability evidence (same shape as
  ;; cohort-attempts-summary, scoped to one configuration).
  (define rows (hash-ref config 'shas '()))
  (define all-attempts (append* (map (lambda (s) (hash-ref s 'attempts '())) rows)))
  (hasheq 'total-attempts
          (length all-attempts)
          'failures
          (count (lambda (a) (equal? (hash-ref a 'result #f) "failure")) all-attempts)
          'cancelled
          (count (lambda (a) (equal? (hash-ref a 'result #f) "cancelled")) all-attempts)
          'successes
          (count (lambda (a) (equal? (hash-ref a 'result #f) "success")) all-attempts)
          'reruns
          (count (lambda (a) (equal? (hash-ref a 'result #f) "rerun")) all-attempts)))

(define (configuration-attempts-recorded config)
  (count (lambda (r) (positive? (length (hash-ref r 'attempts '())))) (hash-ref config 'shas '())))

(define (configuration-inventory-equal? config baseline-digests)
  (andmap (lambda (r)
            (equal? (hash-ref r 'inventory-digest #f)
                    (hash-ref baseline-digests (hash-ref r 'sha "") #f)))
          (hash-ref config 'shas '())))

(define (configuration-inventory-mismatch-count config baseline-digests)
  (count (lambda (r)
           (not (equal? (hash-ref r 'inventory-digest #f)
                        (hash-ref baseline-digests (hash-ref r 'sha "") #f))))
         (hash-ref config 'shas '())))

(define (reliability-regression? leg-summary baseline-summary)
  (> (+ (hash-ref leg-summary 'failures 0)
        (hash-ref leg-summary 'cancelled 0)
        (hash-ref leg-summary 'reruns 0))
     (+ (hash-ref baseline-summary 'failures 0)
        (hash-ref baseline-summary 'cancelled 0)
        (hash-ref baseline-summary 'reruns 0))))

(define (decision-lane-verdict manifest lane-id)
  (define lanes (filter (lambda (l) (equal? (hash-ref l 'lane) lane-id)) decision-lanes))
  (define lane (and (pair? lanes) (car lanes)))
  (unless lane
    (error 'decision-lane-verdict "unknown decision lane: ~a" lane-id))
  (define config-id (hash-ref lane 'config-id))
  (define config (configuration-by-config-id manifest config-id))
  (define baseline-config
    (findf (lambda (c) (hash-ref c 'required #f)) (hash-ref manifest 'configurations '())))
  (define baseline-digests
    (if baseline-config
        (for/hash ([r (in-list (hash-ref baseline-config 'shas '()))])
          (values (hash-ref r 'sha "") (hash-ref r 'inventory-digest "")))
        (hash)))
  (define baseline-summary
    (if baseline-config
        (configuration-attempts-summary baseline-config)
        (hasheq 'failures 0 'cancelled 0 'reruns 0)))
  (define reasons '())
  (define (fail! reason)
    (set! reasons (cons reason reasons)))

  (cond
    [(not config)
     (fail! (format "configuration ~a is not registered in the cohort manifest" config-id))]
    [else
     (define complete? (configuration-complete? config))
     (define samples (configuration-timing-samples config))
     (define rows (hash-ref config 'shas '()))
     (define summary (configuration-attempts-summary config))
     (define inv-equal? (configuration-inventory-equal? config baseline-digests))
     (define mismatch-count (configuration-inventory-mismatch-count config baseline-digests))
     (define p50 (and (pair? samples) (cohort-quantile samples 0.50)))
     (define p95 (and (pair? samples) (cohort-quantile samples 0.95)))
     (define timing-gate? (hash-ref lane 'timing-gate #f))
     (define ordering-proof-config
       (and (hash-ref lane 'ordering-proof #f)
            (configuration-by-config-id manifest (hash-ref lane 'ordering-proof))))
     (define ordering-proof-possible?
       (and ordering-proof-config (configuration-complete? ordering-proof-config)))

     (unless complete?
       (fail!
        (format
         "evidence incomplete for ~a: ~a of ~a SHAs have exactly one successful timing sample; cohort status is ~a"
         config-id
         (length samples)
         (length (hash-ref config 'eligible-shas '()))
         (hash-ref manifest 'cohort-status "?"))))
     (unless inv-equal?
       (fail! (format "selected inventory differs from the batch baseline on ~a SHA(s) (config ~a)"
                      mismatch-count
                      config-id)))
     (when (reliability-regression? summary baseline-summary)
       (fail!
        (format
         "reliability regression versus the paired batch baseline: leg failures+cancelled+reruns ~a > baseline ~a"
         (+ (hash-ref summary 'failures 0)
            (hash-ref summary 'cancelled 0)
            (hash-ref summary 'reruns 0))
         (+ (hash-ref baseline-summary 'failures 0)
            (hash-ref baseline-summary 'cancelled 0)
            (hash-ref baseline-summary 'reruns 0)))))
     (when timing-gate?
       (when (and p50 (> p50 fast-p50-max-seconds))
         (fail! (format
                 "p50 ~a s exceeds the ~a s fast target; the target is never revised inside this wave"
                 p50
                 fast-p50-max-seconds)))
       (when (and p95 (> p95 fast-p95-max-seconds))
         (fail! (format
                 "p95 ~a s exceeds the ~a s fast target; the target is never revised inside this wave"
                 p95
                 fast-p95-max-seconds))))
     (when (hash-ref lane 'ordering-proof #f)
       (cond
         [(not ordering-proof-possible?)
          (fail!
           (format
            "stale or missing duration evidence: LPT falls back to FIFO ordering (named reason: paired ~a leg evidence incomplete)"
            (hash-ref lane 'ordering-proof)))]
         [else
          (define fifo-digests
            (for/hash ([r (in-list (hash-ref ordering-proof-config 'shas '()))])
              (values (hash-ref r 'sha "") (hash-ref r 'inventory-digest ""))))
          (define divergent (configuration-inventory-mismatch-count config fifo-digests))
          (unless (zero? divergent)
            (fail!
             (format
              "LPT ordering-only proof violated: per-SHA selected inventory differs from the ~a leg on ~a SHA(s)"
              (hash-ref lane 'ordering-proof)
              divergent)))]))

     (define numbers
       (apply hasheq
              (append (if timing-gate?
                          (list 'p50-seconds (or p50 #f) 'p95-seconds (or p95 #f))
                          (list 'p50-seconds #f 'p95-seconds #f))
                      (list 'samples
                            (length samples)
                            'complete
                            complete?
                            'attempts-recorded
                            (configuration-attempts-recorded config)
                            'failures
                            (hash-ref summary 'failures 0)
                            'cancelled
                            (hash-ref summary 'cancelled 0)
                            'reruns
                            (hash-ref summary 'reruns 0)
                            'inventory-equal-to-baseline
                            (and complete? inv-equal?)))))

     (hasheq 'lane
             lane-id
             'config-id
             config-id
             'timing-gate
             timing-gate?
             'verdict
             (if (null? reasons) "promote" "hold")
             'reasons
             (reverse reasons)
             'numbers
             numbers
             'gate-text
             (if timing-gate? fast-queue-gate-text selected-inventory-gate-text))]))

(define selected-inventory-gate-text
  (string-append "Selected-inventory equality and no reliability regression versus the paired"
                 " batch baseline on the same SHAs (roadmap v1.00.25 §6, W1)."))

(define (decision-report-jsexpr manifest)
  (define lane-verdicts
    (map (lambda (l) (decision-lane-verdict manifest (hash-ref l 'lane))) decision-lanes))
  (define baseline-config
    (findf (lambda (c) (hash-ref c 'required #f)) (hash-ref manifest 'configurations '())))
  (define baseline-samples
    (if baseline-config
        (configuration-timing-samples baseline-config)
        '()))
  (hasheq 'decision-version
          "w1-decision-v1"
          'cohort-id
          (hash-ref manifest 'cohort-id "?")
          'cohort-status
          (hash-ref manifest 'cohort-status "?")
          'baseline-config
          (if baseline-config
              (hash-ref baseline-config 'config-id "?")
              #f)
          'baseline
          (hasheq 'p50-seconds
                  (and (pair? baseline-samples) (cohort-quantile baseline-samples 0.50))
                  'p95-seconds
                  (and (pair? baseline-samples) (cohort-quantile baseline-samples 0.95))
                  'samples
                  (length baseline-samples)
                  'attempts-summary
                  (if baseline-config
                      (configuration-attempts-summary baseline-config)
                      (hasheq)))
          'gates
          (hasheq 'fast-p50-max-seconds
                  (exact->inexact fast-p50-max-seconds)
                  'fast-p95-max-seconds
                  (exact->inexact fast-p95-max-seconds)
                  'gate-text
                  fast-queue-gate-text)
          'lanes
          lane-verdicts
          'overall-verdict
          (if (andmap (lambda (l) (equal? (hash-ref l 'verdict) "promote")) lane-verdicts)
              "promote"
              "hold")))

(define (decision-md-lane-line lane)
  (define nums (hash-ref lane 'numbers))
  (format "| ~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a |"
          (hash-ref lane 'lane)
          (hash-ref lane 'config-id)
          (hash-ref lane 'verdict)
          (if (hash-has-key? nums 'p50-seconds)
              (format "~a" (hash-ref nums 'p50-seconds))
              "n/a")
          (if (hash-has-key? nums 'p95-seconds)
              (format "~a" (hash-ref nums 'p95-seconds))
              "n/a")
          (hash-ref nums 'attempts-recorded 0)
          (hash-ref nums 'failures 0)
          (hash-ref nums 'cancelled 0)
          (hash-ref nums 'reruns 0)
          (if (hash-ref nums 'inventory-equal-to-baseline #f) "yes" "NO")))

(define (decision-reasons-lines lane)
  (if (null? (hash-ref lane 'reasons))
      (list (format "- ~a: all gates passed (promote)." (hash-ref lane 'lane)))
      (for/list ([r (in-list (hash-ref lane 'reasons))])
        (format "- ~a: ~a" (hash-ref lane 'lane) r))))

(define (cohort-decision-md-string manifest)
  (define d (decision-report-jsexpr manifest))
  (define baseline (hash-ref d 'baseline))
  (define lines '())
  (define (out . args)
    (set! lines (append lines (list (apply format args)))))
  (out "# Promotion decision: ~a" (hash-ref d 'cohort-id))
  (out "")
  (out "| Field | Value |")
  (out "|---|---|")
  (out "| Decision version | ~a |" (hash-ref d 'decision-version))
  (out "| Cohort | ~a |" (hash-ref d 'cohort-id))
  (out "| Cohort status | ~a |" (hash-ref d 'cohort-status))
  (out "| Baseline configuration | ~a |" (hash-ref d 'baseline-config))
  (out "| Baseline p50 / p95 (seconds) | ~a / ~a |"
       (hash-ref baseline 'p50-seconds)
       (hash-ref baseline 'p95-seconds))
  (out "")
  (out "## Gates")
  (out "")
  (out "~a" (hash-ref (hash-ref d 'gates) 'gate-text))
  (out "")
  (out "## Lane verdicts")
  (out "")
  (out
   "| Lane | Configuration | Verdict | p50 (s) | p95 (s) | Attempts | Failures | Cancelled | Reruns | Inventory equal |")
  (out "|---|---|---|---|---|---|---|---|---|---|")
  (for ([l (in-list (hash-ref d 'lanes))])
    (out "~a" (decision-md-lane-line l)))
  (out "")
  (out "### Reasons")
  (out "")
  (for ([l (in-list (hash-ref d 'lanes))])
    (for ([line (in-list (decision-reasons-lines l))])
      (out "~a" line)))
  (out "")
  (out (string-append
        "Cohort closure rule: the cohort may only be closed when every registered configuration"
        " has complete paired evidence; otherwise it remains open and every lane records hold."))
  (out "")
  (out
   "Reviewer: coordinator (delivery) — verified against .planning/VALIDATION; targets are never revised inside this wave.")
  (out "")
  (string-join lines "\n"))

(define (cohort-report-base-jsexpr manifest)
  (define vr (validate-cohort manifest))
  (define samples (cohort-timing-samples manifest))
  (define shas (hash-ref manifest 'shas '()))
  (define exclusions (hash-ref manifest 'exclusions '()))
  (define attempts-summary (cohort-attempts-summary manifest))

  (define p50 (cohort-quantile samples 0.50))
  (define p95 (cohort-quantile samples 0.95))

  (define total-pass (apply + (map (lambda (s) (hash-ref s 'pass 0)) shas)))
  (define total-fail (apply + (map (lambda (s) (hash-ref s 'fail 0)) shas)))
  (define total-timeout (apply + (map (lambda (s) (hash-ref s 'timeout 0)) shas)))
  (define total-skip (apply + (map (lambda (s) (hash-ref s 'skip 0)) shas)))
  (define total-flakes (apply + (map (lambda (s) (hash-ref s 'flakes 0)) shas)))
  (define total-parallel-only
    (apply + (map (lambda (s) (hash-ref s 'parallel-only-failures 0)) shas)))
  (define total-runner-minutes (apply + (map (lambda (s) (hash-ref s 'runner-minutes 0)) shas)))
  (define total-file-count (apply + (map (lambda (s) (hash-ref s 'file-count 0)) shas)))
  (define total-test-count (apply + (map (lambda (s) (hash-ref s 'test-count 0)) shas)))
  (define zero-test-shas (filter (lambda (s) (hash-ref s 'zero-test #f)) shas))

  ;; Inventory digest: aggregate of all per-SHA digests (sorted for determinism).
  (define inv-digests (sort (map (lambda (s) (hash-ref s 'inventory-digest "")) shas) string<?))
  (define aggregate-inventory-digest (string-join inv-digests "|"))

  ;; Prepared-env outcomes
  (define pe-outcomes
    (for/list ([s (in-list shas)])
      (hasheq 'sha (hash-ref s 'sha "?") 'prepared-env (hash-ref s 'prepared-env "?"))))

  ;; Queue telemetry
  (define queue-summary
    (hasheq 'total-wait-seconds
            (apply + (map (lambda (s) (hash-ref s 'queue-wait-seconds 0)) shas))
            'max-depth
            (if (null? shas)
                0
                (apply max (map (lambda (s) (hash-ref s 'queue-depth 0)) shas)))
            'samples
            (for/list ([s (in-list shas)])
              (hasheq 'sha
                      (hash-ref s 'sha "?")
                      'wait-seconds
                      (hash-ref s 'queue-wait-seconds 0)
                      'depth
                      (hash-ref s 'queue-depth 0)))))

  (hasheq
   'cohort-id
   (hash-ref manifest 'cohort-id "?")
   'milestone
   (hash-ref manifest 'milestone "?")
   'schema-version
   (hash-ref manifest 'schema-version cohort-schema-version)
   'report-version
   "w4-cohort-v1"
   'validation
   (hasheq 'ok (validation-ok? vr) 'errors (validation-errors vr) 'warnings (validation-warnings vr))
   'cohort-size
   (length shas)
   'expected-size
   (hash-ref manifest 'expected-count expected-cohort-size)
   'exclusion-count
   (length exclusions)
   'statistics
   (hasheq 'sample-count
           (length samples)
           'p50-seconds
           p50
           'p95-seconds
           p95
           'min-seconds
           (if (null? samples)
               #f
               (apply min samples))
           'max-seconds
           (if (null? samples)
               #f
               (apply max samples))
           'mean-seconds
           (if (null? samples)
               #f
               (/ (apply + samples) (length samples) 1.0)))
   'counts
   (hasheq 'total-pass
           total-pass
           'total-fail
           total-fail
           'total-timeout
           total-timeout
           'total-skip
           total-skip
           'total-flakes
           total-flakes
           'total-parallel-only-failures
           total-parallel-only
           'zero-test-shas
           (length zero-test-shas)
           'total-file-count
           total-file-count
           'total-test-count
           total-test-count)
   'reliability
   attempts-summary
   'inventory
   (hasheq 'aggregate-digest
           aggregate-inventory-digest
           'per-sha
           (for/list ([s (in-list shas)])
             (hasheq 'sha
                     (hash-ref s 'sha "?")
                     'digest
                     (hash-ref s 'inventory-digest "?")
                     'file-count
                     (hash-ref s 'file-count 0)
                     'test-count
                     (hash-ref s 'test-count 0))))
   'prepared-env-outcomes
   pe-outcomes
   'queue-telemetry
   queue-summary
   'runner-minutes
   (hasheq 'total
           total-runner-minutes
           'per-sha
           (for/list ([s (in-list shas)])
             (hasheq 'sha (hash-ref s 'sha "?") 'minutes (hash-ref s 'runner-minutes 0))))
   'exclusions
   (for/list ([e (in-list exclusions)])
     (hasheq 'sha
             (hash-ref e 'sha "?")
             'reason
             (hash-ref e 'reason "?")
             'detail
             (hash-ref e 'detail "")))
   'manifest-digest
   (manifest-digest manifest)))

;; Public report builder: base report plus the paired-configurations section
;; when (and only when) the manifest declares configurations.
(define (cohort-report-jsexpr manifest)
  (define base (cohort-report-base-jsexpr manifest))
  (define configurations (report-configurations-section manifest))
  (cond
    [(not configurations) base]
    [else
     (hash-set (hash-set base 'configurations configurations)
               'decision
               (decision-report-jsexpr manifest))]))
(define (cohort-report-json-string manifest)
  (jsexpr->string (cohort-report-jsexpr manifest)))

(define (cohort-report-md-string manifest)
  (define r (cohort-report-jsexpr manifest))
  (define stats (hash-ref r 'statistics))
  (define counts (hash-ref r 'counts))
  (define rel (hash-ref r 'reliability))
  (define inv (hash-ref r 'inventory))
  (define q (hash-ref r 'queue-telemetry))
  (define rm (hash-ref r 'runner-minutes))
  (define lines '())
  (define (out . args)
    (set! lines (append lines (list (apply format args)))))

  ;; Use the canonical milestone version in the heading so generated Markdown
  ;; remains compatible with repository version-lint rules when cohort IDs have
  ;; a suffix such as "-c0".
  (out "# Cohort Report: ~a" (hash-ref r 'milestone))
  (out "")
  (out "| Field | Value |")
  (out "|---|---|")
  (out "| Milestone | ~a |" (hash-ref r 'milestone))
  (out "| Schema version | ~a |" (hash-ref r 'schema-version))
  (out "| Report version | ~a |" (hash-ref r 'report-version))
  (out "| Cohort size | ~a |" (hash-ref r 'cohort-size))
  (out "| Expected size | ~a |" (hash-ref r 'expected-size))
  (out "| Exclusions | ~a |" (hash-ref r 'exclusion-count))
  (out "| Validation | ~a |" (if (hash-ref (hash-ref r 'validation) 'ok) "PASS" "FAIL"))
  (out "")
  (out "## Statistics (linear-interpolation percentile estimator)")
  (out "")
  (out "| Statistic | Value |")
  (out "|---|---|")
  (out "| Sample count | ~a |" (hash-ref stats 'sample-count))
  (out "| p50 (seconds) | ~a |" (hash-ref stats 'p50-seconds))
  (out "| p95 (seconds) | ~a |" (hash-ref stats 'p95-seconds))
  (out "| min (seconds) | ~a |" (hash-ref stats 'min-seconds))
  (out "| max (seconds) | ~a |" (hash-ref stats 'max-seconds))
  (out "| mean (seconds) | ~a |" (hash-ref stats 'mean-seconds))
  (out "")
  (out "## Counts")
  (out "")
  (out "| Count | Value |")
  (out "|---|---|")
  (out "| Total pass | ~a |" (hash-ref counts 'total-pass))
  (out "| Total fail | ~a |" (hash-ref counts 'total-fail))
  (out "| Total timeout | ~a |" (hash-ref counts 'total-timeout))
  (out "| Total skip | ~a |" (hash-ref counts 'total-skip))
  (out "| Total flakes | ~a |" (hash-ref counts 'total-flakes))
  (out "| Parallel-only failures | ~a |" (hash-ref counts 'total-parallel-only-failures))
  (out "| Zero-test SHAs | ~a |" (hash-ref counts 'zero-test-shas))
  (out "| Total file count | ~a |" (hash-ref counts 'total-file-count))
  (out "| Total test count | ~a |" (hash-ref counts 'total-test-count))
  (out "")
  (out "## Reliability evidence")
  (out "")
  (out "| Metric | Value |")
  (out "|---|---|")
  (out "| Total attempts | ~a |" (hash-ref rel 'total-attempts))
  (out "| Successes | ~a |" (hash-ref rel 'successes))
  (out "| Failures | ~a |" (hash-ref rel 'failures))
  (out "| Cancelled | ~a |" (hash-ref rel 'cancelled))
  (out "| Reruns | ~a |" (hash-ref rel 'reruns))
  (out "")
  (out "## Inventory digest")
  (out "")
  (out "Aggregate: `~a`" (hash-ref inv 'aggregate-digest))
  (out "")
  (out "## Queue telemetry")
  (out "")
  (out "| Metric | Value |")
  (out "|---|---|")
  (out "| Total wait (seconds) | ~a |" (hash-ref q 'total-wait-seconds))
  (out "| Max queue depth | ~a |" (hash-ref q 'max-depth))
  (out "")
  (out "## Runner-minute cost")
  (out "")
  (out "| Metric | Value |")
  (out "|---|---|")
  (out "| Total runner-minutes | ~a |" (hash-ref rm 'total))
  (out "")
  (out "## Exclusions")
  (out "")
  (define exclusions (hash-ref r 'exclusions))
  (cond
    [(null? exclusions) (out "(none)")]
    [else
     (out "| SHA | Reason | Detail |")
     (out "|---|---|---|")
     (for ([e (in-list exclusions)])
       (out "| ~a | ~a | ~a |" (hash-ref e 'sha) (hash-ref e 'reason) (hash-ref e 'detail)))])
  (out "")
  (when (hash-has-key? r 'configurations)
    (out "## Paired configurations")
    (out "")
    (out
     "| Configuration | Lane | Scheduler | Ordering | Start SHA | Eligible | Attempts recorded | Inventory equal |")
    (out "|---|---|---|---|---|---|---|---|")
    (for ([c (in-list (hash-ref r 'configurations))])
      (out "| ~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a |"
           (hash-ref c 'config-id "?")
           (hash-ref c 'lane "?")
           (hash-ref c 'scheduler "?")
           (hash-ref c 'ordering "?")
           (hash-ref c 'start-sha "?")
           (hash-ref c 'eligible-count 0)
           (hash-ref c 'attempts-recorded 0)
           (if (hash-ref c 'inventory-equal-to-baseline #f) "yes" "NO")))
    (out ""))
  (when (hash-has-key? r 'configurations)
    (out "## Promotion decision")
    (out "")
    (out "~a" (hash-ref (hash-ref (hash-ref r 'decision) 'gates) 'gate-text))
    (out "")
    (out
     "| Lane | Configuration | Verdict | p50 (s) | p95 (s) | Attempts | Failures | Cancelled | Reruns | Inventory equal |")
    (out "|---|---|---|---|---|---|---|---|---|---|")
    (for ([l (in-list (hash-ref (hash-ref r 'decision) 'lanes))])
      (out "~a" (decision-md-lane-line l)))
    (out "")
    (out "### Reasons")
    (out "")
    (for ([l (in-list (hash-ref (hash-ref r 'decision) 'lanes))])
      (for ([line (in-list (decision-reasons-lines l))])
        (out "~a" line)))
    (out ""))
  (out "## Manifest digest")
  (out "")
  (out "```")
  (out "~a" (hash-ref r 'manifest-digest))
  (out "```")
  (string-join lines "\n"))

;; ============================================================
;; Check mode: byte-identical regeneration
;; ============================================================

(define (cohort-check manifest-path report-path)
  ;; Regenerate the report from the manifest and compare byte-for-byte to
  ;; the stored report.  Returns #t if identical, #f otherwise (with reason).
  (cond
    [(not (file-exists? manifest-path)) (values #f "manifest not found")]
    [(not (file-exists? report-path)) (values #f "report not found")]
    [else
     (define manifest (load-cohort-manifest manifest-path))
     (define regenerated (cohort-report-json-string manifest))
     (define stored (file->string report-path))
     (cond
       [(equal? regenerated stored) (values #t "match")]
       [else (values #f "mismatch")])]))

;; ============================================================
;; CLI
;; ============================================================

(module+ main
  (define manifest-path #f)
  (define out-json #f)
  (define out-md #f)
  (define decision-path #f)
  (define check? #f)

  (command-line
   #:program "cohort-report"
   #:once-each [("--manifest") p "Cohort manifest JSON path" (set! manifest-path p)]
   [("--out-json") p "Write report JSON to path" (set! out-json p)]
   [("--out-md") p "Write report markdown to path" (set! out-md p)]
   [("--decision") p "Write the promotion decision markdown to path" (set! decision-path p)]
   [("--check") "Regenerate from manifest and compare to stored report" (set! check? #t)])

  (cond
    [(not manifest-path)
     (displayln "error: --manifest <path> is required")
     (exit 1)]
    [check?
     (unless out-json
       (displayln "error: --check requires --out-json (the report to compare against)")
       (exit 1))
     (define-values (ok reason) (cohort-check manifest-path out-json))
     (cond
       [ok
        (displayln (format "CHECK PASS: ~a" reason))
        (exit 0)]
       [else
        (displayln (format "CHECK FAIL: ~a" reason))
        (exit 1)])]
    [else
     (define manifest (load-cohort-manifest manifest-path))
     (define vr (validate-cohort manifest))
     (when (validation-ok? vr)
       ;; Validation passed — emit report.
       (define json-str (cohort-report-json-string manifest))
       (cond
         [out-json
          (call-with-output-file out-json #:exists 'replace (lambda (out) (display json-str out)))]
         [else (displayln json-str)]))
     (when (and (validation-ok? vr) out-md)
       (call-with-output-file out-md
                              #:exists 'replace
                              (lambda (out) (display (cohort-report-md-string manifest) out))))
     (when (and (validation-ok? vr) decision-path)
       (call-with-output-file decision-path
                              #:exists 'replace
                              (lambda (out) (display (cohort-decision-md-string manifest) out))))
     (unless (validation-ok? vr)
       (for ([e (in-list (validation-errors vr))])
         (displayln (format "ERROR: ~a" e)))
       (for ([w (in-list (validation-warnings vr))])
         (displayln (format "WARN: ~a" w)))
       (exit 1))]))
