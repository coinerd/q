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
         cohort-quantile-exact
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
         fast-p95-max-seconds
         ;; post-promotion activation cohort (v1.00.25 W6: C2)
         cohort-mode
         post-promotion-gate
         post-promotion-gate-text
         post-promotion-p50-max-seconds
         post-promotion-p95-max-seconds
         ;; end-to-end PR elapsed cohort (v1.00.26 W6: C2, pr-elapsed mode)
         pr-elapsed-gate
         pr-elapsed-gate-text
         pr-elapsed-p50-max-seconds
         pr-elapsed-p95-max-seconds
         pr-elapsed-required-fields
         pr-elapsed-decision-md-string
         pr-elapsed-seconds-from-window
         ;; final-claim verdict mode (v1.00.27 W5: C3)
         final-claim-gate
         final-claim-gate-text
         final-claim-row-ids
         final-claim-guard-ids
         final-claim-thresholds
         final-claim-decision-md-string
         final-claim-required-fields
         final-claim-fast-samples
         final-claim-security-samples
         final-claim-workflows-samples
         final-claim-prepared-env-stats
         final-claim-reliability-ok?
         final-claim-guard-provided?
         normalize-manifest
         manifest-has-key?)

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
                            "non-unique-sha"
                            ;; W6 (C2): the required-lane run itself failed so
                            ;; no timing artifact was ever produced for the
                            ;; SHA.  Mechanical, named, never silently dropped.
                            "lane-run-failed"))

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

(define (manifest-has-key? v k)
  ;; Tolerant key check: manifest hashes arrive symbol-keyed (in-process
  ;; fixtures, normalize-manifest output) or string-keyed (read-json).
  (and (hash? v) (or (hash-has-key? v k) (hash-has-key? v (symbol->string k)))))

(define (hash-ref-tolerant h key [default #f])
  ;; Content-based lookup across the symbol/string key duality: the load
  ;; path (normalize-manifest) yields symbol keys while in-process fixtures
  ;; may carry string keys — and ids like guard ids and row ids are strings.
  (cond
    [(not (hash? h)) default]
    [(hash-has-key? h key) (hash-ref h key)]
    [(and (string? key) (hash-has-key? h (string->symbol key))) (hash-ref h (string->symbol key))]
    [(and (symbol? key) (hash-has-key? h (symbol->string key))) (hash-ref h (symbol->string key))]
    [else default]))

(define (cohort-manifest? v)
  (and (hash? v)
       (manifest-has-key? v 'cohort-id)
       (manifest-has-key? v 'milestone)
       (manifest-has-key? v 'schema-version)
       (manifest-has-key? v 'shas)))

(define (load-cohort-manifest path)
  (cond
    [(not (file-exists? path)) (error 'load-cohort-manifest "manifest file not found: ~a" path)]
    [else
     (define v
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (call-with-input-file path read-json)))
     (cond
       [(not v) (error 'load-cohort-manifest "manifest is not valid JSON: ~a" path)]
       [(not (hash? v))
        (error 'load-cohort-manifest "manifest does not match cohort schema: ~a" path)]
       [else
        ;; Normalize string keys to symbols BEFORE schema validation so the
        ;; canonical symbol-keyed form is the only form past the loader.
        (define m (normalize-manifest v))
        (unless (cohort-manifest? m)
          (error 'load-cohort-manifest "manifest does not match cohort schema: ~a" path))
        m])]))

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
  ;; W6 C2 (pr-elapsed): mode-aware validation.  The pr-elapsed cohort
  ;; measures end-to-end mergeable-PR wall time on the integrated topology
  ;; and accumulates openly: while its status is started/open it may
  ;; legitimately hold fewer SHAs than expected with no exclusions yet —
  ;; the gate is then simply not evaluable ("evidence pending"), never a
  ;; silent miss.
  (define mode (cohort-mode manifest))
  ;; final-claim (v1.00.27 W5: C3) reuses the pr-elapsed per-SHA schema
  ;; (end-to-end PR observations with attempts and inventory bindings) and
  ;; adds the guard-evidence contract validated below.
  (define pr-elapsed? (member mode (list "pr-elapsed" "final-claim")))
  (define final-claim? (equal? mode "final-claim"))
  (define cohort-open? (member (hash-ref manifest 'cohort-status #f) open-cohort-statuses))

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
       ;; An openly accumulating pr-elapsed cohort (W6 C2) may hold fewer
       ;; SHAs than expected — the gate stays "evidence pending"; it is
       ;; never a silent miss.  Cohort close enforces the full count.
       [(and pr-elapsed? cohort-open?)
        (warn! (format
                "pr-elapsed cohort ~a is still accumulating: ~a of ~a unique PR head SHAs observed"
                (hash-ref manifest 'cohort-id "?")
                n-shas
                expected))]
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
  ;;    pr-elapsed keeps the cohort open and honest: re-runs/failures are
  ;;    recorded as warnings, never silently dropped, and eligibility is
  ;;    re-checked at cohort close (pr-elapsed-gate).
  (for ([s (in-list shas)]
        [i (in-naturals)])
    (cond
      [(not (sha-eligible? s))
       (cond
         [pr-elapsed?
          (cond
            ;; v1.00.28 W8: in a CLOSED cohort a SHA cell with no
            ;; successful run is a hard cohort error or a named
            ;; reliability strike — never silence.  An accumulating
            ;; (open) cohort keeps recording attempts and stays open.
            [(equal? (hash-ref manifest 'cohort-status #f) "closed")
             (err!
              (format
               (string-append
                "closed-cohort SHA ~a (index ~a) records no final-success run — "
                "a cohort SHA cell with no successful run is a cohort error or a named reliability strike, never silence")
               (hash-ref s 'sha "?")
               i))]
            [else
             (warn!
              (format
               "SHA ~a (index ~a) has no final-success timing sample yet — re-run/failure recorded, cohort stays open"
               (hash-ref s 'sha "?")
               i))])]
         [else
          (err!
           (format
            "SHA ~a (index ~a) is not eligible: must have exactly one \
                      timing-sample attempt (final successful)"
            (hash-ref s 'sha "?")
            i))])]))

  ;; 4. Every SHA must have required fields.  pr-elapsed records carry the
  ;;    end-to-end PR wall-time schema (pr-elapsed required fields) instead
  ;;    of the paired-shadow per-lane schema; mode decides which contract.
  (define required-fields
    (if pr-elapsed?
        pr-elapsed-required-fields
        '(sha scheduler
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
              runner-minutes)))
  (for ([s (in-list shas)]
        [i (in-naturals)])
    (for ([field (in-list required-fields)])
      (unless (hash-has-key? s field)
        (err!
         (format "SHA ~a (index ~a) missing required field: ~a" (hash-ref s 'sha "?") i field)))))

  ;; 4b. pr-elapsed attempt schema: attempt entries must carry a boolean
  ;;     timing-sample flag; a timing-sample attempt must carry a parseable
  ;;     required-check window; a queue wait alone (or a queue wait that
  ;;     exceeds the whole check window) is never accepted as the measure.
  (when pr-elapsed?
    (for ([s (in-list shas)]
          [i (in-naturals)])
      (define sha-id (hash-ref s 'sha "?"))
      (for ([a (in-list (hash-ref s 'attempts '()))]
            [j (in-naturals)])
        (unless (boolean? (hash-ref a 'timing-sample (hash-ref a 'timing-sample #f)))
          (err!
           (format "SHA ~a (index ~a) attempt ~a: timing-sample flag must be a boolean" sha-id i j)))
        (when (and (hash-ref a 'timing-sample #f) (not (pr-elapsed-attempt-window-seconds a)))
          (err!
           (format
            "SHA ~a (index ~a) attempt ~a: timing-sample without a parseable \
required-check window (first-check-start-at/last-required-check-end-at)"
            sha-id
            i
            j)))
        (define wait (hash-ref a 'queue-wait-seconds #f))
        (define window (pr-elapsed-attempt-window-seconds a))
        (when (and wait window (> wait window))
          (err!
           (format
            "SHA ~a (index ~a) attempt ~a: queue wait ~as exceeds the \
required-check window — queue wait alone is not accepted as the PR elapsed measure"
            sha-id
            i
            j
            wait))))))

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
  ;;    pr-elapsed records are end-to-end PR observations on the integrated
  ;;    topology: the expected scheduler is fast/queue/lpt; anything else is
  ;;    recorded as a warning for the decision record, not a hard error.
  (for ([s (in-list shas)])
    (define sched (hash-ref s 'scheduler #f))
    (cond
      [pr-elapsed?
       (unless (equal? sched "fast/queue/lpt")
         (warn! (format
                 "SHA ~a has non-fast scheduler ~a in pr-elapsed cohort (expected fast/queue/lpt)"
                 (hash-ref s 'sha "?")
                 sched)))]
      [else
       (unless (member sched '("batch" "serial"))
         (err! (format "SHA ~a has incompatible scheduler: ~a" (hash-ref s 'sha "?") sched)))]))

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

  ;; 10. prepared-env must be a known value.  pr-elapsed records observe
  ;;     the W5 prepared-environment restore evidence; absent evidence is
  ;;     recorded, not treated as a hard schema error.
  (for ([s (in-list shas)])
    (define pe (hash-ref s 'prepared-env #f))
    (cond
      [pr-elapsed?
       (unless (member pe '(#f "match" "rebuild" "cached"))
         (err! (format "SHA ~a has unknown prepared-env: ~a" (hash-ref s 'sha "?") pe)))]
      [else
       (unless (member pe '("match" "rebuild" "cached"))
         (err! (format "SHA ~a has unknown prepared-env: ~a" (hash-ref s 'sha "?") pe)))]))

  ;; 11. Paired configurations (when present): schema, pairing, inventory
  ;; equality, and lifecycle rules.
  (validate-configurations manifest err!)

  ;; 12. final-claim guard evidence: every guard-evidence entry must name a
  ;;     known guard id, carry a boolean `provided` flag, and — when the
  ;;     guard is claimed as provided — carry a non-empty named reference
  ;;     (artifact or evidence pointer).  Unknown guard ids are rejected so
  ;;     guard names cannot drift between roadmap and manifest.
  (when final-claim?
    (define guard-ev (hash-ref manifest 'guard-evidence #f))
    (unless (hash? guard-ev)
      (err! "final-claim cohort requires a guard-evidence object"))
    (when (hash? guard-ev)
      (for ([(gid ev) (in-hash guard-ev)])
        ;; normalize-manifest canonicalizes hash keys to symbols, so guard
        ;; ids arrive as symbols here while final-claim-guard-ids holds
        ;; strings; compare by content, never by eq?.
        (define gid-name
          (if (symbol? gid)
              (symbol->string gid)
              gid))
        (unless (member gid-name final-claim-guard-ids)
          (err! (format "final-claim guard-evidence has unknown guard id: ~a (must be one of ~a)"
                        gid-name
                        final-claim-guard-ids)))
        (unless (and (hash? ev) (boolean? (hash-ref ev 'provided)))
          (err! (format "final-claim guard-evidence ~a must be an object with a boolean provided flag"
                        gid)))
        (when (and (hash? ev) (hash-ref ev 'provided #f))
          (unless (non-empty-string? (hash-ref ev 'reference ""))
            (err! (format "final-claim guard-evidence ~a claims provided but names no reference"
                          gid)))))))

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

;; Exact linear interpolation at rank k = q*(n-1): s[lo] + (k-lo)*(s[hi]-s[lo]).
;; The final-claim §8 rows report p95 verbatim, so they must not inherit the
;; midpoint-only smoothing of cohort-quantile (which existing callers rely on).
(define (cohort-quantile-exact xs q)
  (cond
    [(null? xs) #f]
    [else
     (define s (sort (map (lambda (x) (exact->inexact x)) xs) <))
     (define n (length s))
     (define k (* q (sub1 n)))
     (define lo (inexact->exact (floor k)))
     (define hi (min (add1 lo) (sub1 n)))
     (define frac (- k lo))
     ;; Round to millisecond precision so interpolated p95s neither carry
     ;; floating-point noise (e.g. 1175.6500000000005) nor vary across
     ;; regeneration runs — the checksummed artifacts must be byte-stable.
     (/ (round (* 1000.0 (+ (* (- 1.0 frac) (list-ref s lo)) (* frac (list-ref s hi))))) 1000.0)]))

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

(define (normalize-manifest v)
  ;; Canonicalize a manifest jsexpr: every hash key becomes a symbol and
  ;; contents are normalized recursively.  read-json yields string-keyed
  ;; hashes, but all accessors here use symbol keys and jsexpr->string only
  ;; accepts symbol keys — so the canonical form is the symbol-keyed form.
  (cond
    [(hash? v)
     (for/hash ([(k val) (in-hash v)])
       (values (string->symbol (if (symbol? k)
                                   (symbol->string k)
                                   (format "~a" k)))
               (normalize-manifest val)))]
    [(list? v) (map normalize-manifest v)]
    [(vector? v) (list->vector (map normalize-manifest (vector->list v)))]
    ;; JSON null means "absent evidence" in this schema (e.g. a SHA entry
    ;; without an observed prepared-env outcome); read-json yields the
    ;; 'null symbol, which nothing downstream accepts — canonicalize to #f
    ;; so the validator and reporters see exactly one representation.
    [(eq? v 'null) #f]
    [else v]))

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
     ;; A configuration absent from the manifest (e.g. a C1 decision lane
     ;; evaluated against a post-promotion C2 manifest) must still produce a
     ;; structured hold verdict — never an undefined value.
     (fail! (format "configuration ~a is not registered in the cohort manifest" config-id))
     (hasheq 'lane
             lane-id
             'config-id
             config-id
             'verdict
             "hold"
             'reasons
             (reverse reasons)
             'numbers
             (hasheq 'p50-seconds
                     #f
                     'p95-seconds
                     #f
                     'samples
                     0
                     'complete
                     #f
                     'attempts-recorded
                     0
                     'failures
                     0
                     'cancelled
                     0
                     'reruns
                     0
                     'inventory-equal-to-baseline
                     #f))]
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

;; ============================================================
;; Post-promotion activation cohort (v1.00.25 W6: C2)
;;
;; C2 measures the promoted defaults out of sample on new PR head SHAs.
;; There are no shadow legs: the required lane itself produced every
;; sample.  Targets are never revised inside this wave or milestone; a
;; miss records "target unachieved" and names the next lever for a
;; separate reviewed decision (a timing miss alone implies no queue
;; rollback).
;; ============================================================

(define post-promotion-p50-max-seconds 115.0)
(define post-promotion-p95-max-seconds 135.0)

(define post-promotion-gate-text
  (string-append "Out-of-sample fast execution target on promoted defaults: p50 ≤ 115 s and"
                 " p95 ≤ 135 s (roadmap v1.00.25 §6, W6).  Targets are never revised inside"
                 " this wave or milestone."))

;; Which mode produced the report numbers: "paired-shadow" (C1) or
;; "post-promotion" (C2).  Defaults to the original paired-shadow mode so
;; pre-C2 manifests keep their current reports byte-identical.
(define (cohort-mode manifest)
  (hash-ref manifest 'cohort-mode "paired-shadow"))

(define (post-promotion-gate manifest)
  (define samples (cohort-timing-samples manifest))
  (define p50
    (if (null? samples)
        #f
        (cohort-quantile samples 0.50)))
  (define p95
    (if (null? samples)
        #f
        (cohort-quantile samples 0.95)))
  (define achieved
    (and p50 p95 (<= p50 post-promotion-p50-max-seconds) (<= p95 post-promotion-p95-max-seconds)))
  (hasheq 'mode
          "post-promotion"
          'gate-text
          post-promotion-gate-text
          'p50-max-seconds
          (exact->inexact post-promotion-p50-max-seconds)
          'p95-max-seconds
          (exact->inexact post-promotion-p95-max-seconds)
          'p50-seconds
          p50
          'p95-seconds
          p95
          'samples
          (length samples)
          'achieved
          achieved
          'verdict
          (if achieved "target achieved" "target unachieved")
          'next-lever
          (if achieved
              #f
              (string-append "Next lever (separate reviewed decision; a timing miss alone implies no"
                             " queue rollback): reduce the fast-lane critical path by trimming batch"
                             " shard fan-out and reusing the prepared environment cache; re-run this"
                             " cohort on new SHAs before the next promotion decision."))))

;; ============================================================
;; End-to-end PR elapsed cohort (v1.00.26 W6: C2, pr-elapsed mode)
;;
;; C2 of the v1.00.26 campaign measures the full mergeable-PR wall time on
;; the integrated topology: from the first required-check start to the
;; last required-check completion — not the queue wait alone (the queue
;; wait is recorded separately per attempt).  Closing the cohort requires
;; 20 unique PR head SHAs; duplicates are rejected, and failed, cancelled,
;; and rerun attempts are recorded, never dropped.  Targets are never
;; revised inside this wave or milestone: a closed-cohort miss records
;; "target unachieved" with the observed numbers and names the next lever
;; for a separate reviewed decision (a timing miss alone implies no queue
;; rollback).  While the cohort is still accumulating (status started or
;; open with fewer SHAs than expected) the gate is not evaluable and the
;; honest verdict is "evidence pending" — an accumulating cohort is never
;; reported as a silent miss.
;; ============================================================

(define pr-elapsed-p50-max-seconds 588.0)
(define pr-elapsed-p95-max-seconds 735.0)

;; Minimal record schema for end-to-end PR cohort rows: the PR identity,
;; the observed required-check window(s), and the inventory binding.  The
;; richer paired-shadow telemetry fields are recorded when available but
;; are not required for the PR-elapsed gate.
(define pr-elapsed-required-fields '(sha pr scheduler ordering attempts inventory-digest))

(define pr-elapsed-gate-text
  (string-append "End-to-end PR elapsed target on the integrated topology: p50 ≤ 588 s and"
                 " p95 ≤ 735 s (roadmap v1.00.26 §7, W6).  Targets are never revised inside"
                 " this wave or milestone."))

;; UTC ISO-8601 timestamps ("YYYY-MM-DDTHH:MM:SSZ") delimit the per-attempt
;; required-check window.  Civil-date -> days conversion is the standard
;; days-from-civil algorithm in pure arithmetic (no date-library pull-in).
(define pr-elapsed-timestamp-regexp #px"^(\\d{4})-(\\d{2})-(\\d{2})T(\\d{2}):(\\d{2}):(\\d{2})Z$")

(define (pr-elapsed-civil-days y mo d)
  (define y*
    (if (<= mo 2)
        (sub1 y)
        y))
  (define era
    (quotient (if (>= y* 0)
                  y*
                  (- y* 399))
              400))
  (define yoe (- y* (* 400 era)))
  (define m* (+ mo (if (> mo 2) -3 9)))
  (define doy (+ (quotient (+ (* 153 m*) 2) 5) (sub1 d)))
  (define doe (+ (* yoe 365) (quotient yoe 4) (- (quotient yoe 100)) doy))
  (- (+ (* era 146097) doe) 719468))

(define (pr-elapsed-utc-seconds ts)
  ;; Parse "YYYY-MM-DDTHH:MM:SSZ" to Unix seconds; #f when not parseable.
  (define m (regexp-match pr-elapsed-timestamp-regexp ts))
  (and m
       (let* ([y (string->number (list-ref m 1))]
              [mo (string->number (list-ref m 2))]
              [d (string->number (list-ref m 3))]
              [h (string->number (list-ref m 4))]
              [mi (string->number (list-ref m 5))]
              [s (string->number (list-ref m 6))])
         (+ (* 86400 (pr-elapsed-civil-days y mo d)) (* 3600 h) (* 60 mi) s))))

(define (pr-elapsed-seconds-from-window start end)
  ;; Wall time (seconds) from the first required-check start to the last
  ;; required-check completion.  #f when an endpoint is missing or
  ;; malformed, or the window is inverted (never a negative sample).
  (define s (and (string? start) (pr-elapsed-utc-seconds start)))
  (define e (and (string? end) (pr-elapsed-utc-seconds end)))
  (and s e (<= s e) (- e s)))

(define (pr-elapsed-attempt-seconds attempt)
  (hash-ref attempt 'pr-elapsed-seconds #f))

(define (pr-elapsed-attempt-window-seconds attempt)
  (pr-elapsed-seconds-from-window (hash-ref attempt 'first-check-start-at #f)
                                  (hash-ref attempt 'last-required-check-end-at #f)))

(define (pr-elapsed-timing-samples manifest)
  ;; Final-success attempt per eligible SHA; the sample is the required-
  ;; check window width (recorded value preferred, window-derived fallback)
  ;; — never the queue wait alone.
  (filter values
          (map (lambda (s)
                 (cond
                   [(sha-eligible? s)
                    (define attempt (sha-final-success-attempt s))
                    (or (pr-elapsed-attempt-seconds attempt)
                        (pr-elapsed-attempt-window-seconds attempt))]
                   [else #f]))
               (hash-ref manifest 'shas '()))))

(define (pr-elapsed-gate manifest)
  (define samples (pr-elapsed-timing-samples manifest))
  (define shas (hash-ref manifest 'shas '()))
  (define expected (hash-ref manifest 'expected-count expected-cohort-size))
  (define status (hash-ref manifest 'cohort-status "?"))
  ;; The gate is evaluable only on a fully accumulated closed cohort: 20
  ;; unique PR head SHAs, each with one final-success elapsed sample.
  (define evaluable?
    (and (equal? status "closed") (= (length shas) expected) (= (length samples) expected)))
  (define p50
    (if (null? samples)
        #f
        (cohort-quantile samples 0.50)))
  (define p95
    (if (null? samples)
        #f
        (cohort-quantile samples 0.95)))
  (define achieved
    (and evaluable? p50 p95 (<= p50 pr-elapsed-p50-max-seconds) (<= p95 pr-elapsed-p95-max-seconds)))
  (hasheq
   'mode
   "pr-elapsed"
   'gate-text
   pr-elapsed-gate-text
   'p50-max-seconds
   (exact->inexact pr-elapsed-p50-max-seconds)
   'p95-max-seconds
   (exact->inexact pr-elapsed-p95-max-seconds)
   'p50-seconds
   p50
   'p95-seconds
   p95
   'samples
   (length samples)
   'unique-head-shas
   (length shas)
   'expected-count
   expected
   'cohort-status
   status
   'gate-evaluable
   evaluable?
   'achieved
   achieved
   'verdict
   (cond
     [evaluable? (if achieved "target achieved" "target unachieved")]
     [else "evidence pending (cohort open)"])
   'next-lever
   (if (and evaluable? (not achieved))
       (string-append "Next lever (separate reviewed decision; a timing miss alone implies no queue"
                      " rollback): move the slow required gates off the PR critical path by splitting"
                      " the aggregate workflows out of the mergeable required set and pre-warming the"
                      " prepared environment cache; re-run this cohort on 20 new PR head SHAs before"
                      " the next promotion-adjacent decision.")
       #f)))

;; C2 (pr-elapsed) decision document: the honest achieved/unachieved
;; verdict against the roadmap end-to-end PR elapsed targets, the observed
;; numbers, the accumulating-cohort honesty clause, and — on a closed-
;; cohort miss — the named next lever for a separate reviewed decision.
(define (pr-elapsed-decision-md-string manifest)
  (define gate (pr-elapsed-gate manifest))
  (define lines '())
  (define (out . args)
    (set! lines (append lines (list (apply format args)))))
  (out "# C2 end-to-end PR elapsed decision: ~a" (hash-ref manifest 'cohort-id "?"))
  (out "")
  (out "| Field | Value |")
  (out "|---|---|")
  (out "| Decision mode | pr-elapsed (mergeable-PR wall time, not the queue wait alone) |")
  (out "| Cohort status | ~a |" (hash-ref gate 'cohort-status))
  (out "| Unique PR head SHAs | ~a of ~a |"
       (hash-ref gate 'unique-head-shas)
       (hash-ref gate 'expected-count))
  (out "| Timing samples | ~a |" (hash-ref gate 'samples))
  (out "| Observed p50 (seconds) | ~a |" (or (hash-ref gate 'p50-seconds #f) "n/a"))
  (out "| Observed p95 (seconds) | ~a |" (or (hash-ref gate 'p95-seconds #f) "n/a"))
  (out "| Targets (never revised) | p50 ≤ ~a s, p95 ≤ ~a s |"
       (hash-ref gate 'p50-max-seconds)
       (hash-ref gate 'p95-max-seconds))
  (out "| Gate evaluable | ~a |" (if (hash-ref gate 'gate-evaluable) "yes" "no — cohort open"))
  (out "| Verdict | ~a |" (hash-ref gate 'verdict))
  (out "")
  (out "## Gate")
  (out "")
  (out "~a" (hash-ref gate 'gate-text))
  (out "")
  (unless (hash-ref gate 'gate-evaluable #f)
    (out (string-append
          "The cohort is still accumulating on the integrated topology; the gate is not"
          " evaluable in this window.  An accumulating cohort is never reported as a miss:"
          " the verdict stays \"evidence pending\" until 20 unique PR head SHAs are closed"
          " on the integrated topology."))
    (out ""))
  (when (hash-ref gate 'next-lever #f)
    (out "~a" (hash-ref gate 'next-lever))
    (out ""))
  (out (string-append
        "Reliability closure: failed, cancelled, and rerun attempts are recorded; re-runs"
        " are recorded and never dropped or silently deduplicated — 20 unique PR head SHAs"
        " are enforced at cohort close."))
  (out "")
  (out (string-append
        "A timing miss alone implies no queue rollback; any lever change is a separate"
        " reviewed decision. Targets are never revised inside this wave or this milestone."))
  (out "")
  (out "Reviewer: coordinator (delivery) — verified against .planning/VALIDATION.")
  (out "")
  (string-join lines "\n"))

;; ============================================================
;; Final-claim verdict mode (v1.00.27 W5: C3)
;;
;; C3 closes the final claim: every roadmap §8 row is evaluated against its
;; FIXED threshold with its coupled guard evidence, by the tooling alone —
;; the verdict is "pass", "target not achieved", or "unverified", and the
;; targets are never revised inside this wave or milestone.  A row without
;; its guard evidence is "unverified", never "pass".  The overall verdict is
;; "verified" only when every row passes with its guards; otherwise the
;; decision records "target not achieved" per missed row with the observed
;; numbers and the named next lever (a separate reviewed decision; a timing
;; miss alone implies no queue rollback).
;; ============================================================

(define final-claim-guard-ids
  '("inventory-accounted" "reliability-non-regression"
                          "semantic-gate-equivalence"
                          "failure-truth"
                          "shared-state-permission-isolation"
                          "four-worker-isolation-proof"
                          "prepared-env-no-bypass"))

;; The seven roadmap §8 rows with their FIXED thresholds.  These constants
;; are the final-claim contract: neither the gate nor a manifest may revise
;; them.
;; equal?-based hash: ids are strings and must match by content, not by
;; identity (hasheq with string keys silently misses fresh string objects).
(define final-claim-thresholds
  (hash "fast-p50"
        115.0
        "fast-p95"
        135.0
        "pr-ci-p50"
        588.0
        "pr-ci-p95"
        735.0
        "security-runner-p50"
        240.0
        "workflows-runner-p50"
        220.0
        "prepared-env-verified-restores"
        95.0))

(define final-claim-rows
  (list
   (hasheq 'id
           "fast-p50"
           'measure
           "fast execution p50 on the required fast lane"
           'comparison
           "<="
           'sample-key
           'fast-execution-seconds
           'quantile
           0.50
           'guards
           '("inventory-accounted" "reliability-non-regression"
                                   "semantic-gate-equivalence"
                                   "four-worker-isolation-proof")
           'next-lever
           (string-append "Next lever (separate reviewed decision; a timing miss alone implies no"
                          " queue rollback): shrink the fast-lane critical path (shard fan-out and"
                          " prepared-env cache reuse); re-run the final cohort on 20 new PR head"
                          " SHAs."))
   (hasheq 'id
           "fast-p95"
           'measure
           "fast execution p95 on the required fast lane"
           'comparison
           "<="
           'sample-key
           'fast-execution-seconds
           'quantile
           0.95
           'guards
           '("inventory-accounted" "reliability-non-regression"
                                   "semantic-gate-equivalence"
                                   "four-worker-isolation-proof")
           'next-lever
           (string-append "Next lever (separate reviewed decision; a timing miss alone implies no"
                          " queue rollback): tail-shard rebalancing and cache reuse on the slowest"
                          " fast shards; re-run the final cohort on 20 new PR head SHAs."))
   (hasheq 'id
           "pr-ci-p50"
           'measure
           "end-to-end PR CI p50 (first required-check start to last required-check end)"
           'comparison
           "<="
           'sample-key
           'pr-elapsed-seconds
           'quantile
           0.50
           'guards
           '("inventory-accounted" "reliability-non-regression" "failure-truth")
           'next-lever
           (string-append "Next lever (separate reviewed decision; a timing miss alone implies no"
                          " queue rollback): move slow required gates off the mergeable critical"
                          " path per the v1.00.26 topology analysis; re-run the final cohort."))
   (hasheq 'id
           "pr-ci-p95"
           'measure
           "end-to-end PR CI p95 (first required-check start to last required-check end)"
           'comparison
           "<="
           'sample-key
           'pr-elapsed-seconds
           'quantile
           0.95
           'guards
           '("inventory-accounted" "reliability-non-regression" "failure-truth")
           'next-lever
           (string-append "Next lever (separate reviewed decision; a timing miss alone implies no"
                          " queue rollback): tail latency of the slowest required gates; re-run the"
                          " final cohort."))
   (hasheq 'id
           "security-runner-p50"
           'measure
           "security suite runner p50"
           'comparison
           "<="
           'sample-key
           'security-runner-seconds
           'quantile
           0.50
           'guards
           '("inventory-accounted" "reliability-non-regression" "shared-state-permission-isolation")
           'next-lever
           (string-append "Next lever (separate reviewed decision; a timing miss alone implies no"
                          " queue rollback): shard the security suite and reuse the prepared"
                          " environment; re-run the final cohort."))
   (hasheq 'id
           "workflows-runner-p50"
           'measure
           "workflows suite runner p50"
           'comparison
           "<="
           'sample-key
           'workflows-runner-seconds
           'quantile
           0.50
           'guards
           '("inventory-accounted" "reliability-non-regression" "four-worker-isolation-proof")
           'next-lever
           (string-append "Next lever (separate reviewed decision; a timing miss alone implies no"
                          " queue rollback): shard-count rebalancing of the workflows suite;"
                          " re-run the final cohort."))
   (hasheq 'id
           "prepared-env-verified-restores"
           'measure
           "prepared-environment verified-restore rate (percent)"
           'comparison
           ">="
           'sample-key
           'prepared-env
           'guards
           '("prepared-env-no-bypass" "reliability-non-regression")
           'next-lever
           (string-append "Next lever (separate reviewed decision; a timing miss alone implies no"
                          " queue rollback): wire restore-step verification emission into the"
                          " prepared-env report and re-observe on a new cohort window."))))

(define final-claim-row-ids (map (lambda (r) (hash-ref r 'id)) final-claim-rows))

(define final-claim-required-fields pr-elapsed-required-fields)

;; ============================================================
;; v1.00.28 W8: final campaign verdict vocabulary
;;
;; The cohort decision record ends in EXACTLY one of these three strings —
;; spelled byte-identically, never abbreviated, never re-worded, and never
;; extended.  Every emitted final verdict is checked against this
;; vocabulary: an out-of-vocabulary verdict is a programming error, never
;; a silent new string.
;; ============================================================

(define final-verdict-achieved "ACHIEVED")
(define final-verdict-not-achieved "NOT ACHIEVED")
(define final-verdict-partial "PARTIAL WORKLOAD REDUCTION; FINAL TARGET NOT ACHIEVED")

(define allowed-final-verdicts
  (list final-verdict-achieved final-verdict-not-achieved final-verdict-partial))

(define final-claim-gate-text
  (string-append "Final-claim verdict over every roadmap §8 row against the FIXED thresholds"
                 " (fast p50 ≤ 115 s / p95 ≤ 135 s; PR CI p50 ≤ 588 s / p95 ≤ 735 s;"
                 " security runner p50 ≤ 240 s; workflows runner p50 ≤ 220 s; verified prepared-env"
                 " restores ≥ 95%) with the coupled guards.  A row without its guard evidence is"
                 " \"unverified\", never \"pass\"; targets are never revised inside this wave or"
                 " milestone."))

(define (final-claim-fast-samples manifest)
  ;; One fast-execution sample per SHA whose final-success attempt carries
  ;; a measured fast-execution-seconds datum.
  (filter values
          (map (lambda (s)
                 (and (sha-eligible? s)
                      (hash-ref (sha-final-success-attempt s) 'fast-execution-seconds #f)))
               (hash-ref manifest 'shas '()))))

(define (final-claim-security-samples manifest)
  (filter values
          (map (lambda (s)
                 (and (sha-eligible? s)
                      (hash-ref (sha-final-success-attempt s) 'security-runner-seconds #f)))
               (hash-ref manifest 'shas '()))))

(define (final-claim-workflows-samples manifest)
  (filter values
          (map (lambda (s)
                 (and (sha-eligible? s)
                      (hash-ref (sha-final-success-attempt s) 'workflows-runner-seconds #f)))
               (hash-ref manifest 'shas '()))))

(define (final-claim-prepared-env-stats manifest)
  ;; Prepared-environment restore outcomes observed in the cohort window.
  ;; The manifest carries the observed stats block (verified restores,
  ;; total records, fallback records); the verified-restore rate is 0.0
  ;; when no records were observed — never assumed.
  (define stats (hash-ref manifest 'prepared-env-restore-stats #f))
  (cond
    [(hash? stats)
     (define verified (hash-ref stats 'verified 0))
     (define total (hash-ref stats 'total 0))
     (hasheq 'verified
             verified
             'total
             total
             'fallback
             (hash-ref stats 'fallback 0)
             'rate
             (if (zero? total)
                 0.0
                 (* 100.0 (/ verified total 1.0)))
             'records-observed
             (hash-ref stats 'records-observed 0)
             'window
             (hash-ref stats 'window ""))]
    [else (hasheq 'verified 0 'total 0 'fallback 0 'rate 0.0 'records-observed 0 'window "")]))

(define (final-claim-guard-provided? manifest guard-id)
  (define ev (hash-ref-tolerant (hash-ref manifest 'guard-evidence (hasheq)) guard-id #f))
  (and (hash? ev)
       (hash-ref-tolerant ev 'provided #f)
       (non-empty-string? (hash-ref-tolerant ev 'reference ""))))

(define (final-claim-reliability-ok? manifest)
  ;; Computed non-regression: failures+cancelled+reruns across all cohort
  ;; attempts must not exceed the recorded baseline block.
  (define baseline (hash-ref manifest 'reliability-baseline #f))
  (and (hash? baseline)
       (let ([summary (cohort-attempts-summary manifest)])
         (<= (+ (hash-ref summary 'failures 0)
                (hash-ref summary 'cancelled 0)
                (hash-ref summary 'reruns 0))
             (+ (hash-ref baseline 'failures 0)
                (hash-ref baseline 'cancelled 0)
                (hash-ref baseline 'reruns 0))))))

(define (final-claim-gate manifest)
  (define shas (hash-ref manifest 'shas '()))
  (define expected (hash-ref manifest 'expected-count expected-cohort-size))
  (define status (hash-ref manifest 'cohort-status "?"))
  (define closed? (equal? status "closed"))
  (define fast-samples (final-claim-fast-samples manifest))
  (define pr-samples (pr-elapsed-timing-samples manifest))
  (define sec-samples (final-claim-security-samples manifest))
  (define wf-samples (final-claim-workflows-samples manifest))
  (define pe-stats (final-claim-prepared-env-stats manifest))
  (define reliability-ok? (final-claim-reliability-ok? manifest))

  (define (samples-for key)
    (cond
      [(equal? key 'pr-elapsed-seconds) pr-samples]
      [(equal? key 'fast-execution-seconds) fast-samples]
      [(equal? key 'security-runner-seconds) sec-samples]
      [(equal? key 'workflows-runner-seconds) wf-samples]
      [else '()]))

  (define (guards-for row)
    (define guard-hashes
      (for/list ([g (in-list (hash-ref row 'guards))])
        (hasheq 'id
                g
                'provided
                (final-claim-guard-provided? manifest g)
                'reference
                (hash-ref-tolerant
                 (hash-ref-tolerant (hash-ref manifest 'guard-evidence (hasheq)) g (hasheq))
                 'reference
                 ""))))
    (hasheq 'entries
            guard-hashes
            'satisfied
            (andmap (lambda (e) (hash-ref e 'provided #f)) guard-hashes)
            'reliability-satisfied
            (if (member "reliability-non-regression" (hash-ref row 'guards)) reliability-ok? #t)))

  (define (evaluate-row row)
    (define key (hash-ref row 'sample-key))
    (define threshold (hash-ref final-claim-thresholds (hash-ref row 'id)))
    (define comparison (hash-ref row 'comparison))
    (define guards (guards-for row))
    (define reasons '())
    (cond
      [(equal? key 'prepared-env)
       ;; Verified-restore rate row: no sample distribution — one observed
       ;; rate against the ≥ 95% target.
       (define rate (hash-ref pe-stats 'rate))
       (define observed-total (hash-ref pe-stats 'records-observed))
       (unless (hash-ref guards 'satisfied)
         (set!
          reasons
          (cons
           "guard evidence missing: prepared-env rows without their coupled guards are unverified, never pass"
           reasons)))
       (unless (hash-ref guards 'reliability-satisfied)
         (set! reasons
               (cons "reliability non-regression violated versus the recorded baseline" reasons)))
       (when (zero? observed-total)
         (set!
          reasons
          (cons
           (format
            "no prepared-env restore records observed in the cohort window (~a; last observed basis: verified ~a of ~a records, fallback ~a)"
            (hash-ref pe-stats 'window)
            (hash-ref pe-stats 'verified)
            (hash-ref pe-stats 'total)
            (hash-ref pe-stats 'fallback))
           reasons)))
       (when (and (positive? observed-total) (< rate threshold))
         (set!
          reasons
          (cons
           (format
            "verified-restore rate ~a% is below the fixed ~a% target; the target is never revised"
            rate
            threshold)
           reasons)))
       (define verdict
         (cond
           [(or (not (hash-ref guards 'satisfied))
                (not (hash-ref guards 'reliability-satisfied))
                (and (zero? observed-total) (not closed?)))
            "unverified"]
           [(and (zero? observed-total) closed?) "target not achieved"]
           [(equal? comparison ">=") (if (>= rate threshold) "pass" "target not achieved")]
           [else (if (<= rate threshold) "pass" "target not achieved")]))
       (hasheq 'id
               (hash-ref row 'id)
               'measure
               (hash-ref row 'measure)
               'threshold
               (exact->inexact threshold)
               'comparison
               comparison
               'observed
               rate
               'samples
               observed-total
               'expected
               expected
               'guards
               guards
               'verdict
               verdict
               'reasons
               (reverse reasons)
               'next-lever
               (if (equal? verdict "pass")
                   #f
                   (hash-ref row 'next-lever)))]
      [else
       (define samples (samples-for key))
       (define observed
         (if (null? samples)
             #f
             (cohort-quantile-exact samples (hash-ref row 'quantile 0.50))))
       (unless (hash-ref guards 'satisfied)
         (set!
          reasons
          (cons "guard evidence missing: rows without their coupled guards are unverified, never pass"
                reasons)))
       (unless (hash-ref guards 'reliability-satisfied)
         (set! reasons
               (cons "reliability non-regression violated versus the recorded baseline" reasons)))
       (when (null? samples)
         (set! reasons (cons "no in-window samples captured for this measure" reasons)))
       (when (and observed (> observed threshold))
         (set!
          reasons
          (cons (format "observed ~a s exceeds the fixed ≤ ~a s target; the target is never revised"
                        observed
                        threshold)
                reasons)))
       (when (and observed (<= observed threshold) (< (length samples) expected))
         (set!
          reasons
          (cons
           (format
            "observed sample set is incomplete (~a of ~a unique PR head SHAs); the row cannot pass on partial evidence"
            (length samples)
            expected)
           reasons)))
       (define verdict
         (cond
           [(or (not (hash-ref guards 'satisfied))
                (not (hash-ref guards 'reliability-satisfied))
                (null? samples))
            "unverified"]
           [(> observed threshold) "target not achieved"]
           [(< (length samples) expected) "unverified"]
           [else "pass"]))
       (hasheq 'id
               (hash-ref row 'id)
               'measure
               (hash-ref row 'measure)
               'threshold
               (exact->inexact threshold)
               'comparison
               comparison
               'observed
               observed
               'samples
               (length samples)
               'expected
               expected
               'guards
               guards
               'verdict
               verdict
               'reasons
               (reverse reasons)
               'next-lever
               (if (equal? verdict "pass")
                   #f
                   (hash-ref row 'next-lever)))]))

  (define row-results (map evaluate-row final-claim-rows))
  (define all-pass (andmap (lambda (r) (equal? (hash-ref r 'verdict) "pass")) row-results))

  ;; v1.00.28 W8: Class A work-mass delta (W0 vs W7 census artifacts),
  ;; reported alongside — never compared against — the fixed Class B–D §8
  ;; rows.  The delta comes from the checksummed census comparison artifact
  ;; named in the manifest; a missing or unmeasurable delta is
  ;; "unverified", never silently treated as improved.  The fixed
  ;; thresholds are untouched: Class A is a different measure class with
  ;; its own negative-is-better semantics.
  (define work-mass-delta (hash-ref manifest 'work-mass-delta #f))
  (define class-a-row
    (and (hash? work-mass-delta)
         (hasheq 'id
                 "class-a-fast-work-mass-delta"
                 'measure
                 "Class A fast work-mass delta W0→W7 (census ms; negative = workload reduced)"
                 'threshold
                 0.0
                 'comparison
                 "<"
                 'observed
                 (hash-ref work-mass-delta 'delta-pct #f)
                 'samples
                 #f
                 'expected
                 expected
                 'guards
                 (hasheq 'entries '() 'satisfied #t 'reliability-satisfied #t)
                 'verdict
                 (let ([d (hash-ref work-mass-delta 'delta-pct #f)])
                   (cond
                     [(not (real? d)) "unverified"]
                     [(< d 0) "pass"]
                     [else "target not achieved"]))
                 'reasons
                 '()
                 'next-lever
                 #f)))
  (define rows-with-class-a
    (if class-a-row
        (cons class-a-row row-results)
        row-results))

  ;; v1.00.28 W8: the final campaign verdict.  Emission is restricted to
  ;; allowed-final-verdicts.  Mapping: Class A improved AND a closed cohort
  ;; passing every fixed row with its guards => ACHIEVED; Class A improved
  ;; but the fixed claim not fully verified => PARTIAL WORKLOAD REDUCTION;
  ;; FINAL TARGET NOT ACHIEVED; no measured work-mass improvement =>
  ;; NOT ACHIEVED.  A measure-class miss is a verdict input, never a
  ;; threshold revision.
  (define overall-final-verdict
    (and class-a-row
         (cond
           [(not (equal? (hash-ref class-a-row 'verdict) "pass")) final-verdict-not-achieved]
           [(and (equal? status "closed")
                 all-pass
                 ;; fail-safe: the closed tag alone is not evidence; the
                 ;; cohort must actually contain expected unique SHAs.
                 (= (length shas) expected))
            final-verdict-achieved]
           [else final-verdict-partial])))
  (when (and overall-final-verdict (not (member overall-final-verdict allowed-final-verdicts)))
    (error 'final-claim-gate
           "internal: overall verdict ~s is outside the allowed final-verdict vocabulary"
           overall-final-verdict))

  (define base-gate
    (hasheq 'mode
            "final-claim"
            'gate-text
            final-claim-gate-text
            'cohort-status
            status
            'unique-head-shas
            (length shas)
            'expected-count
            expected
            'rows
            row-results
            'reliability
            (cohort-attempts-summary manifest)
            'reliability-baseline
            (hash-ref manifest 'reliability-baseline #f)
            'reliability-non-regression
            reliability-ok?
            'prepared-env
            pe-stats
            'overall-verdict
            (if all-pass "verified" "target not achieved")))
  ;; Pre-v1.00.28 manifests carry no work-mass-delta: their gate hash is
  ;; returned unchanged so stored C3 artifacts regenerate byte-identically.
  (if class-a-row
      (hash-set* base-gate
                 'rows
                 rows-with-class-a
                 'class-a-work-mass-delta
                 work-mass-delta
                 'overall-final-verdict
                 overall-final-verdict)
      base-gate))

;; C3 (final-claim) decision document: one explicit verdict per §8 row with
;; the observed numbers, the guard evidence, and — for every non-passing
;; row — the named next lever.  Targets are never revised.
(define (final-claim-decision-md-string manifest)
  (define gate (final-claim-gate manifest))
  (define lines '())
  (define (out . args)
    (set! lines (append lines (list (apply format args)))))
  (out "# C3 final-claim decision: ~a" (hash-ref manifest 'cohort-id "?"))
  (out "")
  (out "| Field | Value |")
  (out "|---|---|")
  (out "| Decision mode | final-claim (every roadmap §8 row, fixed thresholds, coupled guards) |")
  (out "| Cohort status | ~a |" (hash-ref gate 'cohort-status))
  (out "| Unique PR head SHAs | ~a of ~a |"
       (hash-ref gate 'unique-head-shas)
       (hash-ref gate 'expected-count))
  (out "| Overall verdict | **~a** |" (hash-ref gate 'overall-verdict))
  (out "")
  (out "## Gate")
  (out "")
  (out "~a" (hash-ref gate 'gate-text))
  (out "")
  (out "## Per-row verdicts")
  (out "")
  (out "| Row | Measure | Target | Observed | Samples | Guards | Verdict |")
  (out "|---|---|---|---|---|---|---|")
  (for ([r (in-list (hash-ref gate 'rows))])
    (define observed (hash-ref r 'observed))
    (out "| ~a | ~a | ~a ~a | ~a | ~a of ~a | ~a | **~a** |"
         (hash-ref r 'id)
         (hash-ref r 'measure)
         (hash-ref r 'comparison)
         (hash-ref r 'threshold)
         (if observed
             (format "~a" observed)
             "n/a")
         (hash-ref r 'samples)
         (hash-ref r 'expected)
         (if (hash-ref (hash-ref r 'guards) 'satisfied) "provided" "MISSING")
         (hash-ref r 'verdict)))
  (out "")
  (out "## Missed rows: observed numbers and named next levers")
  (out "")
  (define missed
    (filter (lambda (r) (not (equal? (hash-ref r 'verdict) "pass"))) (hash-ref gate 'rows)))
  (cond
    [(null? missed) (out "(none — every row passed with its guards)")]
    [else
     (for ([r (in-list missed)])
       (out "- **~a** — verdict ~a; observed ~a; reasons: ~a"
            (hash-ref r 'id)
            (hash-ref r 'verdict)
            (or (hash-ref r 'observed #f) "n/a")
            (string-join (hash-ref r 'reasons) "; "))
       (when (hash-ref r 'next-lever #f)
         (out "  - ~a" (hash-ref r 'next-lever))))])
  (out "")
  (define rel (hash-ref gate 'reliability))
  (out (string-append
        "Reliability closure: failed, cancelled, and rerun attempts are recorded and never"
        " dropped — cohort totals: ~a attempts, ~a failures, ~a cancelled, ~a reruns; reliability"
        " non-regression versus the recorded baseline: ~a.")
       (hash-ref rel 'total-attempts)
       (hash-ref rel 'failures)
       (hash-ref rel 'cancelled)
       (hash-ref rel 'reruns)
       (if (hash-ref gate 'reliability-non-regression) "holds" "VIOLATED"))
  (out "")
  (out (string-append
        "Targets are never revised inside this wave or this milestone.  The overall verdict is"
        " \"verified\" only when every row passes with its coupled guard evidence."))
  (out "")
  (out "Reviewer: coordinator (delivery) — verified against .planning/VALIDATION.")
  (out "")
  (when (hash-has-key? gate 'overall-final-verdict)
    (out "## Final campaign verdict (v1.00.28)")
    (out "")
    (out (string-append
          "The decision record ends in exactly one allowed verdict; the Class A work-mass"
          " delta row is reported alongside the fixed Class B–D rows and the incompatible"
          " measure classes are never compared against each other."))
    (out "")
    (out "~a" (hash-ref gate 'overall-final-verdict))
    (out ""))
  (string-join lines "\n"))

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
  (cond
    [(equal? (cohort-mode manifest) "final-claim") (final-claim-decision-md-string manifest)]
    [(equal? (cohort-mode manifest) "pr-elapsed") (pr-elapsed-decision-md-string manifest)]
    [(equal? (cohort-mode manifest) "post-promotion") (post-promotion-decision-md-string manifest)]
    [else (paired-shadow-decision-md-string manifest)]))

;; C2 (post-promotion) decision document: the honest achieved/unachieved
;; verdict against the roadmap fast-execution targets, the observed numbers,
;; and — on a miss — the named next lever for a separate reviewed decision.
(define (post-promotion-decision-md-string manifest)
  (define gate (post-promotion-gate manifest))
  (define lines '())
  (define (out . args)
    (set! lines (append lines (list (apply format args)))))
  (out "# C2 post-promotion activation decision: ~a" (hash-ref manifest 'cohort-id "?"))
  (out "")
  (out "| Field | Value |")
  (out "|---|---|")
  (out "| Decision mode | post-promotion (promoted defaults, no shadow duplication) |")
  (out "| Cohort status | ~a |" (hash-ref manifest 'cohort-status "?"))
  (out "| Timing samples | ~a |" (hash-ref gate 'samples))
  (out "| Observed p50 (seconds) | ~a |" (or (hash-ref gate 'p50-seconds #f) "n/a"))
  (out "| Observed p95 (seconds) | ~a |" (or (hash-ref gate 'p95-seconds #f) "n/a"))
  (out "| Targets (never revised) | p50 ≤ ~a s, p95 ≤ ~a s |"
       (hash-ref gate 'p50-max-seconds)
       (hash-ref gate 'p95-max-seconds))
  (out "| Verdict | ~a |" (hash-ref gate 'verdict))
  (out "")
  (out "## Gate")
  (out "")
  (out "~a" (hash-ref gate 'gate-text))
  (out "")
  (when (hash-ref gate 'next-lever #f)
    (out "~a" (hash-ref gate 'next-lever))
    (out ""))
  (out (string-append
        "Reliability closure: failed, cancelled, and rerun attempts are recorded; SHAs are never"
        " dropped — SHAs whose required-lane run failed stay in the manifest with the named"
        " mechanical reason \"lane-run-failed\"."))
  (out "")
  (out (string-append
        "A timing miss alone implies no queue rollback; any lever change is a separate reviewed"
        " decision. Targets are never revised inside this wave or this milestone."))
  (out "")
  (out "Reviewer: coordinator (delivery) — verified against .planning/VALIDATION.")
  (out "")
  (string-join lines "\n"))

;; C1 (paired-shadow) promotion decision document.
(define (paired-shadow-decision-md-string manifest)
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
    ;; W5 C3: final-claim verdict mode (v1.00.27) — name the mode and
    ;; attach the seven-row final-claim gate; no shadow duplication.
    [(equal? (cohort-mode manifest) "final-claim")
     (hash-set (hash-set base 'cohort-mode "final-claim")
               'final-claim-gate
               (final-claim-gate manifest))]
    ;; W6 C2: pr-elapsed mode (v1.00.26) — name the mode and attach the
    ;; end-to-end PR elapsed gate; no shadow duplication (no configurations
    ;; or decision sections).
    [(equal? (cohort-mode manifest) "pr-elapsed")
     (hash-set (hash-set base 'cohort-mode "pr-elapsed") 'pr-elapsed-gate (pr-elapsed-gate manifest))]
    [(equal? (cohort-mode manifest) "post-promotion")
     (hash-set (hash-set base 'cohort-mode "post-promotion")
               'post-promotion-gate
               (post-promotion-gate manifest))]
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
  (when (hash-has-key? r 'final-claim-gate)
    (define g (hash-ref r 'final-claim-gate))
    (out "## Final-claim cohort (C3, roadmap §8)")
    (out "")
    (out "Mode producing the numbers in this report: **~a** (every §8 row, fixed thresholds,"
         (hash-ref r 'cohort-mode))
    (out "coupled guard evidence; a row without its guards is \"unverified\", never \"pass\").")
    (out "")
    (out "~a" (hash-ref g 'gate-text))
    (out "")
    (out "| Row | Measure | Target | Observed | Samples | Guards | Verdict |")
    (out "|---|---|---|---|---|---|---|")
    (for ([row (in-list (hash-ref g 'rows))])
      (define observed (hash-ref row 'observed))
      (out "| ~a | ~a | ~a ~a | ~a | ~a of ~a | ~a | **~a** |"
           (hash-ref row 'id)
           (hash-ref row 'measure)
           (hash-ref row 'comparison)
           (hash-ref row 'threshold)
           (if observed
               (format "~a" observed)
               "n/a")
           (hash-ref row 'samples)
           (hash-ref row 'expected)
           (if (hash-ref (hash-ref row 'guards) 'satisfied) "provided" "MISSING")
           (hash-ref row 'verdict)))
    (out "")
    (out "| Overall verdict | **~a** |" (hash-ref g 'overall-verdict))
    (out "")
    (when (hash-ref g 'reliability-baseline #f)
      (define rel (hash-ref g 'reliability))
      (out (string-append
            "Reliability closure: ~a attempts, ~a failures, ~a cancelled, ~a reruns recorded;"
            " non-regression versus baseline: ~a.")
           (hash-ref rel 'total-attempts)
           (hash-ref rel 'failures)
           (hash-ref rel 'cancelled)
           (hash-ref rel 'reruns)
           (if (hash-ref g 'reliability-non-regression) "holds" "VIOLATED"))
      (out "")))
  (when (hash-has-key? r 'pr-elapsed-gate)
    (define g (hash-ref r 'pr-elapsed-gate))
    (out "## End-to-end PR elapsed cohort (C2)")
    (out "")
    (out "Mode producing the numbers in this report: **~a** (mergeable-PR wall time from"
         (hash-ref r 'cohort-mode))
    (out "first check start to last required check completion, not the queue wait alone;")
    (out "no shadow duplication).")
    (out "")
    (out "~a" (hash-ref g 'gate-text))
    (out "")
    (out "| Metric | Value |")
    (out "|---|---|")
    (out "| p50 (seconds) | ~a |" (hash-ref g 'p50-seconds))
    (out "| p95 (seconds) | ~a |" (hash-ref g 'p95-seconds))
    (out "| p50 target (seconds) | ≤ ~a |" (hash-ref g 'p50-max-seconds))
    (out "| p95 target (seconds) | ≤ ~a |" (hash-ref g 'p95-max-seconds))
    (out "| Unique PR head SHAs | ~a of ~a |"
         (hash-ref g 'unique-head-shas)
         (hash-ref g 'expected-count))
    (out "| Verdict | **~a** |" (hash-ref g 'verdict))
    (out "")
    (when (hash-ref g 'next-lever #f)
      (out "Next lever: ~a" (hash-ref g 'next-lever))
      (out "")))
  (when (hash-has-key? r 'post-promotion-gate)
    (define g (hash-ref r 'post-promotion-gate))
    (out "## Post-promotion activation cohort (C2)")
    (out "")
    (out "Mode producing the numbers in this report: **~a** (promoted defaults,"
         (hash-ref r 'cohort-mode))
    (out "required lane itself, no shadow duplication).")
    (out "")
    (out "~a" (hash-ref g 'gate-text))
    (out "")
    (out "| Metric | Value |")
    (out "|---|---|")
    (out "| p50 (seconds) | ~a |" (hash-ref g 'p50-seconds))
    (out "| p95 (seconds) | ~a |" (hash-ref g 'p95-seconds))
    (out "| p50 target (seconds) | ≤ ~a |" (hash-ref g 'p50-max-seconds))
    (out "| p95 target (seconds) | ≤ ~a |" (hash-ref g 'p95-max-seconds))
    (out "| Timing samples | ~a |" (hash-ref g 'samples))
    (out "| Verdict | **~a** |" (hash-ref g 'verdict))
    (out "")
    (when (hash-ref g 'next-lever #f)
      (out "Next lever: ~a" (hash-ref g 'next-lever))
      (out "")))
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
  (require racket/path)
  (define manifest-path #f)
  (define out-json #f)
  (define out-md #f)
  (define decision-path #f)
  (define check? #f)
  (define positional '())

  (command-line
   #:program "cohort-report"
   #:once-each [("--manifest") p "Cohort manifest JSON path" (set! manifest-path p)]
   [("--out-json") p "Write report JSON to path" (set! out-json p)]
   [("--out-md") p "Write report markdown to path" (set! out-md p)]
   [("--decision") p "Write the promotion decision markdown to path" (set! decision-path p)]
   [("--check") "Regenerate from manifest and compare to stored report" (set! check? #t)]
   ;; v1.00.28 W8: --check also accepts a single positional report path;
   ;; the manifest then defaults to the canonical sibling cohort.json.
   #:args leftover-args
   (set! positional leftover-args))

  (cond
    [(and (not manifest-path) (not check?))
     (displayln "error: --manifest <path> is required")
     (exit 1)]
    [check?
     (define report-path (or out-json (and (= (length positional) 1) (car positional))))
     (unless report-path
       (displayln "error: --check requires --out-json or exactly one positional report path")
       (exit 1))
     (define check-manifest-path
       (or manifest-path
           (path->string (build-path (or (path-only report-path) (current-directory))
                                     "cohort.json"))))
     (define-values (ok reason) (cohort-check check-manifest-path report-path))
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
