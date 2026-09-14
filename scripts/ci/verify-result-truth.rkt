#!/usr/bin/env racket
#lang racket/base

;; scripts/ci/verify-result-truth.rkt — BUG-0073 result-truth verifier.
;;
;; Verifies aggregate CI result truth at the required aggregate boundary
;; (ci.yml test-aggregate, the only place a green claim can be minted),
;; not merely that some file looks green.  The verifier checks that a
;; claimed-clean aggregate is structurally valid AND consistent with the
;; runner/tee/artifact conclusions supplied to it, so a successful tee
;; (or a partial JSON, a missing shard, invalid totals, a stale run SHA
;; or a failing artifact) can never be mistaken for genuine success.
;;
;; Exit codes (fail closed):
;;   0  verified genuine clean success
;;   1  truth mismatch: runner/tee/artifact conclusion contradicts the
;;      claimed aggregate, or the aggregate is not clean (failures or
;;      timeouts under a clean claim)
;;   2  structural invalidity: missing shard, unparseable JSON, missing
;;      fields, invalid or inconsistent totals
;;   3  run-SHA binding mismatch
;;
;; Offline and deterministic: reads only the aggregate JSON passed in and
;; the CLI flags; no network, threads, sleeps or clocks.

(require json
         racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/port
         racket/string)

;; ---------------------------------------------------------------------------
;; CLI
;; ---------------------------------------------------------------------------

(define aggregate-path
  (make-parameter #f
                  (lambda (v)
                    (if (string? v)
                        (string->path v)
                        v))))
(define expect-shards (make-parameter #f))
(define expect-sha (make-parameter #f))
(define runner-conclusion (make-parameter "success"))
(define artifact-verdict (make-parameter #f))
(define artifact-exit (make-parameter #f))
(define json-out-path (make-parameter #f))

(define reasons '())
(define (record! kind message)
  (set! reasons (cons (hasheq 'kind kind 'message message) reasons)))

;; Sentinel for absent hash fields (distinct from any JSON value).
(define missing-shard-sentinel (gensym 'missing))

(define parse/exit-code
  (command-line
   #:program "verify-result-truth"
   #:once-each [("--aggregate")
                p
                "Path to the aggregate results summary JSON (required)"
                (aggregate-path (string->path p))]
   [("--expect-shards") n "Required number of shard results" (expect-shards (string->number n))]
   [("--expect-sha") sha "Git SHA every shard result must be bound to" (expect-sha sha)]
   [("--runner-conclusion")
    c
    "Runner conclusion for the shard jobs (default success)"
    (runner-conclusion c)]
   [("--artifact-verdict") v "Verdict recorded in the uploaded artifact" (artifact-verdict v)]
   [("--artifact-exit")
    e
    "Exit code recorded in the uploaded artifact"
    (artifact-exit (string->number e))]
   [("--json-out") p "Optional path for the verification report JSON" (json-out-path p)]
   #:args ()
   (void)))

(unless (aggregate-path)
  (raise-user-error 'verify-result-truth "--aggregate is required"))

;; ---------------------------------------------------------------------------
;; Structural reading (exit 2 on any structural problem)
;; ---------------------------------------------------------------------------

(define (structural! message)
  (record! 'structural message))

(define raw-aggregate
  (with-handlers ([exn:fail? (lambda (e)
                               (structural! (format "aggregate unreadable: ~a" (exn-message e)))
                               #f)])
    (with-handlers ([exn:fail? (lambda (e)
                                 (structural! (format "aggregate is not valid JSON: ~a"
                                                      (exn-message e)))
                                 #f)])
      (read-json (open-input-string (file->string (aggregate-path)))))))

(define aggregate (if (hash? raw-aggregate) raw-aggregate #f))
(unless (and raw-aggregate (hash? raw-aggregate))
  (when raw-aggregate
    (structural! "aggregate JSON is not an object")))

(define (nonneg-integer? v)
  (and (integer? v) (>= v 0)))

(define shards
  (cond
    [(not aggregate) '()]
    [(not (hash-has-key? aggregate 'shards))
     (structural! "aggregate has no shards array")
     '()]
    [(not (list? (hash-ref aggregate 'shards)))
     (structural! "aggregate shards is not an array")
     '()]
    [else (hash-ref aggregate 'shards)]))

(define required-shard-fields '(artifact shard file_count pass fail timeout skip))
(for ([shard (in-list shards)])
  (unless (hash? shard)
    (structural! "a shard entry is not an object"))
  (when (hash? shard)
    (for ([field (in-list required-shard-fields)])
      (unless (hash-has-key? shard field)
        (structural! (format "shard ~a is missing field ~a" (hash-ref shard 'artifact "?") field))))))

;; Per-shard totals must be nonnegative and must sum exactly to file_count.
(for ([shard (in-list shards)])
  (when (hash? shard)
    (define fields
      (for/list ([f (in-list '(file_count pass fail timeout skip))])
        (hash-ref shard f #f)))
    (if (andmap nonneg-integer? fields)
        (let ([sum (+ (hash-ref shard 'pass)
                      (hash-ref shard 'fail)
                      (hash-ref shard 'timeout)
                      (hash-ref shard 'skip))])
          (unless (= sum (hash-ref shard 'file_count))
            (structural!
             (format "shard ~a totals are invalid: pass+fail+timeout+skip=~a but file_count=~a"
                     (hash-ref shard 'artifact "?")
                     sum
                     (hash-ref shard 'file_count)))))
        (structural! (format "shard ~a has a non-integer or negative count"
                             (hash-ref shard 'artifact "?"))))))

;; Required shard count.
(when (expect-shards)
  (unless (exact-positive-integer? (expect-shards))
    (structural! "--expect-shards must be a positive integer"))
  (when (and (exact-positive-integer? (expect-shards)) aggregate)
    (unless (= (length shards) (expect-shards))
      (structural! (format "missing shard results: expected ~a shards, found ~a"
                           (expect-shards)
                           (length shards))))))

;; Aggregate totals must equal the sum over shards.
(when (and aggregate (pair? shards))
  (define (aggregate-total field)
    (hash-ref aggregate field missing-shard-sentinel))
  (for ([field (in-list '(file_count pass fail timeout skip))])
    (define shard-sum
      (for/sum ([shard (in-list shards)])
               (if (hash? shard)
                   (let ([v (hash-ref shard field #f)]) (if (nonneg-integer? v) v 0))
                   0)))
    (define reported (aggregate-total field))
    (cond
      [(not (nonneg-integer? reported))
       (structural! (format "aggregate field ~a is missing or not a nonnegative integer" field))]
      [(not (= reported shard-sum))
       (structural!
        (format "aggregate ~a=~a disagrees with shard sum ~a" field reported shard-sum))])))

;; ---------------------------------------------------------------------------
;; Run-SHA binding (exit 3)
;; ---------------------------------------------------------------------------

(when (expect-sha)
  ;; The claimed aggregate itself must be bound to this run's SHA.
  (when (and aggregate (hash-has-key? aggregate 'run_sha))
    (unless (equal? (hash-ref aggregate 'run_sha) (expect-sha))
      (record! 'sha-binding
               (format "aggregate run SHA is ~a but this run is at ~a"
                       (hash-ref aggregate 'run_sha)
                       (expect-sha)))))
  ;; Every shard must carry and match the run SHA — an unbound shard
  ;; cannot prove it executed at the claimed commit.
  (for ([shard (in-list shards)])
    (when (hash? shard)
      (cond
        [(not (hash-has-key? shard 'head_sha))
         (record! 'sha-binding
                  (format "shard ~a has no run SHA binding; cannot prove it ran at ~a"
                          (hash-ref shard 'artifact "?")
                          (expect-sha)))]
        [(not (equal? (hash-ref shard 'head_sha) (expect-sha)))
         (record! 'sha-binding
                  (format "shard ~a ran at ~a but this run is at ~a"
                          (hash-ref shard 'artifact "?")
                          (hash-ref shard 'head_sha)
                          (expect-sha)))]))))

;; ---------------------------------------------------------------------------
;; Truth conclusions (exit 1)
;; ---------------------------------------------------------------------------

(when aggregate
  (define claimed-clean
    (and (nonneg-integer? (hash-ref aggregate 'fail missing-shard-sentinel))
         (nonneg-integer? (hash-ref aggregate 'timeout missing-shard-sentinel))
         (zero? (hash-ref aggregate 'fail))
         (zero? (hash-ref aggregate 'timeout))))

  ;; A clean claim must actually be clean.
  (when (positive? (length shards))
    (define fails
      (for/sum ([s (in-list shards)])
               (if (hash? s)
                   (or (hash-ref s 'fail #f) 0)
                   0)))
    (define timeouts
      (for/sum ([s (in-list shards)])
               (if (hash? s)
                   (or (hash-ref s 'timeout #f) 0)
                   0)))
    (when (and claimed-clean (or (> fails 0) (> timeouts 0)))
      (record!
       'truth-mismatch
       (format "aggregate claims clean but shards report fail=~a timeout=~a" fails timeouts))))

  ;; Gate semantics: this verifier is the only place a green claim can be
  ;; minted, so a non-clean aggregate (any fail or timeout, or missing
  ;; totals) must never verify as genuine success — even when it reports
  ;; its own failure honestly.
  (unless claimed-clean
    (record!
     'truth-mismatch
     (format
      "aggregate is not a clean result (fail=~a timeout=~a); no genuine success can be verified"
      (hash-ref aggregate 'fail #f)
      (hash-ref aggregate 'timeout #f))))

  ;; Runner conclusion: a failed/timed-out runner can never be a green
  ;; aggregate (the BUG-0073 failed-runner/successful-tee canary).
  (unless (equal? (runner-conclusion) "success")
    (when claimed-clean
      (record! 'truth-mismatch
               (format "runner conclusion is ~a but the aggregate claims a clean result"
                       (runner-conclusion)))))

  ;; Tee failures: a tee status recorded per shard must be zero when the
  ;; claim is clean, otherwise captured evidence is incomplete.
  (for ([shard (in-list shards)])
    (when (hash? shard)
      (when (hash-has-key? shard 'tee_exit)
        (define tee-exit (hash-ref shard 'tee_exit))
        (when (and (real? tee-exit) (not (zero? tee-exit)) claimed-clean)
          (record! 'truth-mismatch
                   (format "shard ~a tee failed (tee_exit=~a); clean claim has incomplete evidence"
                           (hash-ref shard 'artifact "?")
                           tee-exit))))))

  ;; Artifact truth: a failing artifact under a successful runner is a
  ;; masked failure.
  (when (or (artifact-verdict) (artifact-exit))
    (define artifact-failed
      (or (and (artifact-verdict)
               (not (member (string-downcase (string-trim (artifact-verdict)))
                            '("passed" "pass" "success" "clean"))))
          (and (artifact-exit) (not (zero? (artifact-exit))))))
    (when (and artifact-failed (equal? (runner-conclusion) "success") claimed-clean)
      (record! 'truth-mismatch
               "artifact reports failure while the runner and aggregate claim success"))))

;; ---------------------------------------------------------------------------
;; Verdict and report
;; ---------------------------------------------------------------------------

(define sha-binding? (findf (lambda (r) (equal? (hash-ref r 'kind) 'sha-binding)) (reverse reasons)))
(define structural? (findf (lambda (r) (equal? (hash-ref r 'kind) 'structural)) (reverse reasons)))
(define truth? (findf (lambda (r) (equal? (hash-ref r 'kind) 'truth-mismatch)) (reverse reasons)))

(define exit-code
  (cond
    [sha-binding? 3]
    [structural? 2]
    [truth? 1]
    [else 0]))

(define report
  (hasheq 'verdict
          (if (zero? exit-code) "verified" "rejected")
          'exit_code
          exit-code
          'aggregate
          (if (aggregate-path)
              (path->string (aggregate-path))
              "")
          'runner_conclusion
          (runner-conclusion)
          'reasons
          (reverse reasons)))

(define (report-json)
  (jsexpr->string report))

(when (json-out-path)
  (with-handlers ([exn:fail? void])
    (with-output-to-file (json-out-path) (lambda () (displayln (report-json))) #:exists 'replace)))

(unless (zero? exit-code)
  (for ([reason (in-list (reverse reasons))])
    (eprintf "verify-result-truth: ~a: ~a\n" (hash-ref reason 'kind) (hash-ref reason 'message)))
  (eprintf "verify-result-truth: verdict rejected (exit ~a)\n" exit-code)
  (exit exit-code))

(printf "verify-result-truth: verdict verified: genuine clean success at the aggregate boundary\n")
(exit 0)
