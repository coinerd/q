#lang racket/base

;; @speed fast
;; @suite testing
;; @isolation offline
;; @boundary scripts/ci

;; W5 (v1.00.25): prepared-environment restore evidence tooling.
;;
;; Modes (see tests/test-prepared-env-report.rkt for the contract):
;;
;;   --emit-restore-record   one machine-readable restore outcome per test
;;                           shard (ci.yml wiring). Reads the telemetry the
;;                           setup action already publishes
;;                           (Q_PREPARED_ENV_STATE / _RESTORE_MS /
;;                           _FALLBACK_MS) without changing install/relink
;;                           semantics or the loud-fallback behavior.
;;   --aggregate DIR         fold shard records into the committed report.
;;   --manifest F --check    the durable machine-checked gate; optionally
;;                           --write-checksums to (re)bind SHA256SUMS.
;;
;; W6 (v1.00.29): the prepared-environment IDENTITY MANIFEST (spec §6 W4):
;;
;;   --identity-emit         write one immutable identity manifest (OS
;;                           image, arch, Racket version + executable
;;                           digest, lock/resolved set, precompile recipe
;;                           revision, policy knobs, artifact digest) and
;;                           derive the artifact identity from it
;;   --identity-compare      expected vs observed; ANY mismatch = loud,
;;                           counted cold fallback (never silent)
;;   --identity-fallback-record  the counted fallback record
;;   --consumers-check       fail-closed consumers.json validation
;;   --savings-check         fail-closed setup-savings.json validation
;;   --rollback-drill        evaluate one consumer's §11.6 rollback
;;
;; Honesty rules enforced here: outcomes are exactly
;; verified | rebuilt | fallback | unknown; every non-verified outcome
;; names its fallback cause ("unknown" itself is only allowed as the
;; honest attribution gap recorded from telemetry, never invented);
;; missing numeric data is the string "unknown", never a fabricated zero;
;; the report is bound to a SHA256SUMS file; and fresh measurements are
;; labeled as such — historical v1.00.11/v1.00.16 numbers are referenced
;; as history, never substituted.

(require json
         (file "../run-tests/sha256.rkt")
         racket/file
         racket/list
         racket/path
         racket/port
         racket/string
         racket/format)

;; ---------------------------------------------------------------------------
;; Deterministic JSON output
;;
;; racket/json's write-json does not accept association lists, and hash
;; iteration order is unspecified, so we emit ordered JSON objects from
;; assoc lists ourselves. Tagged (obj ...) / (arr ...) values make empty
;; collections unambiguous. Byte-identical regeneration is what makes the
;; SHA256SUMS binding meaningful.
;; ---------------------------------------------------------------------------

(define (json-obj . kvs)
  (cons 'obj kvs))
(define (json-arr . xs)
  (cons 'arr xs))

(define (json-string-escape s)
  (string-append "\""
                 (for/fold ([acc ""]) ([c (in-string s)])
                   (string-append acc
                                  (case c
                                    [(#\") "\\\""]
                                    [(#\\) "\\\\"]
                                    [(#\newline) "\\n"]
                                    [(#\return) "\\r"]
                                    [(#\tab) "\\t"]
                                    [else (string c)])))
                 "\""))

(define (hex-digit n)
  (string-ref "0123456789abcdef" n))

(define (hex-encode bs)
  (list->string (append* (for/list ([b (in-bytes bs)])
                           (list (hex-digit (arithmetic-shift b -4))
                                 (hex-digit (bitwise-and b 15)))))))

(define (json-value->string v)
  (cond
    [(string? v) (json-string-escape v)]
    [(real? v) (number->string v)]
    [(boolean? v) (if v "true" "false")]
    [(and (pair? v) (eq? (car v) 'obj))
     (string-append "{"
                    (string-join (for/list ([kv (in-list (cdr v))])
                                   (string-append (json-string-escape (if (symbol? (car kv))
                                                                          (symbol->string (car kv))
                                                                          (format "~a" (car kv))))
                                                  ":"
                                                  (json-value->string (cdr kv))))
                                 ",")
                    "}")]
    [(and (pair? v) (eq? (car v) 'arr))
     (string-append "[" (string-join (map json-value->string (cdr v)) ",") "]")]
    [(null? v) "[]"]
    [else (json-string-escape (format "~a" v))]))

(define (write-json-file path v)
  (with-output-to-file path
                       (lambda ()
                         (display (json-value->string v))
                         (display "\n"))
                       #:exists 'replace))

;; ---------------------------------------------------------------------------
;; Shared normalization
;; ---------------------------------------------------------------------------

(define (classify-outcome state producer-result prepared-env-mode)
  (cond
    [(equal? state "restored") (values "verified" #f)]
    [(equal? state "rebuilt") (values "rebuilt" "restore-mismatch-or-failure")]
    [(or (equal? state "unavailable") (equal? prepared-env-mode "off"))
     (cond
       [(equal? producer-result "skipped") (values "fallback" "producer-skipped")]
       [(equal? producer-result "failure") (values "fallback" "producer-failed")]
       [else (values "unknown" #f)])]
    [else (values "unknown" #f)]))

(define (duration-or-unknown v)
  (if (and (real? v) (positive? v)) v "unknown"))

(define (number-arg args flag)
  (define raw (assoc-value args flag))
  (and (string? raw) (let ([n (string->number raw)]) (and (real? n) (positive? n) n))))

(define (env-positive-number name)
  (define raw (getenv name))
  (and raw (let ([n (string->number raw)]) (and (real? n) (positive? n) n))))

(define (hash-ref-unknown h key)
  (hash-ref h key "unknown"))

;; Records are tagged (obj kv ...) association lists; unwrap before assq.
(define (alist-ref alist key)
  (define kvs
    (if (and (pair? alist) (eq? (car alist) 'obj))
        (cdr alist)
        alist))
  (cond
    [(assq key kvs)
     =>
     cdr]
    [else "unknown"]))

;; ---------------------------------------------------------------------------
;; Argument plumbing
;; ---------------------------------------------------------------------------

(define (assoc-value args flag)
  (let loop ([xs args])
    (cond
      [(null? xs) #f]
      [(equal? (car xs) flag)
       (if (pair? (cdr xs))
           (cadr xs)
           #t)]
      [else (loop (cdr xs))])))

(define (has-flag? args flag)
  (if (member flag args) #t #f))

(define (usage-fail message)
  (fprintf
   (current-error-port)
   "prepared-env-report: ~a
usage:
  prepared-env-report.rkt --emit-restore-record --out F --run-id N --head-sha S --shard N --created-at-utc T \
  [--wall-clock-seconds V] [--fast-env-producer-result R] [--prepared-artifact-name A] [--installer-sha256 H]
  prepared-env-report.rkt --aggregate DIR --out F [--filter-prefix P] [--campaign C]
  prepared-env-report.rkt --manifest F --check [--write-checksums]
  prepared-env-report.rkt --identity-emit --out F --os OS --os-image IMG --arch A --racket-version V \
  --racket-executable-digest D --lock-digest L --resolved-set-digest R --precompile-recipe-revision P \
  --policy-fingerprint PF --artifact-digest AD [--artifact-name-base B]
  prepared-env-report.rkt --identity-compare --expected F --observed F [--out VERDICT] [--consumer NAME]
  prepared-env-report.rkt --identity-fallback-record --out F --consumer NAME --reason REASON \
  [--identity-manifest F] [--created-at-utc T]
  prepared-env-report.rkt --consumers-check PATH
  prepared-env-report.rkt --savings-check PATH
  prepared-env-report.rkt --rollback-drill --consumers F --consumer NAME [--vars K=V,K=V] \
  [--event push|workflow_dispatch] [--producer-result success|skipped|failure]
"
   message)
  2)

;; ---------------------------------------------------------------------------
;; Mode: --emit-restore-record
;; ---------------------------------------------------------------------------

(define (do-emit args)
  (define out (assoc-value args "--out"))
  (unless (string? out)
    (exit (usage-fail "--emit-restore-record requires --out")))
  (define state (or (getenv "Q_PREPARED_ENV_STATE") "unknown"))
  (define restore-ms (or (env-positive-number "Q_PREPARED_ENV_RESTORE_MS") "unknown"))
  (define fallback-ms (or (env-positive-number "Q_PREPARED_ENV_FALLBACK_MS") "unknown"))
  (define producer-result (or (assoc-value args "--fast-env-producer-result") "unknown"))
  (define prepared-env-mode
    (or (getenv "PREPARED_ENV") (assoc-value args "--prepared-env-mode") "unknown"))
  (define wall-clock (or (number-arg args "--wall-clock-seconds") "unknown"))
  (define run-id-raw (or (assoc-value args "--run-id") "unknown"))
  (define shard-raw (or (assoc-value args "--shard") "unknown"))
  (define artifact-name (or (assoc-value args "--prepared-artifact-name") "unknown"))
  (define installer-sha (or (assoc-value args "--installer-sha256") "unknown"))
  (define cache-key
    (if (and (not (equal? artifact-name "unknown")) (not (equal? installer-sha "unknown")))
        (string-append artifact-name ":" installer-sha)
        "unknown"))
  (define-values (outcome fallback-cause) (classify-outcome state producer-result prepared-env-mode))
  (define record-source (or (assoc-value args "--record-source") "ci-emitted"))
  (define record
    (json-obj (cons 'schema "prepared-env-restore-record")
              (cons 'run-id (or (string->number run-id-raw) run-id-raw))
              (cons 'head-sha (or (assoc-value args "--head-sha") "unknown"))
              (cons 'created-at-utc (or (assoc-value args "--created-at-utc") "unknown"))
              (cons 'shard (or (string->number shard-raw) shard-raw))
              (cons 'prepared-env-mode prepared-env-mode)
              (cons 'fast-env-producer-result producer-result)
              (cons 'raw-state state)
              (cons 'outcome outcome)
              (if fallback-cause
                  (cons 'fallback-cause fallback-cause)
                  (cons 'record-source record-source))
              (cons 'restore-ms restore-ms)
              (cons 'fallback-ms fallback-ms)
              (cons 'wall-clock-seconds wall-clock)
              (cons 'cache-key cache-key)
              (cons 'sha-context (or (assoc-value args "--head-sha") "unknown"))))
  (write-json-file out record)
  (printf "prepared-env-report: restore record written to ~a (outcome: ~a)\n" out outcome)
  0)

;; ---------------------------------------------------------------------------
;; Mode: --aggregate
;; ---------------------------------------------------------------------------

(define (record-sort-key rec)
  (list (let ([n (alist-ref rec 'run-id)]) (if (real? n) n 0))
        (let ([n (alist-ref rec 'shard)]) (if (real? n) n 0))))

(define (key<? a b)
  (cond
    [(null? a) #f]
    [(null? b) #t]
    [(< (car a) (car b)) #t]
    [(> (car a) (car b)) #f]
    [else (key<? (cdr a) (cdr b))]))

(define (normalize-input-record h)
  (define-values (outcome fallback-cause)
    ;; Emitted records carry the raw CI state under `raw-state`; hand-fed
    ;; fixtures may use `state`. Both are the same observed telemetry.
    (classify-outcome (let ([st (hash-ref h 'state #f)])
                        (if st
                            st
                            (hash-ref-unknown h 'raw-state)))
                      (hash-ref-unknown h 'fast-env-producer-result)
                      (hash-ref-unknown h 'prepared-env-mode)))
  (json-obj (cons 'run-id (hash-ref-unknown h 'run-id))
            (cons 'head-sha (hash-ref-unknown h 'head-sha))
            (cons 'shard (hash-ref-unknown h 'shard))
            (cons 'created-at-utc (hash-ref-unknown h 'created-at-utc))
            (cons 'prepared-env-mode (hash-ref-unknown h 'prepared-env-mode))
            (cons 'outcome outcome)
            (if fallback-cause
                (cons 'fallback-cause fallback-cause)
                (cons 'record-source (hash-ref-unknown h 'record-source)))
            (cons 'restore-ms (duration-or-unknown (hash-ref-unknown h 'restore-ms)))
            (cons 'fallback-ms (duration-or-unknown (hash-ref-unknown h 'fallback-ms)))
            (cons 'wall-clock-seconds (duration-or-unknown (hash-ref-unknown h 'wall-clock-seconds)))
            (cons 'cache-key
                  ;; Emitted records already carry the composed cache key; only
                  ;; recompose when the input carries the raw components instead.
                  (let ([k (hash-ref h 'cache-key #f)])
                    (if k
                        k
                        (string-append (hash-ref-unknown h 'prepared-artifact-name)
                                       ":"
                                       (hash-ref-unknown h 'installer-sha256)))))
            (cons 'sha-context (hash-ref-unknown h 'head-sha))
            ;; The raw CI state is provenance: carry it through so aggregates stay
            ;; traceable to the exact telemetry the workflow emitted.
            (cons 'raw-state
                  (let ([rs (hash-ref h 'raw-state #f)])
                    (if rs
                        rs
                        (hash-ref-unknown h 'state))))))

(define (outcome-count records outcome)
  (for/sum ([rec (in-list records)]) (if (equal? (alist-ref rec 'outcome) outcome) 1 0)))

(define (fallback-cause-alist records)
  (define causes
    (remove-duplicates (for/list ([rec (in-list records)]
                                  #:when (member (alist-ref rec 'outcome)
                                                 (list "rebuilt" "fallback")))
                         (alist-ref rec 'fallback-cause))))
  (for/list ([cause (in-list (sort causes string<?))])
    (cons cause
          (for/sum ([rec (in-list records)])
                   (if (equal? (alist-ref rec 'fallback-cause) cause) 1 0)))))

(define (gate-verdict v r f)
  (define denom (+ v r f))
  (cond
    [(zero? denom) "insufficient-data"]
    [(>= (* 100.0 (/ v denom)) 95.0) "pass"]
    [else "fallback-causes-named"]))

(define (do-aggregate args)
  (define dir (assoc-value args "--aggregate"))
  (define out (assoc-value args "--out"))
  (unless (and (string? dir) (string? out))
    (exit (usage-fail "--aggregate requires --aggregate DIR and --out F")))
  (define prefix (or (assoc-value args "--filter-prefix") ""))
  (define campaign (or (assoc-value args "--campaign") "v1.00.25"))
  ;; Optional honest provenance override for the window basis: when the
  ;; committed window is reconstructed from real run logs (jobs API step
  ;; wall-clocks) instead of wiring-emitted records, the report must say
  ;; exactly that instead of claiming the wiring emitted it.
  ;; `--basis` is free prose that may contain spaces: consume every token up
  ;; to the next `--flag`, not just the immediately following one.
  (define basis-override
    (let ([tail (member "--basis" args)])
      (let gather ([xs (if tail
                           (cdr tail)
                           '())]
                   [acc '()])
        (cond
          [(null? xs)
           (if (null? acc)
               #f
               (string-join (reverse acc) " "))]
          [(string-prefix? (car xs) "--")
           (if (null? acc)
               #f
               (string-join (reverse acc) " "))]
          [else (gather (cdr xs) (cons (car xs) acc))]))))
  (define files
    (for/list ([p (in-list (directory-list dir))]
               #:when (and (file-exists? (build-path dir p))
                           (string-prefix? (path->string p) prefix)
                           (string-suffix? (path->string p) ".json")))
      (build-path dir p)))
  (define raw-records
    (for/list ([f (in-list files)])
      (with-handlers ([exn:fail? (lambda (_) #f)])
        (with-input-from-file f read-json))))
  (define records
    (sort (for/list ([h (in-list raw-records)]
                     #:when (hash? h))
            (normalize-input-record h))
          key<?
          #:key record-sort-key))
  (define verified (outcome-count records "verified"))
  (define rebuilt (outcome-count records "rebuilt"))
  (define fallback (outcome-count records "fallback"))
  (define unknown (outcome-count records "unknown"))
  (define denom (+ verified rebuilt fallback))
  (define rate
    (if (zero? denom)
        "unknown"
        (* 1.0 (/ verified denom))))
  (define timestamps
    (for/list ([rec (in-list records)]
               #:when (string? (alist-ref rec 'created-at-utc))
               #:unless (equal? (alist-ref rec 'created-at-utc) "unknown"))
      (alist-ref rec 'created-at-utc)))
  ;; ISO-8601 UTC stamps sort lexicographically; min/max via string order.
  (define window-start
    (if (null? timestamps)
        "unknown"
        (car (sort timestamps string<?))))
  (define window-end
    (if (null? timestamps)
        "unknown"
        (car (reverse (sort timestamps string<?)))))
  ;; Fresh setup+execution measurements, grouped per run. The critical
  ;; path is the slowest shard; its wall clock is the execution component
  ;; and its restore_ms is the setup component. Values the telemetry does
  ;; not carry stay "unknown" — never zero, never historical numbers.
  (define runs
    (remove-duplicates (for/list ([rec (in-list records)])
                         (alist-ref rec 'run-id))))
  (define samples
    (for/list ([rid (in-list (sort runs (lambda (a b) (< (if (real? a) a 0) (if (real? b) b 0)))))])
      (define run-recs
        (for/list ([rec (in-list records)]
                   #:when (equal? (alist-ref rec 'run-id) rid))
          rec))
      (define wall-recs
        (for/list ([rec (in-list run-recs)]
                   #:when (real? (alist-ref rec 'wall-clock-seconds)))
          rec))
      (define critical
        (if (null? wall-recs)
            #f
            (argmax (lambda (rec) (alist-ref rec 'wall-clock-seconds)) wall-recs)))
      (define wall
        (if critical
            (alist-ref critical 'wall-clock-seconds)
            "unknown"))
      (define restore-ms
        (if critical
            (alist-ref critical 'restore-ms)
            "unknown"))
      (define setup-plus-exec
        (if (and critical (real? wall) (real? restore-ms))
            (* 1.0 (+ wall (/ restore-ms 1000)))
            "unknown"))
      (json-obj (cons 'run-id rid)
                (cons 'head-sha
                      (if critical
                          (alist-ref critical 'head-sha)
                          "unknown"))
                (cons 'slowest-shard-execution-seconds wall)
                (cons 'critical-path-restore-ms restore-ms)
                (cons 'setup-plus-execution-seconds setup-plus-exec))))
  (define report
    (json-obj
     (cons 'schema-version 1)
     (cons 'kind "prepared-env-restore-report")
     (cons 'campaign campaign)
     (cons 'report-id (string-append campaign "-prepared-env"))
     (cons
      'window
      (json-obj
       (cons 'start-utc window-start)
       (cons 'end-utc window-end)
       (cons 'record-count (length records))
       (cons
        'basis
        (or
         basis-override
         "machine-readable prepared-env-restore records emitted by the W5 ci.yml wiring from \
         real CI runs on the activated defaults (test-results-fast-* artifacts)"))))
     (cons 'restores (apply json-arr records))
     (cons 'observation
           (json-obj (cons 'counts
                           (json-obj (cons 'verified verified)
                                     (cons 'rebuilt rebuilt)
                                     (cons 'fallback fallback)
                                     (cons 'unknown unknown)))
                     (cons 'rate-denominator denom)
                     (cons 'verified-restore-rate rate)
                     (cons 'gate
                           (json-obj (cons 'threshold-percent 95)
                                     (cons 'verdict (gate-verdict verified rebuilt fallback))
                                     (cons 'fallback-causes
                                           (apply json-obj (fallback-cause-alist records)))))))
     (cons
      'fresh-measurements
      (json-obj
       (cons 'label "fresh measurements")
       (cons
        'historical-note
        "The v1.00.11 baseline 488.0 s and the v1.00.16 setup+execution 627.0 s are recorded history; \
        this report contains only fresh measurements taken on the current tree with the same commands and profiles. \
        Historical numbers are referenced as history and are never presented as current values.")
       (cons
        'recipe
        "Per run: critical path = slowest test-results-fast shard wall_clock_seconds \
        (suite fast, 3 shards x 4 workers, FAST_SHARD_COUNT=3); setup component = that shard's prepared_environment restore_ms; \
        both are reported and their sum as setup-plus-execution-seconds. Missing telemetry stays unknown.")
       (cons 'samples (apply json-arr samples))))))
  (write-json-file out report)
  ;; Bind the aggregate report to a SHA256SUMS file so the durable
  ;; artifact round-trips through --check (checksum-bound reporting).
  (define out-dir (path-only out))
  (when out-dir
    (with-output-to-file
     (build-path out-dir "SHA256SUMS")
     (lambda ()
       (display
        (string-append (sha256-hex-of-file out) "  " (path->string (file-name-from-path out)) "\n")))
     #:exists 'replace))
  (printf "prepared-env-report: aggregated ~a restore records into ~a (verified: ~a/~a)\n"
          (length records)
          out
          verified
          denom)
  0)

;; ---------------------------------------------------------------------------
;; Mode: --manifest F --check [--write-checksums]
;; ---------------------------------------------------------------------------

(define (positive-real-or-unknown? v)
  (or (and (real? v) (positive? v)) (equal? v "unknown")))

(define (non-empty-string? v)
  (and (string? v) (positive? (string-length v))))

(define (sha256-hex-of-file path)
  (hex-encode (sha256 (file->bytes path))))

(define (do-check args)
  (define manifest (assoc-value args "--manifest"))
  (unless (string? manifest)
    (exit (usage-fail "--check requires --manifest F")))
  (define write-checksums? (has-flag? args "--write-checksums"))
  (define violations '())
  (define (violate fmt . vs)
    (set! violations (cons (apply format fmt vs) violations)))
  (define report
    (with-handlers ([exn:fail? (lambda (e)
                                 (violate "manifest is not parseable JSON: ~a" (exn-message e))
                                 #f)])
      (with-input-from-file manifest read-json)))
  (when (hash? report)
    (unless (equal? (hash-ref report 'schema-version #f) 1)
      (violate "schema-version must be 1"))
    (unless (equal? (hash-ref report 'kind #f) "prepared-env-restore-report")
      (violate "kind must be prepared-env-restore-report"))
    (define restores (hash-ref report 'restores #f))
    (unless (list? restores)
      (violate "restores must be a list"))
    (when (list? restores)
      (for ([r (in-list restores)]
            [i (in-naturals)])
        (unless (hash? r)
          (violate "restore ~a: not an object" i))
        (when (hash? r)
          (define outcome (hash-ref r 'outcome #f))
          (unless (member outcome (list "verified" "rebuilt" "fallback" "unknown"))
            (violate "restore ~a: outcome ~s is outside verified|rebuilt|fallback|unknown" i outcome))
          (define cause (hash-ref r 'fallback-cause #f))
          (cond
            [(equal? outcome "verified")
             (when cause
               (violate "restore ~a: verified records must not carry a fallback cause" i))]
            [(member outcome (list "rebuilt" "fallback"))
             (unless (non-empty-string? cause)
               (violate "restore ~a: outcome ~a must name its fallback cause" i outcome))]
            [else (void)])
          (for ([field (in-list (list 'restore-ms 'fallback-ms 'wall-clock-seconds))])
            (define v (hash-ref r field #f))
            (unless (positive-real-or-unknown? v)
              (violate
               "restore ~a: ~a must be a positive number or the string \"unknown\", never zero or null (got ~s)"
               i
               field
               v)))
          (unless (non-empty-string? (hash-ref r 'sha-context #f))
            (violate "restore ~a: sha-context must be a non-empty string" i))
          (unless (non-empty-string? (hash-ref r 'cache-key #f))
            (violate "restore ~a: cache-key must be a non-empty string" i)))))
    (define recs
      (if (list? restores)
          restores
          '()))
    (define verified
      (for/sum ([r (in-list recs)] #:when (and (hash? r)
                                               (equal? (hash-ref r 'outcome #f) "verified")))
               1))
    (define rebuilt
      (for/sum ([r (in-list recs)] #:when (and (hash? r) (equal? (hash-ref r 'outcome #f) "rebuilt")))
               1))
    (define fallback
      (for/sum ([r (in-list recs)] #:when (and (hash? r)
                                               (equal? (hash-ref r 'outcome #f) "fallback")))
               1))
    (define unknown
      (for/sum ([r (in-list recs)] #:when (and (hash? r) (equal? (hash-ref r 'outcome #f) "unknown")))
               1))
    (define denom (+ verified rebuilt fallback))
    (define obs (hash-ref report 'observation #f))
    (if (hash? obs)
        (let ()
          (define counts (hash-ref obs 'counts #f))
          (unless (hash? counts)
            (violate "observation.counts must be an object"))
          (when (hash? counts)
            (for ([outcome (in-list (list 'verified 'rebuilt 'fallback 'unknown))]
                  [tally (in-list (list verified rebuilt fallback unknown))])
              (unless (equal? (hash-ref counts outcome -1) tally)
                (violate "observation.counts.~a says ~s but the records say ~a"
                         outcome
                         (hash-ref counts outcome -1)
                         tally))))
          (unless (equal? (hash-ref obs 'rate-denominator #f) denom)
            (violate "rate-denominator says ~s but the records imply ~a"
                     (hash-ref obs 'rate-denominator #f)
                     denom))
          (define stated-rate (hash-ref obs 'verified-restore-rate #f))
          (cond
            [(zero? denom)
             (unless (equal? stated-rate "unknown")
               (violate
                "verified-restore-rate must be \"unknown\" when there is no rate denominator (got ~s)"
                stated-rate))]
            [(not (real? stated-rate))
             (violate "verified-restore-rate must be a number (got ~s)" stated-rate)]
            [(> (abs (- stated-rate (* 1.0 (/ verified denom)))) 1e-9)
             (violate "verified-restore-rate ~s is inconsistent with ~a/~a"
                      stated-rate
                      verified
                      denom)])
          (define gate (hash-ref obs 'gate #f))
          (if (hash? gate)
              (let ()
                (unless (equal? (hash-ref gate 'threshold-percent #f) 95)
                  (violate "gate.threshold-percent must be 95"))
                (define expected-verdict (gate-verdict verified rebuilt fallback))
                (unless (equal? (hash-ref gate 'verdict #f) expected-verdict)
                  (violate "gate.verdict says ~s but the arithmetic says ~s"
                           (hash-ref gate 'verdict #f)
                           expected-verdict))
                (define expected-causes
                  (for/fold ([acc (hash)])
                            ([r (in-list recs)]
                             #:when (and (hash? r)
                                         (member (hash-ref r 'outcome #f)
                                                 (list "rebuilt" "fallback"))))
                    (hash-update acc (string->symbol (hash-ref r 'fallback-cause "unknown")) add1 0)))
                (define stated-causes (hash-ref gate 'fallback-causes #f))
                (unless (hash? stated-causes)
                  (violate "gate.fallback-causes must be an object"))
                (when (hash? stated-causes)
                  (for ([k (in-list (hash-keys stated-causes))])
                    (unless (equal? (hash-ref stated-causes k -1) (hash-ref expected-causes k 0))
                      (violate "gate.fallback-causes[~s] disagrees with the records" k)))
                  (for ([k (in-list (hash-keys expected-causes))])
                    (unless (hash-has-key? stated-causes k)
                      (violate "gate.fallback-causes is missing named cause ~s" k)))))
              (violate "observation.gate must be an object"))
          (define fresh (hash-ref report 'fresh-measurements #f))
          (unless (and (hash? fresh)
                       (equal? (hash-ref fresh 'label #f) "fresh measurements")
                       (list? (hash-ref fresh 'samples #f))
                       (non-empty-string? (hash-ref fresh 'historical-note #f)))
            (violate
             "fresh-measurements must label samples as fresh measurements with a historical note")))
        (violate "observation must be an object")))
  ;; SHA256SUMS binding: write first when requested, then validate.
  (define sums-path (build-path (path-only manifest) "SHA256SUMS"))
  (when write-checksums?
    (with-output-to-file sums-path
                         (lambda ()
                           (display (string-append (sha256-hex-of-file manifest)
                                                   "  "
                                                   (path->string (file-name-from-path manifest))
                                                   "\n")))
                         #:exists 'replace))
  (unless (file-exists? sums-path)
    (violate "SHA256SUMS is missing next to the manifest; bind it with --write-checksums"))
  (when (file-exists? sums-path)
    (define actual (sha256-hex-of-file manifest))
    (define found
      (for/first ([line (in-list (file->lines sums-path))]
                  #:when (string? line)
                  [parts (in-value (string-split line))]
                  #:when (and (>= (length parts) 2)
                              (equal? (second parts) (path->string (file-name-from-path manifest)))))
        (first parts)))
    (unless (and (string? found) (equal? found actual))
      (violate "SHA256SUMS does not match the manifest bytes (expected ~a)" actual)))
  (cond
    [(null? violations)
     (printf "prepared-env-report: PASS ~a\n" manifest)
     0]
    [else
     (fprintf (current-error-port) "prepared-env-report: FAILED ~a\n" manifest)
     (for ([v (in-list (reverse violations))])
       (fprintf (current-error-port) "  - ~a\n" v))
     1]))

;; ---------------------------------------------------------------------------
;; W6 modes: the prepared-environment IDENTITY MANIFEST (spec §6 W4 work
;; item 1: immutable identity = OS image, architecture, Racket version +
;; executable digest, package lock/resolved set, precompile recipe
;; revision, relevant policy, artifact digest).
;;
;;   --identity-emit          write one identity manifest (save side and
;;                            expected-side derivation share this mode)
;;   --identity-compare       compare expected vs observed; ANY mismatch is
;;                            a loud, counted cold fallback (never silent)
;;   --identity-fallback-record  emit the counted fallback record
;;   --consumers-check        fail-closed validation of consumers.json
;;   --savings-check          fail-closed validation of setup-savings.json
;;   --rollback-drill         evaluate one consumer's one-command rollback
;;
;; Honesty rules: digest fields must be well-formed 64-hex SHA-256 values —
;; a missing or malformed digest is a hard usage failure, never an invented
;; value; distinct identity dimensions always derive distinct artifact
;; identities, so a distinct environment can never cross-reuse an artifact
;; by name; and the compare fails closed on missing fields.
;; ---------------------------------------------------------------------------

(define identity-dimensions
  (list "os"
        "os-image"
        "arch"
        "racket-version"
        "racket-executable-digest"
        "lock-digest"
        "resolved-set-digest"
        "precompile-recipe-revision"
        "policy-fingerprint"))

(define identity-digest-dimensions
  (list "racket-executable-digest" "lock-digest" "resolved-set-digest" "artifact-digest"))

(define (hex-digest? v)
  (and (string? v) (regexp-match? #px"^[0-9a-f]{64}$" v)))

(define (sha256-hex-of-string s)
  (hex-encode (sha256 (string->bytes/utf-8 s))))

;; Canonical identity string: the nine dimensions in their fixed order as
;; k=v lines. The artifact identity is derived from this form, so ANY
;; dimension difference (including one) yields a different artifact
;; identity — distinct environments cannot share an artifact identity.
(define (identity-canonical fields)
  (string-join (for/list ([dim (in-list identity-dimensions)])
                 (format "~a=~a" dim (hash-ref fields (string->symbol dim))))
               "\n"))

(define (identity-artifact-name base fields)
  (format "~a-~a" base (substring (sha256-hex-of-string (identity-canonical fields)) 0 16)))

(define (identity-manifest-json artifact-name-base fields)
  (json-obj (cons 'schema "prepared-env-identity@1")
            (cons 'artifact-name-base artifact-name-base)
            (cons 'artifact-identity (identity-artifact-name artifact-name-base fields))
            (cons 'fields
                  (apply json-obj
                         (for/list ([dim (in-list (append identity-dimensions
                                                          (list "artifact-digest")))])
                           (cons (string->symbol dim) (hash-ref fields (string->symbol dim))))))))

(define (do-identity-emit args)
  (define out (assoc-value args "--out"))
  (unless (string? out)
    (exit (usage-fail "--identity-emit requires --out F")))
  (define artifact-base (or (assoc-value args "--artifact-name-base") "prepared-env"))
  (define fields (make-hash))
  (for ([dim (in-list identity-dimensions)])
    (define flag (string-append "--" dim))
    (define raw (assoc-value args flag))
    (unless (string? raw)
      (exit (usage-fail (string-append "--identity-emit requires " flag))))
    (hash-set! fields (string->symbol dim) raw))
  ;; The artifact digest is a manifest dimension but not part of the
  ;; environment profile: two environments are distinct iff one of the nine
  ;; profile dimensions differs. The digest must still be well-formed when
  ;; provided — fail closed instead of inventing one.
  (define artifact-digest (assoc-value args "--artifact-digest"))
  (unless (and (string? artifact-digest) (hex-digest? artifact-digest))
    (exit (usage-fail "--identity-emit requires a well-formed 64-hex --artifact-digest")))
  (for ([dim (in-list identity-digest-dimensions)])
    (unless (or (equal? dim "artifact-digest") (hex-digest? (hash-ref fields (string->symbol dim))))
      (exit (usage-fail (format "--identity-emit: ~a must be a well-formed 64-hex digest" dim)))))
  (for ([dim (in-list
              (list "os" "arch" "racket-version" "precompile-recipe-revision" "policy-fingerprint"))])
    (unless (positive? (string-length (hash-ref fields (string->symbol dim))))
      (exit (usage-fail (format "--identity-emit: ~a must be a non-empty string" dim)))))
  (hash-set! fields 'artifact-digest artifact-digest)
  (write-json-file out (identity-manifest-json artifact-base fields))
  (printf "prepared-env-report: identity manifest written to ~a (artifact identity: ~a)\n"
          out
          (identity-artifact-name artifact-base fields))
  0)

(define (read-identity-manifest! path)
  (define doc
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (exit (die-usage
                              (format "--identity-compare: cannot read identity manifest ~a: ~a"
                                      path
                                      (exn-message e)))))])
      (with-input-from-file path read-json)))
  (unless (and (hash? doc)
               (equal? (hash-ref doc 'schema #f) "prepared-env-identity@1")
               (hash? (hash-ref doc 'fields #f)))
    (exit (die-usage (format "--identity-compare: ~a is not a prepared-env-identity@1 manifest"
                             path))))
  (define fields (hash-ref doc 'fields))
  (for ([dim (in-list identity-dimensions)])
    (unless (hash-has-key? fields (string->symbol dim))
      ;; Fail closed: a manifest that does not fully describe the identity
      ;; is never silently accepted.
      (exit (die-usage (format "--identity-compare: manifest ~a is missing dimension ~a" path dim)))))
  fields)

(define (die-usage message)
  (fprintf (current-error-port) "prepared-env-report: ~a\n" message)
  2)

(define (do-identity-compare args)
  (define expected-path (assoc-value args "--expected"))
  (define observed-path (assoc-value args "--observed"))
  (define out (assoc-value args "--out"))
  (define consumer (or (assoc-value args "--consumer") "unknown"))
  (unless (and (string? expected-path) (string? observed-path))
    (exit (usage-fail "--identity-compare requires --expected F and --observed F")))
  (define expected (read-identity-manifest! expected-path))
  (define observed (read-identity-manifest! observed-path))
  (define mismatches
    (for/list ([dim (in-list identity-dimensions)]
               #:when (not (equal? (hash-ref expected (string->symbol dim))
                                   (hash-ref observed (string->symbol dim)))))
      (json-obj (cons 'field dim)
                (cons 'expected (hash-ref expected (string->symbol dim)))
                (cons 'observed (hash-ref observed (string->symbol dim))))))
  (define artifact-name-expected (identity-artifact-name "prepared-env" expected))
  (define artifact-name-observed (identity-artifact-name "prepared-env" observed))
  (define mismatched?
    (or (pair? mismatches) (not (equal? artifact-name-expected artifact-name-observed))))
  (define warning
    (format
     "::warning::prepared-environment identity mismatch for ~a: ~a; falling back to the full cold setup path (counted, never silent)"
     consumer
     (if (null? mismatches)
         (format "artifact identity ~a != ~a" artifact-name-expected artifact-name-observed)
         (string-join (for/list ([m (in-list mismatches)])
                        (format "~a: expected ~s, observed ~s"
                                (cdr (assoc 'field (cdr m)))
                                (cdr (assoc 'expected (cdr m)))
                                (cdr (assoc 'observed (cdr m)))))
                      "; "))))
  (define verdict
    (json-obj (cons 'schema "prepared-env-identity-verdict@1")
              (cons 'consumer consumer)
              (cons 'expected-artifact-identity artifact-name-expected)
              (cons 'observed-artifact-identity artifact-name-observed)
              (cons 'verdict (if mismatched? "mismatch" "verified"))
              (cons 'mismatch-count (length mismatches))
              (cons 'mismatches (apply json-arr mismatches))
              (cons 'fallback
                    (json-obj (cons 'required mismatched?)
                              (cons 'kind "cold-full-path")
                              (cons 'loud #t)
                              (cons 'counted #t)
                              (cons 'warning warning)
                              (cons 'stamp "prepared-env-identity-fallback-stamp.json")))))
  (when out
    (write-json-file out verdict))
  (when mismatched?
    (displayln warning))
  (printf "prepared-env-report: identity compare for ~a: ~a (~a mismatching dimension(s))\n"
          consumer
          (if mismatched? "MISMATCH" "verified")
          (length mismatches))
  (if mismatched? 1 0))

(define (do-identity-fallback-record args)
  (define out (assoc-value args "--out"))
  (unless (string? out)
    (exit (usage-fail "--identity-fallback-record requires --out F")))
  (define consumer (or (assoc-value args "--consumer") "unknown"))
  (define reason (assoc-value args "--reason"))
  (unless (and (string? reason) (positive? (string-length reason)))
    (exit (usage-fail
           "--identity-fallback-record requires a non-empty --reason (never an unnamed fallback)")))
  (define manifest (assoc-value args "--identity-manifest"))
  (define observed-identity
    (if manifest
        (let ([fields (read-identity-manifest! manifest)])
          (identity-artifact-name "prepared-env" fields))
        "unknown"))
  (write-json-file out
                   (json-obj (cons 'schema "prepared-env-fallback-record@1")
                             (cons 'consumer consumer)
                             (cons 'fallback #t)
                             (cons 'counted #t)
                             (cons 'loud #t)
                             (cons 'reason reason)
                             (cons 'observed-artifact-identity observed-identity)
                             (cons 'created-at-utc
                                   (or (assoc-value args "--created-at-utc") "unknown"))))
  (printf "prepared-env-report: counted fallback record written to ~a (consumer ~a, reason ~a)\n"
          out
          consumer
          reason)
  0)

;; ---------------------------------------------------------------------------
;; W6 modes: consumer matrix + savings honesty checks + rollback drill
;; ---------------------------------------------------------------------------

(define (consumers-violations doc)
  (define violations '())
  (define (violate fmt . vs)
    (set! violations (cons (apply format fmt vs) violations)))
  (cond
    [(not (hash? doc))
     (violate "consumers document is not a JSON object")
     violations]
    [else
     (unless (equal? (hash-ref doc 'schema #f) "prepared-env-consumers@1")
       (violate "schema must be prepared-env-consumers@1"))
     (unless (non-empty-string? (hash-ref doc 'wave #f))
       (violate "wave must be a non-empty string"))
     (define dims (hash-ref doc 'identity-dimensions #f))
     (unless (equal? dims identity-dimensions)
       (violate "identity-dimensions must be exactly the nine canonical dimensions in order"))
     (define producer (hash-ref doc 'producer #f))
     (unless (and (hash? producer)
                  (hash? (hash-ref producer 'env-profile #f))
                  (non-empty-string? (hash-ref producer 'artifact-name #f)))
       (violate "producer must carry an env-profile object and a non-empty artifact-name"))
     (define producer-profile
       (if (and producer (hash? producer))
           (hash-ref producer 'env-profile (hash))
           (hash)))
     (define consumers (hash-ref doc 'consumers #f))
     (unless (and (list? consumers) (pair? consumers))
       (violate "consumers must be a non-empty list"))
     (define seen (hash))
     (when (list? consumers)
       (for ([c (in-list consumers)]
             [i (in-naturals)])
         (cond
           [(not (hash? c)) (violate "consumer ~a: not an object" i)]
           [else
            (define name (hash-ref c 'consumer #f))
            (unless (non-empty-string? name)
              (violate "consumer ~a: missing consumer name" i))
            (when (and (string? name) (hash-has-key? seen name))
              (violate "consumer ~a: duplicate consumer entry" name))
            (when (string? name)
              (set! seen (hash-set seen name #t)))
            (unless (non-empty-string? (hash-ref c 'workflow #f))
              (violate "consumer ~a: missing workflow" name))
            (define profile (hash-ref c 'env-profile #f))
            (unless (and (hash? profile)
                         (for/and ([k (in-list (list 'os 'arch 'racket-version 'policy))])
                           (non-empty-string? (hash-ref profile k #f))))
              (violate "consumer ~a: env-profile must carry non-empty os/arch/racket-version/policy"
                       name))
            (define activation (hash-ref c 'activation #f))
            (unless (member activation (list "activated" "deferred"))
              (violate "consumer ~a: activation must be activated or deferred" name))
            (unless (non-empty-string? (hash-ref c 'reason #f))
              (violate "consumer ~a: every activation decision names its reason" name))
            (define rollback (hash-ref c 'rollback #f))
            (unless (and (string? rollback)
                         (positive? (string-length rollback))
                         (not (string-contains? rollback "\n")))
              (violate "consumer ~a: rollback must be a one-command (single-line) string" name))
            ;; The core no-cross-reuse invariant: only a PROVABLY identical
            ;; env-profile may be activated, and a distinct profile must
            ;; carry its own artifact identity (never the producer's).
            (when (equal? activation "activated")
              (unless (and (hash? profile) (equal? profile producer-profile))
                (violate
                 "consumer ~a: activated with an env-profile distinct from the producer — distinct Racket/platform/policy environments must stay deferred or separate"
                 name))
              (unless (equal? (hash-ref c 'artifact-identity #f)
                              (hash-ref producer 'artifact-identity #f))
                (violate
                 "consumer ~a: activated consumer must reference the producer artifact identity"
                 name)))
            (when (and (hash? profile)
                       (not (equal? profile producer-profile))
                       (equal? (hash-ref c 'artifact-identity #f)
                               (hash-ref producer 'artifact-identity #f)))
              (violate
               "consumer ~a: distinct env-profile shares the producer artifact identity — cross-environment reuse is prohibited"
               name))]))
       (define counts (hash-ref doc 'counts #f))
       (unless (and (hash? counts)
                    (equal? (hash-ref counts 'activated #f)
                            (for/sum ([c
                                       (in-list (if (list? consumers)
                                                    consumers
                                                    (list)))]
                                      #:when (equal? (hash-ref c 'activation #f) "activated"))
                                     1))
                    (equal? (hash-ref counts 'deferred #f)
                            (for/sum ([c
                                       (in-list (if (list? consumers)
                                                    consumers
                                                    (list)))]
                                      #:when (equal? (hash-ref c 'activation #f) "deferred"))
                                     1)))
         (violate "counts must state the exact activated/deferred tallies")))
     (reverse violations)]))

(define (do-consumers-check args)
  (define path (assoc-value args "--consumers-check"))
  (unless (string? path)
    (exit (usage-fail "--consumers-check requires a consumers.json path")))
  (define doc
    (with-handlers ([exn:fail? (lambda (e)
                                 (fprintf (current-error-port)
                                          "prepared-env-report: FAILED ~a (not parseable JSON: ~a)\n"
                                          path
                                          (exn-message e))
                                 (exit 1))])
      (with-input-from-file path read-json)))
  (define violations (consumers-violations doc))
  (cond
    [(null? violations)
     (printf "prepared-env-report: PASS consumers matrix ~a\n" path)
     0]
    [else
     (fprintf (current-error-port) "prepared-env-report: FAILED consumers matrix ~a\n" path)
     (for ([v (in-list violations)])
       (fprintf (current-error-port) "  - ~a\n" v))
     1]))

(define (savings-violations doc)
  (define violations '())
  (define (violate fmt . vs)
    (set! violations (cons (apply format fmt vs) violations)))
  (cond
    [(not (hash? doc))
     (violate "savings document is not a JSON object")
     violations]
    [else
     (unless (equal? (hash-ref doc 'schema #f) "prepared-env-setup-savings@1")
       (violate "schema must be prepared-env-setup-savings@1"))
     (unless (non-empty-string? (hash-ref doc 'baseline-source #f))
       (violate "baseline-source must name the retained baseline"))
     (define rows (hash-ref doc 'consumers #f))
     (unless (and (list? rows) (pair? rows))
       (violate "consumers must be a non-empty list of per-consumer rows"))
     (when (list? rows)
       (for ([row (in-list rows)]
             [i (in-naturals)])
         (cond
           [(not (hash? row)) (violate "savings row ~a: not an object" i)]
           [else
            (define name (hash-ref row 'consumer #f))
            (unless (non-empty-string? name)
              (violate "savings row ~a: missing consumer" i))
            (define measurement (hash-ref row 'measurement #f))
            (unless (member measurement (list "measured" "projected-from-w0-baseline"))
              (violate
               "savings row ~a: measurement must be measured or projected-from-w0-baseline (never a fabricated number)"
               name))
            (cond
              [(equal? measurement "measured")
               (unless (non-empty-string? (hash-ref row 'evidence #f))
                 (violate "savings row ~a: measured rows must cite their retained evidence" name))
               (unless (real? (hash-ref row 'saved-runner-minutes #f))
                 (violate "savings row ~a: measured rows must carry a numeric saved-runner-minutes"
                          name))]
              [else
               (unless (non-empty-string? (hash-ref row 'formula #f))
                 (violate "savings row ~a: projected rows must state their formula" name))
               (define saved (hash-ref row 'saved-runner-minutes #f))
               (unless (or (real? saved) (equal? saved "unknown"))
                 (violate "savings row ~a: projected saved-runner-minutes must be numeric or unknown"
                          name))])
            (define baseline (hash-ref row 'w0-baseline-setup-seconds #f))
            (unless (or (real? baseline) (equal? baseline "unknown"))
              (violate "savings row ~a: w0-baseline-setup-seconds must be numeric or unknown"
                       name))]))
       ;; Every activated consumer needs a row; a row for a non-activated
       ;; consumer is drift.
       (define consumers-path (hash-ref doc 'consumers-source #f))
       (when (and (string? consumers-path) (file-exists? consumers-path))
         (define cdoc
           (with-handlers ([exn:fail? (lambda (_) #f)])
             (with-input-from-file consumers-path read-json)))
         (when (hash? cdoc)
           (define activated
             (for/list ([c (in-list (hash-ref cdoc 'consumers (list)))]
                        #:when (equal? (hash-ref c 'activation #f) "activated"))
               (hash-ref c 'consumer)))
           (define row-names
             (for/list ([r (in-list (if (list? rows)
                                        rows
                                        (list)))]
                        #:when (string? (hash-ref r 'consumer #f)))
               (hash-ref r 'consumer)))
           (for ([a (in-list activated)]
                 #:unless (member a row-names))
             (violate "activated consumer ~a has no savings row" a))
           (for ([r (in-list row-names)]
                 #:unless (member r activated))
             (violate "savings row ~a does not correspond to an activated consumer" r)))))
     (reverse violations)]))

(define (do-savings-check args)
  (define path (assoc-value args "--savings-check"))
  (unless (string? path)
    (exit (usage-fail "--savings-check requires a setup-savings.json path")))
  (define doc
    (with-handlers ([exn:fail? (lambda (e)
                                 (fprintf (current-error-port)
                                          "prepared-env-report: FAILED ~a (not parseable JSON: ~a)\n"
                                          path
                                          (exn-message e))
                                 (exit 1))])
      (with-input-from-file path read-json)))
  (define violations (savings-violations doc))
  (cond
    [(null? violations)
     (printf "prepared-env-report: PASS setup-savings ~a\n" path)
     0]
    [else
     (fprintf (current-error-port) "prepared-env-report: FAILED setup-savings ~a\n" path)
     (for ([v (in-list violations)])
       (fprintf (current-error-port) "  - ~a\n" v))
     1]))

;; Evaluate one consumer's prepared-path decision exactly as the ci.yml
;; PREPARED_ENV expression evaluates it. This is the exercisable form of
;; the §11.6 one-command rollback: setting the consumer's repository
;; variable to off must pin that consumer to the legacy full path while
;; the artifact producer stays available for everyone else.
(define (do-rollback-drill args)
  (define consumers-path (assoc-value args "--consumers"))
  (define name (assoc-value args "--consumer"))
  (unless (and (string? consumers-path) (string? name))
    (exit (usage-fail "--rollback-drill requires --consumers F and --consumer NAME")))
  (define doc
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (with-input-from-file consumers-path read-json)))
  (unless (hash? doc)
    (exit (die-usage (format "--rollback-drill: cannot read consumers matrix ~a" consumers-path))))
  (define row
    (for/first ([c (in-list (hash-ref doc 'consumers (list)))]
                #:when (equal? (hash-ref c 'consumer #f) name))
      c))
  (unless (hash? row)
    (exit (die-usage (format "--rollback-drill: consumer ~a is not in the matrix" name))))
  (define rollback-var (hash-ref row 'rollback-variable #f))
  (unless (non-empty-string? rollback-var)
    (exit (die-usage (format "--rollback-drill: consumer ~a has no rollback-variable" name))))
  (define (var v)
    (define raw (assoc-value args "--vars"))
    (define pairs
      (if (string? raw)
          (for/list ([kv (in-list (string-split raw ","))]
                     #:when (string-contains? kv "="))
            (apply cons (string-split kv "=")))
          '()))
    (cdr (or (assoc v pairs) (cons v ""))))
  (define event (or (assoc-value args "--event") "push"))
  (define producer-result (or (assoc-value args "--producer-result") "success"))
  (define prepared?
    (and (not (equal? (var "RACKET_PREPARED_ARTIFACT") "off"))
         (not (equal? (var rollback-var) "off"))
         (not (equal? event "workflow_dispatch"))
         (equal? producer-result "success")))
  (define decision
    (json-obj (cons 'schema "prepared-env-rollback-drill@1")
              (cons 'consumer name)
              (cons 'rollback-variable rollback-var)
              (cons 'rollback-command (format "gh variable set ~a --body off" rollback-var))
              (cons 'event event)
              (cons 'producer-result producer-result)
              (cons 'prepared-path-in-play (if prepared? #t #f))
              (cons 'effective-setup-path
                    (if prepared? "verified prepared-env restore" "legacy full cold setup"))))
  (displayln (json-value->string decision))
  0)

;; ---------------------------------------------------------------------------
;; Entry point
;; ---------------------------------------------------------------------------

(define (dispatch args)
  (cond
    [(null? args) (usage-fail "no mode given")]
    [(has-flag? args "--emit-restore-record") (do-emit args)]
    [(has-flag? args "--aggregate") (do-aggregate args)]
    [(has-flag? args "--check") (do-check args)]
    [(has-flag? args "--identity-emit") (do-identity-emit args)]
    [(has-flag? args "--identity-compare") (do-identity-compare args)]
    [(has-flag? args "--identity-fallback-record") (do-identity-fallback-record args)]
    [(has-flag? args "--consumers-check") (do-consumers-check args)]
    [(has-flag? args "--savings-check") (do-savings-check args)]
    [(has-flag? args "--rollback-drill") (do-rollback-drill args)]
    [else (usage-fail "unknown arguments")]))

(module+ main
  (exit (dispatch (vector->list (current-command-line-arguments)))))
