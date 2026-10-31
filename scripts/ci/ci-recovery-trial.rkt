#lang racket/base

;; scripts/ci/ci-recovery-trial.rkt — v1.00.32 W2 workflow evidence adapter
;;
;; The thin, offline-testable binder behind the trusted CI-recovery
;; benchmark (docs/reports/CI-RECOVERY-EVIDENCE.md §2/§8; the workflow owns
;; the wiring, this script owns the data mapping — no second telemetry
;; framework, no dispatch logic, no assessment oracle of its own):
;;
;;   validate-dispatch --trial-id T --attempt N --treatment X --head-sha S
;;     Fail closed on an unauthorized or ill-formed trusted-dispatch trial
;;     identity: non-empty trial id, positive integer attempt, treatment
;;     exactly A|B|C, and an exact 40-character lowercase-hex head SHA.
;;
;;   bind-producer --stage DIR --out F
;;     Producer-side binding (runs in the fast-env job): SHA-256 of the
;;     staged prepared-env manifest.json and — when the compiled-root lane
;;     built it — of the staged q-compiled/trusted-root/manifest.rktd.
;;     Both digests are computed over REAL artifact bytes, never invented.
;;
;;   stamp-producer-manifest --shard N --out F --prepared-download DIR
;;                           [--compiled-root DIR]
;;     Consumer-side observed binding (runs in each test shard): hash the
;;     restored compiled-root manifest.rktd when present (basis
;;     "compiled-root-manifest"), else the restored prepared-env
;;     manifest.json (basis "prepared-env-manifest"). Writes NOTHING when
;;     nothing was restored — an unobserved binding is never invented.
;;
;;   collect --run-json F --jobs-json F --required-policy F --records-dir D
;;           --maps-dir D --producer-dir D --trial-id T --attempt N
;;           --treatment X --run-id N --run-attempt N --head-sha S
;;           --workflow-revision-sha S --shard-total N --created-at-utc T
;;           --out-dir D
;;     Assemble ONE per-trial evidence artifact directory for the report
;;     owner: the verbatim Actions run/jobs metadata, the required-check
;;     critical path (max required completion minus run start — jobs run
;;     in parallel and are never summed; queue time is separate), and the
;;     ci-recovery-trial-evidence@1 envelope with producer-derived expected
;;     context, verbatim restore records (stamped with the strict trial
;;     keys the concurrent record repair requires) and 1:1 launcher
;;     telemetry mapped from the compiled-root datum. The ready/refused
;;     verdict itself is NEVER claimed here — the W1 assessor
;;     (prepared-env-report.rkt --trial-assess) remains the only oracle.

(require json
         racket/file
         racket/format
         racket/list
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         (file "../run-tests/sha256.rkt"))

;; The identity manifest is emitted through the evidence owner's own CLI so
;; the prepared-env-identity@1 canonical shape (schema, artifact identity,
;; field order) has exactly one implementation.
(define-runtime-path prepared-env-report-script "prepared-env-report.rkt")

(define trial-envelope-schema "ci-recovery-trial-evidence@1")
(define producer-manifests-schema "ci-recovery-trial-producer-manifests@1")
(define producer-stamp-schema "ci-recovery-trial-producer-stamp@1")
(define critical-path-schema "ci-recovery-trial-critical-path@1")
(define actions-evidence-schema "ci-recovery-actions-evidence@1")
(define aggregate-attestation-schema "ci-recovery-aggregate-restore-attestation@1")
(define trial-treatments (list "A" "B" "C"))

(provide trial-envelope-schema
         producer-manifests-schema
         producer-stamp-schema
         critical-path-schema
         actions-evidence-schema
         aggregate-attestation-schema
         trial-treatments
         validate-dispatch
         bind-producer
         stamp-producer-manifest
         emit-identity
         aggregate-restore-state
         actions-evidence-document
         iso-utc->epoch
         epoch->utc-string
         collect-trial)

;; ------------------------------------------------------------------ util

(define (hex-digest? v)
  (and (string? v) (regexp-match? #px"^[0-9a-f]{64}$" v)))

(define (trial-head-sha? v)
  (and (string? v) (regexp-match? #px"^[0-9a-f]{40}$" v)))

(define (positive-integer-string? v)
  (and (string? v) (regexp-match? #px"^[1-9][0-9]*$" v)))

(define (sha256-hex-of-file path)
  (sha256-hex (file->bytes path)))

;; Canonical, byte-deterministic JSON: object keys sorted lexicographically,
;; so identical inputs produce byte-identical artifacts.
(define (json-string-encode s)
  (with-output-to-string (lambda () (write-json s))))

(define (write-canonical-json v out)
  (cond
    [(hash? v)
     (display "{" out)
     (define kvs
       (sort (hash-map v (lambda (k val) (cons (symbol->string k) val))) string<? #:key car))
     (for ([i (in-naturals)]
           [kv (in-list kvs)])
       (unless (zero? i)
         (display "," out))
       (display (json-string-encode (car kv)) out)
       (display ":" out)
       (write-canonical-json (cdr kv) out))
     (display "}" out)]
    [(list? v)
     (display "[" out)
     (for ([i (in-naturals)]
           [x (in-list v)])
       (unless (zero? i)
         (display "," out))
       (write-canonical-json x out))
     (display "]" out)]
    [(string? v) (display (json-string-encode v) out)]
    [(boolean? v) (display (if v "true" "false") out)]
    [(real? v) (display (with-output-to-string (lambda () (write-json v))) out)]
    ;; Raw API JSON null is retained honestly as JSON null — a cancelled or
    ;; not-yet-started job serializes verbatim, never as an exception and
    ;; never as a false timing.
    [(eq? v 'null) (display "null" out)]
    [else (error 'write-canonical-json "not a JSON value: ~s" v)]))

(define (write-json-file! path v)
  (make-directory* (path-only path))
  (call-with-output-file path
                         (lambda (o)
                           (write-canonical-json v o)
                           (newline o))
                         #:exists 'replace))

(define (read-json-input! path who)
  (with-handlers ([exn:fail? (lambda (e)
                               (error who "cannot read JSON input ~a: ~a" path (exn-message e)))])
    (with-input-from-file path read-json)))

(define (read-json-file-or-#f path)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (with-input-from-file path read-json)))

;; ISO-8601 UTC ("2026-10-30T10:04:00Z") → epoch seconds; #f when absent
;; or unparsable (callers keep it out of the arithmetic, never zero-fill).
;; STRICT calendar validation, consistent with the report owner's
;; parse-utc-seconds (scripts/run-tests/ci-recovery-benchmark-report.rkt):
;; the parse is accepted only when it round-trips through the civil-date
;; formatter, so a non-calendar date like 2026-02-30 is refused instead of
;; silently normalizing into a false timing.
(define (epoch->utc-string t)
  (define days (quotient t 86400))
  (define rem (modulo t 86400))
  (define-values (y mo d) (civil-from-days days))
  (format "~a-~a-~aT~a:~a:~aZ"
          (pad-4 y)
          (pad-2 mo)
          (pad-2 d)
          (pad-2 (quotient rem 3600))
          (pad-2 (quotient (modulo rem 3600) 60))
          (pad-2 (modulo rem 60))))

(define (pad-2 n)
  (define s (number->string n))
  (if (< (string-length s) 2)
      (string-append "0" s)
      s))

(define (pad-4 n)
  (define s (number->string n))
  (if (> (string-length s) 4)
      s
      (string-append (make-string (- 4 (string-length s)) #\0) s)))

(define (civil-from-days z)
  ;; Howard Hinnant's civil_from_days (proleptic Gregorian); the inverse of
  ;; the days-from-civil arithmetic below.
  (define z2 (+ z 719468))
  (define era (quotient z2 146097))
  (define doe (- z2 (* 146097 era)))
  (define yoe
    (quotient (- doe (quotient doe 1460) (- (quotient doe 36524)) (quotient doe 146096)) 365))
  (define y (+ yoe (* 400 era)))
  (define doy (- doe (* 365 yoe) (quotient yoe 4) (- (quotient yoe 100))))
  (define mp (quotient (+ (* 5 doy) 2) 153))
  (define d (+ 1 (- doy (quotient (+ (* 153 mp) 2) 5))))
  (define m (+ mp (if (< mp 10) 3 -9)))
  (values (+ y (if (<= m 2) 1 0)) m d))

(define (iso-utc->epoch v)
  (define m
    (and (string? v)
         (regexp-match #px"^([0-9]{4})-([0-9]{2})-([0-9]{2})T([0-9]{2}):([0-9]{2}):([0-9]{2})Z$" v)))
  (and m
       (let* ([y (string->number (list-ref m 1))]
              [mo (string->number (list-ref m 2))]
              [d (string->number (list-ref m 3))]
              [h (string->number (list-ref m 4))]
              [mi (string->number (list-ref m 5))]
              [s (string->number (list-ref m 6))]
              ;; days-from-civil (proleptic Gregorian), Howard Hinnant
              [y+ (if (<= mo 2)
                      (- y 1)
                      y)]
              [era (quotient (if (>= y+ 0)
                                 y+
                                 (- y+ 399))
                             400)]
              [yoe (- y+ (* 400 era))]
              [mp (+ mo (if (> mo 2) -3 9))]
              [doy (quotient (+ (* 153 mp) 2) 5)]
              [doe (+ (* yoe 365) (quotient yoe 4) (- (quotient yoe 100)) doy (- d 1))]
              [t (+ (* (+ (* era 146097) doe -719468) 86400) (* h 3600) (* mi 60) s)])
         ;; calendar-strict: reject non-calendar dates via the round-trip
         (and (string=? (epoch->utc-string t) v) t))))

;; Observed-or-unknown: "unknown" is the ONLY honest placeholder for
;; unobserved API values. Raw JSON null (read as 'null), a missing key or a
;; non-scalar must never reach a derived artifact as #f (a false timing) or
;; as a Racket symbol (a serialization crash).
(define (observed-or-unknown v)
  (cond
    [(string? v) v]
    [(real? v) v]
    [else "unknown"]))

;; GitHub job names carry the calling workflow prefix ("CI / test (0)") for
;; called reusable workflows; the required-check policy names the bare job.
(define (bare-job-name name)
  (last (string-split (or name "") " / ")))

;; Locate ONE file by exact name anywhere below dir (the artifact extractor
;; layout varies: a pattern download of several artifacts nests one
;; subdirectory per artifact, a single-artifact download extracts flat).
(define (find-named-file dir name)
  (and (string? name)
       (directory-exists? dir)
       (for/first ([p (in-directory dir)]
                   #:when (and (file-exists? p) (equal? (file-name-from-path p) (string->path name))))
         p)))

;; ------------------------------------------------------- validate-dispatch

(define (validate-dispatch #:trial-id trial-id
                           #:attempt attempt
                           #:treatment treatment
                           #:head-sha head-sha)
  (and (non-empty-string? trial-id)
       (exact-positive-integer? attempt)
       (member treatment trial-treatments)
       (trial-head-sha? head-sha)
       #t))

;; ------------------------------------------------------------ bind-producer

(define (bind-producer #:stage stage #:out out)
  (define prepared-manifest (build-path stage "manifest.json"))
  (define compiled-manifest (build-path stage "q-compiled" "trusted-root" "manifest.rktd"))
  (unless (file-exists? prepared-manifest)
    (error 'ci-recovery-trial "bind-producer: no staged manifest.json under ~a" stage))
  (define doc
    (hasheq 'schema
            producer-manifests-schema
            'prepared-env-manifest-sha256
            (sha256-hex-of-file prepared-manifest)
            'compiled-root-manifest-sha256
            (and (file-exists? compiled-manifest) (sha256-hex-of-file compiled-manifest))))
  (define doc-without-false
    (for/hasheq ([(k v) (in-hash doc)]
                 #:unless (not v))
      (values k v)))
  (write-json-file! out doc-without-false)
  0)

;; ------------------------------------------------------------- emit-identity

;; Producer-side W6 identity for the trial envelope (the same ten dimensions
;; the consumer-side setup-racket gate derives, observed HERE from the
;; producer job's real bytes: the lock file, the resolved racket executable
;; and the staged addon store). The manifest itself is written by the W1
;; CLI (--identity-emit), so its canonical shape stays single-sourced.
(define (store-digest-over dir)
  ;; Trial-only recipe: sorted per-file "<digest>  <relative-path>\n"
  ;; lines, hashed again. Relocation never changes identity; changed paths
  ;; or bytes do. The shared setup action's normal-PR recipe is untouched.
  (define files
    (sort (for/list ([p (in-directory dir)]
                     #:when (file-exists? p))
            p)
          path<?))
  (define lines
    (apply bytes-append
           (for/list ([p (in-list files)])
             (bytes-append (string->bytes/utf-8 (sha256-hex (file->bytes p)))
                           #"  "
                           (string->bytes/utf-8 (path->string (find-relative-path dir p)))
                           #"\n"))))
  (sha256-hex lines))

(define (emit-identity #:stage [stage ""]
                       #:addon-store [installed-store #f]
                       #:out out
                       #:lock-file lock-file
                       #:racket-version racket-version
                       #:os os
                       #:os-image os-image
                       #:arch arch
                       #:recipe-revision recipe-revision
                       #:policy-fingerprint policy-fingerprint)
  (define addon-store (or installed-store (build-path stage "addon-store")))
  (unless (directory-exists? addon-store)
    (error 'ci-recovery-trial "emit-identity: no staged addon-store under ~a" stage))
  (unless (and (file-exists? lock-file)
               (non-empty-string? racket-version)
               (non-empty-string? os)
               (non-empty-string? os-image)
               (non-empty-string? arch)
               (non-empty-string? recipe-revision)
               (non-empty-string? policy-fingerprint))
    (error 'ci-recovery-trial "emit-identity: missing required identity input"))
  (define racket-exe (find-executable-path "racket"))
  (unless racket-exe
    (error 'ci-recovery-trial "emit-identity: no racket executable on PATH"))
  (define exec-digest (sha256-hex (file->bytes racket-exe)))
  (define lock-digest (sha256-hex (file->bytes lock-file)))
  (define store-digest (store-digest-over addon-store))
  (define argv
    (list (path->string prepared-env-report-script)
          "--identity-emit"
          "--out"
          out
          "--os"
          os
          "--os-image"
          os-image
          "--arch"
          arch
          "--racket-version"
          racket-version
          "--racket-executable-digest"
          exec-digest
          "--lock-digest"
          lock-digest
          "--resolved-set-digest"
          store-digest
          "--precompile-recipe-revision"
          recipe-revision
          "--policy-fingerprint"
          policy-fingerprint
          "--artifact-digest"
          store-digest
          "--artifact-name-base"
          "prepared-env-fast"))
  (define code
    (parameterize ([current-output-port (current-error-port)])
      (apply system*/exit-code (find-executable-path "racket") argv)))
  (unless (zero? code)
    (error 'ci-recovery-trial "emit-identity: --identity-emit failed with exit code ~a" code))
  0)

;; ---------------------------------------------------- stamp-producer-manifest

(define (restored-file-under dir name)
  ;; The artifact extractor layout varies (upload-artifact stores a single
  ;; directory's bare contents); locate the file rather than guessing a path.
  (for/first ([p (in-directory dir)]
              #:when (equal? (file-name-from-path p) (string->path name)))
    p))

(define (stamp-producer-manifest #:shard shard
                                 #:out out
                                 #:prepared-download prepared-download
                                 #:compiled-root [compiled-root ""])
  (define root-text
    (cond
      [(path? compiled-root) (path->string compiled-root)]
      [(string? compiled-root) compiled-root]
      [else ""]))
  (define compiled-manifest
    (and (non-empty-string? root-text)
         (let ([p (build-path (string->path root-text) "manifest.rktd")]) (and (file-exists? p) p))))
  (define observed
    (or (and compiled-manifest
             (hash 'basis
                   "compiled-root-manifest"
                   'producer-manifest
                   (path->string compiled-manifest)
                   'producer-manifest-digest
                   (sha256-hex-of-file compiled-manifest)))
        (let ([found (and (directory-exists? prepared-download)
                          (restored-file-under prepared-download "manifest.json"))])
          (and found
               (hash 'basis
                     "prepared-env-manifest"
                     'producer-manifest
                     (path->string found)
                     'producer-manifest-digest
                     (sha256-hex-of-file found))))))
  (if observed
      (let ([stamp (hash-union-clean (hasheq 'schema producer-stamp-schema 'shard shard) observed)])
        (write-json-file! out stamp)
        0)
      (begin
        ;; An unobserved binding is never invented: write nothing and
        ;; report the honest miss to the caller (exit code 1).
        (eprintf
         "ci-recovery-trial: no restored producer manifest observed for shard ~a; writing no stamp\n"
         shard)
        (when (file-exists? out)
          (delete-file out))
        1)))

(define (hash-union-clean a b)
  (for/fold ([acc a])
            ([(k v) (in-hash b)]
             #:unless (not v))
    (hash-set acc k v)))

;; ----------------------------------------------------------------- collect

(define (collect-trial #:run-json run-json-path
                       #:jobs-json jobs-json-path
                       #:required-policy required-policy-path
                       #:records-dir records-dir
                       #:maps-dir maps-dir
                       #:producer-dir producer-dir
                       #:trial-id trial-id
                       #:attempt attempt
                       #:treatment treatment
                       #:run-id run-id
                       #:run-attempt run-attempt
                       #:head-sha head-sha
                       #:workflow-revision-sha workflow-revision-sha
                       #:shard-total shard-total
                       #:created-at-utc created-at-utc
                       #:test-inventory-digest test-inventory-digest
                       #:shard-plan shard-plan
                       #:aggregate-attestation [aggregate-attestation #f]
                       #:out-dir out-dir)
  ;; Fail closed on an ill-formed caller identity BEFORE anything is written.
  (unless (validate-dispatch #:trial-id trial-id
                             #:attempt attempt
                             #:treatment treatment
                             #:head-sha head-sha)
    (error 'ci-recovery-trial
           "collect: ill-formed trial identity (trial-id/attempt/treatment/head-sha)"))
  (unless (trial-head-sha? workflow-revision-sha)
    (error 'ci-recovery-trial "collect: ill-formed workflow-revision-sha"))
  (unless (and (exact-positive-integer? shard-total)
               (exact-positive-integer? run-id)
               (exact-positive-integer? run-attempt))
    (error 'ci-recovery-trial "collect: run-id/run-attempt/shard-total must be positive integers"))
  (unless (and (non-empty-string? created-at-utc)
               (file-exists? run-json-path)
               (file-exists? jobs-json-path)
               (file-exists? required-policy-path))
    (error 'ci-recovery-trial "collect: missing required input"))
  (make-directory* out-dir)
  ;; Verbatim raw retention first — every attempt (success, failure or
  ;; cancel) keeps its Actions metadata exactly as the API returned it.
  (copy-file run-json-path (build-path out-dir "run.json") #t)
  (copy-file jobs-json-path (build-path out-dir "jobs.json") #t)
  (define run-doc (read-json-input! run-json-path 'collect))
  (define jobs-doc (read-json-input! jobs-json-path 'collect))
  (define required-names
    (let ([doc (with-input-from-file required-policy-path read)]) (and (list? doc) doc)))
  (unless (and (list? required-names) (andmap string? required-names))
    (error 'ci-recovery-trial "collect: required-pr-checks.policy is not a list of names"))
  (define cp-doc (critical-path-document run-doc jobs-doc required-names workflow-revision-sha))
  (write-json-file! (build-path out-dir "critical-path.json")
                    (hash-set (hash-set cp-doc 'schema critical-path-schema)
                              'trial
                              (hasheq 'trial-id trial-id 'attempt attempt 'treatment treatment)))
  ;; Producer binding: real digest bytes from the producer job's artifact.
  ;; The artifact extractor layout varies (single vs multi-artifact pattern
  ;; downloads nest per-artifact subdirectories), so the named files are
  ;; LOCATED under the download directory rather than guessed flat — all
  ;; real trials of the first cut were refused by the flat-path miss.
  (define binding-path (find-named-file producer-dir "producer-manifests.json"))
  (define identity-path (find-named-file producer-dir "identity-manifest.json"))
  (define binding (and binding-path (read-json-file-or-#f binding-path)))
  (define identity-manifest (and identity-path (read-json-file-or-#f identity-path)))
  (when binding-path
    (copy-file binding-path (build-path out-dir "producer-manifests.json") #t))
  (when identity-path
    (copy-file identity-path (build-path out-dir "identity-manifest.json") #t))
  ;; Aggregate attestation: the test-aggregate job's OBSERVED restore
  ;; outcome (its real setup-racket outputs), downloaded from the run's
  ;; artifact. Missing means unattested — honestly "unknown", never a
  ;; fabricated verified restore.
  (define attestation-path
    (and aggregate-attestation
         (find-named-file aggregate-attestation "prepared-aggregate-restore.json")))
  (when (and aggregate-attestation attestation-path)
    (copy-file attestation-path (build-path out-dir "prepared-aggregate-restore.json") #t))
  (define attestation
    (and attestation-path
         (or (read-json-file-or-#f attestation-path)
             (error 'ci-recovery-trial
                    "collect: aggregate attestation artifact is present but unparseable: ~a"
                    attestation-path))))
  (define aggregate-state (aggregate-restore-state attestation))
  (define expected-digest
    (and binding
         (let ([key (if (string=? treatment "C")
                        'compiled-root-manifest-sha256
                        'prepared-env-manifest-sha256)])
           (hash-ref binding key #f))))
  (define identity-fields (and (hash? identity-manifest) (hash-ref identity-manifest 'fields #f)))
  (when (and (equal? aggregate-state "verified-restore")
             (not (and (equal? (hash-ref attestation 'run-id #f) run-id)
                       (equal? (hash-ref attestation 'run-attempt #f) run-attempt)
                       (equal? (hash-ref attestation 'head-sha #f) head-sha)
                       (equal? (hash-ref attestation 'identity #f) identity-fields)
                       (equal? (hash-ref attestation 'producer-manifest-digest #f) expected-digest))))
    (set! aggregate-state "unknown"))
  (unless (hex-digest? test-inventory-digest)
    (error 'ci-recovery-trial
           "collect: --test-inventory-digest must be a 64-hex digest of the actual test inventory"))
  (unless (and (list? shard-plan)
               (pair? shard-plan)
               (andmap exact-nonnegative-integer? shard-plan)
               (not (check-duplicates shard-plan)))
    (error 'ci-recovery-trial
           "collect: --shard-plan must be a non-empty comma list of unique shard integers"))
  (define envelope
    (trial-envelope #:treatment treatment
                    #:trial-id trial-id
                    #:attempt attempt
                    #:run-id run-id
                    #:head-sha head-sha
                    #:created-at-utc created-at-utc
                    #:shard-total shard-total
                    #:expected-digest expected-digest
                    #:identity-fields identity-fields
                    #:records-dir records-dir
                    #:maps-dir maps-dir))
  (write-json-file! (build-path out-dir "envelope.json") envelope)
  ;; One-line JSONL twins so the report owner's CLI consumes the collected
  ;; evidence DIRECTLY (docs/reports/CI-RECOVERY-EXPERIMENT.md §4):
  ;;   racket scripts/run-tests/ci-recovery-benchmark-report.rkt \
  ;;     --envelopes ENVELOPES.jsonl --actions ACTIONS.jsonl --out REPORT.json
  (write-json-file! (build-path out-dir "envelope.jsonl") envelope)
  (define actions
    (actions-evidence-document #:run-doc run-doc
                               #:jobs-doc jobs-doc
                               #:required-names required-names
                               #:identity-fields identity-fields
                               #:test-inventory-digest test-inventory-digest
                               #:shard-plan shard-plan
                               #:workflow-revision-sha workflow-revision-sha
                               #:aggregate-state aggregate-state
                               #:attestation attestation
                               #:records-dir records-dir))
  (write-json-file! (build-path out-dir "actions.json") actions)
  (write-json-file! (build-path out-dir "actions.jsonl") actions)
  ;; Fail-closed treatment honesty: a B/C trial whose aggregate restore was
  ;; NOT attested as verified (missing, unknown or a cold fallback) is never
  ;; a successful prepared-aggregate measurement. All raw evidence above is
  ;; retained; the nonzero exit refuses the attempt loudly instead of
  ;; classifying it as prepared treatment.
  (define refuse-code
    (if (or (and (member treatment (list "B" "C")) (not (equal? aggregate-state "verified-restore")))
            (and (equal? treatment "A") (not (equal? aggregate-state "off")))
            (equal? (hash-ref run-doc 'conclusion #f) "cancelled"))
        (begin
          (eprintf
           "ci-recovery-trial: refused treatment ~a (aggregate ~a or cancelled run); all evidence retained\n"
           treatment
           aggregate-state)
          1)
        0))
  (write-json-file! (build-path out-dir "collection.json")
                    (hasheq 'result
                            (if (zero? refuse-code) "collected" "refused")
                            'run-conclusion
                            (observed-or-unknown (hash-ref run-doc 'conclusion #f))
                            'aggregate-state
                            aggregate-state))
  (printf
   "ci-recovery-trial: collected trial ~a attempt ~a treatment ~a into ~a (envelope: ~a; actions: ~a; aggregate restore: ~a)\n"
   trial-id
   attempt
   treatment
   out-dir
   (path->string (build-path out-dir "envelope.json"))
   (path->string (build-path out-dir "actions.json"))
   aggregate-state)
  refuse-code)

(define (critical-path-document run-doc jobs-doc required-names workflow-revision-sha)
  (define run-started (hash-ref run-doc 'run_started_at #f))
  (define start-epoch (iso-utc->epoch run-started))
  (define jobs (hash-ref jobs-doc 'jobs #f))
  (unless (and (hash? run-doc) (hash? jobs-doc) (list? jobs))
    (error 'ci-recovery-trial "collect: run/jobs metadata is not the gh api shape"))
  (define (job-required? j)
    (member (bare-job-name (hash-ref j 'name #f)) required-names))
  (define required-jobs (filter job-required? jobs))
  (define (timing j)
    (define s (iso-utc->epoch (hash-ref j 'started_at #f)))
    (define e (iso-utc->epoch (hash-ref j 'completed_at #f)))
    (and s e start-epoch (>= s start-epoch) (>= e s) (hash 'start s 'end e)))
  (define observed-required (map (lambda (j) (bare-job-name (hash-ref j 'name #f))) required-jobs))
  (define missing-required
    (sort (for/list ([n (in-list required-names)]
                     #:unless (member n observed-required))
            n)
          string<?))
  (define required-critical-path-seconds
    ;; Same rule as the report (§5.1): the path exists ONLY when the required
    ;; job set is complete and every required job has parseable completion —
    ;; a cancelled/missing required job makes the path "unknown", never a
    ;; misleading max over the jobs that did finish.
    (if (and start-epoch (null? missing-required) (pair? required-jobs) (andmap timing required-jobs))
        (apply max
               (for/list ([j (in-list required-jobs)])
                 (- (hash-ref (timing j) 'end) start-epoch)))
        #f))
  (define sorted-required
    (sort required-jobs string<? #:key (lambda (j) (bare-job-name (hash-ref j 'name #f)))))
  (define timed-jobs
    (for/list ([j (in-list sorted-required)])
      (define t (timing j))
      (hasheq 'name
              (bare-job-name (hash-ref j 'name #f))
              'job-id
              (observed-or-unknown (hash-ref j 'id #f))
              'conclusion
              (observed-or-unknown (hash-ref j 'conclusion #f))
              'started_at
              (observed-or-unknown (hash-ref j 'started_at #f))
              'completed_at
              (observed-or-unknown (hash-ref j 'completed_at #f))
              'queue-seconds
              (if t
                  (- (hash-ref t 'start) start-epoch)
                  "unknown")
              'execution-seconds
              (if t
                  (- (hash-ref t 'end) (hash-ref t 'start))
                  "unknown"))))
  (hasheq 'basis
          (string-append
           "required critical path = max over required jobs of (completed_at - run_started_at);"
           " jobs run in parallel and are NEVER summed and never reduced to a single job duration;"
           " queue time is reported separately (started_at - run_started_at);"
           " missing/skipped required checks are named, never zero-filled")
          'run-id
          (observed-or-unknown (hash-ref run-doc 'id #f))
          'run-attempt
          (observed-or-unknown (hash-ref run-doc 'run_attempt #f))
          'event
          (observed-or-unknown (hash-ref run-doc 'event #f))
          'head-branch
          (observed-or-unknown (hash-ref run-doc 'head_branch #f))
          'head-sha
          (observed-or-unknown (hash-ref run-doc 'head_sha #f))
          'workflow-revision-sha
          workflow-revision-sha
          'run-started-at
          (observed-or-unknown run-started)
          'required-job-names
          (sort required-names string<?)
          'required-jobs
          timed-jobs
          'missing-required-jobs
          missing-required
          'non-required-jobs
          (sort (for/list ([j (in-list jobs)]
                           #:unless (job-required? j))
                  (bare-job-name (hash-ref j 'name #f)))
                string<?)
          'required-critical-path-seconds
          (or required-critical-path-seconds "unknown")))

(define (trial-envelope #:treatment treatment
                        #:trial-id trial-id
                        #:attempt attempt
                        #:run-id run-id
                        #:head-sha head-sha
                        #:created-at-utc created-at-utc
                        #:shard-total shard-total
                        #:expected-digest expected-digest
                        #:identity-fields identity-fields
                        #:records-dir records-dir
                        #:maps-dir maps-dir)
  (define expected
    (let ([base (hasheq 'shard-inventory (range 0 shard-total))])
      (if identity-fields
          (hash-set base 'identity identity-fields)
          base)))
  ;; A missing producer digest stays MISSING (the assessor refuses
  ;; expected-producer-manifest-digest-malformed); it is never zero-filled.
  (define expected+
    (if expected-digest
        (hash-set expected 'producer-manifest-digest expected-digest)
        expected))
  (hasheq 'schema
          trial-envelope-schema
          'trial
          (hasheq 'trial-id
                  trial-id
                  'attempt
                  attempt
                  'treatment
                  treatment
                  'run-id
                  run-id
                  'head-sha
                  head-sha
                  'created-at-utc
                  created-at-utc)
          'expected
          expected+
          'shards
          (filter (lambda (entry) (or (hash-has-key? entry 'record) (hash-has-key? entry 'launcher)))
                  (for/list ([sid (in-list (range 0 shard-total))])
                    (shard-entry #:sid sid
                                 #:run-id run-id
                                 #:trial-id trial-id
                                 #:attempt attempt
                                 #:treatment treatment
                                 #:records-dir records-dir
                                 #:maps-dir maps-dir)))))

(define (artifact-dir-for base-dir pattern-shard)
  ;; Artifact directories are named "<prefix>-<run-id>-<shard>"; match on the
  ;; trailing shard so the binding to the run id stays explicit in the file
  ;; names and any run-id mismatch surfaces in the assessor's refusal.
  (for/first ([p (in-directory base-dir)]
              #:when (and (directory-exists? p)
                          (let ([m (regexp-match #px"([0-9]+)-([0-9]+)$"
                                                 (path->string (file-name-from-path p)))])
                            (and m (equal? (string->number (list-ref m 2)) pattern-shard)))))
    p))

(define (first-file-with-name dir name)
  (for/first ([p (in-directory dir)]
              #:when (and (file-exists? p) (equal? (file-name-from-path p) (string->path name))))
    p))

;; The shard's OWN measured identity: the canonical prepared-env-identity@1
;; manifest the consumer-side setup-racket emitted (via --identity-emit) from
;; the RESTORED environment of THIS shard, captured into the per-shard map
;; artifact as shard-<n>.identity.json. This is what the strict assessor
;; compares field-by-field against the PRODUCER-bound expected identity — a
;; measured-vs-produced comparison, never a producer value stamped back onto
;; the record (which would make the identity check tautological). A shard
;; that did not capture its identity carries none: the assessor refuses
;; record-identity-missing instead of the record borrowing the producer's.
(define (shard-observed-identity maps-dir sid)
  (define dir (and maps-dir (artifact-dir-for maps-dir sid)))
  (and dir
       (let ([p (first-file-with-name dir (format "shard-~a.identity.json" sid))])
         (and p (let ([doc (read-json-file-or-#f p)]) (and (hash? doc) (hash-ref doc 'fields #f)))))))

(define (shard-entry #:sid sid
                     #:run-id run-id
                     #:trial-id trial-id
                     #:attempt attempt
                     #:treatment treatment
                     #:records-dir records-dir
                     #:maps-dir maps-dir)
  (define record-doc
    (let ([dir (and records-dir (artifact-dir-for records-dir sid))])
      (and dir
           (for/first ([p (in-directory dir)]
                       #:when (and (file-exists? p)
                                   (regexp-match? #rx"record-.*\\.json$"
                                                  (path->string (file-name-from-path p)))))
             (read-json-file-or-#f p)))))
  (define entry (hasheq 'shard sid))
  (define entry+record
    (if (hash? record-doc)
        (hash-set entry
                  'record
                  (stamp-record record-doc
                                #:trial-id trial-id
                                #:attempt attempt
                                #:treatment treatment
                                #:observed-identity (shard-observed-identity maps-dir sid)))
        entry))
  (define map-dir (and maps-dir (artifact-dir-for maps-dir sid)))
  (define datum
    (and map-dir
         (let ([p (first-file-with-name map-dir (format "shard-~a.launch.json" sid))])
           (and p
                (with-handlers ([exn:fail? (lambda (_) #f)])
                  (with-input-from-file p read))))))
  (define stamp
    (and map-dir
         (let ([p (first-file-with-name map-dir (format "shard-~a.producer.json" sid))])
           (read-json-file-or-#f p))))
  (if datum
      (hash-set entry+record 'launcher (launcher-json datum stamp))
      entry+record))

;; The concurrent strict-record repair requires per-record trial keys equal
;; to the trial/expected blocks. Stamp ONLY keys the record emitter does not
;; carry yet, and only from caller-held values — a record that already binds
;; them wins verbatim.
(define (stamp-record record-doc
                      #:trial-id trial-id
                      #:attempt attempt
                      #:treatment treatment
                      #:observed-identity observed-identity)
  (define stamped
    (for/fold ([acc record-doc])
              ([(k v) (in-hash (hasheq 'trial-id
                                       trial-id
                                       'attempt
                                       attempt
                                       'treatment
                                       treatment
                                       'identity
                                       (or observed-identity #f)))]
               #:unless (not v))
      (if (hash-has-key? acc k)
          acc
          (hash-set acc k v))))
  stamped)

(define (launcher-json datum stamp)
  ;; Map the Racket datum written by write-launch-telemetry! 1:1 into the
  ;; envelope's launcher object (docs/reports/CI-RECOVERY-EVIDENCE.md §2):
  ;; (compiled-root-launch-telemetry (mode . "…") (exit-code . n) (wall-ms . n)
  ;;  (root-hits . n) (fallbacks . n) (loads . n) (bytes-served . n)
  ;;  (mechanism . "…"))
  (unless (and (list? datum) (pair? datum) (equal? (car datum) 'compiled-root-launch-telemetry))
    (error 'ci-recovery-trial "launch telemetry is not a compiled-root-launch-telemetry datum"))
  (define launcher
    (for/hasheq ([field (in-list (cdr datum))]
                 #:when (pair? field))
      (values (car field) (cdr field))))
  (define launcher+digest
    (if (and stamp (hash-ref stamp 'producer-manifest-digest #f))
        (hash-set launcher 'producer-manifest-digest (hash-ref stamp 'producer-manifest-digest))
        launcher))
  (hash-set launcher+digest 'raw-datum (with-output-to-string (lambda () (write datum)))))

;; ------------------------------------------------- actions evidence (report)

;; The report owner's input contract ci-recovery-actions-evidence@1
;; (docs/reports/CI-RECOVERY-EXPERIMENT.md §3.2) had NO producer in the
;; first cut: the collected trial artifact carried the RAW Actions
;; metadata, but no document the report CLI could read directly. This
;; transform is that producer: raw run/jobs metadata + the producer-side
;; observed identity + caller-bound inventory/plan/harness inputs → ONE
;; consumable document. Timestamps and durations come from the raw API
;; fields and the emitted records ONLY — nothing is fabricated.

(define (run-workflow-name run-doc)
  ;; run.path is "<repo>/.github/workflows/<file>.yml@<ref>"; the contract
  ;; names the workflow FILE.
  (define p (hash-ref run-doc 'path #f))
  (if (string? p)
      (let ([m (regexp-match #rx"/workflows/([^/@]+)" p)])
        (if m
            (cadr m)
            "unknown"))
      "unknown"))

(define (actions-run-block run-doc)
  (hasheq 'run-id
          (observed-or-unknown (hash-ref run-doc 'id #f))
          'head-sha
          (observed-or-unknown (hash-ref run-doc 'head_sha #f))
          'run-attempt
          (observed-or-unknown (hash-ref run-doc 'run_attempt #f))
          'status
          (observed-or-unknown (hash-ref run-doc 'status #f))
          'conclusion
          (observed-or-unknown (hash-ref run-doc 'conclusion #f))
          'workflow
          (run-workflow-name run-doc)
          'event
          (observed-or-unknown (hash-ref run-doc 'event #f))
          'run-started-at
          (observed-or-unknown (hash-ref run-doc 'run_started_at #f))))

(define (actions-jobs-block jobs-doc required-names)
  (define jobs (hash-ref jobs-doc 'jobs #f))
  (for/list ([j (in-list (if (list? jobs)
                             jobs
                             '()))]
             #:when (hash? j))
    (define bare (bare-job-name (hash-ref j 'name #f)))
    (hasheq 'name
            bare
            'role
            (if (member bare required-names) "required" "pilot")
            'status
            (observed-or-unknown (hash-ref j 'status #f))
            'conclusion
            (observed-or-unknown (hash-ref j 'conclusion #f))
            'started-at
            (observed-or-unknown (hash-ref j 'started_at #f))
            'completed-at
            (observed-or-unknown (hash-ref j 'completed_at #f)))))

(define (actions-config-block identity-fields
                              test-inventory-digest
                              shard-plan
                              workflow-revision-sha
                              aggregate-state)
  ;; The identity dimensions are the PRODUCER job's observed values (same
  ;; source as the envelope's expected identity); inventory/plan/harness are
  ;; caller-bound from the actual checkout and matrix; the aggregate state is
  ;; the aggregate job's observed restore attestation.
  (define (f k)
    (define v (and (hash? identity-fields) (hash-ref identity-fields k #f)))
    (if (string? v) v "unknown"))
  (hasheq 'runner-image
          (f 'os-image)
          'platform
          (f 'arch)
          'racket-version
          (f 'racket-version)
          'racket-executable-digest
          (f 'racket-executable-digest)
          'lock-digest
          (f 'lock-digest)
          'resolved-set-digest
          (f 'resolved-set-digest)
          'precompile-recipe-revision
          (f 'precompile-recipe-revision)
          'policy-fingerprint
          (f 'policy-fingerprint)
          'test-inventory-digest
          test-inventory-digest
          'shard-plan
          shard-plan
          'harness-revision
          (string-append "ci-recovery-benchmark.yml@" workflow-revision-sha)
          'prepared-aggregate-restore
          aggregate-state))

(define (observed-record-numbers records-dir)
  ;; Costs ACTUALLY observed in this run's emitted restore records; absent
  ;; components stay "unknown" downstream (never zero-filled).
  (for/fold ([producer-secs (list)]
             [restore-ms (list)])
            ([p (in-directory records-dir)]
             #:when (and (file-exists? p)
                         (regexp-match? #rx"record-.*\\.json$"
                                        (path->string (file-name-from-path p)))))
    (define doc (read-json-file-or-#f p))
    (define pv (and (hash? doc) (hash-ref doc 'compiled-root-producer-seconds #f)))
    (define rv (and (hash? doc) (hash-ref doc 'restore-ms #f)))
    (values (if (real? pv)
                (cons pv producer-secs)
                producer-secs)
            (if (real? rv)
                (cons rv restore-ms)
                restore-ms))))

(define (aggregate-restore-state attestation)
  ;; Honest classification of the aggregate job's ACTUAL restore from its
  ;; attestation (its real setup-racket outputs). Unknown is the only
  ;; fallback: a missing or malformed attestation never reads as verified.
  (cond
    [(not (and (hash? attestation)
               (equal? (hash-ref attestation 'schema #f) aggregate-attestation-schema)
               (let ([ms (hash-ref attestation 'restore-ms #f)])
                 (or (equal? ms "unknown") (and (real? ms) (>= ms 0))))))
     "unknown"]
    [else
     (define requested (hash-ref attestation 'prepared-requested #f))
     (define result (hash-ref attestation 'result #f))
     (cond
       [(equal? requested "off") "off"]
       [(and (equal? requested "auto")
             (equal? result "success")
             (equal? (hash-ref attestation 'schema #f) aggregate-attestation-schema)
             (exact-positive-integer? (hash-ref attestation 'run-id #f))
             (exact-positive-integer? (hash-ref attestation 'run-attempt #f))
             (trial-head-sha? (hash-ref attestation 'head-sha #f))
             (equal? (hash-ref attestation 'identity-result #f) "verified")
             (let ([state (hash-ref attestation 'state #f)])
               (and (non-empty-string? state) (not (member state '("unknown" "unavailable")))))
             (let ([identity (hash-ref attestation 'identity #f)])
               (and (hash? identity) (= (hash-count identity) 10)))
             (hex-digest? (hash-ref attestation 'producer-manifest-digest #f)))
        "verified-restore"]
       [(equal? result "failure") "cold-fallback"]
       [(equal? result "skipped") "skipped"]
       [else "unknown"])]))

(define (actions-components-block records-dir attestation aggregate-state)
  (define-values (producer-secs restore-ms) (observed-record-numbers records-dir))
  (define attested-restore-ms
    (let ([v (and (hash? attestation) (hash-ref attestation 'restore-ms #f))])
      (and (real? v) (equal? aggregate-state "verified-restore") v)))
  (hasheq 'compiled-root-producer-seconds
          (if (null? producer-secs)
              "unknown"
              (apply max producer-secs))
          'prepared-aggregate-seconds
          (if attested-restore-ms
              (/ attested-restore-ms 1000.0)
              "unknown")
          'restore-ms
          (if (null? restore-ms)
              "unknown"
              (apply max restore-ms))
          ;; the proof-bundle cost is not emitted by this run's evidence; it
          ;; stays unknown rather than being invented
          'proof-seconds
          "unknown"))

(define (parse-shard-plan s)
  ;; "0,1,2" → (list 0 1 2); the CLI fails closed on anything else.
  (define trimmed (string-trim (or s "")))
  (if (string=? trimmed "")
      '()
      (for/list ([tok (in-list (string-split trimmed ","))])
        (string->number (string-trim tok)))))

(define (actions-evidence-document #:run-doc run-doc
                                   #:jobs-doc jobs-doc
                                   #:required-names required-names
                                   #:identity-fields identity-fields
                                   #:test-inventory-digest test-inventory-digest
                                   #:shard-plan shard-plan
                                   #:workflow-revision-sha workflow-revision-sha
                                   #:aggregate-state aggregate-state
                                   #:attestation attestation
                                   #:records-dir records-dir)
  (hasheq 'schema
          actions-evidence-schema
          'aggregate-attestation
          (or attestation 'null)
          'run
          (actions-run-block run-doc)
          'required-job-names
          (sort required-names string<?)
          'jobs
          (actions-jobs-block jobs-doc required-names)
          'config
          (actions-config-block identity-fields
                                test-inventory-digest
                                shard-plan
                                workflow-revision-sha
                                aggregate-state)
          'components
          (actions-components-block records-dir attestation aggregate-state)))

;; ----------------------------------------------------------------- the CLI

(module+ main
  (define argv (vector->list (current-command-line-arguments)))
  (define (usage-fail msg)
    (eprintf "ci-recovery-trial: ~a\nusage:\n" msg)
    (eprintf
     "  ci-recovery-trial.rkt validate-dispatch --trial-id T --attempt N --treatment X --head-sha S\n")
    (eprintf "  ci-recovery-trial.rkt bind-producer --stage DIR --out F\n")
    (eprintf
     "  ci-recovery-trial.rkt emit-identity --stage DIR --out F --lock-file F --racket-version V\n")
    (eprintf "      --os OS --os-image IMG --arch A --recipe-revision R --policy-fingerprint PF\n")
    (eprintf
     "  ci-recovery-trial.rkt stamp-producer-manifest --shard N --out F --prepared-download DIR [--compiled-root DIR]\n")
    (eprintf
     "  ci-recovery-trial.rkt collect --run-json F --jobs-json F --required-policy F --records-dir D\n")
    (eprintf
     "      --maps-dir D --producer-dir D --aggregate-attestation D --trial-id T --attempt N --treatment X --run-id N\n")
    (eprintf
     "      --run-attempt N --head-sha S --workflow-revision-sha S --shard-total N --shard-plan 0,1,2\n")
    (eprintf "      --test-inventory-digest HEX --created-at-utc T --out-dir D\n")
    (exit 2))
  (define (flag args name)
    (let loop ([xs args])
      (cond
        [(null? xs) #f]
        [(string=? (car xs) name)
         (if (null? (cdr xs))
             (usage-fail (format "missing value for ~a" name))
             (cadr xs))]
        [else (loop (cdr xs))])))
  (define cmd
    (if (null? argv)
        (usage-fail "no mode given")
        (car argv)))
  (define rest
    (if (null? argv)
        '()
        (cdr argv)))
  (define (string->pos-int v who)
    (unless (positive-integer-string? v)
      (usage-fail (format "~a must be a positive integer" who)))
    (string->number v))
  (define code
    (cond
      [(string=? cmd "validate-dispatch")
       (define ok
         (validate-dispatch #:trial-id (or (flag rest "--trial-id") "")
                            #:attempt (string->pos-int (or (flag rest "--attempt") "0") "--attempt")
                            #:treatment (or (flag rest "--treatment") "")
                            #:head-sha (or (flag rest "--head-sha") "")))
       (if ok 0 1)]
      [(string=? cmd "bind-producer")
       (bind-producer #:stage (or (flag rest "--stage")
                                  (usage-fail "bind-producer requires --stage DIR"))
                      #:out (or (flag rest "--out") (usage-fail "bind-producer requires --out F")))]
      [(string=? cmd "emit-identity")
       (emit-identity #:stage (or (flag rest "--stage") "")
                      #:addon-store (flag rest "--addon-store")
                      #:out (or (flag rest "--out") (usage-fail "emit-identity requires --out F"))
                      #:lock-file (or (flag rest "--lock-file")
                                      (usage-fail "emit-identity requires --lock-file F"))
                      #:racket-version (or (flag rest "--racket-version") "")
                      #:os (or (flag rest "--os") "")
                      #:os-image (or (flag rest "--os-image") "")
                      #:arch (or (flag rest "--arch") "")
                      #:recipe-revision (or (flag rest "--recipe-revision") "")
                      #:policy-fingerprint (or (flag rest "--policy-fingerprint") ""))]
      [(string=? cmd "stamp-producer-manifest")
       (stamp-producer-manifest #:shard (let ([v (string->number (or (flag rest "--shard") ""))])
                                          (unless (exact-nonnegative-integer? v)
                                            (usage-fail "--shard must be a nonnegative integer"))
                                          v)
                                #:out (or (flag rest "--out")
                                          (usage-fail "stamp-producer-manifest requires --out F"))
                                #:prepared-download (or (flag rest "--prepared-download") "")
                                #:compiled-root (or (flag rest "--compiled-root") ""))]
      [(string=? cmd "attest-aggregate")
       (define (env name)
         (define v (getenv name))
         (if (non-empty-string? v) v "unknown"))
       (define (numeric name)
         (define v (string->number (env name)))
         (if (and (real? v) (>= v 0)) v "unknown"))
       (define identity
         (and (flag rest "--identity") (read-json-file-or-#f (flag rest "--identity"))))
       (define stamp (and (flag rest "--stamp") (read-json-file-or-#f (flag rest "--stamp"))))
       (write-json-file! (or (flag rest "--out") (usage-fail "attest-aggregate requires --out F"))
                         (hasheq 'schema
                                 aggregate-attestation-schema
                                 'run-id
                                 (numeric "RUN_ID")
                                 'run-attempt
                                 (numeric "RUN_ATTEMPT")
                                 'head-sha
                                 (env "HEAD_SHA")
                                 'prepared-requested
                                 (env "AGG_REQUESTED")
                                 'result
                                 (env "AGG_RESULT")
                                 'state
                                 (env "AGG_STATE")
                                 'identity-result
                                 (env "AGG_IDENTITY_RESULT")
                                 'restore-ms
                                 (numeric "AGG_RESTORE_MS")
                                 'identity
                                 (if (hash? identity)
                                     (hash-ref identity 'fields "unknown")
                                     "unknown")
                                 'producer-manifest-digest
                                 (if (hash? stamp)
                                     (hash-ref stamp 'producer-manifest-digest "unknown")
                                     "unknown")))
       0]
      [(string=? cmd "collect")
       (collect-trial
        #:run-json (or (flag rest "--run-json") (usage-fail "collect requires --run-json F"))
        #:jobs-json (or (flag rest "--jobs-json") (usage-fail "collect requires --jobs-json F"))
        #:required-policy (or (flag rest "--required-policy")
                              (usage-fail "collect requires --required-policy F"))
        #:records-dir (or (flag rest "--records-dir") (usage-fail "collect requires --records-dir D"))
        #:maps-dir (or (flag rest "--maps-dir") (usage-fail "collect requires --maps-dir D"))
        #:producer-dir (or (flag rest "--producer-dir")
                           (usage-fail "collect requires --producer-dir D"))
        #:trial-id (or (flag rest "--trial-id") "")
        #:attempt (string->pos-int (or (flag rest "--attempt") "0") "--attempt")
        #:treatment (or (flag rest "--treatment") "")
        #:run-id (string->pos-int (or (flag rest "--run-id") "0") "--run-id")
        #:run-attempt (string->pos-int (or (flag rest "--run-attempt") "0") "--run-attempt")
        #:head-sha (or (flag rest "--head-sha") "")
        #:workflow-revision-sha (or (flag rest "--workflow-revision-sha") "")
        #:shard-total (string->pos-int (or (flag rest "--shard-total") "0") "--shard-total")
        #:created-at-utc (or (flag rest "--created-at-utc") "")
        #:test-inventory-digest (or (flag rest "--test-inventory-digest") "")
        #:shard-plan (parse-shard-plan (or (flag rest "--shard-plan") ""))
        #:aggregate-attestation (flag rest "--aggregate-attestation")
        #:out-dir (or (flag rest "--out-dir") (usage-fail "collect requires --out-dir D")))]
      [else (usage-fail (format "unknown mode ~a" cmd))]))
  (exit code))
