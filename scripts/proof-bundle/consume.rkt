#lang racket/base

;; q/scripts/proof-bundle/consume.rkt — v1.00.29 W9: CLI adapter over the
;; canonical q.proof-bundle/1 writer (bundle-writer.rkt) and the read-only
;; fail-closed consumer validator (bundle-validator.rkt).
;;
;; Two subcommands:
;;
;;   write   — assemble a q.proof-bundle/1 from a claim/context JSON document
;;             plus scalar producer arguments, finalize it (content-addressed
;;             bundle_id, producer completeness gate §5.2–§5.16) and write the
;;             canonical single-line JSON to --out. Prints the bundle_id.
;;   consume — run validate-proof-bundle-file (validator steps 1–15) for a
;;             bundle + consumer request pair, write the q.reuse-decision/1
;;             record (§11.3) to --out and exit
;;                 0  'reusable        (the ONLY exit code that permits skip)
;;                 3  not-reusable     (authentic but incompatible: fallback)
;;                 4  invalid          (corrupt/inauthentic: fallback)
;;             Any other failure (unreadable files, bad usage) also never
;;             yields 0. NEVER a silent skip: the caller treats every
;;             non-zero exit as "run the suite".
;;
;; The --claims-json document for `write` carries the claim records plus the
;; producer-side run context (all values that vary per run):
;;
;;   { "created_at": "<RFC3339 UTC>",            (required)
;;     "workflow_path": ".github/workflows/...", (required)
;;     "producer": { run_id, run_attempt, job_id, job_name, event, ref,
;;                   trust_tier },               (required, §5.3)
;;     "claims": [ { claim_id, claim_version, proof_class,
;;                   required_environment_class, gate_class, result } ],
;;                                               (required, non-empty, §5.5)
;;     "selection" | "command" | "environment" | "policy" |
;;     "prepared_environment" | "result" | "artifacts", (required §5.6–§5.12)
;;     "environment_class": "...",               (required, §5.16)
;;     "attestation_digest": "sha256:...",       (required, §5.13)
;;     "allowed_consumers": ["..."],             (required, §5.15)
;;     "subject_mode" (default "commit"), "subject_dirty" (default false),
;;     "denied_consumers" (default []), "release_reusable" (default false),
;;     "verification_key_or_identity"
;;        (default "policy:q/release-evidence-v1"),
;;     "compatibility_notes" (default ""),
;;     "retention_policy_id" (default derived from --retention-days),
;;     ...any other keys (e.g. digest-source annotations) are ignored. }
;;
;; Determinism: for byte-identical inputs the written bundle is byte-identical
;; (canonical writer); `created_at` MUST therefore be supplied by the caller
;; instead of being invented here. consume records carry the decision-time
;; wall clock in `decided_at` only (the retained record is runtime evidence,
;; not a canonical artifact).
;;
;; No network, no threads, no sleeps. Dependencies: racket/base plus the
;; json, racket/contract, racket/file, racket/string and racket/date
;; collections only (no new info.rkt deps).

(require json
         racket/contract
         racket/date
         racket/file
         racket/string
         (only-in "bundle-writer.rkt" finalize-proof-bundle write-proof-bundle!)
         (only-in "bundle-validator.rkt" reuse-decision-record validate-proof-bundle-file))

;; Named contract bound at column 0 so scripts/check-deps.rkt's textual
;; scanner never mistakes an indented combinator for an external dependency
;; (same discipline as bundle-writer.rkt).
(define decision-contract (or/c 'reusable (list/c 'not-reusable symbol?) (list/c 'invalid symbol?)))

(provide (contract-out
          (write-command (-> (listof string?) integer?))
          (consume-command (-> (listof string?) integer?))
          ;; any/c on purpose: anything unanticipated must still fail closed
          ;; (exit 4), which is exactly what the function guarantees.
          (decision-exit-code (-> any/c integer?))
          (normalize-request (-> hash? hash?))
          (augmented-decision-record (-> (or/c string? hash?) hash? decision-contract hash?))
          (rfc3339-z->seconds (-> string? (or/c exact-integer? #f)))
          (seconds->rfc3339-z (-> exact-integer? string?))))

;; ---------------------------------------------------------------------------
;; Exit-code contract (§11.3: no silent skip)
;; ---------------------------------------------------------------------------

(define (decision-exit-code decision)
  (cond
    [(eq? decision 'reusable) 0]
    [(and (pair? decision) (eq? (car decision) 'not-reusable)) 3]
    [else 4]))

;; ---------------------------------------------------------------------------
;; Time helpers (strict RFC3339 UTC, matching the validator's parser)
;; ---------------------------------------------------------------------------

(define rfc3339-z-rx #px"^([0-9]{4})-([0-9]{2})-([0-9]{2})T([0-9]{2}):([0-9]{2}):([0-9]{2})Z$")

(define (rfc3339-z->seconds s)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define m (and (string? s) (regexp-match rfc3339-z-rx s)))
    (unless m
      (error 'rfc3339-z->seconds "malformed RFC3339 UTC timestamp: ~s" s))
    (define (n i)
      (string->number (list-ref m i)))
    (find-seconds (n 6) (n 5) (n 4) (n 3) (n 2) (n 1) #t)))

(define (seconds->rfc3339-z s)
  (define d (seconds->date s #f))
  (format "~a-~a-~aT~a:~a:~aZ"
          (pad2 (date-year d))
          (pad2 (date-month d))
          (pad2 (date-day d))
          (pad2 (date-hour d))
          (pad2 (date-minute d))
          (pad2 (date-second d))))

(define (pad2 v)
  (define s (number->string v))
  (if (< (string-length s) 2)
      (string-append "0" s)
      s))

;; ---------------------------------------------------------------------------
;; Flag parsing (hand-rolled; deterministic, no cmdline exits)
;; ---------------------------------------------------------------------------

(define (flag-lookup args flag)
  (let loop ([rest args])
    (cond
      [(null? rest) #f]
      [(string=? (car rest) flag)
       (cond
         [(null? (cdr rest)) (error 'consume.rkt "missing value for ~a" flag)]
         [else (cadr rest)])]
      [else (loop (cdr rest))])))

(define (usage-error what message)
  (fprintf (current-error-port) "consume.rkt ~a: ~a~n" what message)
  2)

;; ---------------------------------------------------------------------------
;; JSON helpers
;; ---------------------------------------------------------------------------

;; read-json yields string keys in some Racket versions and symbol keys in
;; others; the writer/validator vocabulary is symbol-keyed hashes, so
;; normalize defensively (same discipline as bundle-validator.rkt).
(define (json->symbols v)
  (cond
    [(hash? v)
     (for/hash ([(k val) (in-hash v)])
       (values (if (string? k)
                   (string->symbol k)
                   k)
               (json->symbols val)))]
    [(list? v) (map json->symbols v)]
    [else v]))

(define (read-json-file path what)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (json->symbols (call-with-input-file path read-json))))

;; The decision vocabulary is symbol-valued ('reusable, 'not-reusable:<reason>,
;; 'none); write-json accepts symbol KEYS but not symbol VALUES, so serialize
;; symbols as strings (deep, deterministic).
(define (json-safe v)
  (cond
    [(hash? v)
     (for/hash ([(k val) (in-hash v)])
       (values k (json-safe val)))]
    [(list? v) (map json-safe v)]
    [(symbol? v) (symbol->string v)]
    [else v]))

(define (write-json-file path record)
  (define-values (base _name _dir?) (split-path path))
  (when base
    (make-directory* base))
  (call-with-output-file path
                         (lambda (out)
                           (write-json (json-safe record) out)
                           (newline out))
                         #:exists 'replace))

;; ---------------------------------------------------------------------------
;; Request normalization (JSON text -> the validator's request vocabulary)
;; ---------------------------------------------------------------------------

;; The validator's in-memory request vocabulary (the one its own tests build
;; with hasheq) is: symbol keys everywhere, EXCEPT the `expected-artifacts`
;; map, whose keys are artifact names — DATA compared by `equal?` against the
;; bundle's string-valued artifact names — plus symbol values for the two
;; enum fields `subject-binding` and `claim-gate-class`. JSON has only
;; strings, so the adapter translates exactly those positions. This is a
;; faithful encoding of the validator's request contract, not a weakening:
;; unknown enum strings stay unknown and fail closed inside the validator.
(define (normalize-request request)
  (define (as-symbol v)
    (if (string? v)
        (string->symbol v)
        v))
  (define (keys->strings h)
    (for/hash ([(k v) (in-hash h)])
      (values (if (symbol? k)
                  (symbol->string k)
                  k)
              v)))
  (define overrides
    (hasheq 'subject-binding
            (as-symbol (hash-ref request 'subject-binding #f))
            'claim-gate-class
            (as-symbol (hash-ref request 'claim-gate-class #f))
            'expected-artifacts
            (keys->strings (hash-ref request 'expected-artifacts (hasheq)))))
  (for/hash ([(k v) (in-hash request)])
    (values k (hash-ref overrides k v))))

;; ---------------------------------------------------------------------------
;; q.reuse-decision/1 record assembly (validator step 15 + §11.3 fields)
;; ---------------------------------------------------------------------------

;; Extends the validator's deterministic record with the §11.3 no-silent-
;; fallback fields and the adapter's decision-time evidence. `bundle` is the
;; bundle TEXT (or a parsed hash) for content addressing; the empty string
;; (unreadable bundle) records bundle_id "unknown".
(define (augmented-decision-record bundle request decision)
  (define base (reuse-decision-record bundle request decision))
  (define reusable? (eq? decision 'reusable))
  (define reason (hash-ref base 'reason #f))
  (hash-set
   (hash-set
    (hash-set (hash-set (hash-set (hash-set (hash-set (hash-set base
                                                                'consumer-id
                                                                (hash-ref request 'consumer-id #f))
                                                      'source-sha
                                                      (hash-ref request 'expected-commit-sha #f))
                                            'affected-claim-set
                                            (hash-ref request 'required-claim-ids '()))
                                  'zero-tests-run
                                  reusable?)
                        'normal-proof-executed
                        (not reusable?))
              'fallback-cause
              (cond
                [reusable? 'none]
                [reason reason]
                [else 'consume-not-run]))
    'request-current-time
    (hash-ref request 'current-time #f))
   'decided-at
   (seconds->rfc3339-z (current-seconds))))

;; ---------------------------------------------------------------------------
;; `write` subcommand
;; ---------------------------------------------------------------------------

(define (required-doc-key doc key what)
  (define v (hash-ref doc key #f))
  (unless v
    (error 'write-command "claims document is missing ~a (required for ~a)" key what))
  v)

(define (section-or-error doc key)
  (define v (hash-ref doc key #f))
  (unless (hash? v)
    (error 'write-command "claims document section ~a must be an object" key))
  v)

;; Assembles the full q.proof-bundle/1 spec (§5.2–§5.16) from the claims
;; document and the scalar producer arguments, then finalizes it through the
;; canonical writer (which fails closed on any incomplete bundle).
(define (assemble-bundle-spec doc
                              #:producer-identity producer-identity
                              #:workflow-revision-sha workflow-revision-sha
                              #:commit-sha commit-sha
                              #:tree-sha tree-sha
                              #:repository repository
                              #:retention-days retention-days)
  (define created-at (required-doc-key doc 'created_at "§5.2 created_at"))
  (define environment-class (required-doc-key doc 'environment_class "§5.16 compatibility"))
  (define attestation-digest (required-doc-key doc 'attestation_digest "§5.13 provenance"))
  (define allowed-consumers (required-doc-key doc 'allowed_consumers "§5.15 authorization"))
  (define claims (required-doc-key doc 'claims "§5.5 claims"))
  (unless (and (list? claims) (pair? claims))
    (error 'write-command "claims document claims must be a non-empty list"))
  (define producer-doc (section-or-error doc 'producer))
  (define retention-id
    (or (hash-ref doc 'retention_policy_id #f)
        (string-append "q.proof-bundle" ".1:retention-" (number->string retention-days) "d")))
  (define created-seconds
    (or (rfc3339-z->seconds created-at)
        (error 'write-command "created_at must be an RFC3339 UTC timestamp: ~s" created-at)))
  (define retain-until (seconds->rfc3339-z (+ created-seconds (* 86400 retention-days))))
  (finalize-proof-bundle
   (hasheq 'schema
           "q.proof-bundle/1"
           'bundle_id
           ""
           'created_at
           created-at
           'producer
           (hash-set* producer-doc
                      'workflow_path
                      (required-doc-key doc 'workflow_path "§5.3 workflow_path")
                      'repository
                      repository
                      'workflow_revision_sha
                      workflow-revision-sha)
           'subject
           (hasheq 'subject_mode
                   (or (hash-ref doc 'subject_mode #f) "commit")
                   'commit_sha
                   commit-sha
                   'tree_sha
                   tree-sha
                   'dirty
                   (hash-ref doc 'subject_dirty #f))
           'claims
           claims
           'selection
           (section-or-error doc 'selection)
           'command
           (section-or-error doc 'command)
           'environment
           (section-or-error doc 'environment)
           'policy
           (section-or-error doc 'policy)
           'prepared_environment
           (section-or-error doc 'prepared_environment)
           'result
           (section-or-error doc 'result)
           'artifacts
           (required-doc-key doc 'artifacts "§5.12 artifacts")
           'provenance
           (hasheq 'attestation_format
                   (or (hash-ref doc 'attestation_format #f) "q.attestation/1")
                   'attestation_digest
                   attestation-digest
                   'producer_identity
                   producer-identity
                   'source_repository
                   repository
                   'source_workflow_revision_sha
                   workflow-revision-sha
                   'verification_method
                   "repo-owned-validator"
                   'verification_key_or_identity
                   (or (hash-ref doc 'verification_key_or_identity #f)
                       "policy:q/release-evidence-v1"))
           'retention
           (hasheq 'policy_id
                   retention-id
                   'created_at
                   created-at
                   'retain_until
                   retain-until
                   'immutable
                   #t
                   'store
                   "evidence-store"
                   'retention_verified_at
                   created-at)
           'authorization
           (hasheq 'allowed_consumers
                   allowed-consumers
                   'denied_consumers
                   (hash-ref doc 'denied_consumers '())
                   'release_reusable
                   (hash-ref doc 'release_reusable #f))
           'compatibility
           (hasheq 'compatibility_policy_version
                   1
                   'environment_class
                   environment-class
                   'satisfies_classes
                   (or (hash-ref doc 'satisfies_classes #f) (list environment-class))
                   'subject_scope
                   "exact-commit"
                   'notes
                   (or (hash-ref doc 'compatibility_notes #f)
                       "q.proof-bundle spec 5.16; no implicit close-enough comparison")))))

(define (write-command args)
  (define claims-path (flag-lookup args "--claims-json"))
  (define producer-identity (flag-lookup args "--producer-identity"))
  (define workflow-revision-sha (flag-lookup args "--workflow-revision-sha"))
  (define commit-sha (flag-lookup args "--commit-sha"))
  (define tree-sha (flag-lookup args "--tree-sha"))
  (define repository (flag-lookup args "--repository"))
  (define out (flag-lookup args "--out"))
  (define retention-days
    (cond
      [(flag-lookup args "--retention-days")
       =>
       string->number]
      [else 14]))
  ;; Usage guards abort with exit 2: a partially-understood invocation must
  ;; never fall through into assembly.
  (cond
    [(not
      (and claims-path producer-identity workflow-revision-sha commit-sha tree-sha repository out))
     (usage-error "write"
                  (string-append
                   "requires --claims-json F --producer-identity S --workflow-revision-sha S"
                   " --commit-sha S --tree-sha S --repository S --out F [--retention-days N]"))]
    [(not (and retention-days (exact-positive-integer? retention-days)))
     (usage-error "write" "--retention-days must be a positive integer")]
    [else
     (define doc (read-json-file claims-path "claims document"))
     (unless (hash? doc)
       (usage-error "write"
                    (format "claims document ~a is unreadable or not a JSON object" claims-path)))
     (with-handlers ([exn:fail?
                      (lambda (e)
                        (fprintf (current-error-port)
                                 "consume.rkt write: refusing to write an unprovable bundle: ~a~n"
                                 (exn-message e))
                        1)])
       (define finalized
         (assemble-bundle-spec doc
                               #:producer-identity producer-identity
                               #:workflow-revision-sha workflow-revision-sha
                               #:commit-sha commit-sha
                               #:tree-sha tree-sha
                               #:repository repository
                               #:retention-days retention-days))
       (write-proof-bundle! finalized #:path out)
       (displayln (hash-ref finalized 'bundle_id))
       0)]))

;; ---------------------------------------------------------------------------
;; `consume` subcommand
;; ---------------------------------------------------------------------------

(define (consume-command args)
  (define bundle (flag-lookup args "--bundle"))
  (define request-path (flag-lookup args "--request-json"))
  (define out (flag-lookup args "--out"))
  (cond
    [(not (and bundle request-path out))
     (usage-error "consume" "requires --bundle F --request-json F --out F")]
    [else
     (define raw-request (read-json-file request-path "consumer request"))
     (unless (hash? raw-request)
       ;; An unreadable consumer request can never authorize a skip: record a
       ;; fail-closed invalid decision and exit 4.
       (define record
         (augmented-decision-record "" (hasheq) (list 'invalid 'invalid:unreadable-consumer-request)))
       (write-json-file out record)
       (printf "consume: decision=invalid reason=invalid:unreadable-consumer-request exit=4~n"))
     (if (hash? raw-request)
         (consume-bundle! bundle (normalize-request raw-request) out)
         4)]))

;; Validation core of the consume subcommand (request already normalized).
(define (consume-bundle! bundle request out)
  (define-values (decision bundle-text)
    (with-handlers ([exn:fail? (lambda (_) (values (list 'invalid 'invalid:unreadable-bundle) ""))])
      (define text
        (with-handlers ([exn:fail? (lambda (_) "")])
          (file->string bundle)))
      (values (with-handlers ([exn:fail? (lambda (_) (list 'invalid 'invalid:unreadable-bundle))])
                (validate-proof-bundle-file bundle request))
              text)))
  (define record (augmented-decision-record bundle-text request decision))
  (write-json-file out record)
  (define code (decision-exit-code decision))
  (printf "consume: decision=~a reason=~a exit=~a~n"
          (if (pair? decision)
              (car decision)
              decision)
          (if (pair? decision)
              (cadr decision)
              "none")
          code)
  code)

;; ---------------------------------------------------------------------------
;; CLI dispatch (direct invocation only; module top level stays pure so
;; `raco test` and dynamic-require have no side effects)
;; ---------------------------------------------------------------------------

(define (usage)
  (displayln
   "usage: consume.rkt write --claims-json F --producer-identity S --workflow-revision-sha S")
  (displayln
   "                      --commit-sha S --tree-sha S --repository S --out F [--retention-days N]")
  (displayln "       consume.rkt consume --bundle F --request-json F --out F")
  (displayln "exit codes (consume): 0 reusable, 3 not-reusable, 4 invalid"))

(module+ main
  (define argv (vector->list (current-command-line-arguments)))
  (cond
    [(null? argv)
     (usage)
     (exit 2)]
    [(string=? (car argv) "write") (exit (write-command (cdr argv)))]
    [(string=? (car argv) "consume") (exit (consume-command (cdr argv)))]
    [else
     (usage)
     (exit 2)]))
