#lang racket/base

;; q/scripts/proof-bundle/bundle-validator.rkt — read-only, fail-closed
;; consumer validation for q.proof-bundle/1 (v1.00.29 W5, spec §5.17)
;;
;; Implements the 15-step consumer validation algorithm:
;;   1. parse canonical bundle            9.  environment compatibility
;;   2. verify schema                    10.  policy/profile compatibility
;;   3. verify bundle digest             11.  result completeness + pass
;;   4. provenance / producer identity   12.  every referenced artifact digest
;;   5. repository/workflow revision     13.  retention horizon + immutability
;;   6. exact subject identity           14.  consumer authorization
;;   7. claim set                        15.  retained reuse-decision record
;;   8. selection/inventory compatibility
;;
;; FAIL-CLOSED: every unknown, missing or mismatched field aborts with a
;; named reason. Decisions are:
;;   'reusable
;;   (list 'not-reusable 'not-reusable:<reason>)   authentic, but incompatible
;;                                                 with the consumer's stated
;;                                                 need (policy/compatibility)
;;   (list 'invalid     'invalid:<reason>)         corrupt / inauthentic /
;;                                                 incomplete (integrity)
;; A missing field is never compatible: missing fields are `invalid`, never
;; `not-reusable`. Every step is a short-circuit or-chain: the FIRST violated
;; check aborts, so no violation can be silently discarded.
;;
;; §5.13: producer identity and the immutable workflow revision are verified
;; against the consumer's expected identity — never against checksums alone.
;; Every fixture in tests/fixtures/proof-bundle/ ships a SHA256SUMS file that
;; is CONSISTENT with its (possibly corrupted) bundle.json, so no rejection
;; can be explained by checksums: reasons come from the semantic steps, which
;; proves there is no checksum-only acceptance path.
;;
;; Read-only: never writes files. No network, threads or sleeps.
;; Dependencies: racket/base plus json, racket/contract, racket/file,
;; racket/string, racket/date (`json` under its installed collection name).

(require json
         racket/contract
         racket/date
         racket/file
         racket/string)

;; Shared digest/canonicalization from the canonical writer — consumers and
;; producers must never disagree on how bundle_id is derived.
(require (only-in "bundle-writer.rkt" canonical-json proof-bundle-id))

(define decision/c (or/c 'reusable (list/c 'not-reusable symbol?) (list/c 'invalid symbol?)))

(provide (contract-out (validate-proof-bundle (-> (or/c string? hash?) hash? decision/c))
                       (validate-proof-bundle-file (-> path-string? hash? decision/c))
                       (consumer-request-errors (-> hash? (listof string?)))
                       (reuse-decision-record (-> (or/c string? hash?) hash? decision/c hash?))
                       (proof-bundle-id (-> jsexpr? string?))
                       (canonical-json (-> jsexpr? string?))))

;; ---------------------------------------------------------------------------
;; Decision helpers
;; ---------------------------------------------------------------------------

(define (invalid reason)
  (list 'invalid reason))
(define (not-reusable reason)
  (list 'not-reusable reason))

;; ---------------------------------------------------------------------------
;; Consumer request contract (fail-closed on an incomplete request too)
;; ---------------------------------------------------------------------------

(define required-request-keys
  (list 'expected-repository
        'expected-producer-identity
        'expected-workflow-revision-sha
        'expected-commit-sha
        'expected-tree-sha
        'subject-binding
        'expected-environment
        'expected-security-profile
        'expected-test-profile
        'expected-sandbox-profile
        'expected-feature-flags-digest
        'expected-relevant-environment-digest
        'expected-selection-digests
        'expected-artifacts
        'expected-result-summary-digest
        'expected-prepared-env-artifact-digest
        'current-time
        'minimum-retention-until
        'consumer-id
        'consumer-mode
        'required-claim-ids
        'claim-gate-class
        'expected-run-attempt))

(define (consumer-request-errors request)
  (for/list ([key required-request-keys]
             #:unless (hash-has-key? request key))
    (format "missing-request-field:~a" key)))

;; ---------------------------------------------------------------------------
;; Canonical form, parsing, format helpers
;; ---------------------------------------------------------------------------

(define proof-bundle-schema "q.proof-bundle/1")

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

;; Returns (values parsed-or-#f canonical-ok?).
(define (parse-bundle-text text)
  (define trimmed
    (if (string-suffix? text "\n")
        (substring text 0 (- (string-length text) 1))
        text))
  (define parsed
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (read-json (open-input-string trimmed))))
  (cond
    [(not (hash? parsed)) (values #f #f)]
    [else
     (define symbolized (json->symbols parsed))
     (values symbolized (string=? trimmed (canonical-json symbolized)))]))

;; Strict RFC3339 UTC ("...Z") timestamps; anything else fails closed.
(define rfc3339-z-rx #px"^([0-9]{4})-([0-9]{2})-([0-9]{2})T([0-9]{2}):([0-9]{2}):([0-9]{2})Z$")

(define (timestamp-seconds s)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define m (and (string? s) (regexp-match rfc3339-z-rx s)))
    (unless m
      (error 'timestamp "malformed RFC3339 UTC timestamp: ~s" s))
    (define (n i)
      (string->number (list-ref m i)))
    (find-seconds (n 6) (n 5) (n 4) (n 3) (n 2) (n 1) #t)))

(define digest-rx #px"^sha256:[0-9a-f]{64}$")
(define sha40-rx #px"^[0-9a-f]{40}$")

(define (digest-ok? v)
  (and (string? v) (regexp-match? digest-rx v)))
(define (sha40-ok? v)
  (and (string? v) (regexp-match? sha40-rx v)))

;; ---------------------------------------------------------------------------
;; Closed shape tables (§5.2–§5.16; §5.1 "fail-closed on unknown")
;; ---------------------------------------------------------------------------

(define required-bundle-fields
  (list 'schema
        'bundle_id
        'created_at
        'producer
        'subject
        'claims
        'selection
        'command
        'environment
        'policy
        'prepared_environment
        'result
        'artifacts
        'provenance
        'retention
        'authorization
        'compatibility))

(define section-field-table
  (hash
   'producer
   '(repository workflow_path
                workflow_revision_sha
                run_id
                run_attempt
                job_id
                job_name
                event
                ref
                trust_tier)
   'subject
   '(subject_mode commit_sha tree_sha dirty)
   'selection
   '(inventory_digest metadata_manifest_digest
                      covers_manifest_digest
                      selected_manifest_digest
                      selected_count
                      selection_mode
                      selector_revision_sha
                      explanation_artifact_digest)
   'command
   '(argv cwd_contract flags_digest runner_revision_sha)
   'environment
   '(os os_version
        runner_image
        architecture
        racket_version
        racket_executable_digest
        racket_installer_digest
        package_lock_digest
        resolved_package_set_digest
        locale
        timezone)
   'policy
   '(test_profile security_profile sandbox_profile feature_flags_digest relevant_environment_digest)
   'prepared_environment
   '(mode artifact_digest build_recipe_digest verification_status compiled_cache_digest)
   'result
   '(status started_at
            completed_at
            elapsed_ms
            tests_passed
            tests_failed
            tests_skipped
            allowed_skip_reasons
            timeouts
            cancelled
            retry_count
            result_summary_digest)
   'provenance
   '(attestation_format attestation_digest
                        producer_identity
                        source_repository
                        source_workflow_revision_sha
                        verification_method
                        verification_key_or_identity)
   'retention
   '(policy_id created_at retain_until immutable store retention_verified_at)
   'authorization
   '(allowed_consumers denied_consumers release_reusable)
   'compatibility
   '(compatibility_policy_version environment_class satisfies_classes subject_scope notes)))

;; Each claim requires these keys (spec §5.5). `gate_class` is the repo's W0
;; claim-metadata extension (required-vs-observational status; §9 case 19:
;; an observational proof is never reusable by a required gate). Only `pass`
;; claims are reusable.
(define required-claim-fields
  '(claim_id claim_version proof_class required_environment_class result gate_class))

(define required-artifact-fields '(name media_type digest size_bytes store store_object_id))

;; Step 2: schema + closed shape (first error wins, named, fail-closed).
(define (shape-error bundle)
  (or (for/or ([k (in-hash-keys bundle)]
               #:unless (memq k required-bundle-fields))
        (format "unknown-field:~a" k))
      (for/or ([f required-bundle-fields]
               #:unless (hash-has-key? bundle f))
        (format "missing-field:~a" f))
      (for/or ([(section subs) (in-hash section-field-table)])
        (define sect (hash-ref bundle section #f))
        (cond
          [(not (hash? sect)) (format "malformed-section:~a" section)]
          [else
           (or (for/or ([sub subs]
                        #:unless (hash-has-key? sect sub))
                 (format "missing-field:~a.~a" section sub))
               (for/or ([k (in-hash-keys sect)]
                        #:unless (memq k subs))
                 (format "unknown-field:~a.~a" section k))
               #f)]))
      (let ([claims (hash-ref bundle 'claims #f)])
        (cond
          [(not (and (list? claims) (pair? claims))) "malformed-section:claims"]
          [else
           (or (for/or ([claim claims]
                        [i (in-naturals)]
                        #:unless (and (hash? claim)
                                      (for/and ([sub required-claim-fields])
                                        (hash-has-key? claim sub))))
                 (format "missing-field:claims.~a" i))
               (for/or ([claim claims]
                        [i (in-naturals)]
                        #:when #t
                        [k (in-hash-keys claim)]
                        #:unless (memq k required-claim-fields))
                 (format "unknown-field:claims.~a.~a" i k))
               #f)]))
      (let ([artifacts (hash-ref bundle 'artifacts #f)])
        (cond
          [(not (list? artifacts)) "malformed-section:artifacts"]
          [else
           (or (for/or ([art artifacts]
                        [i (in-naturals)]
                        #:unless (and (hash? art)
                                      (for/and ([sub required-artifact-fields])
                                        (hash-has-key? art sub))))
                 (format "missing-field:artifacts.~a" i))
               (for/or ([art artifacts]
                        [i (in-naturals)]
                        #:when #t
                        [k (in-hash-keys art)]
                        #:unless (memq k required-artifact-fields))
                 (format "unknown-field:artifacts.~a.~a" i k))
               #f)]))))

;; ---------------------------------------------------------------------------
;; Steps 3–14. Each step returns #f (continue) or a decision (abort). Every
;; step is a short-circuit or-chain: the first violated check aborts.
;; ---------------------------------------------------------------------------

;; Step 3: verify bundle digest (content address, §5.2).
(define (id-and-shape-step bundle request)
  (define stated (hash-ref bundle 'bundle_id #f))
  (or
   (and (not (and (string? stated) (string-prefix? stated "sha256:") (= 71 (string-length stated))))
        (invalid 'invalid:malformed-bundle-id))
   (and (not (string=? stated (proof-bundle-id bundle))) (invalid 'invalid:bundle-id-mismatch))))

;; Step 4: provenance / producer identity (§5.13) — never checksum-only.
(define (identity-step bundle request)
  (define provenance (hash-ref bundle 'provenance))
  (or (and (not (equal? (hash-ref provenance 'producer_identity #f)
                        (hash-ref request 'expected-producer-identity)))
           (invalid 'invalid:producer-identity-mismatch))
      (and (not (equal? (hash-ref provenance 'verification_method #f) "repo-owned-validator"))
           (invalid 'invalid:unknown-verification-method))
      (and (not (digest-ok? (hash-ref provenance 'attestation_digest #f)))
           (invalid 'invalid:malformed-digest:provenance.attestation_digest))))

;; Step 5: repository + immutable workflow revision policy (§5.3, §5.13) and
;; producer-run freshness (§9 case 20: stale bundle from an earlier attempt).
(define (workflow-step bundle request)
  (define producer (hash-ref bundle 'producer))
  (define provenance (hash-ref bundle 'provenance))
  (define attempt (hash-ref producer 'run_attempt #f))
  (or (and (not (equal? (hash-ref producer 'repository #f) (hash-ref request 'expected-repository)))
           (invalid 'invalid:producer-repository-mismatch))
      (and (not (equal? (hash-ref provenance 'source_repository #f)
                        (hash-ref request 'expected-repository)))
           (invalid 'invalid:source-repository-mismatch))
      ;; A moving branch name is insufficient provenance (§5.3).
      (and (not (sha40-ok? (hash-ref producer 'workflow_revision_sha #f)))
           (invalid 'invalid:moving-ref-provenance))
      (and (not (equal? (hash-ref producer 'workflow_revision_sha #f)
                        (hash-ref provenance 'source_workflow_revision_sha #f)))
           (invalid 'invalid:workflow-revision-divergence))
      (and (not (equal? (hash-ref producer 'workflow_revision_sha #f)
                        (hash-ref request 'expected-workflow-revision-sha)))
           (not-reusable 'not-reusable:workflow-revision-mismatch))
      (and (not (exact-integer? attempt)) (invalid 'invalid:malformed-run-attempt))
      (and (< attempt (hash-ref request 'expected-run-attempt))
           (not-reusable 'not-reusable:stale-attempt))
      (and (> attempt (hash-ref request 'expected-run-attempt))
           (not-reusable 'not-reusable:run-attempt-mismatch))))

;; Step 6: exact subject identity (§5.4, §9 case 7). Commit-bound consumers
;; reject tree-only substitution; dirty producer state is always rejected.
(define (subject-step bundle request)
  (define subject (hash-ref bundle 'subject))
  (or
   (and (not (eq? (hash-ref subject 'dirty #t) #f)) (invalid 'invalid:dirty-subject))
   (and (not (member (hash-ref subject 'subject_mode #f) '("commit" "tree")))
        (invalid 'invalid:unknown-subject-mode))
   (and (not (sha40-ok? (hash-ref subject 'commit_sha #f))) (invalid 'invalid:malformed-commit-sha))
   (and (not (sha40-ok? (hash-ref subject 'tree_sha #f))) (invalid 'invalid:malformed-tree-sha))
   (and (eq? (hash-ref request 'subject-binding) 'commit)
        (or (and (not (equal? (hash-ref subject 'subject_mode) "commit"))
                 (not-reusable 'not-reusable:tree-only-substitution))
            (and (not (equal? (hash-ref subject 'commit_sha) (hash-ref request 'expected-commit-sha)))
                 (not-reusable 'not-reusable:subject-mismatch))
            (and (not (equal? (hash-ref subject 'tree_sha) (hash-ref request 'expected-tree-sha)))
                 (not-reusable 'not-reusable:tree-mismatch))
            (and (not (equal? (hash-ref (hash-ref bundle 'compatibility) 'subject_scope #f)
                              "exact-commit"))
                 (not-reusable 'not-reusable:tree-only-substitution))
            #f))
   #f))

;; Step 7: claim set (§5.5). Only `pass` claims are reusable; requested claims
;; must be covered; an observational proof is never a required gate's proof.
(define (claims-step bundle request)
  (define claims (hash-ref bundle 'claims))
  (define present-ids
    (for/hash ([claim claims])
      (values (hash-ref claim 'claim_id) claim)))
  (or (for/or ([claim claims]
               [i (in-naturals)])
        (and (not (equal? (hash-ref claim 'result #f) "pass"))
             (invalid (string->symbol (format "invalid:claim-not-pass:~a" i)))))
      (for/or ([required (hash-ref request 'required-claim-ids)]
               #:unless (hash-has-key? present-ids required))
        (not-reusable 'not-reusable:claim-not-covered))
      (for/or ([claim claims])
        ;; JSON values are strings: normalize gate_class to a symbol.
        (define raw (hash-ref claim 'gate_class #f))
        (define gate-class
          (if (string? raw)
              (string->symbol raw)
              raw))
        (cond
          [(not gate-class) (not-reusable 'not-reusable:claim-gate-class-unknown)]
          [(not (member gate-class '(required observational)))
           (invalid 'invalid:unknown-claim-gate-class)]
          [(and (eq? (hash-ref request 'claim-gate-class) 'required) (not (eq? gate-class 'required)))
           (not-reusable 'not-reusable:observational-proof-for-required-gate)]
          [else #f]))))

;; Step 8: selection/inventory compatibility (§5.6). No reuse when the
;; consuming claim depends on an unproven inventory/manifest revision.
(define (selection-step bundle request)
  (define selection (hash-ref bundle 'selection))
  (define expected (hash-ref request 'expected-selection-digests))
  (for/or ([field '(selected_manifest_digest inventory_digest
                                             metadata_manifest_digest
                                             covers_manifest_digest)])
    (define value (hash-ref selection field #f))
    (cond
      [(not (digest-ok? value))
       (invalid (string->symbol (format "invalid:malformed-digest:selection.~a" field)))]
      [(not (equal? value (hash-ref expected field #f)))
       (not-reusable (case field
                       [(selected_manifest_digest) 'not-reusable:manifest-mismatch]
                       [(inventory_digest) 'not-reusable:inventory-mismatch]
                       [(metadata_manifest_digest) 'not-reusable:metadata-manifest-mismatch]
                       [else 'not-reusable:covers-mismatch]))]
      [else #f])))

;; Step 9: environment compatibility (§5.8) + prepared environment (§5.10).
;; Cross-version or cross-platform reuse is prohibited unless the claim
;; explicitly declares such compatibility; default: incompatible.
(define environment-field-order
  (list 'os
        'os_version
        'runner_image
        'architecture
        'racket_version
        'racket_executable_digest
        'racket_installer_digest
        'package_lock_digest
        'resolved_package_set_digest
        'locale
        'timezone))

(define (environment-step bundle request)
  (define environment (hash-ref bundle 'environment))
  (define expected (hash-ref request 'expected-environment))
  (or (for/or ([field environment-field-order])
        (unless (hash-has-key? expected field)
          (error 'environment-step "consumer request has no expectation for environment.~a" field))
        (and (not (equal? (hash-ref environment field #f) (hash-ref expected field)))
             (not-reusable (string->symbol (format "not-reusable:environment-mismatch:~a" field)))))
      (let ([prepared (hash-ref bundle 'prepared_environment)])
        (or (and (not (member (hash-ref prepared 'mode #f) '("cold" "restored")))
                 (invalid 'invalid:unknown-prepared-env-mode))
            (and (not (equal? (hash-ref prepared 'verification_status #f) "verified"))
                 (invalid 'invalid:prepared-env-unverified))
            (and (not (equal? (hash-ref prepared 'artifact_digest #f)
                              (hash-ref request 'expected-prepared-env-artifact-digest)))
                 (not-reusable 'not-reusable:prepared-env-mismatch))
            #f))))

;; Step 10: policy/profile compatibility (§5.9). Strict-security evidence is
;; never satisfied by ordinary-profile evidence.
(define security-profile-rank (hash "none" 0 "ordinary" 1 "strict" 2))

(define (policy-step bundle request)
  (define policy (hash-ref bundle 'policy))
  (define producer-rank (hash-ref security-profile-rank (hash-ref policy 'security_profile #f) #f))
  (define required-rank
    (hash-ref security-profile-rank (hash-ref request 'expected-security-profile) #f))
  (unless required-rank
    (error 'policy-step "unknown required security profile in request"))
  (or (and (not producer-rank) (invalid 'invalid:unknown-security-profile))
      (and (< producer-rank required-rank) (not-reusable 'not-reusable:security-profile-insufficient))
      (and (not (equal? (hash-ref policy 'security_profile)
                        (hash-ref request 'expected-security-profile)))
           (not-reusable 'not-reusable:security-profile-mismatch))
      (and (not (equal? (hash-ref policy 'test_profile) (hash-ref request 'expected-test-profile)))
           (not-reusable 'not-reusable:test-profile-mismatch))
      (and (not (equal? (hash-ref policy 'sandbox_profile)
                        (hash-ref request 'expected-sandbox-profile)))
           (not-reusable 'not-reusable:sandbox-profile-mismatch))
      (and (not (equal? (hash-ref policy 'feature_flags_digest)
                        (hash-ref request 'expected-feature-flags-digest)))
           (not-reusable 'not-reusable:profile-digest-mismatch))
      (and (not (equal? (hash-ref policy 'relevant_environment_digest)
                        (hash-ref request 'expected-relevant-environment-digest)))
           (not-reusable 'not-reusable:profile-digest-mismatch))))

;; Step 11: result completeness + `pass` (§5.11). A cancelled, timed-out,
;; incomplete, malformed, unexpectedly skipped, or failed producer bundle is
;; never reusable.
(define (result-step bundle request)
  (define result (hash-ref bundle 'result))
  (or (and (not (eq? (hash-ref result 'cancelled #t) #f))
           (not-reusable 'not-reusable:producer-cancelled))
      (and (not (equal? (hash-ref result 'timeouts #f) 0))
           (not-reusable 'not-reusable:producer-timeout))
      (and (not (equal? (hash-ref result 'status #f) "pass"))
           (not-reusable 'not-reusable:producer-result-not-pass))
      (and (not (equal? (hash-ref result 'tests_failed #f) 0))
           (not-reusable 'not-reusable:producer-tests-failed))
      (and (> (hash-ref result 'tests_skipped 0) 0)
           (null? (hash-ref result 'allowed_skip_reasons '()))
           (not-reusable 'not-reusable:unexpected-skip))
      (and (not (and (timestamp-seconds (hash-ref result 'started_at #f))
                     (timestamp-seconds (hash-ref result 'completed_at #f))))
           (invalid 'invalid:malformed-timestamp))
      (and (not (and (exact-integer? (hash-ref result 'elapsed_ms #f))
                     (>= (hash-ref result 'elapsed_ms) 0)))
           (invalid 'invalid:malformed-result))
      (and (not (digest-ok? (hash-ref result 'result_summary_digest #f)))
           (invalid 'invalid:malformed-digest:result.result_summary_digest))
      (and (not (equal? (hash-ref result 'result_summary_digest)
                        (hash-ref request 'expected-result-summary-digest)))
           (not-reusable 'not-reusable:result-summary-mismatch))))

;; Step 12: verify EVERY referenced artifact digest (§5.12) against the
;; consumer's independently computed expectations (not the bundle's claims).
(define (artifacts-step bundle request)
  (define expected (hash-ref request 'expected-artifacts))
  (define referenced
    (for/hash ([art (hash-ref bundle 'artifacts)])
      (values (hash-ref art 'name) art)))
  (or (for/or ([art (hash-ref bundle 'artifacts)]
               [i (in-naturals)])
        (define name (hash-ref art 'name #f))
        (cond
          [(not (hash-has-key? expected name))
           (invalid (string->symbol (format "invalid:unknown-artifact:~a" name)))]
          [(not (equal? (hash-ref art 'digest) (hash-ref expected name)))
           (invalid (string->symbol (format "invalid:artifact-digest-mismatch:~a" name)))]
          [else #f]))
      (for/or ([(name _) (in-hash expected)]
               #:unless (hash-has-key? referenced name))
        (invalid (string->symbol (format "invalid:stale-artifact-expectation:~a" name))))))

;; Step 13: retention horizon + immutability (§5.14). Rejects expired
;; evidence, too-short horizons and mutable/unverified stores.
(define (retention-step bundle request)
  (define retention (hash-ref bundle 'retention))
  (define retain-until (timestamp-seconds (hash-ref retention 'retain_until #f)))
  (define verified-at (timestamp-seconds (hash-ref retention 'retention_verified_at #f)))
  (define now (timestamp-seconds (hash-ref request 'current-time)))
  (define minimum (timestamp-seconds (hash-ref request 'minimum-retention-until)))
  (or (and (not (eq? (hash-ref retention 'immutable #f) #t))
           (invalid 'invalid:retention-not-immutable))
      (and (not (equal? (hash-ref retention 'store #f) "evidence-store"))
           (not-reusable 'not-reusable:store-not-immutable))
      (and (not (and retain-until verified-at now minimum)) (invalid 'invalid:malformed-timestamp))
      (and (< retain-until now) (not-reusable 'not-reusable:retention-expired))
      (and (< retain-until minimum) (not-reusable 'not-reusable:retention-shortfall))
      (and (> verified-at now) (invalid 'invalid:retention-verified-in-future))))

;; Step 14: consumer authorization (§5.15). Release reuse is opt-in per
;; claim; the default is false.
(define (authorization-step bundle request)
  (define authorization (hash-ref bundle 'authorization))
  (define consumer-id (hash-ref request 'consumer-id))
  (or (and (equal? (hash-ref request 'consumer-mode) "release")
           (not (eq? (hash-ref authorization 'release_reusable #f) #t))
           (not-reusable 'not-reusable:release-reuse-not-permitted))
      (and (member consumer-id (hash-ref authorization 'denied_consumers '()))
           (not-reusable 'not-reusable:consumer-denied))
      (and (not (member consumer-id (hash-ref authorization 'allowed_consumers '())))
           (not-reusable 'not-reusable:consumer-not-authorized))))

;; The ordered pipeline: §5.17 steps 3–14 (steps 1, 2 and 15 wrap them).
(define validation-steps
  (list id-and-shape-step ; step 3
        identity-step ; step 4
        workflow-step ; step 5
        subject-step ; step 6
        claims-step ; step 7
        selection-step ; step 8
        environment-step ; step 9
        policy-step ; step 10
        result-step ; step 11
        artifacts-step ; step 12
        retention-step ; step 13
        authorization-step)) ; step 14

(define (first-decision steps bundle request)
  (let loop ([steps steps])
    (cond
      [(null? steps) #f]
      [else
       (define decision ((car steps) bundle request))
       (if decision
           decision
           (loop (cdr steps)))])))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(define (validate-proof-bundle bundle request)
  ;; Fail-closed on an incomplete consumer request (short-circuits before any
  ;; bundle processing).
  (if (not (null? (consumer-request-errors request)))
      (invalid 'invalid:incomplete-consumer-request)
      (let-values ([(parsed canonical-ok?) (cond
                                             [(string? bundle) (parse-bundle-text bundle)]
                                             [(hash? bundle) (values (json->symbols bundle) #t)]
                                             [else (values #f #f)])])
        (cond
          [(not parsed) (invalid 'invalid:unparseable-bundle)]
          [(and (string? bundle) (not canonical-ok?)) (invalid 'invalid:non-canonical-encoding)]
          ;; Step 2: schema + closed shape.
          [(not (equal? (hash-ref parsed 'schema #f) proof-bundle-schema))
           (invalid 'invalid:schema-mismatch)]
          [(shape-error parsed)
           =>
           (lambda (e) (invalid (string->symbol (format "invalid:~a" e))))]
          ;; Steps 3–14, then step 15: 'reusable (the retained record is
          ;; emitted separately by reuse-decision-record so validation stays
          ;; pure).
          [else (or (first-decision validation-steps parsed request) 'reusable)]))))

(define (validate-proof-bundle-file path request)
  (validate-proof-bundle (file->string path) request))

;; Step 15: emit a retained reuse-decision record (§5.17; §11.3
;; no-silent-fallback). Deterministic content: no wall-clock fields.
(define (reuse-decision-record bundle request decision)
  (define bundle-id
    (cond
      [(string? bundle)
       (define-values (parsed _) (parse-bundle-text bundle))
       (if parsed
           (proof-bundle-id parsed)
           "unknown")]
      [(hash? bundle) (proof-bundle-id (json->symbols bundle))]
      [else "unknown"]))
  (hasheq 'schema
          "q.reuse-decision/1"
          'decision
          (if (pair? decision)
              (car decision)
              decision)
          'reason
          (and (pair? decision) (cadr decision))
          'bundle_id
          bundle-id
          'consumer_id
          (hash-ref request 'consumer-id #f)
          'consumer_mode
          (hash-ref request 'consumer-mode #f)
          'claim_ids
          (hash-ref request 'required-claim-ids '())
          'subject_commit_sha
          (hash-ref request 'expected-commit-sha #f)
          'validated_steps
          15))
