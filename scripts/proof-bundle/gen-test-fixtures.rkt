#lang racket/base

;; q/scripts/proof-bundle/gen-test-fixtures.rkt — v1.00.29 W5 one-off
;;
;; Deterministically regenerates every fixture under
;; tests/fixtures/proof-bundle/: one VALID base bundle (valid-base/) plus one
;; fixture directory per unsafe-reuse case from bundle spec §9 (all twenty).
;;
;; Construction discipline (this is what makes the fixtures probative):
;;   - The base bundle is written by the CANONICAL writer
;;     (write-proof-bundle!) and therefore passes the producer completeness
;;     gate and carries a content-addressed bundle_id by construction.
;;   - Each threat-model case is a MINIMAL, NAMED mutation of the finalized
;;     base (deep path set/remove), after which bundle_id is RECOMPUTED so the
;;     mutated bundle is internally consistent — exactly what a producer-side
;;     bug or a consistent forgery looks like. Only the semantic validation
;;     steps can catch these; checksums cannot.
;;   - Every fixture directory's SHA256SUMS covers its own (possibly
;;     corrupted) bundle.json and is always consistent, so no validator
;;     rejection can be explained by checksums: there is no checksum-only
;;     acceptance path (§5.13).
;;   - All values are compile-time literals; regeneration is byte-identical
;;     (verified by tests/test-proof-bundle-writer.rkt).
;;
;; raco test runs every .rkt in this directory at module load; side effects
;; are behind the direct-invocation guard (same pattern as
;; w3-seed-flake-bundles.rkt) — raco test must pass.
;;
;; No sleeps, no threads, no network.

;; Public surface for the test files: base-proof-bundle (the valid base
;; spec), fixture-cases (the named mutation table) and regenerate!.
(provide base-proof-bundle
         fixture-cases
         regenerate!)

(require racket/file
         racket/string
         (only-in "bundle-writer.rkt"
                  finalize-proof-bundle
                  proof-bundle-id
                  proof-bundle->string
                  sha256-hex))

;; ---------------------------------------------------------------------------
;; Deterministic constants (mirrored by the consumer request in
;; tests/test-proof-bundle-validator.rkt)
;; ---------------------------------------------------------------------------

(define workflow-revision-sha "3f7a1c2b9d4e5f60718293a4b5c6d7e8f90a1b2c")
(define workflow-revision-sha-changed "7e1d4c8f2a6b9e30f5a1c7d8b2e4f6a90c1d3e5b")
(define commit-sha "9c8b7a654321fedcba9876543210fedcba987654")
(define tree-sha "1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b")
(define runner-revision-sha "5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a")

(define digest-inventory "sha256:0101010101010101010101010101010101010101010101010101010101010101")
(define digest-metadata-manifest
  "sha256:0202020202020202020202020202020202020202020202020202020202020202")
(define digest-covers-manifest
  "sha256:0303030303030303030303030303030303030303030303030303030303030303")
(define digest-selected-manifest
  "sha256:0404040404040404040404040404040404040404040404040404040404040404")
(define digest-selected-manifest-changed
  "sha256:0404040404040404040404040404040404040404040404040404040404040405")
(define digest-command-flags
  "sha256:1111111111111111111111111111111111111111111111111111111111111111")
(define digest-racket-executable
  "sha256:1212121212121212121212121212121212121212121212121212121212121212")
(define digest-racket-installer
  "sha256:1313131313131313131313131313131313131313131313131313131313131313")
(define digest-package-lock "sha256:1414141414141414141414141414141414141414141414141414141414141414")
(define digest-package-lock-changed
  "sha256:1414141414141414141414141414141414141414141414141414141414141415")
(define digest-resolved-package-set
  "sha256:1515151515151515151515151515151515151515151515151515151515151515")
(define digest-feature-flags
  "sha256:1616161616161616161616161616161616161616161616161616161616161616")
(define digest-relevant-environment
  "sha256:1717171717171717171717171717171717171717171717171717171717171717")
(define digest-prepared-artifact
  "sha256:1818181818181818181818181818181818181818181818181818181818181818")
(define digest-prepared-artifact-changed
  "sha256:1818181818181818181818181818181818181818181818181818181818181819")
(define digest-prepared-build-recipe
  "sha256:1919191919191919191919191919191919191919191919191919191919191919")
(define digest-prepared-compiled-cache
  "sha256:1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a")
(define digest-result-summary
  "sha256:2020202020202020202020202020202020202020202020202020202020202020")
(define digest-result-summary-changed
  "sha256:2020202020202020202020202020202020202020202020202020202020202021")
(define digest-artifact-log "sha256:2121212121212121212121212121212121212121212121212121212121212121")
(define digest-artifact-log-changed
  "sha256:2121212121212121212121212121212121212121212121212121212121212122")
(define digest-attestation "sha256:2222222222222222222222222222222222222222222222222222222222222222")

;; ---------------------------------------------------------------------------
;; The VALID base bundle (§5.2–§5.16)
;; ---------------------------------------------------------------------------

(define (base-proof-bundle)
  (hasheq 'schema
          "q.proof-bundle/1"
          'bundle_id
          ""
          'created_at
          "2026-09-11T10:00:00Z"
          'producer
          (hasheq 'repository
                  "coinerd/q"
                  'workflow_path
                  ".github/workflows/ci.yml"
                  'workflow_revision_sha
                  workflow-revision-sha
                  'run_id
                  "run-1234567890"
                  'run_attempt
                  3
                  'job_id
                  "job-9876543210"
                  'job_name
                  "test / fast"
                  'event
                  "push"
                  'ref
                  "refs/heads/main"
                  'trust_tier
                  "tier-1")
          'subject
          (hasheq 'subject_mode "commit" 'commit_sha commit-sha 'tree_sha tree-sha 'dirty #f)
          'claims
          (list (hasheq 'claim_id
                        "claim:unit:fast-gate"
                        'claim_version
                        1
                        'proof_class
                        "unit"
                        'required_environment_class
                        "linux-racket-8.10-strict"
                        'gate_class
                        "required"
                        'result
                        "pass"))
          'selection
          (hasheq 'inventory_digest
                  digest-inventory
                  'metadata_manifest_digest
                  digest-metadata-manifest
                  'covers_manifest_digest
                  digest-covers-manifest
                  'selected_manifest_digest
                  digest-selected-manifest
                  'selected_count
                  12
                  'selection_mode
                  "explicit"
                  'selector_revision_sha
                  workflow-revision-sha
                  'explanation_artifact_digest
                  digest-artifact-log)
          'command
          (hasheq 'argv
                  (list "racket" "scripts/run-tests.rkt" "--suite" "fast")
                  'cwd_contract
                  "repo-root"
                  'flags_digest
                  digest-command-flags
                  'runner_revision_sha
                  runner-revision-sha)
          'environment
          (hasheq 'os
                  "linux"
                  'os_version
                  "24.04"
                  'runner_image
                  "ghcr.io/actions-runner/ubuntu-24.04@sha256:3d5f2f0a"
                  'architecture
                  "x86_64"
                  'racket_version
                  "8.10"
                  'racket_executable_digest
                  digest-racket-executable
                  'racket_installer_digest
                  digest-racket-installer
                  'package_lock_digest
                  digest-package-lock
                  'resolved_package_set_digest
                  digest-resolved-package-set
                  'locale
                  "C.UTF-8"
                  'timezone
                  "UTC")
          'policy
          (hasheq 'test_profile
                  "fast"
                  'security_profile
                  "strict"
                  'sandbox_profile
                  "sandboxed"
                  'feature_flags_digest
                  digest-feature-flags
                  'relevant_environment_digest
                  digest-relevant-environment)
          'prepared_environment
          (hasheq 'mode
                  "restored"
                  'artifact_digest
                  digest-prepared-artifact
                  'build_recipe_digest
                  digest-prepared-build-recipe
                  'verification_status
                  "verified"
                  'compiled_cache_digest
                  digest-prepared-compiled-cache)
          'result
          (hasheq 'status
                  "pass"
                  'started_at
                  "2026-09-11T10:00:00Z"
                  'completed_at
                  "2026-09-11T10:05:00Z"
                  'elapsed_ms
                  300000
                  'tests_passed
                  42
                  'tests_failed
                  0
                  'tests_skipped
                  0
                  'allowed_skip_reasons
                  (list)
                  'timeouts
                  0
                  'cancelled
                  #f
                  'retry_count
                  0
                  'result_summary_digest
                  digest-result-summary)
          'artifacts
          (list (hasheq 'name
                        "unit-tests-log"
                        'media_type
                        "text/plain"
                        'digest
                        digest-artifact-log
                        'size_bytes
                        1234
                        'store
                        "evidence-store"
                        'store_object_id
                        "obj-0001"))
          'provenance
          (hasheq 'attestation_format
                  "q.attestation/1"
                  'attestation_digest
                  digest-attestation
                  'producer_identity
                  "workflow:coinerd/q/.github/workflows/ci.yml"
                  'source_repository
                  "coinerd/q"
                  'source_workflow_revision_sha
                  workflow-revision-sha
                  'verification_method
                  "repo-owned-validator"
                  'verification_key_or_identity
                  "policy:q/release-evidence-v1")
          'retention
          (hasheq 'policy_id
                  "release-evidence-v1"
                  'created_at
                  "2026-09-11T10:00:00Z"
                  'retain_until
                  "2027-09-11T10:00:00Z"
                  'immutable
                  #t
                  'store
                  "evidence-store"
                  'retention_verified_at
                  "2026-09-11T10:05:00Z")
          'authorization
          (hasheq 'allowed_consumers
                  (list "gate:main-required")
                  'denied_consumers
                  (list)
                  'release_reusable
                  #f)
          'compatibility
          (hasheq 'compatibility_policy_version
                  1
                  'environment_class
                  "linux-racket-8.10-strict"
                  'satisfies_classes
                  (list "linux-racket-8.10-strict")
                  'subject_scope
                  "exact-commit"
                  'notes
                  "q.proof-bundle/1 spec 5.16; no implicit close-enough comparison")))

;; ---------------------------------------------------------------------------
;; Deep path mutation helpers (path elements: symbol keys / list indexes)
;; ---------------------------------------------------------------------------

(define (list-replace lst i value)
  (cond
    [(zero? i) (cons value (cdr lst))]
    [(null? lst) (error 'list-replace "index out of range")]
    [else (cons (car lst) (list-replace (cdr lst) (sub1 i) value))]))

(define (path-set v path value)
  (cond
    [(null? path) (error 'path-set "empty path")]
    [(and (null? (cdr path)) (hash? v)) (hash-set v (car path) value)]
    [(and (null? (cdr path)) (list? v)) (list-replace v (car path) value)]
    [(hash? v) (hash-set v (car path) (path-set (hash-ref v (car path)) (cdr path) value))]
    [(list? v) (list-replace v (car path) (path-set (list-ref v (car path)) (cdr path) value))]
    [else (error 'path-set "cannot set path ~s in ~s" path v)]))

(define (path-remove v path)
  (cond
    [(null? path) (error 'path-remove "empty path")]
    [(and (null? (cdr path)) (hash? v)) (hash-remove v (car path))]
    [(hash? v) (hash-set v (car path) (path-remove (hash-ref v (car path)) (cdr path)))]
    [else (error 'path-remove "cannot remove path ~s in ~s" path v)]))

;; Recomputes the content address after a mutation, so the bundle stays
;; internally consistent (a consistent forgery or a consistent producer bug).
(define (resign bundle)
  (hash-set bundle 'bundle_id (proof-bundle-id bundle)))

;; ---------------------------------------------------------------------------
;; The twenty §9 threat-model cases
;; ---------------------------------------------------------------------------

(define (fixture-cases)
  (define base (finalize-proof-bundle (base-proof-bundle)))
  (list
   (list "valid-base" (lambda (b) b))
   ;; 1. same commit, different Racket version
   (list "wrong-racket" (lambda (b) (resign (path-set b (list 'environment 'racket_version) "8.9"))))
   ;; 2. same commit, different OS/platform
   (list "wrong-os" (lambda (b) (resign (path-set b (list 'environment 'os) "macos"))))
   ;; 3. same commit, weaker security profile
   (list "weaker-security"
         (lambda (b) (resign (path-set b (list 'policy 'security_profile) "ordinary"))))
   ;; 4. same commit, changed package lock
   (list "changed-package-lock"
         (lambda (b)
           (resign
            (path-set b (list 'environment 'package_lock_digest) digest-package-lock-changed))))
   ;; 5. same commit, changed runner/workflow revision
   (list "changed-workflow-revision"
         (lambda (b)
           (resign (path-set
                    (path-set b (list 'producer 'workflow_revision_sha) workflow-revision-sha-changed)
                    (list 'provenance 'source_workflow_revision_sha)
                    workflow-revision-sha-changed))))
   ;; 6. same commit, changed selected manifest
   (list "changed-manifest"
         (lambda (b)
           (resign (path-set b
                             (list 'selection 'selected_manifest_digest)
                             digest-selected-manifest-changed))))
   ;; 7. same tree, different commit with commit-bound claim
   (list "tree-vs-commit"
         (lambda (b)
           (resign (path-set (path-set b (list 'subject 'subject_mode) "tree")
                             (list 'compatibility 'subject_scope)
                             "explicit-tree"))))
   ;; 8. expired bundle
   (list "expired"
         (lambda (b) (resign (path-set b (list 'retention 'retain_until) "2020-01-01T00:00:00Z"))))
   ;; 9. mutable/unverified evidence store
   (list "mutable-store"
         (lambda (b) (resign (path-set b (list 'retention 'store) "artifact-archive"))))
   ;; 10. forged/unknown producer identity
   (list "forged-identity"
         (lambda (b)
           (resign (path-set b
                             (list 'provenance 'producer_identity)
                             "workflow:unknown-fork/q/.github/workflows/ci.yml"))))
   ;; 11. artifact checksum mismatch (internally consistent; the consumer's
   ;; independently computed artifact digest disagrees)
   (list "checksum-mismatch"
         (lambda (b) (resign (path-set b (list 'artifacts 0 'digest) digest-artifact-log-changed))))
   ;; 12. result summary mismatch
   (list "result-mismatch"
         (lambda (b)
           (resign (path-set b (list 'result 'result_summary_digest) digest-result-summary-changed))))
   ;; 13. producer timeout/cancellation
   (list "timeout-cancel"
         (lambda (b)
           (resign
            (path-set (path-set b (list 'result 'cancelled) #t) (list 'result 'status) "cancelled"))))
   ;; 14. unexpected skip
   (list "unexpected-skip" (lambda (b) (resign (path-set b (list 'result 'tests_skipped) 3))))
   ;; 15. consumer not authorized
   (list "unauthorized-consumer"
         (lambda (b)
           (resign (path-set b (list 'authorization 'allowed_consumers) (list "gate:other")))))
   ;; 16. release consumer attempting to reuse release_reusable=false
   (list "release-reusable-false"
         (lambda (b)
           (resign
            (path-set b (list 'authorization 'allowed_consumers) (list "gate:release-required")))))
   ;; 17. prepared-env digest mismatch
   (list "prepared-env-digest"
         (lambda (b)
           (resign (path-set b
                             (list 'prepared_environment 'artifact_digest)
                             digest-prepared-artifact-changed))))
   ;; 18. missing field treated as compatible (FAIL-CLOSED: it must not be)
   (list "missing-field" (lambda (b) (resign (path-remove b (list 'selection)))))
   ;; 19. observational proof offered to required gate
   (list "observational-to-required"
         (lambda (b) (resign (path-set b (list 'claims 0 'gate_class) "observational"))))
   ;; 20. stale bundle from earlier run attempt
   (list "stale-attempt" (lambda (b) (resign (path-set b (list 'producer 'run_attempt) 2))))))

;; ---------------------------------------------------------------------------
;; Regeneration (deterministic; byte-identical output)
;; ---------------------------------------------------------------------------

(define (write-fixture! dir bundle)
  (make-directory* dir)
  (define text (proof-bundle->string bundle))
  (call-with-output-file (build-path dir "bundle.json")
                         (lambda (out) (display text out))
                         #:exists 'replace)
  (define sums (string-append (sha256-hex (string->bytes/utf-8 text)) "  bundle.json\n"))
  (call-with-output-file (build-path dir "SHA256SUMS")
                         (lambda (out) (display sums out))
                         #:exists 'replace))

;; Regenerates all fixture directories under TARGET (the repository fixture
;; root by default). Returns the list of written directory names.
(define (regenerate! [target #f])
  (define fixtures-root
    (or target
        (let-values ([(base _name _dir?) (split-path (resolved-module-path-name
                                                      (variable-reference->resolved-module-path
                                                       (#%variable-reference))))])
          (build-path base ".." ".." "tests" "fixtures" "proof-bundle"))))
  (define base (finalize-proof-bundle (base-proof-bundle)))
  (for/list ([case (fixture-cases)])
    (define name (car case))
    (define bundle ((cadr case) base))
    (write-fixture! (build-path fixtures-root name) bundle)
    name))

;; ---------------------------------------------------------------------------
;; Direct-invocation guard
;; ---------------------------------------------------------------------------

(module+ main
  (define written (regenerate!))
  (printf "gen-test-fixtures: wrote ~a fixture directories~n" (length written)))

;; raco test executes this module; keep it free of side effects.
(module+ test
  ;; One purity assertion so the generator is test-visible without writing:
  ;; every case bundle must carry a well-formed recomputed bundle_id.
  (require racket/contract
           racket/string)
  (define base (finalize-proof-bundle (base-proof-bundle)))
  (for ([case (fixture-cases)])
    (define bundle ((cadr case) base))
    (define id (proof-bundle-id bundle))
    (unless (and (string-prefix? id "sha256:") (= 71 (string-length id)))
      (error 'gen-test-fixtures "malformed bundle_id for ~a" (car case)))))
