#lang racket

;; @covers scripts/proof-bundle/bundle-writer.rkt

;; @speed fast  ;; @suite default
;; @boundary unit

;; tests/test-proof-bundle-writer.rkt — the campaign's W5 wave: canonical q.proof-bundle/1
;; writer. Covers canonicalization stability (same input → same bundle_id),
;; required-field completeness (the 17 top-level fields of
;; this cycle's proof-bundle-schema JSON "required" list, hardcoded here), digest
;; correctness (FIPS 180-4 known-answer vectors + content-address derivation)
;; and schema conformance of the written bundle. Also proves that the §9
;; fixture corpus under tests/fixtures/proof-bundle/ regenerates
;; byte-identically. No sleeps, no threads.

(require rackunit
         racket/file
         racket/string
         json
         (only-in "../scripts/proof-bundle/bundle-writer.rkt"
                  proof-bundle-schema
                  required-bundle-fields
                  canonical-json
                  sha256-hex
                  sha256-string
                  proof-bundle-id
                  complete-bundle-errors
                  finalize-proof-bundle
                  proof-bundle->string
                  write-proof-bundle!)
         (only-in "../scripts/proof-bundle/gen-test-fixtures.rkt"
                  base-proof-bundle
                  fixture-cases
                  regenerate!))

;; ---------------------------------------------------------------------------
;; Repo layout (cwd-independent: resolved from this module's own path)
;; ---------------------------------------------------------------------------

(define-values (this-file-dir _tf-name _tf-dir?)
  (split-path (resolved-module-path-name (variable-reference->resolved-module-path
                                          (#%variable-reference)))))

(define repo-root (simplify-path (build-path this-file-dir "..")))

(define fixtures-root (build-path repo-root "tests" "fixtures" "proof-bundle"))

(define (fixture-path name file)
  (build-path fixtures-root name file))

;; ---------------------------------------------------------------------------
;; 1. Digest correctness: FIPS 180-4 known-answer vectors
;; ---------------------------------------------------------------------------

(define sha256-suite
  (test-suite "SHA-256 digest correctness"

    (test-case "empty input (FIPS known answer)"
      (check-equal? (sha256-string "")
                    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))

    (test-case "\"abc\" (FIPS known answer)"
      (check-equal? (sha256-string "abc")
                    "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"))

    (test-case "56-byte input (padding-boundary known answer)"
      (check-equal? (sha256-string "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
                    "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"))

    (test-case "multi-block input (FIPS known answer)"
      (check-equal? (sha256-string "The quick brown fox jumps over the lazy dog")
                    "d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592"))

    (test-case "bytes->hex output is lowercase 64 hex"
      (define hex (sha256-hex #"q"))
      (check-equal? (string-length hex) 64)
      (check-not-false (regexp-match? #px"^[0-9a-f]{64}$" hex)))))

;; ---------------------------------------------------------------------------
;; 2. Canonicalization: sorted keys, deterministic formatting
;; ---------------------------------------------------------------------------

(define canonical-suite
  (test-suite "Canonical JSON"

    (test-case "keys are sorted; separators are minimal (known answer)"
      (check-equal? (canonical-json (hasheq 'b 2 'a "x")) "{\"a\":\"x\",\"b\":2}"))

    (test-case "nested arrays and booleans"
      (check-equal? (canonical-json (hasheq 'list (list 1 #t "z") 'flag #f))
                    "{\"flag\":false,\"list\":[1,true,\"z\"]}"))

    (test-case "control characters are escaped"
      (check-equal? (canonical-json (hasheq 'k "a\nb\"c\\d")) "{\"k\":\"a\\nb\\\"c\\\\d\"}"))

    (test-case "key insertion order does not change the encoding"
      (define h1 (hasheq 'zebra 1 'alpha 2 'middle 3))
      (define h2 (hasheq 'middle 3 'zebra 1 'alpha 2))
      (check-equal? (canonical-json h1) (canonical-json h2))
      (check-equal? (canonical-json h1) "{\"alpha\":2,\"middle\":3,\"zebra\":1}"))

    (test-case "same input written twice yields identical bytes"
      (define tmp1 (make-temporary-file "q-w5-w1-~a.json"))
      (define tmp2 (make-temporary-file "q-w5-w2-~a.json"))
      (write-proof-bundle! (base-proof-bundle) #:path tmp1)
      (write-proof-bundle! (base-proof-bundle) #:path tmp2)
      (check-equal? (file->bytes tmp1) (file->bytes tmp2))
      (delete-directory/files tmp1)
      (delete-directory/files tmp2))))

;; ---------------------------------------------------------------------------
;; 3. Content-addressed bundle_id
;; ---------------------------------------------------------------------------

(define (valid-spec)
  (finalize-proof-bundle (base-proof-bundle)))

(define bundle-id-suite
  (test-suite "Content-addressed bundle_id"

    (test-case "bundle_id = sha256 over canonical form with bundle_id empty"
      (define spec (valid-spec))
      (define expected
        (string-append "sha256:" (sha256-string (canonical-json (hash-set spec 'bundle_id "")))))
      (check-equal? (hash-ref spec 'bundle_id) expected))

    (test-case "bundle_id shape: sha256:[0-9a-f]{64}"
      (define id (hash-ref (valid-spec) 'bundle_id))
      (check-equal? (string-length id) 71)
      (check-not-false (regexp-match? #px"^sha256:[0-9a-f]{64}$" id)))

    (test-case "any field change changes the content address"
      (define spec (valid-spec))
      (define mutated (hash-set spec 'created_at "2026-09-11T10:00:01Z"))
      (check-not-equal? (proof-bundle-id mutated) (hash-ref spec 'bundle_id)))

    (test-case "key order does not change the content address"
      (define a (hasheq 'schema proof-bundle-schema 'x 1))
      (define b (hasheq 'x 1 'schema proof-bundle-schema))
      (check-equal? (proof-bundle-id a) (proof-bundle-id b)))))

;; ---------------------------------------------------------------------------
;; 4. Required-field completeness (the 17 top-level fields, hardcoded)
;; ---------------------------------------------------------------------------

(define schema-required-top-level-fields
  (list "schema"
        "bundle_id"
        "created_at"
        "producer"
        "subject"
        "claims"
        "selection"
        "command"
        "environment"
        "policy"
        "prepared_environment"
        "result"
        "artifacts"
        "provenance"
        "retention"
        "authorization"
        "compatibility"))

(define completeness-suite
  (test-suite "Required-field completeness"

    (test-case "writer metadata covers exactly the schema's 17 required names"
      (check-equal? (sort required-bundle-fields string<?)
                    (sort schema-required-top-level-fields string<?)))

    (test-case "the valid base bundle is complete"
      (check-equal? (complete-bundle-errors (valid-spec)) '()))

    (test-case "removing any required field is caught with a named error"
      (for ([field schema-required-top-level-fields])
        (define broken (hash-remove (valid-spec) (string->symbol field)))
        (check-not-false (member (string-append "missing-field:" field)
                                 (complete-bundle-errors broken))
                         (string-append "no error for removed field " field))))

    (test-case "writer refuses to finalize an incomplete bundle"
      (check-exn #rx"refusing to finalize an incomplete proof bundle"
                 (lambda () (finalize-proof-bundle (hash-remove (base-proof-bundle) 'retention)))))))

;; ---------------------------------------------------------------------------
;; 5. Schema conformance of the written bundle
;; ---------------------------------------------------------------------------

;; Required sub-fields per section, hardcoded from
;; this cycle's proof-bundle-schema JSON / spec §5.3–§5.16.
(define schema-required-subfields
  (hasheq
   'producer
   (list "repository"
         "workflow_path"
         "workflow_revision_sha"
         "run_id"
         "run_attempt"
         "job_id"
         "job_name"
         "event"
         "ref"
         "trust_tier")
   'subject
   (list "subject_mode" "commit_sha" "tree_sha" "dirty")
   'selection
   (list "inventory_digest"
         "metadata_manifest_digest"
         "covers_manifest_digest"
         "selected_manifest_digest"
         "selected_count"
         "selection_mode"
         "selector_revision_sha"
         "explanation_artifact_digest")
   'command
   (list "argv" "cwd_contract" "flags_digest" "runner_revision_sha")
   'environment
   (list "os"
         "os_version"
         "runner_image"
         "architecture"
         "racket_version"
         "racket_executable_digest"
         "racket_installer_digest"
         "package_lock_digest"
         "resolved_package_set_digest"
         "locale"
         "timezone")
   'policy
   (list "test_profile"
         "security_profile"
         "sandbox_profile"
         "feature_flags_digest"
         "relevant_environment_digest")
   'prepared_environment
   (list "mode" "artifact_digest" "build_recipe_digest" "verification_status" "compiled_cache_digest")
   'result
   (list "status"
         "started_at"
         "completed_at"
         "elapsed_ms"
         "tests_passed"
         "tests_failed"
         "tests_skipped"
         "allowed_skip_reasons"
         "timeouts"
         "cancelled"
         "retry_count"
         "result_summary_digest")
   'provenance
   (list "attestation_format"
         "attestation_digest"
         "producer_identity"
         "source_repository"
         "source_workflow_revision_sha"
         "verification_method"
         "verification_key_or_identity")
   'retention
   (list "policy_id" "created_at" "retain_until" "immutable" "store" "retention_verified_at")
   'authorization
   (list "allowed_consumers" "denied_consumers" "release_reusable")
   'compatibility
   (list "compatibility_policy_version" "environment_class" "satisfies_classes" "subject_scope")))

;; read-json in this environment yields symbol keys; normalize to string keys
;; for the JSON-shape assertions.
(define (json->strings v)
  (cond
    [(hash? v)
     (for/hash ([(k val) (in-hash v)])
       (values (if (symbol? k)
                   (symbol->string k)
                   k)
               (json->strings val)))]
    [(list? v) (map json->strings v)]
    [else v]))

(define (read-bundle-json path)
  (json->strings (call-with-input-file path read-json)))

(define conformance-suite
  (test-suite "Schema conformance of the written bundle"

    (test-case "written file parses as JSON with exactly the 17 top-level keys"
      (define tmp (make-temporary-file "q-w5-c1-~a.json"))
      (write-proof-bundle! (base-proof-bundle) #:path tmp)
      (define parsed (read-bundle-json tmp))
      (check-equal? (sort (hash-keys parsed) string<?)
                    (sort schema-required-top-level-fields string<?))
      (delete-directory/files tmp))

    (test-case "every section carries its schema-required sub-fields"
      (define parsed (json->strings (valid-spec)))
      (for ([(section subs) (in-hash schema-required-subfields)])
        (define sect (hash-ref parsed (symbol->string section)))
        (for ([sub subs])
          (check-true (hash-has-key? sect sub) (format "~a.~a missing" section sub)))))

    (test-case "claim carries the schema-required claim fields and result=pass"
      (define parsed (json->strings (valid-spec)))
      (define claim (first (hash-ref parsed "claims")))
      (for ([sub
             (list "claim_id" "claim_version" "proof_class" "required_environment_class" "result")])
        (check-true (hash-has-key? claim sub) (format "claim.~a missing" sub)))
      (check-equal? (hash-ref claim "result") "pass"))

    (test-case "bundle_id of the written file matches the schema pattern"
      (define parsed (json->strings (valid-spec)))
      (check-not-false (regexp-match? #px"^sha256:[0-9a-f]{64}$" (hash-ref parsed "bundle_id"))))))

;; ---------------------------------------------------------------------------
;; 6. Fixture-corpus determinism: regeneration is byte-identical
;; ---------------------------------------------------------------------------

(define determinism-suite
  (test-suite "Fixture regeneration determinism"

    (test-case "every fixture case regenerates byte-identically"
      (define tmp-root (make-temporary-file "q-w5-fix-~a" 'directory))
      (define written (regenerate! tmp-root))
      (check-equal? (sort written string<?) (sort (map car (fixture-cases)) string<?))
      (for ([name written])
        (check-equal? (file->bytes (build-path tmp-root name "bundle.json"))
                      (file->bytes (fixture-path name "bundle.json"))
                      (string-append name "/bundle.json differs"))
        (check-equal? (file->bytes (build-path tmp-root name "SHA256SUMS"))
                      (file->bytes (fixture-path name "SHA256SUMS"))
                      (string-append name "/SHA256SUMS differs")))
      (delete-directory/files tmp-root))))

;; ---------------------------------------------------------------------------
;; Runner wiring
;; ---------------------------------------------------------------------------

(define all-writer-tests
  (test-suite "q.proof-bundle/1 writer"
    sha256-suite
    canonical-suite
    bundle-id-suite
    completeness-suite
    conformance-suite
    determinism-suite))

(module+ test
  (require rackunit/text-ui)
  (run-tests all-writer-tests))

(module+ main
  (require rackunit/text-ui)
  (run-tests all-writer-tests))
