#lang racket

;; @covers scripts/proof-bundle/bundle-validator.rkt

;; @speed fast  ;; @suite default
;; @boundary unit

;; tests/test-proof-bundle-validator.rkt — the campaign's W5 wave: fail-closed consumer
;; validation for q.proof-bundle/1.
;;
;; Coverage:
;;   - EVERY fixture directory under tests/fixtures/proof-bundle/ is validated
;;     with the expected named decision: the valid base is 'reusable and all
;;     twenty §9 threat-model cases are rejected with their named reason.
;;   - Category semantics: missing-field yields invalid (never not-reusable).
;;   - No checksum-only path: a tampered producer identity whose file and
;;     SHA256SUMS stay mutually consistent is still rejected (§5.13).
;;   - Inline adversarial cases: moving-ref-only provenance, retention
;;     shortfall, dirty subject, non-canonical encoding, unparseable bundle,
;;     bundle-id mismatch, incomplete consumer request, artifact expectation
;;     mismatches, gate-class unknown, and the retained reuse-decision record.
;;
;; The consumer request below is the consumer's OWN expectation, hardcoded
;; independently of the generator (constants mirror tests/fixtures/proof-bundle
;; base bundle by contract, not by import). No sleeps, no threads.

(require rackunit
         racket/file
         racket/string
         json
         (only-in "../scripts/proof-bundle/bundle-writer.rkt"
                  canonical-json
                  sha256-string
                  proof-bundle-id)
         (only-in "../scripts/proof-bundle/bundle-validator.rkt"
                  validate-proof-bundle
                  validate-proof-bundle-file
                  consumer-request-errors
                  reuse-decision-record))

;; ---------------------------------------------------------------------------
;; Repo layout (cwd-independent)
;; ---------------------------------------------------------------------------

(define-values (this-file-dir _tf-name _tf-dir?)
  (split-path (resolved-module-path-name (variable-reference->resolved-module-path
                                          (#%variable-reference)))))

(define repo-root (simplify-path (build-path this-file-dir "..")))

(define fixtures-root (build-path repo-root "tests" "fixtures" "proof-bundle"))

(define (fixture-file name file)
  (build-path fixtures-root name file))

;; ---------------------------------------------------------------------------
;; Local mutation helpers over parsed bundles (symbol-keyed hashes)
;; ---------------------------------------------------------------------------

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

;; Loads the committed valid base bundle, applies MUTATION to the parsed
;; bundle, re-addresses it (consistent forgery discipline), and returns the
;; canonical text.
(define (mutated-canonical-text mutation)
  (define parsed
    (json->symbols (call-with-input-file (fixture-file "valid-base" "bundle.json") read-json)))
  (canonical-json (hash-set (mutation parsed) 'bundle_id (proof-bundle-id (mutation parsed)))))

(define (text->bytes t)
  (string->bytes/utf-8 t))

;; ---------------------------------------------------------------------------
;; The consumer request: what the consuming gate independently expects.
;; These constants mirror the valid-base fixture by CONTRACT (they encode the
;; consumer's need), never by import.
;; ---------------------------------------------------------------------------

(define wf-sha "3f7a1c2b9d4e5f60718293a4b5c6d7e8f90a1b2c")
(define commit-sha "9c8b7a654321fedcba9876543210fedcba987654")
(define tree-sha "1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b")

(define d-inventory "sha256:0101010101010101010101010101010101010101010101010101010101010101")
(define d-metadata "sha256:0202020202020202020202020202020202020202020202020202020202020202")
(define d-covers "sha256:0303030303030303030303030303030303030303030303030303030303030303")
(define d-selected "sha256:0404040404040404040404040404040404040404040404040404040404040404")
(define d-command-flags "sha256:1111111111111111111111111111111111111111111111111111111111111111")
(define d-racket-exec "sha256:1212121212121212121212121212121212121212121212121212121212121212")
(define d-racket-inst "sha256:1313131313131313131313131313131313131313131313131313131313131313")
(define d-package-lock "sha256:1414141414141414141414141414141414141414141414141414141414141414")
(define d-resolved-set "sha256:1515151515151515151515151515151515151515151515151515151515151515")
(define d-feature-flags "sha256:1616161616161616161616161616161616161616161616161616161616161616")
(define d-relevant-env "sha256:1717171717171717171717171717171717171717171717171717171717171717")
(define d-prepared-artifact "sha256:1818181818181818181818181818181818181818181818181818181818181818")
(define d-result-summary "sha256:2020202020202020202020202020202020202020202020202020202020202020")
(define d-artifact-log "sha256:2121212121212121212121212121212121212121212121212121212121212121")

(define (base-request)
  (hasheq 'expected-repository
          "coinerd/q"
          'expected-producer-identity
          "workflow:coinerd/q/.github/workflows/ci.yml"
          'expected-workflow-revision-sha
          wf-sha
          'expected-commit-sha
          commit-sha
          'expected-tree-sha
          tree-sha
          'subject-binding
          'commit
          'expected-environment
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
                  d-racket-exec
                  'racket_installer_digest
                  d-racket-inst
                  'package_lock_digest
                  d-package-lock
                  'resolved_package_set_digest
                  d-resolved-set
                  'locale
                  "C.UTF-8"
                  'timezone
                  "UTC")
          'expected-security-profile
          "strict"
          'expected-test-profile
          "fast"
          'expected-sandbox-profile
          "sandboxed"
          'expected-feature-flags-digest
          d-feature-flags
          'expected-relevant-environment-digest
          d-relevant-env
          'expected-selection-digests
          (hasheq 'inventory_digest
                  d-inventory
                  'metadata_manifest_digest
                  d-metadata
                  'covers_manifest_digest
                  d-covers
                  'selected_manifest_digest
                  d-selected)
          'expected-artifacts
          (hash "unit-tests-log" d-artifact-log)
          'expected-result-summary-digest
          d-result-summary
          'expected-prepared-env-artifact-digest
          d-prepared-artifact
          'current-time
          "2026-09-11T12:00:00Z"
          'minimum-retention-until
          "2027-01-01T00:00:00Z"
          'consumer-id
          "gate:main-required"
          'consumer-mode
          "regular"
          'required-claim-ids
          (list "claim:unit:fast-gate")
          'claim-gate-class
          'required
          'expected-run-attempt
          3))

(define (request-for fixture-name)
  ;; Per-case consumer context (only the release case consumes differently).
  (if (string=? fixture-name "release-reusable-false")
      (hash-set* (base-request) 'consumer-id "gate:release-required" 'consumer-mode "release")
      (base-request)))

;; ---------------------------------------------------------------------------
;; Expected decisions: the valid base plus all twenty §9 threat-model cases
;; ---------------------------------------------------------------------------

(define expected-decisions
  (hash "valid-base"
        'reusable
        ;; 1. same commit, different Racket version
        "wrong-racket"
        (list 'not-reusable 'not-reusable:environment-mismatch:racket_version)
        ;; 2. same commit, different OS/platform
        "wrong-os"
        (list 'not-reusable 'not-reusable:environment-mismatch:os)
        ;; 3. same commit, weaker security profile
        "weaker-security"
        (list 'not-reusable 'not-reusable:security-profile-insufficient)
        ;; 4. same commit, changed package lock
        "changed-package-lock"
        (list 'not-reusable 'not-reusable:environment-mismatch:package_lock_digest)
        ;; 5. same commit, changed runner/workflow revision
        "changed-workflow-revision"
        (list 'not-reusable 'not-reusable:workflow-revision-mismatch)
        ;; 6. same commit, changed selected manifest
        "changed-manifest"
        (list 'not-reusable 'not-reusable:manifest-mismatch)
        ;; 7. same tree, different commit with commit-bound claim
        "tree-vs-commit"
        (list 'not-reusable 'not-reusable:tree-only-substitution)
        ;; 8. expired bundle
        "expired"
        (list 'not-reusable 'not-reusable:retention-expired)
        ;; 9. mutable/unverified evidence store
        "mutable-store"
        (list 'not-reusable 'not-reusable:store-not-immutable)
        ;; 10. forged/unknown producer identity
        "forged-identity"
        (list 'invalid 'invalid:producer-identity-mismatch)
        ;; 11. artifact checksum mismatch
        "checksum-mismatch"
        (list 'invalid 'invalid:artifact-digest-mismatch:unit-tests-log)
        ;; 12. result summary mismatch
        "result-mismatch"
        (list 'not-reusable 'not-reusable:result-summary-mismatch)
        ;; 13. producer timeout/cancellation
        "timeout-cancel"
        (list 'not-reusable 'not-reusable:producer-cancelled)
        ;; 14. unexpected skip
        "unexpected-skip"
        (list 'not-reusable 'not-reusable:unexpected-skip)
        ;; 15. consumer not authorized
        "unauthorized-consumer"
        (list 'not-reusable 'not-reusable:consumer-not-authorized)
        ;; 16. release consumer attempting to reuse release_reusable=false
        "release-reusable-false"
        (list 'not-reusable 'not-reusable:release-reuse-not-permitted)
        ;; 17. prepared-env digest mismatch
        "prepared-env-digest"
        (list 'not-reusable 'not-reusable:prepared-env-mismatch)
        ;; 18. missing field treated as compatible — must be invalid (fail-closed)
        "missing-field"
        (list 'invalid 'invalid:missing-field:selection)
        ;; 19. observational proof offered to required gate
        "observational-to-required"
        (list 'not-reusable 'not-reusable:observational-proof-for-required-gate)
        ;; 20. stale bundle from earlier run attempt
        "stale-attempt"
        (list 'not-reusable 'not-reusable:stale-attempt)))

(define fixture-suite
  (test-suite "§9 threat-model fixtures are rejected with named reasons"

    (test-case "every fixture directory has an expected decision (and vice versa)"
      (define discovered
        (sort (for/list ([p (in-list (directory-list fixtures-root))]
                         #:when (directory-exists? (build-path fixtures-root p)))
                (path->string p))
              string<?))
      (check-equal? discovered (sort (hash-keys expected-decisions) string<?)))

    (test-case "valid-base is accepted as reusable"
      (check-equal? (validate-proof-bundle-file (fixture-file "valid-base" "bundle.json")
                                                (base-request))
                    'reusable))

    (test-case "all twenty unsafe-reuse cases are rejected with the expected reason"
      (define results
        (for/list ([name (in-list (sort (hash-keys expected-decisions) string<?))]
                   #:unless (string=? name "valid-base"))
          (cons name
                (validate-proof-bundle-file (fixture-file name "bundle.json") (request-for name)))))
      ;; Print the shadow-decision table once for retained evidence.
      (for ([r results])
        (printf ";; validator: ~a -> ~s~n" (car r) (cdr r)))
      (for ([r results])
        (define actual (cdr r))
        (define expected (hash-ref expected-decisions (car r)))
        (unless (equal? actual expected)
          (printf ";; validator MISMATCH ~a: actual=~s expected=~s~n" (car r) actual expected))
        (check-true (equal? actual expected) (string-append "unexpected decision for " (car r)))))

    (test-case "category semantics: missing field is invalid, never not-reusable"
      (define decision
        (validate-proof-bundle-file (fixture-file "missing-field" "bundle.json") (base-request)))
      (check-equal? (car decision) 'invalid))

    (test-case "category semantics: authentic-but-incompatible is not-reusable"
      (define decision
        (validate-proof-bundle-file (fixture-file "expired" "bundle.json") (base-request)))
      (check-equal? (car decision) 'not-reusable))))

;; ---------------------------------------------------------------------------
;; Inline adversarial cases over the committed valid base
;; ---------------------------------------------------------------------------

(define adversarial-suite
  (test-suite "Adversarial inline cases (no checksum-only path)"

    (test-case "moving-ref-only provenance is rejected (§5.3)"
      (check-equal?
       (validate-proof-bundle
        (mutated-canonical-text
         (lambda (b)
           (path-set (path-set b (list 'producer 'workflow_revision_sha) "refs/heads/main")
                     (list 'provenance 'source_workflow_revision_sha)
                     "refs/heads/main")))
        (base-request))
       (list 'invalid 'invalid:moving-ref-provenance)))

    (test-case "insufficient retention horizon is rejected (§5.14)"
      (check-equal?
       (validate-proof-bundle (mutated-canonical-text
                               (lambda (b)
                                 (path-set b (list 'retention 'retain_until) "2026-10-01T00:00:00Z")))
                              (base-request))
       (list 'not-reusable 'not-reusable:retention-shortfall)))

    (test-case "dirty producer subject is rejected (§5.4)"
      (check-equal? (validate-proof-bundle
                     (mutated-canonical-text (lambda (b) (path-set b (list 'subject 'dirty) #t)))
                     (base-request))
                    (list 'invalid 'invalid:dirty-subject)))

    (test-case "claim without gate_class fails closed (missing required claim field)"
      (check-equal?
       (validate-proof-bundle
        (mutated-canonical-text
         (lambda (b)
           (path-set b (list 'claims 0) (hash-remove (list-ref (hash-ref b 'claims) 0) 'gate_class))))
        (base-request))
       (list 'invalid 'invalid:missing-field:claims.0)))

    (test-case "bundle-id mismatch is detected when a field is tampered"
      (define parsed
        (json->symbols (call-with-input-file (fixture-file "valid-base" "bundle.json") read-json)))
      (define tampered (canonical-json (hash-set parsed 'created_at "2026-09-11T11:00:00Z")))
      (check-equal? (validate-proof-bundle tampered (base-request))
                    (list 'invalid 'invalid:bundle-id-mismatch)))

    (test-case "non-canonical encoding is rejected (fail-closed on format)"
      (define text (file->string (fixture-file "valid-base" "bundle.json")))
      (check-equal? (validate-proof-bundle text (base-request)) 'reusable)
      (define spaced (string-append (substring text 0 1) " " (substring text 1)))
      (check-equal? (validate-proof-bundle spaced (base-request))
                    (list 'invalid 'invalid:non-canonical-encoding)))

    (test-case "unparseable bundle is rejected"
      (check-equal? (validate-proof-bundle "not-json-at-all" (base-request))
                    (list 'invalid 'invalid:unparseable-bundle)))

    (test-case "incomplete consumer request is rejected (fail-closed both ways)"
      (check-true (pair? (consumer-request-errors (hash-remove (base-request) 'consumer-id))))
      (check-equal? (validate-proof-bundle (file->string (fixture-file "valid-base" "bundle.json"))
                                           (hash-remove (base-request) 'consumer-id))
                    (list 'invalid 'invalid:incomplete-consumer-request)))

    (test-case "unknown artifact in expectations is flagged (stale expectation)"
      (check-equal?
       (validate-proof-bundle (file->string (fixture-file "valid-base" "bundle.json"))
                              (hash-set (base-request)
                                        'expected-artifacts
                                        (hash "unit-tests-log" d-artifact-log "ghost-log" d-covers)))
       (list 'invalid 'invalid:stale-artifact-expectation:ghost-log)))

    (test-case "no checksum-only path: consistent forgery of producer identity is rejected"
      ;; Tamper the producer identity AND recompute the content address AND
      ;; regenerate SHA256SUMS so file and checksums stay consistent: the
      ;; validator must still reject on identity, proving it never accepts on
      ;; checksums alone (§5.13).
      (define tampered-text
        (mutated-canonical-text (lambda (b)
                                  (path-set b
                                            (list 'provenance 'producer_identity)
                                            "workflow:attacker/q/.github/workflows/ci.yml"))))
      (define tmp-dir (make-temporary-file "q-w5-forge-~a" 'directory))
      (define tmp-bundle (build-path tmp-dir "bundle.json"))
      (define tmp-sums (build-path tmp-dir "SHA256SUMS"))
      (call-with-output-file tmp-bundle (lambda (out) (display tampered-text out)) #:exists 'replace)
      (call-with-output-file
       tmp-sums
       (lambda (out) (display (string-append (sha256-string tampered-text) "  bundle.json\n") out))
       #:exists 'replace)
      ;; the tampered directory is self-consistent...
      (check-true (string-suffix? (file->string tmp-bundle) tampered-text))
      ;; ...and is still rejected on identity, not checksums.
      (check-equal? (validate-proof-bundle-file tmp-bundle (base-request))
                    (list 'invalid 'invalid:producer-identity-mismatch))
      (delete-directory/files tmp-dir))

    (test-case "step 15: the retained reuse-decision record carries the verdict"
      (define request (base-request))
      (define accept-record
        (reuse-decision-record (file->string (fixture-file "valid-base" "bundle.json"))
                               request
                               'reusable))
      (check-equal? (hash-ref accept-record 'decision) 'reusable)
      (check-equal? (hash-ref accept-record 'reason) #f)
      (check-equal? (hash-ref accept-record 'consumer_id) "gate:main-required")
      (check-equal? (hash-ref accept-record 'validated_steps) 15)
      (define reject-record
        (reuse-decision-record (file->string (fixture-file "expired" "bundle.json"))
                               request
                               (list 'not-reusable 'not-reusable:retention-expired)))
      (check-equal? (hash-ref reject-record 'decision) 'not-reusable)
      (check-equal? (hash-ref reject-record 'reason) 'not-reusable:retention-expired))))

;; ---------------------------------------------------------------------------
;; Runner wiring
;; ---------------------------------------------------------------------------

(define all-validator-tests
  (test-suite "q.proof-bundle/1 fail-closed validator"
    fixture-suite
    adversarial-suite))

(module+ test
  (require rackunit/text-ui)
  (run-tests all-validator-tests))

(module+ main
  (require rackunit/text-ui)
  (run-tests all-validator-tests))
