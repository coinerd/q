#lang racket

;; @covers scripts/proof-bundle/consume.rkt

;; @speed fast  ;; @suite fast
;; @boundary integration

;; tests/test-proof-bundle-consume.rkt — v1.00.29 W9: the consumer boundary
;; for q.proof-bundle/1 reuse (dup-04: scheduled full-regression platform lane
;; consuming the ci.yml#test-platform proof at the same SHA).
;;
;; §9 threat model AT THE CONSUMER BOUNDARY — every rejection path below ends
;; in a consume exit of 3 (not-reusable) or 4 (invalid), i.e. the consuming
;; lane falls back to RUNNING THE SUITE. There is no outcome here that skips
;; the suite without a 'reusable validator decision; a tampered or corrupted
;; bundle can never produce a green skip.
;;
;;   (a) tampered/corrupted bundle (byte flipped inside an artifact body)
;;       -> invalid -> exit 4 -> fallback (suite would run)
;;   (b) stale commit SHA in the consumer request -> not-reusable -> exit 3
;;   (c) expired retention (current-time past retain_until) -> rejected -> 3
;;   (d) wrong required-claim-id -> not-reusable:claim-not-covered -> exit 3
;;   (e) unauthorized consumer-id / unauthorized release mode -> exit 3
;;   (f) valid bundle + request -> exit 0, and the q.reuse-decision/1 record
;;       carries consumer-id, decision, bundle_id, reason, the zero-tests-run
;;       statement and the timestamp fields (§11.3)
;;   (g) missing bundle file -> invalid -> exit 4
;;
;; The bundle under test is built through the WRITE subcommand itself
;; (in-process: the same functions the CLI wraps — no subprocess), so the
;; producer-side completeness gate is exercised too. The consumer request is
;; the consumer's OWN expectation, hardcoded independently of the writer
;; (constants mirror tests/fixtures/proof-bundle discipline), with enums
;; spelled the way JSON spells them ("commit", "required") so the adapter's
;; request normalization is exercised.
;;
;; The workflow-contract section pins the W9 wiring textually (precedent:
;; tests/test-w9-ci-workflow-verification.rkt): ci.yml#test-platform still
;; runs the suite unconditionally AND uploads the bundle;
;; full-regression.yml's macos lane retains the suite step behind the
;; `bundle_ok != 'true'` fallback condition; release.yml stays untouched.
;;
;; No sleeps, no threads, no network.

(require rackunit
         racket/file
         racket/list
         racket/string
         json
         (only-in "../scripts/proof-bundle/consume.rkt"
                  write-command
                  consume-command
                  decision-exit-code
                  normalize-request))

;; ---------------------------------------------------------------------------
;; Repo layout (cwd-independent)
;; ---------------------------------------------------------------------------

(define-values (this-file-dir _tf-name _tf-dir?)
  (split-path (resolved-module-path-name (variable-reference->resolved-module-path
                                          (#%variable-reference)))))

(define repo-root (simplify-path (build-path this-file-dir "..")))

;; ---------------------------------------------------------------------------
;; Deep symbolization (read-json yields symbol keys here; keep the normalizer
;; robust for string-key readers too)
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

(define (read-bundle-field path field)
  (hash-ref (json->symbols (call-with-input-file path read-json)) field))

;; ---------------------------------------------------------------------------
;; Deterministic producer-side constants (the ci.yml#test-platform stand-in)
;; ---------------------------------------------------------------------------

(define wf-rev "3f7a1c2b9d4e5f60718293a4b5c6d7e8f90a1b2c")
(define commit-sha "aa11bb22cc33dd44ee55ff66aa11bb22cc33dd44")
(define tree-sha "bb22cc33dd44ee55ff66aa11bb22cc33dd44ee55")
(define commit-sha-other "cc33dd44ee55ff66aa11bb22cc33dd44ee55ff66aa")

(define d-inventory "sha256:4141414141414141414141414141414141414141414141414141414141414141")
(define d-metadata "sha256:4242424242424242424242424242424242424242424242424242424242424242")
(define d-covers "sha256:4343434343434343434343434343434343434343434343434343434343434343")
(define d-selected "sha256:4444444444444444444444444444444444444444444444444444444444444444")
(define d-runner-rev "5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a5a")
(define d-flags "sha256:4545454545454545454545454545454545454545454545454545454545454545")
(define d-racket-exec "sha256:4646464646464646464646464646464646464646464646464646464646464646")
(define d-racket-inst "sha256:4747474747474747474747474747474747474747474747474747474747474747")
(define d-package-lock "sha256:4848484848484848484848484848484848484848484848484848484848484848")
(define d-resolved-set "sha256:4949494949494949494949494949494949494949494949494949494949494949")
(define d-feature-flags "sha256:4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a")
(define d-relevant-env "sha256:4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b")
(define d-prepared-artifact "sha256:4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c")
(define d-prepared-recipe "sha256:4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d")
(define d-prepared-cache "sha256:4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e")
(define d-result-summary "sha256:5050505050505050505050505050505050505050505050505050505050505050")
(define d-log "sha256:5151515151515151515151515151515151515151515151515151515151515151")
(define d-attestation "sha256:5252525252525252525252525252525252525252525252525252525252525252")

;; The claim/context document the CI producer step would write (symbol keys,
;; exactly what read-json yields from the claim.json on disk).
(define (claims-doc)
  (hasheq 'created_at
          "2026-09-12T10:00:00Z"
          'workflow_path
          ".github/workflows/ci.yml"
          'environment_class
          "linux-racket-8.10-platform"
          'attestation_digest
          d-attestation
          'allowed_consumers
          (list "workflow:full-regression.yml:test-platform")
          'denied_consumers
          (list)
          'release_reusable
          #f
          'producer
          (hasheq 'run_id
                  "run-2468"
                  'run_attempt
                  1
                  'job_id
                  "test-platform"
                  'job_name
                  "test-platform"
                  'event
                  "push"
                  'ref
                  "refs/heads/main"
                  'trust_tier
                  "tier-1")
          'claims
          (list (hasheq 'claim_id
                        "claim:platform-cross-version:macos-fast-suite"
                        'claim_version
                        1
                        'proof_class
                        "platform"
                        'required_environment_class
                        "linux-racket-8.10-platform"
                        'gate_class
                        "required"
                        'result
                        "pass"))
          'selection
          (hasheq 'inventory_digest
                  d-inventory
                  'metadata_manifest_digest
                  d-metadata
                  'covers_manifest_digest
                  d-covers
                  'selected_manifest_digest
                  d-selected
                  'selected_count
                  12
                  'selection_mode
                  "suite:platform"
                  'selector_revision_sha
                  d-runner-rev
                  'explanation_artifact_digest
                  d-log)
          'command
          (hasheq 'argv
                  (list "racket" "scripts/run-tests.rkt" "--suite" "platform")
                  'cwd_contract
                  "repo-root"
                  'flags_digest
                  d-flags
                  'runner_revision_sha
                  d-runner-rev)
          'environment
          (hasheq 'os
                  "linux"
                  'os_version
                  "24.04"
                  'runner_image
                  "ubuntu-24.04"
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
          'policy
          (hasheq 'test_profile
                  "platform"
                  'security_profile
                  "strict"
                  'sandbox_profile
                  "none"
                  'feature_flags_digest
                  d-feature-flags
                  'relevant_environment_digest
                  d-relevant-env)
          'prepared_environment
          (hasheq 'mode
                  "cold"
                  'artifact_digest
                  d-prepared-artifact
                  'build_recipe_digest
                  d-prepared-recipe
                  'verification_status
                  "verified"
                  'compiled_cache_digest
                  d-prepared-cache)
          'result
          (hasheq 'status
                  "pass"
                  'started_at
                  "2026-09-12T09:55:00Z"
                  'completed_at
                  "2026-09-12T10:00:00Z"
                  'elapsed_ms
                  300000
                  'tests_passed
                  12
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
                  d-result-summary)
          'artifacts
          (list (hasheq 'name
                        "platform-tests-log"
                        'media_type
                        "text/plain"
                        'digest
                        d-log
                        'size_bytes
                        1024
                        'store
                        "evidence-store"
                        'store_object_id
                        "test-platform-macos@run-2468")
                (hasheq 'name
                        "platform-test-results"
                        'media_type
                        "application/json"
                        'digest
                        d-result-summary
                        'size_bytes
                        2048
                        'store
                        "evidence-store"
                        'store_object_id
                        "test-results-platform@run-2468"))))

;; ---------------------------------------------------------------------------
;; Consumer request: what the consuming gate independently expects (§5.17
;; request vocabulary, JSON spelling for the enum fields)
;; ---------------------------------------------------------------------------

(define (base-request)
  (hasheq 'expected-repository
          "coinerd/q"
          'expected-producer-identity
          "ci.yml:test-platform"
          'expected-workflow-revision-sha
          wf-rev
          'expected-commit-sha
          commit-sha
          'expected-tree-sha
          tree-sha
          'subject-binding
          "commit"
          'expected-environment
          (hasheq 'os
                  "linux"
                  'os_version
                  "24.04"
                  'runner_image
                  "ubuntu-24.04"
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
          "platform"
          'expected-sandbox-profile
          "none"
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
          ;; artifact NAMES are data: after the JSON round trip they arrive
          ;; as symbol keys and the adapter must translate them back to the
          ;; validator's string-keyed vocabulary
          'expected-artifacts
          (hasheq 'platform-tests-log d-log 'platform-test-results d-result-summary)
          'expected-result-summary-digest
          d-result-summary
          'expected-prepared-env-artifact-digest
          d-prepared-artifact
          'current-time
          "2026-09-12T11:00:00Z"
          'minimum-retention-until
          "2026-09-12T11:00:00Z"
          'consumer-id
          "workflow:full-regression.yml:test-platform"
          'consumer-mode
          "regular"
          'required-claim-ids
          (list "claim:platform-cross-version:macos-fast-suite")
          'claim-gate-class
          "required"
          'expected-run-attempt
          1))

;; ---------------------------------------------------------------------------
;; Bundle/fixture management (built through the WRITE subcommand; memoized so
;; the module top level stays side-effect free)
;; ---------------------------------------------------------------------------

(define write-args
  (lambda (dir claims-file bundle-file)
    ;; the CLI contract takes STRINGS (argv), never path objects
    (list "--claims-json"
          (path->string claims-file)
          "--producer-identity"
          "ci.yml:test-platform"
          "--workflow-revision-sha"
          wf-rev
          "--commit-sha"
          commit-sha
          "--tree-sha"
          tree-sha
          "--repository"
          "coinerd/q"
          "--retention-days"
          "14"
          "--out"
          (path->string bundle-file))))

(define run-dir #f)
(define claims-file #f)
(define bundle-file #f)

(define (ensure-bundle!)
  (unless run-dir
    (set! run-dir (make-temporary-file "q-w9-consume-~a" 'directory))
    (set! claims-file (build-path run-dir "claim.json"))
    (set! bundle-file (build-path run-dir "bundle.json"))
    (call-with-output-file claims-file (lambda (out) (write-json (claims-doc) out)) #:exists 'replace)
    (define code (write-command (write-args run-dir claims-file bundle-file)))
    (unless (zero? code)
      (error 'test-proof-bundle-consume "write subcommand failed with exit ~a" code)))
  run-dir)

;; Writes REQUEST (a hasheq) to a temp JSON file inside run-dir and returns
;; the path — enums are written the way JSON spells them.
(define (request-file! request)
  (define p (make-temporary-file "q-w9-req-~a.json" #f (ensure-bundle!)))
  (call-with-output-file p (lambda (out) (write-json request out)) #:exists 'replace)
  p)

;; Runs the consume subcommand in-process; returns (cons exit-code record-path).
(define (consume! bundle-path request-path out-name)
  (define out-file (build-path (ensure-bundle!) out-name))
  (define stdout (open-output-string))
  (define code
    (parameterize ([current-output-port stdout])
      (consume-command (list "--bundle"
                             (path->string bundle-path)
                             "--request-json"
                             (path->string request-path)
                             "--out"
                             (path->string out-file)))))
  (cons code out-file))

(define (decision-record path)
  (json->symbols (call-with-input-file path read-json)))

(define rfc3339-rx #px"^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$")

;; Flips one byte inside the FIRST artifact digest of the bundle file —
;; corruption of an artifact body claim, not of the structural frame.
(define (tampered-bundle-file!)
  (define src (file->string bundle-file))
  (define m1 (regexp-match-positions #rx"\"artifacts\"" src))
  (unless m1
    (error 'tamper "no artifacts section"))
  (define from (cdr (car m1)))
  (define m2 (regexp-match-positions #px"[0-9a-f]" src from))
  (unless m2
    (error 'tamper "no digest hex after artifacts"))
  (define i (car (car m2)))
  (define ch (string-ref src i))
  (define replacement (if (char=? ch #\0) "1" "0"))
  (define out (make-temporary-file "q-w9-tampered-~a.json" #f (ensure-bundle!)))
  (call-with-output-file
   out
   (lambda (o) (display (string-append (substring src 0 i) replacement (substring src (add1 i))) o))
   #:exists 'replace)
  out)

;; ---------------------------------------------------------------------------
;; CLI adapter suites
;; ---------------------------------------------------------------------------

(define write-suite
  (test-suite "write subcommand: canonical, deterministic, fail-closed"

    (test-case "assembles and writes the bundle; prints the bundle_id; exit 0"
      (ensure-bundle!)
      (define stdout (open-output-string))
      (define code
        (parameterize ([current-output-port stdout])
          (write-command (write-args run-dir claims-file bundle-file))))
      (check-equal? code 0)
      (check-equal? (string-trim (get-output-string stdout))
                    (read-bundle-field bundle-file 'bundle_id))
      (check-true (string-prefix? (read-bundle-field bundle-file 'bundle_id) "sha256:")))

    (test-case "determinism: byte-identical input yields a byte-identical bundle"
      (define other (build-path run-dir "bundle-again.json"))
      (define stdout (open-output-string))
      (define code
        (parameterize ([current-output-port stdout])
          (write-command (write-args run-dir claims-file other))))
      (check-equal? code 0)
      (check-equal? (file->bytes other) (file->bytes bundle-file)))

    (test-case "producer completeness gate: an incomplete claims document is refused (exit 1)"
      (define broken-doc (hash-remove (claims-doc) 'result))
      (define broken-claims (build-path run-dir "claim-broken.json"))
      (call-with-output-file broken-claims
                             (lambda (out) (write-json broken-doc out))
                             #:exists 'replace)
      (define code
        (parameterize ([current-output-port (open-output-string)])
          (write-command (write-args run-dir broken-claims (build-path run-dir "no.json")))))
      (check-equal? code 1)
      (check-false (file-exists? (build-path run-dir "no.json"))))

    (test-case "usage errors exit 2 (never 0)"
      (check-equal? (parameterize ([current-output-port (open-output-string)])
                      (write-command (list "--claims-json" "x")))
                    2))))

;; The §9 threat model at the consumer boundary. Every rejection asserts a
;; non-zero exit AND that the retained record declares the fallback (the
;; suite would run): a green skip without 'reusable is impossible.
(define consume-suite
  (test-suite "§9 threat model at the consumer boundary"

    (test-case "(f) valid bundle + valid request: exit 0 and a complete §11.3 record"
      (define result (consume! bundle-file (request-file! (base-request)) "decision-f.json"))
      (check-equal? (car result) 0)
      (define rec (decision-record (cdr result)))
      ;; decision/reason values round-trip through JSON as strings
      (check-equal? (hash-ref rec 'schema) "q.reuse-decision/1")
      (check-equal? (hash-ref rec 'decision) "reusable")
      (check-equal? (hash-ref rec 'reason) #f)
      (check-equal? (hash-ref rec 'bundle_id) (read-bundle-field bundle-file 'bundle_id))
      (check-equal? (hash-ref rec 'consumer-id) "workflow:full-regression.yml:test-platform")
      (check-equal? (hash-ref rec 'consumer_id) "workflow:full-regression.yml:test-platform")
      (check-equal? (hash-ref rec 'consumer_mode) "regular")
      (check-equal? (hash-ref rec 'validated_steps) 15)
      ;; the zero-tests-run statement: reuse means the suite did NOT run
      (check-equal? (hash-ref rec 'zero-tests-run) #t)
      (check-equal? (hash-ref rec 'normal-proof-executed) #f)
      (check-equal? (hash-ref rec 'fallback-cause) "none")
      ;; timestamp fields (§11.3 record carries the decision-time evidence)
      (check-true (regexp-match? rfc3339-rx (hash-ref rec 'decided-at)))
      (check-equal? (hash-ref rec 'request-current-time) "2026-09-12T11:00:00Z")
      (check-equal? (hash-ref rec 'source-sha) commit-sha)
      (check-equal? (hash-ref rec 'affected-claim-set)
                    (list "claim:platform-cross-version:macos-fast-suite"))
      (check-equal? (hash-ref rec 'claim_ids) (list "claim:platform-cross-version:macos-fast-suite")))

    (test-case "(a) tampered bundle (byte flipped in an artifact digest) -> invalid -> exit 4 -> fallback"
      (define tampered (tampered-bundle-file!))
      (define result (consume! tampered (request-file! (base-request)) "decision-a.json"))
      (check-equal? (car result) 4)
      (define rec (decision-record (cdr result)))
      (check-equal? (hash-ref rec 'decision) "invalid")
      ;; NEVER a green skip: the record declares the fallback (suite runs)
      (check-equal? (hash-ref rec 'zero-tests-run) #f)
      (check-equal? (hash-ref rec 'normal-proof-executed) #t)
      (check-equal? (hash-ref rec 'fallback-cause) (hash-ref rec 'reason))
      (check-true (and (hash-ref rec 'reason) #t)))

    (test-case "(b) stale commit SHA in the request -> not-reusable -> exit 3"
      (define stale (hash-set (base-request) 'expected-commit-sha commit-sha-other))
      (define result (consume! bundle-file (request-file! stale) "decision-b.json"))
      (check-equal? (car result) 3)
      (define rec (decision-record (cdr result)))
      (check-equal? (hash-ref rec 'decision) "not-reusable")
      (check-equal? (hash-ref rec 'reason) "not-reusable:subject-mismatch")
      (check-equal? (hash-ref rec 'normal-proof-executed) #t))

    (test-case "(c) expired retention (current-time past retain_until) -> rejected -> exit 3"
      (define expired
        (hash-set* (base-request)
                   'current-time
                   "2026-10-01T00:00:00Z"
                   'minimum-retention-until
                   "2026-10-01T00:00:00Z"))
      (define result (consume! bundle-file (request-file! expired) "decision-c.json"))
      (check-equal? (car result) 3)
      (check-equal? (hash-ref (decision-record (cdr result)) 'reason)
                    "not-reusable:retention-expired"))

    (test-case "(d) wrong required-claim-id -> not-reusable:claim-not-covered -> exit 3"
      (define wrong-claim
        (hash-set (base-request) 'required-claim-ids (list "claim:some:other-claim")))
      (define result (consume! bundle-file (request-file! wrong-claim) "decision-d.json"))
      (check-equal? (car result) 3)
      (check-equal? (hash-ref (decision-record (cdr result)) 'reason)
                    "not-reusable:claim-not-covered"))

    (test-case "(e) unauthorized consumer-id -> not-reusable:consumer-not-authorized -> exit 3"
      (define intruder (hash-set (base-request) 'consumer-id "workflow:attacker.yml:elsewhere"))
      (define result (consume! bundle-file (request-file! intruder) "decision-e1.json"))
      (check-equal? (car result) 3)
      (check-equal? (hash-ref (decision-record (cdr result)) 'reason)
                    "not-reusable:consumer-not-authorized"))

    (test-case "(e) release mode over a release_reusable=false bundle -> exit 3"
      (define release-consumer
        (hash-set* (base-request) 'consumer-mode "release" 'consumer-id "gate:release-required"))
      (define result (consume! bundle-file (request-file! release-consumer) "decision-e2.json"))
      (check-equal? (car result) 3)
      (check-equal? (hash-ref (decision-record (cdr result)) 'reason)
                    "not-reusable:release-reuse-not-permitted"))

    (test-case "(g) missing bundle file -> invalid -> exit 4 -> fallback"
      (define missing (build-path (ensure-bundle!) "does-not-exist.json"))
      (define result (consume! missing (request-file! (base-request)) "decision-g.json"))
      (check-equal? (car result) 4)
      (define rec (decision-record (cdr result)))
      (check-equal? (hash-ref rec 'decision) "invalid")
      (check-equal? (hash-ref rec 'bundle_id) "unknown")
      (check-equal? (hash-ref rec 'normal-proof-executed) #t))

    (test-case "unreadable consumer request JSON -> invalid -> exit 4"
      (define garbage (make-temporary-file "q-w9-garbage-~a.json" #f (ensure-bundle!)))
      (call-with-output-file garbage (lambda (out) (display "not-json-at-all" out)) #:exists 'replace)
      (define result (consume! bundle-file garbage "decision-h.json"))
      (check-equal? (car result) 4)
      (check-equal? (hash-ref (decision-record (cdr result)) 'decision) "invalid"))

    (test-case "incomplete consumer request (missing key) -> invalid -> exit 4"
      (define incomplete (hash-remove (base-request) 'consumer-id))
      (define result (consume! bundle-file (request-file! incomplete) "decision-i.json"))
      (check-equal? (car result) 4)
      (check-equal? (hash-ref (decision-record (cdr result)) 'reason)
                    "invalid:incomplete-consumer-request"))))

(define adapter-suite
  (test-suite "Adapter contracts"

    (test-case "decision-exit-code: 0 reusable, 3 not-reusable, 4 invalid/unknown"
      (check-equal? (decision-exit-code 'reusable) 0)
      (check-equal? (decision-exit-code (list 'not-reusable 'not-reusable:stale-attempt)) 3)
      (check-equal? (decision-exit-code (list 'invalid 'invalid:unparseable-bundle)) 4)
      ;; anything unanticipated fails closed on the strictest side
      (check-equal? (decision-exit-code 'unexpected-atom) 4))

    (test-case "normalize-request: JSON enums -> validator symbols; artifact names stay strings"
      (define normalized
        (normalize-request
         (hasheq 'subject-binding
                 "commit"
                 'claim-gate-class
                 "required"
                 'expected-artifacts
                 (hasheq 'platform-test-results
                         "sha256:5050505050505050505050505050505050505050505050505050505050505050"))))
      (check-equal? (hash-ref normalized 'subject-binding) 'commit)
      (check-equal? (hash-ref normalized 'claim-gate-class) 'required)
      (check-equal? (hash-keys (hash-ref normalized 'expected-artifacts))
                    (list "platform-test-results")))))

;; ---------------------------------------------------------------------------
;; W9 workflow-contract pins (textual; precedent:
;; tests/test-w9-ci-workflow-verification.rkt)
;; ---------------------------------------------------------------------------

(define ci-yml-path (build-path repo-root ".github" "workflows" "ci.yml"))
(define fr-yml-path (build-path repo-root ".github" "workflows" "full-regression.yml"))
(define release-yml-path (build-path repo-root ".github" "workflows" "release.yml"))
(define dup-json-path
  (build-path repo-root "artifacts" "proof-graph" "v1.00.29-w0" "duplicate-classification.json"))

;; Extracts a top-level (2-space indented) job section from workflow text.
(define (job-section text job-name)
  (define lines (string-split text "\n"))
  (define header (string-append "  " job-name ":"))
  (define start (index-of lines header))
  (and start
       (let loop ([rest (list-tail lines (add1 start))]
                  [acc '()])
         (cond
           [(null? rest) (string-join (reverse acc) "\n")]
           [(regexp-match? #rx"^  [a-zA-Z][a-zA-Z0-9_-]*:\\s*$" (car rest))
            (string-join (reverse acc) "\n")]
           [else (loop (cdr rest) (cons (car rest) acc))]))))

;; 0-based index of the first line containing NEEDLE (#f when absent).
(define (index-of-line text needle)
  (for/first ([l (in-list (string-split text "\n"))]
              [i (in-naturals)]
              #:when (string-contains? l needle))
    i))

(define workflow-suite
  (test-suite "W9 workflow contracts (dup-04 wiring, no silent skip anywhere)"

    (test-case "ci.yml test-platform still produces the bundle AND runs the suite unconditionally"
      (define text (file->string ci-yml-path))
      (define section (job-section text "test-platform"))
      (check-true (and section #t) "test-platform job must exist")
      (check-true (string-contains? section "Produce proof bundle"))
      (check-true (string-contains? section "scripts/proof-bundle/consume.rkt write"))
      (check-true (string-contains? section "proof-bundle-platform"))
      (check-true (string-contains? section "retention-days: 14"))
      (check-true (string-contains? section "actions/upload-artifact@v7"))
      (check-true (string-contains? section "claim:platform-cross-version:macos-fast-suite"))
      ;; the producer job consumes nothing and never gates on bundle_ok
      (check-false (string-contains? section "bundle_ok"))
      ;; the suite step is unconditional: no `if:` between its name and the
      ;; next step
      (define lines (string-split section "\n"))
      (define start
        (for/first ([l (in-list lines)]
                    [i (in-naturals)]
                    #:when (string-prefix? l "      - name: Run platform-cross suite"))
          i))
      (check-true (and start #t) "the platform suite step must still exist in ci.yml")
      (define step-lines
        (let loop ([rest (list-tail lines start)])
          (cond
            [(null? (cdr rest)) (list (car rest))]
            [(string-prefix? (cadr rest) "      - name:") (list (car rest))]
            [else (cons (car rest) (loop (cdr rest)))])))
      (check-false (for/or ([l (in-list step-lines)])
                     (string-prefix? (string-trim l) "if:"))
                   "the ci.yml suite step must run unconditionally"))

    (test-case "full-regression.yml retains the macos suite step behind the fallback condition"
      (define text (file->string fr-yml-path))
      (check-true (string-contains? text "Resolve + consume proof bundle"))
      (check-true (string-contains? text "scripts/proof-bundle/consume.rkt consume"))
      (check-true (string-contains? text "-n proof-bundle-platform"))
      (check-true (string-contains? text "reuse-decision-platform"))
      (check-true (string-contains? text "test-results-platform"))
      (check-true (string-contains? text "if: steps.consume.outputs.bundle_ok != 'true'")
                  "ANY consume failure must fall back to running the suite")
      (define suite-idx (index-of-line text "Run platform-cross suite (macos-arm64)"))
      (define if-idx (index-of-line text "steps.consume.outputs.bundle_ok != 'true'"))
      (define resolve-idx (index-of-line text "Resolve + consume proof bundle"))
      (check-not-false suite-idx)
      (check-not-false if-idx)
      (check-not-false resolve-idx)
      (when (and suite-idx if-idx resolve-idx)
        ;; the condition is the suite step's own `if:` (it follows the step
        ;; name) and it comes after the consume step
        (check-true
         (< resolve-idx suite-idx if-idx)
         "consume precedes the suite step; the fallback condition is the suite step's own if:")))

    (test-case "full-regression.yml resolves the bundle against the SAME commit only"
      (define text (file->string fr-yml-path))
      (check-true (string-contains? text "gh run list --workflow ci.yml --commit \"$GITHUB_SHA\""))
      (check-true (string-contains? text "git rev-parse \"$GITHUB_SHA\":.github/workflows/ci.yml"))
      ;; §11.3: the decision artifact is uploaded for every run
      (check-true (string-contains? text "name: Upload reuse decision (always; §11.3)"))
      ;; no exit masking in this workflow (mirrors the test-ci-workflows pin)
      (check-false (regexp-match? #px"\\|\\s*true" text)))

    (test-case "release.yml stays untouched by the W9 reuse wiring"
      (define text (file->string release-yml-path))
      (check-false (string-contains? text "proof-bundle"))
      (check-false (string-contains? text "bundle_ok")))

    (test-case "W0 accounting input pins dup-04 as the removed exact duplicate"
      (define dup (json->symbols (call-with-input-file dup-json-path read-json)))
      (define dup04
        (for/first ([p (in-list (hash-ref dup 'pairs))]
                    #:when (equal? (hash-ref p 'pair_id) "dup-04"))
          p))
      (check-true (and dup04 #t) "dup-04 must exist")
      (check-equal? (hash-ref dup04 'class) "exact_duplicate")
      (check-true (string-contains? (hash-ref dup04 'a)
                                    "claim:platform-cross-version:macos-fast-suite"))
      (check-true (string-contains? (hash-ref dup04 'b) "claim:full-regression:macos-platform-suite"))
      (define inputs (hash-ref (hash-ref dup 'avoidable_duplicate_proof_mass_v0) 'inputs))
      (check-equal? (hash-ref inputs 'dup-04_ci_platform_seconds_observed) 389)
      (check-equal? (hash-ref inputs 'dup-04_fullreg_platform_setup_seconds_observed) 5199))))

;; ---------------------------------------------------------------------------
;; Runner wiring
;; ---------------------------------------------------------------------------

(define all-consume-tests
  (test-suite "q.proof-bundle/1 consumer boundary (W9 dup-04 reuse)"
    write-suite
    consume-suite
    adapter-suite
    workflow-suite))

(module+ test
  (require rackunit/text-ui)
  (run-tests all-consume-tests))

(module+ main
  (require rackunit/text-ui)
  (run-tests all-consume-tests))
