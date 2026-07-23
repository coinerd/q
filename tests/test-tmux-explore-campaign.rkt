#lang racket/base

;; tests/test-tmux-explore-campaign.rkt — W7 campaign orchestrator tests

;; @speed slow
(require rackunit
         racket/file
         racket/hash
         racket/port
         racket/string
         racket/system
         "../scripts/tmux-explore/campaign.rkt"
         "../scripts/tmux-explore/evidence.rkt"
         "../scripts/tmux-explore/verifiers.rkt")

;; T1: Mock campaign runs all 9 scenarios
(test-case "mock campaign runs all 9 scenarios"
  (define result (run-campaign #:mode 'mock))
  (check-true (campaign-result-passed? result) "campaign should pass in mock mode")
  (check-equal? (length (campaign-result-scenario-results result)) 9 "should have 9 scenario results")
  (check-true (hash? (campaign-result-manifest result)) "manifest should be a hash"))

;; T2: Campaign manifest has correct structure
(test-case "campaign manifest structure"
  (define result (run-campaign #:mode 'mock))
  (define manifest (campaign-result-manifest result))
  (check-equal? (hash-ref manifest 'tag) "all-nine-campaign" "tag should be all-nine-campaign")
  (check-equal? (hash-ref manifest 'schema-version) 2 "schema-version should be 2")
  (check-equal? (hash-ref manifest 'scenario-count) 9 "scenario-count should be 9")
  (check-true (hash-has-key? manifest 'github-attestation) "should have github-attestation")
  (check-true (hash-has-key? manifest 'previous-manifest-hash) "should have previous-manifest-hash")
  (check-true (hash-has-key? manifest 'scenario-manifests) "should have scenario-manifests")
  (check-equal? (length (hash-ref manifest 'scenario-manifests))
                9
                "should have 9 scenario manifests"))

;; T3: Campaign verification passes
(test-case "campaign verification"
  (define result (run-campaign #:mode 'mock))
  (define verification (verify-campaign result))
  (check-true (hash-ref verification 'valid?) "campaign should verify"))

;; T4: Campaign manifest verification passes
(test-case "campaign manifest verification"
  (define result (run-campaign #:mode 'mock))
  (define manifest (campaign-result-manifest result))
  (define verification (verify-campaign-manifest manifest))
  (check-true (hash-ref verification 'valid?) "manifest should verify"))

;; T5: Manifest chain verification
(test-case "manifest chain verification"
  (define result1 (run-campaign #:mode 'mock))
  (define manifest1 (campaign-result-manifest result1))
  (define result2 (run-campaign #:mode 'mock #:previous-manifest manifest1))
  (define manifest2 (campaign-result-manifest result2))
  (check-true (verify-manifest-chain manifest2 manifest1) "chain should verify"))

;; T6: Credential proof in mock mode
(test-case "credential proof mock"
  (define result (run-campaign #:mode 'mock))
  (check-true (campaign-result-credential-proof result) "credential proof should pass in mock"))

;; T7: Campaign cleanup
(test-case "campaign cleanup"
  (define output-dir (make-temporary-file "q-campaign-test-~a" 'directory))
  (define result (run-campaign #:mode 'mock #:output-dir output-dir))
  (campaign-cleanup! output-dir)
  (check-false (directory-exists? output-dir) "output dir should be removed after cleanup"))

;; T8: Campaign manifest with unknown repo-sha fails verification
(test-case "campaign manifest with invalid repo-sha fails"
  (define result (run-campaign #:mode 'mock))
  (define manifest (campaign-result-manifest result))
  (define bad-manifest (hash-set manifest 'repo-sha "invalid"))
  (define verification (verify-campaign-manifest bad-manifest))
  (check-false (hash-ref verification 'valid?) "should fail with invalid repo-sha"))

;; T9: Campaign manifest with wrong scenario count fails
(test-case "campaign manifest with wrong scenario count fails"
  (define result (run-campaign #:mode 'mock))
  (define manifest (campaign-result-manifest result))
  (define bad-manifest (hash-set manifest 'scenario-count 8))
  (define verification (verify-campaign-manifest bad-manifest))
  (check-false (hash-ref verification 'valid?) "should fail with wrong scenario count"))

;; T10: Campaign result has cleanup-verified
(test-case "campaign cleanup verified"
  (define result (run-campaign #:mode 'mock))
  (check-true (campaign-result-cleanup-verified? result) "cleanup should be verified in mock mode"))
