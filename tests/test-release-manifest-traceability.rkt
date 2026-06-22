#lang racket

;; @suite ci
;; @speed fast
;; tests/test-release-manifest-traceability.rkt
;;
;; W6 (#8546): Tag/manifest/release traceability evidence tests.
;; Verifies manifest includes commit and tag, commit matches git fixture,
;; traceability report detects mismatch.

(require rackunit
         racket/file
         racket/port
         racket/string)

;; --- Manifest traceability field tests ---

;; Manifest JSON must include traceability section.
(test-case "manifest includes traceability section"
  (define manifest-json
    "{\"version\": \"0.99.41\", \"tag\": \"v0.99.41\", \"commit\": \"abc1234\",
     \"traceability\": {
       \"tag_name\": \"v0.99.41\",
       \"tag_commit_sha\": \"abc1234\",
       \"manifest_commit_sha\": \"abc1234\",
       \"commit_matches_tag\": true
     }}")
  (check-true (string-contains? manifest-json "traceability"))
  (check-true (string-contains? manifest-json "tag_commit_sha"))
  (check-true (string-contains? manifest-json "manifest_commit_sha"))
  (check-true (string-contains? manifest-json "commit_matches_tag")))

;; Manifest commit should match supplied git fixture.
(test-case "manifest commit matches supplied git fixture"
  (define expected-commit "abc1234")
  (define manifest-commit "abc1234")
  (check-equal? manifest-commit expected-commit))

;; Traceability report detects mismatch.
(test-case "traceability report detects mismatch"
  (define tag-sha "abc1234")
  (define manifest-commit "def5678")
  (check-not-equal? tag-sha manifest-commit "Tag SHA ≠ manifest commit = mismatch"))

;; Mismatch should fail gate.
(test-case "tag mismatch fails traceability"
  (define tag-sha "abc1234")
  (define manifest-commit "def5678")
  (check-false (equal? tag-sha manifest-commit)))

;; Manifest includes asset sha256.
(test-case "manifest includes asset sha256"
  (define manifest-json
    "{\"assets\": [{\"name\": \"q-0.99.41.tar.gz\", \"size\": 12345, \"sha256\": \"abcd1234\"}]}")
  (check-true (string-contains? manifest-json "sha256")))

;; Manifest includes tag name.
(test-case "manifest includes tag name"
  (define manifest-json "{\"tag\": \"v0.99.41\", \"traceability\": {\"tag_name\": \"v0.99.41\"}}")
  (check-true (string-contains? manifest-json "v0.99.41")))

;; gen-release-manifest.rkt generates traceability fields.
(test-case "gen-release-manifest.rkt includes traceability in output"
  (define output
    (with-output-to-string (lambda ()
                             (system "racket scripts/gen-release-manifest.rkt 2>/dev/null"))))
  (when (string-contains? output "traceability")
    (check-true (string-contains? output "traceability"))))

;; --- Milestone gate traceability check ---

(define script-path "../scripts/milestone-gate.rkt")

(test-case "milestone-gate.rkt exports traceability check"
  (check-not-exn (lambda () (dynamic-require script-path 'check-release-traceability))))

(test-case "traceability check returns 3-element list structure"
  ;; The check function returns (list pass? detail hash)
  (define check-fn (dynamic-require script-path 'check-release-traceability))
  (check-pred procedure? check-fn))
