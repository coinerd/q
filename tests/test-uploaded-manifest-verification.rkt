#lang racket

;; F-15 #8753: Uploaded manifest verification tests.
;; The milestone traceability check must download and parse the uploaded
;; release-manifest.json and verify all commit/tag/hash fields match.

;; @speed fast
(require rackunit
         racket/string
         racket/runtime-path)

(define-runtime-path script-path "../scripts/gen-release-manifest.rkt")
(define verify (dynamic-require script-path 'verify-uploaded-manifest))
(define manifest-valid? (dynamic-require script-path 'manifest-valid?))
(define manifest-errors (dynamic-require script-path 'manifest-validation-errors))

(define full-sha "abcdef0123456789abcdef0123456789abcdef01")
(define tag-object-sha "0123456789abcdef0123456789abcdef01234567")

(define valid-manifest-json
  (string-append "{\"version\":\"0.99.51\","
                 "\"tag\":\"v0.99.51\","
                 "\"commit\":\""
                 full-sha
                 "\","
                 "\"date\":\"2026-07-20\","
                 "\"traceability\":{\"tag_name\":\"v0.99.51\","
                 "\"tag_commit_sha\":\""
                 full-sha
                 "\","
                 "\"tag_object_sha\":\""
                 tag-object-sha
                 "\","
                 "\"manifest_commit_sha\":\""
                 full-sha
                 "\","
                 "\"commit_matches_tag\":true},"
                 "\"assets\":[{\"name\":\"q-0.99.51.tar.gz\",\"size\":1000,\"sha256\":\"n/a\"}],"
                 "\"compatibility\":{\"min-racket\":\"8.10\"},"
                 "\"verification\":\"racket main.rkt --version\"}"))

(test-case "verify-uploaded-manifest: accepts valid manifest"
  (define result (verify valid-manifest-json "0.99.51" "v0.99.51" "q-0.99.51.tar.gz"))
  (check-true (manifest-valid? result) (string-join (manifest-errors result) "; ")))

(test-case "verify-uploaded-manifest: rejects wrong version"
  (define result (verify valid-manifest-json "0.99.50" "v0.99.50" "q-0.99.50.tar.gz"))
  (check-false (manifest-valid? result))
  (check-true (ormap (lambda (e) (string-contains? e "version")) (manifest-errors result))))

(test-case "verify-uploaded-manifest: rejects wrong tag"
  (define result (verify valid-manifest-json "0.99.51" "v0.99.50" "q-0.99.51.tar.gz"))
  (check-false (manifest-valid? result))
  (check-true (ormap (lambda (e) (string-contains? e "tag")) (manifest-errors result))))

(test-case "verify-uploaded-manifest: rejects wrong tarball name"
  (define result (verify valid-manifest-json "0.99.51" "v0.99.51" "q-0.99.50.tar.gz"))
  (check-false (manifest-valid? result))
  (check-true (ormap (lambda (e) (string-contains? e "tarball")) (manifest-errors result))))

(test-case "verify-uploaded-manifest: rejects invalid JSON"
  (define result (verify "not json at all" "0.99.51" "v0.99.51" "q-0.99.51.tar.gz"))
  (check-false (manifest-valid? result)))

;; F-15 #8772: Reject short commit SHA (< 40 chars)
(test-case "verify-uploaded-manifest: rejects short commit SHA"
  (define short-manifest
    (string-append "{\"version\":\"0.99.51\","
                   "\"tag\":\"v0.99.51\","
                   "\"commit\":\"shortsha\","
                   "\"date\":\"2026-07-20\","
                   "\"traceability\":{\"tag_name\":\"v0.99.51\","
                   "\"tag_commit_sha\":\""
                   full-sha
                   "\","
                   "\"manifest_commit_sha\":\""
                   full-sha
                   "\","
                   "\"commit_matches_tag\":true},"
                   "\"assets\":[{\"name\":\"q-0.99.51.tar.gz\",\"size\":1000,\"sha256\":\"n/a\"}],"
                   "\"compatibility\":{\"min-racket\":\"8.10\"},"
                   "\"verification\":\"racket main.rkt --version\"}"))
  (define result (verify short-manifest "0.99.51" "v0.99.51" "q-0.99.51.tar.gz"))
  (check-false (manifest-valid? result))
  (check-true (ormap (lambda (e) (string-contains? e "40 hex")) (manifest-errors result))
              (string-join (manifest-errors result) "; ")))

(test-case "verify-uploaded-manifest: rejects manifest with missing fields"
  (define result (verify "{\"version\":\"0.99.51\"}" "0.99.51" "v0.99.51" "q-0.99.51.tar.gz"))
  (check-false (manifest-valid? result)))
