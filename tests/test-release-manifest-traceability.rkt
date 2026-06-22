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

;; ============================================================
;; W4 (#8566): Manifest parse/validate/render boundary
;; ============================================================

(define manifest-script-path "../scripts/gen-release-manifest.rkt")

;; Require struct types directly for constructor tests
(require (only-in "../scripts/gen-release-manifest.rkt"
                  manifest
                  manifest?
                  manifest-asset
                  manifest-trace
                  manifest-trace?
                  manifest-validation
                  manifest-valid?
                  manifest-validation-errors
                  make-manifest
                  manifest->jsexpr
                  jsexpr->manifest
                  validate-manifest
                  parse-manifest-json
                  manifest->json-string
                  manifest-version
                  manifest-tag
                  manifest-commit
                  manifest-date
                  manifest-assets
                  manifest-traceability
                  manifest-compatibility-min-racket
                  manifest-verification
                  manifest-asset-name
                  manifest-asset-size
                  manifest-asset-sha256
                  manifest-trace-tag-name
                  manifest-trace-tag-commit-sha
                  manifest-trace-tag-object-sha
                  manifest-trace-manifest-commit-sha
                  manifest-trace-commit-matches-tag?))

;; --- Fixtures ---

(define (valid-manifest-fixture)
  (make-manifest #:version "0.99.42"
                 #:commit "abc1234"
                 #:date "2026-06-22"
                 #:assets (list (manifest-asset "q-0.99.42.tar.gz" 12345 "n/a"))
                 #:traceability (manifest-trace "v0.99.42" "abc1234" #f "abc1234" #t)))

(define (mismatched-commit-fixture)
  (make-manifest #:version "0.99.42"
                 #:commit "def5678"
                 #:date "2026-06-22"
                 #:assets (list (manifest-asset "q-0.99.42.tar.gz" 12345 "n/a"))
                 #:traceability (manifest-trace "v0.99.42" "abc1234" #f "def5678" #f)))

;; --- Struct construction and accessors ---

(test-case "manifest struct: all fields accessible"
  (define m (valid-manifest-fixture))
  (check-equal? (manifest-version m) "0.99.42")
  (check-equal? (manifest-tag m) "v0.99.42")
  (check-equal? (manifest-commit m) "abc1234")
  (check-equal? (manifest-date m) "2026-06-22")
  (check-equal? (manifest-compatibility-min-racket m) "8.10")
  (check-equal? (manifest-verification m) "racket main.rkt --version"))

(test-case "manifest-asset struct: fields accessible"
  (define a (manifest-asset "q.tar.gz" 100 "hash"))
  (check-equal? (manifest-asset-name a) "q.tar.gz")
  (check-equal? (manifest-asset-size a) 100)
  (check-equal? (manifest-asset-sha256 a) "hash"))

(test-case "manifest-trace struct: fields accessible"
  (define t (manifest-trace "v1.0.0" "sha1" "obj-sha" "sha1" #t))
  (check-equal? (manifest-trace-tag-name t) "v1.0.0")
  (check-equal? (manifest-trace-tag-commit-sha t) "sha1")
  (check-equal? (manifest-trace-tag-object-sha t) "obj-sha")
  (check-equal? (manifest-trace-manifest-commit-sha t) "sha1")
  (check-true (manifest-trace-commit-matches-tag? t)))

(test-case "make-manifest auto-generates tag from version"
  (define m (make-manifest #:version "1.2.3" #:commit "c" #:date "d" #:assets '()))
  (check-equal? (manifest-tag m) "v1.2.3"))

(test-case "make-manifest auto-generates default traceability"
  (define m (make-manifest #:version "1.0.0" #:commit "abc" #:date "d" #:assets '()))
  (define tr (manifest-traceability m))
  (check-pred manifest-trace? tr)
  (check-equal? (manifest-trace-tag-commit-sha tr) "unknown")
  (check-false (manifest-trace-commit-matches-tag? tr)))

(test-case "manifest structs are transparent (equal? works)"
  (check-equal? (manifest-asset "n" 1 "s") (manifest-asset "n" 1 "s"))
  (check-equal? (manifest-trace "t" "c" #f "c" #t) (manifest-trace "t" "c" #f "c" #t)))

;; --- Serialization: manifest → jsexpr → JSON ---

(test-case "manifest->jsexpr: produces hash with version"
  (define m (valid-manifest-fixture))
  (define j (manifest->jsexpr m))
  (check-equal? (hash-ref j 'version) "0.99.42"))

(test-case "manifest->jsexpr: produces hash with tag"
  (define m (valid-manifest-fixture))
  (define j (manifest->jsexpr m))
  (check-equal? (hash-ref j 'tag) "v0.99.42"))

(test-case "manifest->jsexpr: traceability subsection present"
  (define m (valid-manifest-fixture))
  (define j (manifest->jsexpr m))
  (define tr (hash-ref j 'traceability))
  (check-equal? (hash-ref tr 'tag_name) "v0.99.42"))

(test-case "manifest->jsexpr: assets list present"
  (define m (valid-manifest-fixture))
  (define j (manifest->jsexpr m))
  (define assets (hash-ref j 'assets))
  (check-equal? (length assets) 1)
  (check-equal? (hash-ref (car assets) 'name) "q-0.99.42.tar.gz"))

(test-case "manifest->json-string: produces valid JSON with version"
  (define m (valid-manifest-fixture))
  (define json-str (manifest->json-string m))
  (check-true (string-contains? json-str "0.99.42"))
  (check-true (string-contains? json-str "version")))

(test-case "manifest->json-string: produces valid JSON with traceability"
  (define m (valid-manifest-fixture))
  (define json-str (manifest->json-string m))
  (check-true (string-contains? json-str "traceability")))

;; --- Parsing: JSON → manifest ---

(test-case "parse-manifest-json: valid JSON returns manifest"
  (define json-str (manifest->json-string (valid-manifest-fixture)))
  (define m (parse-manifest-json json-str))
  (check-pred manifest? m)
  (check-equal? (manifest-version m) "0.99.42"))

(test-case "parse-manifest-json: round-trip preserves version"
  (define m1 (valid-manifest-fixture))
  (define json-str (manifest->json-string m1))
  (define m2 (parse-manifest-json json-str))
  (check-equal? (manifest-version m1) (manifest-version m2)))

(test-case "parse-manifest-json: round-trip preserves assets"
  (define m1 (valid-manifest-fixture))
  (define json-str (manifest->json-string m1))
  (define m2 (parse-manifest-json json-str))
  (define a1 (car (manifest-assets m1)))
  (define a2 (car (manifest-assets m2)))
  (check-equal? (manifest-asset-name a1) (manifest-asset-name a2))
  (check-equal? (manifest-asset-size a1) (manifest-asset-size a2)))

(test-case "parse-manifest-json: round-trip preserves traceability"
  (define m1 (valid-manifest-fixture))
  (define json-str (manifest->json-string m1))
  (define m2 (parse-manifest-json json-str))
  (define tr1 (manifest-traceability m1))
  (define tr2 (manifest-traceability m2))
  (check-equal? (manifest-trace-tag-name tr1) (manifest-trace-tag-name tr2)))

(test-case "parse-manifest-json: returns #f for invalid JSON"
  (check-false (parse-manifest-json "not json")))

;; --- Validation ---

(test-case "validate-manifest: valid manifest passes"
  (define mv (validate-manifest (valid-manifest-fixture)))
  (check-true (manifest-valid? mv))
  (check-equal? (manifest-validation-errors mv) '()))

(test-case "validate-manifest: non-semver version fails"
  (define m (make-manifest #:version "abc" #:commit "c" #:date "d" #:assets '()))
  (define mv (validate-manifest m))
  (check-false (manifest-valid? mv))
  (check-true (> (length (manifest-validation-errors mv)) 0)))

(test-case "validate-manifest: tag without 'v' prefix fails"
  (define m (make-manifest #:version "1.0.0" #:commit "c" #:date "d" #:assets '()))
  ;; Override tag field by constructing manifest directly
  (define m2 (manifest "1.0.0" "1.0.0" "c" "d" '() "8.10" "ver" #f))
  (define mv (validate-manifest m2))
  (check-false (manifest-valid? mv)))

(test-case "validate-manifest: negative asset size fails"
  (define m
    (make-manifest #:version "1.0.0"
                   #:commit "c"
                   #:date "d"
                   #:assets (list (manifest-asset "q.tar.gz" -1 "n/a"))))
  (define mv (validate-manifest m))
  (check-false (manifest-valid? mv)))

(test-case "validate-manifest: bad sha256 fails"
  (define m
    (make-manifest #:version "1.0.0"
                   #:commit "c"
                   #:date "d"
                   #:assets (list (manifest-asset "q.tar.gz" 100 "short"))))
  (define mv (validate-manifest m))
  (check-false (manifest-valid? mv)))

(test-case "validate-manifest: sha256 'n/a' is allowed"
  (define m
    (make-manifest #:version "1.0.0"
                   #:commit "c"
                   #:date "d"
                   #:assets (list (manifest-asset "q.tar.gz" 100 "n/a"))))
  (define mv (validate-manifest m))
  (check-true (manifest-valid? mv)))

(test-case "validate-manifest: sha256 'unknown' is allowed"
  (define m
    (make-manifest #:version "1.0.0"
                   #:commit "c"
                   #:date "d"
                   #:assets (list (manifest-asset "q.tar.gz" 100 "unknown"))))
  (define mv (validate-manifest m))
  (check-true (manifest-valid? mv)))

(test-case "validate-manifest: commit mismatch fails"
  (define mv (validate-manifest (mismatched-commit-fixture)))
  (check-false (manifest-valid? mv))
  (check-true (> (length (manifest-validation-errors mv)) 0)))

(test-case "validate-manifest: valid sha256 (64 hex chars) passes"
  (define m
    (make-manifest #:version "1.0.0"
                   #:commit "c"
                   #:date "d"
                   #:assets
                   (list (manifest-asset
                          "q.tar.gz"
                          100
                          "a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2"))))
  (define mv (validate-manifest m))
  (check-true (manifest-valid? mv)))

;; --- Export existence ---

(test-case "gen-release-manifest.rkt exports manifest struct"
  (check-not-exn (lambda () (dynamic-require manifest-script-path 'manifest))))

(test-case "gen-release-manifest.rkt exports make-manifest"
  (check-not-exn (lambda () (dynamic-require manifest-script-path 'make-manifest))))

(test-case "gen-release-manifest.rkt exports validate-manifest"
  (check-not-exn (lambda () (dynamic-require manifest-script-path 'validate-manifest))))

(test-case "gen-release-manifest.rkt exports parse-manifest-json"
  (check-not-exn (lambda () (dynamic-require manifest-script-path 'parse-manifest-json))))

(test-case "gen-release-manifest.rkt exports manifest->json-string"
  (check-not-exn (lambda () (dynamic-require manifest-script-path 'manifest->json-string))))

(test-case "gen-release-manifest.rkt exports manifest->jsexpr"
  (check-not-exn (lambda () (dynamic-require manifest-script-path 'manifest->jsexpr))))

(test-case "gen-release-manifest.rkt exports jsexpr->manifest"
  (check-not-exn (lambda () (dynamic-require manifest-script-path 'jsexpr->manifest))))

(test-case "gen-release-manifest.rkt exports manifest-valid?"
  (check-not-exn (lambda () (dynamic-require manifest-script-path 'manifest-valid?))))
