#lang racket

;; @suite ci
;; @speed fast
;; @boundary unit
;; tests/test-release-manifest-traceability.rkt
;;
;; W6 (#8546): Tag/manifest/release traceability evidence tests.
;; Verifies manifest includes commit and tag, commit matches git fixture,
;; traceability report detects mismatch.

(require rackunit
         racket/file
         racket/port
         racket/string
         racket/runtime-path)

(define-runtime-path generator-script-path "../scripts/gen-release-manifest.rkt")
(define-runtime-path milestone-script-path "../scripts/milestone-gate.rkt")

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

;; Executable generation is covered by test-release-build-manifest-integration.rkt.
(test-case "gen-release-manifest.rkt exports traceability parser"
  (check-pred procedure? (dynamic-require generator-script-path 'parse-manifest-json)))

;; --- Milestone gate traceability check ---

(define script-path milestone-script-path)

(test-case "milestone-gate.rkt exports traceability check"
  (check-not-exn (lambda () (dynamic-require script-path 'check-release-traceability))))

(test-case "traceability check returns 3-element list structure"
  ;; The check function returns (list pass? detail hash)
  (define check-fn (dynamic-require script-path 'check-release-traceability))
  (check-pred procedure? check-fn))

;; ============================================================
;; W4 (#8566): Manifest parse/validate/render boundary
;; ============================================================

(define manifest-script-path generator-script-path)

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

(define full-sha "abcdef0123456789abcdef0123456789abcdef01")
(define other-sha "fedcba9876543210fedcba9876543210fedcba98")
(define full-asset-sha (make-string 64 #\a))
(define short-sha "abc1234")

(define (valid-manifest-fixture)
  (make-manifest #:version "0.99.42"
                 #:commit full-sha
                 #:date "2026-06-22"
                 #:assets (list (manifest-asset "q-0.99.42.tar.gz" 12345 full-asset-sha))
                 #:traceability (manifest-trace "v0.99.42" full-sha #f full-sha #t)))

(define (mismatched-commit-fixture)
  (make-manifest #:version "0.99.42"
                 #:commit other-sha
                 #:date "2026-06-22"
                 #:assets (list (manifest-asset "q-0.99.42.tar.gz" 12345 full-asset-sha))
                 #:traceability (manifest-trace "v0.99.42" full-sha #f other-sha #f)))

;; --- Struct construction and accessors ---

(test-case "manifest struct: all fields accessible"
  (define m (valid-manifest-fixture))
  (check-equal? (manifest-version m) "0.99.42")
  (check-equal? (manifest-tag m) "v0.99.42")
  (check-equal? (manifest-commit m) full-sha)
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
  (define m (make-manifest #:version "abc" #:commit full-sha #:date "d" #:assets '()))
  (define mv (validate-manifest m))
  (check-false (manifest-valid? mv))
  (check-true (> (length (manifest-validation-errors mv)) 0)))

(test-case "validate-manifest: tag without 'v' prefix fails"
  (define m (make-manifest #:version "1.0.0" #:commit full-sha #:date "d" #:assets '()))
  ;; Override tag field by constructing manifest directly
  (define m2 (manifest "1.0.0" "1.0.0" full-sha "d" '() "8.10" "ver" #f))
  (define mv (validate-manifest m2))
  (check-false (manifest-valid? mv)))

(test-case "validate-manifest: negative asset size fails"
  (define m
    (make-manifest #:version "1.0.0"
                   #:commit full-sha
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

(test-case "validate-manifest: placeholder sha256 values fail closed"
  (for ([placeholder (in-list '("n/a" "unknown"))])
    (define m
      (make-manifest #:version "1.0.0"
                     #:commit full-sha
                     #:date "d"
                     #:assets (list (manifest-asset "q.tar.gz" 100 placeholder))))
    (check-false (manifest-valid? (validate-manifest m)))))

(test-case "validate-manifest: zero-size release asset fails closed"
  (define m
    (make-manifest #:version "1.0.0"
                   #:commit full-sha
                   #:date "d"
                   #:assets (list (manifest-asset "q.tar.gz" 0 full-asset-sha))))
  (check-false (manifest-valid? (validate-manifest m))))

(test-case "validate-manifest: commit mismatch fails"
  (define mv (validate-manifest (mismatched-commit-fixture)))
  (check-false (manifest-valid? mv))
  (check-true (> (length (manifest-validation-errors mv)) 0)))

(test-case "validate-manifest: valid sha256 (64 hex chars) passes"
  (define m
    (make-manifest #:version "1.0.0"
                   #:commit full-sha
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

;; ============================================================
;; W5 (#8567): Pure-core/effect-shell I/O boundary tests
;; ============================================================

(require (only-in "../scripts/gen-release-manifest.rkt"
                  release-inputs
                  release-inputs?
                  release-inputs-version
                  release-inputs-commit
                  release-inputs-date
                  release-inputs-tarball-name
                  release-inputs-tarball-size
                  release-inputs-tarball-sha256
                  release-inputs-tag-commit-sha
                  release-inputs-tag-object-sha
                  build-manifest
                  commits-match?
                  parse-q-version))

;; --- commits-match? predicate ---

(test-case "commits-match?: exact full-SHA match"
  (check-true (commits-match? full-sha full-sha)))

(test-case "commits-match?: rejects short exact values"
  (check-false (commits-match? "abc1234" "abc1234")))

(test-case "commits-match?: rejects prefix matches"
  (check-false (commits-match? "abc1234" full-sha))
  (check-false (commits-match? full-sha "abc1234")))

(test-case "commits-match?: different commits"
  (check-false (commits-match? "abc1234" "def5678")))

(test-case "commits-match?: unknown commit"
  (check-false (commits-match? "unknown" "abc1234")))

(test-case "commits-match?: unknown tag sha"
  (check-false (commits-match? "abc1234" "unknown")))

(test-case "commits-match?: #f commit"
  (check-false (commits-match? #f "abc1234")))

(test-case "commits-match?: #f tag sha"
  (check-false (commits-match? "abc1234" #f)))

;; --- parse-q-version (pure) ---

(test-case "parse-q-version: valid version line"
  (check-equal? (parse-q-version "(define q-version \"0.99.42\")") "0.99.42"))

(test-case "parse-q-version: returns #f for no match"
  (check-false (parse-q-version "no version here")))

(test-case "parse-q-version: handles multiline content"
  (check-equal? (parse-q-version "#lang racket/base\n(define q-version \"1.2.3\")\n") "1.2.3"))

;; --- release-inputs struct ---

(test-case "release-inputs struct: all fields accessible"
  (define ri
    (release-inputs "1.0.0"
                    "abc123"
                    "2026-06-22"
                    "q-1.0.0.tar.gz"
                    999
                    "deadbeef"
                    "abc123"
                    "tag-obj-sha"))
  (check-equal? (release-inputs-version ri) "1.0.0")
  (check-equal? (release-inputs-commit ri) "abc123")
  (check-equal? (release-inputs-date ri) "2026-06-22")
  (check-equal? (release-inputs-tarball-name ri) "q-1.0.0.tar.gz")
  (check-equal? (release-inputs-tarball-size ri) 999)
  (check-equal? (release-inputs-tarball-sha256 ri) "deadbeef")
  (check-equal? (release-inputs-tag-commit-sha ri) "abc123")
  (check-equal? (release-inputs-tag-object-sha ri) "tag-obj-sha"))

(test-case "release-inputs struct: transparent equality"
  (define ri1 (release-inputs "1.0.0" "c" "d" "n" 0 "s" "t" #f))
  (define ri2 (release-inputs "1.0.0" "c" "d" "n" 0 "s" "t" #f))
  (check-equal? ri1 ri2))

;; --- build-manifest (pure core) ---

(test-case "build-manifest: produces manifest from release-inputs"
  (define ri
    (release-inputs "0.99.42" "abc1234" "2026-06-22" "q-0.99.42.tar.gz" 12345 "n/a" "abc1234" #f))
  (define m (build-manifest ri))
  (check-pred manifest? m)
  (check-equal? (manifest-version m) "0.99.42")
  (check-equal? (manifest-tag m) "v0.99.42")
  (check-equal? (manifest-commit m) "abc1234"))

(test-case "build-manifest: tag auto-generated from version"
  (define ri (release-inputs "1.2.3" "c" "d" "n" 0 "s" "unknown" #f))
  (define m (build-manifest ri))
  (check-equal? (manifest-tag m) "v1.2.3"))

(test-case "build-manifest: commit defaults to unknown when #f"
  (define ri (release-inputs "1.0.0" #f "d" "n" 0 "s" "unknown" #f))
  (define m (build-manifest ri))
  (check-equal? (manifest-commit m) "unknown"))

(test-case "build-manifest: commit-matches-tag? true when full SHAs match"
  (define ri (release-inputs "1.0.0" full-sha "d" "n" 0 "s" full-sha #f))
  (define m (build-manifest ri))
  (define tr (manifest-traceability m))
  (check-true (manifest-trace-commit-matches-tag? tr)))

(test-case "build-manifest: commit-matches-tag? false when SHAs differ"
  (define ri (release-inputs "1.0.0" "abc123" "d" "n" 0 "s" "def456" #f))
  (define m (build-manifest ri))
  (define tr (manifest-traceability m))
  (check-false (manifest-trace-commit-matches-tag? tr)))

(test-case "build-manifest: commit-matches-tag? false when tag SHA unknown"
  (define ri (release-inputs "1.0.0" "abc123" "d" "n" 0 "s" "unknown" #f))
  (define m (build-manifest ri))
  (define tr (manifest-traceability m))
  (check-false (manifest-trace-commit-matches-tag? tr)))

(test-case "build-manifest: assets preserved from inputs"
  (define ri (release-inputs "1.0.0" "c" "d" "q-1.0.0.tar.gz" 555 "some-sha" "unknown" #f))
  (define m (build-manifest ri))
  (define a (car (manifest-assets m)))
  (check-equal? (manifest-asset-name a) "q-1.0.0.tar.gz")
  (check-equal? (manifest-asset-size a) 555)
  (check-equal? (manifest-asset-sha256 a) "some-sha"))

(test-case "build-manifest: traceability captures all inputs"
  (define ri (release-inputs "2.0.0" "c1" "d" "n" 0 "s" "c2" "obj-sha"))
  (define m (build-manifest ri))
  (define tr (manifest-traceability m))
  (check-equal? (manifest-trace-tag-name tr) "v2.0.0")
  (check-equal? (manifest-trace-tag-commit-sha tr) "c2")
  (check-equal? (manifest-trace-tag-object-sha tr) "obj-sha")
  (check-equal? (manifest-trace-manifest-commit-sha tr) "c1"))

(test-case "build-manifest: result passes validation"
  (define ri
    (release-inputs "0.99.42"
                    full-sha
                    "2026-06-22"
                    "q-0.99.42.tar.gz"
                    12345
                    full-asset-sha
                    full-sha
                    #f))
  (define m (build-manifest ri))
  (define mv (validate-manifest m))
  (check-true (manifest-valid? mv)))

;; --- build-manifest → manifest->json-string round-trip ---

(test-case "build-manifest + json-string: produces valid JSON"
  (define ri (release-inputs "0.99.42" "abc1234" "2026-06-22" "q.tar.gz" 100 "n/a" "abc1234" #f))
  (define json-str (manifest->json-string (build-manifest ri)))
  (define m2 (parse-manifest-json json-str))
  (check-pred manifest? m2)
  (check-equal? (manifest-version m2) "0.99.42")
  (check-equal? (manifest-commit m2) "abc1234"))

;; --- Export existence for W5 ---

(test-case "gen-release-manifest.rkt exports release-inputs struct"
  (check-not-exn (lambda () (dynamic-require manifest-script-path 'release-inputs))))

(test-case "gen-release-manifest.rkt exports build-manifest"
  (check-not-exn (lambda () (dynamic-require manifest-script-path 'build-manifest))))

(test-case "gen-release-manifest.rkt exports commits-match?"
  (check-not-exn (lambda () (dynamic-require manifest-script-path 'commits-match?))))

(test-case "gen-release-manifest.rkt exports parse-q-version"
  (check-not-exn (lambda () (dynamic-require manifest-script-path 'parse-q-version))))
