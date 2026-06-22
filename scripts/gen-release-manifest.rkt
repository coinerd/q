#!/usr/bin/env racket
#lang racket/base

;; gen-release-manifest.rkt — Generate a structured release manifest (JSON).
;;
;; Usage:
;;   racket scripts/gen-release-manifest.rkt [TARBALL_PATH]
;;
;; If TARBALL_PATH is provided, includes file size and SHA-256 checksum.
;; Outputs JSON to stdout.

(require racket/file
         racket/port
         racket/string
         racket/system
         racket/path
         json)

;; Provide W4 parse/validate/render boundary + W5 pure-core/effect-shell
(provide (struct-out manifest)
         (struct-out manifest-asset)
         (struct-out manifest-trace)
         (struct-out manifest-validation)
         manifest-valid?
         manifest-validation-errors
         make-manifest
         manifest->jsexpr
         jsexpr->manifest
         validate-manifest
         parse-manifest-json
         manifest->json-string
         ;; W5 (#8567): pure-core/effect-shell I/O boundary
         (struct-out release-inputs)
         build-manifest
         commits-match?
         parse-q-version)

;; ---------------------------------------------------------------------------
;; W4 (#8566): Manifest domain model — parse/validate/render boundary
;; ---------------------------------------------------------------------------
;; Isolates the manifest data model from both JSON I/O and shell effects.
;; Pure functions: make-manifest, manifest->jsexpr, jsexpr->manifest,
;; validate-manifest, parse-manifest-json, manifest->json-string.

;; A single release asset (tarball).
(struct manifest-asset (name size sha256) #:transparent)

;; Traceability evidence linking tag to commit.
(struct manifest-trace
        (tag-name tag-commit-sha tag-object-sha manifest-commit-sha commit-matches-tag?)
  #:transparent)

;; The complete manifest domain object.
(struct manifest (version tag commit date assets compatibility-min-racket verification traceability)
  #:transparent)

;; Validation result: valid? + list of error strings (empty if valid).
(struct manifest-validation (valid? error-list) #:transparent)

(define (manifest-valid? mv)
  (and (manifest-validation? mv) (manifest-validation-valid? mv)))

(define (manifest-validation-errors mv)
  (if (manifest-validation? mv)
      (manifest-validation-error-list mv)
      '("not a manifest-validation")))

;; ---------------------------------------------------------------------------
;; Manifest constructor (pure — no I/O)
;; ---------------------------------------------------------------------------

(define (make-manifest #:version version
                       #:commit commit
                       #:date date
                       #:assets assets
                       #:compatibility-min-racket [min-racket "8.10"]
                       #:verification [verification "racket main.rkt --version"]
                       #:traceability [traceability #f])
  (define tag-name (format "v~a" version))
  (manifest version
            tag-name
            commit
            date
            assets
            min-racket
            verification
            (or traceability (manifest-trace tag-name "unknown" #f commit #f))))

;; ---------------------------------------------------------------------------
;; Serialization: manifest → jsexpr (pure — no I/O)
;; ---------------------------------------------------------------------------

(define (manifest->jsexpr m)
  (hasheq 'version
          (manifest-version m)
          'tag
          (manifest-tag m)
          'commit
          (manifest-commit m)
          'date
          (manifest-date m)
          'traceability
          (hasheq 'tag_name
                  (manifest-trace-tag-name (manifest-traceability m))
                  'tag_commit_sha
                  (manifest-trace-tag-commit-sha (manifest-traceability m))
                  'tag_object_sha
                  (manifest-trace-tag-object-sha (manifest-traceability m))
                  'manifest_commit_sha
                  (manifest-trace-manifest-commit-sha (manifest-traceability m))
                  'commit_matches_tag
                  (manifest-trace-commit-matches-tag? (manifest-traceability m)))
          'assets
          (for/list ([a (in-list (manifest-assets m))])
            (hasheq 'name
                    (manifest-asset-name a)
                    'size
                    (manifest-asset-size a)
                    'sha256
                    (manifest-asset-sha256 a)))
          'compatibility
          (hasheq 'min-racket (manifest-compatibility-min-racket m))
          'verification
          (manifest-verification m)))

(define (manifest->json-string m)
  (jsexpr->string (manifest->jsexpr m)))

;; ---------------------------------------------------------------------------
;; Parsing: jsexpr → manifest (pure — no I/O)
;; ---------------------------------------------------------------------------

(define (jsexpr->manifest j)
  (define version (hash-ref j 'version #f))
  (define tag (hash-ref j 'tag #f))
  (define commit (hash-ref j 'commit #f))
  (define date (hash-ref j 'date #f))
  (define raw-assets (hash-ref j 'assets '()))
  (define compat (hash-ref j 'compatibility (hasheq)))
  (define verification (hash-ref j 'verification #f))
  (define traceability-j (hash-ref j 'traceability #f))
  (define assets
    (for/list ([a (in-list (if (list? raw-assets)
                               raw-assets
                               '()))])
      (manifest-asset (hash-ref a 'name "unknown") (hash-ref a 'size 0) (hash-ref a 'sha256 "n/a"))))
  (define traceability
    (if (hash? traceability-j)
        (manifest-trace (hash-ref traceability-j 'tag_name "")
                        (hash-ref traceability-j 'tag_commit_sha "unknown")
                        (hash-ref traceability-j 'tag_object_sha #f)
                        (hash-ref traceability-j 'manifest_commit_sha "unknown")
                        (hash-ref traceability-j 'commit_matches_tag #f))
        #f))
  (make-manifest #:version version
                 #:commit commit
                 #:date date
                 #:assets assets
                 #:compatibility-min-racket (hash-ref compat 'min-racket "8.10")
                 #:verification verification
                 #:traceability traceability))

;; Parse a JSON string directly into a manifest.
(define (parse-manifest-json json-string)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (define j (string->jsexpr json-string))
    (and (hash? j) (jsexpr->manifest j))))

;; ---------------------------------------------------------------------------
;; Validation (pure — no I/O)
;; ---------------------------------------------------------------------------

(define semver-rx #px"^[0-9]+\\.[0-9]+\\.[0-9]+$")
(define sha256-rx #px"^[0-9a-fA-F]{64}$")

(define (validate-manifest m)
  (define errors '())
  ;; Required fields
  (unless (and (string? (manifest-version m)) (regexp-match? semver-rx (manifest-version m)))
    (set! errors (cons (format "version must be semver X.Y.Z, got: ~a" (manifest-version m)) errors)))
  (unless (and (string? (manifest-tag m)) (string-prefix? (manifest-tag m) "v"))
    (set! errors (cons (format "tag must start with 'v', got: ~a" (manifest-tag m)) errors)))
  (unless (string? (manifest-commit m))
    (set! errors (cons "commit must be a string" errors)))
  (unless (string? (manifest-date m))
    (set! errors (cons "date must be a string" errors)))
  ;; Assets
  (for ([a (in-list (manifest-assets m))]
        [i (in-naturals)])
    (unless (string? (manifest-asset-name a))
      (set! errors (cons (format "asset[~a]: name missing" i) errors)))
    (unless (and (integer? (manifest-asset-size a)) (>= (manifest-asset-size a) 0))
      (set! errors (cons (format "asset[~a]: size must be non-negative integer" i) errors)))
    (unless (or (equal? (manifest-asset-sha256 a) "n/a")
                (equal? (manifest-asset-sha256 a) "unknown")
                (regexp-match? sha256-rx (manifest-asset-sha256 a)))
      (set! errors
            (cons (format "asset[~a]: sha256 must be 64 hex chars or 'n/a', got: ~a"
                          i
                          (manifest-asset-sha256 a))
                  errors))))
  ;; Traceability: only flag mismatch when both SHAs are known and differ
  (define tr (manifest-traceability m))
  (when (manifest-trace? tr)
    (define tag-sha (manifest-trace-tag-commit-sha tr))
    (define commit-sha (manifest-trace-manifest-commit-sha tr))
    (unless (or (manifest-trace-commit-matches-tag? tr)
                (equal? tag-sha "unknown")
                (equal? commit-sha "unknown")
                (not tag-sha)
                (not commit-sha))
      (set! errors
            (cons (format "traceability: commit does not match tag (commit=~a, tag_sha=~a)"
                          commit-sha
                          tag-sha)
                  errors))))
  (manifest-validation (null? errors) (reverse errors)))

;; ---------------------------------------------------------------------------
;; W5 (#8567): Pure-core / effect-shell I/O boundary
;; ---------------------------------------------------------------------------
;; Separates manifest construction (pure) from data collection (effects).
;; The pure core takes a release-inputs struct and produces a manifest.
;; The effect shell collects raw data from files, git, and the environment.

;; Raw inputs gathered from the environment (no processing, just collection).
(struct release-inputs
        (version commit date tarball-name tarball-size tarball-sha256 tag-commit-sha tag-object-sha)
  #:transparent)

;; Pure: determine if two commit SHAs match (exact, prefix, or both unknown).
(define (commits-match? commit tag-commit-sha)
  (and commit
       tag-commit-sha
       (not (equal? commit "unknown"))
       (not (equal? tag-commit-sha "unknown"))
       (or (string=? commit tag-commit-sha)
           (string-prefix? tag-commit-sha commit)
           (string-prefix? commit tag-commit-sha))))

;; Pure: build a manifest from raw release inputs.
;; No I/O — takes a release-inputs struct, produces a manifest struct.
(define (build-manifest ri)
  (define version (release-inputs-version ri))
  (define commit (release-inputs-commit ri))
  (define tag-name (format "v~a" version))
  (define tag-commit-sha (release-inputs-tag-commit-sha ri))
  (define commit* (or commit "unknown"))
  (define commit-matches? (commits-match? commit* tag-commit-sha))
  (make-manifest #:version version
                 #:commit commit*
                 #:date (release-inputs-date ri)
                 #:assets (list (manifest-asset (release-inputs-tarball-name ri)
                                                (release-inputs-tarball-size ri)
                                                (release-inputs-tarball-sha256 ri)))
                 #:traceability (manifest-trace tag-name
                                                tag-commit-sha
                                                (release-inputs-tag-object-sha ri)
                                                commit*
                                                commit-matches?)))

;; ---------------------------------------------------------------------------
;; Version parsing (pure — takes file content string)
;; ---------------------------------------------------------------------------

(define (parse-q-version content)
  (define m (regexp-match #rx"\\(define q-version \"([0-9]+\\.[0-9]+\\.[0-9]+)\"" content))
  (and m (cadr m)))

;; ---------------------------------------------------------------------------
;; SHA-256 of a file (uses sha256sum command)
;; ---------------------------------------------------------------------------

(define (file-sha256 path)
  (define out
    (with-output-to-string (lambda ()
                             (system (format "sha256sum ~a 2>/dev/null || shasum -a 256 ~a"
                                             (path->string path)
                                             (path->string path))))))
  (define parts (string-split out))
  (if (pair? parts)
      (car parts)
      "unknown"))

;; ---------------------------------------------------------------------------
;; File size
;; ---------------------------------------------------------------------------

(define (file-size-bytes path)
  (file-size path))

;; ---------------------------------------------------------------------------
;; JSON output (manual — no dependency on json library)
;; ---------------------------------------------------------------------------

(define (git-tag-sha tag)
  "Get the SHA that the tag points to (short)."
  (define out
    (with-output-to-string (lambda () (system (format "git rev-list -n 1 ~a 2>/dev/null" tag)))))
  (define trimmed (string-trim out))
  (if (string=? trimmed "") "unknown" trimmed))

(define (git-tag-object-sha tag)
  "Get the annotated tag object SHA if the tag is annotated, #f if lightweight."
  (define out
    (with-output-to-string (lambda () (system (format "git rev-parse ~a 2>/dev/null" tag)))))
  (define trimmed (string-trim out))
  (if (or (string=? trimmed "") (regexp-match? #rx"unknown revision" trimmed)) #f trimmed))

(define (emit-manifest version commit date tarball-path)
  ;; Effect shell: collect raw inputs from filesystem and git
  (define ri (collect-release-inputs version commit date tarball-path))
  ;; Pure core: build manifest from inputs
  (define m (build-manifest ri))
  ;; Render: serialize and print
  (displayln (manifest->json-string m)))

;; ---------------------------------------------------------------------------
;; Effect shell: collect raw inputs from the environment
;; ---------------------------------------------------------------------------

(define (collect-release-inputs version commit date tarball-path)
  (define size
    (if tarball-path
        (file-size-bytes tarball-path)
        0))
  (define sha
    (if tarball-path
        (file-sha256 tarball-path)
        "n/a"))
  (define tarball-name
    (if tarball-path
        (path->string (file-name-from-path tarball-path))
        (format "q-~a.tar.gz" version)))
  (define tag-name (format "v~a" version))
  (define tag-commit-sha (git-tag-sha tag-name))
  (define tag-obj-sha (git-tag-object-sha tag-name))
  (release-inputs version commit date tarball-name size sha tag-commit-sha tag-obj-sha))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(define (main)
  (define args (vector->list (current-command-line-arguments)))

  ;; Read version
  (define util-path (build-path (current-directory) "util" "version.rkt"))
  (unless (file-exists? util-path)
    (displayln "ERROR: util/version.rkt not found")
    (exit 1))
  (define version (parse-q-version (file->string util-path)))
  (unless version
    (displayln "ERROR: could not parse version from util/version.rkt")
    (exit 1))

  ;; Get git commit
  (define commit
    (let ([out (with-output-to-string (lambda () (system "git rev-parse --short HEAD 2>/dev/null")))])
      (string-trim out)))

  ;; Get date
  (define date
    (let ([out (with-output-to-string (lambda () (system "date -u +%Y-%m-%d")))]) (string-trim out)))

  ;; Tarball path (optional)
  (define tarball-path
    (if (pair? args)
        (let ([p (string->path (car args))]) (if (file-exists? p) p #f))
        #f))

  (emit-manifest version commit date tarball-path))

(module+ main
  (main))
