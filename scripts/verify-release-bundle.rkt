#!/usr/bin/env racket
#lang racket/base

;; Verify one immutable release asset against a manifest and trusted identity.
;; Usage: verify-release-bundle.rkt --version V --tag TAG --commit SHA
;;          --tag-object SHA TARBALL MANIFEST

(require racket/file
         racket/format
         racket/match
         racket/path
         racket/string
         openssl
         (only-in file/sha1 bytes->hex-string)
         (prefix-in manifest: "gen-release-manifest.rkt")
         json
         racket/list
         racket/port
         racket/set
         racket/system)

(provide verify-bundle
         compute-file-sha256
         check-bundle-integrity
         main)

(struct bundle-check (valid? errors) #:transparent)

(define full-sha-rx #px"^[0-9a-f]{40}$")
(define semver-rx #px"^[0-9]+\\.[0-9]+\\.[0-9]+$")

(define (compute-file-sha256 path)
  (call-with-input-file path (lambda (in) (bytes->hex-string (sha256-bytes in))) #:mode 'binary))

(define (check-bundle-integrity actual-size expected-size actual-sha256 expected-sha256 tarball-name)
  (define errors '())
  (unless (equal? actual-size expected-size)
    (set!
     errors
     (cons
      (format "size mismatch for ~a: actual ~a, expected ~a" tarball-name actual-size expected-size)
      errors)))
  (unless (equal? actual-sha256 expected-sha256)
    (set! errors
          (cons (format "SHA-256 mismatch for ~a: actual ~a, expected ~a"
                        tarball-name
                        actual-sha256
                        expected-sha256)
                errors)))
  (bundle-check (null? errors) (reverse errors)))

(define allowed-top-level-keys
  (set 'schema_version
       'version
       'tag
       'commit
       'date
       'assets
       'compatibility
       'verification
       'traceability
       'provenance))
(define required-top-level-keys
  (set 'schema_version
       'version
       'tag
       'commit
       'date
       'assets
       'compatibility
       'verification
       'traceability
       'provenance))
(define allowed-provenance-keys
  (set 'workflow_run_id 'workflow_run_url 'workflow_source_sha 'generator_tooling_sha))
(define allowed-asset-keys (set 'name 'size 'sha256))
(define allowed-trace-keys
  (set 'tag_name 'tag_commit_sha 'tag_object_sha 'manifest_commit_sha 'commit_matches_tag))
(define allowed-compatibility-keys (set 'min-racket))

(define (unknown-key-errors label value allowed)
  (if (hash? value)
      (for/list ([key (in-set (set-subtract (list->set (hash-keys value)) allowed))])
        (format "~a contains unknown field: ~a" label key))
      (list (format "~a must be an object" label))))

(define (object-shape-errors label value allowed required)
  (define errors (unknown-key-errors label value allowed))
  (when (hash? value)
    (for ([key (in-set required)])
      (unless (hash-has-key? value key)
        (set! errors (cons (format "~a missing required field: ~a" label key) errors)))))
  errors)

(define (strict-schema-errors json-text)
  (with-handlers ([exn:fail? (lambda (e)
                               (list (format "invalid manifest JSON: ~a" (exn-message e))))])
    (define raw (string->jsexpr json-text))
    (cond
      [(not (hash? raw)) (list "manifest root must be an object")]
      [else
       (define errors (unknown-key-errors "manifest" raw allowed-top-level-keys))
       (for ([key (in-set required-top-level-keys)])
         (unless (hash-has-key? raw key)
           (set! errors (cons (format "manifest missing required field: ~a" key) errors))))
       (unless (equal? (hash-ref raw 'schema_version #f) 1)
         (set! errors (cons "schema_version must equal 1" errors)))
       (define raw-version (hash-ref raw 'version #f))
       (define raw-tag (hash-ref raw 'tag #f))
       (unless (and (string? raw-version) (regexp-match? semver-rx raw-version))
         (set! errors (cons "version must be semver string" errors)))
       (unless (and (string? raw-tag)
                    (string? raw-version)
                    (string=? raw-tag (format "v~a" raw-version)))
         (set! errors (cons "tag must equal vVERSION" errors)))
       (define provenance (hash-ref raw 'provenance #f))
       (set! errors
             (append (unknown-key-errors "provenance" provenance allowed-provenance-keys) errors))
       (when (hash? provenance)
         (for ([key (in-set allowed-provenance-keys)])
           (unless (and (hash-has-key? provenance key) (string? (hash-ref provenance key)))
             (set! errors (cons (format "provenance.~a must be a string" key) errors))))
         (for ([key (in-list '(workflow_source_sha generator_tooling_sha))])
           (when (and (hash-has-key? provenance key)
                      (not (regexp-match? full-sha-rx (hash-ref provenance key))))
             (set! errors (cons (format "provenance.~a must be a full SHA" key) errors)))))
       (define assets (hash-ref raw 'assets #f))
       (unless (and (list? assets) (= (length assets) 1))
         (set! errors (cons "assets must be an array containing exactly one object" errors)))
       (when (list? assets)
         (for ([asset (in-list assets)]
               [index (in-naturals)])
           (define label (format "asset[~a]" index))
           (set! errors
                 (append (object-shape-errors label asset allowed-asset-keys allowed-asset-keys)
                         errors))
           (when (hash? asset)
             (unless (and (string? (hash-ref asset 'name #f))
                          (not (string=? (hash-ref asset 'name "") "")))
               (set! errors (cons (format "~a.name must be nonempty string" label) errors)))
             (unless (and (exact-integer? (hash-ref asset 'size #f))
                          (positive? (hash-ref asset 'size 0)))
               (set! errors (cons (format "~a.size must be positive integer" label) errors)))
             (unless (and (string? (hash-ref asset 'sha256 #f))
                          (regexp-match? #px"^[0-9a-f]{64}$" (hash-ref asset 'sha256 "")))
               (set! errors (cons (format "~a.sha256 must be 64 lowercase hex" label) errors))))))
       (define traceability (hash-ref raw 'traceability #f))
       (set! errors
             (append
              (object-shape-errors "traceability" traceability allowed-trace-keys allowed-trace-keys)
              errors))
       (when (hash? traceability)
         (for ([key (in-list '(tag_commit_sha tag_object_sha manifest_commit_sha))])
           (unless (and (string? (hash-ref traceability key #f))
                        (regexp-match? full-sha-rx (hash-ref traceability key "")))
             (set! errors (cons (format "traceability.~a must be a full SHA" key) errors))))
         (unless (boolean? (hash-ref traceability 'commit_matches_tag #f))
           (set! errors (cons "traceability.commit_matches_tag must be boolean" errors))))
       (define compatibility (hash-ref raw 'compatibility #f))
       (set! errors
             (append (object-shape-errors "compatibility"
                                          compatibility
                                          allowed-compatibility-keys
                                          allowed-compatibility-keys)
                     errors))
       (when (and (hash? compatibility) (not (string? (hash-ref compatibility 'min-racket #f))))
         (set! errors (cons "compatibility.min-racket must be string" errors)))
       (reverse errors)])))

(define (capture-command executable . arguments)
  (define stdout (open-output-string))
  (define stderr (open-output-string))
  (define status
    (parameterize ([current-output-port stdout]
                   [current-error-port stderr])
      (apply system*/exit-code executable arguments)))
  (values status (get-output-string stdout) (get-output-string stderr)))

(define (unsafe-path? raw)
  (define normalized (string-replace raw "\\\\" "/"))
  (define parts (filter (lambda (part) (not (member part '("" ".")))) (string-split normalized "/")))
  (or (string-prefix? normalized "/")
      (regexp-match? #px"^[A-Za-z]:/" normalized)
      (member ".." parts)
      (null? parts)
      (not (string=? (car parts) "q"))))

(define (archive-safety-errors tarball-path)
  (define tar (find-executable-path "tar"))
  (cond
    [(not tar) (list "tar executable unavailable for archive validation")]
    [else
     (define-values (list-status names-output names-error) (capture-command tar "tzf" tarball-path))
     (define-values (verbose-status verbose-output verbose-error)
       (capture-command tar "tvzf" tarball-path))
     (define errors '())
     (unless (zero? list-status)
       (set! errors (cons (format "cannot list archive: ~a" names-error) errors)))
     (unless (zero? verbose-status)
       (set! errors (cons (format "cannot inspect archive types: ~a" verbose-error) errors)))
     (define names (filter (lambda (name) (not (string=? name ""))) (string-split names-output "\n")))
     (when (> (length names) 100000)
       (set! errors (cons "archive exceeds 100000-entry limit" errors)))
     (define expanded-size 0)
     (for ([line (in-list (string-split verbose-output "\n"))]
           #:unless (string=? line ""))
       (define fields (string-split line))
       (when (>= (length fields) 3)
         (define maybe-size (string->number (list-ref fields 2)))
         (when (exact-nonnegative-integer? maybe-size)
           (when (> maybe-size (* 100 1024 1024))
             (set! errors (cons "archive entry exceeds 100 MiB limit" errors)))
           (set! expanded-size (+ expanded-size maybe-size))))
       (define mode (car fields))
       (when (or (regexp-match? #px"[sS]" mode)
                 (and (>= (string-length mode) 9) (char=? (string-ref mode 8) #\w)))
         (set! errors (cons (format "unsafe archive mode: ~a" mode) errors))))
     (when (> expanded-size (* 1024 1024 1024))
       (set! errors (cons "archive expanded size exceeds 1 GiB limit" errors)))
     (define compressed-size (file-size tarball-path))
     (when (or (zero? compressed-size) (> expanded-size (* 200 compressed-size)))
       (set! errors (cons "archive compression ratio exceeds 200:1 limit" errors)))
     (define seen (mutable-set))
     (for ([name (in-list names)])
       (define normalized
         (string-join (filter (lambda (part) (not (member part '("" "."))))
                              (string-split (string-replace name "\\\\" "/") "/"))
                      "/"))
       (when (unsafe-path? name)
         (set! errors (cons (format "unsafe archive path: ~a" name) errors)))
       (when (set-member? seen normalized)
         (set! errors (cons (format "duplicate normalized archive path: ~a" normalized) errors)))
       (set-add! seen normalized))
     (for ([line (in-list (string-split verbose-output "\n"))]
           #:unless (string=? line ""))
       (define type (string-ref line 0))
       (when (member type '(#\b #\c #\p #\s))
         (set! errors (cons (format "unsafe special archive entry: ~a" line) errors)))
       (define link-match
         (or (regexp-match #px" -> (.+)$" line) (regexp-match #px" link to (.+)$" line)))
       (when (and link-match (unsafe-path? (cadr link-match)))
         (set! errors (cons (format "escaping archive link: ~a" (cadr link-match)) errors))))
     (reverse errors)]))

(define (verify-bundle tarball-path
                       manifest-path
                       #:version [trusted-version #f]
                       #:tag [trusted-tag #f]
                       #:commit [trusted-commit #f]
                       #:tag-object [trusted-tag-object #f])
  (define errors '())
  (define (add! message)
    (set! errors (cons message errors)))
  (define manifest-json
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (add! (format "cannot read manifest ~a: ~a" manifest-path (exn-message e)))
                       #f)])
      (file->string manifest-path)))
  (when manifest-json
    (set! errors (append (reverse (strict-schema-errors manifest-json)) errors))
    (with-handlers ([exn:fail? (lambda (e)
                                 (add! (format "cannot inspect provenance: ~a" (exn-message e))))])
      (define raw (string->jsexpr manifest-json))
      (define provenance (hash-ref raw 'provenance (hasheq)))
      (define trusted-source
        (or (getenv "Q_RELEASE_WORKFLOW_SOURCE_SHA")
            (getenv "GITHUB_WORKFLOW_SHA")
            (getenv "GITHUB_SHA")
            #f))
      (define trusted-tooling (or (getenv "Q_RELEASE_TOOLING_SHA") trusted-source))
      (when (and trusted-source
                 (regexp-match? full-sha-rx trusted-source)
                 (not (equal? (hash-ref provenance 'workflow_source_sha #f) trusted-source)))
        (add! "provenance.workflow_source_sha does not match trusted workflow source"))
      (when (and trusted-tooling
                 (regexp-match? full-sha-rx trusted-tooling)
                 (not (equal? (hash-ref provenance 'generator_tooling_sha #f) trusted-tooling)))
        (add! "provenance.generator_tooling_sha does not match trusted tooling"))))
  (define m (and manifest-json (manifest:parse-manifest-json manifest-json)))
  (cond
    [(not m)
     (when (null? errors)
       (add! "manifest is not valid JSON"))]
    [else
     (define semantic-validation (manifest:validate-manifest m))
     (set! errors (append (reverse (manifest:manifest-validation-errors semantic-validation)) errors))
     (with-handlers ([exn:fail? (lambda (e)
                                  (add! (format "archive validation failed: ~a" (exn-message e))))])
       (set! errors (append (reverse (archive-safety-errors tarball-path)) errors)))
     (define assets (manifest:manifest-assets m))
     (unless (= (length assets) 1)
       (add! (format "manifest must contain exactly one asset; found ~a" (length assets))))
     (when (= (length assets) 1)
       (define asset (car assets))
       (define actual-name (path->string (file-name-from-path tarball-path)))
       (unless (string=? actual-name (manifest:manifest-asset-name asset))
         (add! (format "asset name mismatch: actual ~a, expected ~a"
                       actual-name
                       (manifest:manifest-asset-name asset))))
       (with-handlers ([exn:fail?
                        (lambda (e)
                          (add! (format "cannot read tarball ~a: ~a" tarball-path (exn-message e))))])
         (define integrity
           (check-bundle-integrity (file-size tarball-path)
                                   (manifest:manifest-asset-size asset)
                                   (compute-file-sha256 tarball-path)
                                   (manifest:manifest-asset-sha256 asset)
                                   actual-name))
         (set! errors (append (reverse (bundle-check-errors integrity)) errors))))
     (define tr (manifest:manifest-traceability m))
     (when trusted-version
       (unless (equal? (manifest:manifest-version m) trusted-version)
         (add! (format "version mismatch: manifest ~a, trusted ~a"
                       (manifest:manifest-version m)
                       trusted-version)))
       (unless (equal? (manifest:manifest-tag m) trusted-tag)
         (add!
          (format "tag mismatch: manifest ~a, trusted ~a" (manifest:manifest-tag m) trusted-tag)))
       (unless (equal? (manifest:manifest-commit m) trusted-commit)
         (add! (format "commit mismatch: manifest ~a, trusted ~a"
                       (manifest:manifest-commit m)
                       trusted-commit)))
       (unless (and (manifest:manifest-trace? tr)
                    (equal? (manifest:manifest-trace-tag-name tr) trusted-tag)
                    (equal? (manifest:manifest-trace-tag-commit-sha tr) trusted-commit)
                    (equal? (manifest:manifest-trace-manifest-commit-sha tr) trusted-commit)
                    (manifest:manifest-trace-commit-matches-tag? tr))
         (add! "traceability does not bind the trusted tag and commit exactly"))
       (unless (and (manifest:manifest-trace? tr)
                    (equal? (manifest:manifest-trace-tag-object-sha tr) trusted-tag-object))
         (add! (format "tag object mismatch: trusted ~a" trusted-tag-object))))])
  (bundle-check (null? errors) (reverse errors)))

(define usage
  "Usage: verify-release-bundle.rkt --version V --tag TAG --commit SHA --tag-object SHA TARBALL MANIFEST")

(define (main)
  (match (vector->list (current-command-line-arguments))
    [(list "--version"
           version
           "--tag"
           tag
           "--commit"
           commit
           "--tag-object"
           tag-object
           tarball
           manifest)
     (define invocation-errors
       (filter values
               (list (and (not (regexp-match? semver-rx version)) "--version must be X.Y.Z")
                     (and (not (string=? tag (format "v~a" version))) "--tag must equal vVERSION")
                     (and (not (regexp-match? full-sha-rx commit))
                          "--commit must be a full 40-character SHA")
                     (and (not (regexp-match? full-sha-rx tag-object))
                          "--tag-object must be a full 40-character SHA"))))
     (if (pair? invocation-errors)
         (begin
           (for ([error (in-list invocation-errors)])
             (eprintf "verify-release-bundle: ~a\n" error))
           (exit 2))
         (let ([result (verify-bundle tarball
                                      manifest
                                      #:version version
                                      #:tag tag
                                      #:commit commit
                                      #:tag-object tag-object)])
           (if (bundle-check-valid? result)
               (begin
                 (displayln "Bundle verification PASSED")
                 (exit 0))
               (begin
                 (eprintf "Bundle verification FAILED:\n")
                 (for ([error (in-list (bundle-check-errors result))])
                   (eprintf "  - ~a\n" error))
                 (exit 1)))))]
    [_
     (eprintf "~a\n" usage)
     (exit 2)]))

(module+ main
  (main))
