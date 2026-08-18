#lang racket

;; @suite release-smoke
;; @speed fast
;; @boundary integration
;; W1 (#9071): production-path RED contracts for release build → manifest → verify.

(require rackunit
         json
         openssl
         racket/file
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         (only-in file/sha1 bytes->hex-string)
         (only-in "../scripts/gen-release-manifest.rkt"
                  manifest
                  manifest-asset
                  manifest-trace
                  manifest-valid?
                  manifest->json-string
                  parse-manifest-json
                  commits-match?
                  validate-manifest))

(define-runtime-path repo-root "..")
(define-runtime-path generator-path "../scripts/gen-release-manifest.rkt")
(define-runtime-path verifier-path "../scripts/verify-release-bundle.rkt")
(define-runtime-path release-yml-path "../.github/workflows/release.yml")
(define-runtime-path release-core-path "../.github/workflows/release-core.yml")

(define q-version (dynamic-require (build-path repo-root "util" "version.rkt") 'q-version))
(define full-sha "abcdef0123456789abcdef0123456789abcdef01")
(define other-sha "fedcba9876543210fedcba9876543210fedcba98")

(define (run/racket script . args)
  (define stdout (open-output-string))
  (define stderr (open-output-string))
  (define exit-code
    (parameterize ([current-directory repo-root]
                   [current-output-port stdout]
                   [current-error-port stderr])
      (apply system*/exit-code (find-executable-path "racket") script args)))
  (values exit-code (get-output-string stdout) (get-output-string stderr)))

(define (run-generator . args)
  (apply run/racket generator-path args))

(define (check-generator-rejected path expected-message-rx)
  (define-values (exit-code stdout stderr) (run-generator (path->string path)))
  (check-not-equal? exit-code 0 (format "generator accepted invalid tarball: ~a" path))
  (check-regexp-match expected-message-rx
                      (string-append stdout stderr)
                      "failure must explain the violated tarball contract"))

(test-case "generator rejects a directory as tarball input"
  (define dir (make-temporary-file "q-release-dir-~a" 'directory))
  (dynamic-wind void
                (lambda () (check-generator-rejected dir #rx"(?i:regular file|directory)"))
                (lambda () (delete-directory/files dir))))

(test-case "generator rejects an absent tarball path"
  (define missing (build-path (find-system-path 'temp-dir) "q-definitely-absent-release.tar.gz"))
  (when (file-exists? missing)
    (delete-file missing))
  (check-generator-rejected missing #rx"(?i:not found|does not exist|missing)"))

(test-case "generator rejects an empty tarball"
  (define dir (make-temporary-file "q-empty-release-~a" 'directory))
  (define path (build-path dir (format "q-~a.tar.gz" q-version)))
  (call-with-output-file path #:exists 'truncate void)
  (dynamic-wind void
                (lambda () (check-generator-rejected path #rx"(?i:empty|nonempty)"))
                (lambda () (delete-directory/files dir))))

(test-case "generator rejects a wrong tarball name"
  (define path (make-temporary-file "not-q-release-~a.tar.gz"))
  (call-with-output-file path #:exists 'truncate (lambda (out) (display "bytes" out)))
  (dynamic-wind void
                (lambda () (check-generator-rejected path #rx"(?i:name|q-.*tar\\.gz)"))
                (lambda () (delete-file path))))

(test-case "generator rejects a tarball for another version"
  (define path (build-path (find-system-path 'temp-dir) "q-0.0.0.tar.gz"))
  (call-with-output-file path #:exists 'truncate (lambda (out) (display "bytes" out)))
  (dynamic-wind void
                (lambda () (check-generator-rejected path #rx"(?i:version|name)"))
                (lambda () (delete-file path))))

(test-case "explicit immutable identity produces a semantically valid manifest"
  (define dir (make-temporary-file "q-release-positive-~a" 'directory))
  (define path (build-path dir (format "q-~a.tar.gz" q-version)))
  (call-with-output-file path #:exists 'truncate (lambda (out) (display "release bytes" out)))
  (dynamic-wind void
                (lambda ()
                  (define-values (exit-code stdout stderr)
                    (run-generator "--version"
                                   q-version
                                   "--tag"
                                   (string-append "v" q-version)
                                   "--commit"
                                   full-sha
                                   "--tag-commit"
                                   full-sha
                                   "--tag-object"
                                   other-sha
                                   (path->string path)))
                  (check-equal? exit-code 0 stderr)
                  (define parsed (string->jsexpr stdout))
                  (check-equal? (hash-ref parsed 'commit) full-sha)
                  (check-equal? (hash-ref (hash-ref parsed 'traceability) 'tag_commit_sha) full-sha)
                  (check-equal? (hash-ref (hash-ref parsed 'traceability) 'tag_object_sha) other-sha)
                  (check-true (manifest-valid? (validate-manifest (parse-manifest-json stdout)))))
                (lambda () (delete-directory/files dir))))

(test-case "commit matching requires exact full-SHA equality"
  (check-false (commits-match? "abcdef0" full-sha))
  (check-true (commits-match? full-sha full-sha)))

(define (validation-for #:version [version "1.2.3"]
                        #:tag [tag "v1.2.3"]
                        #:commit [commit full-sha]
                        #:trace-tag [trace-tag "v1.2.3"]
                        #:tag-commit [tag-commit full-sha]
                        #:manifest-commit [manifest-commit full-sha]
                        #:matches? [matches? #t])
  (validate-manifest (manifest version
                               tag
                               commit
                               "2026-07-20"
                               (list (manifest-asset "q-1.2.3.tar.gz" 1 (make-string 64 #\a)))
                               "8.10"
                               "racket main.rkt --version"
                               (manifest-trace trace-tag tag-commit #f manifest-commit matches?))))

(test-case "semantic validation binds tag to version"
  (check-false (manifest-valid? (validation-for #:tag "v9.9.9" #:trace-tag "v9.9.9"))))
(test-case "semantic validation binds trace tag to manifest tag"
  (check-false (manifest-valid? (validation-for #:trace-tag "v9.9.9"))))
(test-case "semantic validation binds trace commit to top-level commit"
  (check-false (manifest-valid? (validation-for #:manifest-commit other-sha))))
(test-case "commit_matches_tag cannot override unequal commits"
  (check-false (manifest-valid? (validation-for #:tag-commit other-sha #:matches? #t))))

(test-case "real tarball passes generator then semantic verifier"
  (define dir (make-temporary-file "q-release-bundle-~a" 'directory))
  (define payload-dir (build-path dir "q"))
  (define tarball (build-path dir (format "q-~a.tar.gz" q-version)))
  (define manifest-path (build-path dir "release-manifest.json"))
  (make-directory payload-dir)
  (call-with-output-file (build-path payload-dir "README.md") (lambda (out) (display "fixture" out)))
  (check-equal? (system*/exit-code (find-executable-path "tar") "czf" tarball "-C" dir "q") 0)
  (dynamic-wind void
                (lambda ()
                  (define-values (generator-exit manifest-json generator-error)
                    (run-generator "--version"
                                   q-version
                                   "--tag"
                                   (string-append "v" q-version)
                                   "--commit"
                                   full-sha
                                   "--tag-commit"
                                   full-sha
                                   "--tag-object"
                                   other-sha
                                   (path->string tarball)))
                  (check-equal? generator-exit 0 generator-error)
                  (call-with-output-file manifest-path
                                         #:exists 'truncate
                                         (lambda (out) (display manifest-json out)))
                  (define-values (verify-exit verify-output verify-error)
                    (run/racket verifier-path
                                "--version"
                                q-version
                                "--tag"
                                (string-append "v" q-version)
                                "--commit"
                                full-sha
                                "--tag-object"
                                other-sha
                                (path->string tarball)
                                (path->string manifest-path)))
                  (check-equal? verify-exit 0 (string-append verify-output verify-error)))
                (lambda () (delete-directory/files dir))))

(define release-yml (file->string release-yml-path))
(define release-core (file->string release-core-path))

(define (bounded-section content start-marker [end-marker #f])
  (define start-match (regexp-match-positions (regexp (regexp-quote start-marker)) content))
  (if (not (pair? start-match))
      ""
      (let* ([start (caar start-match)]
             [end-match
              (and end-marker
                   (regexp-match-positions (regexp (regexp-quote end-marker)) content start))]
             [end (if end-marker
                      (if (pair? end-match)
                          (caar end-match)
                          (string-length content))
                      (string-length content))])
        (substring content start end))))

(define manifest-step
  (bounded-section release-core
                   "      - name: Generate release manifest"
                   "      - name: Generate release notes"))
(define verify-draft-job (bounded-section release-core "  verify-draft:" "  publish:"))
(define verify-public-job (bounded-section release-core "  verify-public:"))
(define release-suite-steps
  (list (cons "fast"
              (bounded-section release-yml
                               "      - name: Run tests (fast suite)"
                               "      - name: Record gate evidence (arch)"))
        (cons "arch"
              (bounded-section release-yml
                               "      - name: Record gate evidence (arch)"
                               "      - name: Record gate evidence (workflows)"))
        (cons "workflows"
              (bounded-section release-yml
                               "      - name: Record gate evidence (workflows)"
                               "      - name: Record gate evidence (tui)"))
        (cons "tui"
              (bounded-section release-yml
                               "      - name: Record gate evidence (tui)"
                               "      - name: Strict release readiness"))))

(test-case "manifest step consumes the build tarball output"
  (check-true (string-contains? manifest-step "steps.build.outputs.TARBALL_PATH")))

(test-case "draft and public jobs do not make manifest verification optional"
  (check-false (string-contains? verify-draft-job "if [ -f \"$LOCAL_MANIFEST\" ]; then"))
  (check-false (string-contains? verify-public-job "if [ -f \"$LOCAL_MANIFEST\" ]; then")))

(test-case "draft and public jobs each invoke semantic bundle verifier"
  (check-true (string-contains? verify-draft-job "scripts/verify-release-bundle.rkt"))
  (check-true (string-contains? verify-public-job "scripts/verify-release-bundle.rkt")))

(test-case "each release suite step preserves its own nonzero pipeline failure"
  (for ([entry (in-list release-suite-steps)])
    (define suite (car entry))
    (define step (cdr entry))
    (check-regexp-match (pregexp (format "run-tests\\.rkt.*--suite ~a" suite)) step)
    (check-true (string-contains? step "set -o pipefail")
                (format "~a step must preserve the runner exit through tee" suite))
    (for ([forbidden (in-list (list "|| true" "[warn]" "continuing" "exit 0"))])
      (check-false (string-contains? step forbidden)
                   (format "~a step suppresses failure with ~a" suite forbidden))))
  (check-false (regexp-match? #px"continue-on-error:[[:space:]]*true" release-yml)))
