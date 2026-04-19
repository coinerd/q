#lang racket

;; test-pkg-registry.rkt — Tests for package registry
;;
;; Issue #1295: GAP-01a — Package index format and discovery
;; Issue #1296: GAP-01b — Package discovery, install, and update CLI
;; Issue #1297: GAP-01c — CI validation for package metadata

(require rackunit
         racket/file
         racket/path
         json
         (only-in "../pkg/registry.rkt"
                  load-package-index
                  index-packages
                  search-packages
                  get-package-info
                  resolve-version
                  validate-index-entry
                  validate-index
                  install-package!
                  verify-package))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (make-tmp-dir)
  (define tmp (make-temporary-file "pkg-registry-test-~a"))
  (delete-file tmp)
  (make-directory tmp)
  tmp)

(define (cleanup tmp)
  (when (directory-exists? tmp)
    (delete-directory/files tmp)))

(define (make-test-index)
  (hasheq
   'version
   1
   'updated
   "2026-04-19T00:00:00Z"
   'packages
   (list
    (hasheq 'name
            "q-skills-extra"
            'version
            "0.1.0"
            'description
            "Extra skill definitions for q agent"
            'author
            "coinerd"
            'repo
            "https://github.com/coinerd/q-skills-extra"
            'tarball
            "https://github.com/coinerd/q-skills-extra/releases/download/v0.1.0/q-skills-extra.tar.gz"
            'checksum
            "a" ; placeholder, will fail format check
            'min-q-version
            "0.10.0"
            'keywords
            '("skills" "agent" "extensions"))
    (hasheq
     'name
     "q-provider-gemini"
     'version
     "0.2.1"
     'description
     "Google Gemini provider adapter for q"
     'author
     "coinerd"
     'repo
     "https://github.com/coinerd/q-provider-gemini"
     'tarball
     "https://github.com/coinerd/q-provider-gemini/releases/download/v0.2.1/q-provider-gemini.tar.gz"
     'checksum
     "b"
     'max-q-version
     "0.12.0"
     'keywords
     '("provider" "gemini" "google"))
    (hasheq
     'name
     "q-tui-theme-dark"
     'version
     "1.0.0"
     'description
     "Dark theme for the q TUI"
     'author
     "coinerd"
     'repo
     "https://github.com/coinerd/q-tui-theme-dark"
     'tarball
     "https://github.com/coinerd/q-tui-theme-dark/releases/download/v1.0.0/q-tui-theme-dark.tar.gz"
     'checksum
     "c"
     'keywords
     '("tui" "theme" "dark")))))

(define (make-valid-index)
  (hasheq 'version
          1
          'updated
          "2026-04-19T00:00:00Z"
          'packages
          (list (hasheq 'name
                        "q-skills-extra"
                        'version
                        "0.1.0"
                        'description
                        "Extra skill definitions"
                        'author
                        "coinerd"
                        'repo
                        "https://github.com/coinerd/q-skills-extra"
                        'checksum
                        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
                        'keywords
                        '("skills")))))

;; ---------------------------------------------------------------------------
;; Tests: load-package-index (#1295)
;; ---------------------------------------------------------------------------

(test-case "load-package-index: reads local index file"
  (define tmp (make-tmp-dir))
  (define idx-path (build-path tmp "index.json"))
  (define test-idx (make-test-index))
  (call-with-output-file idx-path (λ (out) (write-json test-idx out)) #:exists 'truncate)
  (define loaded (load-package-index (path->string idx-path)))
  (check-equal? (hash-ref loaded 'version) 1)
  (check-equal? (length (hash-ref loaded 'packages)) 3)
  (cleanup tmp))

(test-case "load-package-index: returns empty for missing file"
  (define loaded (load-package-index "/nonexistent/path/index.json"))
  (check-equal? (hash-ref loaded 'version) 1)
  (check-equal? (hash-ref loaded 'packages) '()))

(test-case "load-package-index: loads default index"
  (define loaded (load-package-index))
  (check-equal? (hash-ref loaded 'version) 1))

;; ---------------------------------------------------------------------------
;; Tests: search-packages (#1295)
;; ---------------------------------------------------------------------------

(test-case "search-packages: finds by name"
  (define idx (make-test-index))
  (define results (search-packages idx "gemini"))
  (check-equal? (length results) 1)
  (check-equal? (hash-ref (car results) 'name) "q-provider-gemini"))

(test-case "search-packages: finds by keyword"
  (define idx (make-test-index))
  (define results (search-packages idx "skills"))
  (check-equal? (length results) 1)
  (check-equal? (hash-ref (car results) 'name) "q-skills-extra"))

(test-case "search-packages: finds by description"
  (define idx (make-test-index))
  (define results (search-packages idx "dark"))
  (check-equal? (length results) 1)
  (check-equal? (hash-ref (car results) 'name) "q-tui-theme-dark"))

(test-case "search-packages: case insensitive"
  (define idx (make-test-index))
  (define results (search-packages idx "GEMINI"))
  (check-equal? (length results) 1))

(test-case "search-packages: empty results for no match"
  (define idx (make-test-index))
  (check-equal? (search-packages idx "nonexistent") '()))

;; ---------------------------------------------------------------------------
;; Tests: get-package-info (#1295)
;; ---------------------------------------------------------------------------

(test-case "get-package-info: exact match"
  (define idx (make-test-index))
  (define pkg (get-package-info idx "q-skills-extra"))
  (check-not-false pkg)
  (check-equal? (hash-ref pkg 'version) "0.1.0"))

(test-case "get-package-info: returns #f for missing"
  (define idx (make-test-index))
  (check-false (get-package-info idx "nonexistent")))

;; ---------------------------------------------------------------------------
;; Tests: resolve-version (#1295)
;; ---------------------------------------------------------------------------

(test-case "resolve-version: returns package without q-version filter"
  (define idx (make-test-index))
  (define pkg (resolve-version idx "q-skills-extra"))
  (check-not-false pkg))

(test-case "resolve-version: compatible with min-q-version"
  (define idx (make-test-index))
  (define pkg (resolve-version idx "q-skills-extra" "0.11.0"))
  (check-not-false pkg))

(test-case "resolve-version: incompatible with min-q-version"
  (define idx (make-test-index))
  (define pkg (resolve-version idx "q-skills-extra" "0.9.0"))
  (check-false pkg))

(test-case "resolve-version: compatible with max-q-version"
  (define idx (make-test-index))
  (define pkg (resolve-version idx "q-provider-gemini" "0.11.0"))
  (check-not-false pkg))

(test-case "resolve-version: incompatible with max-q-version"
  (define idx (make-test-index))
  (define pkg (resolve-version idx "q-provider-gemini" "0.12.0"))
  (check-false pkg))

;; ---------------------------------------------------------------------------
;; Tests: validate-index-entry (#1297)
;; ---------------------------------------------------------------------------

(test-case "validate-index-entry: valid entry"
  (define entry
    (hasheq 'name
            "q-test"
            'version
            "1.0.0"
            'description
            "Test package"
            'author
            "test"
            'repo
            "https://github.com/test/test"
            'checksum
            "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
  (check-equal? (validate-index-entry entry) '()))

(test-case "validate-index-entry: missing required fields"
  (define entry (hasheq 'name "q-test"))
  (define errors (validate-index-entry entry))
  (check-true (> (length errors) 0)))

(test-case "validate-index-entry: invalid name format"
  (define entry
    (hasheq 'name
            "Invalid_Name"
            'version
            "1.0.0"
            'description
            "Test"
            'author
            "test"
            'repo
            "https://github.com/test/test"
            'checksum
            "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
  (define errors (validate-index-entry entry))
  (check-true (ormap (λ (e) (string-contains? e "invalid name")) errors)))

(test-case "validate-index-entry: invalid checksum format"
  (define entry
    (hasheq 'name
            "q-test"
            'version
            "1.0.0"
            'description
            "Test"
            'author
            "test"
            'repo
            "https://github.com/test/test"
            'checksum
            "not-a-checksum"))
  (define errors (validate-index-entry entry))
  (check-true (ormap (λ (e) (string-contains? e "invalid checksum")) errors)))

(test-case "validate-index-entry: invalid version format"
  (define entry
    (hasheq 'name
            "q-test"
            'version
            "v1"
            'description
            "Test"
            'author
            "test"
            'repo
            "https://github.com/test/test"
            'checksum
            "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
  (define errors (validate-index-entry entry))
  (check-true (ormap (λ (e) (string-contains? e "invalid version")) errors)))

;; ---------------------------------------------------------------------------
;; Tests: validate-index (#1297)
;; ---------------------------------------------------------------------------

(test-case "validate-index: valid index"
  (define idx (make-valid-index))
  (define report (validate-index idx))
  (check-equal? (hash-ref report 'valid?) #t)
  (check-equal? (hash-ref report 'package-count) 1))

(test-case "validate-index: invalid entries reported"
  (define idx (hasheq 'version 1 'updated "" 'packages (list (hasheq 'name "bad-name"))))
  (define report (validate-index idx))
  (check-equal? (hash-ref report 'valid?) #f)
  (check-true (> (length (hash-ref report 'errors)) 0)))

;; ---------------------------------------------------------------------------
;; Tests: install-package! (#1296)
;; ---------------------------------------------------------------------------

(test-case "install-package!: fails gracefully without tarball"
  (define entry (hasheq 'name "q-test" 'version "1.0.0"))
  (define result (install-package! entry))
  (check-equal? (hash-ref result 'success?) #f))

;; ---------------------------------------------------------------------------
;; Tests: verify-package (#1296)
;; ---------------------------------------------------------------------------

(test-case "verify-package: returns hash with expected keys"
  (define entry
    (hasheq 'name
            "q-nonexistent-pkg-xyz"
            'checksum
            "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
  (define result (verify-package entry))
  ;; verify-package runs raco pkg show <name> — returns hash with valid? and checksum-match?
  (check-true (hash-has-key? result 'valid?))
  (check-true (hash-has-key? result 'checksum-match?))
  (check-true (hash-has-key? result 'details)))
