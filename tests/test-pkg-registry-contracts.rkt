#lang racket/base

;; test-pkg-registry-contracts.rkt — Contract blame tests for pkg/registry
;; Wave W0: F1 — Verify typed contracts on all exported functions.

;; @speed fast
(require rackunit
         racket/contract
         (only-in racket/port)
         (file "../pkg/registry.rkt"))

;; ═══════════════════════════════════════════════════════════════════
;; load-package-index: accepts path-string? or #f (optional)
;; ═══════════════════════════════════════════════════════════════════

(test-case "load-package-index: no args returns hash"
  (define result (load-package-index))
  (check-true (hash? result)))

(test-case "load-package-index: accepts path-string"
  (define result (load-package-index "nonexistent.json"))
  (check-true (hash? result)))

;; ═══════════════════════════════════════════════════════════════════
;; index-packages: hash? → list?
;; ═══════════════════════════════════════════════════════════════════

(test-case "index-packages: returns list from hash"
  (define idx (hash 'packages (list (hash 'name "foo"))))
  (define result (index-packages idx))
  (check-true (list? result))
  (check-equal? (length result) 1))

;; ═══════════════════════════════════════════════════════════════════
;; search-packages: hash? string? → list?
;; ═══════════════════════════════════════════════════════════════════

(test-case "search-packages: returns matching packages"
  (define idx
    (hash 'packages
          (list (hash 'name "foo-bar" 'description "A foo package" 'keywords '("test"))
                (hash 'name "baz-qux" 'description "A baz package" 'keywords '()))))
  (define result (search-packages idx "foo"))
  (check-equal? (length result) 1)
  (check-equal? (hash-ref (car result) 'name) "foo-bar"))

(test-case "search-packages: empty result for no match"
  (define idx (hash 'packages '()))
  (define result (search-packages idx "nonexistent"))
  (check-equal? result '()))

;; ═══════════════════════════════════════════════════════════════════
;; get-package-info: hash? string? → (or/c hash? #f)
;; ═══════════════════════════════════════════════════════════════════

(test-case "get-package-info: returns entry by name"
  (define idx (hash 'packages (list (hash 'name "foo" 'version "1.0.0"))))
  (define result (get-package-info idx "foo"))
  (check-true (hash? result))
  (check-equal? (hash-ref result 'name) "foo"))

(test-case "get-package-info: returns #f for missing"
  (define idx (hash 'packages '()))
  (check-false (get-package-info idx "nonexistent")))

;; ═══════════════════════════════════════════════════════════════════
;; resolve-version: hash? string? [string?] → (or/c hash? #f)
;; ═══════════════════════════════════════════════════════════════════

(test-case "resolve-version: returns package without q-version"
  (define idx (hash 'packages (list (hash 'name "foo" 'version "1.0.0"))))
  (define result (resolve-version idx "foo"))
  (check-true (hash? result)))

(test-case "resolve-version: returns #f for missing package"
  (define idx (hash 'packages '()))
  (check-false (resolve-version idx "nonexistent")))

;; ═══════════════════════════════════════════════════════════════════
;; validate-index-entry: hash? → (listof string)
;; ═══════════════════════════════════════════════════════════════════

(test-case "validate-index-entry: valid entry returns empty list"
  (define entry
    (hash 'name
          "foo-bar"
          'version
          "1.0.0"
          'description
          "A test package"
          'author
          "test"
          'repo
          "https://example.com/repo"
          'checksum
          (make-string 64 #\a)))
  (check-equal? (validate-index-entry entry) '()))

(test-case "validate-index-entry: missing name returns error"
  (define entry
    (hash 'version
          "1.0.0"
          'description
          "test"
          'author
          "test"
          'repo
          "test"
          'checksum
          (make-string 64 #\a)))
  (define errors (validate-index-entry entry))
  (check-true (> (length errors) 0)))

;; ═══════════════════════════════════════════════════════════════════
;; validate-index: hash? → hash with 'valid? 'errors 'package-count
;; ═══════════════════════════════════════════════════════════════════

(test-case "validate-index: empty index is valid"
  (define result (validate-index (hash 'packages '())))
  (check-true (hash-ref result 'valid?))
  (check-equal? (hash-ref result 'package-count) 0))

(test-case "validate-index: returns hash with expected keys"
  (define result (validate-index (hash 'packages '())))
  (check-true (hash-has-key? result 'valid?))
  (check-true (hash-has-key? result 'errors))
  (check-true (hash-has-key? result 'package-count)))

;; ═══════════════════════════════════════════════════════════════════
;; version<=? and version<?: string? string? → boolean?
;; ═══════════════════════════════════════════════════════════════════

(test-case "version<=?: equal versions"
  (check-true (version<=? "1.0.0" "1.0.0")))

(test-case "version<=?: less-than"
  (check-true (version<=? "1.0.0" "2.0.0")))

(test-case "version<?: strictly less"
  (check-true (version<? "1.0.0" "1.0.1"))
  (check-false (version<? "1.0.0" "1.0.0")))

;; ═══════════════════════════════════════════════════════════════════
;; fetch-remote-index: optional string? → (or/c hash? #f)
;; ═══════════════════════════════════════════════════════════════════

(test-case "fetch-remote-index: returns #f for invalid URL"
  (check-false (fetch-remote-index "http://127.0.0.1:1/nonexistent")))

;; ═══════════════════════════════════════════════════════════════════
;; list-installed-packages: → list?
;; ═══════════════════════════════════════════════════════════════════

(test-case "list-installed-packages: returns list"
  (define result (list-installed-packages))
  (check-true (list? result)))
