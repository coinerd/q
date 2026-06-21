#lang racket

;; @suite ci
;; @speed fast
;; tests/test-milestone-gate.rkt
;; W5 (#8522): Tests for milestone-gate.rkt release truth verification.
;;
;; Tests use mocked release data (jsexpr hashes) to verify the pure
;; validation logic without network calls.

(require rackunit
         racket/file
         racket/string
         racket/port
         racket/system
         json)

;; ── Script loading ──

(define script-path "../scripts/milestone-gate.rkt")

(define (dynamic-script sym)
  (dynamic-require script-path sym))

;; ── Mock data builders ──

(define (make-release-data #:tag-name [tag "v0.99.40"] #:draft [draft #f] #:assets [assets '()])
  (hasheq 'tag_name
          tag
          'draft
          draft
          'prerelease
          #f
          'assets
          assets
          'published_at
          "2026-06-21T00:00:00Z"))

(define (make-asset name)
  (hasheq 'name name 'url (format "https://api.github.com/assets/~a" name) 'size 12345))

(define (complete-release-data)
  (make-release-data #:assets (list (make-asset "q-0.99.40.tar.gz")
                                    (make-asset "release-manifest.json"))))

(define (tarball-only-release-data)
  (make-release-data #:assets (list (make-asset "q-0.99.40.tar.gz"))))

(define (manifest-only-release-data)
  (make-release-data #:assets (list (make-asset "release-manifest.json"))))

(define (assetless-release-data)
  (make-release-data #:assets '()))

;; ============================================================
;; extract-version-from-milestone-title tests
;; ============================================================

(test-case "extract-version: v-prefixed title"
  (define result ((dynamic-script 'extract-version-from-milestone-title) "v0.99.40 — 2026-06-21"))
  (check-equal? result "0.99.40"))

(test-case "extract-version: bare version title"
  (define result ((dynamic-script 'extract-version-from-milestone-title) "0.99.40"))
  (check-equal? result "0.99.40"))

(test-case "extract-version: title with extra text"
  (define result
    ((dynamic-script 'extract-version-from-milestone-title)
     "v0.99.40 GitHub Release Automation Restoration"))
  (check-equal? result "0.99.40"))

(test-case "extract-version: malformed title returns #f"
  (define result ((dynamic-script 'extract-version-from-milestone-title) "no version here"))
  (check-false result))

;; ============================================================
;; validate-release-data: complete release passes
;; ============================================================

(test-case "validate-release-data: complete release passes"
  (define result ((dynamic-script 'validate-release-data) (complete-release-data) "0.99.40"))
  (check-true (car result) "should pass")
  (check-true (string-contains? (cadr result) "verified"))
  ;; Check the release hash structure
  (define release-hash (hash-ref (caddr result) 'release))
  (check-true (hash-ref release-hash 'exists))
  (check-true (hash-ref release-hash 'tarball_asset))
  (check-true (hash-ref release-hash 'manifest_asset))
  (check-true (hash-ref release-hash 'pass)))

;; ============================================================
;; validate-release-data: missing release fails
;; ============================================================

(test-case "validate-release-data: no release fails"
  (define result ((dynamic-script 'validate-release-data) #f "0.99.40"))
  (check-false (car result) "should fail")
  (check-true (string-contains? (cadr result) "No release"))
  (define release-hash (hash-ref (caddr result) 'release))
  (check-false (hash-ref release-hash 'exists))
  (check-false (hash-ref release-hash 'pass)))

;; ============================================================
;; validate-release-data: assetless release fails
;; ============================================================

(test-case "validate-release-data: assetless release fails"
  (define result ((dynamic-script 'validate-release-data) (assetless-release-data) "0.99.40"))
  (check-false (car result) "should fail")
  (check-true (string-contains? (cadr result) "missing all assets"))
  (define release-hash (hash-ref (caddr result) 'release))
  (check-false (hash-ref release-hash 'tarball_asset))
  (check-false (hash-ref release-hash 'manifest_asset)))

;; ============================================================
;; validate-release-data: tarball-only release fails
;; ============================================================

(test-case "validate-release-data: tarball-only release fails"
  (define result ((dynamic-script 'validate-release-data) (tarball-only-release-data) "0.99.40"))
  (check-false (car result) "should fail")
  (check-true (string-contains? (cadr result) "missing manifest"))
  (define release-hash (hash-ref (caddr result) 'release))
  (check-true (hash-ref release-hash 'tarball_asset))
  (check-false (hash-ref release-hash 'manifest_asset)))

;; ============================================================
;; validate-release-data: manifest-only release fails
;; ============================================================

(test-case "validate-release-data: manifest-only release fails"
  (define result ((dynamic-script 'validate-release-data) (manifest-only-release-data) "0.99.40"))
  (check-false (car result) "should fail")
  (check-true (string-contains? (cadr result) "missing tarball"))
  (define release-hash (hash-ref (caddr result) 'release))
  (check-false (hash-ref release-hash 'tarball_asset))
  (check-true (hash-ref release-hash 'manifest_asset)))

;; ============================================================
;; validate-release-data: draft release fails
;; ============================================================

(test-case "validate-release-data: draft release fails"
  (define data
    (make-release-data #:draft #t
                       #:assets (list (make-asset "q-0.99.40.tar.gz")
                                      (make-asset "release-manifest.json"))))
  (define result ((dynamic-script 'validate-release-data) data "0.99.40"))
  (check-false (car result) "should fail")
  (check-true (string-contains? (cadr result) "draft")))

;; ============================================================
;; validate-release-data: tag mismatch fails
;; ============================================================

(test-case "validate-release-data: tag mismatch fails"
  (define data
    (make-release-data #:tag-name "v0.99.39"
                       #:assets (list (make-asset "q-0.99.39.tar.gz")
                                      (make-asset "release-manifest.json"))))
  (define result ((dynamic-script 'validate-release-data) data "0.99.40"))
  (check-false (car result) "should fail")
  (check-true (string-contains? (cadr result) "mismatch")))

;; ============================================================
;; release-has-asset? helper tests
;; ============================================================

(test-case "release-has-asset?: finds existing asset"
  (define data (complete-release-data))
  (check-true ((dynamic-script 'release-has-asset?) data "q-0.99.40.tar.gz")))

(test-case "release-has-asset?: returns #f for missing asset"
  (define data (complete-release-data))
  (check-false ((dynamic-script 'release-has-asset?) data "nonexistent.zip")))

(test-case "release-has-asset?: handles #f release-data"
  (check-false ((dynamic-script 'release-has-asset?) #f "anything.zip")))

;; ============================================================
;; JSON structure tests
;; ============================================================

(test-case "make-release-check-result: produces valid structure"
  (define result ((dynamic-script 'make-release-check-result) #t "v0.99.40" #t #t #t "all good"))
  (define release-hash (hash-ref result 'release))
  (check-equal? (hash-ref release-hash 'exists) #t)
  (check-equal? (hash-ref release-hash 'tag) "v0.99.40")
  (check-equal? (hash-ref release-hash 'tarball_asset) #t)
  (check-equal? (hash-ref release-hash 'manifest_asset) #t)
  (check-equal? (hash-ref release-hash 'pass) #t)
  (check-equal? (hash-ref release-hash 'detail) "all good"))

;; ============================================================
;; Script existence tests
;; ============================================================

(test-case "milestone-gate.rkt exists and compiles"
  (check-true (file-exists? "../scripts/milestone-gate.rkt"))
  (check-not-exn (lambda () (dynamic-require "../scripts/milestone-gate.rkt" #f))))
