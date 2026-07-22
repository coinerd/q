#lang racket/base

;; scripts/gsd-gates/gate-evidence.rkt — Release evidence verification
;;
;; Extracted from scripts/milestone-gate.rkt (v0.99.42+).
;; Pure functions for verifying GitHub release evidence.
;;
;; This module is DISTINCT from scripts/run-tests/gate-evidence.rkt,
;; which records test-run evidence. This module verifies release
;; artifacts (tarball, manifest) against milestone metadata.

(provide extract-version-from-milestone-title
         release-has-asset?
         make-release-check-result
         validate-release-data)

;; Extract version (X.Y.Z) from milestone title like "v0.99.40 ..." or "0.99.40 ..."
;; Returns string or #f.
(define (extract-version-from-milestone-title title)
  (define m (regexp-match #rx"v?([0-9]+\\.[0-9]+\\.[0-9]+)" title))
  (and m (cadr m)))

;; Check if release data has an asset with the given name.
(define (release-has-asset? release-data asset-name)
  (and release-data
       (hash? release-data)
       (let ([assets (hash-ref release-data 'assets '())])
         (for/or ([a (in-list assets)])
           (and (hash? a) (equal? (hash-ref a 'name #f) asset-name))))))

;; Build a release check result hash for JSON output.
(define (make-release-check-result exists tag tarball manifest pass detail)
  (hasheq 'release
          (hasheq 'exists
                  exists
                  'tag
                  (or tag "")
                  'tarball_asset
                  tarball
                  'manifest_asset
                  manifest
                  'pass
                  pass
                  'detail
                  detail)))

;; Validate release data for completeness.
;; Returns (list pass? detail-string release-check-hash).
;; release-data: jsexpr from GitHub API or #f
;; version: string like "0.99.40"
(define (validate-release-data release-data version)
  (define expected-tag (format "v~a" version))
  (define expected-tarball (format "q-~a.tar.gz" version))
  (cond
    ;; No release at all
    [(not release-data)
     (list #f
           (format "No release for ~a" expected-tag)
           (make-release-check-result #f #f #f #f #f (format "No release for ~a" expected-tag)))]
    ;; Release exists but is draft
    [(hash-ref release-data 'draft #f)
     (list #f
           (format "Release ~a is draft" expected-tag)
           (make-release-check-result #t
                                      expected-tag
                                      #f
                                      #f
                                      #f
                                      (format "Release ~a is draft" expected-tag)))]
    ;; Tag mismatch
    [(not (equal? (hash-ref release-data 'tag_name #f) expected-tag))
     (define actual (hash-ref release-data 'tag_name "?"))
     (list #f
           (format "Release tag mismatch: expected ~a, got ~a" expected-tag actual)
           (make-release-check-result #t
                                      actual
                                      #f
                                      #f
                                      #f
                                      (format "Tag mismatch: ~a ≠ ~a" actual expected-tag)))]
    [else
     ;; Check assets
     (define has-tarball (release-has-asset? release-data expected-tarball))
     (define has-manifest (release-has-asset? release-data "release-manifest.json"))
     (define all-ok (and has-tarball has-manifest))
     (define detail
       (cond
         [(and has-tarball has-manifest)
          (format "Release ~a verified: tarball + manifest present" expected-tag)]
         [(and (not has-tarball) (not has-manifest))
          (format "Release ~a missing all assets" expected-tag)]
         [(not has-tarball) (format "Release ~a missing tarball (~a)" expected-tag expected-tarball)]
         [else (format "Release ~a missing manifest" expected-tag)]))
     (list all-ok
           detail
           (make-release-check-result #t expected-tag has-tarball has-manifest all-ok detail))]))
