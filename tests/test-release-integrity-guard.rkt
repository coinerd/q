#lang racket

;; @speed fast
;; @suite fast
;; @boundary integration

;; test-release-integrity-guard.rkt
;; Tests for release integrity guard (scripts/release-integrity-guard.rkt)
;; Phases 5, 12, 11: characterization, probe, intentional-change
;;
;; Three test categories:
;;   1. Characterization — snapshot protected artifacts, run a representative
;;      release transformation, verify byte-identical.
;;   2. Probe (negative) — artificially mutate a protected file mid-release,
;;      verify the guard catches it with a loud error.
;;   3. Intentional-change — pre-modify a protected file before release start,
;;      verify automation preserves (not erases) the developer-authored change.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         racket/port
         racket/list
         racket/string
         racket/runtime-path
         "../scripts/release-integrity-guard.rkt")

(define-runtime-path repo-root "..")

;; ═══════════════════════════════════════════════════════════════════════════
;; Helpers
;; ═══════════════════════════════════════════════════════════════════════════

;; Copy docs/architecture/*.rktd to a temp directory, so we can mutate
;; without touching the real repo. The temp tree mirrors the project
;; structure (docs/architecture/*.rktd) so that load-protected-artifacts
;; and snapshot-artifacts resolve paths correctly.
(define (make-temp-repo-tree)
  (define tmp (make-temporary-file "release-integ-~a" 'directory))
  (define arch-src (simple-form-path (build-path repo-root "docs" "architecture")))
  (define arch-dst (build-path tmp "docs" "architecture"))
  (make-directory* arch-dst)
  (for ([f (in-directory arch-src)]
        #:when (and (file-exists? f)
                    (let ([ext (filename-extension f)])
                      (and ext (equal? (bytes->string/utf-8 ext) "rktd")))))
    (define rel (find-relative-path arch-src f))
    (copy-file f (build-path arch-dst rel)))
  tmp)

(define (cleanup-temp-repo-tree tmp)
  (when (and tmp (directory-exists? tmp))
    (delete-directory/files tmp)))

;; ═══════════════════════════════════════════════════════════════════════════
;; Characterization tests
;; ═══════════════════════════════════════════════════════════════════════════

(define-test-suite characterization-tests
                   ;; Test 1: Characterization — artifacts unchanged after no-op
                   (test-case "protected artifacts byte-identical after no-op automation"
                     (define tmp (make-temp-repo-tree))
                     (define before (snapshot-artifacts tmp))
                     ;; Run a no-op "automation" step (just read files, don't modify)
                     (define after (snapshot-artifacts tmp))
                     (define violations (compare-snapshots before after))
                     (check-true (null? violations) "no mutations should be detected")
                     (cleanup-temp-repo-tree tmp)))

;; ═══════════════════════════════════════════════════════════════════════════
;; Probe tests (negative — mutations must be caught)
;; ═══════════════════════════════════════════════════════════════════════════

(define-test-suite
 probe-tests
 ;; Test 2: Probe — artificial mutation caught
 (test-case "artificial mutation of protected file is caught"
   (define tmp (make-temp-repo-tree))
   (define before (snapshot-artifacts tmp))
   ;; Simulate accidental mutation: append a comment to a .rktd file
   (define target (build-path tmp "docs" "architecture" "dependency-policy.rktd"))
   (call-with-output-file target
                          #:exists 'append
                          (lambda (out) (displayln "\n;; ACCIDENTAL MUTATION" out)))
   (define after (snapshot-artifacts tmp))
   (define violations (compare-snapshots before after))
   (check-not-false violations "mutation must be detected")
   (check-equal? (length violations) 1)
   (check-equal? (integrity-violation-path (car violations))
                 "docs/architecture/dependency-policy.rktd")
   (check-equal? (integrity-violation-reason (car violations)) 'byte-changed)
   ;; Verify the report message is loud and actionable
   (define report (format-violation-report violations))
   (check-true (string-contains? report "INTEGRITY VIOLATION"))
   (check-true (string-contains? report "dependency-policy.rktd"))
   (check-true (string-contains? report "not part of the declared release change set"))
   (cleanup-temp-repo-tree tmp))
 ;; Test 3: Probe — file removal caught
 ;; When a file is deleted, snapshot-artifacts still creates a snapshot entry
 ;; with sha256=#f (because the path is still in the registry). The sha
 ;; changes from the original hash to #f, producing a 'byte-changed violation.
 ;; The 'file-removed reason only fires when the path disappears from the
 ;; after snapshots entirely (e.g., removed from the registry).
 (test-case "removal of protected file is caught"
   (define tmp (make-temp-repo-tree))
   (define before (snapshot-artifacts tmp))
   (delete-file (build-path tmp "docs" "architecture" "parameter-inventory.rktd"))
   (define after (snapshot-artifacts tmp))
   (define violations (compare-snapshots before after))
   (check-not-false violations "file removal must be detected")
   (check-equal? (length violations) 1)
   (check-equal? (integrity-violation-path (car violations))
                 "docs/architecture/parameter-inventory.rktd")
   (check-equal? (integrity-violation-reason (car violations)) 'byte-changed)
   (check-false (integrity-violation-after-sha (car violations))
                "after-sha should be #f for deleted file")
   (cleanup-temp-repo-tree tmp))
 ;; Test 3b: Direct 'file-removed reason — when artifact disappears from
 ;; the after snapshots entirely (path not in registry anymore)
 (test-case "file-removed violation when artifact disappears from after snapshot"
   (define before (list (integrity-snapshot "docs/architecture/test.rktd" "abc123")))
   (define after '()) ;; artifact disappeared — no after snapshot
   (define violations (compare-snapshots before after))
   (check-equal? (length violations) 1)
   (check-equal? (integrity-violation-reason (car violations)) 'file-removed)
   (check-equal? (integrity-violation-path (car violations)) "docs/architecture/test.rktd"))
 ;; Test 3c: Direct 'file-added reason — new artifact appears in after
 (test-case "file-added violation when new artifact appears in after snapshot"
   (define before '())
   (define after (list (integrity-snapshot "docs/architecture/new.rktd" "def456")))
   (define violations (compare-snapshots before after))
   (check-equal? (length violations) 1)
   (check-equal? (integrity-violation-reason (car violations)) 'file-added)
   (check-equal? (integrity-violation-path (car violations)) "docs/architecture/new.rktd"))
 ;; Test 3d: format-violation-report covers all three reason types
 (test-case "format-violation-report covers file-removed and file-added"
   (define removed-violation
     (integrity-violation "docs/architecture/gone.rktd" "abc" #f 'file-removed))
   (define removed-report (format-violation-report (list removed-violation)))
   (check-true (string-contains? removed-report "INTEGRITY VIOLATION"))
   (check-true (string-contains? removed-report "gone.rktd"))
   (check-true (string-contains? removed-report "removed"))

   (define added-violation (integrity-violation "docs/architecture/new.rktd" #f "def" 'file-added))
   (define added-report (format-violation-report (list added-violation)))
   (check-true (string-contains? added-report "INTEGRITY VIOLATION"))
   (check-true (string-contains? added-report "new.rktd"))
   (check-true (string-contains? added-report "appeared"))))

;; ═══════════════════════════════════════════════════════════════════════════
;; Intentional-change tests
;; ═══════════════════════════════════════════════════════════════════════════

(define-test-suite
 intentional-change-tests
 ;; Test 4: Intentional-change — pre-existing modification preserved
 (test-case "pre-release intentional change is allowed and preserved"
   (define tmp (make-temp-repo-tree))
   ;; Developer modifies a protected file BEFORE release start (intentional)
   (define target (build-path tmp "docs" "architecture" "dependency-policy.rktd"))
   (call-with-output-file target
                          #:exists 'append
                          (lambda (out) (displayln "\n;; INTENTIONAL PRE-RELEASE CHANGE" out)))
   (define intended-content (file->bytes target))
   ;; Snapshot at release start — captures the intentional change
   (define before (snapshot-artifacts tmp))
   ;; Run "automation" (no-op) — must not erase or modify the intentional change
   (define after (snapshot-artifacts tmp))
   (define violations (compare-snapshots before after))
   (check-true (null? violations) "intentional pre-release change must not trigger violation")
   ;; Verify the file still has the intentional content (not reverted)
   (check-equal? (file->bytes target) intended-content)
   (cleanup-temp-repo-tree tmp)))

;; ═══════════════════════════════════════════════════════════════════════════
;; Syntax validation and registry self-consistency tests
;; ═══════════════════════════════════════════════════════════════════════════

(define-test-suite
 syntax-and-registry-tests
 ;; Test 5: .rktd syntax validation — all protected .rktd files parse
 (test-case "validate-rktd-syntax passes for all protected .rktd files"
   (define artifacts (load-protected-artifacts repo-root))
   (for ([entry (in-list artifacts)]
         #:when (string-suffix? (car entry) ".rktd"))
     (define result (validate-rktd-syntax (build-path repo-root (car entry))))
     (check-true (eq? result #t) (format "rktd syntax invalid for ~a: ~a" (car entry) result))))
 ;; Test 6: Registry self-consistency — all listed files exist
 (test-case "registry lists only existing files"
   (define artifacts (load-protected-artifacts repo-root))
   (for ([entry (in-list artifacts)])
     (check-true (file-exists? (build-path repo-root (car entry)))
                 (format "registry references missing file: ~a" (car entry)))))
 ;; Test 7: validate-rktd-syntax catches broken syntax
 (test-case "validate-rktd-syntax catches malformed .rktd"
   (define tmp (make-temporary-file "bad-rktd-~a.rktd"))
   (call-with-output-file tmp #:exists 'truncate (lambda (out) (displayln "((unclosed paren" out)))
   (define result (validate-rktd-syntax tmp))
   (check-true (string? result) "malformed rktd should return error string")
   (check-true (> (string-length result) 0) "error message should be non-empty")
   (delete-file tmp)))

;; ═══════════════════════════════════════════════════════════════════════════
;; Combined suite
;; ═══════════════════════════════════════════════════════════════════════════

(define-test-suite all-release-integrity-guard-tests
                   characterization-tests
                   probe-tests
                   intentional-change-tests
                   syntax-and-registry-tests)

(module+ test
  (run-tests all-release-integrity-guard-tests))

(module+ main
  (run-tests all-release-integrity-guard-tests))
