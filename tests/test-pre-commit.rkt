#lang racket

;; @speed slow
;; @suite default
;; @timeout 600

;; BOUNDARY: integration

;; q/tests/test-pre-commit.rkt — tests for scripts/pre-commit.rkt
;;
;; These tests verify the pre-commit hook runs lint checks via lint-all.rkt
;; and that --no-tests skips the test phase to avoid infinite recursion.

(require rackunit
         racket/port
         racket/file)

;; --- Resolve q/ directory from test location ---

(define q-dir
  (simplify-path
   (build-path (path-only (resolved-module-path-name (variable-reference->resolved-module-path
                                                      (#%variable-reference))))
               "..")))

;; --- Helper: run pre-commit with --no-tests and capture output + exit code ---

(define (run-pre-commit)
  (define-values (sp out in err)
    (parameterize ([current-directory q-dir])
      (subprocess #f #f #f (find-executable-path "racket") "scripts/pre-commit.rkt" "--no-tests")))
  (close-output-port in)
  ;; Wait with timeout (lint-all can be slow on first run)
  ;; @boundary integration  ;; @isolation process  ;; @mutates fs
  (define result (sync/timeout 120 sp))
  (when (not result)
    (subprocess-kill sp #t))
  (define code
    (if result
        (subprocess-status sp)
        1))
  (define out-str (port->string out))
  (close-input-port out)
  (close-input-port err)
  (values out-str code))

;; --- Lint checks appear in output ---

(test-case "lint-checks-run-via-lint-all"
  (define-values (out code) (run-pre-commit))
  ;; lint-all.rkt outputs "q CI Lint" banner
  (check-regexp-match #rx"q CI Lint" out)
  ;; version-sync check should appear
  (check-regexp-match #rx"version-sync" out))

(test-case "lint-checks-produce-version-sync-result"
  (define-values (out code) (run-pre-commit))
  ;; version-sync should appear as either PASS or FAIL
  (check-regexp-match #rx"version-sync" out))

(test-case "no-tests-flag-skips-test-phase"
  (define-values (out code) (run-pre-commit))
  ;; Should NOT contain "Affected Tests" section
  (check-false (regexp-match #rx"Affected Tests" out))
  ;; Should contain the --no-tests label
  (check-regexp-match #rx"--no-tests" out))

(test-case "lint-blocks-on-version-drift"
  ;; Run in an isolated disposable copy to avoid modifying the live workspace.
  ;; The copy contains only the minimal tree needed for the version-sync check
  ;; to run: info.rkt (with a drifted version), util/version.rkt (canonical),
  ;; scripts/pre-commit.rkt, scripts/lint-all.rkt, scripts/sync-version.rkt
  ;; (plus its dependencies version-surface.rkt and version-guard.rkt), and a
  ;; dummy main.rkt for pre-commit's guard check.
  ;;
  ;; Other lint checks (format, version-validate, etc.) have their scripts
  ;; missing in the copy, so lint-all.rkt SKIPs them. Only version-sync runs
  ;; and detects the drift -> [FAIL] version-sync + exit 1.
  ;;
  ;; Regression: verify live q/info.rkt bytes are unchanged after test.
  (define info-path (build-path q-dir "info.rkt"))
  (define original-bytes (file->bytes info-path))
  (define tmp-dir (make-temporary-directory))

  (define (cp src rel-dest)
    (define dest (build-path tmp-dir rel-dest))
    (make-directory* (path-only dest))
    (copy-file src dest))

  (dynamic-wind
   (lambda ()
     ;; Build isolated copy with drifted info.rkt version
     (cp (build-path q-dir "info.rkt") "info.rkt")
     (cp (build-path q-dir "util" "version.rkt") "util/version.rkt")
     (cp (build-path q-dir "scripts" "pre-commit.rkt") "scripts/pre-commit.rkt")
     (cp (build-path q-dir "scripts" "lint-all.rkt") "scripts/lint-all.rkt")
     (cp (build-path q-dir "scripts" "sync-version.rkt") "scripts/sync-version.rkt")
     (cp (build-path q-dir "scripts" "version-surface.rkt") "scripts/version-surface.rkt")
     (cp (build-path q-dir "scripts" "version-guard.rkt") "scripts/version-guard.rkt")
     ;; Dummy main.rkt for pre-commit's guard check
     (call-with-output-file (build-path tmp-dir "main.rkt")
                            (lambda (out) (display "" out))
                            #:exists 'truncate)
     ;; Break the copy's info.rkt version to cause drift
     (define copy-info (file->string (build-path tmp-dir "info.rkt")))
     (call-with-output-file
      (build-path tmp-dir "info.rkt")
      (lambda (out)
        (display (regexp-replace #rx"\"[0-9]+\\.[0-9]+\\.[0-9]+\"" copy-info "\"0.99.0\"") out))
      #:exists 'truncate))
   (lambda ()
     ;; Run pre-commit --no-tests from the isolated copy
     (define-values (sp out in err)
       (parameterize ([current-directory tmp-dir])
         (subprocess #f #f #f (find-executable-path "racket") "scripts/pre-commit.rkt" "--no-tests")))
     (close-output-port in)
     (define result (sync/timeout 120 sp))
     (when (not result)
       (subprocess-kill sp #t))
     (define code
       (if result
           (subprocess-status sp)
           1))
     (define out-str (port->string out))
     (close-input-port out)
     (close-input-port err)
     ;; version-sync check should FAIL
     (check-regexp-match #rx"\\[FAIL\\] version-sync" out-str)
     (check-not-equal? code 0)
     ;; Regression: live info.rkt bytes unchanged
     (check-equal? (file->bytes info-path) original-bytes))
   (lambda ()
     ;; Cleanup: recursively remove temp directory
     (when (directory-exists? tmp-dir)
       (delete-directory/files tmp-dir)))))

;; --- Mode label ---

(test-case "mode-label-reflects-no-tests"
  (define-values (out code) (run-pre-commit))
  (check-regexp-match #rx"lint only" out))

;; --- Source code: file-exists? guards ---

(test-case "lint-script-has-file-exists-guard"
  (define src (file->string (build-path q-dir "scripts" "pre-commit.rkt")))
  (check-regexp-match #rx"lint-all\\.rkt" src)
  (check-regexp-match #rx"file-exists\\?" src))

(test-case "pre-commit-delegates-to-lint-all"
  (define src (file->string (build-path q-dir "scripts" "pre-commit.rkt")))
  ;; Should reference lint-all.rkt for all lint checks
  (check-regexp-match #rx"lint-all\\.rkt" src)
  ;; Should have fast-lint-checks list
  (check-regexp-match #rx"fast-lint-checks" src))

;; --- Staged lint must check canonicality WITHOUT rewriting files ---

(test-case "staged-lint-never-runs-raco-fmt-i"
  ;; The old behavior ran `raco fmt -i` during the hook: the index snapshot
  ;; kept the unformatted content while the working tree was silently
  ;; rewritten, dirtying the tree after the commit landed.
  (define src (file->string (build-path q-dir "scripts" "pre-commit.rkt")))
  ;; No EXECUTION of raco fmt -i remains (prose in the remediation hint is fine).
  (check-false (regexp-match #rx"raco fmt -i ~a" src))
  (check-regexp-match #rx"RE-STAGE" src))

(test-case "check-fmt-canonical-detects-and-never-rewrites"
  (define check-fmt-canonical
    (dynamic-require (build-path q-dir "scripts" "pre-commit.rkt") 'check-fmt-canonical))
  (define tmp-path (make-temporary-file "pc-fmt-~a.rkt" #f q-dir))
  (dynamic-wind
   (lambda () (void))
   (lambda ()
     ;; Non-canonical: raco-fmt would wrap the body onto its own line.
     (define bad "#lang racket\n(define (f x)x)\n")
     (call-with-output-file tmp-path (lambda (out) (display bad out)) #:exists 'truncate)
     (check-eq? (check-fmt-canonical tmp-path) 'not-canonical)
     ;; The file must be byte-identical after the check.
     (check-equal? (file->string tmp-path) bad)
     ;; Canonical content passes.
     (define good "#lang racket\n(define (f x)\n  x)\n")
     (call-with-output-file tmp-path (lambda (out) (display good out)) #:exists 'truncate)
     (check-eq? (check-fmt-canonical tmp-path) 'canonical)
     (check-equal? (file->string tmp-path) good))
   (lambda ()
     (when (file-exists? tmp-path)
       (delete-file tmp-path)))))
