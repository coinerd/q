#lang racket

;; q/tests/test-pre-commit.rkt — tests for scripts/pre-commit.rkt
;;
;; These tests verify the pre-commit hook phases including version
;; sync lint and metrics lint added in Wave 1 (#1752).

(require rackunit
         racket/port
         racket/file)

;; --- Resolve q/ directory from test location ---

(define q-dir
  (simplify-path
   (build-path (path-only (resolved-module-path-name (variable-reference->resolved-module-path
                                                      (#%variable-reference))))
               "..")))

;; --- Helper: run pre-commit and capture output + exit code ---

(define (run-pre-commit)
  (define cmd (format "cd ~a && racket scripts/pre-commit.rkt" q-dir))
  (define port (open-input-string (with-output-to-string (lambda () (void (system cmd))))))
  (define exit-code
    (parameterize ([current-directory q-dir])
      (system/exit-code "racket scripts/pre-commit.rkt")))
  (values (port->string port) exit-code))

;; --- Version lint ---

(test-case "version-lint-phase-appears-in-output"
  (define-values (out code) (run-pre-commit))
  (check-regexp-match #rx"Version Sync Lint" out)
  (check-regexp-match #rx"Version sync:" out))

(test-case "version-lint-passes-when-in-sync"
  ;; With correct version, should report PASS
  (define-values (out code) (run-pre-commit))
  (check-regexp-match #rx"Version sync: PASS" out))

(test-case "version-lint-blocks-on-drift"
  ;; Break info.rkt version → pre-commit should detect drift
  (define info-path (build-path q-dir "info.rkt"))
  (define original (file->string info-path))
  (dynamic-wind
   (lambda ()
     ;; Replace version with 0.99.0 in info.rkt
     (call-with-output-file
      info-path
      (lambda (out)
        (display (regexp-replace #rx"\"[0-9]+\\.[0-9]+\\.[0-9]+\"" original "\"0.99.0\"") out))
      #:exists 'truncate))
   (lambda ()
     (define-values (out code) (run-pre-commit))
     ;; sync-version.rkt exits 1 on drift → pre-commit reports FAIL
     (check-regexp-match #rx"Version sync: FAIL" out)
     (check-not-equal? code 0))
   (lambda ()
     (call-with-output-file info-path (lambda (out) (display original out)) #:exists 'truncate))))

;; --- Metrics lint ---

(test-case "metrics-lint-phase-appears-in-output"
  (define-values (out code) (run-pre-commit))
  (check-regexp-match #rx"Metrics Lint" out))

(test-case "metrics-lint-passes-when-in-sync"
  (define-values (out code) (run-pre-commit))
  (check-regexp-match #rx"Metrics lint: PASS" out))

;; --- Mode label ---

(test-case "default-mode-label-includes-version-and-metrics"
  (define-values (out code) (run-pre-commit))
  (check-regexp-match #rx"format \\+ version \\+ metrics \\+ tests" out))

;; --- Phase ordering ---

(test-case "phases-run-in-correct-order"
  (define-values (out code) (run-pre-commit))
  (define fmt-pos (regexp-match-positions #rx"--- Format Lint ---" out))
  (define ver-pos (regexp-match-positions #rx"--- Version Sync Lint" out))
  (define met-pos (regexp-match-positions #rx"--- Metrics Lint" out))
  (check-true (and fmt-pos ver-pos (< (caar fmt-pos) (caar ver-pos)))
              "Format lint should run before version lint")
  (check-true (and ver-pos met-pos (< (caar ver-pos) (caar met-pos)))
              "Version lint should run before metrics lint"))

;; --- Source code: file-exists? guards ---

(test-case "version-lint-has-file-exists-guard"
  (define src (file->string (build-path q-dir "scripts" "pre-commit.rkt")))
  (check-regexp-match #rx"sync-version\\.rkt" src)
  (check-regexp-match #rx"file-exists\\?" src))

(test-case "metrics-lint-has-file-exists-guard"
  (define src (file->string (build-path q-dir "scripts" "pre-commit.rkt")))
  (check-regexp-match #rx"metrics\\.rkt" src)
  (check-regexp-match #rx"file-exists\\?" src))
