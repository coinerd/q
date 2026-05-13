#lang racket

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
  (define cmd (format "cd ~a && racket scripts/pre-commit.rkt --no-tests" q-dir))
  (define-values (exit-code out)
    (parameterize ([current-directory q-dir])
      (let ([sp-out (open-output-string)])
        (define code
          (parameterize ([current-output-port sp-out])
            (system/exit-code "racket scripts/pre-commit.rkt --no-tests")))
        (values code (get-output-string sp-out)))))
  (values out exit-code))

;; --- Lint checks appear in output ---

(test-case "lint-checks-run-via-lint-all"
  (define-values (out code) (run-pre-commit))
  ;; lint-all.rkt outputs "q CI Lint" banner
  (check-regexp-match #rx"q CI Lint" out)
  ;; version-sync check should appear
  (check-regexp-match #rx"version-sync" out))

(test-case "lint-checks-pass-when-in-sync"
  (define-values (out code) (run-pre-commit))
  (check-regexp-match #rx"\\[PASS\\] version-sync" out))

(test-case "no-tests-flag-skips-test-phase"
  (define-values (out code) (run-pre-commit))
  ;; Should NOT contain "Affected Tests" section
  (check-false (regexp-match #rx"Affected Tests" out))
  ;; Should contain the --no-tests label
  (check-regexp-match #rx"--no-tests" out))

(test-case "lint-blocks-on-version-drift"
  ;; Break info.rkt version → pre-commit should detect drift
  (define info-path (build-path q-dir "info.rkt"))
  (define original (file->string info-path))
  (dynamic-wind
   (lambda ()
     (call-with-output-file
      info-path
      (lambda (out)
        (display (regexp-replace #rx"\"[0-9]+\\.[0-9]+\\.[0-9]+\"" original "\"0.99.0\"") out))
      #:exists 'truncate))
   (lambda ()
     (define-values (out code) (run-pre-commit))
     ;; version-sync check should FAIL
     (check-regexp-match #rx"\\[FAIL\\] version-sync" out)
     (check-not-equal? code 0))
   (lambda ()
     (call-with-output-file info-path (lambda (out) (display original out)) #:exists 'truncate))))

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
