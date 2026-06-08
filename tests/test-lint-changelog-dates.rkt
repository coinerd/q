#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; tests/test-lint-changelog-dates.rkt — Unit tests for lint-changelog-dates.rkt

(require rackunit
         racket/file
         racket/path
         racket/string
         racket/port
         racket/system)

;; ── Helpers ──

(define q-dir
  (simplify-path
   (build-path (path-only (resolved-module-path-name (variable-reference->resolved-module-path
                                                      (#%variable-reference))))
               "..")))

(define (run-validator input-text)
  ;; Write CHANGELOG.md to a temp dir, run validator, return exit code + output
  (define tmp-dir (make-temporary-file "changelog-test-~a" 'directory))
  (define changelog-path (build-path tmp-dir "CHANGELOG.md"))
  (with-output-to-file changelog-path (lambda () (display input-text)))
  (define script-path (build-path q-dir "scripts" "lint-changelog-dates.rkt"))
  (define-values (sp out in err)
    (subprocess #f
                #f
                #f
                (find-executable-path "racket")
                (path->string script-path)
                "--changelog"
                (path->string changelog-path)))
  (close-output-port in)
  (define stdout-text (port->string out))
  (close-input-port out)
  (close-input-port err)
  (subprocess-wait sp)
  (define exit-code (subprocess-status sp))
  (delete-directory/files tmp-dir)
  (values exit-code stdout-text))

;; ── Test: Valid CHANGELOG ──

(define valid-changelog
  "# Changelog

## v0.1.2 — 2026-01-02

### Some Feature

- Did a thing

## v0.1.1 — 2026-01-01

### Another Feature

- Did another thing
")

;; ── Test: Future date ──

(define future-date-changelog
  "# Changelog

## v0.1.2 — 2099-12-31

### Future Feature

- Not yet released
")

;; ── Test: Missing date ──

(define missing-date-changelog "# Changelog

## v0.1.2

### No Date

- Missing date field
")

;; ── Test: Out of order (warning only) ──

(define out-of-order-changelog
  "# Changelog

## v0.1.1 — 2026-01-01

### First

- Thing

## v0.1.2 — 2026-01-02

### Second

- Thing
")

;; ── Tests ──

(test-case "valid changelog passes"
  (define-values (code out) (run-validator valid-changelog))
  (check-equal? code 0)
  (check-true (string-contains? out "CHANGELOG dates OK")))

(test-case "future date fails"
  (define-values (code out) (run-validator future-date-changelog))
  (check-equal? code 1)
  (check-true (string-contains? out "ERROR"))
  (check-true (string-contains? out "future date")))

(test-case "missing date fails"
  (define-values (code out) (run-validator missing-date-changelog))
  (check-equal? code 1)
  (check-true (string-contains? out "ERROR"))
  (check-true (string-contains? out "missing date")))

(test-case "out of order is a warning (passes)"
  (define-values (code out) (run-validator out-of-order-changelog))
  (check-equal? code 0)
  (check-true (string-contains? out "WARNING")))
