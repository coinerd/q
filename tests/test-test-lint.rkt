#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: unit
;; @suite runtime
;; @boundary unit
;; @speed fast
;; @mutates none
;; Tests for lint-tests.rkt v0.83.4 checks (#6748)

(require racket/string
         racket/system
         racket/list
         rackunit
         racket/port
         racket/file)

;; We test the lint checks by creating temp files and running the checks directly.
;; Since lint-tests.rkt runs (main) at module load, we can't require it directly.
;; Instead, we duplicate the check logic for unit testing.

;; ---------------------------------------------------------------------------
;; Helper: simulate check-metadata-presence
;; ---------------------------------------------------------------------------

(define (has-metadata-tag? lines tag)
  (for/or ([line (in-list lines)]
           [i (in-naturals 1)]
           #:break (> i 30))
    (regexp-match? (regexp-quote (string-append "@" tag)) line)))

(test-case "has-metadata-tag? detects @suite"
  (check-true (has-metadata-tag? '(";; @suite tui") "suite"))
  (check-false (has-metadata-tag? '(";; no tag here") "suite")))

(test-case "has-metadata-tag? detects @speed"
  (check-true (has-metadata-tag? '(";; @speed fast") "speed"))
  (check-false (has-metadata-tag? '(";; @suite tui") "speed")))

(test-case "has-metadata-tag? detects @mutates"
  (check-true (has-metadata-tag? '(";; @mutates env") "mutates")))

(test-case "has-metadata-tag? ignores deep lines"
  ;; Lines beyond 30 are not scanned
  (define lines (append (build-list 30 (lambda (_) "")) '(";; @speed fast")))
  (check-false (has-metadata-tag? lines "speed")))

;; ---------------------------------------------------------------------------
;; Helper: simulate check-local-with-temp-dir
;; ---------------------------------------------------------------------------

(define approved-helpers
  '("tests/helpers/temp-fs.rkt"
    "tests/helpers/fixtures.rkt"
    "tests/helpers/test-sandbox.rkt"
    "tests/workflows/fixtures/temp-project.rkt"))

(define (is-approved-helper? filepath)
  (for/or ([m (in-list approved-helpers)])
    (string-suffix? filepath m)))

(test-case "approved helper detection"
  (check-true (is-approved-helper? "q/tests/helpers/fixtures.rkt"))
  (check-true (is-approved-helper? "tests/helpers/temp-fs.rkt"))
  (check-false (is-approved-helper? "tests/test-foo.rkt"))
  (check-false (is-approved-helper? "tests/helpers/session-fixture.rkt")))

;; ---------------------------------------------------------------------------
;; Helper: simulate check-env-mutation-tag
;; ---------------------------------------------------------------------------

(define (has-env-mutation-tag? lines)
  (for/or ([line (in-list lines)]
           [i (in-naturals 1)]
           #:break (> i 30))
    (or (regexp-match? #rx";+[ \t]*@mutates" line)
        (regexp-match? #rx";+[ \t]*@isolation" line))))

(define (has-putenv? lines)
  (for/or ([line (in-list lines)])
    (string-contains? line "putenv")))

(test-case "env mutation detection"
  (check-false (has-env-mutation-tag? '("#lang racket" "(test-case \"foo\" ...)")))
  (check-true (has-env-mutation-tag? '(";; @mutates env")))
  (check-true (has-env-mutation-tag? '(";; @isolation sandboxed-home")))
  (check-true (has-putenv? '("(putenv \"HOME\" \"/tmp\")")))
  (check-false (has-putenv? '("(getenv \"HOME\")"))))

;; ---------------------------------------------------------------------------
;; Integration: lint script runs without crash
;; ---------------------------------------------------------------------------

(test-case "lint-tests.rkt runs"
  (define orig-dir (current-directory))
  (dynamic-wind
    (lambda () (current-directory (build-path orig-dir "..")))
    (lambda ()
      (define-values (sp stdout-in stdin-out stderr-in)
        (subprocess #f #f #f (find-executable-path "racket") "scripts/lint-tests.rkt"))
      (define output (port->string stdout-in))
      (close-input-port stdout-in)
      (close-input-port stderr-in)
      (close-output-port stdin-out)
      (define exit-code (subprocess-status sp))
      ;; Lint may have errors (exit 1) but should not crash (exit 2)
      (check-true (<= exit-code 1))
      (check-true (string-contains? output "---")))
    (lambda () (current-directory orig-dir))))
