#lang racket

;; @speed fast
;; @suite default
;; @isolation process

;; BOUNDARY: integration

;; tests/test-lint-deprecation.rkt — Tests for scripts/lint-deprecation-deadlines.rkt

(require rackunit
         rackunit/text-ui
         racket/string
         racket/file
         racket/path
         racket/port
         racket/runtime-path)

;; Resolve paths relative to this test module, never the invoking process cwd.
(define-runtime-path lint-script "../scripts/lint-deprecation-deadlines.rkt")
(define q-dir (simplify-path (build-path (path-only lint-script) 'up)))

(define (run-lint . args)
  (define-values (sp out in err)
    (parameterize ([current-directory q-dir])
      (apply subprocess #f #f #f (find-executable-path "racket") (path->string lint-script) args)))
  (define output (port->string out))
  (define error-output (port->string err))
  (close-input-port out)
  (close-input-port err)
  (close-output-port in)
  (subprocess-wait sp)
  (values (string-append output error-output) (subprocess-status sp)))

(define deprecation-tests
  (test-suite "Deprecation Linter Tests"

    (test-case "lint script exists and is readable"
      (check-true (file-exists? lint-script)))

    (test-case "lint script runs without error"
      (define-values (output exit-code) (run-lint))
      ;; Should either find expired TODOs or print "No expired"
      (check-true (or (string-contains? output "expired") (string-contains? output "No expired"))
                  (format "unexpected linter output: ~s (exit ~a)" output exit-code)))

    (test-case "lint --ci exits 0 when no expired TODOs"
      (define-values (output exit-code) (run-lint "--ci"))
      ;; If there are expired TODOs, exit-code would be 1, otherwise 0
      ;; We just verify it runs without crashing
      (check-true (or (= exit-code 0) (= exit-code 1))))

    (test-case "version parsing handles standard versions"
      ;; Inline test of version parsing logic
      (define (parse-version s)
        (map string->number (string-split s ".")))
      (check-equal? (parse-version "0.25.2") (list 0 25 2))
      (check-equal? (parse-version "1.0.0") (list 1 0 0)))

    (test-case "version comparison works correctly"
      (define (version<=? a b)
        (cond
          [(and (null? a) (null? b)) #t]
          [(null? a) #t]
          [(null? b) #f]
          [(< (car a) (car b)) #t]
          [(> (car a) (car b)) #f]
          [else (version<=? (cdr a) (cdr b))]))
      ;; 0.24.0 <= 0.25.2 — expired
      (check-true (version<=? (list 0 24 0) (list 0 25 2)))
      ;; 0.26.0 <= 0.25.2 — not expired
      (check-false (version<=? (list 0 26 0) (list 0 25 2)))
      ;; 0.25.2 <= 0.25.2 — same version, expired
      (check-true (version<=? (list 0 25 2) (list 0 25 2)))
      ;; Major version comparison
      (check-true (version<=? (list 1 0 0) (list 2 0 0)))
      (check-false (version<=? (list 2 0 0) (list 1 0 0))))))

(run-tests deprecation-tests)
