#lang racket

;; tests/test-main-entrypoint.rkt — Subprocess-level tests for main.rkt entry point
;; BOUNDARY: integration (process-level)
;;
;; Validates that `racket main.rkt --version`, `racket main.rkt --help`,
;; and related entrypoint paths work correctly at the process level.

(require rackunit
         racket/system
         racket/port
         racket/string
         (only-in "../interfaces/cli.rkt" parse-cli-args))

(define main-path "main.rkt")

(define (run-main . args)
  "Run main.rkt with given args as subprocess, return (values exit-code stdout stderr)"
  (define racket-bin (find-executable-path "racket"))
  (define stdout-out (open-output-string))
  (define stderr-out (open-output-string))
  (define all-args (cons main-path args))
  (define result (apply process*/ports stdout-out #f stderr-out racket-bin all-args))
  (define sp (list-ref result 0))
  (define out-in (list-ref result 1))
  (define pid (list-ref result 2))
  (define err-in (list-ref result 3))
  (define ctrl (list-ref result 4))
  (when out-in (close-output-port out-in))
  (ctrl 'wait)
  (define exit-code (ctrl 'exit-code))
  (values exit-code
          (get-output-string stdout-out)
          (get-output-string stderr-out)))

;; ============================================================
;; --version
;; ============================================================

(test-case "main.rkt --version exits 0 and prints version"
  (define-values (code out err) (run-main "--version"))
  (check-equal? code 0 (format "expected exit 0, got ~a. stderr: ~a" code err))
  (check-true (string-contains? out "q version")
              (format "expected 'q version' in output, got: ~a" out)))

;; ============================================================
;; --help
;; ============================================================

(test-case "main.rkt --help exits 0 and prints usage"
  (define-values (code out err) (run-main "--help"))
  (check-equal? code 0 (format "expected exit 0, got ~a. stderr: ~a" code err))
  (check-true (string-contains? out "Usage:")
              (format "expected 'Usage:' in output, got: ~a" out)))

;; ============================================================
;; --version via -V alias
;; ============================================================

(test-case "main.rkt -V exits 0 and prints version"
  (define-values (code out err) (run-main "-V"))
  (check-equal? code 0 (format "expected exit 0, got ~a. stderr: ~a" code err))
  (check-true (string-contains? out "q version")
              (format "expected 'q version' in output, got: ~a" out)))

;; ============================================================
;; --help via -h alias
;; ============================================================

(test-case "main.rkt -h exits 0 and prints usage"
  (define-values (code out err) (run-main "-h"))
  (check-equal? code 0 (format "expected exit 0, got ~a. stderr: ~a" code err))
  (check-true (string-contains? out "Usage:")
              (format "expected 'Usage:' in output, got: ~a" out)))

;; ============================================================
;; In-process zero-arg parse-cli-args test
;; ============================================================

(test-case "parse-cli-args zero-arg call works (in-process)"
  ;; This is the P0 bug: contract says (-> any/c cli-config?) requiring 1 arg
  ;; but implementation has optional arg. This test will FAIL until W1 fixes the contract.
  (define cfg (parse-cli-args))
  (check-pred hash? cfg "parse-cli-args with zero args should return a config"))

;; ============================================================
;; Doctor smoke test
;; ============================================================

(test-case "main.rkt doctor exits without crash"
  (define-values (code out err) (run-main "doctor"))
  ;; Doctor may exit non-zero if issues found, but should NOT crash
  (check-true (<= code 2)
              (format "doctor should exit 0-2, got ~a. stderr: ~a" code err)))
