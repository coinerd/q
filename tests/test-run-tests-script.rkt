#lang racket

;; @speed slow
;; @suite default
;; @isolation process

;; BOUNDARY: integration

;; tests/test-run-tests-script.rkt — Tests for scripts/run-tests.rkt
;;
;; NOTE: Only tests script metadata (exists, compiles, help).
;; Does NOT invoke full test suite (that would be recursive and slow).
;; @boundary unit  ;; @mutates fs

(require rackunit
         rackunit/text-ui
         racket/runtime-path)

;; Resolve to the directory containing this test file
(define-runtime-path here ".")
(define project-root (simplify-path (build-path here ".."))) ;; q/ root

(define script-path (build-path project-root "scripts" "run-tests.rkt"))

(define (q-system/cmd cmd)
  (parameterize ([current-directory project-root])
    (system/exit-code cmd)))

(define run-tests-script-tests
  (test-suite "run-tests script"

    (test-case "script file exists"
      (check-true (file-exists? script-path)))

    (test-case "script compiles without error"
      (define exit-code (q-system/cmd "raco make scripts/run-tests.rkt 2>&1"))
      (check-equal? exit-code 0))

    (test-case "--help exits successfully"
      (define exit-code (q-system/cmd "racket scripts/run-tests.rkt --help 2>&1"))
      (check-equal? exit-code 0))

    (test-case "--suite fast help accepted"
      (define exit-code (q-system/cmd "racket scripts/run-tests.rkt --suite fast --help 2>&1"))
      (check-equal? exit-code 0))

    (test-case "--sequential flag accepted"
      (define exit-code (q-system/cmd "racket scripts/run-tests.rkt --sequential --help 2>&1"))
      (check-equal? exit-code 0))

    (test-case "script uses racket/base"
      (define content (file->string script-path))
      (check-not-false (string-contains? content "#lang racket/base")))

    ;; BUG-0033 (v1.00.20 W5): the canonical runner is
    ;; q/scripts/run-tests.rkt; it must at least LOAD from an arbitrary
    ;; cwd (the wave-doc convention `cd <project-base>/q && racket
    ;; scripts/run-tests.rkt` additionally holds, but loading must not
    ;; silently depend on the invocation directory).
    (test-case "documented entry point loads from an arbitrary cwd (BUG-0033)"
      (parameterize ([current-directory (find-system-path 'temp-dir)])
        (define exit-code
          (system*/exit-code (find-executable-path "racket") (path->string script-path) "--help"))
        (check-equal? exit-code 0)))

    ;; BUG-0033 (v1.00.20 W5): every tracked test file must be invocable
    ;; via `racket <file>` from ANY cwd (spot-check from the system temp
    ;; dir; broader battery in tests/test-cwd-independence.rkt).
    (test-case "tracked tests invocable from arbitrary cwd - spot-check (BUG-0033)"
      (parameterize ([current-directory (find-system-path 'temp-dir)])
        (define exit-code
          (system*/exit-code
           (find-executable-path "racket")
           (path->string (build-path project-root "tests" "test-ui-action-adapters.rkt"))))
        (check-equal? exit-code 0)))))

(run-tests run-tests-script-tests)
