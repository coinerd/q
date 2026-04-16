#lang racket

;; tests/test-run-tests-script.rkt — Tests for scripts/run-tests.rkt
;;
;; NOTE: Only tests script metadata (exists, compiles, help).
;; Does NOT invoke full test suite (that would be recursive and slow).

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
      (check-not-false (string-contains? content "#lang racket/base")))))

(run-tests run-tests-script-tests)
