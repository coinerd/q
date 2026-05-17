#lang racket

;; tests/test-define-extension-expand.rkt — Extension struct tests (F17)
;; BOUNDARY: pure

(require rackunit
         rackunit/text-ui
         "../util/extensions.rkt")

(define expansion-tests
  (test-suite "define-extension-expand"

    (test-case "extension struct predicate"
      (check-true (procedure? extension?)))

    (test-case "extension struct accessors"
      (check-true (procedure? extension-name))
      (check-true (procedure? extension-version))
      (check-true (procedure? extension-hooks)))

    (test-case "extension construction"
      (define ext (extension "test" "1.0" "1" (hash)))
      (check-equal? (extension-name ext) "test")
      (check-equal? (extension-version ext) "1.0")
      (check-equal? (extension-api-version ext) "1")
      (check-equal? (extension-hooks ext) (hash)))))

(run-tests expansion-tests)
