#lang racket/base

;; @speed fast  ;; @suite extensions

;; tests/test-gsd-plan-validator-unit.rkt — Unit tests for GSD plan validator (T-6)

(require racket/base
         racket/string
         rackunit
         rackunit/text-ui
         "../extensions/gsd/plan-types.rkt"
         "../extensions/gsd/plan-validator.rkt")

;; Helper constructors
(define (make-wave #:index [idx 0]
                   #:title [title "Test wave"]
                   #:files [files '("src/foo.rkt")]
                   #:verify [verify "raco test"]
                   #:tasks [tasks '()])
  (gsd-wave idx title 'pending "" files tasks verify '()))

(define (make-plan #:waves [waves '()])
  (gsd-plan waves #f '() '()))

(define (make-norm-wave #:index [idx 0]
                        #:title [title "Test wave"]
                        #:files [files '("src/foo.rkt")]
                        #:verify [verify "raco test"])
  (gsd-normalized-wave idx title '("do task") verify '() files 'pending))

(define (make-norm-plan #:waves [waves '()])
  (gsd-normalized-plan waves #f (hasheq) #f))

(define suite
  (test-suite "GSD plan validator unit tests"

    ;; === validate-plan-strict ===

    (test-case "valid plan → empty errors"
      (define plan (make-plan #:waves (list (make-wave))))
      (define result (validate-plan-strict plan))
      (check-equal? (validation-result-errors result) '()))

    (test-case "plan with no waves → error"
      (define plan (make-plan #:waves '()))
      (define result (validate-plan-strict plan))
      (check-not-false (member "Plan has no waves" (validation-result-errors result))))

    (test-case "wave with no files → warning, not error (v0.75.8 relaxed)"
      (define plan (make-plan #:waves (list (make-wave #:files '() #:verify ""))))
      (define result (validate-plan-strict plan))
      ;; v0.75.8: docs-only plans with no file refs are now warnings, not errors
      (check-not-false (member "Plan has no file references in any wave — docs-only plan"
                               (validation-result-warnings result))))

    (test-case "wave with no title → warnings"
      (define plan (make-plan #:waves (list (make-wave #:title ""))))
      (define result (validate-plan-strict plan))
      (check-not-false (member "Wave 0: no explicit title" (validation-result-warnings result))))

    (test-case "all waves with no files → warning (v0.75.8 relaxed)"
      (define plan
        (make-plan #:waves (list (make-wave #:files '()) (make-wave #:files '() #:index 1))))
      (define result (validate-plan-strict plan))
      (check-not-false (member "Plan has no file references in any wave — docs-only plan"
                               (validation-result-warnings result))))

    ;; === validate-normalized-plan ===

    (test-case "valid normalized plan → gsd-validated-plan"
      (define plan (make-norm-plan #:waves (list (make-norm-wave))))
      (define result (validate-normalized-plan plan))
      (check-true (gsd-validated-plan? result)))

    (test-case "normalized plan with no waves → error result"
      (define plan (make-norm-plan #:waves '()))
      (define result (validate-normalized-plan plan))
      (check-true (validation-result? result))
      (check-not-false (member "Plan has no waves" (validation-result-errors result))))

    (test-case "normalized wave with no files → validates (v0.75.8 relaxed)"
      (define plan (make-norm-plan #:waves (list (make-norm-wave #:files '()))))
      (define result (validate-normalized-plan plan))
      ;; v0.75.8: docs-only plan with no file refs now validates
      (check-true (gsd-validated-plan? result)))

    (test-case "normalized wave with no verify → valid (warnings don't block)"
      (define plan (make-norm-plan #:waves (list (make-norm-wave #:verify ""))))
      (define result (validate-normalized-plan plan))
      ;; Missing verify is a warning, not an error — plan still validates
      (check-true (or (gsd-validated-plan? result)
                      (and (validation-result? result) (null? (validation-result-errors result))))))

    (test-case "all normalized waves with no files → validates (v0.75.8 relaxed)"
      (define plan
        (make-norm-plan #:waves (list (make-norm-wave #:files '())
                                      (make-norm-wave #:files '() #:index 1))))
      (define result (validate-normalized-plan plan))
      ;; v0.75.8: docs-only plans now validate (warnings, not errors)
      (check-true (gsd-validated-plan? result)))

    ;; === valid-plan->go? ===

    (test-case "valid plan → go allowed"
      (define plan (make-plan #:waves (list (make-wave))))
      (check-true (valid-plan->go? plan)))

    (test-case "invalid plan → go denied"
      (define plan (make-plan #:waves '()))
      (check-false (valid-plan->go? plan)))

    ;; === format-validation-report ===

    (test-case "format report includes errors"
      (define result (validation-result '("Error 1" "Error 2") '("Warning 1")))
      (define report (format-validation-report result))
      (check-not-false (string-contains? report "Error 1"))
      (check-not-false (string-contains? report "Error 2"))
      (check-not-false (string-contains? report "Warning 1")))

    (test-case "format report with no issues → valid message"
      (define result (validation-result '() '()))
      (define report (format-validation-report result))
      (check-not-false (string-contains? report "Plan is valid")))))

(run-tests suite)
