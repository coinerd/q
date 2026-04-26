#lang racket

;; tests/test-gsd-plan-validator.rkt — Plan validator tests
;;
;; Wave 2a: Strict validation rules that block /go.

(require rackunit
         "../extensions/gsd/plan-validator.rkt"
         "../extensions/gsd/plan-types.rkt")

;; ============================================================
;; Helper: valid wave
;; ============================================================

(define (valid-wave idx title files verify)
  (gsd-wave idx title 'pending "root cause" files '() verify '()))

;; ============================================================
;; Valid plans
;; ============================================================

(test-case "well-formed plan with 3 waves passes validation"
  (define plan
    (gsd-plan
     (list (valid-wave 0 "Setup" '("a.rkt") "raco test a")
           (valid-wave 1 "Implement" '("b.rkt") "raco test b")
           (valid-wave 2 "Verify" '("c.rkt") "raco test c"))
     "" '() '()))
  (define result (validate-plan-strict plan))
  (check-true (validation-valid? result))
  (check-equal? (length (validation-errors result)) 0))

(test-case "valid-plan->go? returns #t for valid plan"
  (define plan
    (gsd-plan (list (valid-wave 0 "Fix" '("x.rkt") "raco test")) "" '() '()))
  (check-true (valid-plan->go? plan)))

;; ============================================================
;; Error cases (block /go)
;; ============================================================

(test-case "plan with 0 waves → error"
  (define plan (gsd-plan '() "" '() '()))
  (define result (validate-plan-strict plan))
  (check-false (validation-valid? result))
  (check-true (ormap (lambda (e) (string-contains? e "no waves"))
                     (validation-errors result))))

(test-case "wave without title → error"
  (define plan
    (gsd-plan (list (gsd-wave 0 "" 'pending "" '("a.rkt") '() "test" '())) "" '() '()))
  (define result (validate-plan-strict plan))
  (check-false (validation-valid? result))
  (check-true (ormap (lambda (e) (string-contains? e "missing title"))
                     (validation-errors result))))

(test-case "wave without files → error"
  (define plan
    (gsd-plan (list (gsd-wave 0 "Fix" 'pending "" '() '() "test" '())) "" '() '()))
  (define result (validate-plan-strict plan))
  (check-false (validation-valid? result))
  (check-true (ormap (lambda (e) (string-contains? e "no file"))
                     (validation-errors result))))

;; ============================================================
;; Warning cases (allow /go)
;; ============================================================

(test-case "wave without verify → warning, not error"
  (define plan
    (gsd-plan (list (valid-wave 0 "Fix" '("a.rkt") "")) "" '() '()))
  (define result (validate-plan-strict plan))
  (check-true (validation-valid? result))
  (check-equal? (length (validation-errors result)) 0)
  (check-true (ormap (lambda (w) (string-contains? w "verify"))
                     (validation-warnings result))))

(test-case "wave without root-cause → warning"
  (define plan
    (gsd-plan (list (gsd-wave 0 "Fix" 'pending "" '("a.rkt") '() "test" '()))
              "" '() '()))
  (define result (validate-plan-strict plan))
  (check-true (validation-valid? result))
  (check-true (ormap (lambda (w) (string-contains? w "root-cause"))
                     (validation-warnings result))))

;; ============================================================
;; Format report
;; ============================================================

(test-case "format-validation-report shows checkmark for valid plan"
  (define plan
    (gsd-plan (list (valid-wave 0 "Fix" '("a.rkt") "test")) "" '() '()))
  (define result (validate-plan-strict plan))
  (define report (format-validation-report result))
  (check-true (string-contains? report "valid")))

(test-case "format-validation-report shows errors for invalid plan"
  (define plan (gsd-plan '() "" '() '()))
  (define result (validate-plan-strict plan))
  (define report (format-validation-report result))
  (check-true (string-contains? report "ERRORS")))
