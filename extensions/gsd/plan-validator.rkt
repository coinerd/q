#lang typed/racket

;; extensions/gsd/plan-validator.rkt — Plan structure validation
;;
;; Migrated to #lang typed/racket in v0.22.8 W2.
;; Enhanced validation: files missing is an error (not warning).

(require racket/format
         racket/string
         "plan-types.rkt")

(: validate-plan-strict : gsd-plan -> validation-result)
(define (validate-plan-strict plan)
  (define waves (gsd-plan-waves plan))
  (define-values (errors warnings)
    (for/fold :
              (Values
               [Listof String]
               [Listof String])
      ([errors
        :
        (Listof String)
        (if (null? waves)
            (list "Plan has no waves")
            '())]
       [warnings : (Listof String) '()])
      ([w waves])
      (define widx (gsd-wave-index w))
      (define prefix (format "Wave ~a" widx))
      (define new-warnings
        (cond
          [(string=? (gsd-wave-title w) "") (cons (format "~a: no explicit title" prefix) warnings)]
          [else warnings]))
      (define new-warnings2
        (if (null? (gsd-wave-files w))
            (cons (format "~a: no file references — wave may not produce changes" prefix)
                  new-warnings)
            new-warnings))
      (define new-warnings3
        (if (or (not (gsd-wave-verify w)) (string=? (gsd-wave-verify w) ""))
            (cons (format "~a: no verify command" prefix) new-warnings2)
            new-warnings2))
      (define new-warnings4
        (if (string=? (gsd-wave-root-cause w) "")
            (cons (format "~a: no root-cause/objective" prefix) new-warnings3)
            new-warnings3))
      (define new-warnings5
        (if (string=? (gsd-wave-title w) "")
            (cons (format "~a: cannot derive wave doc slug (empty title)" prefix) new-warnings4)
            new-warnings4))
      (values errors new-warnings5)))
  ;; v0.75.8: Downgraded from ERROR to WARNING — docs-only plans have no file refs
  (define final-warnings
    (if (and (pair? waves) (andmap (lambda ([w : gsd-wave]) (null? (gsd-wave-files w))) waves))
        (cons "Plan has no file references in any wave — docs-only plan" warnings)
        warnings))
  (validation-result (reverse errors) (reverse final-warnings)))

;; v0.24.2: Validate normalized plan and wrap as gsd-validated-plan.
;; Takes a gsd-normalized-plan (already structurally valid from normalization)
;; and checks semantic validity only. Returns gsd-validated-plan on success.
(: validate-normalized-plan : gsd-normalized-plan -> (U gsd-validated-plan validation-result))
(define (validate-normalized-plan norm-plan)
  (define waves (gsd-normalized-plan-waves norm-plan))
  (define-values (errors warnings)
    (for/fold :
              (Values
               [Listof String]
               [Listof String])
      ([errors
        :
        (Listof String)
        (if (null? waves)
            (list "Plan has no waves")
            '())]
       [warnings : (Listof String) '()])
      ([w waves])
      (define widx (gsd-normalized-wave-index w))
      (define prefix (format "Wave ~a" widx))
      (define new-warnings
        (if (string=? (gsd-normalized-wave-title w) "")
            (cons (format "~a: no explicit title" prefix) warnings)
            warnings))
      (define new-warnings2
        (if (null? (gsd-normalized-wave-files w))
            (cons (format "~a: no file references — wave may not produce changes" prefix)
                  new-warnings)
            new-warnings))
      (define new-warnings3
        (if (string=? (gsd-normalized-wave-verify-command w) "")
            (cons (format "~a: no verify command" prefix) new-warnings2)
            new-warnings2))
      (values errors new-warnings3)))
  ;; v0.75.8: Downgraded from ERROR to WARNING — docs-only plans have no file refs
  (define final-warnings
    (if (and (pair? waves)
             (andmap (lambda ([w : gsd-normalized-wave]) (null? (gsd-normalized-wave-files w)))
                     waves))
        (cons "Plan has no file references in any wave — docs-only plan" warnings)
        warnings))
  (if (null? errors)
      (gsd-validated-plan norm-plan)
      (validation-result (reverse errors) (reverse final-warnings))))

(: valid-plan->go? : gsd-plan -> Boolean)
(define (valid-plan->go? plan)
  (define result (validate-plan-strict plan))
  (validation-valid? result))

(: format-validation-report : validation-result -> String)
(define (format-validation-report result)
  (define errs (validation-result-errors result))
  (define warns (validation-result-warnings result))
  (define parts
    (let ([error-part (if (null? errs)
                          '()
                          (list (format "❌ ERRORS (block /go):\n~a"
                                        (string-join (for/list :
                                                       (Listof String)
                                                       ([e : String errs])
                                                       (format "  - ~a" e))
                                                     "\n"))))]
          [warn-part (if (null? warns)
                         '()
                         (list (format "⚠️  WARNINGS:\n~a"
                                       (string-join (for/list :
                                                      (Listof String)
                                                      ([w : String warns])
                                                      (format "  - ~a" w))
                                                    "\n"))))])
      (append warn-part error-part)))
  (if (null? errs)
      (string-append "✅ Plan is valid.\n"
                     (if (null? parts)
                         ""
                         (string-join (reverse parts) "\n\n")))
      (string-join (reverse parts) "\n\n")))

(provide validate-plan-strict
         format-validation-report
         valid-plan->go?
         validate-normalized-plan)
