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
  (: errors : (Listof String))
  (define errors '())
  (: warnings : (Listof String))
  (define warnings '())
  (when (null? waves)
    (set! errors (cons "Plan has no waves" errors)))
  (for ([w waves])
    (define widx (gsd-wave-index w))
    (define prefix (format "Wave ~a" widx))
    (when (string=? (gsd-wave-title w) "")
      (set! warnings (cons (format "~a: no explicit title" prefix) warnings)))
    (when (null? (gsd-wave-files w))
      (set! warnings
            (cons (format "~a: no file references — wave may not produce changes" prefix) warnings)))
    (when (or (not (gsd-wave-verify w)) (string=? (gsd-wave-verify w) ""))
      (set! warnings (cons (format "~a: no verify command" prefix) warnings)))
    (when (string=? (gsd-wave-root-cause w) "")
      (set! warnings (cons (format "~a: no root-cause/objective" prefix) warnings)))
    (when (string=? (gsd-wave-title w) "")
      (set! warnings
            (cons (format "~a: cannot derive wave doc slug (empty title)" prefix) warnings))))
  (when (and (pair? waves) (andmap (lambda ([w : gsd-wave]) (null? (gsd-wave-files w))) waves))
    (set! errors (cons "Plan has no file references in any wave — nothing to execute" errors)))
  (validation-result (reverse errors) (reverse warnings)))

;; v0.24.2: Validate normalized plan and wrap as gsd-validated-plan.
;; Takes a gsd-normalized-plan (already structurally valid from normalization)
;; and checks semantic validity only. Returns gsd-validated-plan on success.
(: validate-normalized-plan : gsd-normalized-plan -> (U gsd-validated-plan validation-result))
(define (validate-normalized-plan norm-plan)
  (define waves (gsd-normalized-plan-waves norm-plan))
  (: errors : (Listof String))
  (define errors '())
  (: warnings : (Listof String))
  (define warnings '())
  (when (null? waves)
    (set! errors (cons "Plan has no waves" errors)))
  (for ([w waves])
    (define widx (gsd-normalized-wave-index w))
    (define prefix (format "Wave ~a" widx))
    (when (string=? (gsd-normalized-wave-title w) "")
      (set! warnings (cons (format "~a: no explicit title" prefix) warnings)))
    (when (null? (gsd-normalized-wave-files w))
      (set! warnings
            (cons (format "~a: no file references — wave may not produce changes" prefix) warnings)))
    (when (string=? (gsd-normalized-wave-verify-command w) "")
      (set! warnings (cons (format "~a: no verify command" prefix) warnings))))
  (when (and (pair? waves)
             (andmap (lambda ([w : gsd-normalized-wave]) (null? (gsd-normalized-wave-files w)))
                     waves))
    (set! errors (cons "Plan has no file references in any wave — nothing to execute" errors)))
  (if (null? errors)
      (gsd-validated-plan norm-plan)
      (validation-result (reverse errors) (reverse warnings))))

(: valid-plan->go? : gsd-plan -> Boolean)
(define (valid-plan->go? plan)
  (define result (validate-plan-strict plan))
  (validation-valid? result))

(: format-validation-report : validation-result -> String)
(define (format-validation-report result)
  (define errs (validation-result-errors result))
  (define warns (validation-result-warnings result))
  (: parts : (Listof String))
  (define parts '())
  (when (not (null? errs))
    (set! parts
          (cons (format "❌ ERRORS (block /go):\n~a"
                        (string-join (for/list :
                                       (Listof String)
                                       ([e : String errs])
                                       (format "  - ~a" e))
                                     "\n"))
                parts)))
  (when (not (null? warns))
    (set! parts
          (cons (format "⚠️  WARNINGS:\n~a"
                        (string-join (for/list :
                                       (Listof String)
                                       ([w : String warns])
                                       (format "  - ~a" w))
                                     "\n"))
                parts)))
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
