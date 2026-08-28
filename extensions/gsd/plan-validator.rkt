#lang typed/racket

;; extensions/gsd/plan-validator.rkt — Plan structure validation
;;
;; Migrated to #lang typed/racket in v0.22.8 W2.
;; Enhanced validation: files missing is an error (not warning).

(require racket/format
         racket/string
         "plan-types.rkt")

;; BUG-0023 (W2): actionable zero-waves diagnostic. Rejections previously
;; collapsed to the single unactionable "Plan has no waves". This companion
;; error names the accepted plan format with a skeleton example, so authors
;; can fix the plan shape immediately. Kept as a SEPARATE error string so
;; existing assertions on "Plan has no waves" keep matching.
;; v1.00.22 (W1, BUG-0023 residual enforcement): the inline `## Wave N:`
;; format is no longer an accepted form at /go — an inline-only plan is
;; rejected earlier with inline-format-rejection-diagnostic — so this
;; diagnostic names only the canonical index grammar.
(: no-waves-format-diagnostic : String)
(define no-waves-format-diagnostic
  (string-append
   "found 0 index entries (`- [Inbox] W0: Title → waves/W0-slug.md`) and 0 inline `## Wave N:` sections — "
   "expected:\n"
   "  - Index format in .planning/PLAN.md: `- [Inbox] W0: Title → waves/W0-title-slug.md` "
   "(the target wave doc .planning/waves/W0-title-slug.md must exist)"))

;; BUG-0023 residual enforcement (v1.00.22 W1): the inline `## Wave N:`
;; fallback is no longer ACCEPTED at /go. BUG-0035 (v1.00.20 W6) named the
;; canonical format in an advisory deprecation warning with removal
;; targeted after v1.00.21; this executes that removal as a named error.
;; An inline-only PLAN.md (zero index rows, ≥ 1 inline wave section) is
;; rejected at /go with this message, which names the canonical
;; `- [Inbox] W0: Title → waves/W0-slug.md` index format.
(: inline-format-rejection-diagnostic : String)
(define inline-format-rejection-diagnostic
  (string-append
   "PLAN.md uses inline `## Wave N:` sections — this format was deprecated (BUG-0035) "
   "and is no longer accepted at /go. Migrate to the PLAN.md index grammar, "
   "one row per wave: - [Inbox] W0: Title → waves/W0-slug.md"))

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
            (list "Plan has no waves" no-waves-format-diagnostic)
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
            (list "Plan has no waves" no-waves-format-diagnostic)
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
         validate-normalized-plan
         no-waves-format-diagnostic
         inline-format-rejection-diagnostic)
