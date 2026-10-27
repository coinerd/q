#lang racket/base

;; scripts/validate-plan.rkt — standalone plan-validation CLI (BUG-0048)
;;
;; Canonical plan validator for plan AUTHORS, runnable outside the TUI:
;;   racket scripts/validate-plan.rkt <project-base>/.planning/PLAN.md
;;   racket scripts/validate-plan.rkt -b <project-base>
;;   racket scripts/validate-plan.rkt        (defaults to ./.planning/PLAN.md)
;;   racket scripts/validate-plan.rkt --explain [-b <base>] [--campaign <plan-id>]
;;
;; Runs the SAME checks /go runs before a campaign — PLAN.md index
;; parsing, strict wave-doc existence (BUG-0023), Files/Verify/Done
;; wave-doc lint (BUG-0041), status consistency, strict plan lint —
;; plus the mechanical file-attribution sanity check (advisory) — by
;; delegating to the shared kernel validate-plan-artifacts in
;; extensions/gsd/planning-artifact-validation.rkt (also called by /go's
;; validate-plan-for-go), so this CLI and /go cannot diverge. Both consume
;; the same structured findings: a stable machine code, operator text, and
;; a named recovery action per finding.
;;
;; W1 (GSD-SIMPLIFICATION-PLAN): `--explain` is a READ-ONLY campaign
;; inspection. It prints the plan path, the stored plan-id, the manifest
;; hash recomputed from the durable record, the frozen bound plan-body hash
;; versus the live one, the declared (and bound) wave paths, per-wave status
;; projections, and snapshot drift classes. It never writes, never rebinds,
;; never repairs, and never resnapshots anything. Without `--campaign` the
;; newest durable campaign record under .planning/campaigns/ is inspected.
;; `--campaign <plan-id>` additionally feeds that campaign's snapshot drift
;; to the validator. Under an explicit campaign, every actual drift class
;; (including missing/content/frozen-contract/symlink) and unreadable state
;; is an error; absent snapshots and unknown campaigns remain advisory.
;;
;; Exit codes: 0 = plan is /go-ready (warnings may still print),
;;             1 = plan rejected (errors named per wave),
;;             2 = usage error.

(require racket/match
         racket/path
         racket/string
         (only-in "../extensions/gsd/planning-artifact-validation.rkt"
                  validate-plan-artifacts
                  format-plan-validation-findings
                  plan-artifact-explanation
                  format-plan-artifact-explanation))

(define (usage msg)
  (eprintf "validate-plan: ~a\n" msg)
  (eprintf
   "usage: racket scripts/validate-plan.rkt [-b <base-dir>] [<plan.md>] [--explain] [--campaign <plan-id>]\n")
  (exit 2))

(define base-arg #f)
(define plan-arg #f)
(define explain? #f)
(define campaign-arg #f)

(let parse ([rest (vector->list (current-command-line-arguments))])
  (match rest
    [(list) (void)]
    [(list (or "-b" "--base") dir more ...)
     (when base-arg
       (usage "-b given twice"))
     (set! base-arg dir)
     (parse more)]
    [(list (or "-h" "--help") _ ...) (usage "help: validate a GSD plan before /go")]
    [(list (or "-e" "--explain") more ...)
     (set! explain? #t)
     (parse more)]
    [(list "--campaign" id more ...)
     (when campaign-arg
       (usage "--campaign given twice"))
     (set! campaign-arg id)
     ;; --campaign only has meaning for the read-only inspection surface.
     (set! explain? #t)
     (parse more)]
    ;; An unknown option is a USAGE error (exit 2), never a plan path.
    [(list (? (lambda (a) (string-prefix? a "-")) a) _ ...) (usage (format "unknown option: ~a" a))]
    [(list p)
     (when plan-arg
       (usage "more than one plan path given"))
     (set! plan-arg p)
     (parse '())]
    [_ (usage "unexpected arguments")]))

(define plan-path
  (cond
    [plan-arg (path->complete-path plan-arg)]
    [base-arg (build-path (path->complete-path base-arg) ".planning" "PLAN.md")]
    [else (build-path (current-directory) ".planning" "PLAN.md")]))

(define base-dir
  (cond
    [base-arg (simplify-path (path->complete-path base-arg))]
    [else
     (define parent (path-only plan-path))
     (unless parent
       (usage "cannot derive project base from plan path"))
     (simplify-path (build-path parent 'up))]))

(printf "== validate-plan (BUG-0048) ==\nplan: ~a\nbase: ~a\n\n" plan-path base-dir)

(define explanation
  (and explain?
       (if campaign-arg
           (plan-artifact-explanation base-dir #:campaign-id campaign-arg)
           (plan-artifact-explanation base-dir))))

(when explanation
  (displayln (format-plan-artifact-explanation explanation))
  (newline))

;; The same kernel /go runs. #:campaign-id is passed only for an explicitly
;; requested campaign, so the ordinary authoring path keeps its historical
;; acceptance while an operator inspecting a bound campaign sees its drift
;; findings (every actual drift class blocks).
(define result
  (cond
    [(and explain? campaign-arg)
     (validate-plan-artifacts base-dir
                              plan-path
                              #:campaign-id campaign-arg
                              #:explanation explanation)]
    [explain? (validate-plan-artifacts base-dir plan-path #:explanation explanation)]
    [else (validate-plan-artifacts base-dir plan-path)]))

(displayln (format-plan-validation-findings result))

(exit (if (hash-ref result 'ok?) 0 1))
