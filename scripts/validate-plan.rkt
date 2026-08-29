#lang racket/base

;; scripts/validate-plan.rkt — standalone plan-validation CLI (BUG-0048)
;;
;; Canonical plan validator for plan AUTHORS, runnable outside the TUI:
;;   racket scripts/validate-plan.rkt <project-base>/.planning/PLAN.md
;;   racket scripts/validate-plan.rkt -b <project-base>
;;   racket scripts/validate-plan.rkt        (defaults to ./.planning/PLAN.md)
;;
;; Runs the SAME checks /go runs before a campaign — PLAN.md index
;; parsing, strict wave-doc existence (BUG-0023), Files/Verify/Done
;; wave-doc lint (BUG-0041), status consistency, strict plan lint —
;; plus the mechanical file-attribution sanity check (advisory) — by
;; delegating to the shared kernel validate-plan-artifacts in
;; extensions/gsd/wave-executor.rkt (also called by /go's
;; validate-plan-for-go), so this CLI and /go cannot diverge.
;;
;; Exit codes: 0 = plan is /go-ready (warnings may still print),
;;             1 = plan rejected (errors named per wave),
;;             2 = usage error.

(require racket/match
         racket/path
         racket/string
         (only-in "../extensions/gsd/wave-executor.rkt"
                  validate-plan-artifacts
                  format-plan-validation-findings
                  plan-validation-error-findings))

(define (usage msg)
  (eprintf "validate-plan: ~a\n" msg)
  (eprintf "usage: racket scripts/validate-plan.rkt [-b <base-dir>] [<plan.md>]\n")
  (exit 2))

(define base-arg #f)
(define plan-arg #f)

(let parse ([rest (vector->list (current-command-line-arguments))])
  (match rest
    [(list) (void)]
    [(list (or "-b" "--base") dir more ...)
     (when base-arg (usage "-b given twice"))
     (set! base-arg dir)
     (parse more)]
    [(list (or "-h" "--help") _ ...)
     (usage "help: validate a GSD plan before /go")]
    [(list p)
     (when plan-arg (usage "more than one plan path given"))
     (set! plan-arg p)]
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
     (unless parent (usage "cannot derive project base from plan path"))
     (simplify-path (build-path parent 'up))]))

(printf "== validate-plan (BUG-0048) ==\nplan: ~a\nbase: ~a\n\n" plan-path base-dir)

(define result (validate-plan-artifacts base-dir plan-path))
(displayln (format-plan-validation-findings result))

(exit (if (hash-ref result 'ok?) 0 1))
