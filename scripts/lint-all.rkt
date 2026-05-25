#!/usr/bin/env racket
#lang racket/base

;; scripts/lint-all.rkt — Unified CI lint runner
;;
;; Runs all lint/check scripts in a single process via subprocess.
;; Zero refactoring of existing scripts needed.
;;
;; Exit codes:
;;   0 — all checks pass
;;   1 — one or more failures
;;
;; Usage:
;;   racket scripts/lint-all.rkt            # run all checks
;;   racket scripts/lint-all.rkt --list     # list checks without running
;;   racket scripts/lint-all.rkt --only format,version  # run subset

(require racket/list
         racket/string
         racket/system
         racket/port)

;; ── Check definitions ──
;; Each check: (name script-path args continue-on-error?)

(define checks
  (list (list "format" "scripts/lint-format.rkt" '() #f)
        (list "version-sync" "scripts/sync-version.rkt" '("--all") #f)
        (list "version-validate" "scripts/sync-version.rkt" '("--validate") #f)
        (list "version-cross" "scripts/lint-version.rkt" '() #f)
        (list "protocols" "scripts/check-protocols.rkt" '() #f)
        (list "imports" "scripts/check-imports.rkt" '() #f)
        (list "deps" "scripts/check-deps.rkt" '() #f)
        (list "metrics-sync" "scripts/metrics.rkt" '("--lint") #f)
        (list "metrics-lint" "scripts/metrics.rkt" '("--lint") #f)
        (list "prose" "scripts/metrics.rkt" '("--lint-prose") #f)
        (list "readme-status" "scripts/sync-readme-status.rkt" '("--check") #f)
        (list "changelog-dates" "scripts/lint-changelog-dates.rkt" '() #f)
        (list "audit" "scripts/audit-project.rkt" '("--ci") #t)
        (list "tests" "scripts/lint-tests.rkt" '() #f)
        (list "deprecation" "scripts/lint-deprecation-deadlines.rkt" '("--ci") #f)
        (list "ci-readiness" "scripts/lint-ci-readiness.rkt" '() #f)
        (list "arch" "scripts/arch-report.rkt" '("--ci") #t)
        (list "ivg" "scripts/lint-ivg.rkt" '() #f)
        (list "release-readiness" "scripts/lint-release-readiness.rkt" '() #f)
        (list "doc-freshness" "scripts/lint-doc-freshness.rkt" '() #f)
        (list "widened-ledger" "scripts/lint-widened-ledger.rkt" '() #t)
        (list "contract-changes" "scripts/lint-contract-changes.rkt" '("--diff" "HEAD") #t)
        (list "hotspot" "scripts/hotspot-report.rkt" '("--ci") #t)))

;; ── Helpers ──

(define (find-racket)
  (or (find-executable-path "racket")
      (find-executable-path "raco")
      (error "racket not found in PATH")))

(define (run-check name script args)
  (define racket-path (find-racket))
  (define cmd-args (append (list script) args))
  (define-values (sp child-stdout child-stdin child-stderr)
    (apply subprocess #f #f #f racket-path cmd-args))
  (close-output-port child-stdin)
  (define stdout-text (port->string child-stdout))
  (define stderr-text (port->string child-stderr))
  (close-input-port child-stdout)
  (close-input-port child-stderr)
  (subprocess-wait sp)
  (define code (subprocess-status sp))
  (values code stdout-text stderr-text))

(define (print-banner n)
  (printf "~n── q CI Lint (~a checks) ──~n" n))

(define (print-summary passed failed warned)
  (printf "~n── Summary: ~a passed" passed)
  (when (positive? failed)
    (printf ", ~a failed" failed))
  (when (positive? warned)
    (printf ", ~a warnings" warned))
  (displayln " ──"))

;; ── Main ──

(define (main)
  (define argv (vector->list (current-command-line-arguments)))

  ;; --list mode
  (when (member "--list" argv)
    (for ([c (in-list checks)])
      (printf "  ~a — ~a ~a~n"
              (car c)
              (cadr c)
              (if (null? (caddr c))
                  ""
                  (string-join (caddr c) " "))))
    (exit 0))

  ;; --only filter
  (define only-arg (findf (λ (a) (string-prefix? a "--only=")) argv))
  (define only-names
    (if only-arg
        (string-split (substring only-arg (string-length "--only=")) ",")
        #f))

  (define active-checks
    (if only-names
        (filter (λ (c) (member (car c) only-names)) checks)
        checks))

  (print-banner (length active-checks))

  (define passed 0)
  (define failed 0)
  (define warned 0)

  (for ([c (in-list active-checks)])
    (define name (car c))
    (define script (cadr c))
    (define args (caddr c))
    (define continue? (cadddr c))

    ;; Check script exists
    (unless (file-exists? script)
      (printf "  [SKIP] ~a — script not found: ~a~n" name script)
      (set! warned (add1 warned)))

    (when (file-exists? script)
      (define-values (code stdout stderr) (run-check name script args))
      (cond
        [(zero? code)
         (printf "  [PASS] ~a~n" name)
         (set! passed (add1 passed))]
        [continue?
         (printf "  [WARN] ~a (non-blocking)~n" name)
         (when (and stderr (not (string=? stderr "")))
           (for ([line (in-list (string-split stderr "\n"))])
             (when (non-empty-string? line)
               (printf "         ~a~n" line))))
         (set! warned (add1 warned))]
        [else
         (printf "  [FAIL] ~a~n" name)
         ;; Show relevant output lines (last 5 of stdout + any stderr)
         (when (and stdout (not (string=? stdout "")))
           (define lines (filter non-empty-string? (string-split stdout "\n")))
           (define tail (take-right lines (min 5 (length lines))))
           (for ([line (in-list tail)])
             (printf "         ~a~n" line)))
         (when (and stderr (not (string=? stderr "")))
           (for ([line (in-list (string-split stderr "\n"))])
             (when (non-empty-string? line)
               (printf "         ~a~n" line))))
         (set! failed (add1 failed))])))

  (print-summary passed failed warned)

  (exit (if (positive? failed) 1 0)))

(main)
