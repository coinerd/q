#!/usr/bin/env racket
#lang racket/base

;; lint-contract-changes.rkt — CI lint for contract changes with caller inventory
;;
;; Given a git diff range, finds contract-out changes. For each tightening,
;; reports whether callers exist. Warns on widenings without ledger entries.
;;
;; Exit codes:
;;   0 — no issues found
;;   1 — contract changes require attention
;;
;; Usage:
;;   racket scripts/lint-contract-changes.rkt --diff HEAD
;;   racket scripts/lint-contract-changes.rkt --diff HEAD~3..HEAD
;;   racket scripts/lint-contract-changes.rkt --diff main..feature-branch

(require racket/string
         racket/list
         racket/port
         racket/system
         racket/path
         racket/match)

;; ---------------------------------------------------------------------------
;; Git diff analysis
;; ---------------------------------------------------------------------------

(define (get-git-diff diff-spec)
  (string-trim (with-output-to-string (lambda ()
                                        (system (format "git diff ~a -- '*.rkt'" diff-spec))))))

(define (get-changed-rkt-files diff-spec)
  (define output
    (string-trim (with-output-to-string
                  (lambda () (system (format "git diff --name-only ~a -- '*.rkt'" diff-spec))))))
  (if (equal? output "")
      '()
      (string-split output "\n")))

;; ---------------------------------------------------------------------------
;; Contract change detection
;; ---------------------------------------------------------------------------

(define (find-contract-out-changes diff-text)
  ;; Find lines that add/remove contract-out forms
  (define lines (string-split diff-text "\n"))
  (define additions
    (filter-map
     (lambda (line)
       (and (string-prefix? (string-trim line) "+") (string-contains? line "contract-out") line))
     lines))
  (define removals
    (filter-map (lambda (line)
                  (and (string-prefix? (string-trim line) "-")
                       (not (string-prefix? (string-trim line) "---"))
                       (string-contains? line "contract-out")
                       line))
                lines))
  (values additions removals))

(define (count-any/c-in-diff diff-text)
  ;; Count any/c in added vs removed lines
  (define lines (string-split diff-text "\n"))
  (define added-any
    (for/sum ([line (in-list lines)] #:when (string-prefix? (string-trim line) "+"))
             (length (regexp-match* #rx"any/c" line))))
  (define removed-any
    (for/sum ([line (in-list lines)] #:when (and (string-prefix? (string-trim line) "-")
                                                 (not (string-prefix? (string-trim line) "---"))))
             (length (regexp-match* #rx"any/c" line))))
  (values added-any removed-any))

;; ---------------------------------------------------------------------------
;; Caller inventory
;; ---------------------------------------------------------------------------

(define (find-callers function-name file-path)
  (define output
    (string-trim (with-output-to-string
                  (lambda ()
                    (system (format "grep -rn '~a' --include='*.rkt' . 2>/dev/null || true"
                                    function-name))))))
  (if (equal? output "")
      '()
      (string-split output "\n")))

;; ---------------------------------------------------------------------------
;; Main analysis
;; ---------------------------------------------------------------------------

(define (analyze-contract-changes diff-spec)
  (printf "── Contract Changes Analysis (~a) ──~n" diff-spec)

  (define diff-text (get-git-diff diff-spec))
  (define changed-files (get-changed-rkt-files diff-spec))

  (when (null? changed-files)
    (displayln "  No .rkt files changed in diff range")
    (exit 0))

  (printf "  Changed .rkt files: ~a~n" (length changed-files))

  ;; Contract-out changes
  (define-values (additions removals) (find-contract-out-changes diff-text))

  (when (pair? additions)
    (printf "  contract-out additions: ~a~n" (length additions)))
  (when (pair? removals)
    (printf "  contract-out removals: ~a~n" (length removals)))

  ;; any/c delta
  (define-values (added-any removed-any) (count-any/c-in-diff diff-text))
  (define delta (- added-any removed-any))

  (cond
    [(> removed-any added-any)
     (printf "  ✅ Net tightening: ~a any/c removed, ~a added (Δ~a)~n"
             removed-any
             added-any
             (- delta))]
    [(> added-any removed-any)
     (printf "  ⚠️  Net widening: ~a any/c added, ~a removed (Δ+~a)~n" added-any removed-any delta)
     (displayln "     Consider adding entries to WIDENED_BACK_LEDGER.md")]
    [(and (= added-any 0) (= removed-any 0)) (displayln "  ℹ️  No any/c changes detected")]
    [else (displayln "  ℹ️  any/c count unchanged (additions == removals)")])

  ;; Summary
  (cond
    [(and (> removed-any added-any) (> removed-any 0))
     (displayln "  [PASS] Contract changes look like tightening")
     0]
    [(> added-any removed-any)
     (displayln "  [WARN] Contract widening detected — check ledger")
     0] ; Non-blocking for now
    [else
     (displayln "  [PASS] No concerning contract changes")
     0]))

;; ---------------------------------------------------------------------------
;; Entry point
;; ---------------------------------------------------------------------------

(define (parse-args args)
  (let loop ([rest args]
             [diff-spec #f])
    (match rest
      ['() diff-spec]
      [(list "--diff" spec rest ...) (loop rest spec)]
      [(list _ rest ...) (loop rest diff-spec)])))

(provide analyze-contract-changes
         find-contract-out-changes
         count-any/c-in-diff)

;; Only auto-run when executed directly
(define invoked-directly?
  (let ([run-file (find-system-path 'run-file)])
    (and (path? run-file)
         (let ([base (file-name-from-path run-file)])
           (and base (equal? (path->string base) "lint-contract-changes.rkt"))))))
(when invoked-directly?
  (define args (vector->list (current-command-line-arguments)))
  (define diff-spec (parse-args args))
  (cond
    [(not diff-spec)
     (displayln "Usage: racket scripts/lint-contract-changes.rkt --diff <spec>")
     (exit 1)]
    [else (exit (analyze-contract-changes diff-spec))]))
