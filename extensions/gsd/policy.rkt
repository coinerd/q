#lang racket/base

;; extensions/gsd/policy.rkt — Unified GSD Policy Engine
;; STABILITY: evolving
;;
;; F4 fix: Single decision function for all guard checks.
;; Consolidates BLOCKED-TOOLS from state-machine.rkt and
;; write guards from core.rkt into one policy module.

(require racket/match
         racket/string)

(provide policy-decision
         policy-decision?
         policy-allowed?
         policy-blocked?
         policy-reason
         policy-tags
         gsd-decide-action
         blocked-tools-for)

;; ============================================================
;; Policy decision struct
;; ============================================================

(struct policy-decision (allowed? reason tags) #:transparent)

(define (policy-allowed? d)
  (policy-decision-allowed? d))

(define (policy-blocked? d)
  (not (policy-decision-allowed? d)))

(define (policy-reason d)
  (policy-decision-reason d))

(define (policy-tags d)
  (policy-decision-tags d))

;; ============================================================
;; Consolidated tool blocklist (from state-machine.rkt BLOCKED-TOOLS)
;; ============================================================

(define (blocked-tools-for mode)
  (case mode
    [(plan-written) '("edit" "write" "bash")]
    [(executing) '("planning-write")]
    [(verifying) '("edit" "write" "bash" "planning-write")]
    [else '()]))

;; ============================================================
;; Unified decision function
;; ============================================================

;; (gsd-decide-action ctx action) → policy-decision
;; ctx: hasheq with 'mode, 'tool, 'target-path, 'pinned-dir
;; action: symbol — 'tool-call, 'write-file, 'edit-plan, 'transition
(define (gsd-decide-action ctx action)
  (define mode (hash-ref ctx 'mode 'idle))
  (case action
    [(tool-call)
     (define tool (hash-ref ctx 'tool ""))
     (define blocked (blocked-tools-for mode))
     (if (member tool blocked)
         (policy-decision #f
                          (format "Tool '~a' blocked in ~a mode" tool mode)
                          (list 'tool-blocked mode tool))
         (policy-decision #t #f (list 'tool-allowed mode tool)))]
    [(write-file)
     (define target (hash-ref ctx 'target-path ""))
     (define pinned (hash-ref ctx 'pinned-dir ""))
     (if (and (eq? mode 'executing) (in-planning-dir? target pinned))
         (policy-decision #f
                          (format "Cannot write to ~a during execution" target)
                          (list 'write-blocked 'executing target))
         (policy-decision #t #f (list 'write-allowed mode target)))]
    [(edit-plan)
     (if (eq? mode 'executing)
         (policy-decision #f "Cannot edit plan during execution" '(edit-plan-blocked))
         (policy-decision #t #f '(edit-plan-allowed)))]
    [(transition)
     (define target-mode (hash-ref ctx 'target-mode #f))
     (policy-decision #t #f (list 'transition mode target-mode))]
    [else (policy-decision #t #f (list 'unknown-action action))]))

;; ============================================================
;; Internal helpers
;; ============================================================

;; Path normalization for security (prevents .. traversal)
(require racket/path)

(define (in-planning-dir? target pinned)
  (and (string? target)
       (non-empty-string? target)
       (string? pinned)
       (non-empty-string? pinned)
       (let ([ct (with-handlers ([exn:fail? (λ (_) target)])
                   (path->string (simple-form-path (string->path target))))]
             [cp (with-handlers ([exn:fail? (λ (_) pinned)])
                   (path->string (simple-form-path (string->path pinned))))])
         (or (string=? ct cp) (string-prefix? ct (string-append cp "/"))))))
