#lang racket

;;; test-inline-format-deprecation.rkt — W0 characterization pin for
;;; BUG-0023 (residual): the inline `## Wave N:` plan format STILL loads
;;; through the `parse-waves-from-markdown` fallback even when the PLAN.md
;;; index has NO `- [Status] Wn:` rows. The BUG-0035 deprecation warning is
;;; advisory-only (warn, not block): the precedence seam in
;;; command-handlers.rkt (`(or plan-from-index ...)`) falls back to the
;;; inline parser and /go accepts the result.
;;;
;;; Flip owner: W1 (inline-format deprecation enforcement). When W1 makes
;;; the canonical index format mandatory, this pin must be flipped into a
;;; fix-regression test asserting the inline-only plan is REJECTED.

(require rackunit
         "../extensions/gsd/plan-types.rkt"
         "../extensions/gsd/wave-docs.rkt")

;; A plan written entirely in the inline `## Wave N:` style — no
;; `- [Inbox] W0: ...` index rows at all.
(define plan-without-index
  (string-append
   "# Plan: inline-only characterization\n\n"
   "## Wave 0: Baseline\n\n"
   "Inline body text.\n\n"
   "## Verify\n\n"
   "racket tests/nothing.rkt\n\n"
   "## Done\n\n"
   "- one row\n"))

;; --- Pin 1: the index parser finds ZERO rows (index is authoritative --
;; when present). The BUG-0023 residual is that its absence silently
;; degrades to the fallback instead of failing validation.
(check-equal?
 (parse-plan-index plan-without-index)
 '()
 "inline-only plan has no `- [Status] Wn:` index rows")

;; --- Pin 2: the inline fallback still parses the plan (accepted, not
;; blocked). This is TODAY's red behavior at the precedence seam: /go
;; proceeds with these waves despite the (advisory) BUG-0035 warning.
(define waves (parse-waves-from-markdown plan-without-index))

(check-pred (lambda (ws) (>= (length ws) 1)) waves
            "inline-format fallback still loads the plan (warn, not block)")
(check-equal? (gsd-wave-index (car waves)) 0
              "fallback yields Wave 0")
(check-equal? (gsd-wave-status (car waves)) 'pending
              "fallback waves default to 'pending status")
(check-pred (lambda (w) (non-empty-string? (gsd-wave-title w))) (car waves)
            "fallback yields a non-empty wave title")

(displayln "PASS test-inline-format-deprecation (BUG-0023 residual pin: inline-only plan still accepted via fallback)")
