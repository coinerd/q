#lang racket/base

;; extensions/gsd/plan-validator.rkt — Plan structure validation
;;
;; Wave 2a of v0.21.0: Mechanical validation before /go is allowed.
;; DD-4: Structured plan format with mechanical validation.
;;
;; Error rules (block /go):
;;   - Plan has < 1 wave
;;   - Wave missing title
;;   - Wave has no files (was warning, upgraded to error per spec)
;;
;; Warning rules (display but allow):
;;   - Wave missing verify command
;;   - Wave missing root-cause/objective

(require racket/format
         racket/string
         "plan-types.rkt")

(provide validate-plan-strict
         format-validation-report
         valid-plan->go?)

;; ============================================================
;; Strict validation
;; ============================================================

;; Enhanced validation: files missing is an error (not warning).
;; Returns validation-result with errors and warnings.
(define (validate-plan-strict plan)
  (define waves (gsd-plan-waves plan))
  (define errors '())
  (define warnings '())
  ;; Check: plan has at least 1 wave
  (when (null? waves)
    (set! errors (cons "Plan has no waves" errors)))
  ;; Per-wave checks
  (for ([w waves])
    (define widx (gsd-wave-index w))
    (define prefix (format "Wave ~a" widx))
    ;; Error: missing title
    (when (string=? (gsd-wave-title w) "")
      (set! errors (cons (format "~a: missing title" prefix) errors)))
    ;; Error: no file references (DD-4: strict)
    (when (null? (gsd-wave-files w))
      (set! errors (cons (format "~a: no file references" prefix) errors)))
    ;; Warning: no verify command
    (when (string=? (gsd-wave-verify w) "")
      (set! warnings (cons (format "~a: no verify command" prefix) warnings)))
    ;; Warning: no root-cause/objective
    (when (string=? (gsd-wave-root-cause w) "")
      (set! warnings (cons (format "~a: no root-cause/objective" prefix) warnings))))
  (validation-result (reverse errors) (reverse warnings)))

;; ============================================================
;; Helpers
;; ============================================================

;; Can we proceed to /go? Only if no errors.
(define (valid-plan->go? plan)
  (define result (validate-plan-strict plan))
  (validation-valid? result))

;; Format validation result for display.
(define (format-validation-report result)
  (define errors (validation-errors result))
  (define warnings (validation-warnings result))
  (define parts '())
  (when (not (null? errors))
    (set! parts
          (cons (format "❌ ERRORS (block /go):\n~a"
                        (string-join (for/list ([e errors])
                                       (format "  - ~a" e))
                                     "\n"))
                parts)))
  (when (not (null? warnings))
    (set! parts
          (cons (format "⚠️  WARNINGS:\n~a"
                        (string-join (for/list ([w warnings])
                                       (format "  - ~a" w))
                                     "\n"))
                parts)))
  (if (null? errors)
      (string-append "✅ Plan is valid.\n"
                     (if (null? parts)
                         ""
                         (string-join (reverse parts) "\n\n")))
      (string-join (reverse parts) "\n\n")))
