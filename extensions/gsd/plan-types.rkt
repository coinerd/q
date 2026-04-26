#lang racket/base

;; extensions/gsd/plan-types.rkt — Structured Plan/Task/Wave types
;;
;; Wave 0 of v0.21.0: Machine-parseable plan types with validation.
;; Plans use a structured format with required fields.
;; The extension validates plan structure before allowing /go.

(require racket/match
         racket/string
         racket/list)

;; ============================================================
;; Core structs (immutable)
;; ============================================================

(struct gsd-task (name files action verify done status) #:transparent)

(struct gsd-wave (index title status root-cause files tasks verify done-criteria) #:transparent)

(struct gsd-plan (waves context-bundle constraints must-haves) #:transparent)

(struct validation-result (errors warnings) #:transparent)

;; Aliases for cleaner API
(define validation-errors validation-result-errors)
(define validation-warnings validation-result-warnings)

;; ============================================================
;; Convenience constructors
;; ============================================================

(define (make-gsd-task name files action verify [done ""])
  (gsd-task name files action verify done 'pending))

(define (make-gsd-wave index title root-cause files tasks verify done-criteria)
  (gsd-wave index title 'pending root-cause files tasks verify done-criteria))

;; ============================================================
;; Status setters (return new struct — immutable)
;; ============================================================

(define (gsd-task-set-status t status)
  (struct-copy gsd-task t [status status]))

(define (gsd-wave-set-status w status)
  (struct-copy gsd-wave w [status status]))

;; ============================================================
;; Validation helpers
;; ============================================================

(define (validation-valid? vr)
  (null? (validation-errors vr)))

;; ============================================================
;; Parsing: markdown → (listof gsd-wave)
;; ============================================================

;; Parse PLAN.md content into structured waves.
;; Looks for "## Wave N: Title" headers and extracts fields from
;; bullet points within each wave section.
(define (parse-waves-from-markdown md-text)
  (define lines (string-split md-text "\n"))
  ;; Find wave header line indices
  (define wave-starts
    (for/list ([line lines]
               [idx (in-naturals)]
               #:when (regexp-match #rx"^## +[Ww]ave +[0-9]+" line))
      idx))
  (define wave-end-idxs
    (if (< (length wave-starts) 2)
        (list (sub1 (length lines)))
        (append (map sub1 (cdr wave-starts)) (list (sub1 (length lines))))))
  ;; Parse each wave section
  (for/list ([start wave-starts]
             [end wave-end-idxs])
    (parse-wave-section (take (drop lines start) (add1 (- end start))))))

;; Parse a single wave section (list of lines, first is header).
(define (parse-wave-section lines)
  (define header (car lines))
  (define body-lines (cdr lines))
  ;; Extract index and title from header
  (define header-match (regexp-match #rx"^## +[Ww]ave +([0-9]+) *: *(.+)$" header))
  (define idx
    (if header-match
        (string->number (cadr header-match))
        0))
  (define title
    (if header-match
        (string-trim (caddr header-match))
        ""))
  ;; Extract fields from bullet points
  (define root-cause "")
  (define files '())
  (define verify-cmd "")
  (define done-criteria '())
  (for ([line body-lines])
    (cond
      [(regexp-match #rx"^- +[Rr]oot *[Cc]ause *: *(.+)$" line)
       =>
       (lambda (m) (set! root-cause (string-trim (cadr m))))]
      [(regexp-match #rx"^- +[Ff]ile *: *(.+)$" line)
       =>
       (lambda (m) (set! files (append files (list (string-trim (cadr m))))))]
      [(regexp-match #rx"^- +[Vv]erify *: *(.+)$" line)
       =>
       (lambda (m) (set! verify-cmd (string-trim (cadr m))))]
      [(regexp-match #rx"^- +[Dd]one *: *(.+)$" line)
       =>
       (lambda (m) (set! done-criteria (append done-criteria (list (string-trim (cadr m))))))]))
  (gsd-wave idx title 'pending root-cause files '() verify-cmd done-criteria))

;; ============================================================
;; Validation
;; ============================================================

(define (validate-plan plan)
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
    (when (string=? (gsd-wave-title w) "")
      (set! errors (cons (format "~a: missing title" prefix) errors)))
    (when (null? (gsd-wave-files w))
      (set! warnings (cons (format "~a: no file references" prefix) warnings)))
    (when (string=? (gsd-wave-verify w) "")
      (set! warnings (cons (format "~a: no verify command" prefix) warnings))))
  (validation-result (reverse errors) (reverse warnings)))

;; ============================================================
;; Plan utilities
;; ============================================================

(define (plan-wave-ref plan idx)
  (for/first ([w (gsd-plan-waves plan)]
              #:when (= (gsd-wave-index w) idx))
    w))

(define (plan-pending-waves plan)
  (filter (lambda (w) (eq? (gsd-wave-status w) 'pending)) (gsd-plan-waves plan)))

(define (plan-next-pending-wave plan)
  (for/first ([w (gsd-plan-waves plan)]
              #:when (eq? (gsd-wave-status w) 'pending))
    w))

;; ============================================================
;; Provide (after all definitions)
;; ============================================================

;; Struct types + accessors
(provide gsd-task
         gsd-task?
         gsd-task-name
         gsd-task-files
         gsd-task-action
         gsd-task-verify
         gsd-task-done
         gsd-task-status
         gsd-task-set-status

         gsd-wave
         gsd-wave?
         gsd-wave-index
         gsd-wave-title
         gsd-wave-status
         gsd-wave-root-cause
         gsd-wave-files
         gsd-wave-tasks
         gsd-wave-verify
         gsd-wave-done-criteria
         gsd-wave-set-status

         gsd-plan
         gsd-plan?
         gsd-plan-waves
         gsd-plan-context-bundle
         gsd-plan-constraints
         gsd-plan-must-haves

         ;; Constructors with defaults
         make-gsd-task
         make-gsd-wave

         ;; Parsing
         parse-waves-from-markdown

         ;; Validation
         validation-result
         validation-result?
         validation-errors
         validation-warnings
         validation-valid?
         validate-plan

         ;; Plan utilities
         plan-wave-ref
         plan-pending-waves
         plan-next-pending-wave)
