#lang racket

;;; test-inline-format-deprecation.rkt — W1 pin (BUG-0023 residual,
;;; FLIPPED from the W0 characterization): the inline `## Wave N:`
;;; fallback is no longer ACCEPTED at /go. An inline-only PLAN.md
;;; (zero index rows, ≥ 1 inline wave section) is REJECTED with a
;;; named error that names the canonical
;;; `- [Inbox] W0: Title → waves/W0-slug.md` index format
;;; (`inline-format-rejection-diagnostic`). A canonical plan loads
;;; silently (zero deprecation warnings). BUG-0035's roadmap
;;; ("warn in v1.00.20, remove after the following release") is executed here.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/list
         "../extensions/gsd/plan-types.rkt"
         "../extensions/gsd/wave-docs.rkt"
         "../extensions/gsd/plan-validator.rkt"
         "../extensions/gsd/command-handlers.rkt")

;; ------------------------------------------------------------
;; Fixtures
;; ------------------------------------------------------------

(define INLINE-ONLY-PLAN "# Plan: Inline Only

## Wave 0: Fix the seam

## Files

- File: q/src.rkt
")

(define CANONICAL-INDEX-PLAN
  "# Plan: Canonical

## Waves

- [Inbox] W0: Fix the seam → waves/W0-fix-the-seam.md
")

(define ZERO-WAVE-PLAN "# Plan: Empty

Some prose, no wave grammar at all.
")

(define MIXED-PLAN
  "# Plan: Mixed

## Waves

- [Inbox] W0: Index wins → waves/W0-index-wins.md

## Wave 0: inline section that must NOT win

## Files

- File: q/from-inline.rkt
")

(define CANONICAL-WAVE-DOC
  "# W0

Status: Inbox

## Files

- File: q/from-index.rkt

## Verify

racket tests/test-dummy.rkt
")

;; make-tmp-planning : string? (listof (list/c string? string?)) -> path?
;; Create <tmp>/.planning with PLAN.md and optional wave-doc files
;; under <tmp>/.planning/waves/.
(define (make-tmp-planning plan-text wave-docs)
  (define tmp (make-temporary-file "w1-inline-enforce-~a" 'directory))
  (make-directory* (build-path tmp ".planning" "waves"))
  (with-output-to-file (build-path tmp ".planning" "PLAN.md") (lambda () (display plan-text)))
  (for ([wd wave-docs])
    (with-output-to-file (build-path tmp ".planning" "waves" (car wd))
                         (lambda () (display (cadr wd)))))
  tmp)

(define (rm-tmp-planning tmp)
  (delete-directory/files tmp))

;; with-tmp-planning : string? (listof (list/c string? string?)) procedure? -> any
(define (with-tmp-planning plan-text wave-docs thunk)
  (define tmp (make-tmp-planning plan-text wave-docs))
  (dynamic-wind void thunk (lambda () (rm-tmp-planning tmp))))

(define suite
  (test-suite "BUG-0023 residual (v1.00.22 W1): inline `## Wave N:` plan rejected at /go"

    ;; --------------------------------------------------------
    ;; Parser-level characterization (unchanged by W1)
    ;; --------------------------------------------------------

    (test-case "parse-plan-index returns 0 entries for an inline-only plan"
      ;; The index parser is authoritative: an inline-only PLAN.md has no
      ;; index grammar, so the index parse yields nothing.
      (check-equal? (parse-plan-index INLINE-ONLY-PLAN) '()))

    (test-case "parse-waves-from-markdown still parses inline sections"
      ;; The inline parser survives for transitional surfaces (docs,
      ;; archive tooling), but /go no longer ACCEPTS its output.
      (define waves (parse-waves-from-markdown INLINE-ONLY-PLAN))
      (check-equal? (length waves) 1)
      (check-equal? (count-inline-wave-sections INLINE-ONLY-PLAN) 1))

    (test-case "count-inline-wave-sections counts every inline section"
      (check-equal? (count-inline-wave-sections ZERO-WAVE-PLAN) 0)
      (check-equal? (count-inline-wave-sections CANONICAL-INDEX-PLAN) 0)
      ;; Mixed plans: inline sections are counted even when the index wins.
      (check-equal? (count-inline-wave-sections MIXED-PLAN) 1))

    ;; --------------------------------------------------------
    ;; The /go seam enforcement itself (validate-plan-for-go) is
    ;; pinned in `seam-suite` below: inline-only → named rejection,
    ;; canonical → silent ok, zero-wave → actionable no-waves error,
    ;; mixed → index wins.
    ))

(define seam-suite
  (test-suite "BUG-0023 residual (v1.00.22 W1): /go seam enforcement"

    (test-case "inline-only plan: /go rejects with inline-format-rejection-diagnostic"
      (define tmp (make-tmp-planning INLINE-ONLY-PLAN '()))
      (dynamic-wind void
                    (lambda ()
                      (define result (validate-plan-for-go tmp))
                      (match result
                        [(list 'error msg)
                         (check-equal? msg inline-format-rejection-diagnostic)
                         ;; The rejection must NAME the canonical index grammar so the
                         ;; author can migrate the plan shape immediately.
                         (check-true (regexp-match? #rx"- \\[Inbox\\] W0: Title → waves/W0-slug\\.md"
                                                    msg)
                                     "rejection names the canonical index grammar")
                         (check-true (string-contains? msg "no longer accepted")
                                     "rejection states the format is no longer accepted")]
                        [other (fail (format "expected (list 'error ...), got: ~e" other))]))
                    (lambda () (rm-tmp-planning tmp))))

    (test-case "canonical index plan: /go loads silently (no deprecation warnings)"
      (define tmp
        (make-tmp-planning CANONICAL-INDEX-PLAN
                           (list (list "W0-fix-the-seam.md" CANONICAL-WAVE-DOC))))
      (dynamic-wind void
                    (lambda ()
                      ;; Canonical text carries zero deprecated authoring forms.
                      (check-equal? (plan-format-deprecation-warnings CANONICAL-INDEX-PLAN) '())
                      (define result (validate-plan-for-go tmp))
                      (match result
                        [(list 'ok plan _ _)
                         (check-equal? (length (gsd-plan-waves plan)) 1)
                         (check-equal? (gsd-wave-title (first (gsd-plan-waves plan))) "Fix the seam")]
                        [other (fail (format "expected (list 'ok ...), got: ~e" other))]))
                    (lambda () (rm-tmp-planning tmp))))

    (test-case "zero-wave plan: /go still rejects with the actionable no-waves diagnostic"
      (define tmp (make-tmp-planning ZERO-WAVE-PLAN '()))
      (dynamic-wind void
                    (lambda ()
                      (define result (validate-plan-for-go tmp))
                      (match result
                        [(list 'error msg)
                         (check-true (string-contains? msg "Plan has no waves")
                                     "keeps the historical no-waves error")
                         (check-true (string-contains? msg no-waves-format-diagnostic)
                                     "keeps the actionable format diagnostic")
                         ;; BUG-0023 residual enforcement: the no-waves diagnostic names
                         ;; ONLY the canonical index grammar (inline is no longer an
                         ;; accepted form).
                         (check-false (string-contains? msg "Inline format in")
                                      "no-waves diagnostic no longer offers the inline format")]
                        [other (fail (format "expected (list 'error ...), got: ~e" other))]))
                    (lambda () (rm-tmp-planning tmp))))

    (test-case "mixed plan (index rows + inline sections): index wins, /go accepts"
      ;; A canonical index row makes the plan load via the index; stray
      ;; inline sections are inert noise (the fallback never runs when
      ;; the index parse succeeds).
      (define tmp (make-tmp-planning MIXED-PLAN (list (list "W0-index-wins.md" CANONICAL-WAVE-DOC))))
      (dynamic-wind void
                    (lambda ()
                      (define result (validate-plan-for-go tmp))
                      (match result
                        [(list 'ok plan _ _)
                         (check-equal? (length (gsd-plan-waves plan)) 1)
                         ;; INDEX wins: files come from waves/W0-index-wins.md, not the
                         ;; inline section.
                         (check-equal? (gsd-wave-files (first (gsd-plan-waves plan)))
                                       '("q/from-index.rkt"))]
                        [other (fail (format "expected (list 'ok ...), got: ~e" other))]))
                    (lambda () (rm-tmp-planning tmp))))

    (test-case "deprecated-but-parseable forms keep the ADVISORY warning path (warn, not block)"
      ;; Action 2: transitional authoring forms (relaxed status-less index
      ;; rows) still load at /go and still get the BUG-0035 advisory
      ;; warning — only the inline-only ACCEPTANCE was removed.
      (define RELAXED-PLAN
        "# Plan: Relaxed

## Waves

- W0: Status-less row → waves/W0-index-wins.md
")
      (check-true (pair? (plan-format-deprecation-warnings RELAXED-PLAN))
                  "advisory deprecation warning retained for relaxed rows")
      (define tmp
        (make-tmp-planning RELAXED-PLAN (list (list "W0-index-wins.md" CANONICAL-WAVE-DOC))))
      (dynamic-wind void
                    (lambda ()
                      (define result (validate-plan-for-go tmp))
                      (match result
                        [(list 'ok plan _ _)
                         (check-equal? (length (gsd-plan-waves plan)) 1)
                         ;; Relaxed rows still load: warn, not block.
                         (void plan)]
                        [other (fail (format "expected (list 'ok ...), got: ~e" other))]))
                    (lambda () (rm-tmp-planning tmp))))))

(module+ main
  (void (run-tests (test-suite "inline-format-deprecation"
                     suite
                     seam-suite))))
