#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: unit

;;; test-check-version-expectations.rkt — tests for check-version-expectations
;;; (BUG-0009). The lint itself runs in CI (lint-all + release preflight);
;;; these tests pin its parsing/derivation helpers.
;;;
;;; NOTE (BUG-0009): this file intentionally contains NO literal copy of the
;;; current release version. Detector fixtures use synthetic version strings
;;; ("9.87.65"), so this self-test is scanned by the very lint it tests.

(require rackunit
         racket/string
         "../scripts/check-version-expectations.rkt")

;; ===========================================================================
;; canonical-version-from-content
;; ===========================================================================

(test-case "canonical-version-parses-define-q-version"
  (check-equal? (canonical-version-from-content
                 "#lang racket/base\n(define q-version \"9.87.65\")\n")
                "9.87.65"))

(test-case "canonical-version-parses-with-contract-out-wrappers"
  ;; util/version.rkt shape: provide contract-out + plain define, comments first.
  (check-equal? (canonical-version-from-content
                 (string-join '("#lang racket/base"
                                ";; STABILITY: stable"
                                "(provide (contract-out [q-version string?]))"
                                ""
                                "(define q-version \"9.87.65\")")
                              "\n"))
                "9.87.65"))

(test-case "canonical-version-absent-returns-false"
  (check-false (canonical-version-from-content "#lang racket/base\n(define other \"9.87.65\")\n")))

;; ===========================================================================
;; find-version-literals
;; ===========================================================================

(test-case "find-version-literals-reports-line-numbers"
  (define lines
    (list ";; header"
          "(check-equal? q-version \"9.87.65\")"
          "(check-true #t)"
          ";; mention of 9.87.65 in a comment still counts"))
  (check-equal? (find-version-literals lines "9.87.65") '(2 4)))

(test-case "find-version-literals-empty-when-none"
  (check-equal? (find-version-literals '("no literals here") "9.87.65") '()))

(test-case "find-version-literals-substring-is-conservative"
  ;; Matching is substring-based by design: while canonical is "9.87.6", a
  ;; line containing "9.87.65" is still flagged (an embedded version literal
  ;; is suspect). Conservative direction: false positive, never false negative.
  (check-equal? (find-version-literals '("x 9.87.65 y") "9.87.6") '(1)))

;; ===========================================================================
;; Derivation convention (the point of BUG-0009)
;; ===========================================================================

(test-case "expected-string-is-derived-not-duplicated"
  ;; Tests must format expectations from q-version, never re-type the literal.
  (define canonical (canonical-version-from-content "(define q-version \"9.87.65\")\n"))
  (check-equal? (format "q v~a — session restored" canonical)
                "q v9.87.65 — session restored"))
