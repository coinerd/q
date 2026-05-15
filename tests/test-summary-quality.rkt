#lang racket/base

;; tests/test-summary-quality.rkt — Tests for summary quality gates (SAL-04, CA-04)
;;
;; Tests for:
;;   - extract-key-entities (summary-entities.rkt)
;;   - check-entity-preservation (summary-entities.rkt)
;;   - entity-preservation-appendix (summary-entities.rkt)
;;   - simple-summary-text file path extraction (context-summary.rkt)

(require rackunit
         (only-in "../util/protocol-types.rkt" make-message make-text-part)
         (only-in "../runtime/context-assembly/summary-entities.rkt"
                  extract-key-entities
                  check-entity-preservation
                  entity-preservation-appendix)
         (only-in "../runtime/context-summary.rkt" simple-summary-text)
         racket/string)

;; Helper: create a simple message
(define (make-simple-message role text [id (gensym 'msg)])
  (make-message id #f role 'text (list (make-text-part text)) 0 #f))

;; ---------------------------------------------------------------------------
;; extract-key-entities
;; ---------------------------------------------------------------------------

(test-case "extract-key-entities finds file paths"
  (define msgs (list (make-simple-message 'user "Edit runtime/context-assembly.rkt and fix the bug")))
  (define entities (extract-key-entities msgs))
  (check-not-false (member "runtime/context-assembly.rkt" entities)
                   (format "Expected runtime/context-assembly.rkt in ~a" entities)))

(test-case "extract-key-entities finds definitions"
  (define msgs (list (make-simple-message 'assistant "(define (foo-bar x) (+ x 1))")))
  (define entities (extract-key-entities msgs))
  (check-not-false (ormap (lambda (e) (string-contains? e "define")) entities)
                   (format "Expected definition match in ~a" entities)))

(test-case "extract-key-entities respects max-entities limit"
  (define msgs
    (for/list ([i (in-range 50)])
      (make-simple-message 'assistant (format "Edit file~a.rkt and file~a.py" i i))))
  (define entities (extract-key-entities msgs 10))
  (check-true (<= (length entities) 10)))

;; ---------------------------------------------------------------------------
;; check-entity-preservation
;; ---------------------------------------------------------------------------

(test-case "check-entity-preservation detects missing entities"
  (define entities '("foo.rkt" "bar.rkt" "baz.rkt"))
  (define missing (check-entity-preservation entities "summary mentions foo.rkt"))
  (check-equal? missing '("bar.rkt" "baz.rkt")))

(test-case "check-entity-preservation returns empty when all preserved"
  (define entities '("foo.rkt" "bar.rkt"))
  (define missing (check-entity-preservation entities "summary has foo.rkt and bar.rkt"))
  (check-equal? missing '()))

;; ---------------------------------------------------------------------------
;; entity-preservation-appendix
;; ---------------------------------------------------------------------------

(test-case "entity-preservation-appendix generates appendix for missing"
  (define result (entity-preservation-appendix '("lost.rkt" "gone.rkt")))
  (check-true (string-contains? result "lost.rkt"))
  (check-true (string-contains? result "gone.rkt"))
  (check-true (string-contains? result "Key Entities")))

(test-case "entity-preservation-appendix returns empty for no missing"
  (check-equal? (entity-preservation-appendix '()) ""))

;; ---------------------------------------------------------------------------
;; simple-summary-text file extraction (CA-04)
;; ---------------------------------------------------------------------------

(test-case "simple-summary-text extracts file paths"
  (define msgs
    (for/list ([i (in-range 5)])
      (make-simple-message 'assistant (format "Edited ~a.rkt file" i))))
  (define text (simple-summary-text msgs))
  (check-true (string-contains? text "files:"))
  (check-true (string-contains? text "fallback")))

(test-case "simple-summary-text caps at 30 entries"
  (define msgs
    (for/list ([i (in-range 40)])
      (make-simple-message 'assistant (format "Entry ~a" i))))
  (define text (simple-summary-text msgs))
  ;; Entry 30 should NOT be present (0-indexed, capped at 30)
  (check-false (string-contains? text "Entry 30")))
