#lang racket/base

;; tests/test-gsd-relaxed-index.rkt — v0.75.7 W2 tests
;; Relaxed wave index parser: handles plans without [Status] brackets

(require rackunit
         rackunit/text-ui
         racket/file
         "../extensions/gsd/wave-docs.rkt")

(define-test-suite
 test-relaxed-index
 (test-case "parse-plan-index matches standard format with brackets"
   (define plan-text "# Plan: Test\n## Waves\n- [Inbox] W0: setup → waves/W0-setup.md\n")
   (define entries (parse-plan-index plan-text))
   (check-equal? (length entries) 1)
   (check-equal? (wave-index-entry-idx (car entries)) 0)
   (check-equal? (wave-index-entry-slug (car entries)) "setup")
   (check-equal? (wave-index-entry-status (car entries)) "Inbox"))
 (test-case "parse-plan-index matches relaxed format without brackets"
   (define plan-text "# Plan: Test\n## Waves\n- W0: Fix the bug\n- W1: Add tests\n")
   (define entries (parse-plan-index plan-text))
   (check-equal? (length entries) 2)
   (check-equal? (wave-index-entry-idx (car entries)) 0)
   (check-equal? (wave-index-entry-title (car entries)) "Fix the bug")
   (check-equal? (wave-index-entry-status (car entries)) "Inbox")
   (check-equal? (wave-index-entry-idx (cadr entries)) 1)
   (check-equal? (wave-index-entry-slug (car entries)) "fix-the-bug"))
 (test-case "parse-plan-index matches relaxed format with arrow target"
   (define plan-text "# Plan: Test\n## Waves\n- W0: Fix → waves/W0-fix.md\n")
   (define entries (parse-plan-index plan-text))
   (check-equal? (length entries) 1)
   (check-equal? (wave-index-entry-slug (car entries)) "fix"))
 (test-case "parse-plan-index ignores non-wave lines"
   (define plan-text
     "# Plan: Test\n## Overview\nSome description\n## Waves\n- W0: Fix\n## Constraints\n- Be careful\n")
   (define entries (parse-plan-index plan-text))
   (check-equal? (length entries) 1)
   (check-equal? (wave-index-entry-title (car entries)) "Fix"))
 (test-case "parse-plan-index handles mixed standard and relaxed"
   (define plan-text
     "# Plan: Test\n## Waves\n- [DONE] W0: setup → waves/W0-setup.md\n- W1: implement\n")
   (define entries (parse-plan-index plan-text))
   (check-equal? (length entries) 2)
   (check-equal? (wave-index-entry-status (car entries)) "DONE")
   (check-equal? (wave-index-entry-status (cadr entries)) "Inbox")))

(run-tests test-relaxed-index)
