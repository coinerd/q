#lang racket/base

;; @speed fast  ;; @suite extensions

;; tests/test-gsd-wave-gen-validation.rkt — v0.75.7 W1 tests
;; Wave generation validation: wave-exists?, parse-plan-index, wave cleanup

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string
         "../extensions/gsd/wave-docs.rkt")

(define-test-suite
 test-wave-gen-validation
 (test-case "wave-exists? returns #f for nonexistent wave"
   (define tmp (make-temporary-file "wave-test-~a" 'directory))
   (define result (wave-exists? tmp 0 "nonexistent"))
   (check-false result)
   (delete-directory/files tmp))
 (test-case "wave-exists? returns #t after writing wave doc"
   (define tmp (make-temporary-file "wave-test-~a" 'directory))
   (write-wave-doc! tmp 0 "test-slug" "## Action\nDo stuff" "Inbox")
   (check-true (wave-exists? tmp 0 "test-slug"))
   (delete-directory/files tmp))
 (test-case "parse-plan-index extracts wave entries with slugs"
   (define plan-text
     "# Plan: Test\n## Waves\n- [Inbox] W0: setup → waves/W0-setup.md\n- [Inbox] W1: implement → waves/W1-implement.md\n")
   (define entries (parse-plan-index plan-text))
   (check-equal? (length entries) 2)
   (check-equal? (wave-index-entry-idx (car entries)) 0)
   (check-equal? (wave-index-entry-idx (cadr entries)) 1)
   (check-equal? (wave-index-entry-slug (car entries)) "setup")
   (check-equal? (wave-index-entry-slug (cadr entries)) "implement"))
 (test-case "missing first wave doc detected via wave-exists?"
   (define tmp (make-temporary-file "wave-test-~a" 'directory))
   (define plan-text "# Plan: Test\n## Waves\n- [Inbox] W0: setup → waves/W0-setup.md\n")
   (define entries (parse-plan-index plan-text))
   (define first-entry (car entries))
   (check-false
    (wave-exists? tmp (wave-index-entry-idx first-entry) (wave-index-entry-slug first-entry)))
   (delete-directory/files tmp))
 (test-case "present first wave doc passes wave-exists? check"
   (define tmp (make-temporary-file "wave-test-~a" 'directory))
   (define plan-text "# Plan: Test\n## Waves\n- [Inbox] W0: setup → waves/W0-setup.md\n")
   (define entries (parse-plan-index plan-text))
   (define first-entry (car entries))
   (write-wave-doc! tmp
                    (wave-index-entry-idx first-entry)
                    (wave-index-entry-slug first-entry)
                    "## Action\nDo stuff"
                    "Inbox")
   (check-true
    (wave-exists? tmp (wave-index-entry-idx first-entry) (wave-index-entry-slug first-entry)))
   (delete-directory/files tmp)))

(run-tests test-wave-gen-validation)
