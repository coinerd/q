#lang racket

;; tests/test-cl3-wave3-benchmark.rkt — Wave 3 evaluation harness contract.
;; @speed fast
;; @suite testing
;; @boundary unit

(require rackunit
         racket/file
         racket/path
         racket/runtime-path
         racket/string
         "../scripts/cl3-wave3-benchmark.rkt"
         "../scripts/cl3-wave3-tasks.rkt")

(define-runtime-path repo-root "..")

(define (copy-module! root task)
  (define source (build-path repo-root (cl3-task-module task)))
  (define target (build-path root (cl3-task-module task)))
  (make-directory* (path-only target))
  (copy-file source target #t)
  target)

(test-case "three real component-logic edits are selected"
  (check-equal? (length tasks) 3)
  (check-equal? (map cl3-task-module tasks)
                '("util/hash-utils.rkt" "runtime/iteration/retry-policy.rkt"
                                        "extensions/gsd/campaign-decision.rkt"))
  (for ([task (in-list tasks)])
    (check-true (file-exists? (build-path repo-root (cl3-task-module task))))
    (check-not-equal? (cl3-task-expected-before task) (cl3-task-expected-after task))))

(test-case "every edit anchor is unique in its real module"
  ;; Anchors must occur exactly once, otherwise the edit is ambiguous.
  (define (count haystack needle)
    (length (regexp-match* (regexp (regexp-quote needle)) haystack)))
  (for ([task (in-list tasks)])
    (define source (file->string (build-path repo-root (cl3-task-module task))))
    (check-equal? (count source (cl3-task-before-snippet task)) 1 (cl3-task-name task))))

(test-case "apply-edit! and reset-source! round-trip on a copy"
  (define root (make-temporary-directory "cl3-wave3-test-~a"))
  (dynamic-wind void
                (lambda ()
                  (for ([task (in-list tasks)])
                    (copy-module! root task)
                    (define original (file->string (build-path root (cl3-task-module task))))
                    (apply-edit! root task "test edit")
                    (check-true (string-contains? (file->string (build-path root
                                                                            (cl3-task-module task)))
                                                  (cl3-task-after-snippet task)))
                    (reset-source! root task)
                    (check-equal? (file->string (build-path root (cl3-task-module task))) original)
                    (delete-directory/files (path-only (build-path root (cl3-task-module task))))))
                (lambda () (delete-directory/files root))))

(test-case "expression code requires the module and prints the public value"
  (define code (expression-code (make-temporary-directory "cl3-wave3-expr-~a") (car tasks)))
  (check-true (string-contains? code "(require (file "))
  (check-true (string-contains? code "hash-ref*"))
  (check-true (string-contains? code "displayln")))

(test-case "first-output-line ignores blank leading lines"
  (check-equal? (first-output-line "\n  absent \n") "absent")
  (check-equal? (first-output-line "") ""))

(test-case "wave 3 harness remains outside production verification"
  (define qt (file->string (build-path repo-root "qt")))
  (define registry (file->string (build-path repo-root "tools" "registry-table" "core-tools.rkt")))
  (for ([text (in-list (list qt registry))])
    (check-false (string-contains? text "cl3-wave3"))
    (check-false (string-contains? text "racket_repl"))))
