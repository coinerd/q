#lang racket/base

;; BOUNDARY: integration
;; @suite workflows
;; @boundary integration
;; @speed fast
;; @mutates env
;; @isolation temp-dir
;; Test that sandbox integrates with workflow temp-project fixture.

(require rackunit
         racket/file
         racket/path
         "../helpers/test-sandbox.rkt"
         "fixtures/temp-project.rkt")

(test-case "sandbox and temp-project coexist"
  ;; Create a sandboxed project and verify temp-project works inside it
  (with-test-sandbox #:isolate-cwd? #t
                     (lambda (sb)
                       ;; Verify we're in sandbox project dir
                       (check-true (directory-exists? (test-sandbox-project-dir sb)))
                       ;; Create a temp project inside the sandbox
                       (define-values (proj-dir sess-dir)
                         (make-temp-project '(("hello.txt" . "world"))))
                       (check-true (directory-exists? proj-dir))
                       (check-true (file-exists? (build-path proj-dir "hello.txt")))
                       (check-equal? (file->string (build-path proj-dir "hello.txt")) "world")
                       ;; Cleanup temp project
                       (cleanup-temp-project! proj-dir sess-dir))))

(test-case "sandbox env isolation doesn't break temp-project"
  (with-test-sandbox #:isolate-home? #t
                     (lambda (sb)
                       (define-values (proj-dir sess-dir)
                         (make-temp-project '(("test.rkt" . "#lang racket"))))
                       (check-true (directory-exists? proj-dir))
                       (cleanup-temp-project! proj-dir sess-dir))))
