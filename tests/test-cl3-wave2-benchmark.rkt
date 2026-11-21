#lang racket

;; tests/test-cl3-wave2-benchmark.rkt — Wave 2 evaluation harness contract.
;; @speed fast
;; @suite testing
;; @boundary unit

(require rackunit
         racket/file
         racket/runtime-path
         racket/string
         "../scripts/cl3-wave2-benchmark.rkt"
         "../scripts/cl3-wave2-tasks.rkt")

(define-runtime-path repo-root "..")

(test-case "three representative real q modules are selected"
  (check-equal? (length tasks) 3)
  (check-equal? (map cl3-task-name tasks)
                '("small-pure-utility" "typed-dependency-chain" "gsd-decision-module"))
  (for ([task (in-list tasks)])
    (check-true (file-exists? (build-path repo-root (cl3-task-module task))))
    (check-true (< (cl3-task-before task) (cl3-task-after task))))
  (check-true (cl3-task-typed? (cadr tasks))))

(test-case "probe source is typed only for the Typed Racket task"
  (define untyped (probe-code (car tasks) 41))
  (define typed (probe-code (cadr tasks) 7))
  (check-false (string-contains? untyped ": cl3-probe-value"))
  (check-true (string-contains? typed "(: cl3-probe-value (-> Integer))"))
  (check-true (string-contains? typed "(define (cl3-probe-value) 7)")))

(test-case "probe result parser rejects errors and accepts value plus memory"
  (check-equal? (parse-probe "(41 12345)") (cons 41 12345))
  (check-false (parse-probe "#hash((error . #t))"))
  (check-false (parse-probe "not-a-list"))
  (check-false (parse-probe "(41 not-an-integer)")))

(test-case "wave 2 harness remains outside production verification"
  (define qt (file->string (build-path repo-root "qt")))
  (define registry (file->string (build-path repo-root "tools" "registry-table" "core-tools.rkt")))
  (for ([text (in-list (list qt registry))])
    (check-false (string-contains? text "cl3-wave2"))
    (check-false (string-contains? text "racket_repl"))))
