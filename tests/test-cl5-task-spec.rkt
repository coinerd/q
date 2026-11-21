#lang racket

;; @speed fast  ;; @suite default
;; @boundary unit

;; tests/test-cl5-task-spec.rkt — CL5 W1 (#9982): the frozen-corpus projection
;; in scripts/cl5/task-spec.rkt is a copy, not a second authority. These tests
;; pin that every embedded prompt string and every embedded .rkt path appears
;; VERBATIM in docs/reports/CL0-AGENT-TASK-BENCHMARK-003e425d.md, and that the
;; validator rejects malformed specs.

(require rackunit
         racket/file
         racket/runtime-path
         racket/string
         "../scripts/cl5/task-spec.rkt")

(define-runtime-path cl0-report "../docs/reports/CL0-AGENT-TASK-BENCHMARK-003e425d.md")
(define report-text (file->string cl0-report))

;; Collect every .rkt path embedded anywhere in a spec (ownership set,
;; owner-location paths, and recursively inside the answer-spec hash).
(define (spec-paths spec)
  (define (walk value)
    (cond
      [(and (string? value) (string-suffix? value ".rkt")) (list value)]
      [(hash? value) (append-map walk (hash-values value))]
      [(list? value) (append-map walk value)]
      [else '()]))
  (remove-duplicates (append (hash-ref spec 'ownership-set)
                             (hash-ref spec 'owner-location-paths)
                             (walk (hash-ref spec 'answer-spec)))))

(module+ test
  (test-case "specs are exactly the five frozen tasks in corpus order"
    (check-equal? (map (lambda (spec) (hash-ref spec 'id)) task-specs) '(T1 T3 T4 T2 T5))
    (check-equal? (length task-specs) 5))

  (test-case "every spec cites the frozen corpus revision"
    (for ([spec (in-list task-specs)])
      (check-equal? (hash-ref spec 'corpus-revision) "003e425d2f1206436eb9c0b84353604ce85bb082")))

  (test-case "kinds and oracle kinds match the CL0 report structure"
    (check-equal? (hash-ref (task-spec 'T1) 'kind) 'navigation)
    (check-equal? (hash-ref (task-spec 'T3) 'kind) 'navigation)
    (check-equal? (hash-ref (task-spec 'T4) 'kind) 'navigation)
    (check-equal? (hash-ref (task-spec 'T2) 'kind) 'implementation)
    (check-equal? (hash-ref (task-spec 'T5) 'kind) 'implementation)
    (check-equal? (hash-ref (task-spec 'T1) 'oracle) 'text)
    (check-equal? (hash-ref (task-spec 'T2) 'oracle) 'execution)
    (check-equal? (hash-ref (task-spec 'T5) 'oracle) 'execution))

  (test-case "every embedded prompt appears verbatim in the CL0 report"
    (for ([spec (in-list task-specs)])
      (check-true (string-contains? report-text (hash-ref spec 'prompt))
                  (format "prompt of ~a not found verbatim in the report" (hash-ref spec 'id)))))

  (test-case "every embedded .rkt path appears verbatim in the CL0 report"
    (for* ([spec (in-list task-specs)]
           [path (in-list (spec-paths spec))])
      (check-true
       (string-contains? report-text path)
       (format "path ~a of ~a not found verbatim in the report" path (hash-ref spec 'id)))))

  (test-case "T3 answer set has the 13 frozen dependents and fan-out 0"
    (define answer (hash-ref (task-spec 'T3) 'answer-spec))
    (check-equal? (length (hash-ref answer 'direct-dependents)) 13)
    (check-equal? (hash-ref answer 'require-fan-out) 0))

  (test-case "task-spec lookup by id; unknown id raises a clear error"
    (check-equal? (hash-ref (task-spec 'T4) 'id) 'T4)
    (check-exn #rx"unknown task id" (lambda () (task-spec 'T9))))

  (test-case "validator accepts the frozen corpus projection"
    (check-equal? (validate-task-specs task-specs) task-specs))

  (test-case "validator rejects an empty prompt"
    (define bad (hash-set (task-spec 'T1) 'prompt ""))
    (check-exn #rx"empty prompt" (lambda () (validate-task-specs (list bad)))))

  (test-case "validator rejects a missing ownership set"
    (define bad (hash-remove (task-spec 'T2) 'ownership-set))
    (check-exn #rx"no ownership set" (lambda () (validate-task-specs (list bad)))))

  (test-case "validator rejects an implementation task with an empty ownership set"
    (define bad (hash-set (task-spec 'T2) 'ownership-set '()))
    (check-exn #rx"must own at least one path" (lambda () (validate-task-specs (list bad)))))

  (test-case "validator rejects an unknown kind"
    (define bad (hash-set (task-spec 'T1) 'kind 'mystery))
    (check-exn #rx"unknown kind" (lambda () (validate-task-specs (list bad)))))

  (test-case "validator rejects a reordered or incomplete task list"
    (check-exn #rx"corpus order" (lambda () (validate-task-specs (reverse task-specs))))
    (check-exn #rx"corpus order" (lambda () (validate-task-specs (take task-specs 4))))))
