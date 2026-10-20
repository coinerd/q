#lang racket
;; @speed fast
;; @suite extensions
;; @boundary unit
;; @timeout 120
;; New API contract tests; no historical/live behavioral RED claim.
(require rackunit
         "../extensions/gsd/supplemental-evidence-format.rkt"
         "../extensions/gsd/effective-verify-binding.rkt"
         "../extensions/gsd/evidence-repair-format.rkt"
         "../extensions/gsd/review-proof-format.rkt")
(define context
  (hasheq 'plan-id
          (make-string 64 #\a)
          'wave
          2
          'attempt-id
          "local-attempt"
          'attempt-fence
          3
          'receipt-generation
          4
          'receipt-sha256
          (make-string 64 #\b)
          'evidence-revision
          0
          'repo
          "/fixture/repo"
          'branch
          "fixture-work"
          'origin
          "https://github.com/local/fixture"
          'head
          (make-string 40 #\c)
          'tree
          (make-string 40 #\d)
          'snapshot-sha256
          (make-string 64 #\e)
          'green-proof-sha256
          (make-string 64 #\f)))
(define base-command
  (string-join
   '("raco fmt -i runtime/local.rkt" "raco make runtime/local.rkt"
                                     "racket scripts/run-tests.rkt tests/test-original.rkt"
                                     "racket scripts/run-tests.rkt --suite fast"
                                     "racket scripts/lint-all.rkt")
   " && "))
(define test-path "tests/test-local-witness.rkt")
(define bounds
  (hasheq 'model-dispatches 1 'verify-timeout-sec 14400 'probe-timeout-sec 600 'deadline-sec 18000))
(define task
  (seal-supplemental-record (hasheq 'kind
                                    "supplemental-evidence-task/v1"
                                    'context
                                    context
                                    'base-command
                                    base-command
                                    'effective-command
                                    (make-effective-verify-command base-command test-path)
                                    'test-path
                                    test-path
                                    'harness-path
                                    "tests/helpers/local-witness.rkt"
                                    'owner-paths
                                    '("README.md" "reports/local.json")
                                    'baseline-head
                                    (make-string 40 #\1)
                                    'baseline-tree
                                    (make-string 40 #\2)
                                    'control-plane-head
                                    (make-string 40 #\3)
                                    'bounds
                                    bounds)))
(define (task-with key value)
  (seal-supplemental-record (hash-set task key value)))

(module+ test
  (test-case "task integrity reuses canonical evidence digest exactly"
    (check-true (valid-supplemental-task? task))
    (check-true (valid-supplemental-record? task))
    (check-equal? (supplemental-record-digest task) (canonical-evidence-digest task))
    (check-equal? (hash-ref task 'sha256) (canonical-evidence-digest task))
    (check-equal? (seal-supplemental-record task) task)
    (check-true (immutable? task))
    (check-equal? (supplemental-record-digest (make-immutable-hash (reverse (hash->list task))))
                  (supplemental-record-digest task)))
  (test-case "sealing freezes caller strings without retaining mutable aliases"
    (define head (make-string 40 #\a))
    (define sealed (task-with 'baseline-head head))
    (string-set! head 0 #\b)
    (check-true (valid-supplemental-task? sealed))
    (check-true (immutable? (hash-ref sealed 'baseline-head)))
    (check-true (immutable? (hash-ref sealed 'sha256)))
    (check-not-equal? (hash-ref sealed 'baseline-head) head))
  (test-case "every mandatory field is required"
    (for ([key (hash-keys task)])
      (check-false (valid-supplemental-task? (hash-remove task key)) (symbol->string key))))
  (test-case "mutability, noncanonical payload and integrity refuse"
    (check-false (valid-supplemental-task? (hash-copy task)))
    (check-false (valid-supplemental-task? (hash-set task 'baseline-head (make-string 40 #\4))))
    (check-false (valid-supplemental-task? (hash-set task 'sha256 (make-string 64 #\0))))
    (check-false (valid-supplemental-task?
                  (hash-set task 'sha256 (string-upcase (hash-ref task 'sha256)))))
    (check-false (valid-supplemental-task? (task-with 'extra "unknown task field")))
    (check-exn exn:fail? (lambda () (seal-supplemental-record (hash-copy task))))
    (check-exn exn:fail?
               (lambda () (seal-supplemental-record (hash-set task 'context (hash-copy context))))))
  (test-case "identity and kind validation has no campaign-specific constants"
    (for ([change (list (cons 'kind "supplemental-evidence-task/v2")
                        (cons 'kind "declared-verify-green")
                        (cons 'baseline-head (make-string 39 #\a))
                        (cons 'baseline-tree (make-string 40 #\G))
                        (cons 'control-plane-head #f)
                        (cons 'context (hash-remove context 'receipt-sha256))
                        (cons 'context (hash-set context 'attempt-fence -1))
                        (cons 'context (hash-set context 'green-proof-sha256 "invalid")))])
      (check-false (valid-supplemental-task? (task-with (car change) (cdr change))))))
  (test-case "exact bounded policy: dispatch 1, verify 14400, probe 600, deadline one day"
    (for ([change (list (cons 'model-dispatches 0)
                        (cons 'model-dispatches 2)
                        (cons 'model-dispatches 1.0)
                        (cons 'verify-timeout-sec 14401)
                        (cons 'verify-timeout-sec 0)
                        (cons 'probe-timeout-sec 601)
                        (cons 'probe-timeout-sec -1)
                        (cons 'deadline-sec 0)
                        (cons 'deadline-sec 86401)
                        (cons 'deadline-sec +inf.0)
                        (cons 'deadline-sec 1/2))])
      ;; Unsupported JSON numbers must fail sealing rather than become accepted records.
      (define b (hash-set bounds (car change) (cdr change)))
      (cond
        [(or (inexact? (cdr change)) (not (integer? (cdr change))))
         (check-exn exn:fail? (lambda () (task-with 'bounds b)))]
        [else (check-false (valid-supplemental-task? (task-with 'bounds b)))]))
    (check-false (valid-supplemental-task? (task-with 'bounds (hash-remove bounds 'deadline-sec))))
    (check-false (valid-supplemental-task? (task-with 'bounds (hash-set bounds 'retries 1)))))
  (test-case "exact safe scopes and effective command are required"
    (for ([change (list (cons 'test-path "tests/../escape.rkt")
                        (cons 'test-path "runtime/production.rkt")
                        (cons 'harness-path "tests/not-a-helper.rkt")
                        (cons 'harness-path "tests/helpers/../escape.rkt")
                        (cons 'owner-paths '("../escape"))
                        (cons 'owner-paths '("README.md" "README.md"))
                        (cons 'owner-paths (list test-path))
                        (cons 'owner-paths '("tests/helpers/local-witness.rkt"))
                        (cons 'owner-paths '("/absolute"))
                        (cons 'owner-paths '("reports//x"))
                        (cons 'owner-paths '("reports/*.json"))
                        (cons 'owner-paths #f)
                        (cons 'effective-command base-command)
                        (cons 'effective-command (string-append base-command " && true")))])
      (check-false (valid-supplemental-task? (task-with (car change) (cdr change))))))
  (test-case "other kinds are descriptors, not approval or execution authority"
    (for ([kind '("supplemental-evidence-candidate/v1" "supplemental-evidence-grant/v1"
                                                       "supplemental-evidence-approval/v1"
                                                       "supplemental-evidence-supersession/v1"
                                                       "supplemental-evidence-promotion/v1")])
      (define descriptor
        (seal-supplemental-record (hasheq 'kind
                                          kind
                                          'context
                                          context
                                          'task-sha256
                                          (hash-ref task 'sha256)
                                          'details
                                          (hasheq 'operation "fixture" 'paths '("local")))))
      (check-true (valid-supplemental-record? descriptor))
      (check-false (valid-supplemental-task? descriptor))
      (check-false (valid-owned-proof-manifest? descriptor))
      (check-false (valid-supplemental-record? (hash-remove descriptor 'task-sha256)))
      (check-false (valid-supplemental-record?
                    (seal-supplemental-record (hash-set descriptor 'task-sha256 "bad"))))))
  (test-case "legacy and unknown records never silently dispatch as supplemental"
    (for ([record (list #f
                        '()
                        "record"
                        (hasheq)
                        (seal-supplemental-record (hasheq 'kind "declared-verify-green"))
                        (seal-supplemental-record (hasheq 'kind "supplemental-evidence-mystery/v1"))
                        (seal-supplemental-record
                         (hasheq 'kind "supplemental-evidence-grant/v1" 'approved #t)))])
      (check-false (valid-supplemental-record? record))
      (check-false (valid-supplemental-task? record)))))
