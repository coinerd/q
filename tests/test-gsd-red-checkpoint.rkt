#lang racket/base
;; @speed fast
;; @suite extensions
;; @boundary unit
;; @timeout 120
(require rackunit
         "../extensions/gsd/red-checkpoint.rkt")
(define checkpoint
  (hasheq 'schema "red-checkpoint/v1"
          'plan-id (make-string 64 #\a)
          'wave 0
          'attempt-id "attempt-1"
          'attempt-fence 1
          'snapshot-sha256 (make-string 64 #\b)
          'repo "/repo"
          'branch "delivery"
          'origin "origin"
          'baseline-head (make-string 40 #\c)
          'baseline-tree (make-string 40 #\d)
          'checkpoint-head (make-string 40 #\e)
          'checkpoint-tree (make-string 40 #\f)
          'frozen-files '("src/main.rkt" "tests/regression.rkt")
          'selected-verify "raco test tests/regression.rkt"
          'test-files (list (hasheq 'path "tests/regression.rkt" 'sha256 (make-string 64 #\a)))))
(module+ test
  (test-case "preimplementation checkpoint is distinct from GREEN receipt authority"
    (check-true (valid-red-checkpoint? checkpoint))
    (check-false (hash-has-key? checkpoint 'green-proof-sha256))
    (check-false (valid-red-checkpoint? (hash-set checkpoint 'schema "delivery-receipt/v1")))
    (for ([field '(green-proof-sha256 owned-proof receipt-sha256)])
      (check-false (valid-red-checkpoint? (hash-set checkpoint field (make-string 64 #\a))))))
  (test-case "checkpoint freezes source, test bytes, selected Verify and FILES"
    (for ([field '(plan-id wave attempt-id attempt-fence snapshot-sha256 repo branch origin
                          baseline-head baseline-tree checkpoint-head checkpoint-tree
                          frozen-files selected-verify test-files)])
      (check-false (valid-red-checkpoint? (hash-remove checkpoint field))))
    (check-false (valid-red-checkpoint? (hash-set checkpoint 'attempt-fence -1)))
    (check-false (valid-red-checkpoint? (hash-set checkpoint 'selected-verify " ")))
    (check-false (valid-red-checkpoint? (hash-set checkpoint 'baseline-head (hash-ref checkpoint 'checkpoint-head)))))
  (test-case "literal frozen scope rejects ambiguous grants independently of author owner"
    (for ([path '("/tests/a.rkt" "tests//a.rkt" "tests/./a.rkt"
                  "tests/../a.rkt" "tests/*.rkt" "tests/a?.rkt"
                  "tests/[a].rkt" "tests/{a}.rkt" "tests/a\\b.rkt"
                  "tests/a:rkt" " tests/a.rkt" "tests/a.rkt\n")])
      (check-false
       (valid-red-checkpoint?
        (hash-set checkpoint 'frozen-files (list path)))))
    (check-false
     (valid-red-checkpoint?
      (hash-set checkpoint 'frozen-files '("tests/a.rkt" "tests/a.rkt")))))
  (test-case "test overlay cannot expand scope or include implementation"
    (for ([path '("src/main.rkt" "tests/outside.rkt" "tests/../src/main.rkt")])
      (check-false
       (valid-red-checkpoint?
        (hash-set checkpoint 'test-files
                  (list (hasheq 'path path 'sha256 (make-string 64 #\a)))))))
    (check-false (valid-red-checkpoint? (hash-set checkpoint 'test-files '())))
    (check-false
     (valid-red-checkpoint?
      (hash-set checkpoint 'test-files
                (append (hash-ref checkpoint 'test-files) (hash-ref checkpoint 'test-files)))))))
