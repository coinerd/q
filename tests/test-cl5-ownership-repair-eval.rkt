#lang racket

;; @speed slow  ;; @suite default
;; @boundary unit

;; tests/test-cl5-ownership-repair-eval.rkt — offline eligibility evaluation for
;; the risk-prioritized ownership repair (issue #9995).
;;
;;  1. eligibility follows the existing admission contract exactly
;;  2. an already-admitted candidate is reported, never re-admitted
;;  3. one eligible detector makes a module eligible; review blocks otherwise
;;  4. repository data: the high-risk gaps are evaluated, and every verdict is
;;     one of the declared values

(require rackunit
         racket/list
         racket/runtime-path
         (file "../scripts/cl5/ownership-repair-eval.rkt"))

(define-runtime-path repo-root "..")

(define unit-fast (hash 'boundary "unit" 'speed 'fast))

(module+ test
  (test-case "eligibility follows the admission contract"
    (check-equal? (classify-candidate unit-fast '() 4 'rejected) 'eligible)
    (check-equal? (classify-candidate unit-fast '() 4 'admitted) 'already-admitted)
    (check-equal? (classify-candidate #f '() 4 'rejected) 'ineligible)
    ;; integration boundary
    (check-equal? (classify-candidate (hash 'boundary "integration" 'speed 'fast) '() 4 'rejected)
                  'needs-review)
    ;; slow tier (metadata arrives as a symbol)
    (check-equal? (classify-candidate (hash 'boundary "unit" 'speed 'slow) '() 4 'rejected)
                  'needs-review)
    ;; capability hit in the reviewed closure
    (check-equal?
     (classify-candidate unit-fast (list (hasheq 'file "x" 'capability 'subprocess)) 4 'rejected)
     'needs-review)
    ;; oversized reviewed closure
    (check-equal? (classify-candidate unit-fast '() 40 'rejected) 'needs-review)
    (check-equal? (classify-candidate unit-fast '() 2 'rejected) 'eligible))

  (test-case "missing metadata keys are treated as ineligible, not eligible"
    (check-equal? (classify-candidate (hash) '() 1 'rejected) 'needs-review)
    (check-equal? (classify-candidate #f '() 1 'admitted) 'ineligible))

  (test-case "repository: the high-risk gaps evaluate to declared verdicts"
    (define rows (evaluate-candidates (path->complete-path repo-root) #:min-risk 5))
    (check-true (> (length rows) 0))
    (for ([row (in-list rows)])
      (check-true (and (memq (hash-ref row 'verdict)
                                '(eligible needs-review ineligible already-admitted))
                         #t))
      (check-true (> (length (hash-ref row 'candidates)) 0))
      (for ([c (in-list (hash-ref row 'candidates))])
        (check-true (and (memq (hash-ref c 'verdict)
                               '(eligible needs-review ineligible already-admitted))
                         #t)
                    (hash-ref c 'test))))))
