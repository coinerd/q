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
         racket/file
         racket/list
         racket/runtime-path
         (file "../scripts/cl5/ownership-repair-eval.rkt"))

(define-runtime-path repo-root "..")

(define unit-fast (hash 'boundary "unit" 'speed 'fast))

(module+ test
  (test-case "eligibility follows the admission contract"
    (check-equal? (classify-candidate unit-fast '() 4 'rejected #t) 'eligible)
    (check-equal? (classify-candidate unit-fast '() 4 'admitted #t) 'already-admitted)
    (check-equal? (classify-candidate #f '() 4 'rejected #t) 'ineligible)
    ;; integration boundary
    (check-equal? (classify-candidate (hash 'boundary "integration" 'speed 'fast) '() 4 'rejected #t)
                  'needs-review)
    ;; slow tier (metadata arrives as a symbol)
    (check-equal? (classify-candidate (hash 'boundary "unit" 'speed 'slow) '() 4 'rejected #t)
                  'needs-review)
    ;; capability hit in the reviewed closure
    (check-equal?
     (classify-candidate unit-fast (list (hasheq 'file "x" 'capability 'subprocess)) 4 'rejected #t)
     'needs-review)
    ;; oversized reviewed closure
    (check-equal? (classify-candidate unit-fast '() 40 'rejected #t) 'needs-review)
    (check-equal? (classify-candidate unit-fast '() 2 'rejected #t) 'eligible))

  (test-case "failed or unparseable dependency inspection is never eligible"
    ;; Unit + fast + a clean scan over an EMPTY closure would look eligible;
    ;; unavailable closure evidence must block that.
    (check-equal? (classify-candidate unit-fast '() 0 'rejected #f) 'evidence-unavailable)
    ;; A real fixture: the candidate requires a module that cannot be parsed.
    (define root (make-temporary-file "cl5-eval-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (make-directory* (build-path root "tests"))
       (call-with-output-file (build-path root "broken.rkt")
                              (lambda (out) (display "#lang racket/base\n(define (oops\n" out)))
       (call-with-output-file (build-path root "tests/candidate.rkt")
                              (lambda (out)
                                (display "#lang racket/base\n(require \"../broken.rkt\")\n"
                                         out)))
       (define candidate (evaluate-candidate root "tests/candidate.rkt"))
       (check-equal? (hash-ref candidate 'verdict) 'evidence-unavailable)
       ;; closure-of refuses an unparseable member; the message is kept so the
       ;; failure is inspectable instead of looking like a clean empty closure.
       (define error (hash-ref candidate 'closure-error))
       (check-not-false error)
       (check-true (string-contains? error "does not parse")))
     (lambda () (delete-directory/files root))))

  (test-case "missing metadata keys are treated as ineligible, not eligible"
    (check-equal? (classify-candidate (hash) '() 1 'rejected #t) 'needs-review)
    (check-equal? (classify-candidate #f '() 1 'admitted #t) 'ineligible))

  (test-case "repository: the high-risk gaps evaluate to declared verdicts"
    (define rows (evaluate-candidates (path->complete-path repo-root) #:min-risk 5))
    (check-true (> (length rows) 0))
    (for ([row (in-list rows)])
      (check-true (and (memq (hash-ref row 'verdict)
                                '(eligible needs-review ineligible already-admitted evidence-unavailable))
                         #t))
      (check-true (> (length (hash-ref row 'candidates)) 0))
      (for ([c (in-list (hash-ref row 'candidates))])
        (check-true (and (memq (hash-ref c 'verdict)
                               '(eligible needs-review ineligible already-admitted evidence-unavailable))
                         #t)
                    (hash-ref c 'test))))))
