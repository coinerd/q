#lang racket

;; @speed slow  ;; @suite default
;; @boundary unit

;; tests/test-cl5-guard-evaluation.rkt — advisory guard evaluation over the
;; frozen corpus (issue #9996).
;;
;;  1. the corpus list is the registered executable case set
;;  2. every corpus change escalates with at least one named reason and never
;;     reports 'selected
;;  3. clean controls with an audited detector are selected without escalation
;;  4. the D5 case names tests/test-gsd-wave-status.rkt

(require rackunit
         racket/list
         racket/runtime-path
         (file "../scripts/cl5/guard-evaluation.rkt"))

(define-runtime-path repo-root "..")

(module+ test
  (test-case "the corpus list is the registered executable case set"
    (define entries (corpus-changes))
    (check-true (> (length entries) 0))
    (check-true (and (member 'D5 (map (lambda (e) (hash-ref e 'case)) entries)) #t))
    (define d5
      (for/first ([e (in-list entries)]
                  #:when (eq? (hash-ref e 'case) 'D5))
        e))
    (check-equal? (hash-ref d5 'changed) (list "extensions/gsd/wave-status.rkt")))

  (test-case "every corpus change escalates and none reports a green"
    (define evaluation (evaluate-guard-corpus (path->complete-path repo-root)))
    (for ([row (in-list (hash-ref evaluation 'corpus))])
      (check-equal? (hash-ref row 'verdict) 'escalate (symbol->string (hash-ref row 'case)))
      (check-equal? (hash-ref row 'lane) 'comprehensive (symbol->string (hash-ref row 'case)))
      (check-true (pair? (hash-ref row 'escalations)) (symbol->string (hash-ref row 'case)))
      (check-equal? (hash-ref row 'claims) 'attribution-and-admission-only)))

  (test-case "the D5 advisory names the documented detector"
    (define evaluation (evaluate-guard-corpus (path->complete-path repo-root)))
    (define d5
      (for/first ([row (in-list (hash-ref evaluation 'corpus))]
                  #:when (eq? (hash-ref row 'case) 'D5))
        row))
    (check-not-false d5)
    (check-true (and (member 'unadmitted-cover (hash-ref d5 'escalations)) #t))
    (check-true (and (member "tests/test-gsd-wave-status.rkt" (hash-ref d5 'named-tests)) #t)))

  (test-case "clean controls with audited detectors are selected, not escalated"
    (define evaluation (evaluate-guard-corpus (path->complete-path repo-root)))
    (define controls (hash-ref evaluation 'controls))
    (check-true (> (length controls) 0))
    (for ([row (in-list controls)])
      (check-equal? (hash-ref row 'verdict) 'selected (symbol->string (hash-ref row 'case)))
      (check-equal? (hash-ref row 'escalations) '() (symbol->string (hash-ref row 'case))))))
