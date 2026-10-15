#lang racket/base
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @timeout 120
(require rackunit
         "../extensions/gsd/evidence-author-repair.rkt")
(module+ test
  (test-case "phase validation preserves frozen tests and exact scope"
    (define files '("src/main.rkt" "tests/regression.rkt"))
    (define tests '("tests/regression.rkt"))
    (check-true (validate-author-phase-paths! 'test-author files tests tests))
    (check-true (validate-author-phase-paths! 'implementation files tests '("src/main.rkt")))
    (for ([case (in-list
                 (list (list 'test-author '("src/main.rkt"))
                       (list 'implementation tests)
                       (list 'implementation '("outside.rkt"))
                       (list 'unknown '())
                       (list 'test-author '("tests/../src/main.rkt"))))])
      (check-exn exn:fail?
                 (lambda () (validate-author-phase-paths! (car case) files tests (cadr case)))))
    (check-exn exn:fail?
               (lambda () (validate-author-phase-paths! 'test-author files '("outside.rkt") '()))))
  (test-case "author scope is exact frozen FILES, not a directory or pattern grant"
    (check-true (literal-author-scope? '("src/main.rkt" "tests/regression.rkt")))
    (for ([files (in-list '( () ("src/") ("../outside.rkt") ("/tmp/outside.rkt")
                            ("src/*.rkt") ("src/./main.rkt") ("src//main.rkt")
                            ("src/../main.rkt") ("src\\main.rkt")
                            ("src/main.rkt" "src/main.rkt")))])
      (check-false (literal-author-scope? files))))
  (test-case "an absent live guard grants no author permission"
    (check-equal?
     (hash-ref (evidence-author-repair-preflight "." "unused" 0 #:guard (lambda () #f)) 'code)
     "stale-live-guard")))
