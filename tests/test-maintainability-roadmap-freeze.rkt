#lang racket/base

;; tests/test-maintainability-roadmap-freeze.rkt — v0.99.87 W4 freeze fitness
;; @speed fast
;; @suite arch
;;
;; The roadmap freeze is a machine-readable contract: every MA finding has an
;; owner, target, closure proof, and release assignment; every follow-up wave
;; is frozen; and v0.99.91 cannot silently switch away from approved Path B.

(require rackunit
         racket/format
         racket/list
         racket/runtime-path
         racket/string)

(define-runtime-path freeze-path "../docs/architecture/maintainability-roadmap-freeze-v0.99.87.rktd")

(define (read-freeze)
  (call-with-input-file freeze-path read))

(define (field row
               key)
  (hash-ref row key (lambda () (fail-check (format "missing ~a in ~v" key row)))))

(define expected-finding-ids
  (for/list ([n (in-range 1 13)])
    (string->symbol (format "MA-~a" (~r n #:min-width 2 #:pad-string "0")))))

(define expected-wave-counts
  '((v0.99.88 . 5) (v0.99.89 . 5) (v0.99.90 . 6) (v0.99.91 . 5) (v0.99.92 . 6)))

(module+ test
  (define freeze (read-freeze))
  (define findings
    (field freeze
           'findings))
  (define milestones
    (field freeze
           'milestones))

  (test-case "F1 all twelve findings are uniquely and completely assigned"
    (define ids
      (map (lambda (row)
             (field row
                    'id))
           findings))
    (check-equal? (sort ids symbol<?) expected-finding-ids)
    (check-equal? (length ids) (length (remove-duplicates ids)))
    (for ([row (in-list findings)])
      (check-not-false (memq (field row
                                    'status)
                             '(closed guarded partial open rejected)))
      (check-true (non-empty-string? (field row
                                            'owner)))
      (check-true (pair? (field row
                                'target-waves)))
      (check-true (non-empty-string? (field row
                                            'closure-proof)))
      (check-true (non-empty-string? (field row
                                            'release)))
      (check-true (pair? (field row
                                'evidence)))))

  (test-case "F2 follow-up milestone wave maps are complete and total 27"
    (check-equal? (map (lambda (m)
                         (field m
                                'id))
                       milestones)
                  (map car expected-wave-counts))
    (for ([milestone (in-list milestones)]
          [expected (in-list expected-wave-counts)])
      (define waves
        (field milestone
               'waves))
      (define github-issues
        (field milestone
               'github-issues))
      (check-true (exact-positive-integer? (field milestone
                                                  'github-milestone)))
      (check-equal? (length github-issues) (length waves))
      (check-equal? (length (remove-duplicates github-issues)) (length waves))
      (check-equal? (length waves) (cdr expected))
      (check-equal? (length (remove-duplicates (map (lambda (w)
                                                      (field w
                                                             'id))
                                                    waves)))
                    (length waves))
      (for ([wave (in-list waves)])
        (check-true (non-empty-string? (field wave
                                              'title)))
        (check-true (non-empty-string? (field wave
                                              'goal)))
        (check-true (non-empty-string? (field wave
                                              'gate)))
        (check-true (non-empty-string? (field wave
                                              'acceptance)))))
    (check-equal? (apply +
                         (map (lambda (m)
                                (length (field m
                                               'waves)))
                              milestones))
                  27))

  (test-case "F3 provider campaign is immutably Path B"
    (define provider
      (findf (lambda (m)
               (eq? (field m
                           'id)
                    'v0.99.91))
             milestones))
    (check-eq? (field provider
                      'approved-path)
               'path-b)
    (check-equal? (map (lambda (w)
                         (field w
                                'id))
                       (field provider
                              'waves))
                  '(W0 W1-B W2-B W3-B W4-B))
    (check-true (regexp-match? #rx"fewer than two"
                               (field provider
                                      'path-rationale))))

  (test-case "F4 amendment and closure policy are frozen"
    (check-equal? (field freeze
                         'schema-version)
                  1)
    (check-eq? (field freeze
                      'status)
               'frozen)
    (check-true (field freeze
                       'amendment-required?))
    (check-true (field freeze
                       'scope-stop?))
    (check-equal? (field freeze
                         'series-wave-count)
                  32)
    (check-equal? (field freeze
                         'follow-up-wave-count)
                  27))

  (test-case "F5 freeze and W4 candidate SHAs are pinned"
    (check-equal? (field freeze
                         'freeze-candidate-sha)
                  "d18c6898ded4086aa534316d167de184fdb6ec5a")
    (check-equal? (field freeze
                         'w4-candidate-sha)
                  "93045f4d1e6c9396ec94312c7bbc93958c41847a")))
