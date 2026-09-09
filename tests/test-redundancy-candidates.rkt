#lang racket/base
;; tests/test-redundancy-candidates.rkt — W5 semantic-redundancy TDD (v1.00.28)
;; @covers scripts/run-tests/redundancy-candidates.rkt
;; @covers scripts/run-tests/mutation-pilot.rkt
;; @speed fast
;; @suite run-tests
;; @boundary unit
;;
;; Contract under test (wave TDD obligations):
;;   1. detector pairs test-cases sharing @covers identity + fixture builders
;;      + stimulus shape but asserting different post-stimulus aspects;
;;   2. groups carry a consolidation-policy recommendation with rank
;;      share-setup(1) < merge-complementary(2) < keep-both(3) < delete(4);
;;   3. a detection-power ambiguity MUST resolve to "keep-both", never delete;
;;   4. candidates.json emission is deterministic and evidence-linked;
;;   5. adequacy --check (mutation-pilot) is fail-closed: every changed test
;;      count carries before/after counts, a behavior mapping, a failure
;;      history review, and mutation evidence; a previously killed mutant
;;      surviving the consolidated suite is RED; suites must be green.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string
         (only-in "../scripts/run-tests/redundancy-candidates.rkt"
                  parse-test-file
                  detect-redundancy-groups
                  candidates-json
                  preference-rank)
         (only-in "../scripts/run-tests/mutation-pilot.rkt" validate-adequacy))

;; ---------- helpers ----------

(define (write-file! path content)
  (call-with-output-file path (lambda (p) (display content p)) #:exists 'replace))

(define (make-sample-tree!)
  (define root (make-temporary-file "red-cand-~a" 'directory))
  (make-directory* (build-path root "tests"))
  (write-file! (build-path root "tests" "a.rkt")
               #<<EOF
#lang racket/base
;; @covers extensions/widget.rkt
(require rackunit)
(define (make-widget-fixture!) (list 'widget))
(test-case "widget asserts label"
  (define w (make-widget-fixture!))
  (check-equal? w (list 'widget))
  (check-true (label-of w)))
(test-case "widget asserts size"
  (define w (make-widget-fixture!))
  (check-equal? (size-of w) 3))
EOF
               )
  (write-file! (build-path root "tests" "c.rkt")
               #<<EOF
#lang racket/base
;; @covers extensions/other.rkt
(require rackunit)
(define (make-widget-fixture!) (list 'widget))
(test-case "other uses widget fixture"
  (define w (make-widget-fixture!))
  (check-true (pair? w)))
EOF
               )
  (write-file! (build-path root "tests" "d.rkt")
               #<<EOF
#lang racket/base
;; @covers extensions/gadget.rkt
(require rackunit)
(test-case "gadget case one"
  (define g (make-gadget))
  (check-equal? g 7))
(test-case "gadget case two"
  (define g (make-gadget))
  (check-equal? g 7))
EOF
               )
  (write-file! (build-path root "tests" "e.rkt")
               #<<EOF
#lang racket/base
;; @covers extensions/gizmo.rkt
(require rackunit)
(test-case "gizmo without prelude"
  (check-true (gizmo?)))
(test-case "gizmo after priming"
  (prime-gizmo! 3)
  (check-true (gizmo?)))
EOF
               )
  root)

(define (find-record rs name)
  (findf (lambda (r) (string=? (hash-ref r 'name) name)) rs))

;; ---------- detector: parsing ----------

(define parser-suite
  (test-suite "parse-test-file"
    (test-case "extracts covers, fixtures, stimulus, assertions per test-case"
      (define root (make-sample-tree!))
      (define rs (parse-test-file (build-path root "tests" "a.rkt")))
      (check-equal? (length rs) 2)
      (define r1 (find-record rs "widget asserts label"))
      (check-equal? (hash-ref r1 'covers) (list "extensions/widget.rkt"))
      (check-equal? (hash-ref r1 'fixtures) (list "make-widget-fixture!"))
      (check-not-false (member "check-equal?" (hash-ref r1 'assertions)))
      (check-not-false (member "check-true" (hash-ref r1 'assertions)))
      (check-not-false (member "make-widget-fixture!" (hash-ref r1 'stimulus)))
      ;; assertion call itself must not leak into the stimulus fingerprint
      (check-false (member "check-equal?" (hash-ref r1 'stimulus))))))

;; ---------- detector: grouping + policy ----------

(define grouping-suite
  (test-suite "detect-redundancy-groups"
    (test-case "same covers + same stimulus + differing assertions -> merge-complementary (rank 2)"
      (define root (make-sample-tree!))
      (define rs (parse-test-file (build-path root "tests" "a.rkt")))
      (define groups (hash-ref (detect-redundancy-groups (list rs)) 'same_covers))
      (check-equal? (length groups) 1)
      (define g (car groups))
      (check-equal? (length (hash-ref g 'members)) 2)
      (check-equal? (hash-ref g 'preference) "merge-complementary")
      (check-equal? (hash-ref g 'requires_adequacy) #f))

    (test-case "same covers + same stimulus + identical assertions -> delete candidate requiring adequacy (rank 4)"
      (define root (make-sample-tree!))
      (define rs (parse-test-file (build-path root "tests" "d.rkt")))
      (define groups (hash-ref (detect-redundancy-groups (list rs)) 'same_covers))
      (check-equal? (length groups) 1)
      (define g (car groups))
      (check-equal? (hash-ref g 'preference) "delete-with-adequacy")
      (check-equal? (hash-ref g 'requires_adequacy) #t))

    (test-case "differing stimulus is a detection-power ambiguity -> keep-both (rank 3), never delete"
      (define root (make-sample-tree!))
      (define rs (parse-test-file (build-path root "tests" "e.rkt")))
      (define groups (hash-ref (detect-redundancy-groups (list rs)) 'same_covers))
      (check-equal? (length groups) 1)
      (define g (car groups))
      (check-equal? (hash-ref g 'preference) "keep-both")
      (check-equal? (hash-ref g 'requires_adequacy) #f))

    (test-case "invariant: a delete recommendation only exists when the stimulus is identical"
      (define root (make-sample-tree!))
      (define a (parse-test-file (build-path root "tests" "a.rkt")))
      (define d (parse-test-file (build-path root "tests" "d.rkt")))
      (define e (parse-test-file (build-path root "tests" "e.rkt")))
      (define groups (hash-ref (detect-redundancy-groups (list a d e)) 'same_covers))
      (for ([g (in-list groups)])
        (when (string=? (hash-ref g 'preference) "delete-with-adequacy")
          (check-true (hash-ref g 'stimulus_equal)))))

    (test-case "shared fixture builders across different covers -> share-setup (rank 1)"
      (define root (make-sample-tree!))
      (define a (parse-test-file (build-path root "tests" "a.rkt")))
      (define c (parse-test-file (build-path root "tests" "c.rkt")))
      (define groups (hash-ref (detect-redundancy-groups (list a c)) 'shared_fixtures))
      (check-equal? (length groups) 1)
      (define g (car groups))
      (check-equal? (hash-ref g 'fixture_key) (list "make-widget-fixture!"))
      (check-equal? (hash-ref g 'preference) "share-setup")
      (check-not-false (member "tests/a.rkt"
                               (map (lambda (m) (hash-ref m 'path)) (hash-ref g 'members))))
      (check-not-false (member "tests/c.rkt"
                               (map (lambda (m) (hash-ref m 'path)) (hash-ref g 'members)))))))

;; ---------- emission ----------

(define emission-suite
  (test-suite "candidates-json"
    (test-case "emission is deterministic and evidence-linked"
      (define root (make-sample-tree!))
      (define a (parse-test-file (build-path root "tests" "a.rkt")))
      (define d (parse-test-file (build-path root "tests" "d.rkt")))
      (define e (parse-test-file (build-path root "tests" "e.rkt")))
      (define js1 (candidates-json (detect-redundancy-groups (list a d e)) "v1.00.28-w5"))
      (define js2 (candidates-json (detect-redundancy-groups (list a d e)) "v1.00.28-w5"))
      (check-equal? js1 js2)
      (check-equal? (hash-ref js1 'schema) "redundancy-candidates/1")
      (check-equal? (hash-ref js1 'package) "v1.00.28-w5")
      (define evidence
        (string-join (for/list ([g (in-list (hash-ref js1 'same_covers_groups))])
                       (hash-ref g 'evidence))
                     " "))
      (check-true (string-contains? evidence "tests/a.rkt#widget asserts label"))
      (check-true (string-contains? evidence "tests/d.rkt#gadget case one")))))

;; ---------- policy ranking ----------

(define policy-suite
  (test-suite "preference-rank"
    (test-case "policy order: share-setup < merge-complementary < keep-both < delete"
      (check-true (< (preference-rank "share-setup")
                     (preference-rank "merge-complementary")
                     (preference-rank "keep-both")
                     (preference-rank "delete-with-adequacy"))))))

;; ---------- adequacy --check (fail-closed) ----------

(define (adequacy-fixture #:behavior-mapping [bm (list (hasheq 'from "t-1" 'to "t-merged"))]
                          #:reviewed [reviewed #t]
                          #:mutants
                          [mutants (list (hasheq 'id "M-1" 'previously "killed" 'after "killed"))]
                          #:suites [suites (hasheq 'fast "green")]
                          #:before [before 2]
                          #:after [after 1])
  (hasheq 'schema
          "test-consolidation-adequacy/1"
          'package
          "v1.00.28-w5"
          'counts
          (hasheq 'before before 'after after)
          'behavior_mapping
          bm
          'failure_history
          (hasheq 'reviewed reviewed)
          'mutation_evidence
          (hasheq 'mode "manual-micro-mutations" 'mutants mutants)
          'suites
          suites))

(define adequacy-suite
  (test-suite "validate-adequacy (mutation-pilot --check)"
    (test-case "complete evidence payload is accepted"
      (define r (validate-adequacy (adequacy-fixture)))
      (check-true (hash-ref r 'ok))
      (check-equal? (hash-ref r 'reasons) '()))

    (test-case "changed count with missing behavior mapping fails closed"
      (define r (validate-adequacy (adequacy-fixture #:behavior-mapping '())))
      (check-false (hash-ref r 'ok))
      (check-true (string-contains? (string-join (hash-ref r 'reasons) "; ") "behavior_mapping")))

    (test-case "unchanged count does not require a behavior mapping"
      (define r (validate-adequacy (adequacy-fixture #:behavior-mapping '() #:before 2 #:after 2)))
      (check-true (hash-ref r 'ok)))

    (test-case "failure history must be reviewed"
      (define r (validate-adequacy (adequacy-fixture #:reviewed #f)))
      (check-false (hash-ref r 'ok)))

    (test-case "a previously killed mutant surviving the consolidated suite is RED"
      (define r
        (validate-adequacy
         (adequacy-fixture #:mutants
                           (list (hasheq 'id "M-1" 'previously "killed" 'after "survived")))))
      (check-false (hash-ref r 'ok))
      (check-true (string-contains? (string-join (hash-ref r 'reasons) "; ") "M-1")))

    (test-case "all mutants still killed after consolidation is accepted"
      (define r
        (validate-adequacy
         (adequacy-fixture #:mutants (list (hasheq 'id "M-1" 'previously "killed" 'after "killed")
                                           (hasheq 'id "M-2" 'previously "killed" 'after "killed")))))
      (check-true (hash-ref r 'ok)))

    (test-case "consolidation with non-green suites fails closed"
      (define r (validate-adequacy (adequacy-fixture #:suites (hasheq 'fast "red"))))
      (check-false (hash-ref r 'ok)))

    (test-case "unknown schema fails closed"
      (define r (validate-adequacy (hash-set (adequacy-fixture) 'schema "bogus/9")))
      (check-false (hash-ref r 'ok)))))

;; ---------- runner ----------

(define redundancy-suite
  (test-suite "W5 redundancy candidates + adequacy check"
    parser-suite
    grouping-suite
    emission-suite
    policy-suite
    adequacy-suite))

(module+ main
  (exit (run-tests redundancy-suite)))
