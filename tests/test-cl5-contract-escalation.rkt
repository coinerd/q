#lang racket

;; @speed slow  ;; @suite default
;; @boundary unit

;; tests/test-cl5-contract-escalation.rkt — isolated contract-aware escalation
;; prototype (issue #9997).
;;
;;  1. a declared vocabulary contract yields its consumers' verification set
;;  2. a high-fan-in module with no declaration escalates unmapped-contract
;;  3. a declared consumer that no longer requires the module is stale
;;  4. a declared test that does not exist is unknown
;;  5. a declaration that yields no runnable test escalates no-coverage
;;  6. a low-fan-in module with a detector is unaffected

(require rackunit
         racket/list
         racket/runtime-path
         (file "../scripts/cl5/contract-escalation.rkt"))

(define-runtime-path repo-root "..")

(define requires-fixture
  (hash "extensions/gsd/wave-status.rkt"
        '()
        "extensions/gsd/campaign-state.rkt"
        (list "extensions/gsd/wave-status.rkt")
        "extensions/gsd/go-orchestrator.rkt"
        (list "extensions/gsd/campaign-state.rkt")
        "extensions/gsd/orphan.rkt"
        '()))

(define dependents-fixture
  (hash "extensions/gsd/wave-status.rkt"
        (list "extensions/gsd/campaign-state.rkt")
        "extensions/gsd/campaign-state.rkt"
        (list "extensions/gsd/go-orchestrator.rkt")
        "extensions/gsd/hot.rkt"
        (list "a" "b" "c")
        "extensions/gsd/cold.rkt"
        '()))

(define declared-fixture
  (hash "extensions/gsd/campaign-state.rkt"
        (list "tests/test-gsd-campaign-state.rkt")
        "extensions/gsd/cold.rkt"
        (list "tests/test-cold.rkt")))

(define known-tests
  (hash "tests/test-gsd-wave-status.rkt"
        #t
        "tests/test-gsd-campaign-state.rkt"
        #t
        "tests/test-cold.rkt"
        #t))

(define two-dependents
  (hash "extensions/gsd/wave-status.rkt" (list "extensions/gsd/campaign-state.rkt"
                                                "extensions/gsd/go-orchestrator.rkt")))

(define vocabulary-contract
  (hasheq 'module
          "extensions/gsd/wave-status.rkt"
          'kind
          'vocabulary
          'consumers
          (list "extensions/gsd/campaign-state.rkt")
          'tests
          (list "tests/test-gsd-wave-status.rkt")))

(module+ test
  (test-case "a declared vocabulary contract yields its consumers' verification set"
    (define d
      (contract-decision (list "extensions/gsd/wave-status.rkt")
                         (list vocabulary-contract)
                         declared-fixture
                         dependents-fixture
                         requires-fixture
                         known-tests))
    (check-equal? (hash-ref d 'verdict) 'covered)
    (check-equal? (hash-ref d 'escalations) '())
    (check-equal? (hash-ref d 'required)
                  (list "tests/test-gsd-campaign-state.rkt" "tests/test-gsd-wave-status.rkt")))

  (test-case "omitting a graph-known dependent is partial, never covered"
    (define d (contract-decision (list "extensions/gsd/wave-status.rkt")
                                (list vocabulary-contract)
                                declared-fixture
                                two-dependents
                                requires-fixture
                                known-tests))
    ;; The declaration names only campaign-state; go-orchestrator depends on the
    ;; module too and is therefore an untested consumer.
    (check-equal? (hash-ref d 'verdict) 'escalate)
    (define e (for/first ([x (in-list (hash-ref d 'escalations))]
                          #:when (eq? (hash-ref x 'code) 'partial-consumers))
                x))
    (check-not-false e)
    (check-equal? (hash-ref e 'tests) (list "extensions/gsd/go-orchestrator.rkt")))

  (test-case "a consumer detector missing from disk escalates unknown-test"
    ;; campaign-state's declared detector is not in known-tests.
    (define d (contract-decision (list "extensions/gsd/wave-status.rkt")
                                (list vocabulary-contract)
                                declared-fixture
                                dependents-fixture
                                requires-fixture
                                (hash "tests/test-gsd-wave-status.rkt" #t)))
    (check-equal? (hash-ref d 'verdict) 'escalate)
    (check-true (and (memq 'unknown-test
                           (map (lambda (e) (hash-ref e 'code)) (hash-ref d 'escalations)))
                     #t)))

  (test-case "a declared consumer with no detector is not silently ignored"
    (define no-detector (hash "extensions/gsd/undetected.rkt" '()))
    (define contract (hasheq 'module
                             "extensions/gsd/wave-status.rkt"
                             'kind
                             'vocabulary
                             'consumers
                             (list "extensions/gsd/campaign-state.rkt" "extensions/gsd/undetected.rkt")
                             'tests
                             (list "tests/test-gsd-wave-status.rkt")))
    (define d (contract-decision (list "extensions/gsd/wave-status.rkt")
                                (list contract)
                                no-detector
                                (hash "extensions/gsd/wave-status.rkt"
                                      (list "extensions/gsd/campaign-state.rkt"
                                            "extensions/gsd/undetected.rkt"))
                                requires-fixture
                                (hash "tests/test-gsd-wave-status.rkt" #t)))
    (check-equal? (hash-ref d 'verdict) 'escalate)
    (check-true (and (memq 'no-consumer-detector
                           (map (lambda (e) (hash-ref e 'code)) (hash-ref d 'escalations)))
                     #t)))

  (test-case "an unparseable contract or consumer makes the graph evidence unavailable"
    (define d (contract-decision (list "extensions/gsd/wave-status.rkt")
                                (list vocabulary-contract)
                                declared-fixture
                                dependents-fixture
                                requires-fixture
                                known-tests
                                #:graph-failures (list "extensions/gsd/wave-status.rkt")))
    (check-equal? (hash-ref d 'verdict) 'escalate)
    (check-true (and (memq 'graph-unavailable
                           (map (lambda (e) (hash-ref e 'code)) (hash-ref d 'escalations)))
                     #t)))

  (test-case "an empty required set is never coverage"
    (define d (contract-decision (list "extensions/gsd/cold.rkt")
                                '()
                                (hash)
                                dependents-fixture
                                requires-fixture
                                (hash)))
    (check-equal? (hash-ref d 'verdict) 'escalate)
    (check-true (and (memq 'no-required-tests
                           (map (lambda (e) (hash-ref e 'code)) (hash-ref d 'escalations)))
                     #t)))

  (test-case "a missing or corrupt manifest blocks a complete-coverage claim"
    (for ([status (in-list '(missing corrupt))])
      (define d (contract-decision (list "extensions/gsd/wave-status.rkt")
                                  (list vocabulary-contract)
                                  declared-fixture
                                  dependents-fixture
                                  requires-fixture
                                  known-tests
                                  #:manifest-status status))
      (check-equal? (hash-ref d 'verdict) 'escalate)
      (check-true (and (memq 'attribution-unavailable
                             (map (lambda (e) (hash-ref e 'code)) (hash-ref d 'escalations)))
                       #t))))

  (test-case "a high-fan-in module without a declaration escalates unmapped-contract"
    (define d
      (contract-decision (list "extensions/gsd/hot.rkt")
                         (list vocabulary-contract)
                         declared-fixture
                         dependents-fixture
                         requires-fixture
                         known-tests))
    (check-equal? (hash-ref d 'verdict) 'escalate)
    (define codes (map (lambda (e) (hash-ref e 'code)) (hash-ref d 'escalations)))
    (check-true (and (memq 'unmapped-contract codes) #t))
    (check-equal? (hash-ref d 'unmatched-high-fan-in) (list "extensions/gsd/hot.rkt")))

  (test-case "a declared consumer that no longer requires the module is stale"
    (define stale
      (hasheq 'module
              "extensions/gsd/wave-status.rkt"
              'kind
              'vocabulary
              'consumers
              (list "extensions/gsd/orphan.rkt")
              'tests
              (list "tests/test-gsd-wave-status.rkt")))
    (define d
      (contract-decision (list "extensions/gsd/wave-status.rkt")
                         (list stale)
                         declared-fixture
                         dependents-fixture
                         requires-fixture
                         known-tests))
    (check-equal? (hash-ref d 'verdict) 'escalate)
    (check-true
     (and (memq 'stale-consumer (map (lambda (e) (hash-ref e 'code)) (hash-ref d 'escalations))) #t)))

  (test-case "a declared test that does not exist is unknown, not ignored"
    (define broken
      (hasheq 'module
              "extensions/gsd/wave-status.rkt"
              'kind
              'vocabulary
              'consumers
              (list "extensions/gsd/campaign-state.rkt")
              'tests
              (list "tests/test-does-not-exist.rkt")))
    (define d
      (contract-decision (list "extensions/gsd/wave-status.rkt")
                         (list broken)
                         declared-fixture
                         dependents-fixture
                         requires-fixture
                         known-tests))
    (check-equal? (hash-ref d 'verdict) 'escalate)
    (check-true
     (and (memq 'unknown-test (map (lambda (e) (hash-ref e 'code)) (hash-ref d 'escalations))) #t)))

  (test-case "a declaration with no runnable test escalates no-coverage"
    (define empty-contract
      (hasheq 'module "extensions/gsd/wave-status.rkt" 'kind 'vocabulary 'consumers '() 'tests '()))
    (define d
      (contract-decision (list "extensions/gsd/wave-status.rkt")
                         (list empty-contract)
                         declared-fixture
                         dependents-fixture
                         requires-fixture
                         known-tests))
    (check-equal? (hash-ref d 'verdict) 'escalate)
    (check-true
     (and (memq 'no-coverage (map (lambda (e) (hash-ref e 'code)) (hash-ref d 'escalations))) #t)))

  (test-case "repository: the D5 contract computes its set and flags the undetected consumer"
    (define d (evaluate-contract-changes (path->complete-path repo-root)
                                         (list "extensions/gsd/wave-status.rkt")))
    ;; The set is computed (owner test + consumer detectors), but one declared
    ;; consumer (projection-kernel.rkt) has no @covers detector, so the contract
    ;; escalates instead of claiming complete coverage.
    (check-true (and (member "tests/test-gsd-wave-status.rkt" (hash-ref d 'required)) #t))
    (check-true (> (length (hash-ref d 'required)) 1))
    (check-equal? (hash-ref d 'verdict) 'escalate)
    (check-true (and (memq 'no-consumer-detector
                           (map (lambda (e) (hash-ref e 'code)) (hash-ref d 'escalations)))
                     #t)))

  (test-case "repository: a partially declared contract escalates instead of claiming cover"
    ;; util/error/errors.rkt has 44 graph-known dependents; the seed names two.
    (define d (evaluate-contract-changes (path->complete-path repo-root)
                                         (list "util/error/errors.rkt")))
    (check-equal? (hash-ref d 'verdict) 'escalate)
    (check-true (and (memq 'partial-consumers
                           (map (lambda (e) (hash-ref e 'code)) (hash-ref d 'escalations)))
                     #t)))

  (test-case "a change no contract covers reports no-required-tests, not covered"
    (define d
      (contract-decision (list "extensions/gsd/cold.rkt")
                         (list vocabulary-contract)
                         declared-fixture
                         dependents-fixture
                         requires-fixture
                         known-tests))
    (check-equal? (hash-ref d 'verdict) 'escalate)
    (check-true (and (memq 'no-required-tests
                           (map (lambda (e) (hash-ref e 'code)) (hash-ref d 'escalations)))
                     #t))))
