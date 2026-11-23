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

  (test-case "duplicate or malformed contracts are metadata defects, not coverage"
    (define dup (hasheq 'module
                        "extensions/gsd/wave-status.rkt"
                        'kind
                        'vocabulary
                        'consumers
                        (list "extensions/gsd/campaign-state.rkt")
                        'tests
                        (list "tests/test-gsd-wave-status.rkt")))
    (define d (contract-decision (list "extensions/gsd/wave-status.rkt")
                                (list dup dup)
                                declared-fixture
                                dependents-fixture
                                requires-fixture
                                known-tests))
    (check-equal? (hash-ref d 'verdict) 'escalate)
    (check-true (and (memq 'duplicate-contract
                           (map (lambda (e) (hash-ref e 'code)) (hash-ref d 'escalations)))
                     #t))
    (define bad-kind (hash-set dup 'kind 'assertion))
    (define d2 (contract-decision (list "extensions/gsd/wave-status.rkt")
                                 (list bad-kind)
                                 declared-fixture
                                 dependents-fixture
                                 requires-fixture
                                 known-tests))
    (check-true (and (memq 'invalid-contract
                           (map (lambda (e) (hash-ref e 'code)) (hash-ref d2 'escalations)))
                     #t)))

  (test-case "an escalation can never shrink the required test set"
    ;; Pure invariant: whatever the reason for escalating, the selection
    ;; instruction is the comprehensive set (more tests), never fewer.
    (define comprehensive (list "tests/test-aaa.rkt" "tests/test-zzz.rkt"))
    (define (sel-req d) (hash-ref (contract-selection d #:comprehensive comprehensive) 'tests))
    (define covered (contract-decision (list "extensions/gsd/wave-status.rkt")
                                      (list vocabulary-contract)
                                      declared-fixture
                                      dependents-fixture
                                      requires-fixture
                                      known-tests))
    (check-equal? (hash-ref covered 'verdict) 'covered)
    (check-equal? (hash-ref (contract-selection covered #:comprehensive comprehensive)
                            'instruction)
                  'run-required)
    (check-equal? (sel-req covered) (hash-ref covered 'required))
    (for ([case (in-list (list (hash 'manifest-status 'corrupt)
                               (hash 'graph-failures (list "extensions/gsd/wave-status.rkt"))))])
      (define d (contract-decision (list "extensions/gsd/wave-status.rkt")
                                  (list vocabulary-contract)
                                  declared-fixture
                                  dependents-fixture
                                  requires-fixture
                                  known-tests
                                  #:manifest-status (hash-ref case 'manifest-status 'loaded)
                                  #:graph-failures (hash-ref case 'graph-failures '())))
      (check-equal? (hash-ref d 'verdict) 'escalate)
      (check-equal? (hash-ref (contract-selection d #:comprehensive comprehensive)
                              'instruction)
                    'run-comprehensive)
      ;; T_required subset of T_selected is guaranteed by construction, and the
      ;; emitted list is the union (comprehensive ∪ required), not merely a
      ;; differently-named larger set.
      (check-true (subset? (hash-ref d 'required) (sel-req d)))
      (check-true (subset? comprehensive (sel-req d)))
      (check-equal? (check-selection-instruction
                     (contract-selection d #:comprehensive comprehensive))
                    '())))

  (test-case "a comprehensive set that omits a required test cannot narrow the result"
    (define escalated (contract-decision (list "extensions/gsd/wave-status.rkt")
                                        (list vocabulary-contract)
                                        declared-fixture
                                        dependents-fixture
                                        requires-fixture
                                        known-tests
                                        #:manifest-status 'corrupt))
    ;; A hostile/defective caller passes a comprehensive set missing required
    ;; tests; the bridge unions, so inclusion still holds.
    (define sel (contract-selection escalated #:comprehensive (list "tests/test-other.rkt")))
    (check-equal? (hash-ref sel 'instruction) 'run-comprehensive)
    (check-true (subset? (hash-ref escalated 'required) (hash-ref sel 'tests)))
    (check-true (subset? (list "tests/test-other.rkt") (hash-ref sel 'tests)))
    ;; And an enumeration we could not obtain at all is run-all, never empty.
    (define none (contract-selection escalated #:comprehensive 'unresolved))
    (check-equal? (hash-ref none 'instruction) 'run-all)
    (check-equal? (hash-ref none 'tests) 'unresolved))

  (test-case "an exception or unexpected verdict falls back to run-all, never to fewer tests"
    (define ex (contract-selection/fail-closed (lambda () (error "injected"))))
    (check-equal? (hash-ref ex 'instruction) 'run-all)
    (check-equal? (hash-ref ex 'tests) 'unresolved)
    (check-equal? (check-selection-instruction ex) '())
    (define weird (contract-selection (hasheq 'verdict 'maybe 'required (list "tests/test-a.rkt") 'escalations '())
                                     #:comprehensive (list "tests/test-a.rkt" "tests/test-b.rkt")))
    (check-equal? (hash-ref weird 'instruction) 'run-comprehensive)
    (check-true (subset? (list "tests/test-a.rkt") (hash-ref weird 'tests))))

  (test-case "the instruction checker rejects empty and reduced instructions"
    (check-not-equal? (check-selection-instruction (hasheq 'instruction
                                                           'run-required
                                                           'tests
                                                           '()
                                                           'required
                                                           '()))
                      '())
    (check-not-equal? (check-selection-instruction (hasheq 'instruction
                                                           'run-required
                                                           'tests
                                                           (list "tests/test-a.rkt")
                                                           'required
                                                           (list "tests/test-a.rkt" "tests/test-b.rkt")))
                      '())
    (check-not-equal? (check-selection-instruction (hasheq 'instruction
                                                           'run-comprehensive
                                                           'tests
                                                           (list "tests/test-other.rkt")
                                                           'required
                                                           (list "tests/test-a.rkt")))
                      '())
    (check-not-equal? (check-selection-instruction (hasheq 'instruction
                                                           'invented
                                                           'tests
                                                           (list "tests/test-a.rkt")
                                                           'required
                                                           (list "tests/test-a.rkt")))
                      '()))

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

  (test-case "repository: the D5 selection includes every required test in the comprehensive set"
    (define inputs (repo-inputs (path->complete-path repo-root)))
    (define all (hash-ref inputs 'all-tests))
    (define d (evaluate-contract-changes (path->complete-path repo-root)
                                         (list "extensions/gsd/wave-status.rkt")))
    (define sel (contract-selection d #:comprehensive all))
    (check-equal? (hash-ref sel 'instruction) 'run-comprehensive)
    (check-true (subset? (hash-ref d 'required) (hash-ref sel 'tests)))
    (check-true (subset? all (hash-ref sel 'tests)))
    (check-equal? (check-selection-instruction sel) '())
    ;; and the covered path emits exactly the required set
    (define covered (contract-decision (list "extensions/gsd/wave-status.rkt")
                                      (list vocabulary-contract)
                                      declared-fixture
                                      dependents-fixture
                                      requires-fixture
                                      known-tests))
    (check-equal? (hash-ref (contract-selection covered #:comprehensive all) 'tests)
                  (hash-ref covered 'required)))

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
