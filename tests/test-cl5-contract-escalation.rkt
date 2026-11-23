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

  (test-case "repository: the D5 vocabulary contract names its consumer verification set"
    (define d (evaluate-contract-changes (path->complete-path repo-root)
                                         (list "extensions/gsd/wave-status.rkt")))
    (check-equal? (hash-ref d 'verdict) 'covered)
    (check-equal? (hash-ref d 'escalations) '())
    (check-true (and (member "tests/test-gsd-wave-status.rkt" (hash-ref d 'required)) #t))
    ;; the consumer set is real: at least one consumer detector is required
    (check-true (> (length (hash-ref d 'required)) 1)))

  (test-case "a low-fan-in module with a detector is unaffected"
    (define d
      (contract-decision (list "extensions/gsd/cold.rkt")
                         (list vocabulary-contract)
                         declared-fixture
                         dependents-fixture
                         requires-fixture
                         known-tests))
    (check-equal? (hash-ref d 'verdict) 'covered)
    (check-equal? (hash-ref d 'escalations) '())))
