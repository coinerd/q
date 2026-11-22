#lang racket

;; @speed slow  ;; @suite default
;; @boundary unit

;; tests/test-cl5-ownership-audit.rkt — offline coverage-gap audit and the
;; ISOLATED fail-closed selection guard (CL5 W2 follow-up, #9982).
;;
;; These tests never touch the production selector, never execute the test
;; suite under audit, and need no GitHub Actions:
;;   1. classification is exactly the declared rule (confirmed / potential /
;;      covered / unverified)
;;   2. risk ranking is deterministic and ordered
;;   3. covers inversion is module -> tests
;;   4. guard: the frozen D5 change escalates and NAMES the unadmitted detector
;;   5. guard: a clean control with an audited detector is selected
;;   6. guard: an uncovered module escalates; a zero selection can never be green
;;   7. guard: empty ownership evidence escalates (never silently under-selects)
;;   8. repository-level negative control: extensions/gsd/wave-status.rkt is a
;;      confirmed gap whose covering tests include tests/test-gsd-wave-status.rkt
;;   9. repository-level positive control: a module with an audited detector is
;;      selected, not escalated

(require rackunit
         racket/list
         racket/runtime-path
         racket/string
         (file "../scripts/cl5/ownership-audit.rkt")
         (file "../scripts/cl5/selection-guard.rkt"))

(define-runtime-path repo-root "..")

(define owners (list "tests/test-gsd-campaign-decision-pure.rkt" "tests/test-agent-state.rkt"))

(define declared-fixture
  (hash "extensions/gsd/wave-status.rkt"
        (list "tests/test-gsd-wave-status.rkt" "tests/test-audit-v09945-w3-gsd.rkt")
        "extensions/gsd/campaign-decision.rkt"
        (list "tests/test-gsd-campaign-decision-pure.rkt")
        "runtime/new-module.rkt"
        '()))

(define dependents-fixture
  (hash "extensions/gsd/wave-status.rkt"
        (list "extensions/gsd/campaign-decision.rkt")
        "extensions/gsd/campaign-decision.rkt"
        '()
        "runtime/new-module.rkt"
        '()))

(module+ test
  (test-case "module classification follows the declared rule"
    (check-equal? (classify-module "m" '() owners 0) 'unverified)
    (check-equal? (classify-module "m" (list "tests/unaudited.rkt") owners 0) 'confirmed-gap)
    (check-equal? (classify-module "m" (list "tests/test-agent-state.rkt") owners 0) 'covered)
    (check-equal? (classify-module "m" (list "tests/test-agent-state.rkt") owners 3) 'potential-gap)
    (check-equal? (classify-module "m" (list "tests/test-agent-state.rkt") owners 2) 'covered))

  (test-case "risk categories are deterministic and safety-first"
    (check-equal? (risk-category "runtime/credentials/protocol.rkt") 'authorization)
    (check-equal? (risk-category "sandbox/subprocess.rkt") 'security)
    (check-equal? (risk-category "runtime/session/session-store.rkt") 'persistence)
    (check-equal? (risk-category "extensions/gsd/delivery-recovery.rkt") 'recovery)
    (check-equal? (risk-category "llm/provider-openai.rkt") 'provider)
    (check-equal? (risk-category "agent/event-bus.rkt") 'ordering)
    (check-equal? (risk-category "agent/blackboard.rkt") 'shared-state)
    (check-equal? (risk-category "util/strings.rkt") #f)
    (check-equal? (risk-rank "runtime/credentials/protocol.rkt") 6)
    (check-equal? (risk-rank "util/strings.rkt") 1))

  (test-case "covers inversion is module -> declared tests"
    (define inverted
      (invert-covers (hash "tests/b.rkt" (list "x.rkt" "y.rkt") "tests/a.rkt" (list "x.rkt"))))
    (check-equal? (hash-ref inverted "x.rkt") (list "tests/a.rkt" "tests/b.rkt"))
    (check-equal? (hash-ref inverted "y.rkt") (list "tests/b.rkt"))
    (check-equal? (hash-ref inverted "z.rkt" '()) '()))

  (test-case "the frozen D5 change escalates and names the unadmitted detector"
    (define d
      (guard-decision (list "extensions/gsd/wave-status.rkt")
                      declared-fixture
                      owners
                      dependents-fixture))
    (check-equal? (hash-ref d 'verdict) 'escalate)
    (check-equal? (hash-ref d 'lane) 'comprehensive)
    (define codes (map (lambda (e) (hash-ref e 'code)) (hash-ref d 'escalations)))
    (check-true (and (memq 'unadmitted-cover codes) #t))
    (define e
      (for/first ([x (in-list (hash-ref d 'escalations))]
                  #:when (eq? (hash-ref x 'code) 'unadmitted-cover))
        x))
    (check-equal? (hash-ref e 'module) "extensions/gsd/wave-status.rkt")
    (check-true (and (member "tests/test-gsd-wave-status.rkt" (hash-ref e 'tests)) #t)))

  (test-case "a clean control with an audited detector is selected, not escalated"
    (define d
      (guard-decision (list "extensions/gsd/campaign-decision.rkt")
                      declared-fixture
                      owners
                      dependents-fixture))
    (check-equal? (hash-ref d 'verdict) 'selected)
    (check-equal? (hash-ref d 'lane) 'quick)
    (check-equal? (hash-ref d 'selected) (list "tests/test-gsd-campaign-decision-pure.rkt"))
    (check-equal? (hash-ref d 'escalations) '()))

  (test-case "an uncovered module escalates and a zero selection is never green"
    (define d
      (guard-decision (list "runtime/new-module.rkt") declared-fixture owners dependents-fixture))
    (check-equal? (hash-ref d 'verdict) 'escalate)
    (define codes (map (lambda (e) (hash-ref e 'code)) (hash-ref d 'escalations)))
    (check-true (and (memq 'uncovered-module codes) #t))
    (check-equal? (hash-ref d 'selected) '()))
  (test-case "an empty change selects nothing and escalates instead of a zero-test green"
    (define d (guard-decision '() declared-fixture owners dependents-fixture))
    (check-equal? (hash-ref d 'verdict) 'escalate)
    (check-true (and (memq 'zero-selection
                           (map (lambda (e) (hash-ref e 'code)) (hash-ref d 'escalations)))
                     #t)))

  (test-case "empty ownership evidence escalates instead of under-selecting"
    (define d
      (guard-decision (list "extensions/gsd/campaign-decision.rkt")
                      declared-fixture
                      '()
                      dependents-fixture))
    (check-equal? (hash-ref d 'verdict) 'escalate)
    (define codes (map (lambda (e) (hash-ref e 'code)) (hash-ref d 'escalations)))
    (check-true (and (memq 'admission-stale codes) #t))
    (check-true (and (memq 'unadmitted-cover codes) #t)))

  (test-case "repository negative control: wave-status is a confirmed gap"
    (define snapshot (audit-snapshot (path->complete-path repo-root)))
    (define row
      (for/first ([r (in-list (append (hash-ref snapshot 'confirmed-gaps)
                                      (hash-ref snapshot 'potential-gaps)))]
                  #:when (equal? (hash-ref r 'module) "extensions/gsd/wave-status.rkt"))
        r))
    (check-not-false row)
    (check-equal? (hash-ref row 'class) 'confirmed-gap)
    (check-true (and (member "tests/test-gsd-wave-status.rkt" (hash-ref row 'unadmitted-tests)) #t))
    (check-equal? (hash-ref row 'audited-tests) '()))

  (test-case "repository positive control: an audited module is covered, not a gap"
    (define snapshot (audit-snapshot (path->complete-path repo-root)))
    (define row
      (for/first ([r (in-list (hash-ref snapshot 'rows))]
                  #:when (equal? (hash-ref r 'module) "extensions/gsd/campaign-decision.rkt"))
        r))
    (check-not-false row)
    (check-not-eq? (hash-ref row 'class) 'confirmed-gap)
    (check-true (and (member "tests/test-gsd-campaign-decision-pure.rkt"
                             (hash-ref row 'audited-tests))
                     #t)))

  (test-case "repository guard escalates the frozen D5 changed file"
    (define result (guard-decide-repo (path->complete-path repo-root) (list "extensions/gsd/wave-status.rkt")))
    (check-equal? (hash-ref (hash-ref result 'decision) 'verdict) 'escalate)
    (define e
      (for/first ([x (in-list (hash-ref (hash-ref result 'decision) 'escalations))]
                  #:when (eq? (hash-ref x 'code) 'unadmitted-cover))
        x))
    (check-not-false e)
    (check-true (and (member "tests/test-gsd-wave-status.rkt" (hash-ref e 'tests)) #t))))
