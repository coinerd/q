#lang racket

;; @speed slow  ;; @suite default
;; @boundary unit

;; tests/test-cl5-ownership-audit.rkt — offline coverage-gap audit and the
;; ISOLATED fail-closed selection guard (CL5 W2 follow-up, #9982).
;;
;; No production selector, no GitHub Actions, and the audited tests are never
;; executed. Coverage:
;;   1. module classification over the full production universe (including the
;;      unverified case the first audit could not produce)
;;   2. risk ranking is deterministic and ordered
;;   3. covers inversion is module -> tests
;;   4. guard: D5 change escalates and NAMES the unadmitted detector
;;   5. guard: clean control (audited + fresh + shipped) is selected
;;   6. guard: uncovered module and empty selection escalate
;;   7. guard: STALE admission (owner present, record not current) escalates
;;   8. guard: missing/corrupt manifest escalates
;;   9. guard: graph parse failure in the changed cone escalates
;;  10. guard: declared detector that the shipped selection did not run escalates
;;  11. repository controls: full denominator, unverified > 0, D5 gap row

(require rackunit
         racket/list
         racket/runtime-path
         (file "../scripts/cl5/ownership-audit.rkt")
         (file "../scripts/cl5/selection-guard.rkt"))

(define-runtime-path repo-root "..")

(define owners (list "tests/test-gsd-campaign-decision-pure.rkt" "tests/test-agent-state.rkt"))

(define fresh-admission
  (hash "tests/test-gsd-campaign-decision-pure.rkt" 'admitted "tests/test-agent-state.rkt" 'admitted))

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

(define (evidence #:admission [admission fresh-admission]
                  #:manifest [manifest 'loaded]
                  #:failures [failures '()]
                  #:shipped [shipped '("tests/test-gsd-campaign-decision-pure.rkt")]
                  #:shipped-status [shipped-status 'ok]
                  #:shipped-escalations [shipped-escalations '()]
                  #:shipped-error [shipped-error #f])
  (hash 'declared
        declared-fixture
        'owners
        owners
        'admission
        admission
        'dependents
        dependents-fixture
        'requires
        (hash)
        'manifest-status
        manifest
        'graph-parse-failures
        failures
        'shipped-status
        shipped-status
        'shipped-escalations
        shipped-escalations
        'shipped-error
        shipped-error
        'shipped-selected
        shipped))

(module+ test
  (test-case "module classification covers the full production universe"
    (check-equal? (classify-module "m" '() owners 0) 'unverified)
    (check-equal? (classify-module "m" (list "tests/unaudited.rkt") owners 0) 'confirmed-gap)
    (check-equal? (classify-module "m" (list "tests/test-agent-state.rkt") owners 0) 'covered)
    (check-equal? (classify-module "m" (list "tests/test-agent-state.rkt") owners 3) 'potential-gap)
    (check-equal? (classify-module "m" (list "tests/test-agent-state.rkt") owners 2) 'covered)
    (check-not-eq? (classify-module "m" '() owners 10) 'covered))

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
    (define d (guard-decision (list "extensions/gsd/wave-status.rkt") (evidence)))
    (check-equal? (hash-ref d 'verdict) 'escalate)
    (check-equal? (hash-ref d 'lane) 'comprehensive)
    (check-equal? (hash-ref d 'claims) 'attribution-and-admission-only)
    (define e
      (for/first ([x (in-list (hash-ref d 'escalations))]
                  #:when (eq? (hash-ref x 'code) 'unadmitted-cover))
        x))
    (check-not-false e)
    (check-equal? (hash-ref e 'module) "extensions/gsd/wave-status.rkt")
    (check-true (and (member "tests/test-gsd-wave-status.rkt"
                             (map (lambda (t) (hash-ref t 'test)) (hash-ref e 'tests)))
                     #t)))

  (test-case "a clean control with audited, fresh, shipped evidence is selected"
    (define d (guard-decision (list "extensions/gsd/campaign-decision.rkt") (evidence)))
    (check-equal? (hash-ref d 'verdict) 'selected)
    (check-equal? (hash-ref d 'lane) 'quick)
    (check-equal? (hash-ref d 'selected) (list "tests/test-gsd-campaign-decision-pure.rkt"))
    (check-equal? (hash-ref d 'escalations) '()))

  (test-case "an uncovered module escalates and an empty change never reports green"
    (define d (guard-decision (list "runtime/new-module.rkt") (evidence)))
    (check-equal? (hash-ref d 'verdict) 'escalate)
    (check-true (and (memq 'uncovered-module
                           (map (lambda (e) (hash-ref e 'code)) (hash-ref d 'escalations)))
                     #t))
    (check-equal? (hash-ref d 'selected) '())
    (define empty-d (guard-decision '() (evidence)))
    (check-equal? (hash-ref empty-d 'verdict) 'escalate)
    (check-true (and (memq 'zero-selection
                           (map (lambda (e) (hash-ref e 'code)) (hash-ref empty-d 'escalations)))
                     #t)))

  (test-case "stale per-owner admission blocks a green even though the table is nonempty"
    (define stale
      (hash "tests/test-gsd-campaign-decision-pure.rkt"
            'review-required
            "tests/test-agent-state.rkt"
            'admitted))
    (define d
      (guard-decision (list "extensions/gsd/campaign-decision.rkt") (evidence #:admission stale)))
    (check-equal? (hash-ref d 'verdict) 'escalate)
    (check-true (and (memq 'unadmitted-cover
                           (map (lambda (e) (hash-ref e 'code)) (hash-ref d 'escalations)))
                     #t))
    (check-equal? (hash-ref d 'selected) '())
    (define d2
      (guard-decision (list "extensions/gsd/campaign-decision.rkt") (evidence #:admission (hash))))
    (check-equal? (hash-ref d2 'verdict) 'escalate))

  (test-case "a missing or corrupt manifest blocks every green"
    (for ([status (in-list '(missing corrupt))])
      (define d
        (guard-decision (list "extensions/gsd/campaign-decision.rkt") (evidence #:manifest status)))
      (check-equal? (hash-ref d 'verdict) 'escalate)
      (check-true (and (memq 'manifest-unavailable
                             (map (lambda (e) (hash-ref e 'code)) (hash-ref d 'escalations)))
                       #t))))

  (test-case "a graph parse failure in the changed cone blocks a green"
    (define d
      (guard-decision (list "extensions/gsd/wave-status.rkt")
                      (evidence #:failures (list "extensions/gsd/wave-status.rkt"))))
    (check-equal? (hash-ref d 'verdict) 'escalate)
    (check-true (and (memq 'graph-parse-failure
                           (map (lambda (e) (hash-ref e 'code)) (hash-ref d 'escalations)))
                     #t))
    (define ok
      (guard-decision (list "extensions/gsd/campaign-decision.rkt")
                      (evidence #:failures (list "unrelated/broken.rkt"))))
    (check-equal? (hash-ref ok 'verdict) 'selected))

  (test-case "attribution the shipped selector did not run is not evidence"
    (define d (guard-decision (list "extensions/gsd/campaign-decision.rkt") (evidence #:shipped '())))
    (check-equal? (hash-ref d 'verdict) 'escalate)
    (check-true (and (memq 'unselected-declared-cover
                           (map (lambda (e) (hash-ref e 'code)) (hash-ref d 'escalations)))
                     #t)))

  (test-case "an unavailable shipped selection can never yield a green"
    (define d (guard-decision (list "extensions/gsd/campaign-decision.rkt")
                              (evidence #:shipped #f
                                        #:shipped-status 'failed
                                        #:shipped-error "compute-impact-selection: graph failure")))
    (check-equal? (hash-ref d 'verdict) 'escalate)
    (check-equal? (hash-ref d 'lane) 'comprehensive)
    (define e (for/first ([x (in-list (hash-ref d 'escalations))]
                          #:when (eq? (hash-ref x 'code) 'selection-unavailable))
                x))
    (check-not-false e)
    (check-true (string-contains? (hash-ref e 'detail) "graph failure"))
    (define implicit
      (guard-decision (list "extensions/gsd/campaign-decision.rkt")
                      (hash-remove (evidence) 'shipped-status)))
    (check-equal? (hash-ref implicit 'verdict) 'escalate))

  (test-case "successful but empty shipped evidence is contradictory, not green"
    (define d (guard-decision (list "extensions/gsd/campaign-decision.rkt")
                              (evidence #:shipped #f #:shipped-status 'ok)))
    (check-equal? (hash-ref d 'verdict) 'escalate)
    (check-true (and (memq 'shipped-evidence-inconsistent
                           (map (lambda (e) (hash-ref e 'code)) (hash-ref d 'escalations)))
                     #t)))

  (test-case "an existing shipped escalation is preserved, never downgraded"
    (define d
      (guard-decision (list "extensions/gsd/campaign-decision.rkt")
                      (evidence #:shipped-escalations
                                (list (hash 'code
                                            'source-escalation-scan
                                            'changed-file
                                            "extensions/gsd/campaign-decision.rkt")))))
    (check-equal? (hash-ref d 'verdict) 'escalate)
    (check-equal? (hash-ref d 'lane) 'comprehensive)
    (define e (for/first ([x (in-list (hash-ref d 'escalations))]
                          #:when (eq? (hash-ref x 'code) 'shipped-escalation-preserved))
                x))
    (check-not-false e)
    (check-true (and (memq 'source-escalation-scan (hash-ref e 'tests)) #t))
    (check-equal? (hash-ref d 'selected) (list "tests/test-gsd-campaign-decision-pure.rkt")))

  (test-case "repository controls: full denominator, gap row and covered row"
    (define snapshot (audit-snapshot (path->complete-path repo-root)))
    (check-true (> (hash-ref snapshot 'production-modules) 700))
    (check-equal? (hash-ref snapshot 'declared-covered-modules)
                  (length (filter (lambda (r) (pair? (hash-ref r 'declared-tests)))
                                  (hash-ref snapshot 'rows))))
    (define unverified (hash-ref snapshot 'unverified '()))
    (check-true (> (length unverified) 0))
    (check-equal? (+ (length unverified) (hash-ref snapshot 'declared-covered-modules))
                  (hash-ref snapshot 'production-modules))
    (define row
      (for/first ([r (in-list (hash-ref snapshot 'rows))]
                  #:when (equal? (hash-ref r 'module) "extensions/gsd/wave-status.rkt"))
        r))
    (check-not-false row)
    (check-equal? (hash-ref row 'class) 'confirmed-gap)
    (check-true (and (member "tests/test-gsd-wave-status.rkt" (hash-ref row 'unadmitted-tests)) #t))
    (check-equal? (hash-ref row 'audited-tests) '())
    (define covered-row
      (for/first ([r (in-list (hash-ref snapshot 'rows))]
                  #:when (equal? (hash-ref r 'module) "extensions/gsd/campaign-decision.rkt"))
        r))
    (check-not-false covered-row)
    (check-not-eq? (hash-ref covered-row 'class) 'confirmed-gap)))
