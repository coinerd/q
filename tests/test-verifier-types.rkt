#lang racket

;; tests/test-verifier-types.rkt — W1 v0.99.5 Verifier Decision Type Tests
;;
;; Tests the verifier-decision struct, JSON round-trip, and convenience
;; constructors.

;; @speed fast
(require rackunit
         rackunit/text-ui
         json
         "../agent/verification/verifier-types.rkt")

(define suite
  (test-suite "Verifier Decision Types (W1 v0.99.5)"

    ;; ── Constants ──

    (test-case "VERIFIER-VERDICTS has correct values"
      (check-equal? VERIFIER-VERDICTS '(approve reject escalate)))

    (test-case "VERIFIER-RISK-LEVELS has correct values"
      (check-equal? VERIFIER-RISK-LEVELS '(low medium high)))

    ;; ── Predicates ──

    (test-case "verifier-verdict? accepts valid verdicts"
      (check-true (verifier-verdict? 'approve))
      (check-true (verifier-verdict? 'reject))
      (check-true (verifier-verdict? 'escalate)))

    (test-case "verifier-verdict? rejects invalid values"
      (check-false (verifier-verdict? 'invalid))
      (check-false (verifier-verdict? "approve"))
      (check-false (verifier-verdict? 123)))

    (test-case "verifier-risk-level? accepts valid levels"
      (check-true (verifier-risk-level? 'low))
      (check-true (verifier-risk-level? 'medium))
      (check-true (verifier-risk-level? 'high)))

    (test-case "verifier-risk-level? rejects invalid values"
      (check-false (verifier-risk-level? 'critical))
      (check-false (verifier-risk-level? "high"))
      (check-false (verifier-risk-level? #f)))

    ;; ── Struct Construction & Field Access ──

    (test-case "verifier-decision constructs with all fields"
      (define vd (verifier-decision 'approve "looks good" 'low #f '("file.rkt") 1234567890))
      (check-equal? (verifier-decision-verdict vd) 'approve)
      (check-equal? (verifier-decision-reason vd) "looks good")
      (check-equal? (verifier-decision-risk-level vd) 'low)
      (check-false (verifier-decision-requires-human? vd))
      (check-equal? (verifier-decision-artifact-refs vd) '("file.rkt"))
      (check-equal? (verifier-decision-timestamp vd) 1234567890))

    (test-case "verifier-decision accepts #f timestamp"
      (define vd (verifier-decision 'reject "bad" 'high #f '() #f))
      (check-false (verifier-decision-timestamp vd)))

    (test-case "verifier-decision? recognizes decisions"
      (define vd (verifier-decision 'approve "" 'low #t '() #f))
      (check-true (verifier-decision? vd)))

    (test-case "verifier-decision? rejects non-decisions"
      (check-false (verifier-decision? "not a decision"))
      (check-false (verifier-decision? 42))
      (check-false (verifier-decision? (hash))))

    ;; ── JSON Round-Trip ──

    (test-case "verifier-decision->json produces correct hash"
      (define vd (verifier-decision 'approve "ok" 'low #f '("a.rkt" "b.rkt") 1000))
      (define j (verifier-decision->json vd))
      (check-equal? (hash-ref j 'verdict) "approve")
      (check-equal? (hash-ref j 'reason) "ok")
      (check-equal? (hash-ref j 'risk_level) "low")
      (check-false (hash-ref j 'requires_human))
      (check-equal? (hash-ref j 'artifact_refs) '("a.rkt" "b.rkt"))
      (check-equal? (hash-ref j 'timestamp) 1000))

    (test-case "JSON round-trip: approve decision"
      (define vd (verifier-decision 'approve "all good" 'low #f '() 5000))
      (define j (verifier-decision->json vd))
      (define vd2 (json->verifier-decision j))
      (check-true (verifier-decision? vd2))
      (check-equal? (verifier-decision-verdict vd2) 'approve)
      (check-equal? (verifier-decision-reason vd2) "all good")
      (check-equal? (verifier-decision-risk-level vd2) 'low)
      (check-false (verifier-decision-requires-human? vd2))
      (check-equal? (verifier-decision-artifact-refs vd2) '())
      (check-equal? (verifier-decision-timestamp vd2) 5000))

    (test-case "JSON round-trip: reject with escalate verdict"
      (define vd (verifier-decision 'escalate "needs review" 'high #t '("x.rkt") #f))
      (define j (verifier-decision->json vd))
      (define vd2 (json->verifier-decision j))
      (check-true (verifier-decision? vd2))
      (check-equal? (verifier-decision-verdict vd2) 'escalate)
      (check-true (verifier-decision-requires-human? vd2))
      (check-false (verifier-decision-timestamp vd2)))

    (test-case "json->verifier-decision returns #f on non-hash"
      (check-false (json->verifier-decision "string"))
      (check-false (json->verifier-decision 42))
      (check-false (json->verifier-decision #f))
      (check-false (json->verifier-decision '())))

    (test-case "json->verifier-decision returns #f on missing fields"
      (check-false (json->verifier-decision (hasheq 'verdict "approve")))
      (check-false (json->verifier-decision (hasheq 'reason "test"))))

    (test-case "json->verifier-decision returns #f on invalid verdict"
      (define bad-j
        (hasheq 'verdict
                "invalid"
                'reason
                "x"
                'risk_level
                "low"
                'requires_human
                #f
                'artifact_refs
                '()
                'timestamp
                #f))
      (check-false (json->verifier-decision bad-j)))

    (test-case "json->verifier-decision returns #f on invalid risk-level"
      (define bad-j
        (hasheq 'verdict
                "approve"
                'reason
                "x"
                'risk_level
                "critical"
                'requires_human
                #f
                'artifact_refs
                '()
                'timestamp
                #f))
      (check-false (json->verifier-decision bad-j)))

    (test-case "json->verifier-decision returns #f on non-string reason"
      (define bad-j
        (hasheq 'verdict
                "approve"
                'reason
                123
                'risk_level
                "low"
                'requires_human
                #f
                'artifact_refs
                '()
                'timestamp
                #f))
      (check-false (json->verifier-decision bad-j)))

    (test-case "json->verifier-decision returns #f on non-list artifact-refs"
      (define bad-j
        (hasheq 'verdict
                "approve"
                'reason
                "x"
                'risk_level
                "low"
                'requires_human
                #f
                'artifact_refs
                "not-a-list"
                'timestamp
                #f))
      (check-false (json->verifier-decision bad-j)))

    (test-case "json->verifier-decision accepts #f timestamp"
      (define good-j
        (hasheq 'verdict
                "approve"
                'reason
                "ok"
                'risk_level
                "low"
                'requires_human
                #f
                'artifact_refs
                '()
                'timestamp
                #f))
      (define vd (json->verifier-decision good-j))
      (check-true (verifier-decision? vd))
      (check-false (verifier-decision-timestamp vd)))

    (test-case "JSON string round-trip via jsexpr functions"
      (define vd (verifier-decision 'reject "fail" 'medium #t '("a.rkt") 9999))
      (define jstr (jsexpr->string (verifier-decision->json vd)))
      (define parsed (string->jsexpr jstr))
      (define vd2 (json->verifier-decision parsed))
      (check-true (verifier-decision? vd2))
      (check-equal? (verifier-decision-verdict vd2) 'reject)
      (check-equal? (verifier-decision-reason vd2) "fail")
      (check-equal? (verifier-decision-risk-level vd2) 'medium)
      (check-true (verifier-decision-requires-human? vd2)))

    ;; ── Convenience Constructors ──

    (test-case "make-approve-decision creates correct verdict"
      (define vd (make-approve-decision "good code"))
      (check-equal? (verifier-decision-verdict vd) 'approve)
      (check-equal? (verifier-decision-reason vd) "good code")
      (check-equal? (verifier-decision-risk-level vd) 'low)
      (check-false (verifier-decision-requires-human? vd))
      (check-equal? (verifier-decision-artifact-refs vd) '()))

    (test-case "make-reject-decision creates correct verdict"
      (define vd (make-reject-decision "broken code" #:risk-level 'high))
      (check-equal? (verifier-decision-verdict vd) 'reject)
      (check-equal? (verifier-decision-reason vd) "broken code")
      (check-equal? (verifier-decision-risk-level vd) 'high)
      (check-false (verifier-decision-requires-human? vd)))

    (test-case "make-escalate-decision creates correct verdict with HITL"
      (define vd (make-escalate-decision "uncertain" #:artifact-refs '("main.rkt")))
      (check-equal? (verifier-decision-verdict vd) 'escalate)
      (check-equal? (verifier-decision-reason vd) "uncertain")
      (check-equal? (verifier-decision-risk-level vd) 'high)
      (check-true (verifier-decision-requires-human? vd))
      (check-equal? (verifier-decision-artifact-refs vd) '("main.rkt")))

    (test-case "make-approve-decision accepts custom risk and refs"
      (define vd
        (make-approve-decision "ok"
                               #:risk-level 'medium
                               #:artifact-refs '("a.rkt" "b.rkt")
                               #:timestamp 12345))
      (check-equal? (verifier-decision-risk-level vd) 'medium)
      (check-equal? (verifier-decision-artifact-refs vd) '("a.rkt" "b.rkt"))
      (check-equal? (verifier-decision-timestamp vd) 12345))

    (test-case "make-reject-decision defaults to high risk"
      (define vd (make-reject-decision "bad"))
      (check-equal? (verifier-decision-risk-level vd) 'high))

    (test-case "make-escalate-decision defaults to high risk and HITL"
      (define vd (make-escalate-decision "needs human"))
      (check-equal? (verifier-decision-risk-level vd) 'high)
      (check-true (verifier-decision-requires-human? vd)))

    ;; ── Contract Violations ──

    (test-case "verifier-decision rejects invalid verdict via contract"
      (check-exn exn:fail:contract? (lambda () (verifier-decision 'invalid "x" 'low #f '() #f))))

    (test-case "make-approve-decision rejects non-string reason"
      (check-exn exn:fail:contract? (lambda () (make-approve-decision 123))))

    (test-case "make-approve-decision rejects invalid risk-level"
      (check-exn exn:fail:contract? (lambda () (make-approve-decision "x" #:risk-level 'critical))))))

(run-tests suite)
