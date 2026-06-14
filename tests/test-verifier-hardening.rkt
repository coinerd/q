#lang racket

;; @speed fast  ;; @suite extensions

;; tests/test-verifier-hardening.rkt — W2 (v0.99.6) Hardening Tests
;;
;; Tests for M1-M4 and L2 hardening fixes:
;; - M3: extract-response-text handles content-part structs and unknown types
;; - M4: verifier LLM timeout raises exn:fail:network
;; - L2: risk threshold override updates reason text

(require rackunit
         rackunit/text-ui
         json
         racket/string
         "../agent/verification/verifier-core.rkt"
         "../agent/verification/verifier-types.rkt"
         "../agent/verification/verifier-prompt.rkt"
         "../llm/provider.rkt"
         "../llm/model.rkt"
         (only-in "../util/event/event.rkt" make-event)
         (only-in "../tui/state-events/core-handlers.rkt" verification-payload-ref))

(define suite
  (test-suite "Verifier Hardening (W2 v0.99.6)"

    ;; ── M3: extract-response-text ──

    (test-case "extract-response-text: hash with text key"
      (define resp (make-model-response (list (hasheq 'text "hello")) (hasheq) "mock" 'end_turn))
      (check-equal? (extract-response-text resp) "hello"))

    (test-case "extract-response-text: string part"
      (define resp (model-response (list "world") (hasheq) "mock" 'end_turn))
      (check-equal? (extract-response-text resp) "world"))

    (test-case "extract-response-text: unknown type produces empty string"
      (define resp (model-response (list 12345) (hasheq) "mock" 'end_turn))
      (check-equal? (extract-response-text resp) ""))

    (test-case "extract-response-text: multiple parts joined"
      (define resp (model-response (list (hasheq 'text "foo") "bar" 999) (hasheq) "mock" 'end_turn))
      (check-equal? (extract-response-text resp) "foobar"))

    ;; ── L2: risk threshold reason text ──

    (test-case "L2: risk threshold override adds reason text"
      (parameterize ([current-verifier-risk-threshold 'low])
        (define base (verifier-decision 'approve "looks good" 'medium #f '() #f))
        (define result (enforce-risk-threshold base))
        (check-true (verifier-decision-requires-human? result))
        (check-true (string-contains? (verifier-decision-reason result) "risk threshold override"))))

    (test-case "L2: non-overriding risk leaves reason unchanged"
      (parameterize ([current-verifier-risk-threshold 'high])
        (define base (verifier-decision 'approve "looks good" 'low #f '() #f))
        (define result (enforce-risk-threshold base))
        (check-false (verifier-decision-requires-human? result))
        (check-equal? (verifier-decision-reason result) "looks good")))

    (test-case "L2: override reason includes risk level and threshold"
      (parameterize ([current-verifier-risk-threshold 'medium])
        (define base (verifier-decision 'approve "ok" 'high #f '() #f))
        (define result (enforce-risk-threshold base))
        (define r (verifier-decision-reason result))
        (check-true (string-contains? r "high"))
        (check-true (string-contains? r "medium"))))

    ;; ── M4: verifier timeout parameter ──

    (test-case "M4: current-verifier-timeout-ms parameter exists"
      (check-true (exact-positive-integer? (current-verifier-timeout-ms))))

    (test-case "M4: timeout default is 120000"
      (check-equal? (current-verifier-timeout-ms) 120000))

    ;; ── M2: payload format robustness ──

    (test-case "M2: direct kebab-case key lookup"
      (define evt (make-event 'test 0 #f #f (hasheq 'artifact-count 5)))
      (check-equal? (verification-payload-ref evt 'artifact-count 0) 5))

    (test-case "M2: camelCase key lookup works for kebab query"
      (define evt (make-event 'test 0 #f #f (hasheq 'artifactCount 7)))
      (check-equal? (verification-payload-ref evt 'artifact-count 0) 7))

    (test-case "M2: GSD-wrapped format with kebab in data"
      (define evt (make-event 'test 0 #f #f (hasheq 'data (hasheq 'verdict 'approve))))
      (check-equal? (verification-payload-ref evt 'verdict #f) 'approve))

    (test-case "M2: GSD-wrapped format with camelCase in data"
      (define evt (make-event 'test 0 #f #f (hasheq 'data (hasheq 'riskLevel 'high))))
      (check-equal? (verification-payload-ref evt 'risk-level #f) 'high))

    (test-case "M2: missing key returns default"
      (define evt (make-event 'test 0 #f #f (hasheq)))
      (check-equal? (verification-payload-ref evt 'missing-key 'fallback) 'fallback))

    (test-case "M2: non-hash payload returns default"
      (define evt (make-event 'test 0 #f #f "not-a-hash"))
      (check-equal? (verification-payload-ref evt 'any-key 'def) 'def))

    ;; ── L1 (v0.99.7): Parameterized prompt limits ──

    (test-case "L1: current-verifier-max-files-shown has default 30"
      (check-equal? (current-verifier-max-files-shown) 30))

    (test-case "L1: current-verifier-max-diff-chars has default 8000"
      (check-equal? (current-verifier-max-diff-chars) 8000))

    (test-case "L1: current-verifier-max-file-lines has default 3"
      (check-equal? (current-verifier-max-file-lines) 3))

    (test-case "L1: parameterized max-files-shown controls truncation"
      (parameterize ([current-verifier-max-files-shown 2])
        (define msg
          (build-verifier-user-message #:plan-summary ""
                                       #:wave-name "W0"
                                       #:files-changed '("a.rkt" "b.rkt" "c.rkt")
                                       #:test-summary ""))
        (check-true (string-contains? msg "... and 1 more file(s) not shown"))))

    (test-case "L1: parameterized max-diff-chars controls truncation"
      (parameterize ([current-verifier-max-diff-chars 10])
        (define msg
          (build-verifier-user-message #:plan-summary ""
                                       #:wave-name "W0"
                                       #:files-changed '()
                                       #:test-summary ""
                                       #:diff-excerpt "0123456789ABCDE"))
        (check-true (string-contains? msg "[truncated]"))))

    ;; ── L3 (v0.99.7): Risk threshold validation ──

    (test-case "L3: valid-risk-levels includes critical"
      (check-true (and (member 'critical valid-risk-levels) #t) "critical is in valid-risk-levels"))

    (test-case "L3: valid-risk-level? accepts low/medium/high/critical"
      (for ([v '(low medium high critical)])
        (check-true (valid-risk-level? v) "~a should be valid")))

    (test-case "L3: valid-risk-level? rejects invalid values"
      (check-false (valid-risk-level? 'extreme))
      (check-false (valid-risk-level? 42))
      (check-false (valid-risk-level? "medium")))

    (test-case "L3: setting invalid risk threshold raises argument error"
      (check-exn (lambda (e)
                   (and (exn:fail:contract? e)
                        (string-contains? (exn-message e) "current-verifier-risk-threshold")))
                 (lambda () (current-verifier-risk-threshold 'extreme))))

    (test-case "L3: valid risk thresholds accepted"
      (for ([v '(low medium high critical)])
        (parameterize ([current-verifier-risk-threshold v])
          (check-equal? (current-verifier-risk-threshold) v))))))

(run-tests suite)
