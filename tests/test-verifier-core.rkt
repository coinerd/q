#lang racket

;; tests/test-verifier-core.rkt — W4 v0.99.5 Verifier Core Tests
;;
;; Tests run-verification with mock providers covering:
;;   - Approve/reject/escalate response parsing
;;   - Non-JSON → auto-reject
;;   - Missing/invalid fields → auto-reject
;;   - Provider errors → escalate
;;   - Risk threshold enforcement

(require rackunit
         rackunit/text-ui
         json
         "../agent/verification/verifier-core.rkt"
         "../agent/verification/verifier-types.rkt"
         "../llm/provider.rkt"
         "../llm/model.rkt")

;; ── Helper: build a mock provider that returns a given JSON string ──

(define (make-json-response-provider json-string)
  (make-mock-provider
   (make-model-response (list (hasheq 'text json-string)) (hasheq) "mock-model" 'end_turn)))

(define (make-raw-response-provider raw-text)
  (make-mock-provider
   (make-model-response (list (hasheq 'text raw-text)) (hasheq) "mock-model" 'end_turn)))

;; Helper: valid JSON decision strings
(define approve-json
  (jsexpr->string (hasheq 'verdict
                          "approve"
                          'reason
                          "Tests pass, code is clean"
                          'risk_level
                          "low"
                          'requires_human
                          #f
                          'artifact_refs
                          '("a.rkt")
                          'timestamp
                          #f)))

(define reject-json
  (jsexpr->string (hasheq 'verdict
                          "reject"
                          'reason
                          "Tests fail"
                          'risk_level
                          "high"
                          'requires_human
                          #t
                          'artifact_refs
                          '("b.rkt")
                          'timestamp
                          #f)))

(define escalate-json
  (jsexpr->string (hasheq 'verdict
                          "escalate"
                          'reason
                          "Uncertain about security"
                          'risk_level
                          "high"
                          'requires_human
                          #t
                          'artifact_refs
                          '("c.rkt")
                          'timestamp
                          #f)))

;; Helper: run-verification with common args
(define (run-with-provider provider #:diff [diff ""])
  (run-verification provider "Test plan" "W4" '("a.rkt" "b.rkt") "10/10 pass" #:diff-excerpt diff))

(define suite
  (test-suite "Verifier Core (W4 v0.99.5)"

    ;; ── Risk Level Utilities ──

    (test-case "risk-level->rank returns correct ordering"
      (check-equal? (risk-level->rank 'low) 0)
      (check-equal? (risk-level->rank 'medium) 1)
      (check-equal? (risk-level->rank 'high) 2))

    (test-case "risk-level->rank returns 0 for unknown level"
      (check-equal? (risk-level->rank 'unknown) 0))

    (test-case "risk-at-or-above-threshold? compares correctly"
      (check-false (risk-at-or-above-threshold? 'low 'medium))
      (check-true (risk-at-or-above-threshold? 'medium 'medium))
      (check-true (risk-at-or-above-threshold? 'high 'medium))
      (check-false (risk-at-or-above-threshold? 'low 'high)))

    ;; ── Response Extraction ──

    (test-case "extract-response-text extracts text from hash parts"
      (define resp
        (make-model-response (list (hasheq 'text "hello world")) (hasheq) "mock" 'end_turn))
      (check-equal? (extract-response-text resp) "hello world"))

    (test-case "extract-response-text handles multiple parts"
      (define resp
        (make-model-response (list (hasheq 'text "part1") (hasheq 'text "part2"))
                             (hasheq)
                             "mock"
                             'end_turn))
      (check-equal? (extract-response-text resp) "part1part2"))

    (test-case "extract-response-text handles non-text hash keys gracefully"
      (define resp (make-model-response (list (hasheq 'type 'image)) (hasheq) "mock" 'end_turn))
      ;; Parts without 'text key return empty string
      (check-equal? (extract-response-text resp) ""))

    ;; ── JSON Response Parsing ──

    (test-case "parse-verifier-response parses valid approve JSON"
      (define parsed (parse-verifier-response approve-json))
      (check-true (verifier-decision? parsed))
      (check-equal? (verifier-decision-verdict parsed) 'approve))

    (test-case "parse-verifier-response parses valid reject JSON"
      (define parsed (parse-verifier-response reject-json))
      (check-true (verifier-decision? parsed))
      (check-equal? (verifier-decision-verdict parsed) 'reject))

    (test-case "parse-verifier-response parses valid escalate JSON"
      (define parsed (parse-verifier-response escalate-json))
      (check-true (verifier-decision? parsed))
      (check-equal? (verifier-decision-verdict parsed) 'escalate))

    (test-case "parse-verifier-response returns #f for non-JSON"
      (check-false (parse-verifier-response "not json at all")))

    (test-case "parse-verifier-response returns #f for empty string"
      (check-false (parse-verifier-response "")))

    (test-case "parse-verifier-response returns #f for invalid verdict"
      (define bad-json
        (jsexpr->string (hasheq 'verdict
                                "maybe"
                                'reason
                                "test"
                                'risk_level
                                "low"
                                'requires_human
                                #f
                                'artifact_refs
                                '()
                                'timestamp
                                #f)))
      (check-false (parse-verifier-response bad-json)))

    ;; ── Risk Threshold Enforcement ──

    (test-case "enforce-risk-threshold forces HITL on high risk"
      (parameterize ([current-verifier-risk-threshold 'medium])
        (define vd (verifier-decision 'approve "ok" 'high #f '() #f))
        (define enforced (enforce-risk-threshold vd))
        (check-true (verifier-decision-requires-human? enforced))))

    (test-case "enforce-risk-threshold leaves low risk unchanged"
      (parameterize ([current-verifier-risk-threshold 'high])
        (define vd (verifier-decision 'approve "ok" 'low #f '() #f))
        (define enforced (enforce-risk-threshold vd))
        (check-false (verifier-decision-requires-human? enforced))))

    (test-case "enforce-risk-threshold leaves existing HITL unchanged"
      (parameterize ([current-verifier-risk-threshold 'low])
        (define vd (verifier-decision 'approve "ok" 'high #t '() #f))
        (define enforced (enforce-risk-threshold vd))
        (check-true (verifier-decision-requires-human? enforced))))

    ;; ── run-verification: Happy Path ──

    (test-case "run-verification returns approve decision for valid response"
      (define provider (make-json-response-provider approve-json))
      (define result (run-with-provider provider))
      (check-true (verifier-decision? result))
      (check-equal? (verifier-decision-verdict result) 'approve)
      (check-equal? (verifier-decision-risk-level result) 'low))

    (test-case "run-verification returns reject decision for valid response"
      (define provider (make-json-response-provider reject-json))
      (define result (run-with-provider provider))
      (check-equal? (verifier-decision-verdict result) 'reject)
      (check-equal? (verifier-decision-risk-level result) 'high))

    (test-case "run-verification returns escalate decision for valid response"
      (define provider (make-json-response-provider escalate-json))
      (define result (run-with-provider provider))
      (check-equal? (verifier-decision-verdict result) 'escalate)
      (check-true (verifier-decision-requires-human? result)))

    ;; ── run-verification: Failure Modes ──

    (test-case "run-verification auto-rejects non-JSON response"
      (define provider (make-raw-response-provider "I think this is fine"))
      (define result (run-with-provider provider))
      (check-equal? (verifier-decision-verdict result) 'reject)
      (check-true (verifier-decision-requires-human? result))
      (check-equal? (verifier-decision-risk-level result) 'high))

    (test-case "run-verification auto-rejects empty response"
      (define provider (make-raw-response-provider ""))
      (define result (run-with-provider provider))
      (check-equal? (verifier-decision-verdict result) 'reject))

    (test-case "run-verification auto-rejects invalid JSON structure"
      (define provider (make-raw-response-provider "{\"foo\": \"bar\"}"))
      (define result (run-with-provider provider))
      (check-equal? (verifier-decision-verdict result) 'reject)
      (check-true (verifier-decision-requires-human? result)))

    (test-case "run-verification auto-rejects markdown-wrapped JSON"
      (define provider (make-raw-response-provider (string-append "```json\n" approve-json "\n```")))
      (define result (run-with-provider provider))
      ;; Markdown wrapping makes it non-JSON → auto-reject
      (check-equal? (verifier-decision-verdict result) 'reject))

    ;; ── run-verification: Risk Threshold ──

    (test-case "run-verification enforces risk threshold on approve"
      ;; approve with medium risk, threshold=medium → requires-human? #t
      (define med-approve-json
        (jsexpr->string (hasheq 'verdict
                                "approve"
                                'reason
                                "ok but uncertain"
                                'risk_level
                                "medium"
                                'requires_human
                                #f
                                'artifact_refs
                                '("x.rkt")
                                'timestamp
                                #f)))
      (define provider (make-json-response-provider med-approve-json))
      (parameterize ([current-verifier-risk-threshold 'medium])
        (define result (run-with-provider provider))
        (check-equal? (verifier-decision-verdict result) 'approve)
        (check-true (verifier-decision-requires-human? result))))

    (test-case "run-verification does not force HITL below threshold"
      ;; approve with low risk, threshold=high → requires-human? unchanged
      (define provider (make-json-response-provider approve-json))
      (parameterize ([current-verifier-risk-threshold 'high])
        (define result (run-with-provider provider))
        (check-equal? (verifier-decision-verdict result) 'approve)
        (check-false (verifier-decision-requires-human? result))))

    ;; ── run-verification: Error Handling ──

    (test-case "run-verification escalates on provider error"
      (define error-provider
        (make-provider (lambda () "error-provider")
                       (lambda () (hasheq))
                       (lambda (req) (error 'test-provider "simulated failure"))
                       (lambda (req) '())))
      (define result (run-with-provider error-provider))
      (check-equal? (verifier-decision-verdict result) 'escalate)
      (check-true (verifier-decision-requires-human? result)))

    (test-case "run-verification escalates on network error"
      (define net-error-provider
        (make-provider (lambda () "net-error-provider")
                       (lambda () (hasheq))
                       (lambda (req)
                         (raise (exn:fail:network "connection refused" (current-continuation-marks))))
                       (lambda (req) '())))
      (define result (run-with-provider net-error-provider))
      (check-equal? (verifier-decision-verdict result) 'escalate))

    ;; ── run-verification: Artifact Refs ──

    (test-case "run-verification passes files-changed to reject decision"
      (define provider (make-raw-response-provider "not json"))
      (define result (run-with-provider provider))
      (check-equal? (verifier-decision-artifact-refs result) '("a.rkt" "b.rkt")))

    (test-case "run-verification passes files-changed to escalate decision"
      (define error-provider
        (make-provider (lambda () "err")
                       (lambda () (hasheq))
                       (lambda (req) (error 'test "fail"))
                       (lambda (req) '())))
      (define result (run-with-provider error-provider))
      (check-equal? (verifier-decision-artifact-refs result) '("a.rkt" "b.rkt")))

    ;; ── Diff Excerpt Handling ──

    (test-case "run-verification handles empty diff excerpt"
      (define provider (make-json-response-provider approve-json))
      (define result (run-with-provider provider #:diff ""))
      (check-equal? (verifier-decision-verdict result) 'approve))

    (test-case "run-verification handles large diff excerpt"
      (define provider (make-json-response-provider approve-json))
      (define result (run-with-provider provider #:diff (make-string 10000 #\x)))
      (check-equal? (verifier-decision-verdict result) 'approve))))

(run-tests suite)
