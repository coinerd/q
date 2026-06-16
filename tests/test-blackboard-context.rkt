#lang racket

;; @speed fast  ;; @suite extensions

;; tests/test-blackboard-context.rkt — W5 (v0.99.7) Blackboard Context Tests
;;
;; Tests for blackboard context snippet generation and injection:
;;   - Empty blackboard → snippet returns #f
;;   - Each formatter produces correct output
;;   - Full blackboard → all sections present
;;   - Injection disabled → no snippet in preamble

(require rackunit
         rackunit/text-ui
         (only-in "../agent/blackboard.rkt"
                  blackboard-state
                  empty-blackboard
                  current-blackboard
                  make-blackboard
                  update-blackboard!)
         (only-in "../runtime/context-assembly/blackboard-context.rkt"
                  build-blackboard-context-snippet
                  MAX-BLACKBOARD-SNIPPET-LEN
                  truncate-snippet
                  format-active-plan
                  format-wave-status
                  format-test-results
                  format-artifacts
                  format-hypotheses
                  format-verifier-decisions)
         (only-in "../runtime/context-assembly/state-aware-builder.rkt"
                  build-state-awareness-preamble
                  current-blackboard-injection-enabled)
         (only-in "../util/message/message.rkt" message-content)
         (only-in "../util/content/content-parts.rkt" text-part-text))

;; Helper: create a populated blackboard-state for testing.
(define (make-populated-state)
  (blackboard-state (hasheq 'wave-count 4 'summary "MAS Blackboard implementation")
                    (list (hasheq 'id "h1" 'question "Why does X fail?" 'agent-name "explorer"))
                    (list (hasheq 'file "test-foo.rkt" 'result 'pass)
                          (hasheq 'file "test-bar.rkt" 'result 'fail))
                    (list (hasheq 'name "plan.md" 'path "/tmp/plan.md" 'artifact-type 'markdown))
                    (hash "W0" 'completed "W1" 'in-progress)
                    (list (hasheq 'verdict 'approve 'risk-level 'low))
                    '()
                    1000))

;; Helper: extract text from a preamble message
(define (preamble-text preamble)
  (text-part-text (car (message-content preamble))))

(define suite
  (test-suite "Blackboard Context (W5 v0.99.7)"

    ;; ── Empty Blackboard ──

    (test-case "empty blackboard → snippet returns #f"
      (check-false (build-blackboard-context-snippet empty-blackboard)))

    ;; ── Individual Formatters ──

    (test-case "format-active-plan with wave-count and summary"
      (define state (make-populated-state))
      (check-equal? (format-active-plan state) "Plan: 4 waves — MAS Blackboard implementation"))

    (test-case "format-active-plan with #f plan → returns #f"
      (check-false (format-active-plan empty-blackboard)))

    (test-case "format-wave-status with waves"
      (define state (make-populated-state))
      (define result (format-wave-status state))
      (check-true (string? result))
      (check-true (string-contains? result "W0=completed"))
      (check-true (string-contains? result "W1=in-progress")))

    (test-case "format-wave-status with empty → returns #f"
      (check-false (format-wave-status empty-blackboard)))

    (test-case "format-test-results with results"
      (define state (make-populated-state))
      (define result (format-test-results state))
      (check-true (string? result))
      (check-true (string-contains? result "test-foo.rkt=pass"))
      (check-true (string-contains? result "test-bar.rkt=fail")))

    (test-case "format-test-results with empty → returns #f"
      (check-false (format-test-results empty-blackboard)))

    (test-case "format-artifacts with artifacts"
      (define state (make-populated-state))
      (define result (format-artifacts state))
      (check-true (string? result))
      (check-true (string-contains? result "plan.md (markdown)")))

    (test-case "format-hypotheses with open hypotheses"
      (define state (make-populated-state))
      (define result (format-hypotheses state))
      (check-true (string? result))
      (check-true (string-contains? result "h1"))
      (check-true (string-contains? result "Why does X fail?")))

    (test-case "format-verifier-decisions with decisions"
      (define state (make-populated-state))
      (define result (format-verifier-decisions state))
      (check-true (string? result))
      (check-true (string-contains? result "approve"))
      (check-true (string-contains? result "low")))

    ;; ── Full Snippet ──

    (test-case "full blackboard → snippet has all sections"
      (define state (make-populated-state))
      (define snippet (build-blackboard-context-snippet state))
      (check-true (string? snippet))
      (check-true (string-contains? snippet "[Blackboard]"))
      (check-true (string-contains? snippet "Plan:"))
      (check-true (string-contains? snippet "Waves:"))
      (check-true (string-contains? snippet "Tests ("))
      (check-true (string-contains? snippet "Artifacts ("))
      (check-true (string-contains? snippet "Open hypotheses"))
      (check-true (string-contains? snippet "Verifier:")))

    (test-case "snippet with only wave-status still returns text"
      (define state (blackboard-state #f '() '() '() (hash "W0" 'completed) '() '() 0))
      (define snippet (build-blackboard-context-snippet state))
      (check-true (string? snippet))
      (check-true (string-contains? snippet "[Blackboard]"))
      (check-true (string-contains? snippet "W0=completed"))
      (check-false (string-contains? snippet "Plan:")))

    ;; ── Injection into Preamble ──

    (test-case "injection disabled → preamble has no blackboard section"
      (parameterize ([current-blackboard-injection-enabled #f])
        (define preamble (build-state-awareness-preamble 'implementation '()))
        (check-not-false preamble)
        (check-false (string-contains? (preamble-text preamble) "[Blackboard]"))))

    (test-case "injection enabled with empty blackboard → no snippet appended"
      (define bb (make-blackboard))
      (parameterize ([current-blackboard-injection-enabled #t]
                     [current-blackboard bb])
        (define preamble (build-state-awareness-preamble 'implementation '()))
        (check-not-false preamble)
        (check-false (string-contains? (preamble-text preamble) "[Blackboard]"))))

    (test-case "injection enabled with populated blackboard → snippet in preamble"
      (define bb (make-blackboard))
      (parameterize ([current-blackboard bb])
        (update-blackboard! (lambda (_)
                              (blackboard-state #f '() '() '() (hash "W0" 'completed) '() '() 0))
                            bb))
      (parameterize ([current-blackboard-injection-enabled #t]
                     [current-blackboard bb])
        (define preamble (build-state-awareness-preamble 'implementation '()))
        (check-not-false preamble)
        (check-true (string-contains? (preamble-text preamble) "[Blackboard]"))
        (check-true (string-contains? (preamble-text preamble) "W0=completed"))))

    ;; ── v0.99.14 W3: Token Budget Guard ──

    (test-case "MAX-BLACKBOARD-SNIPPET-LEN is 500"
      (check-equal? MAX-BLACKBOARD-SNIPPET-LEN 500))

    (test-case "snippet under 500 chars is returned as-is"
      (define state (make-populated-state))
      (define snippet (build-blackboard-context-snippet state))
      (check-true (string? snippet))
      (check-true (<= (string-length snippet) MAX-BLACKBOARD-SNIPPET-LEN)
                  "small blackboard should not be truncated")
      (check-false (string-suffix? snippet "...")))

    (test-case "snippet over 500 chars is truncated to exactly 500"
      (define big-wave-status
        (for/hash ([i (in-range 60)])
          (values (format "Wave~a" i) (if (even? i) 'completed 'in-progress))))
      (define state (blackboard-state #f '() '() '() big-wave-status '() '() 0))
      (define snippet (build-blackboard-context-snippet state))
      (check-true (string? snippet))
      (check-equal? (string-length snippet) MAX-BLACKBOARD-SNIPPET-LEN)
      (check-true (string-suffix? snippet "...")))

    (test-case "truncate-snippet is identity for short strings"
      (check-equal? (truncate-snippet "hello") "hello"))

    (test-case "truncate-snippet truncates long strings to 497+..."
      (define long-string (make-string 1000 #\A))
      (define result (truncate-snippet long-string))
      (check-equal? (string-length result) 500)
      (check-true (string-suffix? result "..."))
      (check-equal? (substring result 0 497) (make-string 497 #\A)))

    (test-case "truncate-snippet boundary: exactly 500 chars untouched"
      (define exact-string (make-string 500 #\B))
      (check-equal? (truncate-snippet exact-string) exact-string))

    (test-case "truncate-snippet boundary: 501 chars truncated"
      (define over-string (make-string 501 #\C))
      (define result (truncate-snippet over-string))
      (check-equal? (string-length result) 500)
      (check-true (string-suffix? result "...")))))

(run-tests suite)
