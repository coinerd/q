#lang racket

;; tests/test-verifier-prompt.rkt — W3 v0.99.5 Verifier Prompt Builder Tests
;;
;; Tests build-verifier-system-prompt and build-verifier-user-message.

(require rackunit
         rackunit/text-ui
         racket/string
         "../agent/verification/verifier-prompt.rkt")

(define suite
  (test-suite "Verifier Prompt Builder (W3 v0.99.5)"

    ;; ── System Prompt ──

    (test-case "system prompt is a non-empty string"
      (define p (build-verifier-system-prompt))
      (check-true (string? p))
      (check-true (> (string-length p) 100)))

    (test-case "system prompt mentions Verifier agent"
      (check-true (string-contains? (build-verifier-system-prompt) "Verifier")))

    (test-case "system prompt requires JSON-only response"
      (check-true (string-contains? (build-verifier-system-prompt) "JSON")))

    (test-case "system prompt lists all three verdicts"
      (define p (build-verifier-system-prompt))
      (check-true (string-contains? p "approve"))
      (check-true (string-contains? p "reject"))
      (check-true (string-contains? p "escalate")))

    (test-case "system prompt includes risk_level field"
      (check-true (string-contains? (build-verifier-system-prompt) "risk_level")))

    (test-case "system prompt includes requires_human field"
      (check-true (string-contains? (build-verifier-system-prompt) "requires_human")))

    (test-case "system prompt mentions artifact_refs"
      (check-true (string-contains? (build-verifier-system-prompt) "artifact_refs")))

    ;; ── User Message: Basic Construction ──

    (test-case "user message includes wave name"
      (define msg
        (build-verifier-user-message #:plan-summary "fix bug"
                                     #:wave-name "W3"
                                     #:files-changed '("a.rkt")
                                     #:test-summary "5/5 pass"))
      (check-true (string-contains? msg "W3")))

    (test-case "user message includes plan summary"
      (define msg
        (build-verifier-user-message #:plan-summary "Implement verifier prompt builder"
                                     #:wave-name "W3"
                                     #:files-changed '("a.rkt")
                                     #:test-summary "5/5 pass"))
      (check-true (string-contains? msg "Implement verifier prompt builder")))

    (test-case "user message includes files changed"
      (define msg
        (build-verifier-user-message
         #:plan-summary "test"
         #:wave-name "W3"
         #:files-changed '("agent/verification/verifier-prompt.rkt" "tests/test-verifier-prompt.rkt")
         #:test-summary "5/5 pass"))
      (check-true (string-contains? msg "verifier-prompt.rkt"))
      (check-true (string-contains? msg "test-verifier-prompt.rkt")))

    (test-case "user message includes test summary"
      (define msg
        (build-verifier-user-message #:plan-summary "test"
                                     #:wave-name "W3"
                                     #:files-changed '("a.rkt")
                                     #:test-summary "42 success(es) 0 failure(s)"))
      (check-true (string-contains? msg "42 success(es) 0 failure(s)")))

    ;; ── User Message: Diff Excerpt ──

    (test-case "user message includes diff excerpt when provided"
      (define msg
        (build-verifier-user-message #:plan-summary "test"
                                     #:wave-name "W3"
                                     #:files-changed '("a.rkt")
                                     #:test-summary "ok"
                                     #:diff-excerpt "+(define x 42)"))
      (check-true (string-contains? msg "(define x 42)")))

    (test-case "user message handles empty diff excerpt"
      (define msg
        (build-verifier-user-message #:plan-summary "test"
                                     #:wave-name "W3"
                                     #:files-changed '("a.rkt")
                                     #:test-summary "ok"))
      (check-true (string-contains? msg "(no diff provided)")))

    (test-case "user message handles explicit empty diff excerpt"
      (define msg
        (build-verifier-user-message #:plan-summary "test"
                                     #:wave-name "W3"
                                     #:files-changed '("a.rkt")
                                     #:test-summary "ok"
                                     #:diff-excerpt ""))
      (check-true (string-contains? msg "(no diff provided)")))

    ;; ── User Message: Edge Cases ──

    (test-case "user message handles empty files-changed list"
      (define msg
        (build-verifier-user-message #:plan-summary "test"
                                     #:wave-name "W3"
                                     #:files-changed '()
                                     #:test-summary "ok"))
      (check-true (string-contains? msg "(none)"))
      (check-true (string-contains? msg "Files Changed (0):")))

    (test-case "user message truncates large file lists"
      (define many-files
        (for/list ([i (in-range 50)])
          (format "file~a.rkt" i)))
      (define msg
        (build-verifier-user-message #:plan-summary "test"
                                     #:wave-name "W3"
                                     #:files-changed many-files
                                     #:test-summary "ok"))
      (check-true (string-contains? msg "more file(s) not shown"))
      ;; Should show file0 but not file49
      (check-true (string-contains? msg "file0.rkt"))
      (check-false (string-contains? msg "file49.rkt")))

    (test-case "user message truncates long diff excerpts"
      (define long-diff (make-string 12000 #\x))
      (define msg
        (build-verifier-user-message #:plan-summary "test"
                                     #:wave-name "W3"
                                     #:files-changed '("a.rkt")
                                     #:test-summary "ok"
                                     #:diff-excerpt long-diff))
      (check-true (string-contains? msg "[truncated]")))))

(run-tests suite)
