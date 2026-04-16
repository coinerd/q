#lang racket

;; tests/test-context-overflow.rkt — FEAT-66: context overflow recovery

(require rackunit
         "../runtime/auto-retry.rkt")

;; ============================================================
;; context-overflow-error? predicate
;; ============================================================

(test-case "context-overflow-error? detects context_length_exceeded"
  (check-true (context-overflow-error? (exn:fail "context_length_exceeded: too many tokens"
                                                 (current-continuation-marks)))))

(test-case "context-overflow-error? detects maximum context"
  (check-true (context-overflow-error? (exn:fail "Maximum context length exceeded"
                                                 (current-continuation-marks)))))

(test-case "context-overflow-error? detects input too long"
  (check-true (context-overflow-error? (exn:fail "Input is too long for this model"
                                                 (current-continuation-marks)))))

(test-case "context-overflow-error? detects request too large"
  (check-true (context-overflow-error? (exn:fail "Request too large, reduce the length of messages"
                                                 (current-continuation-marks)))))

(test-case "context-overflow-error? detects token limit"
  (check-true (context-overflow-error? (exn:fail "Token limit exceeded"
                                                 (current-continuation-marks)))))

(test-case "context-overflow-error? returns #f for rate limit error"
  (check-false (context-overflow-error? (exn:fail "429 Rate limit exceeded"
                                                  (current-continuation-marks)))))

(test-case "context-overflow-error? returns #f for server error"
  (check-false (context-overflow-error? (exn:fail "500 Internal server error"
                                                  (current-continuation-marks)))))

(test-case "context-overflow-error? returns #f for timeout"
  (check-false (context-overflow-error? (exn:fail "Connection timed out"
                                                  (current-continuation-marks)))))

(test-case "context-overflow-error? returns #f for generic error"
  (check-false (context-overflow-error? (exn:fail "Something else went wrong"
                                                  (current-continuation-marks)))))

(test-case "context-overflow-error? is case-insensitive"
  (check-true (context-overflow-error? (exn:fail "CONTEXT_LENGTH EXCEEDED"
                                                 (current-continuation-marks)))))
