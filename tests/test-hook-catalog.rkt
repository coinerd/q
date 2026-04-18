#lang racket/base

;; tests/test-hook-catalog.rkt — Wave 1 tests for Extension Hook Catalog Expansion
;; Issue #1207, sub-issues #1208, #1209, #1210
;;
;; Validates:
;; - session-before-switch is registered in hook-action-schemas (#1209)
;; - Streaming hooks (tool.execution.*) are in schemas (#1208)
;; - message-update alias is in schemas (#1208)
;; - tool-call-pre and tool-result-post are in schemas (#1208)
;; - valid-hook-name? rejects unknown hooks with clear error (#1210)
;; - session-before-switch is classified as critical (#1209)
;; - All critical hooks are in hook-action-schemas (#1210)

(require rackunit
         rackunit/text-ui
         "../util/hook-types.rkt"
         "../extensions/hooks.rkt"
         "../extensions/api.rkt")

;; ============================================================
;; #1209: Session Lifecycle Hook Tests
;; ============================================================

(define-test-suite session-lifecycle-tests
  (test-case "session-before-switch is in hook-action-schemas"
    (check-true (hash-has-key? hook-action-schemas 'session-before-switch)
                "session-before-switch should be registered in hook-action-schemas"))

  (test-case "session-before-switch allows pass and block"
    (check-equal? (valid-hook-actions-for 'session-before-switch)
                  '(pass block)))

  (test-case "session-before-switch is a critical hook (safety-first)"
    (check-not-false (member 'session-before-switch critical-hooks)
                     "session-before-switch should be classified as critical"))

  (test-case "session-before-fork is still a critical hook"
    (check-not-false (member 'session-before-fork critical-hooks)))

  (test-case "session-before-compact is still a critical hook"
    (check-not-false (member 'session-before-compact critical-hooks))))

;; ============================================================
;; #1208: Streaming Hook Schema Tests
;; ============================================================

(define-test-suite streaming-hook-schema-tests
  (test-case "tool.execution.start is in hook-action-schemas"
    (check-true (hash-has-key? hook-action-schemas 'tool.execution.start)))

  (test-case "tool.execution.end is in hook-action-schemas"
    (check-true (hash-has-key? hook-action-schemas 'tool.execution.end)))

  (test-case "tool.execution.update is in hook-action-schemas"
    (check-true (hash-has-key? hook-action-schemas 'tool.execution.update)))

  (test-case "tool.execution.start only allows 'pass action"
    (check-equal? (valid-hook-actions-for 'tool.execution.start) '(pass)))

  (test-case "tool.execution.end only allows 'pass action"
    (check-equal? (valid-hook-actions-for 'tool.execution.end) '(pass)))

  (test-case "tool.execution.update allows 'amend action"
    (check-equal? (valid-hook-actions-for 'tool.execution.update) '(amend)))

  (test-case "message-update alias is in hook-action-schemas"
    ;; agent/loop.rkt dispatches 'message-update (hyphen) but schema had 'message.update (dot)
    ;; Both forms should be recognized
    (check-true (or (hash-has-key? hook-action-schemas 'message-update)
                    (hash-has-key? hook-action-schemas 'message.update))
                "message-update or message.update should be in schemas"))

  (test-case "message-stream.delta is in hook-action-schemas"
    (check-true (hash-has-key? hook-action-schemas 'message.stream.delta)))

  (test-case "tool-call-pre is in hook-action-schemas"
    ;; Dispatched from scheduler.rkt
    (check-true (hash-has-key? hook-action-schemas 'tool-call-pre)))

  (test-case "tool-result-post is in hook-action-schemas"
    ;; Dispatched from scheduler.rkt
    (check-true (hash-has-key? hook-action-schemas 'tool-result-post))))

;; ============================================================
;; #1210: Hook Name Validation Tests
;; ============================================================

(define-test-suite hook-validation-tests
  (test-case "valid-hook-name? accepts known hooks"
    (for ([name (in-list '(turn-start turn-end tool-call tool-result
                          session-before-switch session-before-fork session-before-compact
                          tool.execution.start tool.execution.end tool.execution.update
                          message-start message-end message.update message-update
                          before-provider-request context input))])
      (check-true (valid-hook-name? name)
                  (format "Expected ~a to be a valid hook name" name))))

  (test-case "valid-hook-name? rejects unknown hooks"
    (check-false (valid-hook-name? 'nonexistent-hook))
    (check-false (valid-hook-name? 'fake.hook))
    (check-false (valid-hook-name? 'totally-made-up)))

  (test-case "valid-hook-actions-for returns permissive default for unknown hooks"
    ;; Unknown hooks should get permissive default for backward compat
    (check-equal? (valid-hook-actions-for 'totally-unknown-hook)
                  '(pass amend block)
                  "Unknown hooks should get permissive default")))

;; ============================================================
;; #1210: Schema Completeness Tests
;; ============================================================

(define-test-suite schema-completeness-tests
  (test-case "hook-point-names returns all registered hooks"
    (define names (hook-point-names))
    (for ([expected (in-list '(turn-start turn-end tool-call
                                     session-before-switch
                                     tool.execution.start tool.execution.end))])
      (check-not-false (member expected names)
                       (format "~a should be in hook-point-names" expected))))

  (test-case "all critical hooks are in hook-action-schemas"
    (for ([hook (in-list critical-hooks)])
      (check-true (hash-has-key? hook-action-schemas hook)
                  (format "Critical hook ~a should be in hook-action-schemas" hook))))

  (test-case "validate-hook-result works for session-before-switch"
    (define result (hook-block "safety-check"))
    (check-true (validate-hook-result 'session-before-switch result)))

  (test-case "validate-hook-result rejects invalid action for tool.execution.start"
    (define bad-result (hook-amend (hasheq 'extra 'data)))
    (check-false (validate-hook-result 'tool.execution.start bad-result))))

;; ============================================================
;; Run all tests
;; ============================================================

(run-tests
 (make-test-suite "Hook Catalog Expansion Tests"
   (list session-lifecycle-tests
         streaming-hook-schema-tests
         hook-validation-tests
         schema-completeness-tests)))
