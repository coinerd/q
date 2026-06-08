#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-save-conclusion.rkt — tests for save-conclusion and set-task-state tools
;; v0.75.1 W0: Tool definitions + validation

(require rackunit
         rackunit/text-ui
         "../tools/builtins/save-conclusion.rkt"
         "../tools/builtins/set-task-state.rkt"
         "../tools/tool.rkt"
         (only-in "../runtime/context-assembly/task-conclusion.rkt"
                  task-conclusion-text
                  task-conclusion-relevance-tags))

;; save-conclusion and set-task-state are tool structs created by define-tool

(define (call-tool t args)
  ((tool-execute t) args #f))

(define suite
  (test-suite "task-tools"

    ;; ── Tool metadata ──

    (test-case "save-conclusion is a valid tool"
      (check-true (tool? save-conclusion))
      (check-equal? (tool-name save-conclusion) "save-conclusion"))

    (test-case "set-task-state is a valid tool"
      (check-true (tool? set-task-state))
      (check-equal? (tool-name set-task-state) "set-task-state"))

    ;; ── save-conclusion handler ──

    (test-case "save-conclusion creates conclusion with content"
      (define result (call-tool save-conclusion (hasheq 'content "Found the bug")))
      (check-false (tool-result-is-error? result))
      (check-not-false (tool-result-details result)))

    (test-case "save-conclusion rejects missing content"
      (define result (call-tool save-conclusion (hasheq)))
      (check-true (tool-result-is-error? result)))

    (test-case "save-conclusion rejects invalid category"
      (define result (call-tool save-conclusion (hasheq 'content "test" 'category 'invalid)))
      (check-true (tool-result-is-error? result)))

    (test-case "save-conclusion accepts all valid categories"
      (for ([cat (in-list '(fact decision pattern error-cause test-result))])
        (define result (call-tool save-conclusion (hasheq 'content "test" 'category cat)))
        (check-false (tool-result-is-error? result) (format "category ~a should be accepted" cat))))

    (test-case "save-conclusion with tags"
      (define result (call-tool save-conclusion (hasheq 'content "test" 'tags '(bug pattern))))
      (check-false (tool-result-is-error? result)))

    (test-case "save-conclusion creates task-conclusion in details"
      (define result (call-tool save-conclusion (hasheq 'content "test conclusion" 'tags '(arch))))
      (define details (tool-result-details result))
      (check-not-false (hash-ref details '_conclusion #f))
      (define c (hash-ref details '_conclusion))
      (check-equal? (task-conclusion-text c) "test conclusion")
      (check-equal? (task-conclusion-relevance-tags c) '(arch)))

    ;; ── set-task-state handler ──

    (test-case "set-task-state accepts valid state + event"
      (define result (call-tool set-task-state (hasheq 'state 'exploration 'event 'begin-explore)))
      (check-false (tool-result-is-error? result)))

    (test-case "set-task-state rejects missing state"
      (define result (call-tool set-task-state (hasheq 'event 'begin-explore)))
      (check-true (tool-result-is-error? result)))

    (test-case "set-task-state rejects missing event"
      (define result (call-tool set-task-state (hasheq 'state 'exploration)))
      (check-true (tool-result-is-error? result)))

    (test-case "set-task-state rejects invalid state"
      (define result (call-tool set-task-state (hasheq 'state 'invalid 'event 'begin-explore)))
      (check-true (tool-result-is-error? result)))

    (test-case "set-task-state rejects invalid event"
      (define result (call-tool set-task-state (hasheq 'state 'exploration 'event 'invalid)))
      (check-true (tool-result-is-error? result)))

    (test-case "set-task-state packages target and event in details"
      (define result (call-tool set-task-state (hasheq 'state 'planning 'event 'begin-plan)))
      (define details (tool-result-details result))
      (check-equal? (hash-ref details '_task-state-target #f) 'planning)
      (check-equal? (hash-ref details '_task-state-event #f) 'begin-plan))

    ;; v0.75.9: LLM sends string arguments, not symbols
    (test-case "set-task-state accepts string args (LLM path)"
      (define result (call-tool set-task-state (hasheq 'state "exploration" 'event "begin-explore")))
      (check-false (tool-result-is-error? result)
                   (format "string args should work, got: ~a" (tool-result-content result))))

    (test-case "set-task-state rejects invalid string state"
      (define result (call-tool set-task-state (hasheq 'state "invalid" 'event "begin-explore")))
      (check-true (tool-result-is-error? result)))))

(run-tests suite)
