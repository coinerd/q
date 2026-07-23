#lang racket/base

;; tests/test-mas-guidance.rkt
;; v0.99.21 W1 (§4.1): Tests for MAS delegation guidance in system prompt.

;; @speed fast
(require racket/string
         rackunit
         rackunit/text-ui
         "../agent/mas-guidance.rkt"
         "../runtime/settings-core.rkt")

;; Helper: create q-settings with given config hash
(define (make-test-settings config-hash)
  (q-settings (hash) config-hash config-hash))

(define (make-mas-config #:blackboard [bb #t] #:hot-swap [hs #f] #:verifier [ver #f])
  (hasheq 'mas
          (hasheq 'blackboard
                  (hasheq 'enabled bb)
                  'hot-swap
                  (hasheq 'enabled hs)
                  'verifier
                  (hasheq 'enabled ver))))

(define suite
  (test-suite "MAS Delegation Guidance (v0.99.21 §4.1)"

    (test-case "guidance is a non-empty string when features are set"
      ;; build-mas-delegation-guidance returns a string regardless of
      ;; blackboard state — the gating happens in run-modes.rkt.
      ;; But it should always be non-empty.
      (define s (make-test-settings (make-mas-config)))
      (define result (build-mas-delegation-guidance s))
      (check-true (string? result))
      (check-true (> (string-length result) 100)))

    (test-case "guidance mentions spawn-subagent and spawn-subagents"
      (define s (make-test-settings (make-mas-config)))
      (define result (build-mas-delegation-guidance s))
      (check-true (string-contains? result "spawn-subagent"))
      (check-true (string-contains? result "spawn-subagents")))

    (test-case "guidance includes hot-swap note when enabled"
      (define s (make-test-settings (make-mas-config #:hot-swap #t)))
      (define result (build-mas-delegation-guidance s))
      (check-true (string-contains? result "hot-swap")))

    (test-case "guidance omits hot-swap note when disabled"
      (define s (make-test-settings (make-mas-config #:hot-swap #f)))
      (define result (build-mas-delegation-guidance s))
      (check-false (string-contains? result "hot-swap")))

    (test-case "guidance includes verifier note when enabled"
      (define s (make-test-settings (make-mas-config #:verifier #t)))
      (define result (build-mas-delegation-guidance s))
      (check-true (string-contains? result "verifier")))

    (test-case "guidance omits verifier note when disabled"
      (define s (make-test-settings (make-mas-config #:verifier #f)))
      (define result (build-mas-delegation-guidance s))
      (check-false (string-contains? result "verifier")))

    (test-case "guidance includes delegation header"
      (define s (make-test-settings (make-mas-config)))
      (define result (build-mas-delegation-guidance s))
      (check-true (string-contains? result "Multi-Agent Delegation")))))

(run-tests suite)
