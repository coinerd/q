#lang racket

;; tests/test-settings-flow.rkt — Settings contract violation TDD tests (#1239)
;;
;; These tests prove that the q-settings struct does NOT flow through
;; exec-context to tool implementations. Tests 1-2 should PASS (confirm bug).
;; Tests 3-10 verify the expected behavior after the fix.

(require rackunit
         rackunit/text-ui
         "../runtime/settings.rkt"
         "../tools/tool.rkt"
         "../tools/builtins/bash.rkt"
         "../sandbox/limits.rkt"
         "../agent/event-bus.rkt"
         "../extensions/api.rkt")

;; ============================================================
;; Test suite
;; ============================================================

(define suite
  (test-suite "Settings flow contract tests (#1239)"

    ;; ────────────────────────────────────────────────────────
    ;; Bug confirmation tests (should pass now — proving the bug exists)
    ;; ────────────────────────────────────────────────────────
    (test-case "plain hash is NOT q-settings — confirms bug type"
      (define plain-hash (hasheq 'provider #f 'model "glm-5.1"))
      (check-false (q-settings? plain-hash) "A plain hash should not satisfy q-settings?"))

    (test-case "sandbox-enabled? works with plain hash (post-fix)"
      (define plain-hash (hasheq 'provider #f 'model "glm-5.1"))
      (check-true (sandbox-enabled? plain-hash)
                  "sandbox-enabled? should return default #t with plain hash"))

    ;; ────────────────────────────────────────────────────────
    ;; Expected behavior tests (should pass after fix)
    ;; ────────────────────────────────────────────────────────
    (test-case "sandbox-enabled? works with real q-settings (default)"
      (define settings (q-settings (hash) (hash) (hash)))
      (check-true (sandbox-enabled? settings)
                  "sandbox-enabled? should return #t with empty settings (default)"))

    (test-case "sandbox-enabled? reads nested tools.use-sandbox"
      (define settings (q-settings (hash) (hash) (hash 'tools (hash 'use-sandbox #f))))
      (check-false (sandbox-enabled? settings)
                   "sandbox-enabled? should return #f when tools.use-sandbox is #f"))

    (test-case "sandbox-timeout reads nested tools.sandbox-timeout"
      (define settings (q-settings (hash) (hash) (hash 'tools (hash 'sandbox-timeout 60))))
      (check-equal? (sandbox-timeout settings) 60))

    (test-case "exec-context can carry q-settings as runtime-settings"
      (define settings (q-settings (hash) (hash) (hash)))
      (define ctx (make-exec-context #:runtime-settings settings))
      (check-true (q-settings? (exec-context-runtime-settings ctx))
                  "exec-context-runtime-settings should return q-settings? when set"))

    (test-case "bash tool does not crash with q-settings in exec-context"
      (define settings (q-settings (hash) (hash) (hash)))
      (define ctx (make-exec-context #:runtime-settings settings))
      (define result (tool-bash (hasheq 'command "echo hello") ctx))
      ;; The tool should succeed — no contract violation
      (check-false (and (hash? result)
                        (hash-has-key? result 'error)
                        (let ([text (hash-ref result 'text "")])
                          (string-contains? text "contract violation")))
                   "bash tool should not crash with contract violation when given q-settings"))

    (test-case "setting-ref reads default-provider from q-settings"
      (define settings (q-settings (hash) (hash) (hash 'default-provider "openai")))
      (check-equal? (setting-ref settings 'default-provider #f) "openai"))

    (test-case "setting-ref reads default-model from q-settings"
      (define settings (q-settings (hash) (hash) (hash 'default-model "gpt-4o")))
      (check-equal? (setting-ref settings 'default-model #f) "gpt-4o"))

    (test-case "make-minimal-settings creates q-settings struct"
      (define s (make-minimal-settings #:provider "test-provider" #:model "test-model"))
      (check-pred q-settings? s)
      (check-equal? (setting-ref s 'default-provider #f) "test-provider")
      (check-equal? (setting-ref s 'default-model #f) "test-model"))

    (test-case "make-minimal-settings with no args creates empty settings"
      (define s (make-minimal-settings))
      (check-pred q-settings? s)
      (check-equal? (setting-ref s 'default-provider #f) #f)
      (check-equal? (setting-ref s 'default-model #f) #f))))

(run-tests suite)
