#lang racket

;; test-config-flow.rkt — Settings config flow contract tests
;;
;; Verifies that settings struct flows correctly through the SDK
;; runtime to exec-context.  This would have caught the settings
;; contract violation bug (where iteration.rkt passed a plain hash
;; instead of q-settings).

(require rackunit
         rackunit/text-ui
         "../../../runtime/settings.rkt"
         "../../../tools/tool.rkt"
         "../../../agent/event-bus.rkt")

;; ============================================================
;; Suite
;; ============================================================

(define suite
  (test-suite
   "config-flow: settings struct flow through SDK runtime"

   ;; 1. make-minimal-settings creates valid q-settings
   (test-case
    "config-flow: make-minimal-settings creates valid q-settings"
    (define s (make-minimal-settings #:provider "openai" #:model "gpt-4"))
    (check-true (q-settings? s) "result must be a q-settings struct")
    (check-equal? (setting-ref s 'default-provider) "openai"
                  "default-provider should be openai")
    (check-equal? (setting-ref s 'default-model) "gpt-4"
                  "default-model should be gpt-4"))

   ;; 2. make-minimal-settings with overrides
   (test-case
    "config-flow: make-minimal-settings with overrides"
    (define s (make-minimal-settings
               #:provider "anthropic"
               #:overrides (hash 'tools (hash 'use-sandbox #f))))
    (check-true (q-settings? s))
    (check-false (sandbox-enabled? s)
                 "sandbox should be disabled via overrides"))

   ;; 3. exec-context stores q-settings as runtime-settings
   (test-case
    "config-flow: exec-context stores q-settings as runtime-settings"
    (define s (make-minimal-settings #:provider "openai" #:model "gpt-4"))
    (define ctx (make-exec-context #:runtime-settings s))
    (check-true (exec-context? ctx) "result must be an exec-context")
    (define extracted (exec-context-runtime-settings ctx))
    (check-true (q-settings? extracted)
                "runtime-settings must be a q-settings struct")
    (check-eq? extracted s
               "exec-context must return the exact same q-settings struct"))

   ;; 4. sandbox-enabled? works through exec-context
   (test-case
    "config-flow: sandbox-enabled? works through exec-context"
    (define s (make-minimal-settings
               #:overrides (hash 'tools (hash 'use-sandbox #f))))
    (define ctx (make-exec-context #:runtime-settings s))
    (define extracted (exec-context-runtime-settings ctx))
    (check-false (sandbox-enabled? extracted)
                 "sandbox should be #f through exec-context chain"))

   ;; 5. sandbox-timeout reads through exec-context
   (test-case
    "config-flow: sandbox-timeout reads through exec-context"
    (define s (make-minimal-settings
               #:overrides (hash 'tools (hash 'sandbox-timeout 60))))
    (define ctx (make-exec-context #:runtime-settings s))
    (define extracted (exec-context-runtime-settings ctx))
    (check-equal? (sandbox-timeout extracted) 60
                  "sandbox-timeout should be 60 through exec-context chain"))

   ;; 6. merged hash preserves all keys
   (test-case
    "config-flow: merged hash preserves all keys"
    (define global-hash (hash 'default-provider "openai"
                              'logging (hash 'level "info")))
    (define project-hash (hash 'default-model "gpt-4"
                               'logging (hash 'level "debug")))
    (define merged (merge-settings global-hash project-hash))
    ;; Project overrides global, so logging.level should be "debug"
    (check-equal? (hash-ref merged 'default-provider) "openai"
                  "global key should be present in merged")
    (check-equal? (hash-ref merged 'default-model) "gpt-4"
                  "project key should be present in merged")
    (check-equal? (hash-ref (hash-ref merged 'logging) 'level) "debug"
                  "project should override global for nested keys"))

   ;; 7. setting-ref returns default for missing keys
   (test-case
    "config-flow: setting-ref returns default for missing keys"
    (define s (make-minimal-settings #:provider "openai"))
    (check-equal? (setting-ref s 'nonexistent-key) #f
                  "missing key should return #f by default")
    (check-equal? (setting-ref s 'nonexistent-key "fallback")
                  "fallback"
                  "missing key should return provided default"))

   ))

;; ============================================================
;; Run
;; ============================================================

(run-tests suite)
