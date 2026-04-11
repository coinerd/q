#lang racket

;; tests/test-extension-tiers.rkt — tests for extensions/tiers.rkt
;;
;; Covers:
;;   1. tier? predicate — accepts 5 valid tiers, rejects invalid
;;   2. tier-capabilities — returns correct cumulative lists
;;   3. capability-allowed? — checks tier grants capability
;;   4. valid-api-version? — accepts "1", rejects others
;;   5. validate-extension-tier — hook-point vs declared tier
;;   6. extension-tier-valid? — full api-version + tier check
;;   7. Unknown hook points default to hooks tier

(require rackunit
         "../extensions/api.rkt"
         "../extensions/tiers.rkt")

;; ============================================================
;; 1. tier? predicate
;; ============================================================

(test-case "tier? accepts all five valid tiers"
  (for ([t '(hooks commands session providers tui)])
    (check-pred tier? t (format "tier? should accept ~a" t))))

(test-case "tier? rejects non-tier symbols"
  (check-false (tier? 'admin))
  (check-false (tier? 'root))
  (check-false (tier? 'superuser)))

(test-case "tier? rejects non-symbols"
  (check-false (tier? "hooks"))
  (check-false (tier? 42))
  (check-false (tier? '(hooks)))
  (check-false (tier? #f)))

;; ============================================================
;; 2. tier-capabilities
;; ============================================================

(test-case "tier-capabilities: hooks has hook-dispatch only"
  (check-equal? (tier-capabilities 'hooks) '(hook-dispatch)))

(test-case "tier-capabilities: commands includes hook-dispatch and command-register"
  (check-equal? (tier-capabilities 'commands) '(hook-dispatch command-register)))

(test-case "tier-capabilities: session adds session-lifecycle and compaction-hooks"
  (check-equal? (tier-capabilities 'session)
                '(hook-dispatch command-register session-lifecycle compaction-hooks)))

(test-case "tier-capabilities: providers adds provider-register"
  (check-equal? (tier-capabilities 'providers)
                '(hook-dispatch command-register session-lifecycle compaction-hooks provider-register)))

(test-case "tier-capabilities: tui adds tui-panels and tui-keybindings"
  (check-equal? (tier-capabilities 'tui)
                '(hook-dispatch command-register session-lifecycle compaction-hooks
                  provider-register tui-panels tui-keybindings)))

(test-case "tier-capabilities rejects invalid tier"
  (check-exn exn:fail?
    (λ () (tier-capabilities 'invalid))))

;; ============================================================
;; 3. capability-allowed?
;; ============================================================

(test-case "capability-allowed?: hooks tier allows hook-dispatch"
  (check-true (capability-allowed? 'hooks 'hook-dispatch)))

(test-case "capability-allowed?: hooks tier denies command-register"
  (check-false (capability-allowed? 'hooks 'command-register)))

(test-case "capability-allowed?: commands tier allows command-register"
  (check-true (capability-allowed? 'commands 'command-register)))

(test-case "capability-allowed?: session tier allows compaction-hooks"
  (check-true (capability-allowed? 'session 'compaction-hooks)))

(test-case "capability-allowed?: providers tier allows provider-register"
  (check-true (capability-allowed? 'providers 'provider-register)))

(test-case "capability-allowed?: tui tier allows tui-panels"
  (check-true (capability-allowed? 'tui 'tui-panels)))

(test-case "capability-allowed?: session tier denies provider-register"
  (check-false (capability-allowed? 'session 'provider-register)))

;; ============================================================
;; 4. valid-api-version?
;; ============================================================

(test-case "valid-api-version? accepts \"1\""
  (check-true (valid-api-version? "1")))

(test-case "valid-api-version? rejects \"0\""
  (check-false (valid-api-version? "0")))

(test-case "valid-api-version? rejects \"2\""
  (check-false (valid-api-version? "2")))

(test-case "valid-api-version? rejects empty string"
  (check-false (valid-api-version? "")))

(test-case "supported-api-versions is correct"
  (check-equal? supported-api-versions '("1")))

;; ============================================================
;; 5. validate-extension-tier — hook points vs declared tier
;; ============================================================

(test-case "validate-extension-tier: hook-only extension at hooks tier passes"
  (define ext (extension "simple" "1.0" "1"
                 (hasheq 'context-assembly (λ (p) p)
                         'tool-pre-exec (λ (p) p))))
  (check-eq? (validate-extension-tier ext 'hooks) #t))

(test-case "validate-extension-tier: session hooks at hooks tier fails"
  (define ext (extension "overreach" "1.0" "1"
                 (hasheq 'session-start (λ (p) p))))
  (define result (validate-extension-tier ext 'hooks))
  (check-pred list? result)
  (check-true (>= (length result) 1)
              "should report at least one violation"))

(test-case "validate-extension-tier: session hooks at session tier passes"
  (define ext (extension "session-ext" "1.0" "1"
                 (hasheq 'session-start (λ (p) p)
                         'pre-compact (λ (p) p)
                         'context-assembly (λ (p) p))))
  (check-eq? (validate-extension-tier ext 'session) #t))

(test-case "validate-extension-tier: tui hooks at commands tier fails"
  (define ext (extension "tui-ext" "1.0" "1"
                 (hasheq 'tui-render (λ (p) p)
                         'command-dispatch (λ (p) p))))
  (define result (validate-extension-tier ext 'commands))
  (check-pred list? result)
  (check-true (>= (length result) 1)))

(test-case "validate-extension-tier: providers hook at providers tier passes"
  (define ext (extension "prov-ext" "1.0" "1"
                 (hasheq 'provider-register (λ (p) p))))
  (check-eq? (validate-extension-tier ext 'providers) #t))

(test-case "validate-extension-tier: empty hooks always pass"
  (define ext (extension "empty" "1.0" "1" (hasheq)))
  (check-eq? (validate-extension-tier ext 'hooks) #t))

;; ============================================================
;; 6. extension-tier-valid? — full validation
;; ============================================================

(test-case "extension-tier-valid?: valid api + valid tier passes"
  (define ext (extension "good" "1.0" "1"
                 (hasheq 'context-assembly (λ (p) p))))
  (check-true (extension-tier-valid? ext 'hooks)))

(test-case "extension-tier-valid?: bad api-version fails"
  (define ext (extension "bad-api" "1.0" "99"
                 (hasheq 'context-assembly (λ (p) p))))
  (check-false (extension-tier-valid? ext 'hooks)))

(test-case "extension-tier-valid?: good api but tier violation fails"
  (define ext (extension "tier-violation" "1.0" "1"
                 (hasheq 'session-start (λ (p) p))))
  (check-false (extension-tier-valid? ext 'hooks)))

(test-case "extension-tier-valid?: tui extension at tui tier passes"
  (define ext (extension "full-ext" "1.0" "1"
                 (hasheq 'tui-panel (λ (p) p)
                         'tui-keybinding-help (λ (p) p)
                         'provider-register (λ (p) p))))
  (check-true (extension-tier-valid? ext 'tui)))

;; ============================================================
;; 7. Unknown hook points default to hooks tier
;; ============================================================

(test-case "unknown hook point defaults to hooks tier"
  (define ext (extension "unknown-hook" "1.0" "1"
                 (hasheq 'some-custom-hook (λ (p) p))))
  (check-eq? (validate-extension-tier ext 'hooks) #t))

(test-case "unknown hook point requires at least hooks tier"
  (define ext (extension "unknown-hook-bad" "1.0" "1"
                 (hasheq 'some-custom-hook (λ (p) p)
                         'session-start (λ (p) p))))
  (check-eq? (validate-extension-tier ext 'session) #t))
