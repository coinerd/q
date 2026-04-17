#lang racket

;;; tests/test-tiers.rkt — TDD tests for extensions/tiers.rkt
;;;
;;; Covers:
;;;   - Tier predicate and ordering
;;;   - Cumulative capability map
;;;   - capability-allowed? queries
;;;   - API version validation
;;;   - Hook point → tier mapping
;;;   - validate-extension-tier with violations
;;;   - extension-tier-valid? full validation

(require rackunit
         (only-in "../extensions/api.rkt"
                  extension
                  extension?
                  extension-name
                  extension-api-version
                  extension-hooks)
         (only-in "../extensions/tiers.rkt"
                  tier?
                  tier-capabilities
                  capability-allowed?
                  valid-api-version?
                  supported-api-versions
                  validate-extension-tier
                  extension-tier-valid?
                  hook-point-tier))

;; ============================================================
;; Tier predicate
;; ============================================================

(test-case "tier? returns #t for all five valid tiers"
  (check-true (tier? 'hooks))
  (check-true (tier? 'commands))
  (check-true (tier? 'session))
  (check-true (tier? 'providers))
  (check-true (tier? 'tui)))

(test-case "tier? returns #f for invalid values"
  (check-false (tier? 'invalid))
  (check-false (tier? "hooks"))
  (check-false (tier? 42))
  (check-false (tier? #f)))

;; ============================================================
;; Cumulative capabilities
;; ============================================================

(test-case "hooks tier has only hook-dispatch"
  (check-equal? (tier-capabilities 'hooks) '(hook-dispatch)))

(test-case "commands tier includes hook-dispatch + command-register"
  (check-not-false (member 'hook-dispatch (tier-capabilities 'commands)))
  (check-not-false (member 'command-register (tier-capabilities 'commands))))

(test-case "session tier includes session-lifecycle and compaction-hooks"
  (check-not-false (member 'session-lifecycle (tier-capabilities 'session)))
  (check-not-false (member 'compaction-hooks (tier-capabilities 'session))))

(test-case "providers tier includes provider-register"
  (check-not-false (member 'provider-register (tier-capabilities 'providers))))

(test-case "tui tier includes tui-panels and tui-keybindings"
  (check-not-false (member 'tui-panels (tier-capabilities 'tui)))
  (check-not-false (member 'tui-keybindings (tier-capabilities 'tui))))

(test-case "tier-capabilities raises on invalid tier"
  (check-exn exn:fail:contract?
             (lambda () (tier-capabilities 'nonexistent))))

;; ============================================================
;; capability-allowed?
;; ============================================================

(test-case "capability-allowed? returns #t for granted capabilities"
  (check-true (capability-allowed? 'hooks 'hook-dispatch))
  (check-true (capability-allowed? 'tui 'hook-dispatch))
  (check-true (capability-allowed? 'tui 'tui-panels))
  (check-true (capability-allowed? 'session 'session-lifecycle)))

(test-case "capability-allowed? returns #f for ungranted capabilities"
  (check-false (capability-allowed? 'hooks 'command-register))
  (check-false (capability-allowed? 'hooks 'tui-panels))
  (check-false (capability-allowed? 'commands 'session-lifecycle)))

;; ============================================================
;; API version validation
;; ============================================================

(test-case "valid-api-version? accepts '1'"
  (check-true (valid-api-version? "1")))

(test-case "valid-api-version? rejects unknown versions"
  (check-false (valid-api-version? "2"))
  (check-false (valid-api-version? "0"))
  (check-false (valid-api-version? #f)))

(test-case "supported-api-versions is '1'"
  (check-equal? supported-api-versions '("1")))

;; ============================================================
;; Hook point → tier mapping
;; ============================================================

(test-case "hook-point-tier maps hooks-tier hook points"
  (check-equal? (hook-point-tier 'context-assembly) 'hooks)
  (check-equal? (hook-point-tier 'tool-pre-exec) 'hooks)
  (check-equal? (hook-point-tier 'tool-post-exec) 'hooks))

(test-case "hook-point-tier maps commands-tier hook points"
  (check-equal? (hook-point-tier 'command-dispatch) 'commands)
  (check-equal? (hook-point-tier 'command-register) 'commands))

(test-case "hook-point-tier maps session-tier hook points"
  (check-equal? (hook-point-tier 'session-start) 'session)
  (check-equal? (hook-point-tier 'session-end) 'session)
  (check-equal? (hook-point-tier 'pre-compact) 'session))

(test-case "hook-point-tier maps providers-tier hook points"
  (check-equal? (hook-point-tier 'provider-register) 'providers))

(test-case "hook-point-tier maps tui-tier hook points"
  (check-equal? (hook-point-tier 'tui-panel) 'tui)
  (check-equal? (hook-point-tier 'tui-keybindings) 'tui))

(test-case "hook-point-tier defaults unknown to 'hooks"
  (check-equal? (hook-point-tier 'unknown-hook-point) 'hooks))

;; ============================================================
;; validate-extension-tier
;; ============================================================

(test-case "validate-extension-tier returns #t when hooks within declared tier"
  (define ext (extension "test" "1" "1" (hasheq 'context-assembly (lambda (d) d))))
  (check-eq? (validate-extension-tier ext 'hooks) #t))

(test-case "validate-extension-tier returns #t when hooks match exact tier"
  (define ext (extension "test" "1" "1" (hasheq 'session-start (lambda (d) d))))
  (check-eq? (validate-extension-tier ext 'session) #t))

(test-case "validate-extension-tier returns violations when hook exceeds tier"
  (define ext (extension "test" "1" "1" (hasheq 'tui-panel (lambda (d) d))))
  (define result (validate-extension-tier ext 'hooks))
  (check-true (list? result))
  (check-true (string-contains? (car result) "tui-panel")))

(test-case "validate-extension-tier returns #t when high tier covers all hooks"
  (define ext (extension "test"
                         "1"
                         "1"
                         (hasheq 'context-assembly (lambda (d) d)
                                 'session-start (lambda (d) d)
                                 'tui-panel (lambda (d) d))))
  (check-eq? (validate-extension-tier ext 'tui) #t))

(test-case "validate-extension-tier with empty hooks always valid"
  (define ext (extension "test" "1" "1" (hasheq)))
  (check-eq? (validate-extension-tier ext 'hooks) #t))

;; ============================================================
;; extension-tier-valid?
;; ============================================================

(test-case "extension-tier-valid? returns #t for valid extension"
  (define ext (extension "test" "1" "1" (hasheq 'context-assembly (lambda (d) d))))
  (check-true (extension-tier-valid? ext 'hooks)))

(test-case "extension-tier-valid? returns #f for bad API version"
  (define ext (extension "test" "1" "99" (hasheq 'context-assembly (lambda (d) d))))
  (check-false (extension-tier-valid? ext 'hooks)))

(test-case "extension-tier-valid? returns #f for tier violation"
  (define ext (extension "test" "1" "1" (hasheq 'tui-panel (lambda (d) d))))
  (check-false (extension-tier-valid? ext 'hooks)))
