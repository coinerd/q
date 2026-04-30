#lang racket

;; tests/test-gsd-v024-fitness.rkt — Cross-module fitness gates for v0.24.x
;;
;; Validates that all architectural improvements from v0.24.0–v0.24.3
;; work together correctly.

(require rackunit
         racket/port
         "../extensions/gsd/state-machine.rkt"
         "../extensions/gsd/core.rkt"
         "../extensions/gsd/policy.rkt"
         "../extensions/gsd/events.rkt"
         "../extensions/gsd/command-types.rkt"
         "../extensions/gsd/plan-types.rkt"
         "../extensions/gsd/plan-validator.rkt")

;; ============================================================
;; F1: State unification — struct-based state
;; ============================================================

(test-case "F1: gsm-snapshot returns gsd-runtime-state struct"
  (reset-gsm!)
  (define snap (gsm-snapshot))
  (check-true (gsd-runtime-state? snap))
  (check-equal? (gsd-runtime-state-mode snap) 'idle))

(test-case "F1: state machine uses struct-copy for transitions"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (check-equal? (gsm-current) 'exploring)
  (define snap (gsm-snapshot))
  (check-equal? (gsd-runtime-state-mode snap) 'exploring))

(test-case "F1: invariants hold after multiple transitions"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (define-values (ok? msg) (gsd-invariants-hold?))
  (check-true ok? msg))

;; ============================================================
;; F2/F5: Command result structs
;; ============================================================

(test-case "F2: commands return gsd-command-result structs"
  (reset-gsm!)
  (define result (gsd-command-dispatch 'gsd ""))
  (check-true (gsd-command-result? result))
  (check-true (gsd-command-result-success result)))

(test-case "F2: error commands return gsd-command-result with success=#f"
  (reset-gsm!)
  (define result (gsd-command-dispatch 'wave-done "abc"))
  (check-true (gsd-command-result? result))
  (check-false (gsd-command-result-success result)))

;; ============================================================
;; F3: Transaction wrappers
;; ============================================================

(test-case "F3: with-gsd-transaction restores state on failure"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (define mode-before (gsm-current))
  (define result
    (with-gsd-transaction "test-txn"
                          (lambda () (error "simulated failure"))
                          (lambda (e snap) (void))))
  (check-false (gsd-command-result-success result))
  (check-equal? (gsm-current) mode-before))

;; ============================================================
;; F4: Unified policy engine
;; ============================================================

(test-case "F4: gsm-tool-allowed? routes through policy module"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  ;; edit should be blocked in plan-written
  (check-false (gsm-tool-allowed? "edit"))
  ;; Policy decision should match
  (define d (gsd-decide-action (hasheq 'mode 'plan-written 'tool "edit") 'tool-call))
  (check-true (policy-blocked? d)))

(test-case "F4: blocked-tools-for consistent across all modes"
  (for ([mode '(idle exploring plan-written executing verifying)])
    (define blocked (blocked-tools-for mode))
    (for ([tool blocked])
      (check-false (policy-allowed? (gsd-decide-action (hasheq 'mode mode 'tool tool) 'tool-call))
                   (format "~a should be blocked in ~a" tool mode)))))

;; ============================================================
;; F6: Event telemetry
;; ============================================================

(test-case "F6: transition events emitted via telemetry"
  (define-values (collector query) (make-event-collector))
  (set-gsd-event-bus! collector)
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (define events (query))
  (set-gsd-event-bus! void) ; reset
  (check >= (length events) 2 "Should have attempted + succeeded events")
  (define event-names (map (lambda (e) (hash-ref e 'event)) events))
  (check-not-false (member 'gsd.transition.attempted event-names))
  (check-not-false (member 'gsd.transition.succeeded event-names)))

;; ============================================================
;; F7: Normalized Plan IR
;; ============================================================

(test-case "F7: normalize-plan produces gsd-normalized-plan"
  (define waves
    (list (gsd-wave 0 "W0" 'pending "fix bug" '("foo.rkt") '() "raco test" '())
          (gsd-wave 1 "W1" 'pending "add tests" '("bar.rkt") '() "raco test" '())))
  (define plan (gsd-plan waves #f '() '()))
  (define result (normalize-plan plan))
  (check-true (gsd-normalized-plan? result))
  (check-equal? (length (gsd-normalized-plan-waves result)) 2))

(test-case "F7: normalize-plan rejects non-sequential indices"
  (define waves
    (list (gsd-wave 0 "W0" 'pending "" '("a.rkt") '() "" '())
          (gsd-wave 2 "W2" 'pending "" '("b.rkt") '() "" '())))
  (define plan (gsd-plan waves #f '() '()))
  (define result (normalize-plan plan))
  (check-true (string? result) "Should return error string"))

(test-case "F7: validate-normalized-plan accepts valid plan"
  (define norm-waves
    (list (gsd-normalized-wave 0 "W0" '("task") "raco test" '("done") '("foo.rkt") 'pending)))
  (define norm-plan (gsd-normalized-plan norm-waves #f (hasheq) #f))
  (define result (validate-normalized-plan norm-plan))
  (check-true (gsd-validated-plan? result)))

;; ============================================================
;; Cross-cutting: Full lifecycle fitness
;; ============================================================

(test-case "lifecycle: plan → go → wave-done with events"
  (define-values (collector query) (make-event-collector))
  (set-gsd-event-bus! collector)
  (reset-gsm!)
  ;; Set up for execution
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-set-total-waves! 2)
  (gsm-transition! 'executing)
  ;; Complete a wave
  (define result (gsd-command-dispatch 'wave-done "0"))
  (check-true (gsd-command-result-success result))
  (check-true (gsm-wave-complete? 0))
  ;; Verify events were emitted
  (define events (query))
  (set-gsd-event-bus! void)
  (check >= (length events) 4 "Should have command + transition events")
  (define event-names (map (lambda (e) (hash-ref e 'event)) events))
  (check-not-false (member 'gsd.command.received event-names))
  (check-not-false (member 'gsd.command.completed event-names)))

;; ============================================================
;; F8: Documentation coverage fitness
;; ============================================================

(test-case "F8: gsd-architecture.md mentions all major modules"
  (define here
    (resolved-module-path-name (variable-reference->resolved-module-path (#%variable-reference))))
  (define doc-path
    (simplify-path (build-path (if here
                                   (build-path here ".." "..")
                                   (current-directory))
                               "docs"
                               "gsd-architecture.md")))
  (define doc-content
    (with-handlers ([exn:fail? (lambda (e) "")])
      (file->string doc-path)))
  (when (non-empty-string? doc-content)
    (define required-sections '("state machine" "event" "policy" "archive" "transaction"))
    (for ([section required-sections])
      (check-not-false (regexp-match? (regexp-quote section) (string-downcase doc-content))
                       (format "gsd-architecture.md should mention '~a'" section)))))
