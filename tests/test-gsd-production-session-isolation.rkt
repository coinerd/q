#lang racket

;; @speed fast  ;; @suite runtime

;; BOUNDARY: integration
;; CHARACTERIZATION TEST — documents current state for v0.57.1 remediation

;; tests/test-gsd-production-session-isolation.rkt
;; Pins current production GSD state leakage patterns.
;; These tests document WHAT IS (not what should be).
;; When v0.57.1 fixes these, some tests will flip from "leak observed" to "isolated".

(require rackunit
         rackunit/text-ui
         (only-in "../extensions/gsd/runtime-state-types.rkt"
                  gsd-runtime-state
                  gsd-runtime-state-mode
                  gsd-runtime-state-current-wave
                  make-initial-gsd-state)
         (only-in "../extensions/gsd/session-state.rkt"
                  make-gsd-context
                  gsd-default-ctx
                  gsd-ctx-mode
                  gsd-ctx-state
                  gsd-ctx-set-state!
                  gsd-ctx-history
                  gsd-ctx-set-history!
                  gsd-ctx-plan
                  gsd-ctx-set-plan!
                  gsd-ctx-pinned-dir
                  gsd-ctx-set-pinned-dir!
                  gsd-ctx-event-bus
                  gsd-ctx-set-event-bus!
                  gsd-session-ctx-state-box
                  gsd-session-ctx-plan-box
                  gsd-session-ctx-history-box
                  gsd-session-ctx-pinned-dir-box
                  gsd-session-ctx-event-bus-box
                  gsd-session-ctx-sem
                  current-gsd-state
                  set-gsd-state!
                  current-gsd-mode
                  current-gsd-history
                  set-gsd-history!
                  current-plan-data
                  set-plan-data!
                  current-gsd-event-bus
                  set-gsd-event-bus!)
         (only-in "../extensions/gsd/state-machine.rkt" gsm-transition! gsm-current gsm-reset!))

;; ── Helpers ──

(define (save-gsd-default-state)
  (list (unbox (gsd-session-ctx-state-box gsd-default-ctx))
        (unbox (gsd-session-ctx-plan-box gsd-default-ctx))
        (unbox (gsd-session-ctx-history-box gsd-default-ctx))
        (unbox (gsd-session-ctx-pinned-dir-box gsd-default-ctx))
        (unbox (gsd-session-ctx-event-bus-box gsd-default-ctx))))

(define (restore-gsd-default-state! saved)
  (set-box! (gsd-session-ctx-state-box gsd-default-ctx) (first saved))
  (set-box! (gsd-session-ctx-plan-box gsd-default-ctx) (second saved))
  (set-box! (gsd-session-ctx-history-box gsd-default-ctx) (third saved))
  (set-box! (gsd-session-ctx-pinned-dir-box gsd-default-ctx) (fourth saved))
  (set-box! (gsd-session-ctx-event-bus-box gsd-default-ctx) (fifth saved)))

(define saved-state (save-gsd-default-state))

;; ── Characterization Tests ──

(define gsd-production-isolation-suite
  (test-suite "gsd-production-session-isolation-characterization"

    ;; P1: gsd-default-ctx is a module-level global
    (test-case "CHAR: gsd-default-ctx is shared singleton"
      ;; Any code using current-gsd-state/set-gsd-state! operates on gsd-default-ctx
      (set-gsd-state! (struct-copy gsd-runtime-state (current-gsd-state) [mode 'exploring]))
      (check-equal? (gsd-ctx-mode gsd-default-ctx)
                    'exploring
                    "set-gsd-state! mutates gsd-default-ctx")
      ;; Cleanup
      (set-gsd-state! (struct-copy gsd-runtime-state (current-gsd-state) [mode 'idle])))

    ;; P2: state-machine gsm-transition! operates on gsd-default-ctx
    (test-case "CHAR: gsm-transition! uses gsd-default-ctx global"
      ;; gsm-transition! reads/writes gsd-default-ctx directly (lines 187-196 of state-machine.rkt)
      ;; This means two concurrent sessions cannot have independent GSD modes
      (gsm-reset!)
      (gsm-transition! 'exploring)
      (check-equal? (gsd-ctx-mode gsd-default-ctx)
                    'exploring
                    "gsm-transition! mutates gsd-default-ctx")
      (check-equal? (gsm-current) 'exploring "gsm-current reads from gsd-default-ctx")
      (gsm-reset!))

    ;; P3: current-gsd-state convenience accessor shares global
    (test-case "CHAR: current-gsd-state is gsd-default-ctx accessor"
      (define fresh-ctx (make-gsd-context))
      (gsd-ctx-set-state! fresh-ctx
                          (struct-copy gsd-runtime-state (gsd-ctx-state fresh-ctx) [mode 'verifying]))
      ;; fresh-ctx has mode 'verifying
      (check-equal? (gsd-ctx-mode fresh-ctx) 'verifying)
      ;; But current-gsd-mode still reads from gsd-default-ctx
      ;; This proves that fresh-ctx is NOT connected to the current-gsd-* API
      (check-not-equal? (current-gsd-mode)
                        'verifying
                        "current-gsd-mode does NOT read from fresh-ctx"))

    ;; P4: event-bus is shared via gsd-default-ctx
    (test-case "CHAR: event-bus shared through gsd-default-ctx"
      (define test-bus 'fake-event-bus-for-test)
      (set-gsd-event-bus! test-bus)
      (check-eq? (gsd-ctx-event-bus gsd-default-ctx)
                 test-bus
                 "set-gsd-event-bus! writes gsd-default-ctx")
      ;; Fresh context has no event bus
      (define fresh-ctx (make-gsd-context))
      (check-false (gsd-ctx-event-bus fresh-ctx)
                   "fresh ctx has no event bus — must be explicitly set"))

    ;; P5: pinned-dir is shared via gsd-default-ctx
    (test-case "CHAR: pinned-dir shared through gsd-default-ctx"
      (set-gsd-event-bus! #f) ; cleanup from P4
      (define test-dir "/tmp/test-pinned")
      (gsd-ctx-set-pinned-dir! gsd-default-ctx test-dir)
      (check-equal? (gsd-ctx-pinned-dir gsd-default-ctx) test-dir)
      ;; Fresh context has no pinned dir
      (define fresh-ctx (make-gsd-context))
      (check-false (gsd-ctx-pinned-dir fresh-ctx) "fresh ctx starts with no pinned dir"))

    ;; P6: Independent contexts ARE isolated (proves the API supports isolation)
    (test-case "CHAR: independent make-gsd-context instances are properly isolated"
      (define ctx-a (make-gsd-context))
      (define ctx-b (make-gsd-context))
      ;; Modify ctx-a extensively
      (gsd-ctx-set-state! ctx-a
                          (struct-copy gsd-runtime-state (gsd-ctx-state ctx-a) [mode 'executing]))
      (gsd-ctx-set-plan! ctx-a '(("wave 0" "in-progress")))
      (gsd-ctx-set-history! ctx-a '((idle exploring 100) (exploring executing 200)))
      ;; ctx-b should be unaffected
      (check-equal? (gsd-ctx-mode ctx-b) 'idle)
      (check-false (gsd-ctx-plan ctx-b))
      (check-equal? (gsd-ctx-history ctx-b) '()))

    ;; P7: Document call sites that need migration in v0.57.1
    (test-case "CHAR: document global-dependent call sites for v0.57.1"
      ;; Production modules that directly access gsd-default-ctx or current-gsd-*:
      ;;   - extensions/gsd/state-machine.rkt (lines 187, 196, 230, 232, 240)
      ;;   - extensions/gsd/command-handlers.rkt (line 56: set-gsd-state!)
      ;;   - extensions/gsd-planning.rkt (line 57: set-gsd-state!)
      ;;   - tui/tui-init.rkt (line 73: gsm-current via current-gsd-mode-query)
      ;;
      ;; v0.57.1 should: inject ctx parameter, deprecate globals, add no-new-globals lint
      (check-true #t "documentation placeholder — see comments above"))

    ;; ── Teardown ──
    (test-case "cleanup: restore gsd-default-ctx"
      (restore-gsd-default-state! saved-state)
      (check-true #t "restored"))))

(run-tests gsd-production-isolation-suite)
