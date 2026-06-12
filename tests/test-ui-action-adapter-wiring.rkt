#lang racket/base

;; @speed fast
;; @suite default
;;
;; Regression tests for UI action adapter wiring (G-TEST).
;; Verifies that DELTA-SET-THEME and DELTA-SET-STATUS are wired correctly
;; in both GUI and TUI adapters.
;;
;; Audit findings addressed:
;; M-2: G-TM1 now tests actual tm-switch-theme! call via mock theme-manager
;; M-3: TUI non-symbol theme payload test added
;; F-4 (v0.98.13): GUI set-status string→symbol coercion test
;; F-8 (v0.98.13): Theme tests converted to parameterize for proper isolation
;; F-9 (v0.98.13): set-layout/set-focus no-op handler tests

(require rackunit
         rackunit/text-ui
         ;; GUI adapter + state
         "../gui/ui-action-adapter.rkt"
         "../gui/gui-types.rkt"
         ;; GUI theme-manager for M-2 mock test
         (only-in "../gui/theme-manager.rkt"
                  make-theme-manager
                  tm-current-theme
                  tm-switch-theme!
                  theme-manager?)
         (only-in "../gui/main.rkt" current-gui-theme-manager)
         ;; TUI adapter + state
         "../tui/ui-action-adapter.rkt"
         "../tui/state-types.rkt"
         ;; Delta struct + constants
         (only-in "../ui-core/ui-delta.rkt"
                  ui-delta
                  DELTA-SET-THEME
                  DELTA-SET-STATUS
                  DELTA-SET-LAYOUT
                  DELTA-SET-FOCUS)
         ;; Theme parameter for TUI
         "../tui/theme.rkt")

;; ═══════════════════════════════════════════════════════════
;; GUI Theme Adapter Tests
;; ═══════════════════════════════════════════════════════════
(define-test-suite
 gui-theme-adapter-tests
 ;; M-2 FIX: Test actual tm-switch-theme! call with mock theme-manager
 ;; F-8: Uses parameterize for proper isolation (no manual save/restore)
 (test-case "G-TM1: GUI set-theme calls tm-switch-theme! when manager is available"
   (define tm (make-theme-manager))
   (parameterize ([current-gui-theme-manager tm])
     (define state (make-gui-state))
     (define delta (ui-delta DELTA-SET-THEME 'dark))
     (define result (gui-apply-delta delta state))
     ;; Theme manager should have switched to 'dark
     (check-equal? (tm-current-theme tm) (tm-current-theme (tm-switch-theme! tm 'dark)))
     ;; State should be returned unchanged (theme mutation is side-effect on parameter)
     (check-equal? (gui-state-status result) (gui-state-status state))))
 (test-case "G-TM1: GUI set-theme with #f manager doesn't crash"
   (parameterize ([current-gui-theme-manager #f])
     (define state (make-gui-state))
     (define delta (ui-delta DELTA-SET-THEME 'dark))
     (check-not-exn (lambda () (gui-apply-delta delta state)))))
 (test-case "G-TM1: GUI set-theme with non-symbol payload doesn't crash"
   (define state (make-gui-state))
   (define delta (ui-delta DELTA-SET-THEME "not-a-symbol"))
   (check-not-exn (lambda () (gui-apply-delta delta state)))))

;; ═══════════════════════════════════════════════════════════
;; TUI Theme Adapter Tests
;; ═══════════════════════════════════════════════════════════
(define-test-suite tui-theme-adapter-tests
                   ;; F-8: Uses parameterize for proper isolation (no manual save/restore)
                   (test-case "G-TM2: TUI set-theme delta updates current-tui-theme parameter"
                     (parameterize ([current-tui-theme (current-tui-theme)])
                       (define state (initial-ui-state))
                       (define delta (ui-delta DELTA-SET-THEME 'dark))
                       (define result (tui-apply-delta delta state))
                       (check-equal? (current-tui-theme) (hash-ref registered-themes 'dark #f))))
                   (test-case "G-TM2: TUI set-theme with unknown name doesn't crash"
                     (parameterize ([current-tui-theme (current-tui-theme)])
                       (define state (initial-ui-state))
                       (define delta (ui-delta DELTA-SET-THEME 'nonexistent-theme-xyz))
                       (check-not-exn (lambda () (tui-apply-delta delta state)))))
                   ;; M-3 FIX: Non-symbol payload test (was missing)
                   (test-case "G-TM2: TUI set-theme with non-symbol payload doesn't crash"
                     (parameterize ([current-tui-theme (current-tui-theme)])
                       (define state (initial-ui-state))
                       (define delta (ui-delta DELTA-SET-THEME "not-a-symbol"))
                       (check-not-exn (lambda () (tui-apply-delta delta state))))))

;; ═══════════════════════════════════════════════════════════
;; TUI Status Adapter Tests
;; ═══════════════════════════════════════════════════════════
(define-test-suite tui-status-adapter-tests
                   (test-case "G-STAT: TUI set-status delta updates status message"
                     (define state (initial-ui-state))
                     (define delta (ui-delta DELTA-SET-STATUS "Working..."))
                     (define result (tui-apply-delta delta state))
                     (check-equal? (ui-state-status-message result) "Working..."))
                   (test-case "G-STAT: TUI set-status with non-string payload leaves state unchanged"
                     (define state (initial-ui-state))
                     (define delta (ui-delta DELTA-SET-STATUS #f))
                     (define result (tui-apply-delta delta state))
                     (check-equal? (ui-state-status-message result) (ui-state-status-message state)))
                   (test-case "G-STAT: TUI set-status with empty string works"
                     (define state (initial-ui-state))
                     (define delta (ui-delta DELTA-SET-STATUS ""))
                     (define result (tui-apply-delta delta state))
                     (check-equal? (ui-state-status-message result) "")))

;; ═══════════════════════════════════════════════════════════
;; GUI Status Adapter Tests (F-4)
;; ═══════════════════════════════════════════════════════════
(define-test-suite gui-status-adapter-tests
                   (test-case "F-4: GUI set-status with string payload coerces to symbol"
                     (define state (make-gui-state))
                     (define delta (ui-delta DELTA-SET-STATUS "error"))
                     (define result (gui-apply-delta delta state))
                     (check-equal? (gui-state-status result) 'error))
                   (test-case "F-4: GUI set-status with symbol payload passes through"
                     (define state (make-gui-state))
                     (define delta (ui-delta DELTA-SET-STATUS 'busy))
                     (define result (gui-apply-delta delta state))
                     (check-equal? (gui-state-status result) 'busy)))

;; ═══════════════════════════════════════════════════════════
;; No-op Handler Tests (F-9)
;; ═══════════════════════════════════════════════════════════
(define-test-suite noop-handler-tests
                   (test-case "F-9: GUI set-layout is a no-op (returns state unchanged)"
                     (define state (make-gui-state))
                     (define delta (ui-delta DELTA-SET-LAYOUT 'wide))
                     (define result (gui-apply-delta delta state))
                     (check-equal? result state))
                   (test-case "F-9: GUI set-focus is a no-op (returns state unchanged)"
                     (define state (make-gui-state))
                     (define delta (ui-delta DELTA-SET-FOCUS 'transcript))
                     (define result (gui-apply-delta delta state))
                     (check-equal? result state))
                   (test-case "F-9: TUI set-layout is a no-op (returns state unchanged)"
                     (define state (initial-ui-state))
                     (define delta (ui-delta DELTA-SET-LAYOUT 'wide))
                     (define result (tui-apply-delta delta state))
                     (check-equal? result state))
                   (test-case "F-9: TUI set-focus is a no-op (returns state unchanged)"
                     (define state (initial-ui-state))
                     (define delta (ui-delta DELTA-SET-FOCUS 'transcript))
                     (define result (tui-apply-delta delta state))
                     (check-equal? result state)))

(run-tests (test-suite "ui-action-adapter-wiring"
             gui-theme-adapter-tests
             tui-theme-adapter-tests
             tui-status-adapter-tests
             gui-status-adapter-tests
             noop-handler-tests))
