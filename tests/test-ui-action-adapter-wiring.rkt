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
         (only-in "../ui-core/ui-delta.rkt" ui-delta DELTA-SET-THEME DELTA-SET-STATUS)
         ;; Theme parameter for TUI
         "../tui/theme.rkt")

;; ═══════════════════════════════════════════════════════════
;; GUI Theme Adapter Tests
;; ═══════════════════════════════════════════════════════════
(define-test-suite
 gui-theme-adapter-tests
 ;; M-2 FIX: Test actual tm-switch-theme! call with mock theme-manager
 (test-case "G-TM1: GUI set-theme calls tm-switch-theme! when manager is available"
   (define tm (make-theme-manager))
   (define orig-mgr (current-gui-theme-manager))
   (current-gui-theme-manager tm)
   (define state (make-gui-state))
   (define delta (ui-delta DELTA-SET-THEME 'dark))
   (define result (gui-apply-delta delta state))
   ;; Theme manager should have switched to 'dark
   (check-equal? (tm-current-theme tm) (tm-current-theme (tm-switch-theme! tm 'dark)))
   ;; State should be returned unchanged (theme mutation is side-effect on parameter)
   (check-equal? (gui-state-status result) (gui-state-status state))
   ;; Restore
   (current-gui-theme-manager orig-mgr))
 (test-case "G-TM1: GUI set-theme with #f manager doesn't crash"
   (define orig-mgr (current-gui-theme-manager))
   (current-gui-theme-manager #f)
   (define state (make-gui-state))
   (define delta (ui-delta DELTA-SET-THEME 'dark))
   (check-not-exn (lambda () (gui-apply-delta delta state)))
   (current-gui-theme-manager orig-mgr))
 (test-case "G-TM1: GUI set-theme with non-symbol payload doesn't crash"
   (define state (make-gui-state))
   (define delta (ui-delta DELTA-SET-THEME "not-a-symbol"))
   (check-not-exn (lambda () (gui-apply-delta delta state)))))

;; ═══════════════════════════════════════════════════════════
;; TUI Theme Adapter Tests
;; ═══════════════════════════════════════════════════════════
(define-test-suite tui-theme-adapter-tests
                   (test-case "G-TM2: TUI set-theme delta updates current-tui-theme parameter"
                     (define state (initial-ui-state))
                     (define original-theme (current-tui-theme))
                     (define delta (ui-delta DELTA-SET-THEME 'dark))
                     (define result (tui-apply-delta delta state))
                     (check-equal? (current-tui-theme) (hash-ref registered-themes 'dark #f))
                     (current-tui-theme original-theme))
                   (test-case "G-TM2: TUI set-theme with unknown name doesn't crash"
                     (define state (initial-ui-state))
                     (define original-theme (current-tui-theme))
                     (define delta (ui-delta DELTA-SET-THEME 'nonexistent-theme-xyz))
                     (check-not-exn (lambda () (tui-apply-delta delta state)))
                     (check-equal? (current-tui-theme) original-theme))
                   ;; M-3 FIX: Non-symbol payload test (was missing)
                   (test-case "G-TM2: TUI set-theme with non-symbol payload doesn't crash"
                     (define state (initial-ui-state))
                     (define original-theme (current-tui-theme))
                     (define delta (ui-delta DELTA-SET-THEME "not-a-symbol"))
                     (check-not-exn (lambda () (tui-apply-delta delta state)))
                     (check-equal? (current-tui-theme) original-theme)))

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

(run-tests (test-suite "ui-action-adapter-wiring"
             gui-theme-adapter-tests
             tui-theme-adapter-tests
             tui-status-adapter-tests))
