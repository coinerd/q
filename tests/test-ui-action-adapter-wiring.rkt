#lang racket/base

;; @speed fast
;; @suite default
;;
;; Regression tests for UI action adapter wiring (G-TEST).
;; Verifies that DELTA-SET-THEME and DELTA-SET-STATUS are wired correctly
;; in both GUI and TUI adapters.

(require rackunit
         rackunit/text-ui
         ;; GUI adapter + state
         "../gui/ui-action-adapter.rkt"
         "../gui/gui-types.rkt"
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
 (test-case "G-TM1: GUI set-theme delta returns state unchanged (side-effect on parameter)"
   (define state (make-gui-state))
   (define delta (ui-delta DELTA-SET-THEME 'dark))
   (define result (gui-apply-delta delta state))
   ;; State should be returned unchanged — theme mutation is on parameter, not state
   (check-equal? (gui-state-status result) (gui-state-status state)))
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
                     ;; Save original theme to restore later
                     (define original-theme (current-tui-theme))
                     (define delta (ui-delta DELTA-SET-THEME 'dark))
                     (define result (tui-apply-delta delta state))
                     ;; current-tui-theme should now be the dark theme
                     (check-equal? (current-tui-theme) (hash-ref registered-themes 'dark #f))
                     ;; Restore original
                     (current-tui-theme original-theme))
                   (test-case "G-TM2: TUI set-theme with unknown name doesn't crash"
                     (define state (initial-ui-state))
                     (define original-theme (current-tui-theme))
                     (define delta (ui-delta DELTA-SET-THEME 'nonexistent-theme-xyz))
                     (check-not-exn (lambda () (tui-apply-delta delta state)))
                     ;; Theme should be unchanged since lookup returns #f
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
