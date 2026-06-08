#lang racket

;; @speed fast
;; @suite default

;; q/tests/test-ui-theme-layout.rkt — Tests for ui-core/theme-protocol and layout-protocol

(require rackunit
         rackunit/text-ui
         "../ui-core/theme-protocol.rkt"
         "../ui-core/layout-protocol.rkt")

(define-test-suite
 test-ui-theme-layout
 ;; ── Theme ──
 (test-case "default-theme has all colors"
   (define t (default-theme))
   (check-equal? (theme-ref t 'background) "#1e1e2e")
   (check-equal? (theme-ref t 'foreground) "#cdd6f4")
   (check-equal? (theme-ref t 'accent) "#89b4fa")
   (check-equal? (theme-ref t 'error) "#f38ba8")
   (check-equal? (theme-ref t 'success) "#a6e3a1")
   (check-equal? (theme-ref t 'font-family) "monospace")
   (check-equal? (theme-ref t 'font-size) 14))
 (test-case "theme-ref returns #f for unknown key"
   (define t (default-theme))
   (check-false (theme-ref t 'nonexistent)))
 (test-case "theme-merge: override takes precedence"
   (define base (default-theme))
   (define override (make-ui-theme #:background "#ffffff" #:error "#ff0000"))
   (define merged (theme-merge base override))
   (check-equal? (theme-ref merged 'background) "#ffffff")
   (check-equal? (theme-ref merged 'error) "#ff0000")
   (check-equal? (theme-ref merged 'foreground) "#cdd6f4"))
 ;; ── Layout ──
 (test-case "default-gui-layout has reasonable defaults"
   (define l (default-gui-layout))
   (check-equal? (gui-layout-width l) 120)
   (check-equal? (gui-layout-height l) 40)
   (check-equal? (gui-layout-min-width l) 80)
   (check-equal? (gui-layout-min-height l) 24))
 (test-case "layout-content-area computes correctly"
   (define l (default-gui-layout))
   (define-values (w h) (layout-content-area l))
   (check-equal? w 120)
   (check-equal? h 36))
 (test-case "layout with sidebar reduces content width"
   (define l
     (make-gui-layout #:width 160 #:height 50 #:sidebar-width 30 #:status-height 2 #:input-height 5))
   (define-values (w h) (layout-content-area l))
   (check-equal? w 130)
   (check-equal? h 43)))

(run-tests test-ui-theme-layout)
