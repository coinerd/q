#lang racket

;; @speed fast
;; @suite default

;; q/tests/test-gui-theme-manager.rkt — Tests for gui/theme-manager.rkt

(require rackunit
         rackunit/text-ui
         "../ui-core/theme-protocol.rkt"
         "../gui/theme-manager.rkt")

(define-test-suite test-gui-theme-manager
                   (test-case "make-theme-manager creates manager"
                     (define mgr (make-theme-manager))
                     (check-true (theme-manager? mgr)))
                   (test-case "tm-current-theme returns default"
                     (define mgr (make-theme-manager))
                     (define t (tm-current-theme mgr))
                     (check-true (ui-theme? t)))
                   (test-case "tm-list-themes includes defaults"
                     (define mgr (make-theme-manager))
                     (define names (tm-list-themes mgr))
                     (check-not-false (member 'default names))
                     (check-not-false (member 'light names))
                     (check-not-false (member 'dark names)))
                   (test-case "tm-switch-theme! switches current"
                     (define mgr (make-theme-manager))
                     (tm-switch-theme! mgr 'light)
                     (define t (tm-current-theme mgr))
                     (check-equal? (theme-ref t 'background) "#ffffff"))
                   (test-case "tm-switch-theme! ignores unknown theme"
                     (define mgr (make-theme-manager))
                     (define before (tm-current-theme mgr))
                     (tm-switch-theme! mgr 'nonexistent)
                     (check-equal? (tm-current-theme mgr) before))
                   (test-case "tm-register-theme! adds custom theme"
                     (define mgr (make-theme-manager))
                     (define custom (make-ui-theme #:background "#000000" #:foreground "#00ff00"))
                     (tm-register-theme! mgr 'matrix custom)
                     (tm-switch-theme! mgr 'matrix)
                     (check-equal? (theme-ref (tm-current-theme mgr) 'background) "#000000"))
                   (test-case "tm-customize-color modifies current theme"
                     (define mgr (make-theme-manager))
                     (tm-customize-color mgr 'accent "#ff0000")
                     (check-equal? (theme-ref (tm-current-theme mgr) 'accent) "#ff0000"))
                   (test-case "light theme has white background"
                     (define mgr (make-theme-manager))
                     (tm-switch-theme! mgr 'light)
                     (check-equal? (theme-ref (tm-current-theme mgr) 'background) "#ffffff")))

(run-tests test-gui-theme-manager)
