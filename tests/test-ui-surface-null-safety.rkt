#lang racket

;; @speed fast
;; @suite default

;; q/tests/test-ui-surface-null-safety.rkt — Null-safety regression tests
;;
;; W1 (v0.94.8): Verify all ui-surface callbacks are null-safe and
;; ui-callbacks-installed? checks all 10 fields.

(require rackunit
         rackunit/text-ui
         (only-in "../extensions/ui-surface.rkt"
                  ui-set-header! ui-set-footer!
                  ui-clear-header! ui-clear-footer!
                  ui-make-styled-line ui-make-styled-segment
                  ui-set-status-message!
                  ui-callbacks-installed?
                  current-ui-registry
                  ui-callback-registry))

(define-test-suite
 test-ui-surface-null-safety

 ;; ── Null-safety: no crash when registry not installed (C-3, H-2) ───

 (test-case "ui-set-header! does not crash with no callbacks"
   (parameterize ([current-ui-registry (ui-callback-registry #f #f #f #f #f #f #f #f #f #f)])
     (define bx (box 'state))
     (ui-set-header! bx "test")
     (check-equal? (unbox bx) 'state)))

 (test-case "ui-set-footer! does not crash with no callbacks"
   (parameterize ([current-ui-registry (ui-callback-registry #f #f #f #f #f #f #f #f #f #f)])
     (define bx (box 'state))
     (ui-set-footer! bx "test")
     (check-equal? (unbox bx) 'state)))

 (test-case "ui-clear-header! does not crash with no callbacks"
   (parameterize ([current-ui-registry (ui-callback-registry #f #f #f #f #f #f #f #f #f #f)])
     (define bx (box 'state))
     (ui-clear-header! bx)
     (check-equal? (unbox bx) 'state)))

 (test-case "ui-clear-footer! does not crash with no callbacks"
   (parameterize ([current-ui-registry (ui-callback-registry #f #f #f #f #f #f #f #f #f #f)])
     (define bx (box 'state))
     (ui-clear-footer! bx)
     (check-equal? (unbox bx) 'state)))

 (test-case "ui-make-styled-line returns segments when no callback"
   (parameterize ([current-ui-registry (ui-callback-registry #f #f #f #f #f #f #f #f #f #f)])
     (check-equal? (ui-make-styled-line '(seg1 seg2)) '(seg1 seg2))))

 (test-case "ui-make-styled-segment returns text when no callback"
   (parameterize ([current-ui-registry (ui-callback-registry #f #f #f #f #f #f #f #f #f #f)])
     (check-equal? (ui-make-styled-segment "text" 'bold) "text")))

 (test-case "ui-set-status-message! does not crash with no callbacks"
   (parameterize ([current-ui-registry (ui-callback-registry #f #f #f #f #f #f #f #f #f #f)])
     (define bx (box 'state))
     (ui-set-status-message! bx "test")
     (check-equal? (unbox bx) 'state)))

 ;; ── ui-callbacks-installed? checks all 10 fields (m-2) ───

 (test-case "ui-callbacks-installed? returns #f when all null"
   (parameterize ([current-ui-registry (ui-callback-registry #f #f #f #f #f #f #f #f #f #f)])
     (check-false (ui-callbacks-installed?))))

 (test-case "ui-callbacks-installed? returns #t when all 10 installed"
   (parameterize ([current-ui-registry (ui-callback-registry
                                         void void void void void void
                                         void void void void)])
     (check-true (ui-callbacks-installed?))))

 (test-case "ui-callbacks-installed? returns #f when status-message missing"
   (parameterize ([current-ui-registry (ui-callback-registry
                                         void void void void void void
                                         #f void void void)])
     (check-false (ui-callbacks-installed?))))

 (test-case "ui-callbacks-installed? returns #f when set-extension-widget missing"
   (parameterize ([current-ui-registry (ui-callback-registry
                                         void void void void void void
                                         void #f void void)])
     (check-false (ui-callbacks-installed?))))

 (test-case "ui-callbacks-installed? returns #f when remove-extension-widget missing"
   (parameterize ([current-ui-registry (ui-callback-registry
                                         void void void void void void
                                         void void #f void)])
     (check-false (ui-callbacks-installed?))))

 (test-case "ui-callbacks-installed? returns #f when remove-all-extension-widgets missing"
   (parameterize ([current-ui-registry (ui-callback-registry
                                         void void void void void void
                                         void void void #f)])
     (check-false (ui-callbacks-installed?)))))

(run-tests test-ui-surface-null-safety)
