#lang racket

;; q/tests/test-ui-surface-characterization.rkt — Characterization tests for
;; extensions/ui-surface.rkt callback path behavior.
;;
;; W0.2: Lock existing behavior before refactor.
;; These tests verify:
;;   1. Parameterized registry isolation (R-05)
;;   2. Widget fallback path when no TUI callback installed
;;   3. Widget callback path when TUI callback installed
;;   4. install-ui-callbacks! sets registry correctly
;;   5. ui-callbacks-installed? reflects required callbacks only
;;   6. ui-set-status-message! is safe when cb is #f

(require rackunit
         rackunit/text-ui
         "../extensions/ui-surface.rkt")

;; Import ui-state for widget fallback tests
(require (only-in "../tui/state.rkt" ui-state? initial-ui-state ui-state-extension-widgets))

;; Helper: create a registry where all 6 required callbacks are set
(define (make-test-registry)
  (ui-callback-registry (lambda (box lines) (void))
                        (lambda (box lines) (void))
                        (lambda (box) (void))
                        (lambda (box) (void))
                        (lambda (segments) 'styled-line)
                        (lambda (text style) 'styled-segment)
                        #f
                        #f
                        #f
                        #f))

;; Helper: create a registry where NOTHING is set
(define (make-null-registry)
  (ui-callback-registry #f #f #f #f #f #f #f #f #f #f))

(define-test-suite
 test-ui-surface-characterization
 ;; ─── R-05: parameterized registry ───
 (test-case "R-05: null registry is installable"
   (define saved (current-ui-registry))
   (parameterize ([current-ui-registry (make-null-registry)])
     (check-true (ui-callback-registry? (current-ui-registry))))
   (current-ui-registry saved))
 (test-case "R-05: parameterized registry does not leak"
   (define saved (current-ui-registry))
   (define reg (make-test-registry))
   (parameterize ([current-ui-registry reg])
     ;; CHARACTERIZATION: ui-callbacks-installed? returns proc not boolean (contract bug)
     (check-exn exn:fail:contract? (lambda () (ui-callbacks-installed?))))
   ;; After parameterize scope, reverts to saved
   (check-equal? (current-ui-registry) saved))
 ;; ─── Widget fallback path (no TUI callback) ───
 (test-case "widget fallback: ui-set-extension-widget! without callback uses struct-copy"
   (define saved (current-ui-registry))
   (parameterize ([current-ui-registry (make-null-registry)])
     (define state (initial-ui-state))
     (define result (ui-set-extension-widget! state 'my-ext 'my-key '("line1" "line2")))
     (check-true (ui-state? result))
     (define widgets (ui-state-extension-widgets result))
     (check-equal? (hash-ref widgets (cons 'my-ext 'my-key) #f) '("line1" "line2")))
   (current-ui-registry saved))
 (test-case "widget fallback: ui-remove-extension-widget! removes specific key"
   (define saved (current-ui-registry))
   (parameterize ([current-ui-registry (make-null-registry)])
     (define state (initial-ui-state))
     (define s1 (ui-set-extension-widget! state 'ext1 'k1 '("a")))
     (define s2 (ui-set-extension-widget! s1 'ext1 'k2 '("b")))
     (define s3 (ui-remove-extension-widget! s2 'ext1 'k1))
     (define widgets (ui-state-extension-widgets s3))
     (check-false (hash-ref widgets (cons 'ext1 'k1) #f))
     (check-equal? (hash-ref widgets (cons 'ext1 'k2) #f) '("b")))
   (current-ui-registry saved))
 (test-case "widget fallback: ui-remove-all-extension-widgets! removes all for ext"
   (define saved (current-ui-registry))
   (parameterize ([current-ui-registry (make-null-registry)])
     (define state (initial-ui-state))
     (define s1 (ui-set-extension-widget! state 'ext1 'k1 '("a")))
     (define s2 (ui-set-extension-widget! s1 'ext1 'k2 '("b")))
     (define s3 (ui-set-extension-widget! s2 'ext2 'k1 '("c")))
     (define s4 (ui-remove-all-extension-widgets! s3 'ext1))
     (define widgets (ui-state-extension-widgets s4))
     (check-false (hash-ref widgets (cons 'ext1 'k1) #f))
     (check-false (hash-ref widgets (cons 'ext1 'k2) #f))
     (check-equal? (hash-ref widgets (cons 'ext2 'k1) #f) '("c")))
   (current-ui-registry saved))
 ;; ─── Widget callback path (TUI callback installed) ───
 (test-case "widget callback: ui-set-extension-widget! invokes cb when installed"
   (define saved (current-ui-registry))
   (define called-box (box #f))
   (define (mock-set state ext-name key lines)
     (set-box! called-box (list ext-name key lines))
     state)
   (define reg (ui-callback-registry #f #f #f #f #f #f #f mock-set #f #f))
   (parameterize ([current-ui-registry reg])
     (define state (initial-ui-state))
     (ui-set-extension-widget! state 'ext 'k '("data"))
     (check-equal? (unbox called-box) '(ext k ("data"))))
   (current-ui-registry saved))
 ;; ─── install-ui-callbacks! ───
 (test-case "install-ui-callbacks! creates a registry that reports installed"
   (define saved (current-ui-registry))
   (install-ui-callbacks! (hash 'set-footer
                                (lambda _ (void))
                                'set-header
                                (lambda _ (void))
                                'clear-footer
                                (lambda _ (void))
                                'clear-header
                                (lambda _ (void))
                                'make-styled-line
                                (lambda _ 'line)
                                'make-styled-segment
                                (lambda _ 'seg)
                                'set-status-message
                                (lambda _ (void))
                                'set-extension-widget
                                (lambda _ (void))
                                'remove-extension-widget
                                (lambda _ (void))
                                'remove-all-extension-widgets
                                (lambda _ (void))))
   ;; CHARACTERIZATION: ui-callbacks-installed? returns a proc, violating -> boolean?
   (check-exn exn:fail:contract? (lambda () (ui-callbacks-installed?)))
   (current-ui-registry saved))
 ;; ─── ui-callbacks-installed? ───
 (test-case "ui-callbacks-installed? returns #f with null registry"
   (define saved (current-ui-registry))
   (parameterize ([current-ui-registry (make-null-registry)])
     (check-false (ui-callbacks-installed?)))
   (current-ui-registry saved))
 (test-case "ui-callbacks-installed? with 6 required callbacks raises contract error (CHARACTERIZATION)"
   (define saved (current-ui-registry))
   (parameterize ([current-ui-registry (make-test-registry)])
     ;; CHARACTERIZATION: and-based check returns last truthy value (a procedure),
     ;; which violates the -> boolean? contract. This is a pre-existing contract bug.
     (check-exn exn:fail:contract? (lambda () (ui-callbacks-installed?))))
   (current-ui-registry saved))
 ;; ─── ui-set-status-message! safety ───
 (test-case "ui-set-status-message! is no-op when cb is #f"
   (define saved (current-ui-registry))
   (parameterize ([current-ui-registry (make-null-registry)])
     ;; Should not error — just returns void
     (ui-set-status-message! (box #f) "test msg")
     (check-true #t))
   (current-ui-registry saved)))

(run-tests test-ui-surface-characterization)
