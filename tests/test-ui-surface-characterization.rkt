#lang racket

;; @speed fast
;; @suite default

;; q/tests/test-ui-surface-characterization.rkt — Characterization tests for ui-surface
;;
;; W6 (v0.94.8): T-2 fix — removed redundant manual save/restore.
;; Updated tests to reflect W5 layering fix (no struct-copy fallback).

(require rackunit
         rackunit/text-ui
         "../extensions/ui-surface.rkt"
         (only-in "../tui/state.rkt" initial-ui-state ui-state? ui-state-extension-widgets))

;; Helper: create a registry where all required callbacks are set
(define (make-test-registry)
  (ui-callback-registry (lambda (b lines) (void))
                        (lambda (b lines) (void))
                        (lambda (b) (void))
                        (lambda (b) (void))
                        (lambda (segments) 'styled-line)
                        (lambda (text style) 'styled-segment)
                        (lambda (b msg) (void))
                        (lambda (state ext key lines) state)
                        (lambda (state ext key) state)
                        (lambda (state ext) state)))

(define (make-null-registry)
  (ui-callback-registry #f #f #f #f #f #f #f #f #f #f))

(define-test-suite
 test-ui-surface-characterization
 ;; ─── Registry basics ───
 (test-case "current-ui-registry is a ui-callback-registry"
   (check-true (ui-callback-registry? (current-ui-registry))))
 (test-case "R-05: parameterized registry does not leak"
   (define saved (current-ui-registry))
   (parameterize ([current-ui-registry (make-test-registry)])
     (check-true (ui-callback-registry? (current-ui-registry))))
   ;; After parameterize scope, reverts to saved
   (check-equal? (current-ui-registry) saved))
 ;; ─── Widget fallback path (no callback → returns state unchanged) ───
 ;; W5 layering fix: no more struct-copy fallback; returns state + logs warning.
 (test-case "widget fallback: ui-set-extension-widget! without callback returns state"
   (parameterize ([current-ui-registry (make-null-registry)])
     (define state (initial-ui-state))
     (define result (ui-set-extension-widget! state 'my-ext 'my-key '("line1" "line2")))
     (check-true (ui-state? result))
     ;; No modification — callback not installed
     (check-equal? (hash-count (ui-state-extension-widgets result)) 0)))
 (test-case "widget fallback: ui-remove-extension-widget! without callback returns state"
   (parameterize ([current-ui-registry (make-null-registry)])
     (define state (initial-ui-state))
     (define result (ui-remove-extension-widget! state 'ext1 'k1))
     (check-true (ui-state? result))))
 (test-case "widget fallback: ui-remove-all-extension-widgets! without callback returns state"
   (parameterize ([current-ui-registry (make-null-registry)])
     (define state (initial-ui-state))
     (define result (ui-remove-all-extension-widgets! state 'ext1))
     (check-true (ui-state? result))))
 ;; ─── Widget callback path (TUI callback installed) ───
 (test-case "widget callback: ui-set-extension-widget! invokes cb when installed"
   (define called-box (box #f))
   (define (mock-set state ext-name key lines)
     (set-box! called-box (list ext-name key lines))
     state)
   (define reg (ui-callback-registry #f #f #f #f #f #f #f mock-set #f #f))
   (parameterize ([current-ui-registry reg])
     (define state (initial-ui-state))
     (ui-set-extension-widget! state 'ext 'k '("data"))
     (check-equal? (unbox called-box) '(ext k ("data")))))
 ;; ─── install-ui-callbacks! ───
 (test-case "install-ui-callbacks! creates registry with all callbacks"
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
   (check-true (ui-callbacks-installed?)))
 ;; ─── ui-callbacks-installed? ───
 (test-case "ui-callbacks-installed? returns #f with null registry"
   (parameterize ([current-ui-registry (make-null-registry)])
     (check-false (ui-callbacks-installed?))))
 (test-case "ui-callbacks-installed? returns #t with all 10 callbacks"
   (parameterize ([current-ui-registry (make-test-registry)])
     ;; W1 fix: now checks all 10 fields and returns #t
     (check-true (ui-callbacks-installed?))))
 ;; ─── ui-set-status-message! safety ───
 (test-case "ui-set-status-message! is no-op when cb is #f"
   (parameterize ([current-ui-registry (make-null-registry)])
     (ui-set-status-message! (box #f) "test")
     (check-true #t))))

(run-tests test-ui-surface-characterization)
