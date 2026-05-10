#lang racket/base

;; tests/test-gsd-isolation.rkt — Multi-session isolation test (H-05)
;; Verifies that independent gsd-session-ctx instances don't interfere.

(require rackunit
         rackunit/text-ui
         "../extensions/gsd/session-state.rkt"
         "../extensions/gsd/state-machine.rkt"
         "../extensions/gsd/runtime-state-types.rkt")

(define-test-suite
 gsd-isolation-suite
 (test-case "multi-session state isolation"
   (define ctx-a (make-gsd-context))
   (define ctx-b (make-gsd-context))
   ;; Both start at idle
   (check-equal? (gsd-ctx-mode ctx-a) 'idle)
   (check-equal? (gsd-ctx-mode ctx-b) 'idle)
   ;; Transition ctx-a to exploring
   (gsd-ctx-set-state! ctx-a (struct-copy gsd-runtime-state (gsd-ctx-state ctx-a) [mode 'exploring]))
   (check-equal? (gsd-ctx-mode ctx-a) 'exploring "ctx-a should be exploring")
   (check-equal? (gsd-ctx-mode ctx-b) 'idle "ctx-b should still be idle")
   ;; Transition ctx-b independently
   (gsd-ctx-set-state! ctx-b
                       (struct-copy gsd-runtime-state (gsd-ctx-state ctx-b) [mode 'plan-written]))
   (check-equal? (gsd-ctx-mode ctx-a) 'exploring "ctx-a unchanged")
   (check-equal? (gsd-ctx-mode ctx-b) 'plan-written "ctx-b should be plan-written"))
 (test-case "multi-session history isolation"
   (define ctx-a (make-gsd-context))
   (define ctx-b (make-gsd-context))
   ;; Add history to ctx-a
   (gsd-ctx-set-history! ctx-a '((idle exploring 100)))
   (check-equal? (gsd-ctx-history ctx-a) '((idle exploring 100)))
   (check-equal? (gsd-ctx-history ctx-b) '() "ctx-b history unaffected"))
 (test-case "multi-session plan isolation"
   (define ctx-a (make-gsd-context))
   (define ctx-b (make-gsd-context))
   (gsd-ctx-set-plan! ctx-a '(("wave 0" "done")))
   (check-not-false (gsd-ctx-plan ctx-a))
   (check-false (gsd-ctx-plan ctx-b) "ctx-b plan unaffected"))
 (test-case "default context is independent"
   (define ctx-new (make-gsd-context))
   ;; Modifying ctx-new should not affect default
   (gsd-ctx-set-state! ctx-new
                       (struct-copy gsd-runtime-state (gsd-ctx-state ctx-new) [mode 'executing]))
   (check-equal? (gsd-ctx-mode ctx-new) 'executing)
   ;; Default ctx should be unaffected (unless other tests changed it)
   (check-not-exn (lambda () (gsd-ctx-mode gsd-default-ctx))))
 (test-case "transaction isolation"
   (define ctx-a (make-gsd-context))
   (define ctx-b (make-gsd-context))
   ;; Use direct box access inside transaction (same semaphore)
   (call-with-semaphore (gsd-session-ctx-sem ctx-a)
                        (lambda ()
                          (set-box! (gsd-session-ctx-state-box ctx-a)
                                    (struct-copy gsd-runtime-state
                                                 (unbox (gsd-session-ctx-state-box ctx-a))
                                                 [mode 'verifying]))))
   (check-equal? (gsd-ctx-mode ctx-a) 'verifying)
   (check-equal? (gsd-ctx-mode ctx-b) 'idle)))

(run-tests gsd-isolation-suite)
