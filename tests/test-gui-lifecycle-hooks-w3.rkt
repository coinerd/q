#lang racket

;; @speed fast
;; @suite default

;; q/tests/test-gui-lifecycle-hooks-w3.rkt — W3 lifecycle hook tests
;;
;; W3 (v0.94.8): H-1 (export current-gui-event-runtime),
;; H-3 (error isolation in dispatch-gui-hook!).

(require rackunit
         rackunit/text-ui
         (only-in "../gui/lifecycle-hooks.rkt"
                  current-gui-lifecycle-hooks
                  current-gui-event-runtime
                  register-gui-lifecycle-hook!
                  dispatch-gui-hook!
                  gui-hook-registered?))

(define-test-suite test-gui-lifecycle-hooks-w3
                   ;; ── H-1: current-gui-event-runtime is exported ───
                   (test-case "current-gui-event-runtime is accessible"
                     (check-false (current-gui-event-runtime))
                     (parameterize ([current-gui-event-runtime 'mock-runtime])
                       (check-eq? (current-gui-event-runtime) 'mock-runtime))))

(define-test-suite
 test-gui-lifecycle-hooks-error-isolation
 ;; ── H-3: error isolation in dispatch-gui-hook! ───
 (test-case "error in first handler doesn't prevent second handler"
   (define called? (box #f))
   (parameterize ([current-gui-lifecycle-hooks (hasheq 'agent.idle
                                                       (list (lambda (payload) (error "boom"))
                                                             (lambda (payload)
                                                               (set-box! called? #t))))])
     ;; Should NOT raise, and second handler should still run
     (dispatch-gui-hook! 'agent.idle (hash))
     (check-true (unbox called?))))
 (test-case "dispatch-gui-hook! with no handlers succeeds"
   (parameterize ([current-gui-lifecycle-hooks (hasheq)])
     (dispatch-gui-hook! 'agent.idle (hash))))
 (test-case "dispatch-gui-hook! raises on invalid hook name"
   (check-exn exn:fail? (lambda () (dispatch-gui-hook! 'totally-invalid-hook-name (hash))))))

(run-tests test-gui-lifecycle-hooks-w3)
(run-tests test-gui-lifecycle-hooks-error-isolation)
