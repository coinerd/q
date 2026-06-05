#lang racket

;; q/tests/test-gui-lifecycle-hooks.rkt — Tests for GUI lifecycle hook dispatch
;;
;; W3.1 (v0.94.3): Verify hook registration, dispatch, and multi-handler ordering.

(require rackunit
         rackunit/text-ui
         "../gui/lifecycle-hooks.rkt")

(define-test-suite
 test-gui-lifecycle-hooks

 (test-case "register-gui-lifecycle-hook! adds handler"
   (define called (box #f))
   (parameterize ([current-gui-lifecycle-hooks (hasheq)])
     (register-gui-lifecycle-hook! 'gui.window.opened
                                    (lambda (payload) (set-box! called #t)))
     (dispatch-gui-hook! 'gui.window.opened (hash))
     (check-true (unbox called))))

 (test-case "dispatch-gui-hook! calls all registered handlers in order"
   (define log (box '()))
   (parameterize ([current-gui-lifecycle-hooks (hasheq)])
     (register-gui-lifecycle-hook! 'gui.theme.changed
                                    (lambda (p) (set-box! log (append (unbox log) '(a)))))
     (register-gui-lifecycle-hook! 'gui.theme.changed
                                    (lambda (p) (set-box! log (append (unbox log) '(b)))))
     (dispatch-gui-hook! 'gui.theme.changed (hash 'theme 'dark))
     (check-equal? (unbox log) '(a b))))

 (test-case "dispatch-gui-hook! passes payload to handlers"
   (define received (box #f))
   (parameterize ([current-gui-lifecycle-hooks (hasheq)])
     (register-gui-lifecycle-hook! 'gui.input.submit
                                    (lambda (p) (set-box! received p)))
     (dispatch-gui-hook! 'gui.input.submit (hash 'text "hello"))
     (check-equal? (hash-ref (unbox received) 'text) "hello")))

 (test-case "register-gui-lifecycle-hook! rejects unknown hook name"
   (parameterize ([current-gui-lifecycle-hooks (hasheq)])
     (check-exn exn:fail?
                (lambda ()
                  (register-gui-lifecycle-hook! 'gui.nonexistent
                                                 (lambda (p) (void)))))))

 (test-case "dispatch-gui-hook! rejects unknown hook name"
   (parameterize ([current-gui-lifecycle-hooks (hasheq)])
     (check-exn exn:fail?
                (lambda ()
                  (dispatch-gui-hook! 'gui.nonexistent (hash))))))

 (test-case "gui-hook-registered? returns #t when handler is registered"
   (parameterize ([current-gui-lifecycle-hooks (hasheq)])
     (check-false (gui-hook-registered? 'gui.window.opened))
     (register-gui-lifecycle-hook! 'gui.window.opened (lambda (p) (void)))
     (check-true (gui-hook-registered? 'gui.window.opened))))

 (test-case "dispatch with no handlers is a no-op"
   (parameterize ([current-gui-lifecycle-hooks (hasheq)])
     ;; Should not error
     (dispatch-gui-hook! 'gui.window.closed (hash))
     (check-true #t)))

 (test-case "all 8 GUI hooks are dispatchable"
   (for ([name '(gui.window.opened gui.window.closed
                               gui.theme.changed
                               gui.layout.changed
                               gui.focus.changed
                               gui.input.submit
                               gui.command.execute
                               gui.scroll.request)])
     (parameterize ([current-gui-lifecycle-hooks (hasheq)])
       (register-gui-lifecycle-hook! name (lambda (p) (void)))
       (check-true (gui-hook-registered? name)
                   (format "hook ~a should be registered" name)))))

 (test-case "handlers are isolated between parameterize scopes"
   (define called (box #f))
   (parameterize ([current-gui-lifecycle-hooks (hasheq)])
     (register-gui-lifecycle-hook! 'gui.window.opened
                                    (lambda (p) (set-box! called #t))))
   ;; Different scope — handler should not fire
   (parameterize ([current-gui-lifecycle-hooks (hasheq)])
     (dispatch-gui-hook! 'gui.window.opened (hash)))
   (check-false (unbox called)))
 )

(run-tests test-gui-lifecycle-hooks)
