#lang racket

;;; tests/test-widget-lifecycle.rkt — Lifecycle widget protocol tests (#5256)

(require rackunit
         rackunit/text-ui
         "../extensions/widget-lifecycle.rkt"
         "../tui/state.rkt"
         "../tui/component.rkt")

(define widget-lifecycle-tests
  (test-suite "widget lifecycle"

    ;; ============================================================
    ;; Construction & basic operations
    ;; ============================================================

    (test-case "make-widget-lifecycle creates widget with correct id"
      (define w (make-widget-lifecycle 'test-widget (lambda (state width) '())))
      (check-equal? (widget-lifecycle-id w) 'test-widget)
      (check-false (widget-mounted? w)))

    (test-case "widget-mount! triggers mount callback"
      (define mounted? (box #f))
      (define w
        (make-widget-lifecycle 'mount-test
                               (lambda (state width) '())
                               #:mount (lambda () (set-box! mounted? #t))))
      (check-false (unbox mounted?))
      (widget-mount! w)
      (check-true (unbox mounted?))
      (check-true (widget-mounted? w)))

    (test-case "widget-mount! is idempotent — mount callback called once"
      (define call-count (box 0))
      (define w
        (make-widget-lifecycle 'idempotent-mount
                               (lambda (state width) '())
                               #:mount (lambda () (set-box! call-count (add1 (unbox call-count))))))
      (widget-mount! w)
      (widget-mount! w)
      (widget-mount! w)
      (check-equal? (unbox call-count) 1))

    (test-case "widget-render auto-mounts and returns lines"
      (define state (initial-ui-state))
      (define w (make-widget-lifecycle 'render-test (lambda (st w) '("line1" "line2"))))
      (define lines (widget-render w state 80))
      (check-true (widget-mounted? w))
      (check-equal? lines '("line1" "line2")))

    (test-case "widget-handle-input with input-fn processes key"
      (define state (initial-ui-state))
      (define w
        (make-widget-lifecycle 'input-test
                               (lambda (st w) '())
                               #:input (lambda (key st) (values st (input-consumed)))))
      (define-values (new-state result) (widget-handle-input w "a" state))
      (check-true (input-consumed? result)))

    (test-case "widget-handle-input without input-fn bubbles"
      (define state (initial-ui-state))
      (define w (make-widget-lifecycle 'no-input (lambda (st w) '())))
      (define-values (new-state result) (widget-handle-input w "a" state))
      (check-true (input-bubble? result)))

    (test-case "widget-unmount! triggers unmount callback"
      (define unmounted? (box #f))
      (define w
        (make-widget-lifecycle 'unmount-test
                               (lambda (st w) '())
                               #:unmount (lambda () (set-box! unmounted? #t))))
      (widget-mount! w)
      (check-true (widget-mounted? w))
      (widget-unmount! w)
      (check-false (widget-mounted? w))
      (check-true (unbox unmounted?)))

    (test-case "widget-unmount! is idempotent when not mounted"
      (define unmount-count (box 0))
      (define w
        (make-widget-lifecycle 'idempotent-unmount
                               (lambda (st w) '())
                               #:unmount
                               (lambda () (set-box! unmount-count (add1 (unbox unmount-count))))))
      (widget-unmount! w)
      (widget-unmount! w)
      (check-equal? (unbox unmount-count) 0))

    ;; ============================================================
    ;; widget->component conversion
    ;; ============================================================

    (test-case "widget->component creates q-component with correct id"
      (define w (make-widget-lifecycle 'conv-test (lambda (st w) '())))
      (define c (widget->component w))
      (check-true (q-component? c))
      (check-equal? (q-component-id c) 'conv-test))

    (test-case "widget->component preserves input handling"
      (define w
        (make-widget-lifecycle 'conv-input
                               (lambda (st w) '())
                               #:input (lambda (key st) (values st (input-consumed)))))
      (define c (widget->component w))
      (check-true (q-component-wants-focus? c))
      (define state (initial-ui-state))
      (define-values (new-st result) (component-handle-input c "x" state))
      (check-true (input-consumed? result)))

    ;; ============================================================
    ;; Registry
    ;; ============================================================

    (test-case "register and lookup lifecycle widget"
      (unregister-lifecycle-widget! 'reg-test)
      (define w (make-widget-lifecycle 'reg-test (lambda (st w) '())))
      (register-lifecycle-widget! w)
      (define found (lookup-lifecycle-widget 'reg-test))
      (check-not-false found)
      (check-equal? (widget-lifecycle-id found) 'reg-test)
      (unregister-lifecycle-widget! 'reg-test))

    (test-case "unregister unmounts widget"
      (unregister-lifecycle-widget! 'unreg-test)
      (define unmounted? (box #f))
      (define w
        (make-widget-lifecycle 'unreg-test
                               (lambda (st w) '())
                               #:unmount (lambda () (set-box! unmounted? #t))))
      (register-lifecycle-widget! w)
      (widget-mount! w)
      (check-true (widget-mounted? w))
      (unregister-lifecycle-widget! 'unreg-test)
      (check-true (unbox unmounted?))
      (check-false (lookup-lifecycle-widget 'unreg-test)))

    (test-case "list-lifecycle-widgets returns all registered"
      (unregister-lifecycle-widget! 'list-1)
      (unregister-lifecycle-widget! 'list-2)
      (register-lifecycle-widget! (make-widget-lifecycle 'list-1 (lambda (st w) '())))
      (register-lifecycle-widget! (make-widget-lifecycle 'list-2 (lambda (st w) '())))
      (define all (list-lifecycle-widgets))
      (check-true (>= (length all) 2))
      (unregister-lifecycle-widget! 'list-1)
      (unregister-lifecycle-widget! 'list-2))

    (test-case "register replaces existing widget with same id"
      (unregister-lifecycle-widget! 'replace-test)
      (define w1 (make-widget-lifecycle 'replace-test (lambda (st w) '("w1"))))
      (define w2 (make-widget-lifecycle 'replace-test (lambda (st w) '("w2"))))
      (register-lifecycle-widget! w1)
      (register-lifecycle-widget! w2)
      (define found (lookup-lifecycle-widget 'replace-test))
      ;; Should find w2 (replaced w1)
      (define state (initial-ui-state))
      (define lines (widget-render found state 80))
      (check-equal? lines '("w2"))
      (unregister-lifecycle-widget! 'replace-test))

    ;; ============================================================
    ;; Focus management
    ;; ============================================================

    (test-case "focusable-lifecycle-widgets returns only those with input-fn"
      (unregister-lifecycle-widget! 'focus-1)
      (unregister-lifecycle-widget! 'focus-2)
      (register-lifecycle-widget! (make-widget-lifecycle 'focus-1 (lambda (st w) '())))
      (register-lifecycle-widget! (make-widget-lifecycle 'focus-2
                                                         (lambda (st w) '())
                                                         #:input (lambda (k st)
                                                                   (values st (input-consumed)))))
      (define focusable (focusable-lifecycle-widgets))
      (check-equal? (length focusable) 1)
      (check-equal? (widget-lifecycle-id (car focusable)) 'focus-2)
      (unregister-lifecycle-widget! 'focus-1)
      (unregister-lifecycle-widget! 'focus-2))

    (test-case "cycle-lifecycle-focus cycles through focusable widgets"
      (unregister-lifecycle-widget! 'cyc-1)
      (unregister-lifecycle-widget! 'cyc-2)
      (register-lifecycle-widget! (make-widget-lifecycle 'cyc-1
                                                         (lambda (st w) '())
                                                         #:input (lambda (k st)
                                                                   (values st (input-consumed)))))
      (register-lifecycle-widget! (make-widget-lifecycle 'cyc-2
                                                         (lambda (st w) '())
                                                         #:input (lambda (k st)
                                                                   (values st (input-consumed)))))
      (parameterize ([current-lifecycle-focus #f])
        (define first (cycle-lifecycle-focus))
        (check-not-false first)
        (define second (cycle-lifecycle-focus))
        (check-not-equal? first second)
        (define back (cycle-lifecycle-focus))
        (check-equal? back first))
      (unregister-lifecycle-widget! 'cyc-1)
      (unregister-lifecycle-widget! 'cyc-2))

    (test-case "route-lifecycle-input delivers to focused widget"
      (unregister-lifecycle-widget! 'route-test)
      (define received-key (box #f))
      (register-lifecycle-widget! (make-widget-lifecycle 'route-test
                                                         (lambda (st w) '())
                                                         #:input (lambda (k st)
                                                                   (set-box! received-key k)
                                                                   (values st (input-consumed)))))
      (parameterize ([current-lifecycle-focus 'route-test])
        (define state (initial-ui-state))
        (define-values (new-st result) (route-lifecycle-input "x" state))
        (check-true (input-consumed? result))
        (check-equal? (unbox received-key) "x"))
      (unregister-lifecycle-widget! 'route-test))

    (test-case "render-all-lifecycle-widgets concatenates all"
      (unregister-lifecycle-widget! 'render-1)
      (unregister-lifecycle-widget! 'render-2)
      (register-lifecycle-widget! (make-widget-lifecycle 'render-1 (lambda (st w) '("a"))))
      (register-lifecycle-widget! (make-widget-lifecycle 'render-2 (lambda (st w) '("b" "c"))))
      (define state (initial-ui-state))
      (define lines (render-all-lifecycle-widgets state 80))
      (check-equal? (length lines) 3)
      (unregister-lifecycle-widget! 'render-1)
      (unregister-lifecycle-widget! 'render-2))))

(module+ main
  (run-tests widget-lifecycle-tests))
