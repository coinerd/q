#lang racket

(require rackunit
         rackunit/text-ui
         "../tui/component.rkt"
         "../tui/state.rkt")

(define-test-suite
 test-component-model
 ;; ──────────────────────────────
 ;; make-q-component with handle-input
 ;; ──────────────────────────────
 (test-case "make-q-component with handle-input"
   (define counter (box 0))
   (define comp
     (make-q-component (lambda (state width) (list (list (cons 'text "hello"))))
                       #:id 'counter
                       #:handle-input (lambda (data state)
                                        (set-box! counter (add1 (unbox counter)))
                                        (values state (input-consumed)))
                       #:wants-focus? #t))
   (check-equal? (q-component-id comp) 'counter)
   (check-true (q-component-wants-focus? comp))
   (check-true (procedure? (q-component-handle-input-fn comp))))
 ;; ──────────────────────────────
 ;; component-handle-input dispatch
 ;; ──────────────────────────────
 (test-case "component-handle-input dispatches to handler"
   (define comp
     (make-q-component (lambda (state width) '())
                       #:handle-input (lambda (data state) (values state (input-action 'clicked)))))
   (define s0 (initial-ui-state))
   (define-values (s1 result) (component-handle-input comp 'click s0))
   (check-true (input-action? result))
   (check-equal? (input-action-data result) 'clicked))
 (test-case "component-handle-input bubbles without handler"
   (define comp (make-q-component (lambda (state width) '())))
   (define s0 (initial-ui-state))
   (define-values (s1 result) (component-handle-input comp 'key s0))
   (check-true (input-bubble? result)))
 ;; ──────────────────────────────
 ;; input-result constructors
 ;; ──────────────────────────────
 (test-case "input-result constructors"
   (check-true (input-consumed? (input-consumed)))
   (check-true (input-bubble? (input-bubble)))
   (check-true (input-action? (input-action 'data)))
   (check-equal? (input-action-data (input-action 'test)) 'test))
 ;; ──────────────────────────────
 ;; Focusable components
 ;; ──────────────────────────────
 (test-case "focusable-components filters by wants-focus?"
   (define comps
     (list (make-q-component (lambda (s w) '()) #:id 'a #:wants-focus? #t)
           (make-q-component (lambda (s w) '()) #:id 'b)
           (make-q-component (lambda (s w) '()) #:id 'c #:wants-focus? #t)))
   (define focusable (focusable-components comps))
   (check-equal? (length focusable) 2)
   (check-equal? (map q-component-id focusable) '(a c)))
 ;; ──────────────────────────────
 ;; cycle-focus
 ;; ──────────────────────────────
 (test-case "cycle-focus cycles through focusable components"
   (define comps
     (list (make-q-component (lambda (s w) '()) #:id 'a #:wants-focus? #t)
           (make-q-component (lambda (s w) '()) #:id 'b #:wants-focus? #t)
           (make-q-component (lambda (s w) '()) #:id 'c #:wants-focus? #t)))
   (check-equal? (cycle-focus comps 'a) 'b)
   (check-equal? (cycle-focus comps 'b) 'c)
   (check-equal? (cycle-focus comps 'c) 'a))
 (test-case "cycle-focus backward"
   (define comps
     (list (make-q-component (lambda (s w) '()) #:id 'a #:wants-focus? #t)
           (make-q-component (lambda (s w) '()) #:id 'b #:wants-focus? #t)))
   (check-equal? (cycle-focus comps 'a -1) 'b)
   (check-equal? (cycle-focus comps 'b -1) 'a))
 (test-case "cycle-focus returns #f when no focusable components"
   (define comps (list (make-q-component (lambda (s w) '()) #:id 'a)))
   (check-false (cycle-focus comps #f)))
 ;; ──────────────────────────────
 ;; Backward compatibility
 ;; ──────────────────────────────
 (test-case "component-render still works with new fields"
   (define comp
     (make-q-component (lambda (state width) (list (list (cons 'text "hello world"))))
                       #:handle-input (lambda (d s) (values s (input-consumed)))))
   (define lines (component-render comp (initial-ui-state) 80))
   (check-equal? (length lines) 1))
 ;; ──────────────────────────────
 ;; ui-state focused-component
 ;; ──────────────────────────────
 (test-case "set-focused-component updates ui-state"
   (define s0 (initial-ui-state))
   (define s1 (set-focused-component s0 'my-comp))
   (check-equal? (ui-state-focused-component s1) 'my-comp))
 (test-case "clear-focused-component resets to #f"
   (define s0 (set-focused-component (initial-ui-state) 'my-comp))
   (define s1 (clear-focused-component s0))
   (check-false (ui-state-focused-component s1)))
 (test-case "focused-component defaults to #f"
   (define s0 (initial-ui-state))
   (check-false (ui-state-focused-component s0))))

(run-tests test-component-model)
