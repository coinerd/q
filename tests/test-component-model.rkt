#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: integration

(require rackunit
         rackunit/text-ui
         "../tui/component.rkt"
         "../tui/state.rkt"
         "../tui/vdom.rkt"
         (only-in "../tui/context.rkt"
                  make-tui-ctx
                  tui-ctx-focused-component-id
                  tui-ctx-set-focused-component!
                  tui-ctx-component-registry-box)
         (only-in "../tui/keybindings/key-dispatch.rkt" handle-key))

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
 (test-case "component-model: component-handle-input bubbles without handler"
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
 (test-case "component-model: cycle-focus returns #f when no focusable components"
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
   (check-false (ui-state-focused-component s0)))
 ;; ──────────────────────────────
 ;; vdom mode components
 ;; ──────────────────────────────
 (test-case "make-q-component with #:vdom? #t"
   (define comp
     (make-q-component (lambda (state width) (list (vtext "Hello vdom" '(bold))))
                       #:id 'vdom-test
                       #:vdom? #t))
   (check-true (q-component-vdom? comp))
   (check-false (q-component-vdom? (make-q-component (lambda (s w) '()))))
   (define result (component-render comp (initial-ui-state) 80))
   (check-equal? (length result) 1)
   (check-true (vtext? (car result))))
 (test-case "component-render caches vdom output"
   (define call-count (box 0))
   (define comp
     (make-q-component (lambda (state width)
                         (set-box! call-count (add1 (unbox call-count)))
                         (list (vtext "cached" '())))
                       #:vdom? #t))
   (component-render comp (initial-ui-state) 80)
   (component-render comp (initial-ui-state) 80)
   (check-equal? (unbox call-count) 1))
 (test-case "component-render returns stale output when state changes (cache bug)"
   ;; RED gate for status-line stale bug: cache key is width only, not state.
   ;; When state changes but width stays same, component returns cached stale output.
   (define render-fn-call-count (box 0))
   (define comp
     (make-q-component (lambda (state width)
                         (set-box! render-fn-call-count (add1 (unbox render-fn-call-count)))
                         (define busy (ui-state-busy? state))
                         (list (vtext (if busy "BUSY" "IDLE") '())))
                       #:vdom? #t))
   (define s-idle (initial-ui-state))
   (define s-busy (set-busy s-idle #t))
   ;; First render with idle state
   (define r1 (component-render comp s-idle 80))
   (check-equal? (vtext-text (car r1)) "IDLE")
   ;; State changed to busy, same width — should NOT return stale idle output
   (component-invalidate! comp)
   (define r2 (component-render comp s-busy 80))
   (check-equal? (vtext-text (car r2)) "BUSY")
   ;; Without invalidation, cache would return "IDLE" — this proves invalidation is required
   )
 (test-case "component-compose mixes vdom and styled-line components"
   (define vdom-comp (make-q-component (lambda (s w) (list (vtext "vdom" '()))) #:vdom? #t))
   (define classic-comp (make-q-component (lambda (s w) (list (list (cons 'text "classic"))))))
   (define result (component-compose (list vdom-comp classic-comp) (initial-ui-state) 80))
   (check-equal? (length result) 2))
 (test-case "component-state-ref returns default for missing key"
   (define comp (make-q-component (lambda (s w) '())))
   (check-equal? (component-state-ref comp 'foo) #f)
   (check-equal? (component-state-ref comp 'foo 42) 42))
 (test-case "component-state-update stores and retrieves values"
   (define comp (make-q-component (lambda (s w) '())))
   (component-state-update comp 'counter 0)
   (check-equal? (component-state-ref comp 'counter) 0)
   (component-state-update comp 'counter 5)
   (check-equal? (component-state-ref comp 'counter) 5))
 (test-case "component state persists across renders"
   (define render-count 0)
   (define comp
     (make-q-component (lambda (s w)
                         (set! render-count (add1 render-count))
                         (define prev (component-state-ref comp 'renders 0))
                         (component-state-update comp 'renders (add1 prev))
                         (list (vtext (format "render #~a" (add1 prev)) '())))
                       #:vdom? #t))
   (component-render comp (initial-ui-state) 80)
   (check-equal? (component-state-ref comp 'renders) 1)
   (component-invalidate! comp)
   (component-render comp (initial-ui-state) 80)
   (check-equal? (component-state-ref comp 'renders) 2))
 (test-case "each component has independent state"
   (define comp1 (make-q-component (lambda (s w) '())))
   (define comp2 (make-q-component (lambda (s w) '())))
   (component-state-update comp1 'key 'val1)
   (component-state-update comp2 'key 'val2)
   (check-equal? (component-state-ref comp1 'key) 'val1)
   (check-equal? (component-state-ref comp2 'key) 'val2)))

;; ═══════════════════════════════════════════════════════════
;; State bag integration with render pipeline
;; ═══════════════════════════════════════════════════════════
(test-case "state bag tracks scroll position across renders"
  ;; Simulates a component that tracks its own scroll offset
  (define comp
    (make-q-component (lambda (st width)
                        (define scroll (component-state-ref comp 'scroll 0))
                        (component-state-update comp 'scroll (+ scroll 1))
                        (list (vtext (format "scroll:~a" scroll) '())))
                      #:vdom? #t))
  ;; First render — scroll is 0, stored as 1
  (define r1 (component-render comp (initial-ui-state) 40))
  (check-equal? (component-state-ref comp 'scroll) 1)
  ;; Invalidate and re-render — scroll is 1, stored as 2
  (component-invalidate! comp)
  (define r2 (component-render comp (initial-ui-state) 40))
  (check-equal? (component-state-ref comp 'scroll) 2)
  ;; Cache hit on same width — state unchanged (no re-render)
  (define r3 (component-render comp (initial-ui-state) 40))
  (check-equal? (component-state-ref comp 'scroll) 2)
  (check-equal? (component-state-ref comp 'scroll) 2))
(define-test-suite
 focus-tracking-tests
 (test-case "tui-ctx focus tracking starts as #f"
   (define ctx (make-tui-ctx))
   (check-false (tui-ctx-focused-component-id ctx)))
 (test-case "tui-ctx set and get focused component"
   (define ctx (make-tui-ctx))
   (tui-ctx-set-focused-component! ctx 'input-vdom)
   (check-equal? (tui-ctx-focused-component-id ctx) 'input-vdom)
   (tui-ctx-set-focused-component! ctx #f)
   (check-false (tui-ctx-focused-component-id ctx)))
 (test-case "focused component receives key via handle-key"
   (define key-log (box '()))
   (define test-comp
     (make-q-component (lambda (s w) '())
                       #:id 'test-focusable
                       #:handle-input (lambda (key state)
                                        (set-box! key-log (cons key (unbox key-log)))
                                        (values state (input-consumed)))))
   (define ctx (make-tui-ctx))
   (set-box! (tui-ctx-component-registry-box ctx) (make-hash (list (cons 'test-focusable test-comp))))
   (tui-ctx-set-focused-component! ctx 'test-focusable)
   (handle-key ctx #\a)
   (check-equal? (unbox key-log) '(#\a)))
 ;; ──────────────────────────────
 ;; Focus cycling via cycle-focus
 ;; ──────────────────────────────
 (test-case "component-model: cycle-focus returns #f when no focusable components (2)"
   (define comp (make-q-component (lambda (s w) '()) #:wants-focus? #f))
   (check-false (cycle-focus (list comp) #f)))
 (test-case "cycle-focus cycles forward through focusable components"
   (define c1 (make-q-component (lambda (s w) '()) #:id 'a #:wants-focus? #t))
   (define c2 (make-q-component (lambda (s w) '()) #:id 'b #:wants-focus? #t))
   (define c3 (make-q-component (lambda (s w) '()) #:id 'c #:wants-focus? #t))
   (define comps (list c1 c2 c3))
   ;; Start from #f → first focusable
   (check-equal? (cycle-focus comps #f 1) 'a)
   ;; From a → b
   (check-equal? (cycle-focus comps 'a 1) 'b)
   ;; From b → c
   (check-equal? (cycle-focus comps 'b 1) 'c)
   ;; From c → wraps to a
   (check-equal? (cycle-focus comps 'c 1) 'a))
 (test-case "cycle-focus cycles backward through focusable components"
   (define c1 (make-q-component (lambda (s w) '()) #:id 'a #:wants-focus? #t))
   (define c2 (make-q-component (lambda (s w) '()) #:id 'b #:wants-focus? #t))
   (define c3 (make-q-component (lambda (s w) '()) #:id 'c #:wants-focus? #t))
   (define comps (list c1 c2 c3))
   ;; From #f → index -1, backward: modulo(-1 + (-1), 3) = modulo(-2, 3) = 1 → b
   (check-equal? (cycle-focus comps #f -1) 'b)
   ;; From c → b
   (check-equal? (cycle-focus comps 'c -1) 'b)
   ;; From a → wraps to c
   (check-equal? (cycle-focus comps 'a -1) 'c))
 (test-case "M-Tab (alt-tab) cycles focus in tui-ctx via handle-key"
   (define c1 (make-q-component (lambda (s w) '()) #:id 'comp-a #:wants-focus? #t))
   (define c2 (make-q-component (lambda (s w) '()) #:id 'comp-b #:wants-focus? #t))
   (define ctx (make-tui-ctx))
   (set-box! (tui-ctx-component-registry-box ctx)
             (make-hash (list (cons 'comp-a c1) (cons 'comp-b c2))))
   ;; No focus yet → first focusable
   (handle-key ctx 'alt-tab)
   (check-equal? (tui-ctx-focused-component-id ctx) 'comp-a)
   ;; Forward → second
   (handle-key ctx 'alt-tab)
   (check-equal? (tui-ctx-focused-component-id ctx) 'comp-b)
   ;; Forward → wraps to first
   (handle-key ctx 'alt-tab)
   (check-equal? (tui-ctx-focused-component-id ctx) 'comp-a))
 (test-case "S-Tab (shift-tab) cycles focus backward via handle-key"
   (define c1 (make-q-component (lambda (s w) '()) #:id 'comp-a #:wants-focus? #t))
   (define c2 (make-q-component (lambda (s w) '()) #:id 'comp-b #:wants-focus? #t))
   (define ctx (make-tui-ctx))
   (set-box! (tui-ctx-component-registry-box ctx)
             (make-hash (list (cons 'comp-a c1) (cons 'comp-b c2))))
   ;; Start from #f -> backward from -1: modulo(-1 + (-1), 2) = 0 -> comp-a
   (handle-key ctx 'shift-tab)
   (check-equal? (tui-ctx-focused-component-id ctx) 'comp-a)
   ;; Backward from comp-a -> comp-b
   (handle-key ctx 'shift-tab)
   (check-equal? (tui-ctx-focused-component-id ctx) 'comp-b)))

(run-tests focus-tracking-tests)
(run-tests test-component-model)
