#lang racket

;;; tests/tui/component.rkt — Tests for tui/component.rkt

(require rackunit
         rackunit/text-ui
         "../../../q/tui/component.rkt"
         "../../../q/tui/render.rkt"
         "../../../q/tui/state.rkt")

(define-test-suite test-component

  ;; ──────────────────────────────
  ;; make-q-component
  ;; ──────────────────────────────

  (test-case "make-q-component creates a component with defaults"
    (define comp (make-q-component (lambda (st w) '()) #:id 'test))
    (check-equal? (q-component-id comp) 'test)
    (check-false (component-cached-width comp)))

  (test-case "make-q-component assigns anonymous id by default"
    (define comp (make-q-component (lambda (st w) '())))
    (check-equal? (q-component-id comp) 'anonymous))

  ;; ──────────────────────────────
  ;; component-render — cache miss
  ;; ──────────────────────────────

  (test-case "component-render calls render-fn on cache miss"
    (define called? (box #f))
    (define comp
      (make-q-component
       (lambda (st w)
         (set-box! called? #t)
         (list (styled-line (list (styled-segment "hello" '())))))
       #:id 'test))
    (define state (initial-ui-state))
    (define result (component-render comp state 80))
    (check-true (unbox called?))
    (check-equal? (length result) 1)
    (check-equal? (styled-line->text (car result)) "hello"))

  ;; ──────────────────────────────
  ;; component-render — cache hit
  ;; ──────────────────────────────

  (test-case "component-render returns cached on same width"
    (define call-count (box 0))
    (define comp
      (make-q-component
       (lambda (st w)
         (set-box! call-count (add1 (unbox call-count)))
         (list (styled-line (list (styled-segment "cached" '())))))
       #:id 'test))
    (define state (initial-ui-state))
    ;; First call — cache miss
    (define r1 (component-render comp state 80))
    (check-equal? (unbox call-count) 1)
    ;; Second call — cache hit (same width)
    (define r2 (component-render comp state 80))
    (check-equal? (unbox call-count) 1)  ;; NOT called again
    (check-equal? r1 r2))

  (test-case "component-render re-renders on width change"
    (define call-count (box 0))
    (define comp
      (make-q-component
       (lambda (st w)
         (set-box! call-count (add1 (unbox call-count)))
         (list (styled-line (list (styled-segment (format "w=~a" w) '())))))
       #:id 'test))
    (define state (initial-ui-state))
    ;; First call at width 80
    (component-render comp state 80)
    (check-equal? (unbox call-count) 1)
    ;; Width change — re-render
    (define r2 (component-render comp state 40))
    (check-equal? (unbox call-count) 2)
    (check-equal? (styled-line->text (car r2)) "w=40"))

  ;; ──────────────────────────────
  ;; component-cached-width
  ;; ──────────────────────────────

  (test-case "component-cached-width returns #f before render"
    (define comp (make-q-component (lambda (st w) '())))
    (check-false (component-cached-width comp)))

  (test-case "component-cached-width returns width after render"
    (define comp (make-q-component (lambda (st w) '())))
    (component-render comp (initial-ui-state) 120)
    (check-equal? (component-cached-width comp) 120))

  ;; ──────────────────────────────
  ;; component-invalidate!
  ;; ──────────────────────────────

  (test-case "component-invalidate! clears cache"
    (define comp
      (make-q-component
       (lambda (st w) (list (styled-line (list (styled-segment "x" '())))))
       #:id 'test))
    (component-render comp (initial-ui-state) 80)
    (check-equal? (component-cached-width comp) 80)
    (component-invalidate! comp)
    (check-false (component-cached-width comp)))

  (test-case "component-invalidate! calls custom invalidate-fn"
    (define invalidated? (box #f))
    (define comp
      (make-q-component
       (lambda (st w) '())
       #:id 'test
       #:invalidate-fn (lambda () (set-box! invalidated? #t))))
    (component-invalidate! comp)
    (check-true (unbox invalidated?)))

  (test-case "component-render re-computes after invalidate"
    (define call-count (box 0))
    (define comp
      (make-q-component
       (lambda (st w)
         (set-box! call-count (add1 (unbox call-count)))
         (list (styled-line (list (styled-segment "fresh" '())))))
       #:id 'test))
    (component-render comp (initial-ui-state) 80)
    (check-equal? (unbox call-count) 1)
    (component-invalidate! comp)
    (component-render comp (initial-ui-state) 80)
    (check-equal? (unbox call-count) 2))

  ;; ──────────────────────────────
  ;; component-compose
  ;; ──────────────────────────────

  (test-case "component-compose concatenates multiple components"
    (define comp-a
      (make-q-component
       (lambda (st w)
         (list (styled-line (list (styled-segment "A" '())))))
       #:id 'a))
    (define comp-b
      (make-q-component
       (lambda (st w)
         (list (styled-line (list (styled-segment "B" '())))))
       #:id 'b))
    (define result (component-compose (list comp-a comp-b) (initial-ui-state) 80))
    (check-equal? (length result) 2)
    (check-equal? (styled-line->text (car result)) "A")
    (check-equal? (styled-line->text (cadr result)) "B"))

  (test-case "component-compose with empty list returns empty"
    (define result (component-compose '() (initial-ui-state) 80))
    (check-equal? result '()))

  (test-case "component-compose respects caching per-component"
    (define a-count (box 0))
    (define b-count (box 0))
    (define comp-a
      (make-q-component
       (lambda (st w)
         (set-box! a-count (add1 (unbox a-count)))
         (list (styled-line (list (styled-segment "A" '())))))
       #:id 'a))
    (define comp-b
      (make-q-component
       (lambda (st w)
         (set-box! b-count (add1 (unbox b-count)))
         (list (styled-line (list (styled-segment "B" '())))))
       #:id 'b))
    ;; First compose — both miss
    (component-compose (list comp-a comp-b) (initial-ui-state) 80)
    (check-equal? (unbox a-count) 1)
    (check-equal? (unbox b-count) 1)
    ;; Second compose — both hit
    (component-compose (list comp-a comp-b) (initial-ui-state) 80)
    (check-equal? (unbox a-count) 1)
    (check-equal? (unbox b-count) 1))

  ;; ──────────────────────────────
  ;; Integration: component with real render functions
  ;; ──────────────────────────────

  (test-case "component wrapping render-status-bar"
    (define state (initial-ui-state #:session-id "test" #:model-name "gpt-4"))
    (define status-comp
      (make-q-component
       (lambda (st w) (list (render-status-bar st w)))
       #:id 'status-bar))
    (define result (component-render status-comp state 80))
    (check-equal? (length result) 1)
    (define text (styled-line->text (car result)))
    (check-true (string-contains? text "gpt-4")))
  )

(run-tests test-component)
