#lang racket

;; @speed fast
;; @suite default

;; q/tests/test-ui-dispatch.rkt — Tests for ui-core/dispatch

(require rackunit
         rackunit/text-ui
         "../ui-core/dispatch.rkt")

;; Helper: create a mock runtime that captures emitted events
(define (make-mock-runtime)
  (define events-box (box '()))
  (hash 'emit-event
        (lambda (evt) (set-box! events-box (append (unbox events-box) (list evt))))
        'events-box
        events-box))

(define (emitted-events runtime)
  (unbox (hash-ref runtime 'events-box)))

(define-test-suite test-ui-dispatch
                   (test-case "dispatch-submit! emits user.input event"
                     (define rt (make-mock-runtime))
                     (dispatch-submit! rt "hello world")
                     (define evts (emitted-events rt))
                     (check-equal? (length evts) 1)
                     (check-equal? (hash-ref (car evts) 'type) "user.input")
                     (check-equal? (hash-ref (car evts) 'text) "hello world"))
                   (test-case "dispatch-cancel! emits user.cancel event"
                     (define rt (make-mock-runtime))
                     (dispatch-cancel! rt)
                     (define evts (emitted-events rt))
                     (check-equal? (length evts) 1)
                     (check-equal? (hash-ref (car evts) 'type) "user.cancel"))
                   (test-case "dispatch-scroll! emits ui.scroll event with direction"
                     (define rt (make-mock-runtime))
                     (dispatch-scroll! rt 'down)
                     (define evts (emitted-events rt))
                     (check-equal? (length evts) 1)
                     (check-equal? (hash-ref (car evts) 'type) "ui.scroll")
                     (check-equal? (hash-ref (car evts) 'direction) 'down))
                   (test-case "dispatch-scroll! supports all directions"
                     (for ([dir '(up down top bottom)])
                       (define rt (make-mock-runtime))
                       (dispatch-scroll! rt dir)
                       (check-equal? (hash-ref (car (emitted-events rt)) 'direction) dir)))
                   (test-case "dispatch-command! emits user.command with name and args"
                     (define rt (make-mock-runtime))
                     (dispatch-command! rt "model" '("gpt-4"))
                     (define evts (emitted-events rt))
                     (check-equal? (length evts) 1)
                     (check-equal? (hash-ref (car evts) 'type) "user.command")
                     (check-equal? (hash-ref (car evts) 'command) "model")
                     (check-equal? (hash-ref (car evts) 'args) '("gpt-4")))
                   (test-case "dispatch-resize! emits ui.resize event"
                     (define rt (make-mock-runtime))
                     (dispatch-resize! rt 120 40)
                     (define evts (emitted-events rt))
                     (check-equal? (hash-ref (car evts) 'type) "ui.resize")
                     (check-equal? (hash-ref (car evts) 'cols) 120)
                     (check-equal? (hash-ref (car evts) 'rows) 40))
                   (test-case "dispatch-focus! emits ui.focus event"
                     (define rt (make-mock-runtime))
                     (dispatch-focus! rt 'transcript)
                     (define evts (emitted-events rt))
                     (check-equal? (hash-ref (car evts) 'type) "ui.focus")
                     (check-equal? (hash-ref (car evts) 'component) 'transcript))
                   (test-case "no emit-event in runtime — graceful no-op"
                     (define rt (hash))
                     ;; Should not error
                     (dispatch-submit! rt "test")
                     (dispatch-cancel! rt)
                     (dispatch-scroll! rt 'up)
                     (dispatch-command! rt "help" '())
                     (check-true #t)))

(run-tests test-ui-dispatch)
