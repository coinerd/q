#lang racket/base

(require rackunit
         rackunit/text-ui
         racket/port
         "../gui/main.rkt"
         "../gui/state-sync.rkt")

(define (file-source path)
  (with-input-from-file path
    (lambda () (port->string (current-input-port)))))

(define main-src (file-source "gui/main.rkt"))
(define sync-src (file-source "gui/state-sync.rkt"))

(define test-double-event-fix
  (test-suite
   "double-event-fix"
   (test-case "model.stream.completed appears exactly once in state-sync.rkt cond clauses"
     (define count 0)
     (let loop ([pos 0])
       (define found (regexp-match-positions #rx"model[.]stream[.]completed" sync-src pos))
       (when found
         (set! count (+ count 1))
         (loop (cdar found))))
     ;; Should be exactly 1: the dedicated cond clause
     (check-equal? count 1))

   (test-case "turn.completed clause does NOT contain model.stream.completed"
     (check-false (regexp-match? #rx"turn[.]completed.*model[.]stream[.]completed" sync-src)))

   (test-case "make-gui-event-subscriber moved out of main.rkt"
     (check-false (regexp-match? #rx"[(]define [(]make-gui-event-subscriber" main-src)))

   (test-case "make-gui-event-subscriber exists in state-sync.rkt"
     (check-true (regexp-match? #rx"[(]define [(]make-gui-event-subscriber" sync-src)))))

(define test-flatten-extraction
  (test-suite
   "flatten-extraction"
   (test-case "make-notify-gui-callback exists in state-sync.rkt"
     (check-true (regexp-match? #rx"[(]define [(]make-notify-gui-callback" sync-src)))

   (test-case "gui-state-lock exists in state-sync.rkt"
     (check-true (regexp-match? #rx"[(]define gui-state-lock" sync-src)))

   (test-case "gui/main.rkt requires state-sync.rkt"
     (check-true (regexp-match? #rx"state-sync[.]rkt\"" main-src)))))

(run-tests (test-suite "gui-state-sync-w0" test-double-event-fix test-flatten-extraction))
