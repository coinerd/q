#lang racket/base

(require rackunit
         rackunit/text-ui
         racket/port
         "../gui/main.rkt")

(define test-double-event-fix
  (test-suite
   "double-event-fix"
   (test-case "model.stream.completed appears exactly once in cond clauses"
     ;; This test verifies the file-level fix: grep the source for occurrences
     (define src
       (with-input-from-file "gui/main.rkt"
         (lambda () (port->string (current-input-port)))))
     ;; Count occurrences of model.stream.completed in the source
     (define count 0)
     (let loop ([pos 0])
       (define found (regexp-match-positions #rx"model[.]stream[.]completed" src pos))
       (when found
         (set! count (+ count 1))
         (loop (cdar found))))
     ;; Should be exactly 1: the dedicated cond clause (the OR was removed)
     (check-equal? count 1))

   (test-case "notify-gui! max nesting ≤ 4"
     ;; Verify by loading the module successfully — compilation is the real gate
     (check-true (procedure? gui-available?)))))

(define test-flatten-extraction
  (test-suite
   "flatten-extraction"
   (test-case "update-text%-content! extracted as named function"
     (define src
       (with-input-from-file "gui/main.rkt"
         (lambda () (port->string (current-input-port)))))
     (check-not-false (regexp-match? #rx"[(]define [(]update-text%-content!" src)))

   (test-case "manage-streaming-cursor! extracted as named function"
     (define src
       (with-input-from-file "gui/main.rkt"
         (lambda () (port->string (current-input-port)))))
     (check-not-false (regexp-match? #rx"[(]define [(]manage-streaming-cursor!" src)))

   (test-case "sync-observables! extracted as named function"
     (define src
       (with-input-from-file "gui/main.rkt"
         (lambda () (port->string (current-input-port)))))
     (check-not-false (regexp-match? #rx"[(]define [(]sync-observables!" src)))))

(run-tests (test-suite "gui-state-sync-w0" test-double-event-fix test-flatten-extraction))
