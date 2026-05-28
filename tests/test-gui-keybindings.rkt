#lang racket/base

;; q/tests/test-gui-keybindings.rkt — Tests for keyboard shortcut registry
;;
;; Tests for gui/components/keybindings.rkt

(require rackunit
         rackunit/text-ui
         "../gui/components/keybindings.rkt")

;; ── lookup-keybinding tests ──

(define-test-suite test-lookup-keybinding

  (test-case "finds ctrl+l"
    (check equal? (lookup-keybinding #\l #t) 'clear))

  (test-case "finds ctrl+c"
    (check equal? (lookup-keybinding #\c #t) 'interrupt))

  (test-case "finds ctrl+s"
    (check equal? (lookup-keybinding #\s #t) 'save))

  (test-case "ignores non-ctrl keys"
    (check equal? (lookup-keybinding #\l #f) #f))

  (test-case "returns #f for unbound key"
    (check equal? (lookup-keybinding #\a #t) #f)))

(run-tests test-lookup-keybinding)

;; ── key-event->action tests ──

(define-test-suite test-key-event->action

  (test-case "returns action for ctrl+key"
    (check equal? (key-event->action #\k #t) 'compact))

  (test-case "returns #f without ctrl"
    (check equal? (key-event->action #\k #f) #f)))

(run-tests test-key-event->action)

;; ── list-keybindings tests ──

(define-test-suite test-list-keybindings

  (test-case "returns all bindings as pairs"
    (define result (list-keybindings))
    (check = (length result) 5)
    (check-not-false (assoc #\l result))
    (check-not-false (assoc #\k result))
    (check-not-false (assoc #\c result))
    (check-not-false (assoc #\s result))
    (check-not-false (assoc #\q result)))

  (test-case "all values are symbols"
    (for ([pair (in-list (list-keybindings))])
      (check-pred symbol? (cdr pair)))))

(run-tests test-list-keybindings)

;; ── default-keybindings tests ──

(define-test-suite test-default-keybindings

  (test-case "is a hash"
    (check-pred hash? default-keybindings))

  (test-case "has expected bindings"
    (check equal? (hash-ref default-keybindings #\l) 'clear)
    (check equal? (hash-ref default-keybindings #\k) 'compact)
    (check equal? (hash-ref default-keybindings #\c) 'interrupt)
    (check equal? (hash-ref default-keybindings #\s) 'save)
    (check equal? (hash-ref default-keybindings #\q) 'quit)))

(run-tests test-default-keybindings)
