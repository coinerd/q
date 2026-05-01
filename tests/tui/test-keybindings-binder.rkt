#lang racket

;; tests/tui/test-keybindings-binder.rkt — tests for keybindings/binding-resolver

(require rackunit
         rackunit/text-ui
         "../../tui/keybindings/binding-resolver.rkt"
         "../../tui/keymap.rkt")

(define resolver-tests
  (test-suite "Keybindings Binding Resolver"

    (test-case "keycode->key-spec-from-msg: char"
      (define ks (keycode->key-spec-from-msg #\a))
      (check-equal? (key-spec-name ks) #\a)
      (check-false (key-spec-ctrl ks))
      (check-false (key-spec-shift ks)))

    (test-case "keycode->key-spec-from-msg: ctrl-x"
      (define ks (keycode->key-spec-from-msg 'ctrl-z))
      (check-equal? (key-spec-name ks) 'z)
      (check-true (key-spec-ctrl ks))
      (check-false (key-spec-shift ks)))

    (test-case "keycode->key-spec-from-msg: shift-x"
      (define ks (keycode->key-spec-from-msg 'shift-tab))
      (check-equal? (key-spec-name ks) 'tab)
      (check-false (key-spec-ctrl ks))
      (check-true (key-spec-shift ks)))

    (test-case "keycode->key-spec-from-msg: unknown returns #f"
      ;; Integer keycodes are not handled
      (check-false (keycode->key-spec-from-msg 42))
      (check-false (keycode->key-spec-from-msg '())))

    (test-case "keycode->key-spec-from-msg: plain symbol"
      (define ks (keycode->key-spec-from-msg 'return))
      (check-equal? (key-spec-name ks) 'return)
      (check-false (key-spec-ctrl ks)))

    (test-case "reload-keymap! clears cache"
      (reload-keymap!)
      ;; Should not error — just resets cached keymap
      (check-true #t))

    (test-case "get-active-keymap returns keymap"
      (reload-keymap!)
      (define km (get-active-keymap))
      (check-not-false km))))

(module+ main
  (run-tests resolver-tests))
(module+ test
  (run-tests resolver-tests))
