#lang racket

;; tests/tui/keymap.rkt — Tests for configurable keybindings (Issue #411)

(require rackunit
         rackunit/text-ui
         "../../../q/tui/keymap.rkt")

(define keymap-tests
  (test-suite
   "Configurable keybindings"

   (test-case "key-spec creation and access"
     (define ks (key-spec #\a #t #f #f))
     (check-equal? (key-spec-name ks) #\a)
     (check-true (key-spec-ctrl ks))
     (check-false (key-spec-shift ks))
     (check-false (key-spec-alt ks)))

   (test-case "keycode->key-spec"
     (define ks (keycode->key-spec 'up #:ctrl #t))
     (check-equal? (key-spec-name ks) 'up)
     (check-true (key-spec-ctrl ks)))

   (test-case "key-spec equality"
     (define a (key-spec #\a #t #f #f))
     (define b (key-spec #\a #t #f #f))
     (define c (key-spec #\a #f #t #f))
     (check-true (key-spec=? a b))
     (check-false (key-spec=? a c)))

   (test-case "keymap-add! and lookup"
     (define km (make-keymap))
     (keymap-add! km (key-spec 'up #f #f #f) 'scroll-up)
     (check-equal? (keymap-lookup km (key-spec 'up #f #f #f)) 'scroll-up)
     (check-false (keymap-lookup km (key-spec 'down #f #f #f))))

   (test-case "keymap-add! overwrites existing"
     (define km (make-keymap))
     (keymap-add! km (key-spec #\a #t #f #f) 'select-all)
     (keymap-add! km (key-spec #\a #t #f #f) 'new-action)
     (check-equal? (keymap-lookup km (key-spec #\a #t #f #f)) 'new-action))

   (test-case "keymap-remove!"
     (define km (make-keymap))
     (keymap-add! km (key-spec #\x #t #f #f) 'cut)
     (keymap-remove! km (key-spec #\x #t #f #f))
     (check-false (keymap-lookup km (key-spec #\x #t #f #f))))

   (test-case "keymap-list returns entries"
     (define km (make-keymap))
     (keymap-add! km (key-spec 'up #f #f #f) 'scroll-up)
     (keymap-add! km (key-spec 'down #f #f #f) 'scroll-down)
     (check-equal? (length (keymap-list km)) 2))

   (test-case "keymap-find-conflicts returns empty for clean keymap"
     (define km (make-keymap))
     (keymap-add! km (key-spec 'up #f #f #f) 'scroll-up)
     (keymap-add! km (key-spec 'down #f #f #f) 'scroll-down)
     (check-equal? (keymap-find-conflicts km) '()))

   (test-case "keymap-merge combines keymaps"
     (define base (make-keymap))
     (keymap-add! base (key-spec 'up #f #f #f) 'scroll-up)
     (define override (make-keymap))
     (keymap-add! override (key-spec #\j #f #f #f) 'scroll-down)
     (keymap-merge base override)
     (check-equal? (keymap-lookup base (key-spec #\j #f #f #f)) 'scroll-down)
     ;; base keeps its own binding
     (check-equal? (keymap-lookup base (key-spec 'up #f #f #f)) 'scroll-up))

   (test-case "default-keymap has standard bindings"
     (define km (default-keymap))
     (check-equal? (keymap-lookup km (key-spec 'up #f #f #f)) 'scroll-up)
     (check-equal? (keymap-lookup km (key-spec 'return #f #f #f)) 'submit)
     (check-equal? (keymap-lookup km (key-spec #\c #t #f #f)) 'copy))

   (test-case "parse-key-string handles C-a"
     (define ks (parse-key-string "C-a"))
     (check-equal? (key-spec-name ks) #\a)
     (check-true (key-spec-ctrl ks))
     (check-false (key-spec-alt ks)))

   (test-case "parse-key-string handles C-M-x"
     (define ks (parse-key-string "C-M-x"))
     (check-equal? (key-spec-name ks) #\x)
     (check-true (key-spec-ctrl ks))
     (check-true (key-spec-alt ks)))

   (test-case "parse-key-string handles up"
     (define ks (parse-key-string "up"))
     (check-equal? (key-spec-name ks) 'up))

   (test-case "parse-key-string handles S-right"
     (define ks (parse-key-string "S-right"))
     (check-equal? (key-spec-name ks) 'right)
     (check-true (key-spec-shift ks)))

   (test-case "key-spec->keycode produces readable name"
     (define ks (key-spec #\a #t #f #f))
     (check-equal? (key-spec->keycode ks) '|C-a|))
   ))

(run-tests keymap-tests)
