#lang racket

;; @speed fast
;; @suite default

;; q/tests/test-gui-input.rkt — Tests for gui/views/input.rkt

(require rackunit
         rackunit/text-ui
         "../ui-core/theme-protocol.rkt"
         "../gui/views/input.rkt")

(define-test-suite
 test-gui-input
 ;; ── Pure text manipulation ──
 (test-case "input-insert-text at cursor"
   (check-equal? (input-insert-text "hello" 2 "XY") "heXYllo"))
 (test-case "input-insert-text at beginning"
   (check-equal? (input-insert-text "hello" 0 ">>") ">>hello"))
 (test-case "input-insert-text at end"
   (check-equal? (input-insert-text "hello" 5 "!") "hello!"))
 (test-case "input-delete-char removes char before cursor"
   (check-equal? (input-delete-char "hello" 2) "hllo"))
 (test-case "input-delete-char at cursor 0 is no-op"
   (check-equal? (input-delete-char "hello" 0) "hello"))
 (test-case "input-delete-char on empty string is no-op"
   (check-equal? (input-delete-char "" 0) ""))
 (test-case "input-move-cursor left"
   (check-equal? (input-move-cursor "hello" 3 'left) 2))
 (test-case "input-move-cursor right"
   (check-equal? (input-move-cursor "hello" 3 'right) 4))
 (test-case "input-move-cursor home"
   (check-equal? (input-move-cursor "hello" 3 'home) 0))
 (test-case "input-move-cursor end"
   (check-equal? (input-move-cursor "hello" 0 'end) 5))
 (test-case "input-move-cursor left at 0 stays at 0"
   (check-equal? (input-move-cursor "hello" 0 'left) 0))
 ;; ── Render ──
 (test-case "render-input-area produces view descriptor"
   (define result (render-input-area (default-theme)))
   (check-equal? (hash-ref result 'view) 'input-area)
   (check-equal? (hash-ref result 'text) "")
   (check-equal? (hash-ref result 'cursor) 0)
   (check-equal? (hash-ref result 'prompt) "> ")
   (check-true (hash-ref result 'focused)))
 (test-case "render-input-area with text and cursor"
   (define result (render-input-area (default-theme) #:text "hello" #:cursor 3))
   (check-equal? (hash-ref result 'text) "hello")
   (check-equal? (hash-ref result 'cursor) 3)
   (check-true (hash-ref result 'has-content)))
 (test-case "render-input-area shows placeholder when empty"
   (define result (render-input-area (default-theme) #:placeholder "Type here..."))
   (check-equal? (hash-ref result 'text) "Type here..."))
 (test-case "render-input-area shows text when not empty"
   (define result (render-input-area (default-theme) #:text "hello" #:placeholder "Type here..."))
   (check-equal? (hash-ref result 'text) "hello")))

(run-tests test-gui-input)
