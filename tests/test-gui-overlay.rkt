#lang racket

;; @speed fast
;; @suite default

;; q/tests/test-gui-overlay.rkt — Tests for gui/views/overlay.rkt

(require rackunit
         rackunit/text-ui
         "../ui-core/theme-protocol.rkt"
         "../gui/views/overlay.rkt")

(define-test-suite
 test-gui-overlay
 (test-case "render-alert produces alert view"
   (define result (render-alert (default-theme) "Something happened"))
   (check-equal? (hash-ref result 'view) 'alert)
   (check-equal? (hash-ref result 'message) "Something happened")
   (check-equal? (hash-ref result 'severity) 'info))
 (test-case "render-alert with title and severity"
   (define result (render-alert (default-theme) "Error!" #:title "Oops" #:severity 'error))
   (check-equal? (hash-ref result 'title) "Oops")
   (check-equal? (hash-ref result 'severity) 'error)
   (check-equal? (hash-ref result 'color) "#f38ba8"))
 (test-case "render-confirm produces confirm view"
   (define result (render-confirm (default-theme) "Are you sure?"))
   (check-equal? (hash-ref result 'view) 'confirm)
   (check-equal? (hash-ref result 'message) "Are you sure?")
   (check-equal? (hash-ref result 'confirm-text) "OK")
   (check-equal? (hash-ref result 'cancel-text) "Cancel"))
 (test-case "render-confirm with custom button text"
   (define result
     (render-confirm (default-theme) "Delete?" #:confirm-text "Delete" #:cancel-text "Keep"))
   (check-equal? (hash-ref result 'confirm-text) "Delete")
   (check-equal? (hash-ref result 'cancel-text) "Keep"))
 (test-case "render-prompt produces prompt view"
   (define result (render-prompt (default-theme) "Enter your name:"))
   (check-equal? (hash-ref result 'view) 'prompt)
   (check-equal? (hash-ref result 'message) "Enter your name:")
   (check-equal? (hash-ref result 'placeholder) "Type here..."))
 (test-case "render-prompt with default value"
   (define result (render-prompt (default-theme) "Name:" #:default-value "Alice"))
   (check-equal? (hash-ref result 'default-value) "Alice")))

(run-tests test-gui-overlay)
