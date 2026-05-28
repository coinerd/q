#lang racket

;; q/tests/test-gui-status.rkt — Tests for gui/views/status.rkt

(require rackunit
         rackunit/text-ui
         "../ui-core/theme-protocol.rkt"
         "../gui/views/status.rkt")

(define-test-suite test-gui-status
                   (test-case "status-text maps symbols to display strings"
                     (check-equal? (status-text 'idle) "Ready")
                     (check-equal? (status-text 'processing) "Processing...")
                     (check-equal? (status-text 'error) "Error")
                     (check-equal? (status-text 'cancelled) "Cancelled"))
                   (test-case "status-text passes through strings"
                     (check-equal? (status-text "Custom status") "Custom status"))
                   (test-case "status-text returns Ready for unknown symbols"
                     (check-equal? (status-text 'unknown) "Ready"))
                   (test-case "render-status-bar produces hash with required keys"
                     (define result (render-status-bar (default-theme)))
                     (check-equal? (hash-ref result 'view) 'status-bar)
                     (check-true (string? (hash-ref result 'left)))
                     (check-true (string? (hash-ref result 'right))))
                   (test-case "render-status-bar includes model name"
                     (define result (render-status-bar (default-theme) #:model "gpt-4"))
                     (check-true (string-contains? (hash-ref result 'left) "gpt-4")))
                   (test-case "render-status-bar includes turn count"
                     (define result (render-status-bar (default-theme) #:turn 5))
                     (check-true (string-contains? (hash-ref result 'right) "Turn: 5")))
                   (test-case "render-status-bar includes token count when provided"
                     (define result (render-status-bar (default-theme) #:tokens 1234))
                     (check-true (string-contains? (hash-ref result 'right) "Tokens: 1234")))
                   (test-case "render-status-bar omits tokens when #f"
                     (define result (render-status-bar (default-theme) #:tokens #f))
                     (check-false (string-contains? (hash-ref result 'right) "Tokens:"))))

(run-tests test-gui-status)
