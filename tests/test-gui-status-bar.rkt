#lang racket

;; @speed fast
;; @suite default

(require rackunit
         rackunit/text-ui
         "../gui/views/status.rkt"
         "../ui-core/theme-protocol.rkt")

(define theme (default-theme))

(run-tests
 (test-suite
  "gui-status-bar"

  (test-case "basic status without extras"
    (define result (render-status-bar theme #:status 'idle))
    (check-not-false (hash-ref result 'left))
    (check-true (string-contains? (hash-ref result 'left) "Ready")))

  (test-case "status with context percent"
    (define result (render-status-bar theme #:status 'processing #:context-percent 45))
    (check-true (string-contains? (hash-ref result 'left) "ctx:45%")))

  (test-case "status with model name"
    (define result (render-status-bar theme #:status 'processing #:model "gpt-4"))
    (check-true (string-contains? (hash-ref result 'left) "gpt-4")))

  (test-case "status with active goal"
    (define result (render-status-bar theme #:status 'processing #:active-goal "Fix the bug"))
    (check-true (string-contains? (hash-ref result 'left) "Goal: Fix the bug")))

  (test-case "long goal truncated"
    (define long-goal (make-string 50 #\X))
    (define result (render-status-bar theme #:status 'processing #:active-goal long-goal))
    (check-false (string-contains? (hash-ref result 'left) (make-string 50 #\X)))
    (check-true (string-contains? (hash-ref result 'left) "Goal:")))

  (test-case "format-status-string combines all parts"
    (define s (format-status-string "q" 'processing
                                    #:model "gpt-4"
                                    #:context-percent 72
                                    #:active-goal "test"))
    (check-true (string-contains? s "gpt-4"))
    (check-true (string-contains? s "Processing"))
    (check-true (string-contains? s "ctx:72%"))
    (check-true (string-contains? s "Goal: test")))

  (test-case "format-status-string without extras"
    (define s (format-status-string "q" 'idle))
    (check-true (string-contains? s "Ready")))

  (test-case "error status"
    (define result (render-status-bar theme #:status 'error))
    (check-true (string-contains? (hash-ref result 'left) "Error")))))
