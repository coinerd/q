#lang racket/base

;; @speed fast
;; @suite default
;; BOUNDARY: unit

;; tests/test-browser-error-recovery.rkt
;; Verify browser tool error results include recovery hints.

(require rackunit
         racket/string
         "../tools/builtins/browser-tools.rkt"
         "../tools/tool.rkt")

(displayln "test-browser-error-recovery.rkt loaded")

(define (make-exn msg)
  (exn:fail msg (continuation-marks #f)))

(define (result-text r)
  (define content (tool-result-content r))
  (if (and (list? content) (not (null? content)))
      (hash-ref (car content) 'text "")
      ""))

(test-case "browser-error-result appends session hint for 'not found'"
  (define r (browser-error-result "browser_observe" (make-exn "session not found")))
  (define text (result-text r))
  (check-true (string-contains? text "browser_open") "hint should mention browser_open"))

(test-case "browser-error-result appends timeout hint"
  (define r (browser-error-result "browser_click" (make-exn "operation timed out")))
  (define text (result-text r))
  (check-true (string-contains? text "slow to respond") "hint should mention slow response"))

(test-case "browser-error-result appends blocked hint"
  (define r (browser-error-result "browser_open" (make-exn "navigation blocked by policy")))
  (define text (result-text r))
  (check-true (string-contains? text "blocked by browser policy")
              "hint should mention browser policy"))

(test-case "browser-error-result includes original error message"
  (define r (browser-error-result "browser_open" (make-exn "connection refused")))
  (define text (result-text r))
  (check-true (string-contains? text "connection refused")
              "original error message should be preserved"))
