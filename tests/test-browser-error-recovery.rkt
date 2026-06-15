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

(test-case "browser-error-result-hint classifies 'not found' as open-session"
  (check-eq? (browser-error-result-hint (make-exn "session not found")) 'open-session))

(test-case "browser-error-result-hint classifies 'timed out' as retry"
  (check-eq? (browser-error-result-hint (make-exn "operation timed out")) 'retry))

(test-case "browser-error-result-hint classifies 'blocked' as policy"
  (check-eq? (browser-error-result-hint (make-exn "navigation blocked by policy")) 'policy))

(test-case "browser-error-result-hint classifies unknown errors as none"
  (check-eq? (browser-error-result-hint (make-exn "connection refused")) 'none))

(test-case "browser-error-result still includes human-readable hint text"
  (define r (browser-error-result "browser_observe" (make-exn "session not found")))
  (define text (result-text r))
  (check-true (string-contains? text "browser_open") "text output must still mention browser_open"))

(test-case "browser-error-result includes original error message"
  (define r (browser-error-result "browser_open" (make-exn "connection refused")))
  (define text (result-text r))
  (check-true (string-contains? text "connection refused")
              "original error message should be preserved"))
