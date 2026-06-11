#lang racket

;; test-browser-dom-tagging-regression.rkt — Regression tests for DOM tagging
;;
;; W3: Integration-level tests verifying observation round-trip and
;; tool handler exports work correctly with interactive-elements.

(require rackunit
         "../browser/types.rkt"
         "../tools/builtins/browser-tools.rkt")

;; ---------------------------------------------------------------------------
;; Test: browser-observation serialization includes interactive-elements key
;; ---------------------------------------------------------------------------

(test-case "observation serialization includes interactive-elements"
  (define obs (browser-observation
               "https://example.com" "Test" "text" "visible"
               #f #f #f #f '() '() (hash 'width 800 'height 600)
               (list (hasheq 'qId "0" 'tag "button" 'text "Click me"))
               #f))
  (define j (browser-observation->jsexpr obs))
  (check-true (hash-has-key? j 'interactive-elements))
  (define elems (hash-ref j 'interactive-elements))
  (check-equal? (length elems) 1)
  (check-equal? (hash-ref (car elems) 'qId) "0"))

;; ---------------------------------------------------------------------------
;; Test: round-trip preserves interactive-elements
;; ---------------------------------------------------------------------------

(test-case "round-trip preserves interactive-elements"
  (define elements (list (hasheq 'qId "0" 'tag "a" 'href "/home")
                         (hasheq 'qId "1" 'tag "input" 'type "text")))
  (define obs (browser-observation
               "https://example.com" "Test" "text" "visible"
               #f #f #f #f '() '() #f elements #f))
  (define j (browser-observation->jsexpr obs))
  (define obs2 (jsexpr->browser-observation j))
  (check-equal? (length (browser-observation-interactive-elements obs2)) 2)
  (check-equal? (hash-ref (car (browser-observation-interactive-elements obs2)) 'tag) "a")
  (check-equal? (hash-ref (cadr (browser-observation-interactive-elements obs2)) 'type) "text"))

;; ---------------------------------------------------------------------------
;; Test: handle-browser-screenshot exported as procedure
;; ---------------------------------------------------------------------------

(test-case "handle-browser-screenshot is a procedure"
  (check-pred procedure? handle-browser-screenshot))

;; ---------------------------------------------------------------------------
;; Test: empty interactive-elements backward compatible
;; ---------------------------------------------------------------------------

(test-case "empty interactive-elements is backward compatible"
  (define j (hasheq 'url "https://example.com"
                    'title "Test"
                    'text-content "text"
                    'visible-text "visible"))
  (define obs (jsexpr->browser-observation j))
  (check-equal? (browser-observation-interactive-elements obs) '()))
