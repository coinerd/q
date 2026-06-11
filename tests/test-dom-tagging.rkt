#lang racket

;; test-dom-tagging.rkt — DOM tagging & q-id annotation tests
;;
;; W0+W1: Tests for q-id injection in page-state.js and
;; Racket-side observation parser for interactive-elements.

(require rackunit
         racket/port
         "../browser/types.rkt")

;; ---------------------------------------------------------------------------
;; W0: Verify JS source structure
;; ---------------------------------------------------------------------------

(test-case "page-state.js exports injectQIds"
  (define js-src (file->string "../sidecars/playwright/lib/page-state.js"))
  (check-true (string-contains? js-src "async function injectQIds"))
  (check-true (string-contains? js-src "injectQIds")))

(test-case "page-state.js caps elements at 200"
  (define js-src (file->string "../sidecars/playwright/lib/page-state.js"))
  (check-true (string-contains? js-src "MAX_ELEMENTS = 200")))

(test-case "page-state.js returns interactiveElements in extract"
  (define js-src (file->string "../sidecars/playwright/lib/page-state.js"))
  (check-true (string-contains? js-src "interactiveElements")))

;; ---------------------------------------------------------------------------
;; W1: Racket-side observation parser for interactive-elements
;; ---------------------------------------------------------------------------

(test-case "browser-observation has interactive-elements field"
  (define obs (browser-observation
               "https://example.com" "Example" "text" "visible"
               #f #f #f #f '() '() (hash 'width 1280 'height 720)
               '() #f))
  (check-equal? (browser-observation-interactive-elements obs) '()))

(test-case "observation with interactive-elements round-trips through serialization"
  (define elements (list (hasheq 'qId "0" 'tag "button" 'text "Submit")
                         (hasheq 'qId "1" 'tag "a" 'text "Link" 'href "/about")))
  (define obs (browser-observation
               "https://example.com" "Example" "text" "visible"
               #f #f #f #f '() '() (hash 'width 1280 'height 720)
               elements #f))
  ;; Serialize and deserialize
  (define j (browser-observation->jsexpr obs))
  (define obs2 (jsexpr->browser-observation j))
  (check-equal? (length (browser-observation-interactive-elements obs2)) 2)
  (check-equal? (hash-ref (car (browser-observation-interactive-elements obs2)) 'qId) "0")
  (check-equal? (hash-ref (cadr (browser-observation-interactive-elements obs2)) 'tag) "a"))

(test-case "observation without interactive-elements is backward compatible"
  (define j (hasheq 'url "https://example.com"
                    'title "Example"
                    'text-content "text"
                    'visible-text "visible"))
  (define obs (jsexpr->browser-observation j))
  (check-equal? (browser-observation-interactive-elements obs) '()))
