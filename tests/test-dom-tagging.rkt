#lang racket

;; test-dom-tagging.rkt — DOM tagging & q-id annotation tests
;;
;; W0: Tests for the q-id injection feature in page-state.js.
;; Verifies the JS module exports the correct functions.

(require rackunit
         racket/port)

;; ---------------------------------------------------------------------------
;; W0: Verify JS source structure
;; ---------------------------------------------------------------------------

(test-case "page-state.js exports injectQIds"
  (define js-src (file->string "../sidecars/playwright/lib/page-state.js"))
  (check-true (string-contains? js-src "async function injectQIds")
              "injectQIds function must be defined")
  (check-true (string-contains? js-src "injectQIds")
              "module must export injectQIds"))

(test-case "page-state.js interactiveElements selector covers key elements"
  (define js-src (file->string "../sidecars/playwright/lib/page-state.js"))
  (check-true (string-contains? js-src "[onclick]")
              "Must select onclick elements")
  (check-true (string-contains? js-src "q-id")
              "Must set q-id attribute"))

(test-case "page-state.js caps elements at 200"
  (define js-src (file->string "../sidecars/playwright/lib/page-state.js"))
  (check-true (string-contains? js-src "MAX_ELEMENTS = 200")
              "Must cap at 200 elements"))

(test-case "page-state.js returns interactiveElements in extract"
  (define js-src (file->string "../sidecars/playwright/lib/page-state.js"))
  (check-true (string-contains? js-src "interactiveElements")
              "extract must return interactiveElements"))
