#lang racket

;; test-browser-tool-descriptions.rkt — Verify tool descriptions mention q-id
;;
;; W2: Tool description updates for q-id selector support.

(require rackunit
         "../tools/registry-table/spec.rkt"
         "../tools/registry-table/browser-tools.rkt")

;; ---------------------------------------------------------------------------
;; Test: tool descriptions mention q-id
;; ---------------------------------------------------------------------------

(test-case "browser_observe description mentions interactiveElements"
  (define obs-spec
    (findf (lambda (s) (equal? (tool-spec-name s) "browser_observe")) browser-tool-specs))
  (check-true (string-contains? (tool-spec-description obs-spec) "interactiveElements")))

(test-case "browser_click description mentions q-id"
  (define click-spec
    (findf (lambda (s) (equal? (tool-spec-name s) "browser_click")) browser-tool-specs))
  (check-true (string-contains? (tool-spec-description click-spec) "q-id")))

(test-case "browser_type description mentions q-id"
  (define type-spec
    (findf (lambda (s) (equal? (tool-spec-name s) "browser_type")) browser-tool-specs))
  (check-true (string-contains? (tool-spec-description type-spec) "q-id")))

(test-case "browser_extract description mentions q-id"
  (define extract-spec
    (findf (lambda (s) (equal? (tool-spec-name s) "browser_extract")) browser-tool-specs))
  (check-true (string-contains? (tool-spec-description extract-spec) "q-id")))

(test-case "browser_click selector param mentions q-id"
  (define click-spec
    (findf (lambda (s) (equal? (tool-spec-name s) "browser_click")) browser-tool-specs))
  (define props (hash-ref (tool-spec-schema click-spec) 'properties))
  (define sel-desc (hash-ref (hash-ref props 'selector) 'description))
  (check-true (string-contains? sel-desc "q-id")))
