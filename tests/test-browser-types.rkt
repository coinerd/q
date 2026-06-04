#lang racket

;; tests/test-browser-types.rkt — Browser domain type tests
;;
;; Tests for browser/types.rkt: construction, serialization, edge cases.

(require rackunit
         "../browser/types.rkt")

;; ---------------------------------------------------------------------------
;; browser-target
;; ---------------------------------------------------------------------------

(test-case "browser-target construction"
  (define t (browser-target "https://example.com" #f 1280 720))
  (check-equal? (browser-target-url t) "https://example.com")
  (check-equal? (browser-target-viewport-width t) 1280)
  (check-equal? (browser-target-viewport-height t) 720))

(test-case "browser-target default viewport"
  (define j (hasheq 'url "https://example.com"))
  (define t (jsexpr->browser-target j))
  (check-equal? (browser-target-viewport-width t) 1280)
  (check-equal? (browser-target-viewport-height t) 720))

(test-case "browser-target JSON roundtrip"
  (define t (browser-target "https://example.com/path" "tab-1" 1024 768))
  (define rt (jsexpr->browser-target (browser-target->jsexpr t)))
  (check-equal? (browser-target-url rt) "https://example.com/path")
  (check-equal? (browser-target-tab-selector rt) "tab-1")
  (check-equal? (browser-target-viewport-width rt) 1024)
  (check-equal? (browser-target-viewport-height rt) 768))

;; ---------------------------------------------------------------------------
;; browser-action variants
;; ---------------------------------------------------------------------------

(test-case "browser-action-navigate"
  (define a (browser-action-navigate "https://example.com" "load"))
  (check-true (browser-action? a))
  (check-equal? (browser-action-navigate-url a) "https://example.com"))

(test-case "browser-action-click"
  (define a (browser-action-click "#btn" "left"))
  (check-true (browser-action? a))
  (check-equal? (browser-action-click-selector a) "#btn"))

(test-case "browser-action-type"
  (define a (browser-action-type "#input" "hello" #t))
  (check-true (browser-action? a))
  (check-equal? (browser-action-type-text a) "hello")
  (check-true (browser-action-type-clear-first? a)))

(test-case "browser-action-press"
  (define a (browser-action-press "Enter" '()))
  (check-true (browser-action? a)))

(test-case "browser-action-extract"
  (define a (browser-action-extract "h1" "text"))
  (check-true (browser-action? a)))

(test-case "browser-action-screenshot"
  (define a (browser-action-screenshot #f #t))
  (check-true (browser-action? a)))

(test-case "browser-action-scroll"
  (define a (browser-action-scroll "down" 300))
  (check-true (browser-action? a)))

(test-case "browser-action-wait"
  (define a (browser-action-wait "#loaded" 5000))
  (check-true (browser-action? a)))

;; ---------------------------------------------------------------------------
;; browser-action JSON roundtrip
;; ---------------------------------------------------------------------------

(test-case "navigate roundtrip"
  (define a (browser-action-navigate "https://example.com" "domcontentloaded"))
  (define rt (jsexpr->browser-action (browser-action->jsexpr a)))
  (check-true (browser-action-navigate? rt))
  (check-equal? (browser-action-navigate-url rt) "https://example.com"))

(test-case "click roundtrip"
  (define a (browser-action-click "#submit" "left"))
  (define rt (jsexpr->browser-action (browser-action->jsexpr a)))
  (check-true (browser-action-click? rt))
  (check-equal? (browser-action-click-selector rt) "#submit"))

(test-case "type roundtrip"
  (define a (browser-action-type "#search" "query" #t))
  (define rt (jsexpr->browser-action (browser-action->jsexpr a)))
  (check-true (browser-action-type? rt))
  (check-equal? (browser-action-type-text rt) "query")
  (check-true (browser-action-type-clear-first? rt)))

;; ---------------------------------------------------------------------------
;; browser-observation
;; ---------------------------------------------------------------------------

(test-case "browser-observation construction"
  (define o
    (browser-observation "https://example.com"
                         "Example"
                         "full text"
                         "visible"
                         "<html>...</html>"
                         "tree"
                         #f
                         #f
                         '()
                         '()
                         #f
                         (hasheq)))
  (check-equal? (browser-observation-url o) "https://example.com")
  (check-equal? (browser-observation-title o) "Example"))

(test-case "browser-observation JSON roundtrip (no screenshot)"
  (define o
    (browser-observation "https://example.com"
                         "Title"
                         "text"
                         "visible"
                         "dom"
                         "a11y"
                         #f
                         #f
                         '()
                         '()
                         (hasheq 'width 1280 'height 720)
                         (hasheq 'key "value")))
  (define rt (jsexpr->browser-observation (browser-observation->jsexpr o)))
  (check-equal? (browser-observation-url rt) "https://example.com")
  (check-equal? (browser-observation-title rt) "Title")
  (check-false (browser-observation-screenshot-bytes rt)))

(test-case "browser-observation JSON roundtrip (with screenshot)"
  (define o
    (browser-observation "https://example.com"
                         "Title"
                         "text"
                         "visible"
                         "dom"
                         "a11y"
                         "image/png"
                         #"~PNGfake"
                         '()
                         '()
                         #f
                         (hasheq)))
  (define rt (jsexpr->browser-observation (browser-observation->jsexpr o)))
  (check-equal? (browser-observation-url rt) "https://example.com")
  (check-equal? (browser-observation-screenshot-mime rt) "image/png")
  (check-true (bytes? (browser-observation-screenshot-bytes rt))))

;; ---------------------------------------------------------------------------
;; browser-session-info
;; ---------------------------------------------------------------------------

(test-case "browser-session-info construction"
  (define s (browser-session-info "sess-1" "active" 1000 2000 "ephemeral" "/tmp/q-sess"))
  (check-equal? (browser-session-info-id s) "sess-1")
  (check-equal? (browser-session-info-status s) "active"))

(test-case "browser-session-info JSON roundtrip"
  (define s (browser-session-info "sess-2" "active" 1000 2000 "ephemeral" "/tmp/q"))
  (define rt (jsexpr->browser-session-info (browser-session-info->jsexpr s)))
  (check-equal? (browser-session-info-id rt) "sess-2")
  (check-equal? (browser-session-info-status rt) "active")
  (check-equal? (browser-session-info-profile-kind rt) "ephemeral"))
