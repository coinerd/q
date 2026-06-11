#lang racket

;; test-browser-vision-tool.rkt — Screenshot vision dual-path tests

(require rackunit
         "../tools/builtins/browser-tools.rkt"
         "../browser/settings.rkt"
         "../util/content/content-parts.rkt")

;; ---------------------------------------------------------------------------
;; Test: vision-enabled? is a boolean setting
;; ---------------------------------------------------------------------------

(test-case "vision-enabled? is accessible from browser-settings"
  (define s (default-browser-settings))
  (check-false (browser-settings-vision-enabled? s)))

;; ---------------------------------------------------------------------------
;; Test: make-image-part produces image-part
;; ---------------------------------------------------------------------------

(test-case "make-image-part produces correct image-part"
  (define ip (make-image-part "image/png" "base64data" "high"))
  (check-pred image-part? ip)
  (check-equal? (image-part-mime-type ip) "image/png")
  (check-equal? (image-part-data ip) "base64data")
  (check-equal? (image-part-detail ip) "high"))

;; ---------------------------------------------------------------------------
;; Test: handle-browser-screenshot is exported
;; ---------------------------------------------------------------------------

(test-case "handle-browser-screenshot is exported"
  (check-pred procedure? handle-browser-screenshot))
