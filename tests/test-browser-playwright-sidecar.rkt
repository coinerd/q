#lang racket

;; @speed fast
;; @suite default

(require rackunit
         racket/runtime-path
         "../browser/adapter.rkt"
         "../browser/adapters/playwright-sidecar.rkt"
         "../browser/types.rkt")

(define-runtime-path sidecar-path "../sidecars/playwright/q-playwright-sidecar.js")

(define node-available? (and (find-executable-path "node") #t))
(define sidecar-available? (file-exists? sidecar-path))

(when (and node-available? sidecar-available?)
  (test-case "playwright adapter launches sidecar through PATH-resolved node and opens a real page"
    (define captured-stderr (open-output-string))
    (parameterize ([current-error-port captured-stderr])
      (define adapter (make-playwright-adapter (path->string sidecar-path) #:timeout-ms 30000))
      (define session-id "test-browser-playwright-sidecar")
      (define obs (browser-adapter-open adapter session-id "https://example.com"))
      (check-equal? (browser-observation-title obs) "Example Domain")
      (check-true (regexp-match? #rx"Example Domain" (browser-observation-visible-text obs)))
      (define shot (browser-adapter-screenshot adapter session-id))
      (check-equal? (browser-observation-screenshot-mime shot) "image/png")
      (check-true (string? (browser-observation-screenshot-bytes shot)))
      (check-true (> (string-length (browser-observation-screenshot-bytes shot)) 100))
      (browser-adapter-close adapter session-id))
    (check-false (regexp-match? #rx"q-playwright-sidecar ready"
                                (get-output-string captured-stderr)))))
