#lang racket

;; @speed slow
;; @suite integration
;; @requires browser
;; @boundary integration

(require rackunit
         net/url
         racket/file
         racket/runtime-path
         "../browser/adapter.rkt"
         "../browser/adapters/playwright-sidecar.rkt"
         "../browser/types.rkt")

(define-runtime-path sidecar-path "../sidecars/playwright/q-playwright-sidecar.js")

(test-case "playwright adapter launches sidecar through PATH-resolved node and opens a real page"
  (check-not-false (find-executable-path "node") "browser profile requires node on PATH")
  (check-true (file-exists? sidecar-path) "Playwright sidecar script is missing")
  (define fixture-path (make-temporary-file "playwright-sidecar-fixture-~a.html"))
  (dynamic-wind
   (lambda ()
     (call-with-output-file fixture-path
                            #:exists 'truncate/replace
                            (lambda (out)
                              (display "<!doctype html><title>Hermetic Playwright Fixture</title>"
                                       out)
                              (display "<main><h1>Hermetic Playwright Fixture</h1></main>" out))))
   (lambda ()
     (define captured-stderr (open-output-string))
     (parameterize ([current-error-port captured-stderr])
       (define adapter (make-playwright-adapter (path->string sidecar-path) #:timeout-ms 30000))
       (define session-id "test-browser-playwright-sidecar")
       (define obs (browser-adapter-open adapter session-id (url->string (path->url fixture-path))))
       (check-equal? (browser-observation-title obs) "Hermetic Playwright Fixture")
       (check-true (regexp-match? #rx"Hermetic Playwright Fixture"
                                  (browser-observation-visible-text obs)))
       (define shot (browser-adapter-screenshot adapter session-id))
       (check-equal? (browser-observation-screenshot-mime shot) "image/png")
       (check-true (bytes? (browser-observation-screenshot-bytes shot)))
       (check-true (> (bytes-length (browser-observation-screenshot-bytes shot)) 100))
       (browser-adapter-close adapter session-id))
     (check-false (regexp-match? #rx"q-playwright-sidecar ready"
                                 (get-output-string captured-stderr))))
   (lambda () (delete-file fixture-path))))
