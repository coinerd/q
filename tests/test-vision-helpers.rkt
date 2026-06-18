#lang racket

;; tests/test-vision-helpers.rkt — Unit tests for llm/vision-helpers.rkt (NF-10/F-07)

(require rackunit
         rackunit/text-ui
         "../llm/vision-helpers.rkt")

;; ---------------------------------------------------------------------------
;; parse-data-url
;; ---------------------------------------------------------------------------

(define vision-helpers-tests
  (test-suite "vision-helpers: parse-data-url"

    (test-case "valid JPEG data URL"
      (define-values (mime data) (parse-data-url "data:image/jpeg;base64,/9j/4AAQSkZJRg=="))
      (check-equal? mime "image/jpeg")
      (check-equal? data "/9j/4AAQSkZJRg=="))

    (test-case "valid PNG data URL"
      (define-values (mime data) (parse-data-url "data:image/png;base64,iVBORw0KGgo="))
      (check-equal? mime "image/png")
      (check-equal? data "iVBORw0KGgo="))

    (test-case "non-data URL falls back to image/png + raw URL"
      (define-values (mime data) (parse-data-url "https://example.com/image.jpg"))
      (check-equal? mime "image/png")
      (check-equal? data "https://example.com/image.jpg"))

    (test-case "empty string falls back"
      (define-values (mime data) (parse-data-url ""))
      (check-equal? mime "image/png")
      (check-equal? data ""))

    (test-case "data URL without base64 marker falls back"
      (define-values (mime data) (parse-data-url "data:text/html,hello"))
      (check-equal? mime "image/png")
      (check-equal? data "data:text/html,hello"))

    (test-case "webp MIME type preserved"
      (define-values (mime data) (parse-data-url "data:image/webp;base64,UklGRiQ="))
      (check-equal? mime "image/webp")
      (check-equal? data "UklGRiQ="))))

(run-tests vision-helpers-tests)
