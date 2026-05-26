#lang racket

;; tests/test-image-format-contracts.rkt — Contract enforcement for image-format.rkt (#5466)
;;
;; Tests that image format public APIs enforce contracts on invalid inputs.

(require rackunit
         rackunit/text-ui
         "../extensions/image-format.rkt")

(define format-contract-tests
  (test-suite "Image format contracts (#5466)"

    ;; ============================================================
    ;; Positive tests — valid inputs produce hashes
    ;; ============================================================

    (test-case "format-image-openai with valid inputs returns hash"
      (define result (format-image-openai "dGVzdA==" "image/png"))
      (check-true (hash? result))
      (check-equal? (hash-ref result 'type) "image_url"))

    (test-case "format-image-anthropic with valid inputs returns hash"
      (define result (format-image-anthropic "dGVzdA==" "image/jpeg"))
      (check-true (hash? result))
      (check-equal? (hash-ref result 'type) "image"))

    (test-case "format-image-gemini with valid inputs returns hash"
      (define result (format-image-gemini "dGVzdA==" "image/webp"))
      (check-true (hash? result))
      (check-equal? (hash-ref result 'type) "inline_data"))

    (test-case "format-image-for-provider dispatches correctly"
      (define openai-result (format-image-for-provider 'openai "dGVzdA==" "image/png"))
      (check-true (hash? openai-result))
      (define anthropic-result (format-image-for-provider 'anthropic "dGVzdA==" "image/png"))
      (check-true (hash? anthropic-result))
      (define gemini-result (format-image-for-provider 'gemini "dGVzdA==" "image/png"))
      (check-true (hash? gemini-result))
      ;; Unknown provider defaults to openai format
      (define unknown-result (format-image-for-provider 'unknown "dGVzdA==" "image/png"))
      (check-true (hash? unknown-result)))

    ;; ============================================================
    ;; Negative contract tests — invalid inputs raise errors
    ;; ============================================================

    (test-case "format-image-openai rejects non-string base64 (#5466)"
      (check-exn exn:fail:contract? (lambda () (format-image-openai 123 "image/png"))))

    (test-case "format-image-openai rejects non-string media-type (#5466)"
      (check-exn exn:fail:contract? (lambda () (format-image-openai "dGVzdA==" 42))))

    (test-case "format-image-anthropic rejects non-string base64 (#5466)"
      (check-exn exn:fail:contract? (lambda () (format-image-anthropic 'bad "image/png"))))

    (test-case "format-image-gemini rejects non-string base64 (#5466)"
      (check-exn exn:fail:contract? (lambda () (format-image-gemini #f "image/png"))))

    (test-case "format-image-for-provider rejects non-symbol provider (#5466)"
      (check-exn exn:fail:contract?
                 (lambda () (format-image-for-provider "openai" "dGVzdA==" "image/png"))))

    (test-case "format-image-for-provider rejects non-string base64 (#5466)"
      (check-exn exn:fail:contract?
                 (lambda () (format-image-for-provider 'openai '(bad) "image/png"))))

    ;; ============================================================
    ;; Edge cases
    ;; ============================================================

    (test-case "empty base64 string is accepted"
      (define result (format-image-openai "" "image/png"))
      (check-true (hash? result)))

    (test-case "various media types are accepted"
      (for ([mt (in-list '("image/png" "image/jpeg" "image/gif" "image/webp"))])
        (define result (format-image-for-provider 'openai "dGVzdA==" mt))
        (check-true (hash? result) (format "~a should produce hash" mt))))))

(module+ main
  (run-tests format-contract-tests))
