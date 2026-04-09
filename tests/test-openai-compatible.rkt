#lang racket

;; test-openai-compatible.rkt — Tests for OpenAI-compatible provider adapter
;;
;; BUG-34: Verify that error messages are formatted readably (not #hasheq)

(require rackunit
         rackunit/text-ui
         net/url
         json
         "../llm/openai-compatible.rkt")

;; ============================================================
;; Tests for error message formatting (BUG-34)
;; ============================================================

(define-test-suite error-formatting-tests
  (test-case "check-http-status! extracts error.message from JSON response"
    (define error-json
      (jsexpr->bytes
       (hasheq 'error
               (hasheq 'code 1214
                       'message "The messages parameter is illegal. Please check the documentation."))))
    (define exn
      (with-handlers ([exn:fail? identity])
        (check-http-status! #"HTTP/1.1 400 Bad Request" error-json)))
    (check-true (exn? exn))
    (define msg (exn-message exn))
    ;; Should contain status code
    (check-true (string-contains? msg "400") "Message should contain status code")
    ;; Should NOT contain #hasheq (the raw Racket representation)
    (check-false (string-contains? msg "#hasheq") "Message should NOT contain #hasheq")
    ;; Should contain the actual error message text
    (check-true (string-contains? msg "The messages parameter is illegal") "Message should contain the error text"))

  (test-case "check-http-status! extracts top-level message field"
    (define error-json
      (jsexpr->bytes
       (hasheq 'message "Invalid API key provided")))
    (define exn
      (with-handlers ([exn:fail? identity])
        (check-http-status! #"HTTP/1.1 401 Unauthorized" error-json)))
    (check-true (exn? exn))
    (define msg (exn-message exn))
    (check-true (string-contains? msg "401") "Message should contain status code")
    (check-false (string-contains? msg "#hasheq") "Message should NOT contain #hasheq")
    (check-true (string-contains? msg "Invalid API key provided") "Message should contain the error text"))

  (test-case "check-http-status! falls back to raw body when error.message missing"
    (define error-json
      (jsexpr->bytes
       (hasheq 'unknown_field "some value")))
    (define exn
      (with-handlers ([exn:fail? identity])
        (check-http-status! #"HTTP/1.1 500 Internal Server Error" error-json)))
    (check-true (exn? exn))
    (define msg (exn-message exn))
    (check-true (string-contains? msg "500") "Message should contain status code")
    ;; Fallback to raw jsexpr representation (but not #hasheq since it's a hash without 'error or 'message)
    (check-true (string-contains? msg "unknown_field") "Message should contain raw JSON content"))

  (test-case "check-http-status! handles binary/non-JSON response"
    (define binary-data #"<html>Error page</html>")
    (define exn
      (with-handlers ([exn:fail? identity])
        (check-http-status! #"HTTP/1.1 502 Bad Gateway" binary-data)))
    (check-true (exn? exn))
    (define msg (exn-message exn))
    (check-true (string-contains? msg "502") "Message should contain status code")
    (check-true (string-contains? msg "binary body") "Message should indicate binary body"))

  (test-case "check-http-status! extracts error.code when message is missing"
    (define error-json
      (jsexpr->bytes
       (hasheq 'error
               (hasheq 'code 1234))))
    (define exn
      (with-handlers ([exn:fail? identity])
        (check-http-status! #"HTTP/1.1 422 Unprocessable Entity" error-json)))
    (check-true (exn? exn))
    (define msg (exn-message exn))
    (check-true (string-contains? msg "422") "Message should contain status code")
    (check-true (string-contains? msg "1234") "Message should contain error code")
    (check-false (string-contains? msg "#hasheq") "Message should NOT contain #hasheq"))

  (test-case "check-http-status! handles error as string (not hash)"
    (define error-json
      (jsexpr->bytes
       (hasheq 'error "Something went wrong")))
    (define exn
      (with-handlers ([exn:fail? identity])
        (check-http-status! #"HTTP/1.1 400 Bad Request" error-json)))
    (check-true (exn? exn))
    (define msg (exn-message exn))
    (check-true (string-contains? msg "400") "Message should contain status code")
    (check-true (string-contains? msg "Something went wrong") "Message should contain error string")
    (check-false (string-contains? msg "#hasheq") "Message should NOT contain #hasheq")))

;; ============================================================
;; Run all tests
;; ============================================================

(module+ main
  (run-tests error-formatting-tests))
