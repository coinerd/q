#lang racket

;; test-openai-compatible.rkt — Tests for OpenAI-compatible provider adapter
;;
;; BUG-34: Verify that error messages are formatted readably (not #hasheq)

(require rackunit
         rackunit/text-ui
         net/url
         json
         "../llm/provider.rkt"
         "../llm/openai-compatible.rkt"
         "../llm/stream.rkt")

;; ============================================================
;; Tests for error message formatting (BUG-34)
;; ============================================================

(define-test-suite
 error-formatting-tests
 (test-case "check-http-status! extracts error.message from JSON response"
   (define error-json
     (jsexpr->bytes
      (hasheq 'error
              (hasheq 'code
                      1214
                      'message
                      "The messages parameter is illegal. Please check the documentation."))))
   (define exn
     (with-handlers ([exn:fail? identity])
       (check-http-status! #"HTTP/1.1 400 Bad Request" error-json)))
   (check-pred exn? exn)
   (define msg (exn-message exn))
   ;; Should contain status code
   (check-true (string-contains? msg "400") "Message should contain status code")
   ;; Should NOT contain #hasheq (the raw Racket representation)
   (check-false (string-contains? msg "#hasheq") "Message should NOT contain #hasheq")
   ;; Should contain the actual error message text
   (check-true (string-contains? msg "The messages parameter is illegal")
               "Message should contain the error text"))
 (test-case "check-http-status! extracts top-level message field"
   (define error-json (jsexpr->bytes (hasheq 'message "Invalid API key provided")))
   (define exn
     (with-handlers ([exn:fail? identity])
       (check-http-status! #"HTTP/1.1 401 Unauthorized" error-json)))
   (check-pred exn? exn)
   (define msg (exn-message exn))
   (check-true (string-contains? msg "401") "Message should contain status code")
   (check-false (string-contains? msg "#hasheq") "Message should NOT contain #hasheq")
   (check-true (string-contains? msg "Invalid API key provided")
               "Message should contain the error text"))
 (test-case "check-http-status! falls back to raw body when error.message missing"
   (define error-json (jsexpr->bytes (hasheq 'unknown_field "some value")))
   (define exn
     (with-handlers ([exn:fail? identity])
       (check-http-status! #"HTTP/1.1 500 Internal Server Error" error-json)))
   (check-pred exn? exn)
   (define msg (exn-message exn))
   (check-true (string-contains? msg "500") "Message should contain status code")
   ;; Fallback to raw jsexpr representation (but not #hasheq since it's a hash without 'error or 'message)
   (check-true (string-contains? msg "unknown_field") "Message should contain raw JSON content"))
 (test-case "check-http-status! handles binary/non-JSON response"
   (define binary-data #"<html>Error page</html>")
   (define exn
     (with-handlers ([exn:fail? identity])
       (check-http-status! #"HTTP/1.1 502 Bad Gateway" binary-data)))
   (check-pred exn? exn)
   (define msg (exn-message exn))
   (check-true (string-contains? msg "502") "Message should contain status code")
   (check-true (string-contains? msg "binary body") "Message should indicate binary body"))
 (test-case "check-http-status! extracts error.code when message is missing"
   (define error-json (jsexpr->bytes (hasheq 'error (hasheq 'code 1234))))
   (define exn
     (with-handlers ([exn:fail? identity])
       (check-http-status! #"HTTP/1.1 422 Unprocessable Entity" error-json)))
   (check-pred exn? exn)
   (define msg (exn-message exn))
   (check-true (string-contains? msg "422") "Message should contain status code")
   (check-true (string-contains? msg "1234") "Message should contain error code")
   (check-false (string-contains? msg "#hasheq") "Message should NOT contain #hasheq"))
 (test-case "check-http-status! handles error as string (not hash)"
   (define error-json (jsexpr->bytes (hasheq 'error "Something went wrong")))
   (define exn
     (with-handlers ([exn:fail? identity])
       (check-http-status! #"HTTP/1.1 400 Bad Request" error-json)))
   (check-pred exn? exn)
   (define msg (exn-message exn))
   (check-true (string-contains? msg "400") "Message should contain status code")
   (check-true (string-contains? msg "Something went wrong") "Message should contain error string")
   (check-false (string-contains? msg "#hasheq") "Message should NOT contain #hasheq")))

;; ============================================================
;; Run all tests
;; ============================================================

;; ============================================================
;; SEC-10: read-response-body size limit tests
;; ============================================================

(define-test-suite
 api-key-validation-tests
 (test-case "empty API key raises clear error"
   (check-exn exn:fail?
              (lambda ()
                (make-openai-compatible-provider (hash 'api-key "" 'base-url "http://localhost")))
              "empty-api-key"))
 (test-case "missing API key raises clear error"
   (check-exn exn:fail?
              (lambda () (make-openai-compatible-provider (hash 'base-url "http://localhost")))
              "missing-api-key"))
 (test-case "whitespace-only API key raises clear error"
   (check-exn exn:fail?
              (lambda ()
                (make-openai-compatible-provider (hash 'api-key "   " 'base-url "http://localhost")))
              "whitespace-api-key"))
 (test-case "error message mentions OpenAI and OPENAI_API_KEY"
   (define exn
     (with-handlers ([exn:fail? identity])
       (make-openai-compatible-provider (hash 'api-key "" 'base-url "http://localhost"))))
   (check-pred exn? exn)
   (define msg (exn-message exn))
   (check-true (string-contains? msg "OpenAI") "error message mentions OpenAI")
   (check-true (string-contains? msg "OPENAI_API_KEY") "error message mentions OPENAI_API_KEY")
   (check-true (string-contains? msg "API key not set") "error message contains 'API key not set'"))
 (test-case "valid API key does not raise"
   (check-not-exn (lambda ()
                    (make-openai-compatible-provider
                     (hash 'api-key "sk-valid-key-123" 'base-url "http://localhost"))))
   (define prov (make-openai-compatible-provider
                 (hash 'api-key "sk-valid-key-123" 'base-url "http://localhost")))
   (check-equal? (provider-name prov) "openai-compatible")))

;; ============================================================
;; SEC-10: read-response-body size limit tests
;; ============================================================

(define-test-suite response-size-limit-tests
                   (test-case "read-response-body reads normal-sized responses"
                     (define port (open-input-string "Hello World"))
                     (define result (read-response-body port))
                     (check-equal? result (string->bytes/utf-8 "Hello World")))
                   (test-case "read-response-body rejects oversized responses"
                     (define overflow-size (+ max-response-size 1))
                     (define buf (make-bytes 8192 65))
                     (define total-read 0)
                     (define port
                       (make-input-port 'overflow
                                        (lambda (b)
                                          (cond
                                            [(>= total-read overflow-size) eof]
                                            [else
                                             (define n (min 8192 (- overflow-size total-read)))
                                             (bytes-copy! b 0 buf 0 n)
                                             (set! total-read (+ total-read n))
                                             n]))
                                        #f
                                        void))
                     (check-exn #rx"exceeds maximum size limit"
                                (lambda () (read-response-body port))))
                   (test-case "read-response-body handles empty port"
                     (define port (open-input-string ""))
                     (define result (read-response-body port))
                     (check-equal? result (bytes))))

;; ============================================================
;; Issue #137 — OpenAI-compatible 429 rate-limit error includes retry guidance
;; ============================================================

(define-test-suite rate-limit-guidance-tests
                   (test-case "OpenAI HTTP 429 includes wait/retry guidance"
                     (define error-json
                       (jsexpr->bytes (hasheq 'error (hasheq 'message "Rate limit exceeded"))))
                     (define exn
                       (with-handlers ([exn:fail? identity])
                         (check-http-status! #"HTTP/1.1 429 Too Many Requests" error-json)))
                     (check-pred exn? exn)
                     (define msg (exn-message exn))
                     (check-true (or (string-contains? msg "wait")
                                     (string-contains? msg "retry")
                                     (string-contains? msg "Wait")
                                     (string-contains? msg "Retry"))
                                 "429 error includes wait/retry guidance"))
                   (test-case "OpenAI HTTP 429 with rate-limited message"
                     (define exn
                       (with-handlers ([exn:fail? identity])
                         (check-http-status! #"HTTP/1.1 429 Too Many Requests" #"Rate limited")))
                     (check-pred exn? exn)
                     (define msg (exn-message exn))
                     (check-true (string-contains? msg "429") "Message contains status code")
                     (check-true (or (string-contains? msg "wait")
                                     (string-contains? msg "Wait")
                                     (string-contains? msg "retry")
                                     (string-contains? msg "Retry"))
                                 "429 error includes wait/retry guidance")))

;; ============================================================
;; Run all tests (updated to include new suites)
;; ============================================================

(module+ main
  (run-tests error-formatting-tests)
  (run-tests api-key-validation-tests)
  (run-tests response-size-limit-tests)
  (run-tests rate-limit-guidance-tests))
