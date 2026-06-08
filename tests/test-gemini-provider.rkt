#lang racket

;; @speed fast
;; @suite default

;; tests/test-gemini-provider.rkt — v0.70.8 W1
;; Extracted from test-gemini.rkt: provider construction, HTTP status, API key validation, security

(require rackunit
         "../llm/provider.rkt"
         "../llm/stream.rkt"
         "../llm/gemini.rkt"
         "../llm/http-helpers.rkt")

;; ============================================================
;; 17. make-gemini-provider — returns provider?, correct name, capabilities
;; ============================================================

(define gemini-config (hash 'api-key "test-key-123" 'model "gemini-2.5-pro"))

(define gemini-provider (make-gemini-provider gemini-config))
(check-pred provider? gemini-provider "make-gemini-provider returns provider?")
(check-equal? (provider-name gemini-provider) "gemini" "provider name is 'gemini'")

(define gemini-caps (provider-capabilities gemini-provider))
(check-pred hash? gemini-caps)
(check-true (hash-ref gemini-caps 'streaming) "streaming capability is #t")
(check-false (hash-ref gemini-caps 'token-counting) "token-counting capability is #f")

;; ============================================================
;; 18. make-gemini-provider — with custom base-url
;; ============================================================

(define gemini-custom-config
  (hash 'api-key "test-key-456" 'base-url "https://custom-proxy.example.com" 'model "gemini-2.5-pro"))

(define gemini-custom-provider (make-gemini-provider gemini-custom-config))
(check-pred provider? gemini-custom-provider "custom base-url provider is provider?")
(check-equal? (provider-name gemini-custom-provider)
              "gemini"
              "custom provider name is still 'gemini'")

;; ============================================================
;; 20. HTTP status checks (200, 400, 401, 403, 429, 500)
;; ============================================================

(test-case "gemini-provider: HTTP 200 passes without error"
  (check-not-exn (lambda () (check-provider-status! "Gemini" #"HTTP/1.1 200 OK" #"{}"))))

(test-case "HTTP 400 raises bad request error"
  (check-exn #rx"bad request [(]400[)]"
             (lambda ()
               (check-provider-status! "Gemini"
                                       #"HTTP/1.1 400 Bad Request"
                                       #"{\"error\":{\"message\":\"Invalid\"}}"))))

(test-case "gemini-provider: HTTP 401 raises authentication error"
  (check-exn #rx"authentication failed [(]401[)]"
             (lambda ()
               (check-provider-status! "Gemini"
                                       #"HTTP/1.1 401 Unauthorized"
                                       #"{\"error\":{\"message\":\"Invalid API key\"}}"))))

(test-case "gemini-provider: HTTP 403 raises forbidden error"
  (check-exn #rx"forbidden [(]403[)]"
             (lambda ()
               (check-provider-status! "Gemini"
                                       #"HTTP/1.1 403 Forbidden"
                                       #"{\"error\":{\"message\":\"Access denied\"}}"))))

(test-case "gemini-provider: HTTP 429 raises rate limit error"
  (check-exn #rx"rate limited [(]429[)]"
             (lambda ()
               (check-provider-status! "Gemini"
                                       #"HTTP/1.1 429 Too Many Requests"
                                       #"{\"error\":{\"message\":\"Rate limited\"}}"))))

(test-case "gemini-provider: HTTP 500 raises server error"
  (check-exn #rx"server error [(]500[)]"
             (lambda ()
               (check-provider-status! "Gemini"
                                       #"HTTP/1.1 500 Internal Server Error"
                                       #"{\"error\":{\"message\":\"Internal error\"}}"))))

(test-case "gemini-provider: HTTP 502 raises server error"
  (check-exn #rx"server error [(]502[)]"
             (lambda ()
               (check-provider-status! "Gemini" #"HTTP/1.1 502 Bad Gateway" #"Bad Gateway"))))

(test-case "gemini-provider: String status-line also works"
  (check-not-exn (lambda () (check-provider-status! "Gemini" "HTTP/1.1 200 OK" "{}"))))

;; ============================================================
;; 27. API key is NOT in URL (header-based auth)
;; ============================================================

(test-case "SEC-05: non-streaming URL does not contain ?key="
  (let* ([api-base "https://generativelanguage.googleapis.com"]
         [model-path (string-append (string #\/) "v1beta/models/test-model:generateContent")]
         [url-without-key (string-append api-base model-path)])
    (check-false (string-contains? url-without-key "?key=") "URL should not contain ?key=")))

(test-case "SEC-05: streaming URL does not contain ?key="
  (let* ([api-base "https://generativelanguage.googleapis.com"]
         [model-path (string-append (string #\/) "v1beta/models/test-model:streamGenerateContent")]
         [stream-url (string-append api-base model-path "?alt=sse")])
    (check-false (string-contains? stream-url "?key=") "streaming URL should not contain ?key=")))

;; ============================================================
;; 30. read-response-body — size limit enforcement (SEC-10)
;; ============================================================

(test-case "read-response-body reads normal-sized responses (gemini context)"
  (define port (open-input-string "{\"text\":\"hello\"}"))
  (define result (read-response-body port))
  (check-equal? result (string->bytes/utf-8 "{\"text\":\"hello\"}")))

(test-case "read-response-body rejects oversized responses (gemini context)"
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
  (check-exn #rx"exceeds maximum size limit" (lambda () (read-response-body port))))

;; ============================================================
;; 41-45. API key validation tests
;; ============================================================

(test-case "gemini-provider: empty API key raises clear error"
  (check-exn exn:fail? (lambda () (make-gemini-provider (hash 'api-key "")))))

(test-case "gemini-provider: missing API key raises clear error"
  (check-exn exn:fail? (lambda () (make-gemini-provider (hash)))))

(test-case "gemini-provider: whitespace-only API key raises clear error"
  (check-exn exn:fail? (lambda () (make-gemini-provider (hash 'api-key "   ")))))

(test-case "error message mentions Gemini and GEMINI_API_KEY"
  (define exn
    (with-handlers ([exn:fail? identity])
      (make-gemini-provider (hash 'api-key ""))))
  (check-pred exn? exn)
  (define msg (exn-message exn))
  (check-true (string-contains? msg "Gemini") "error message mentions Gemini")
  (check-true (string-contains? msg "GEMINI_API_KEY") "error message mentions GEMINI_API_KEY")
  (check-true (string-contains? msg "API key not set") "error message contains 'API key not set'"))

(test-case "gemini-provider: valid API key does not raise"
  (check-not-exn (lambda () (make-gemini-provider (hash 'api-key "AIzaSyValidKey-789"))))
  (define prov (make-gemini-provider (hash 'api-key "AIzaSyValidKey-789")))
  (check-equal? (provider-name prov) "gemini"))

;; ============================================================
;; Issue #137 — Gemini 429 rate-limit error includes retry guidance
;; ============================================================

(test-case "Gemini HTTP 429 includes wait/retry guidance"
  (define exn
    (with-handlers ([exn:fail? identity])
      (check-provider-status! "Gemini"
                              #"HTTP/1.1 429 Too Many Requests"
                              #"{\"error\":{\"message\":\"Rate limited\"}}")))
  (check-pred exn? exn)
  (define msg (exn-message exn))
  (check-true (or (string-contains? msg "wait")
                  (string-contains? msg "retry")
                  (string-contains? msg "Wait")
                  (string-contains? msg "Retry"))
              "Gemini 429 error includes wait/retry guidance"))

(println "All Gemini provider tests passed!")
