#lang racket/base

;; llm/azure-openai.rkt — Azure OpenAI provider adapter
;;
;; Azure OpenAI uses the same protocol as OpenAI but with:
;;   - Different base URL format: {resource}.openai.azure.com/openai/deployments/{deployment}
;;   - api-key header instead of Bearer token
;;   - api-version query parameter
;;
;; v0.16.0 Wave 2: Hardened streaming (timeout, dynamic-wind port cleanup,
;;   shared parser, proper done chunk with usage data).
;;
;; #1195: Additional LLM Provider Adapters

(require "../util/error-helpers.rkt")
(require "../util/errors.rkt")
(require "provider-errors.rkt")
(require racket/contract
         "timing.rkt"
         (only-in "model-defaults.rkt" OPENAI-DEFAULT-MODEL)
         racket/string
         racket/port
         json
         net/url
         net/http-client
         "model.rkt"
         "provider.rkt"
         "stream.rkt"
         "http-helpers.rkt"
         (only-in "openai-compatible.rkt" openai-build-request-body openai-parse-response))

(provide (contract-out [make-azure-openai-provider (-> hash? provider?)])
         openai-parse-response-from-jsexpr
         check-azure-status!)

;; ============================================================
;; Azure-specific HTTP request
;; ============================================================

(define (azure-do-http-request base-url api-key api-version path body)
  (define url-str
    (string-append (string-trim base-url "/")
                   path
                   (if (string-contains? path "?") "&" "?")
                   "api-version="
                   api-version))
  (define headers (list (format "api-key: ~a" api-key) "Content-Type: application/json"))
  (make-provider-http-request url-str
                              headers
                              (jsexpr->bytes body)
                              #:status-checker check-azure-status!))

(define (check-azure-status! status-line body-bytes)
  (define status-code
    (let ([m (regexp-match #rx#"HTTP/[^ ]+ ([0-9]+)" status-line)])
      (if m
          (string->number (bytes->string/utf-8 (cadr m)))
          0)))
  (unless (= status-code 200)
    (define body-str
      (if (bytes? body-bytes)
          (bytes->string/utf-8 body-bytes #:error-replacement "?")
          ""))
    (raise-provider-error
     (format "HTTP ~a: ~a" status-code (substring body-str 0 (min (string-length body-str) 200)))
     (classify-http-status status-code)
     status-code)))

;; ============================================================
;; Provider constructor
;; ============================================================

(define (make-azure-openai-provider config)
  (define api-key (hash-ref config 'api-key ""))
  (define model-name (hash-ref config 'model OPENAI-DEFAULT-MODEL))
  (define base-url (hash-ref config 'base-url ""))
  (define api-version (hash-ref config 'api-version "2024-02-15-preview"))
  (when (string=? api-key "")
    (raise-credential-error "api-key is required in config" "azure-openai"))

  (make-provider (lambda () "Azure OpenAI")
                 (lambda () (hasheq 'streaming #t))
                 (lambda (req)
                   (define body (openai-build-request-body req))
                   (define js
                     (azure-do-http-request base-url api-key api-version "/chat/completions" body))
                   (openai-parse-response-from-jsexpr js model-name))
                 (lambda (req) (azure-stream base-url api-key api-version req model-name))))

;; W2.2: Replaced inline parser with shared openai-parse-response.
;; Azure responses may lack a 'model' key, so we inject it.
(define (openai-parse-response-from-jsexpr js model-name)
  (define js-with-model
    (if (hash-has-key? js 'model)
        js
        (hash-set js 'model model-name)))
  (openai-parse-response js-with-model))

;; ============================================================
;; Streaming support (v0.16.0 Wave 2: hardened)
;; W2.1: call-with-request-timeout wraps http-sendrecv
;; W2.3: proper done chunk with usage + finish-reason
;; W2.4: dynamic-wind ensures port cleanup on timeout/exception
;; ============================================================

(define (azure-stream base-url api-key api-version req model-name)
  (define _stream-t0 (current-inexact-milliseconds))
  (define body (openai-build-request-body req #:stream? #t))
  (define url-str
    (string-append (string-trim base-url "/") "/chat/completions?api-version=" api-version))
  (define uri (string->url url-str))
  (define host (url-host uri))
  (define url-port-val (url-port uri))
  (define ssl? (equal? (url-scheme uri) "https"))
  (define path-str
    (string-append "/" (string-join (map (lambda (p) (path/param-path p)) (url-path uri)) "/")))
  (define headers (list (format "api-key: ~a" api-key) "Content-Type: application/json"))
  (define body-bytes (jsexpr->bytes body))
  (define response-port-box (box #f))
  (log-stream-setup-timing "azure" _stream-t0)
  (dynamic-wind (lambda () (void))
                (lambda ()
                  (call-with-request-timeout
                   #:cleanup (lambda ()
                               (define rp (unbox response-port-box))
                               (when rp
                                 (with-logged-error "port cleanup" (close-input-port rp))))
                   (lambda ()
                     (define-values (status-line response-headers response-port)
                       (if url-port-val
                           (http-sendrecv host
                                          path-str
                                          #:port url-port-val
                                          #:ssl? ssl?
                                          #:method "POST"
                                          #:headers headers
                                          #:data body-bytes)
                           (http-sendrecv host
                                          path-str
                                          #:ssl? ssl?
                                          #:method "POST"
                                          #:headers headers
                                          #:data body-bytes)))
                     (set-box! response-port-box response-port)
                     (check-azure-status! status-line #"")
                     ;; v0.81.0 W1: Replaced inline SSE loop with shared stream-sse-events.
                     ;; Bonus: now handles tool_calls deltas (was missing in inline version).
                     (stream-sse-events response-port
                                        (lambda (parsed) (list (normalize-openai-chunk parsed)))))))
                (lambda ()
                  ;; W2.4: always close response port on exit
                  (define rp (unbox response-port-box))
                  (when rp
                    (with-logged-error "port cleanup" (close-input-port rp))))))
