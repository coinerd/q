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

(require racket/contract
         racket/string
         racket/generator
         racket/port
         json
         net/url
         net/http-client
         "model.rkt"
         "provider.rkt"
         "stream.rkt"
         "http-helpers.rkt"
         (only-in "openai-compatible.rkt" openai-build-request-body openai-parse-response))

(provide make-azure-openai-provider
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
  (define uri (string->url url-str))
  (define host (url-host uri))
  (define url-port (url-port uri))
  (define ssl? (equal? (url-scheme uri) "https"))
  (define path-str
    (string-append "/" (string-join (map (lambda (p) (path/param-path p)) (url-path uri)) "/")))
  (define headers (list (format "api-key: ~a" api-key) "Content-Type: application/json"))
  (define body-bytes (jsexpr->bytes body))
  (call-with-request-timeout (lambda ()
                               (define-values (status-line response-headers response-port)
                                 (if url-port
                                     (http-sendrecv host
                                                    path-str
                                                    #:port url-port
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
                               (define response-body (read-response-body/timeout response-port))
                               (check-azure-status! status-line response-body)
                               (bytes->jsexpr response-body))))

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
    (error 'azure-openai
           "HTTP ~a: ~a"
           status-code
           (substring body-str 0 (min (string-length body-str) 200)))))

;; ============================================================
;; Provider constructor
;; ============================================================

(define (make-azure-openai-provider config)
  (define api-key (hash-ref config 'api-key ""))
  (define model-name (hash-ref config 'model "gpt-4"))
  (define base-url (hash-ref config 'base-url ""))
  (define api-version (hash-ref config 'api-version "2024-02-15-preview"))
  (when (string=? api-key "")
    (error 'azure-openai "api-key is required in config"))

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
  (dynamic-wind
   (lambda () (void))
   (lambda ()
     (call-with-request-timeout
      #:cleanup (lambda ()
                  (define rp (unbox response-port-box))
                  (when rp
                    (with-handlers ([exn:fail? void])
                      (close-input-port rp))))
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
        (in-generator
         (let loop ()
           (define line (read-line/timeout response-port))
           (cond
             [(eq? line #f) (void)] ; read timeout
             [(eof-object? line) (void)]
             [(string-prefix? line "data: ")
              (define data (string-trim (substring line 6)))
              (cond
                ;; W2.3: proper done chunk
                [(string=? data "[DONE]") (yield (make-stream-chunk #f #f #f #t))]
                [else
                 (define js
                   (with-handlers ([exn:fail? (lambda (e) #f)])
                     (string->jsexpr data)))
                 (when js
                   (define choices (hash-ref js 'choices '()))
                   (when (pair? choices)
                     (define first-choice (car choices))
                     (define delta (hash-ref first-choice 'delta (hasheq)))
                     (define text (hash-ref delta 'content ""))
                     ;; W2.3: extract finish-reason and usage from chunks
                     (define finish-reason
                       (let ([fr (hash-ref first-choice 'finish_reason #f)])
                         (and (string? fr) (string->symbol (string-replace fr "_" "-")))))
                     (define usage
                       (let ([u (hash-ref js 'usage #f)])
                         (and u
                              (hasheq 'prompt-tokens
                                      (hash-ref u 'prompt_tokens 0)
                                      'completion-tokens
                                      (hash-ref u 'completion_tokens 0)
                                      'total-tokens
                                      (hash-ref u 'total_tokens 0)))))
                     (yield (make-stream-chunk text #f usage #f #:finish-reason finish-reason))))
                 (loop)])]
             [else (loop)]))))))
   (lambda ()
     ;; W2.4: always close response port on exit
     (define rp (unbox response-port-box))
     (when rp
       (with-handlers ([exn:fail? void])
         (close-input-port rp))))))
