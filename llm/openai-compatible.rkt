#lang racket/base

;; llm/openai-compatible.rkt — OpenAI-compatible provider adapter
;;
;; Translates normalized model-request structs into OpenAI chat
;; completion API format, and parses responses back into model-response.
;; Supports both non-streaming and streaming modes.
;;
;; HTTP calls use net/http-client from Racket stdlib.
;; SSE parsing delegates to llm/stream.rkt.

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
         "http-helpers.rkt")

;; Provider constructor
(provide make-openai-compatible-provider
         ;; Request/response helpers (exported for testing)
         openai-build-request-body
         openai-parse-response
         check-http-status!)

;; ============================================================
;; Request body construction
;; ============================================================

(define (openai-build-request-body req #:stream? [stream? #f])
  (define settings (model-request-settings req))
  (define base
    (hasheq 'model
            (hash-ref settings 'model "gpt-4")
            'messages
            (model-request-messages req)
            'stream
            stream?))
  ;; Add optional fields
  (define with-temp
    (if (hash-has-key? settings 'temperature)
        (hash-set base 'temperature (hash-ref settings 'temperature))
        base))
  (define with-max-tokens
    (if (hash-has-key? settings 'max-tokens)
        (hash-set with-temp 'max_tokens (hash-ref settings 'max-tokens))
        with-temp))
  ;; Add tools if present
  (define with-tools
    (if (model-request-tools req)
        (hash-set with-max-tokens 'tools (model-request-tools req))
        with-max-tokens))
  with-tools)

;; ============================================================
;; Response parsing
;; ============================================================

(define (openai-parse-response raw)
  (define model-name (hash-ref raw 'model "unknown"))
  (define usage (hash-ref raw 'usage (hash)))
  (define choices (hash-ref raw 'choices '()))
  (define choice
    (if (null? choices)
        #f
        (car choices)))
  (define message
    (if choice
        (hash-ref choice 'message #f)
        #f))
  (define finish-reason
    (if choice
        (let ([fr (hash-ref choice 'finish_reason "stop")])
          (cond
            [(string? fr) (string->symbol (string-replace fr "_" "-"))]
            [else 'stop]))
        'stop))

  ;; Build content list from response
  (define content
    (cond
      [(not message) '()]
      [else
       (define text-content (hash-ref message 'content #f))
       (define tool-calls (hash-ref message 'tool_calls #f))
       ;; Text content
       (append (if (and text-content (string? text-content))
                   (list (hasheq 'type "text" 'text text-content))
                   '())
               ;; Tool calls
               (if tool-calls
                   (for/list ([tc (in-list tool-calls)])
                     (define fn (hash-ref tc 'function (hasheq)))
                     (define args-str (hash-ref fn 'arguments "{}"))
                     (define args
                       (with-handlers ([exn:fail? (lambda (e) args-str)])
                         (string->jsexpr args-str)))
                     (hasheq 'type
                             "tool-call"
                             'id
                             (hash-ref tc 'id)
                             'name
                             (hash-ref fn 'name)
                             'arguments
                             args))
                   '()))]))

  (make-model-response content usage model-name finish-reason))

;; ============================================================
;; HTTP request execution (non-streaming)
;; ============================================================

(define (do-http-request base-url api-key path body)
  (define url-str (string-append (string-trim base-url "/") path))
  (define uri (string->url url-str))
  (define host (url-host uri))
  (define port (url-port uri))
  (define ssl? (equal? (url-scheme uri) "https"))
  (define path-str
    (string-append "/" (string-join (map (lambda (p) (path/param-path p)) (url-path uri)) "/")))
  (define headers (list (format "Authorization: Bearer ~a" api-key) "Content-Type: application/json"))
  (define body-bytes (jsexpr->bytes body))
  ;; v0.14.2 Wave 3: per-model timeout via effective-request-timeout-for
  (define model-name (and (hash? body) (hash-ref body 'model #f)))
  (define timeout-secs
    (if model-name
        (effective-request-timeout-for model-name)
        (current-http-request-timeout)))
  ;; Wrap entire request in overall timeout (SEC-11)
  (call-with-request-timeout (lambda ()
                               (define-values (status-line response-headers response-port)
                                 (if port
                                     (http-sendrecv host
                                                    path-str
                                                    #:port port
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
                               (check-http-status! status-line response-body)
                               (bytes->jsexpr response-body))
                             #:timeout timeout-secs))

;; ============================================================
;; HTTP status check helper
;; ============================================================

(define (extract-error-message jsexpr)
  ;; Extract a readable error message from a JSON error response.
  ;; Tries error.message, then error.code, then message, falls back to #f.
  (cond
    [(not (hash? jsexpr)) #f]
    [(hash-has-key? jsexpr 'error)
     (define err (hash-ref jsexpr 'error))
     (cond
       [(hash? err)
        (cond
          [(hash-has-key? err 'message)
           (define msg (hash-ref err 'message))
           (if (string? msg) msg #f)]
          [(hash-has-key? err 'code) (format "Error code: ~a" (hash-ref err 'code))]
          [else #f])]
       [(string? err) err]
       [else #f])]
    [(hash-has-key? jsexpr 'message)
     (define msg (hash-ref jsexpr 'message))
     (if (string? msg) msg #f)]
    [else #f]))

(define (check-http-status! status-line response-body)
  ;; status-line from http-sendrecv is a byte string — convert first
  (define status-str
    (if (bytes? status-line)
        (bytes->string/utf-8 status-line)
        status-line))
  (define response-bytes
    (if (bytes? response-body)
        response-body
        (string->bytes/utf-8 response-body)))
  ;; Extract numeric status code from "HTTP/1.1 200 OK" or similar
  (define status-code (extract-status-code status-line))
  (cond
    ;; Redirects — http-sendrecv does not follow them automatically
    [(and (>= status-code 300) (< status-code 400))
     (raise-http-error!
      (format
       "API request redirected (~a: ~a). The server returned a redirect — check your base-url in config.json."
       status-code
       status-str))]
    ;; Client/server errors
    [(= status-code 429)
     (define error-text-429
       (with-handlers ([exn:fail? (λ (_)
                                    (format "<binary body ~a bytes>" (bytes-length response-bytes)))])
         (define jsexpr (bytes->jsexpr response-bytes))
         (or (extract-error-message jsexpr) (format "~a" jsexpr))))
     (raise-http-error! (format "API rate limited (429). Please wait and try again.\n~a"
                                error-text-429))]
    [(http-error? status-code)
     (define error-text
       (with-handlers ([exn:fail? (λ (_)
                                    (format "<binary body ~a bytes>" (bytes-length response-bytes)))])
         (define jsexpr (bytes->jsexpr response-bytes))
         (or (extract-error-message jsexpr) (format "~a" jsexpr))))
     (raise-http-error! (format "API request failed (~a): ~a" status-code error-text))]))

;; ============================================================
;; Provider constructor
;; ============================================================

(define (make-openai-compatible-provider config)
  (validate-api-key! "OpenAI" "OPENAI_API_KEY" config)
  (define base-url (hash-ref config 'base-url "https://api.openai.com/v1"))
  (define api-key (hash-ref config 'api-key ""))
  (define default-model (hash-ref config 'model "gpt-4"))

  (define (ensure-model-settings req)
    ;; Merge default-model into request settings if not already set
    (define settings (model-request-settings req))
    (if (hash-has-key? settings 'model)
        req
        (make-model-request (model-request-messages req)
                            (model-request-tools req)
                            (hash-set settings 'model default-model))))

  (define (send req)
    (define req-with-model (ensure-model-settings req))
    (define body (openai-build-request-body req-with-model))
    (define raw (do-http-request base-url api-key "/chat/completions" body))
    (openai-parse-response raw))

  (define (stream req)
    (define req-with-model (ensure-model-settings req))
    (define body (openai-build-request-body req-with-model #:stream? #t))
    (define url-str (string-append (string-trim base-url "/") "/chat/completions"))
    (define uri (string->url url-str))
    (define host (url-host uri))
    (define req-port (url-port uri))
    (define ssl? (equal? (url-scheme uri) "https"))
    (define path-str
      (string-append "/" (string-join (map (lambda (p) (path/param-path p)) (url-path uri)) "/")))
    (define headers
      (list (format "Authorization: Bearer ~a" api-key) "Content-Type: application/json"))
    (define body-bytes (jsexpr->bytes body))
    ;; v0.14.2 Wave 3: per-model timeout for streaming requests
    (define stream-model-name (and (hash? body) (hash-ref body 'model #f)))
    (define stream-timeout
      (if stream-model-name
          (effective-request-timeout-for stream-model-name)
          (current-http-request-timeout)))
    ;; Wrap initial HTTP request in overall timeout (SEC-11)
    ;; call-with-request-timeout returns a single value,
    ;; so we capture the 3 http-sendrecv values in a vector.
    (define result-vec
      (call-with-request-timeout (lambda ()
                                   (define-values (sl rh rp)
                                     (if req-port
                                         (http-sendrecv host
                                                        path-str
                                                        #:port req-port
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
                                   (vector sl rh rp))
                                 #:timeout stream-timeout))
    (define status-line (vector-ref result-vec 0))
    (define response-headers (vector-ref result-vec 1))
    (define response-port (vector-ref result-vec 2))
    ;; Check HTTP status from status-line BEFORE reading body.
    ;; For error responses (4xx/5xx), read the full body to include in error message.
    (define status-str
      (if (bytes? status-line)
          (bytes->string/utf-8 status-line)
          status-line))
    (define status-code
      (let ([m (regexp-match #rx"HTTP/[0-9.]+ ([0-9]+)" status-str)])
        (if m
            (string->number (cadr m))
            0)))
    (when (>= status-code 300)
      ;; Error/redirect — read full body, then raise
      (define err-body (read-response-body/timeout response-port))
      (close-input-port response-port)
      (check-http-status! status-line err-body))
    ;; Status OK — return an incremental generator that reads SSE lines
    ;; from the response port, yielding stream-chunk values as they arrive.
    ;; v0.14.3: Scale SSE stream timeouts with per-model request timeout.
    ;; For slow models (e.g. glm-5.1 with request=900s), the default 60s
    ;; stream-timeout causes premature SSE chunk timeouts during generation.
    (define gen
      (read-sse-chunks response-port
                       #:initial-timeout stream-timeout
                       #:stream-timeout (max 120 (quotient stream-timeout 4))))
    ;; Simple wrapper: yield chunks until done, then close port.
    ;; No dynamic-wind — it fires before/after on every yield which
    ;; causes the port to be closed between yields.
    (generator ()
               (let loop ()
                 (define chunk (gen))
                 (cond
                   [(not chunk)
                    ;; Stream done — close port and yield final #f
                    (close-input-port response-port)
                    (yield #f)]
                   [else
                    (yield chunk)
                    (loop)]))))

  (make-provider (lambda () "openai-compatible")
                 (lambda () (hasheq 'streaming #t 'token-counting #f))
                 send
                 stream))
