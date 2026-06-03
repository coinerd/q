#lang racket/base

;; llm/openai-compatible.rkt — OpenAI-compatible provider adapter
;;
;; Translates normalized model-request structs into OpenAI chat
;; completion API format, and parses responses back into model-response.
;; Supports both non-streaming and streaming modes.
;;
;; HTTP calls use net/http-client from Racket stdlib.
;; SSE parsing delegates to llm/stream.rkt.

(define-logger q-openai)
(require racket/contract
         "timing.rkt"
         (only-in "model-defaults.rkt" OPENAI-DEFAULT-MODEL)
         racket/match
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
         "provider-errors.rkt")

;; Provider constructor
(provide (contract-out [make-openai-compatible-provider (-> (or/c hash? openai-config?) provider?)]
                       [openai-build-request-body (->* (model-request?) (#:stream? boolean?) hash?)]
                       [openai-parse-response (-> (or/c hash? #f) model-response?)]))

;; ============================================================
;; OpenAI Config struct (T2-4)
;; ============================================================

(struct openai-config
        (api-key ; string — API key
         base-url ; string — API base URL
         model ; string — default model name
         max-tokens ; (or/c exact-positive-integer? #f) — max tokens
         temperature) ; (or/c (between/c 0 2) #f) — temperature
  #:transparent)

(define (hash->openai-config h)
  (openai-config (hash-ref h 'api-key "")
                 (hash-ref h 'base-url "https://api.openai.com/v1")
                 (hash-ref h 'model OPENAI-DEFAULT-MODEL)
                 (hash-ref h 'max-tokens #f)
                 (hash-ref h 'temperature #f)))

(provide openai-config
         openai-config?
         openai-config-api-key
         openai-config-base-url
         openai-config-model
         openai-config-max-tokens
         openai-config-temperature
         (contract-out [hash->openai-config (-> hash? openai-config?)]))

;; ============================================================
;; Request body construction
;; ============================================================

(define (openai-build-request-body req #:stream? [stream? #f])
  (define settings (model-request-settings req))
  (define base
    (hasheq 'model
            (hash-ref settings 'model OPENAI-DEFAULT-MODEL)
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
  ;; v0.83.10: Request usage in streaming chunks so cost tracker updates
  (define with-stream-opts
    (if stream?
        (hash-set with-tools 'stream_options (hasheq 'include_usage #t))
        with-tools))
  with-stream-opts)

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
        (translate-stop-reason #f (hash-ref choice 'finish_reason "stop"))
        'stop))

  ;; Build content list from response
  (define content
    (match message
      [#f '()]
      [_
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
                     (define tc-hash
                       (hasheq 'type
                               "tool-call"
                               'id
                               (hash-ref tc 'id)
                               'name
                               (hash-ref fn 'name)
                               'arguments
                               args))
                     ;; Shadow: validate round-trip through tool-call-intent
                     (define tci (hash->tool-call-intent tc-hash))
                     (define round-trip (tool-call-intent->hash tci))
                     (unless (equal? (hash-ref tc-hash 'name) (hash-ref round-trip 'name))
                       (log-q-openai-warning "tool-call-intent shadow mismatch in openai: ~a vs ~a"
                                             (hash-ref tc-hash 'name)
                                             (hash-ref round-trip 'name)))
                     tc-hash)
                   '()))]))

  (make-model-response content usage model-name finish-reason))

;; ============================================================
;; HTTP request execution (non-streaming)
;; ============================================================

(define (do-http-request base-url api-key path body)
  (define url-str (string-append (string-trim base-url "/") path))
  (define headers (list (format "Authorization: Bearer ~a" api-key) "Content-Type: application/json"))
  ;; v0.14.2 Wave 3: per-model timeout via effective-request-timeout-for
  (define model-name (and (hash? body) (hash-ref body 'model #f)))
  (define timeout-secs
    (if model-name
        (effective-request-timeout-for model-name)
        (current-http-request-timeout)))
  (make-provider-http-request url-str
                              headers
                              (jsexpr->bytes body)
                              #:timeout timeout-secs
                              #:status-checker
                              (lambda (sl rb) (check-provider-status! "OpenAI" sl rb))))

;; ============================================================
;; Provider constructor
;; ============================================================

(define (make-openai-compatible-provider config)
  (define cfg
    (if (openai-config? config)
        config
        (hash->openai-config config)))
  (validate-api-key! "OpenAI" "OPENAI_API_KEY" (hasheq 'api-key (openai-config-api-key cfg)))
  (define base-url (openai-config-base-url cfg))
  (define api-key (openai-config-api-key cfg))
  (define default-model (openai-config-model cfg))
  (define default-max-tokens (openai-config-max-tokens cfg))

  (define (ensure-model-settings req)
    ;; Merge default-model and default-max-tokens into request settings if not set
    (define settings (model-request-settings req))
    (define with-model
      (if (hash-has-key? settings 'model)
          settings
          (hash-set settings 'model default-model)))
    (define with-max-tokens
      (if (and default-max-tokens (not (hash-has-key? with-model 'max-tokens)))
          (hash-set with-model 'max-tokens default-max-tokens)
          with-model))
    (make-model-request (model-request-messages req) (model-request-tools req) with-max-tokens))

  (define (send req)
    (define req-with-model (ensure-model-settings req))
    (define body (openai-build-request-body req-with-model))
    (define raw (do-http-request base-url api-key "/chat/completions" body))
    (openai-parse-response raw))

  ;; W-06: Extracted stream request — returns (values response-port stream-timeout cleanup-thunk)
  (define (openai-stream-request req)
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
    (define stream-model-name (and (hash? body) (hash-ref body 'model #f)))
    (define stream-timeout
      (if stream-model-name
          (effective-request-timeout-for stream-model-name)
          (current-http-request-timeout)))
    (define result-vec
      (with-handlers ([exn:fail?
                       (lambda (e)
                         (if (provider-error? e)
                             (raise e)
                             (raise (provider-error
                                     (format "Network error contacting ~a: ~a" host (exn-message e))
                                     (current-continuation-marks)
                                     (hash)
                                     #f
                                     'network))))])
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
                                   #:timeout stream-timeout)))
    (define status-line (vector-ref result-vec 0))
    (define response-headers (vector-ref result-vec 1))
    (define response-port (vector-ref result-vec 2))
    ;; Check HTTP status
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
      (define err-body (read-response-body/timeout response-port))
      (close-input-port response-port)
      (check-provider-status! "OpenAI" status-line err-body))
    (values response-port stream-timeout (lambda () (close-input-port response-port))))

  (define (stream req)
    (define _stream-t0 (current-inexact-milliseconds))
    (define-values (response-port stream-timeout cleanup-thunk)
      (with-handlers ([exn:fail? (lambda (e)
                                   (if (provider-error? e)
                                       (raise e)
                                       (raise (provider-error (format "Stream setup error: ~a"
                                                                      (exn-message e))
                                                              (current-continuation-marks)
                                                              (hash)
                                                              'network
                                                              #f))))])
        (openai-stream-request req)))
    ;; Response port and timeout from openai-stream-request (W-06)
    ;; Status OK — return an incremental generator that reads SSE lines
    ;; from the response port, yielding stream-chunk values as they arrive.
    ;; v0.15.1 Wave 2: Increased stream timeout scaling from /4 to /2.
    ;; Previous formula (max 120 timeout/4) gave 225s for glm-5.1 (request=900s)
    ;; which caused premature SSE timeouts during slow generation.
    ;; New formula gives 450s — enough for models that stall mid-generation.
    ;; v0.45.12 L1: Derive max-total-timeout from model's effective request timeout.
    ;; Use 2x the request timeout as the wall-clock cap (covers slow models).
    ;; For GLM-5.1 (900s): max-total = 1800s (30 min).
    ;; For fast models (120s): max-total = 600s (10 min floor).
    (define max-total-timeout (max 600 (* 2 stream-timeout)))
    (define gen
      (read-sse-chunks response-port
                       #:initial-timeout stream-timeout
                       #:stream-timeout (max 180 (quotient stream-timeout 2))
                       #:max-total-timeout max-total-timeout))
    ;; Simple wrapper: yield chunks until done, then close port.
    ;; No dynamic-wind — it fires before/after on every yield which
    ;; causes the port to be closed between yields.
    (log-stream-setup-timing "openai" _stream-t0)
    (generator ()
               (let loop ()
                 (define chunk (gen))
                 (match chunk
                   [#f
                    ;; Stream done — close port and yield final #f
                    (close-input-port response-port)
                    (yield #f)]
                   [_
                    (yield chunk)
                    (loop)]))))

  (make-provider (lambda () "openai-compatible")
                 (lambda () (hasheq 'streaming #t 'token-counting #f))
                 send
                 stream))
