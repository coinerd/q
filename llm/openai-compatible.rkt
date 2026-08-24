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
         "provider-telemetry.rkt"
         ;; v1.00.13 W2 (#9466): timeout semantics come ONLY from the policy
         ;; module (RL-3); stream.rkt is required for the pure mechanism.
         (only-in "stream.rkt"
                  stream-sse-events
                  close-port-after-stream
                  call-with-request-timeout
                  read-response-body/timeout
                  exn:fail:network:timeout:stream?)
         "request-policy.rkt"
         "http-helpers.rkt"
         "provider-errors.rkt"
         ;; BUG-0019 W2: flag-gated connection pooling (default OFF)
         (only-in "conn-pool.rkt" current-conn-pool pool-acquire! pool-release! pool-send-request!))

;; Provider constructor
(provide (contract-out [make-openai-compatible-provider (-> (or/c hash? openai-config?) provider?)]
                       [openai-build-request-body (->* (model-request?) (#:stream? boolean?) hash?)]
                       [openai-parse-response (-> (or/c hash? #f) model-response?)])
         openai-normalize-message
         openai-normalize-tool
         openai-wrap-stream-error
         ;; v0.99.84: Stream chunk normalization moved from llm/stream.rkt
         ;; Provider-specific OpenAI wire-format parsing belongs in the adapter.
         (contract-out [normalize-openai-chunk (-> hash? (or/c stream-chunk? any/c))]
                       [normalize-openai-chunks
                        (-> (listof hash?) (listof (or/c stream-chunk? any/c)))]))

;; ============================================================
;; Stream chunk normalization (moved from llm/stream.rkt in v0.99.84)
;; ============================================================

;; normalize-openai-chunks : (listof hash?) -> (listof stream-chunk?)
;; Convert a list of OpenAI-format streaming response objects
;; into canonical stream-chunk structs.
(define (normalize-openai-chunks raw-chunks)
  (for/list ([chunk (in-list raw-chunks)])
    (normalize-openai-chunk chunk)))

;; normalize-openai-chunk : hash? -> stream-chunk?
;; Normalize a single OpenAI-format streaming response object into a stream-chunk.
;; Provider-specific: parses choices/delta/content/reasoning_content/tool_calls/usage.
(define (normalize-openai-chunk raw)
  (define choices (hash-ref raw 'choices '()))
  ;; DeepSeek and some other OpenAI-compatible endpoints emit "usage": null on
  ;; intermediate streaming chunks (only the final chunk carries a usage hash).
  ;; q's strict JSON parser maps JSON null to the symbol 'null, which would
  ;; violate the (or/c hash? #f) usage contract on make-stream-chunk. Coerce
  ;; any non-hash usage value (including 'null) to #f.
  (define usage (let ([u (hash-ref raw 'usage #f)]) (if (hash? u) u #f)))
  (define choice
    (if (null? choices)
        #f
        (car choices)))
  (define delta
    (if choice
        (hash-ref choice 'delta #f)
        #f))
  (define finish-reason
    (if choice
        (hash-ref choice 'finish_reason #f)
        #f))
  (define delta-content
    (if delta
        (hash-ref delta 'content #f)
        #f))
  (define delta-text (if (string? delta-content) delta-content #f))
  ;; Extract reasoning_content for thinking models (glm-5.1, DeepSeek-R1)
  ;; DeepSeek emits "reasoning_content": null on chunks where no reasoning delta
  ;; is present (first chunk, after reasoning completes). Coerce non-string
  ;; values (including 'null) to #f for the (or/c string? #f) delta-thinking
  ;; contract.
  (define delta-thinking
    (if delta
        (let ([rt (hash-ref delta 'reasoning_content #f)]) (if (string? rt) rt #f))
        #f))
  (define delta-tool-call
    (if delta
        (let ([tcs (hash-ref delta 'tool_calls #f)])
          (if (and tcs (pair? tcs))
              (car tcs)
              #f))
        #f))
  (make-stream-chunk delta-text
                     delta-tool-call
                     usage
                     (and (string? finish-reason) #t)
                     #:delta-thinking delta-thinking
                     #:finish-reason finish-reason))

;; ============================================================
;; OpenAI Config struct (T2-4)
;; ============================================================

(struct openai-config (api-key base-url model max-tokens temperature)
  #:transparent
  #:property prop:custom-write
  (lambda (cfg out _mode)
    (fprintf out
             "#<openai-config api-key=<REDACTED> base-url=~s model=~s max-tokens=~s temperature=~s>"
             (openai-config-base-url cfg)
             (openai-config-model cfg)
             (openai-config-max-tokens cfg)
             (openai-config-temperature cfg))))

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

(define (openai-normalize-message msg)
  ;; v0.99.58 FIX: Ensure all messages have a content key.
  ;; OpenAI-compatible APIs reject messages without content (400:
  ;; "messages parameter is illegal"). Assistant messages with tool_calls
  ;; but no content are the most common offender.
  (define msg-with-content
    (if (hash-has-key? msg 'content)
        msg
        (hash-set msg 'content (json-null))))
  (define calls (hash-ref msg-with-content 'tool_calls #f))
  (if (not (list? calls))
      msg-with-content
      (hash-set msg-with-content
                'tool_calls
                (for/list ([call (in-list calls)])
                  (define fn (hash-ref call 'function (hasheq)))
                  (define args (hash-ref fn 'arguments "{}"))
                  (hash-set call
                            'function
                            (hash-set fn
                                      'arguments
                                      (if (string? args)
                                          args
                                          (jsexpr->string args))))))))

;; v0.99.58 FIX: Normalize tool schemas for strict OpenAI-compatible APIs.
;; Some providers (ZhipuAI/GLM) reject tool schemas where array properties
;; lack an "items" field. This adds {"items": {"type": "string"}} to any
;; array property missing it.
(define (openai-normalize-tool tool)
  (define fn (hash-ref tool 'function (hasheq)))
  (define params (hash-ref fn 'parameters (hasheq)))
  (define props (hash-ref params 'properties (hasheq)))
  (define fixed-props
    (for/hash ([(k v) (in-hash props)])
      (if (and (hash? v) (equal? (hash-ref v 'type #f) "array") (not (hash-has-key? v 'items)))
          (values k (hash-set v 'items (hasheq 'type "string")))
          (values k v))))
  (define fixed-params (hash-set params 'properties fixed-props))
  (hash-set tool 'function (hash-set fn 'parameters fixed-params)))

(define (openai-build-request-body req #:stream? [stream? #f])
  (define settings (model-request-settings req))
  (define base
    (hasheq 'model
            (hash-ref settings 'model OPENAI-DEFAULT-MODEL)
            'messages
            (map openai-normalize-message (model-request-messages req))
            'stream
            stream?))
  (define with-temp
    (if (hash-has-key? settings 'temperature)
        (hash-set base 'temperature (hash-ref settings 'temperature))
        base))
  (define with-max-tokens
    (if (hash-has-key? settings 'max-tokens)
        (hash-set with-temp 'max_tokens (hash-ref settings 'max-tokens))
        with-temp))
  (define with-tools
    (if (model-request-tools req)
        (hash-set with-max-tokens 'tools (map openai-normalize-tool (model-request-tools req)))
        with-max-tokens))
  (if stream?
      (hash-set with-tools 'stream_options (hasheq 'include_usage #t))
      with-tools))

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
                     (validate-tool-call-intent! tc-hash "openai")
                     tc-hash)
                   '()))]))

  (define native-id (or (hash-ref raw 'id #f) (hash-ref raw 'responseId #f)))
  (make-model-response content
                       usage
                       model-name
                       finish-reason
                       #:provenance (response-native-identity #:adapter "openai-compatible"
                                                              #:native-response-id native-id
                                                              #:native-model model-name)))

;; ============================================================
;; HTTP request execution (non-streaming)
;; ============================================================

(define (do-http-request base-url api-key path body)
  (define url-str (string-append (string-trim base-url "/") path))
  (define headers (list (format "Authorization: Bearer ~a" api-key) "Content-Type: application/json"))
  ;; v1.00.13 W2 (#9466): the resolved request-network policy is the ONLY
  ;; timeout input (RL-3); adapters never interpret raw config.
  (define model-name (and (hash? body) (hash-ref body 'model #f)))
  (define policy (resolve-request-network-policy-for-model model-name))
  (make-provider-http-request url-str
                              headers
                              (jsexpr->bytes body)
                              #:timeout (request-network-policy-request-budget-secs policy)
                              #:connect-timeout (request-network-policy-connect-ttfb-secs policy)
                              #:read-timeout (request-network-policy-body-read-budget-secs policy)
                              #:status-checker
                              (lambda (sl rb) (check-provider-status! "OpenAI" sl rb))))

;; ============================================================
;; Stream-phase error wrapping
;; ============================================================

;; Wrap a stream-phase failure (e.g. an SSL/network read error on the SSE
;; response port) into a structured provider-error with category 'network so the
;; auto-retry layer classifies it as retryable. Existing provider-errors pass
;; through untouched. Mirrors the setup-phase wrapping in openai-stream-request.
;;
;; v0.99.81 W2: exn:fail:network:timeout:stream exceptions are preserved
;; unchanged so the retry-layer circuit breaker can inspect the W1 liveness
;; metadata (received-any-data?, phase). Wrapping them into a plain
;; provider-error would destroy the classification signal.
(define (openai-wrap-stream-error e)
  (cond
    [(provider-error? e) (raise e)]
    [(exn:fail:network:timeout:stream? e) (raise e)]
    [else
     (raise (provider-error (format "Stream read error: ~a" (exn-message e))
                            (current-continuation-marks)
                            (hash)
                            'network
                            #f))]))

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

  ;; W-06: Extracted stream request — returns
  ;;   (values response-port request-network-policy cleanup-thunk)
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

    ;; v1.00.13 W2 (#9466): ONE resolved policy per request (RL-3). The
    ;; adapter never computes timeout values from raw config; it consumes
    ;; the resolved fields below.
    (define policy (resolve-request-network-policy-for-model stream-model-name))
    ;; Own every resource created by http-sendrecv (including resources opened
    ;; before a response port is returned) under a request-scoped custodian.
    ;; Timeout cleanup shuts down that custodian before interrupting the worker;
    ;; on success, ownership remains live until the stream cleanup thunk runs.
    ;; BUG-0019 W2: when a conn-pool is installed (networking.pool.enabled),
    ;; the connection is owned by the POOL's per-entry custodian — a
    ;; request-scoped custodian must NOT wrap pooled sockets, otherwise its
    ;; teardown would kill them (BUG_PLAN landmine). cleanup-response!
    ;; releases the entry back to the pool instead; the pool decides
    ;; keep-vs-discard via its framing/fault rules. Flag OFF (pool #f)
    ;; keeps the legacy request-custodian path byte-for-byte.
    (define pool (current-conn-pool))
    (define request-custodian (and (not pool) (make-custodian)))
    (define pooled-entry-box (box #f))
    (define (cleanup-response!)
      (if pool
          (let ([entry (unbox pooled-entry-box)])
            (when entry
              (with-handlers ([exn:fail? void])
                ;; Idempotent via released?-box; a faulted/abandoned entry
                ;; is discarded by the pool, never reused.
                (pool-release! pool entry))))
          (custodian-shutdown-all request-custodian)))
    ;; Convert pooled header LINES ("Field: value") to http-sendrecv-style
    ;; header pairs so downstream consumers see identical shapes.
    (define (header-lines->assoc lines)
      (for/list ([l (in-list lines)])
        (define m (regexp-match #rx"^([^:]+):[ \t]*(.*)$" l))
        (if m
            (cons (string->bytes/utf-8 (cadr m)) (string->bytes/utf-8 (caddr m)))
            (cons #"" #""))))
    (define result-vec
      (with-handlers ([exn:fail?
                       (lambda (e)
                         (cleanup-response!)
                         (if (provider-error? e)
                             (raise e)
                             (raise (provider-error
                                     (format "Network error contacting ~a: ~a" host (exn-message e))
                                     (current-continuation-marks)
                                     (hash)
                                     'network
                                     #f))))])
        (call-with-request-timeout
         (lambda ()
           ;; Pooled requests must NOT run under the request-scoped
           ;; custodian (it stays #f there); fall back to the ambient one.
           (parameterize ([current-custodian (or request-custodian (current-custodian))])
             ;; v1.00.13 W4 (#9478, RL-4): connect+TLS+status+
             ;; headers bounded by the policy TTFB window; the
             ;; structured phase is 'connect/ttfb.
             (define-values (sl rh rp)
               (provider-sendrecv/ttfb-bounded
                (request-network-policy-connect-ttfb-secs policy)
                (lambda ()
                  (if pool
                      ;; Pooled path: acquire from
                      ;; the pool, send via its
                      ;; minimal HTTP/1.1 client.
                      (let ([entry (pool-acquire! pool host (or req-port (if ssl? 443 80)) ssl?)])
                        (set-box! pooled-entry-box entry)
                        (define-values (s hs bp)
                          (pool-send-request! entry path-str #:headers headers #:data body-bytes))
                        (values (string->bytes/utf-8 s) (header-lines->assoc hs) bp))
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
                                         #:data body-bytes))))
                #:cleanup cleanup-response!))
             (vector sl rh rp)))
         #:timeout (request-network-policy-request-budget-secs policy)
         #:cleanup cleanup-response!)))
    (define status-line (vector-ref result-vec 0))
    (define response-headers (vector-ref result-vec 1))
    (define response-port (vector-ref result-vec 2))
    ;; Any failure after acquisition but before ownership reaches the lazy
    ;; generator must close the response port, including error-body timeouts.
    (with-handlers ([exn? (lambda (e)
                            (cleanup-response!)
                            (raise e))])
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
        (check-provider-status! "OpenAI" status-line err-body))
      (values response-port policy cleanup-response!)))

  (define (stream req)
    (define _stream-t0 (current-inexact-milliseconds))
    (define-values (response-port policy cleanup-thunk)
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
    ;; Response port, resolved policy, and cleanup from openai-stream-request
    ;; (W-06). Status OK — return an incremental generator that reads SSE
    ;; lines from the response port, yielding stream-chunk values as they
    ;; arrive.
    ;;
    ;; v1.00.13 W2 (#9466): all lifecycle windows come from the ONE resolved
    ;; request-network-policy (RL-3/RL-9). History: v0.15.1 widened the stream
    ;; timeout scaling; v0.45.12 L1 set the total cap to 2× the request budget
    ;; with a 600 s floor; v0.99.65 W0 separated thinking/content windows;
    ;; v0.99.78 kept initial short for dead-peer detection; v1.00.12 W1 (#9429)
    ;; moved the values behind the shared phase resolver. v1.00.13 freezes all
    ;; of these formulas in llm/request-policy.rkt — the adapter only consumes
    ;; the resolved fields below (regression matrices:
    ;; tests/test-sse-phase-timeout-bounds.rkt,
    ;; tests/test-provider-network-policy-conformance.rkt).
    (define stream-owns-resources? (box #f))
    (dynamic-wind
     void
     (lambda ()
       (define gen
         (stream-sse-events response-port
                            (lambda (parsed) (list (normalize-openai-chunk parsed)))
                            #:initial-timeout (request-network-policy-initial-idle-secs policy)
                            #:thinking-timeout (request-network-policy-thinking-idle-secs policy)
                            #:stream-timeout (request-network-policy-content-idle-secs policy)
                            #:max-total-timeout (request-network-policy-stream-total-secs policy)))
       ;; Own the response port for the complete lifetime of the lazy generator.
       ;; close-port-after-stream preserves the port across yields and also closes
       ;; it when a consumer abandons and collects the returned generator.
       (log-stream-setup-timing "openai" _stream-t0)
       (define provider-gen
         (generator ()
                    (with-handlers ([exn:break? (lambda (e)
                                                  (cleanup-thunk)
                                                  (raise e))]
                                    [exn:fail? (lambda (e)
                                                 (cleanup-thunk)
                                                 (openai-wrap-stream-error e))])
                      (let loop ()
                        (define chunk (gen))
                        (match chunk
                          [#f
                           (cleanup-thunk)
                           (yield #f)]
                          [_
                           (yield chunk)
                           (loop)])))))
       (define owned-stream
         (close-port-after-stream provider-gen response-port #:cleanup cleanup-thunk))
       ;; Transfer only after finalizer registration succeeds.
       (set-box! stream-owns-resources? #t)
       owned-stream)
     (lambda ()
       (unless (unbox stream-owns-resources?)
         (cleanup-thunk)))))

  (make-provider (lambda () "openai-compatible")
                 (lambda () (hasheq 'streaming #t 'token-counting #f))
                 send
                 stream))
