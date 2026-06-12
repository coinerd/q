#lang racket/base

;; llm/anthropic.rkt — Anthropic provider adapter
;;
;; Translates normalized model-request structs into Anthropic Messages
;; API format, and parses responses back into model-response.
;; Supports both non-streaming and streaming modes.
;;
;; HTTP calls use net/http-client from Racket stdlib.
;; SSE parsing delegates to llm/stream.rkt.

(require racket/contract
         "timing.rkt"
         "../util/error/error-helpers.rkt"
         racket/match
         racket/string
         (only-in "model-defaults.rkt" ANTHROPIC-DEFAULT-MODEL ANTHROPIC-DEFAULT-BASE-URL)
         racket/port
         racket/generator
         json
         net/url
         net/http-client
         "model.rkt"
         "provider.rkt"
         "stream.rkt"
         "http-helpers.rkt"
         (only-in "vision-helpers.rkt" parse-data-url))

;; Provider constructor
(provide (contract-out [make-anthropic-provider (-> hash? provider?)])
         ;; Request/response helpers (exported for testing)
         anthropic-build-request-body
         anthropic-parse-response
         anthropic-parse-stream-chunks
         anthropic-parse-single-event
         ;; Internal helpers for testing
         anthropic-translate-tool)

;; ============================================================
;; Constants
;; ============================================================

;; model defaults in llm/model-defaults.rkt
(define ANTHROPIC-DEFAULT-MAX-TOKENS 16384)
(define ANTHROPIC-VERSION "2023-06-01")

;; ============================================================
;; Provider name helper (supports Kimi coding plan re-use)
;; ============================================================
(define (anthropic-provider-name config)
  (hash-ref config 'provider-name "anthropic"))

;; ============================================================
;; Request body construction
;; ============================================================

;; ============================================================
;; Image block conversion (GAP-V1: cross-provider vision)
;; ============================================================

;; Convert OpenAI-format content blocks to Anthropic content blocks.
(define (openai-block->anthropic block)
  (define btype (hash-ref block 'type "text"))
  (cond
    [(equal? btype "text") block]
    [(equal? btype "image_url")
     (define image-url-hash (hash-ref block 'image_url (hasheq)))
     (define url (hash-ref image-url-hash 'url ""))
     (define-values (mime data) (parse-data-url url))
     (hasheq 'type "image" 'source (hasheq 'type "base64" 'media_type mime 'data data))]
    [else block]))

;; Convert normalized model-request to Anthropic Messages API body.
(define (anthropic-build-request-body req #:stream? [stream? #f])
  (define settings (model-request-settings req))
  (define model-name (hash-ref settings 'model ANTHROPIC-DEFAULT-MODEL))
  (define max-tokens (hash-ref settings 'max-tokens ANTHROPIC-DEFAULT-MAX-TOKENS))

  ;; Build messages — Anthropic requires content as typed blocks
  (define raw-messages (model-request-messages req))
  (define messages
    (for/list ([msg (in-list raw-messages)])
      (define role (hash-ref msg 'role "user"))
      (define content (hash-ref msg 'content ""))
      (cond
        ;; System role: pass through unchanged
        [(equal? role "system") msg]
        ;; Tool role: convert to Anthropic user+tool_result format
        [(equal? role "tool")
         (define tool-call-id
           (if (hash? content)
               (hash-ref content 'toolCallId "")
               ""))
         (define tool-result-content
           (if (hash? content)
               (hash-ref content 'content "")
               (if (string? content) content "")))
         (hasheq
          'role
          "user"
          'content
          (list (hasheq 'type "tool_result" 'tool_use_id tool-call-id 'content tool-result-content)))]
        ;; User with list content (text + images) (GAP-V1)
        [(and (equal? role "user") (list? content))
         (hasheq 'role
                 role
                 'content
                 (for/list ([block (in-list content)])
                   (openai-block->anthropic block)))]
        ;; Assistant with list content (tool calls)
        [(and (equal? role "assistant") (list? content))
         (hasheq 'role
                 "assistant"
                 'content
                 (for/list ([block (in-list content)])
                   (define btype (hash-ref block 'type "text"))
                   (match btype
                     ["text" (hasheq 'type "text" 'text (hash-ref block 'text ""))]
                     ["tool-call"
                      (hasheq 'type
                              "tool_use"
                              'id
                              (hash-ref block 'id "")
                              'name
                              (hash-ref block 'name "")
                              'input
                              (hash-ref block 'arguments (hasheq)))]
                     [_ block])))]
        ;; Simple string content: wrap in text block
        [(string? content) (hasheq 'role role 'content (list (hasheq 'type "text" 'text content)))]
        ;; Fallback: pass through
        [else msg])))

  (define base (hasheq 'model model-name 'max_tokens max-tokens 'messages messages 'stream stream?))

  ;; Add optional temperature
  (define with-temp
    (if (hash-has-key? settings 'temperature)
        (hash-set base 'temperature (hash-ref settings 'temperature))
        base))

  ;; Add optional system prompt from settings
  (define with-system
    (if (hash-has-key? settings 'system)
        (hash-set with-temp 'system (hash-ref settings 'system))
        with-temp))

  ;; Translate tools to Anthropic format if present
  (define with-tools
    (if (model-request-tools req)
        (hash-set with-system
                  'tools
                  (for/list ([tool (in-list (model-request-tools req))])
                    (anthropic-translate-tool tool)))
        with-system))

  with-tools)

;; Translate a normalized tool definition to Anthropic format.
;; Input: {"type":"function","function":{"name":"...","description":"...","parameters":{}}}
;; Output: {"name":"...","description":"...","input_schema":{...}}
(define (anthropic-translate-tool tool)
  (define fn (hash-ref tool 'function tool))
  (define name (hash-ref fn 'name "unknown"))
  (define description (hash-ref fn 'description ""))
  (define parameters (hash-ref fn 'parameters (hasheq)))
  (hasheq 'name name 'description description 'input_schema parameters))

;; ============================================================
;; Response parsing
;; ============================================================

;; Convert Anthropic Messages API response to model-response.
(define (anthropic-parse-response raw)
  (define model-name (hash-ref raw 'model "unknown"))
  (define usage-raw (hash-ref raw 'usage (hasheq)))
  (define stop-reason-raw (hash-ref raw 'stop_reason "end_turn"))
  (define content-blocks (hash-ref raw 'content '()))

  ;; Translate stop reason
  (define stop-reason (translate-stop-reason 'anthropic stop-reason-raw))

  ;; Translate usage: input_tokens → prompt_tokens, output_tokens → completion_tokens
  (define usage
    (hasheq 'prompt_tokens
            (hash-ref usage-raw 'input_tokens 0)
            'completion_tokens
            (hash-ref usage-raw 'output_tokens 0)
            'total_tokens
            (+ (hash-ref usage-raw 'input_tokens 0) (hash-ref usage-raw 'output_tokens 0))))

  ;; Translate content blocks
  (define content
    (for/list ([block (in-list content-blocks)])
      (define type (hash-ref block 'type "text"))
      (match type
        ["text" (hasheq 'type "text" 'text (hash-ref block 'text ""))]
        ["tool_use"
         (define tc-hash
           (hasheq 'type
                   "tool-call"
                   'id
                   (hash-ref block 'id "")
                   'name
                   (hash-ref block 'name "")
                   'arguments
                   (hash-ref block 'input (hasheq))))
         ;; Shadow: validate round-trip through tool-call-intent
         (define tci (hash->tool-call-intent tc-hash))
         (define round-trip (tool-call-intent->hash tci))
         (unless (equal? (hash-ref tc-hash 'name) (hash-ref round-trip 'name))
           (log-warning "tool-call-intent shadow mismatch in anthropic: ~a vs ~a"
                        (hash-ref tc-hash 'name)
                        (hash-ref round-trip 'name)))
         tc-hash]
        [_ block])))

  (make-model-response content usage model-name stop-reason))

;; ============================================================
;; Stream chunk parsing
;; ============================================================

;; Parse Anthropic SSE events into canonical stream-chunk structs.
;; Anthropic streaming events:
;;   - message_start: initial metadata
;;   - content_block_start: new content block begins
;;   - content_block_delta: text/tool delta
;;   - content_block_stop: content block ends
;;   - message_delta: usage + stop_reason
;;   - message_stop: stream complete
(define (anthropic-parse-stream-chunks raw-events)
  (define current-tool-id (box #f))
  (define current-tool-name (box #f))
  (define current-tool-index (box 0))
  (define results '())
  (for ([event (in-list raw-events)])
    (set!
     results
     (append
      results
      (anthropic-parse-single-event event current-tool-id current-tool-name current-tool-index))))
  results)

;; ============================================================
;; Per-event stream parsing (for incremental generator)
;; ============================================================

;; Parse a single Anthropic SSE event into a list of stream-chunks.
;; Mutates tool-id-box, tool-name-box, tool-index-box to track tool state.
(define (anthropic-parse-single-event event tool-id-box tool-name-box tool-index-box)
  (define type (hash-ref event 'type #f))
  (define results '())
  (match type
    ;; Text delta
    ["content_block_delta"
     (define delta (hash-ref event 'delta (hasheq)))
     (define delta-type (hash-ref delta 'type #f))
     (match delta-type
       ["text_delta"
        (define text (hash-ref delta 'text ""))
        (set! results (cons (make-stream-chunk text #f #f #f) results))]
       ["input_json_delta"
        ;; Partial JSON for tool input — emit as tool-call delta
        (define partial-json (hash-ref delta 'partial_json ""))
        (set! results
              (cons (make-stream-chunk
                     #f
                     (hasheq 'index
                             (unbox tool-index-box)
                             'id
                             (unbox tool-id-box)
                             'function
                             (hasheq 'name (unbox tool-name-box) 'arguments partial-json))
                     #f
                     #f)
                    results))]
       [_ (void)])]

    ;; Tool use block starts
    ["content_block_start"
     (define content-block (hash-ref event 'content_block (hasheq)))
     (define cb-type (hash-ref content-block 'type #f))
     (define idx (hash-ref event 'index 0))
     (when (equal? cb-type "tool_use")
       (set-box! tool-id-box (hash-ref content-block 'id ""))
       (set-box! tool-name-box (hash-ref content-block 'name ""))
       (set-box! tool-index-box idx))]

    ;; Message delta: usage + stop reason → done chunk
    ["message_delta"
     (define delta (hash-ref event 'delta (hasheq)))
     (define usage-raw (hash-ref event 'usage (hasheq)))
     (define stop-reason (hash-ref delta 'stop_reason #f))
     (define out-tokens (hash-ref usage-raw 'output_tokens 0))
     (define usage (hasheq 'completion_tokens out-tokens))
     (set! results (cons (make-stream-chunk #f #f usage #t) results))]

    ;; message_start: extract initial usage
    ["message_start"
     (define message (hash-ref event 'message (hasheq)))
     (define usage-raw (hash-ref message 'usage (hasheq)))
     (define in-tokens (hash-ref usage-raw 'input_tokens 0))
     (when (> in-tokens 0)
       (set! results (cons (make-stream-chunk #f #f (hasheq 'prompt_tokens in-tokens) #f) results)))]

    [_ (void)])
  (reverse results))

;; ============================================================
;; HTTP status check (exported for tests)
;; ============================================================
;; HTTP request execution (non-streaming)
;; ============================================================

(define (anthropic-do-http-request base-url api-key path body [provider-name "anthropic"])
  (define url-str (string-append (string-trim base-url "/") path))
  (define headers
    (list* (format "x-api-key: ~a" api-key)
           (format "anthropic-version: ~a" ANTHROPIC-VERSION)
           "Content-Type: application/json"
           (if (equal? provider-name "kimi-coding")
               (list "User-Agent: KimiCLI/1.5")
               '())))
  (make-provider-http-request url-str
                              headers
                              (jsexpr->bytes body)
                              #:status-checker
                              (lambda (sl rb) (check-provider-status! "Anthropic" sl rb))))

;; ============================================================
;; Provider constructor
;; ============================================================
;; Kimi eager-stream helper: reads full HTTP response inside
;; dynamic-wind, parses as JSON, replays as stream chunks.
;; ============================================================

(define (kimi-eager-stream-chunks req base-url api-key provider-name default-model)
  (define chunks-box (box '()))
  (define merged-req (ensure-model-setting req default-model))
  ;; Non-streaming request — response is pure JSON, easier to parse
  (define body (anthropic-build-request-body merged-req #:stream? #f))
  (define url-str (string-append (string-trim base-url "/") "/v1/messages"))
  (define uri (string->url url-str))
  (define host (url-host uri))
  (define path-str
    (string-append "/"
                   (string-join (for/list ([p (in-list (url-path uri))])
                                  (path/param-path p))
                                "/")))
  (define ssl? (and (url-scheme uri) (not (equal? (url-scheme uri) "http"))))
  (define port-num (or (url-port uri) (if ssl? 443 80)))
  (define headers
    (list (format "x-api-key: ~a" api-key)
          (format "anthropic-version: ~a" ANTHROPIC-VERSION)
          "Content-Type: application/json"
          "User-Agent: KimiCLI/1.5"))
  (define body-bytes (jsexpr->bytes body))
  (define response-port-box (box #f))
  (dynamic-wind
   (lambda () (void))
   (lambda ()
     (define result-vec
       (call-with-request-timeout #:cleanup (lambda ()
                                              (define rp (unbox response-port-box))
                                              (when rp
                                                (with-logged-error "port cleanup"
                                                                   (close-input-port rp))))
                                  (lambda ()
                                    (define-values (sl rh rp)
                                      (http-sendrecv host
                                                     path-str
                                                     #:port port-num
                                                     #:ssl? ssl?
                                                     #:method #"POST"
                                                     #:headers headers
                                                     #:data body-bytes))
                                    (set-box! response-port-box rp)
                                    (vector sl rh rp))))
     (define status-line (vector-ref result-vec 0))
     (define response-port (vector-ref result-vec 2))
     (define status-code
       (let ([parts (regexp-match #rx"^HTTP/[^ ]+ ([0-9]+)" (bytes->string/utf-8 status-line))])
         (if parts
             (string->number (cadr parts))
             0)))
     (when (>= status-code 400)
       (define resp-body (port->bytes response-port))
       (check-provider-status! "Anthropic" status-line (bytes->string/utf-8 resp-body)))
     ;; Read full response body inside dynamic-wind (port is still open)
     (define full-body
       (if (>= status-code 400)
           #""
           (port->bytes response-port)))
     ;; Parse as non-streaming JSON response
     (define resp-json (string->jsexpr (bytes->string/utf-8 full-body)))
     (define resp (anthropic-parse-response resp-json))
     ;; Emit chunks for each content block
     (for ([part (in-list (model-response-content resp))]
           [idx (in-naturals)])
       (cond
         [(hash-ref part 'text #f)
          ;; Text content block
          (set-box! chunks-box
                    (cons (make-stream-chunk (hash-ref part 'text) #f #f #f) (unbox chunks-box)))]
         [(equal? (hash-ref part 'type #f) "tool-call")
          ;; Tool call — emit as tool-call delta with full arguments
          (define args-str (jsexpr->string (hash-ref part 'arguments (hasheq))))
          (set-box! chunks-box
                    (cons (make-stream-chunk
                           #f
                           (hasheq 'index
                                   idx
                                   'id
                                   (hash-ref part 'id "")
                                   'function
                                   (hasheq 'name (hash-ref part 'name "") 'arguments args-str))
                           #f
                           #f)
                          (unbox chunks-box)))]))
     ;; Collect done chunk with usage
     (when (model-response-usage resp)
       (set-box! chunks-box
                 (cons (make-stream-chunk #f
                                          #f
                                          (model-response-usage resp)
                                          #t
                                          #:finish-reason
                                          (symbol->string (model-response-stop-reason resp)))
                       (unbox chunks-box)))))
   (lambda ()
     (define rp (unbox response-port-box))
     (when rp
       (with-logged-error "port cleanup" (close-input-port rp)))))
  ;; Replay collected chunks from an in-memory generator
  (define all-chunks (reverse (unbox chunks-box)))
  (generator ()
             (for ([c (in-list all-chunks)])
               (yield c))
             (let loop ()
               (yield #f)
               (loop))))

;; ============================================================

(define (make-anthropic-provider config)
  (validate-api-key! "Anthropic" "ANTHROPIC_API_KEY" config)
  (define base-url (hash-ref config 'base-url ANTHROPIC-DEFAULT-BASE-URL))
  (define api-key (hash-ref config 'api-key ""))
  (define default-model (hash-ref config 'model ANTHROPIC-DEFAULT-MODEL))
  (define provider-name (anthropic-provider-name config))

  (define (send req)
    (define merged-req (ensure-model-setting req default-model))
    (define body (anthropic-build-request-body merged-req))
    (define raw (anthropic-do-http-request base-url api-key "/v1/messages" body provider-name))
    (anthropic-parse-response raw))

  ;; W10.1 (Q-19): dynamic-wind ensures response port cleanup on timeout/exception
  (define (stream req)
    ;; Kimi coding plan: dynamic-wind closes the response port before the
    ;; SSE generator is consumed. Eagerly collect all SSE chunks inside
    ;; dynamic-wind, then replay from an in-memory list.
    (if (equal? provider-name "kimi-coding")
        ;; Kimi: dynamic-wind closes port before generator is consumed.
        ;; Send a streaming request, read the full body inside dynamic-wind,
        ;; parse as non-streaming JSON, and replay chunks from memory.
        (kimi-eager-stream-chunks req base-url api-key provider-name default-model)
        (let ()
          (define _stream-t0 (current-inexact-milliseconds))
          (define merged-req (ensure-model-setting req default-model))
          (define body (anthropic-build-request-body merged-req #:stream? #t))
          (define url-str (string-append (string-trim base-url "/") "/v1/messages"))
          (define uri (string->url url-str))
          (define host (url-host uri))
          (define path-str
            (string-append "/"
                           (string-join (for/list ([p (in-list (url-path uri))])
                                          (path/param-path p))
                                        "/")))
          (define ssl? (and (url-scheme uri) (not (equal? (url-scheme uri) "http"))))
          (define port (or (url-port uri) (if ssl? 443 80)))
          (define headers
            (list* (format "x-api-key: ~a" api-key)
                   (format "anthropic-version: ~a" ANTHROPIC-VERSION)
                   "Content-Type: application/json"
                   "Accept: text/event-stream"
                   (if (equal? provider-name "kimi-coding")
                       (list "User-Agent: KimiCLI/1.5")
                       '())))
          (define body-bytes (jsexpr->bytes body))
          (define response-port-box (box #f))
          (dynamic-wind (lambda () (void))
                        (lambda ()
                          ;; Wrap initial HTTP request in overall timeout (SEC-11)
                          (define result-vec
                            (call-with-request-timeout
                             #:cleanup (lambda ()
                                         (define rp (unbox response-port-box))
                                         (when rp
                                           (with-logged-error "port cleanup" (close-input-port rp))))
                             (lambda ()
                               (define-values (sl rh rp)
                                 (http-sendrecv host
                                                path-str
                                                #:port port
                                                #:ssl? ssl?
                                                #:method #"POST"
                                                #:headers headers
                                                #:data body-bytes))
                               (set-box! response-port-box rp)
                               (vector sl rh rp))))
                          (define status-line (vector-ref result-vec 0))
                          (define response-headers (vector-ref result-vec 1))
                          (define response-port (vector-ref result-vec 2))
                          ;; Check HTTP status before streaming
                          (define status-code
                            (let ([parts (regexp-match #rx"^HTTP/[^ ]+ ([0-9]+)"
                                                       (bytes->string/utf-8 status-line))])
                              (if parts
                                  (string->number (cadr parts))
                                  0)))
                          (when (>= status-code 400)
                            (define resp-body (read-response-body/timeout response-port))
                            (check-provider-status! "Anthropic" status-line resp-body))
                          ;; Incremental SSE parsing — generator yields chunks one at a time
                          (define raw-port response-port)
                          (define current-tool-id (box #f))
                          (define current-tool-name (box #f))
                          (define current-tool-index (box 0))
                          (log-stream-setup-timing "anthropic" _stream-t0)
                          (stream-sse-events raw-port
                                             (lambda (parsed)
                                               (anthropic-parse-single-event parsed
                                                                             current-tool-id
                                                                             current-tool-name
                                                                             current-tool-index))))
                        (lambda ()
                          (define rp (unbox response-port-box))
                          (when rp
                            (with-logged-error "port cleanup" (close-input-port rp))))))))

  (make-provider (lambda () (anthropic-provider-name config))
                 (lambda () (hasheq 'streaming #t 'token-counting #f))
                 send
                 stream))
