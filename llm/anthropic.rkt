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
         racket/string
         racket/port
         json
         net/url
         net/http-client
         "model.rkt"
         "provider.rkt"
         "stream.rkt")

(provide
 make-anthropic-provider
 anthropic-build-request-body
 anthropic-parse-response
 anthropic-parse-stream-chunks
 ;; Internal helpers for testing
 anthropic-translate-tool
 anthropic-translate-stop-reason
 anthropic-check-http-status!)

;; ============================================================
;; Constants
;; ============================================================

(define ANTHROPIC-DEFAULT-MODEL "claude-sonnet-4-20250514")
(define ANTHROPIC-DEFAULT-MAX-TOKENS 4096)
(define ANTHROPIC-VERSION "2023-06-01")
(define ANTHROPIC-DEFAULT-BASE-URL "https://api.anthropic.com")

;; ============================================================
;; Request body construction
;; ============================================================

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
         (hash 'role "user"
               'content (list (hash 'type "tool_result"
                                    'tool_use_id tool-call-id
                                    'content tool-result-content)))]
        ;; Assistant with list content (tool calls)
        [(and (equal? role "assistant") (list? content))
         (hash 'role "assistant"
               'content
               (for/list ([block (in-list content)])
                 (define btype (hash-ref block 'type "text"))
                 (cond
                   [(equal? btype "text")
                    (hash 'type "text" 'text (hash-ref block 'text ""))]
                   [(equal? btype "tool-call")
                    (hash 'type "tool_use"
                          'id (hash-ref block 'id "")
                          'name (hash-ref block 'name "")
                          'input (hash-ref block 'arguments (hash)))]
                   [else block])))]
        ;; Simple string content: wrap in text block
        [(string? content)
         (hash 'role role
               'content (list (hash 'type "text" 'text content)))]
        ;; Fallback: pass through
        [else msg])))

  (define base
    (hasheq 'model model-name
            'max_tokens max-tokens
            'messages messages
            'stream stream?))

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
  (define parameters (hash-ref fn 'parameters (hash)))
  (hash 'name name
        'description description
        'input_schema parameters))

;; ============================================================
;; Response parsing
;; ============================================================

;; Convert Anthropic Messages API response to model-response.
(define (anthropic-parse-response raw)
  (define model-name (hash-ref raw 'model "unknown"))
  (define usage-raw (hash-ref raw 'usage (hash)))
  (define stop-reason-raw (hash-ref raw 'stop_reason "end_turn"))
  (define content-blocks (hash-ref raw 'content '()))

  ;; Translate stop reason
  (define stop-reason (anthropic-translate-stop-reason stop-reason-raw))

  ;; Translate usage: input_tokens → prompt_tokens, output_tokens → completion_tokens
  (define usage
    (hash 'prompt_tokens (hash-ref usage-raw 'input_tokens 0)
          'completion_tokens (hash-ref usage-raw 'output_tokens 0)
          'total_tokens (+ (hash-ref usage-raw 'input_tokens 0)
                           (hash-ref usage-raw 'output_tokens 0))))

  ;; Translate content blocks
  (define content
    (for/list ([block (in-list content-blocks)])
      (define type (hash-ref block 'type "text"))
      (cond
        [(equal? type "text")
         (hash 'type "text" 'text (hash-ref block 'text ""))]
        [(equal? type "tool_use")
         (hash 'type "tool-call"
               'id (hash-ref block 'id "")
               'name (hash-ref block 'name "")
               'arguments (hash-ref block 'input (hash)))]
        [else
         block])))

  (make-model-response content usage model-name stop-reason))

;; Translate Anthropic stop reasons to normalized symbols.
(define (anthropic-translate-stop-reason reason)
  (cond
    [(string? reason)
     (let ([r (string-trim reason)])
       (cond
         [(equal? r "end_turn") 'stop]
         [(equal? r "max_tokens") 'length]
         [(equal? r "stop_sequence") 'stop]
         [(equal? r "tool_use") 'tool-calls]
         [else (string->symbol r)]))]
    [(symbol? reason) reason]
    [else 'stop]))

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
  (define results '())
  (define current-tool-id #f)
  (define current-tool-name #f)
  (define current-tool-index 0)

  (for ([event (in-list raw-events)])
    (define type (hash-ref event 'type #f))
    (cond
      ;; Text delta
      [(equal? type "content_block_delta")
       (define delta (hash-ref event 'delta (hash)))
       (define delta-type (hash-ref delta 'type #f))
       (cond
         [(equal? delta-type "text_delta")
          (define text (hash-ref delta 'text ""))
          (set! results
                (cons (stream-chunk text #f #f #f) results))]
         [(equal? delta-type "input_json_delta")
          ;; Partial JSON for tool input — emit as tool-call delta
          (define partial-json (hash-ref delta 'partial_json ""))
          (set! results
                (cons (stream-chunk
                       #f
                       (hash 'index current-tool-index
                             'id current-tool-id
                             'function (hash 'name current-tool-name
                                             'arguments partial-json))
                       #f #f) results))]
         [else (void)])]

      ;; Tool use block starts
      [(equal? type "content_block_start")
       (define content-block (hash-ref event 'content_block (hash)))
       (define cb-type (hash-ref content-block 'type #f))
       (define idx (hash-ref event 'index 0))
       (when (equal? cb-type "tool_use")
         (set! current-tool-id (hash-ref content-block 'id ""))
         (set! current-tool-name (hash-ref content-block 'name ""))
         (set! current-tool-index idx))]

      ;; Message delta: usage + stop reason → done chunk
      [(equal? type "message_delta")
       (define delta (hash-ref event 'delta (hash)))
       (define usage-raw (hash-ref event 'usage (hash)))
       (define stop-reason (hash-ref delta 'stop_reason #f))
       (define out-tokens (hash-ref usage-raw 'output_tokens 0))
       (define usage (hash 'completion_tokens out-tokens))
       (set! results
             (cons (stream-chunk #f #f usage #t) results))]

      ;; message_start: extract initial usage
      [(equal? type "message_start")
       (define message (hash-ref event 'message (hash)))
       (define usage-raw (hash-ref message 'usage (hash)))
       (define in-tokens (hash-ref usage-raw 'input_tokens 0))
       (when (> in-tokens 0)
         (set! results
               (cons (stream-chunk #f #f (hash 'prompt_tokens in-tokens) #f) results)))]

      [else (void)]))

  (reverse results))

;; ============================================================
;; HTTP status check (exported for tests)
;; ============================================================

(define (anthropic-check-http-status! status-line response-body)
  (define status-str (if (bytes? status-line)
                         (bytes->string/utf-8 status-line)
                         status-line))
  (define status-code
    (let ([parts (regexp-match #rx"^HTTP/[^ ]+ ([0-9]+)" status-str)])
      (if parts (string->number (cadr parts)) 0)))
  (when (>= status-code 400)
    (define error-body (if (bytes? response-body)
                           (bytes->string/utf-8 response-body)
                           response-body))
    (cond
      [(= status-code 401)
       (raise (exn:fail (format "Anthropic API authentication failed (401): ~a"
                                error-body)
                        (current-continuation-marks)))]
      [(= status-code 403)
       (raise (exn:fail (format "Anthropic API forbidden (403): ~a"
                                error-body)
                        (current-continuation-marks)))]
      [(= status-code 429)
       (raise (exn:fail (format "Anthropic API rate limited (429): ~a"
                                error-body)
                        (current-continuation-marks)))]
      [(>= status-code 500)
       (raise (exn:fail (format "Anthropic API server error (~a): ~a"
                                status-code
                                error-body)
                        (current-continuation-marks)))]
      [else
       (raise (exn:fail (format "Anthropic API error (~a): ~a"
                                status-code
                                error-body)
                        (current-continuation-marks)))])))

;; ============================================================
;; HTTP request execution (non-streaming)
;; ============================================================

(define (anthropic-do-http-request base-url api-key path body)
  (define url-str (string-append (string-trim base-url "/") path))
  (define uri (string->url url-str))
  (define headers
    (list (format "x-api-key: ~a" api-key)
          (format "anthropic-version: ~a" ANTHROPIC-VERSION)
          "Content-Type: application/json"))
  (define body-bytes (jsexpr->bytes body))
  (define-values (status-line response-headers response-port)
    (http-sendrecv uri 'POST
                   #:headers headers
                   #:data body-bytes))
  (define response-body (read-response-body response-port))
  ;; Check HTTP status
  (anthropic-check-http-status! status-line response-body)
  (bytes->jsexpr response-body))

;; ============================================================
;; Provider constructor
;; ============================================================

(define (make-anthropic-provider config)
  (define base-url (hash-ref config 'base-url ANTHROPIC-DEFAULT-BASE-URL))
  (define api-key (hash-ref config 'api-key ""))
  (define default-model (hash-ref config 'model ANTHROPIC-DEFAULT-MODEL))

  (define (send req)
    (define merged-req
      (if (hash-has-key? (model-request-settings req) 'model)
          req
          (make-model-request
           (model-request-messages req)
           (model-request-tools req)
           (hash-set (model-request-settings req) 'model default-model))))
    (define body (anthropic-build-request-body merged-req))
    (define raw (anthropic-do-http-request base-url api-key "/v1/messages" body))
    (anthropic-parse-response raw))

  (define (stream req)
    (define merged-req
      (if (hash-has-key? (model-request-settings req) 'model)
          req
          (make-model-request
           (model-request-messages req)
           (model-request-tools req)
           (hash-set (model-request-settings req) 'model default-model))))
    (define body (anthropic-build-request-body merged-req #:stream? #t))
    (define url-str (string-append (string-trim base-url "/") "/v1/messages"))
    (define uri (string->url url-str))
    (define headers
      (list (format "x-api-key: ~a" api-key)
            (format "anthropic-version: ~a" ANTHROPIC-VERSION)
            "Content-Type: application/json"))
    (define body-bytes (jsexpr->bytes body))
    (define-values (status-line response-headers response-port)
      (http-sendrecv uri 'POST
                     #:headers headers
                     #:data body-bytes))
    ;; Check HTTP status before streaming
    (define status-code
      (let ([parts (regexp-match #rx"^HTTP/[^ ]+ ([0-9]+)" (bytes->string/utf-8 status-line))])
        (if parts (string->number (cadr parts)) 0)))
    (when (>= status-code 400)
      (define resp-body (read-response-body/timeout response-port))
      (anthropic-check-http-status! status-line resp-body))
    ;; Incremental SSE parsing — read line-by-line from port
    (define raw-events
      (let loop ([acc '()])
        (define line (read-line/timeout response-port))
        (cond
          [(eq? line #f) (reverse acc)]  ; timeout — return what we have
          [(eof-object? line) (reverse acc)]
          [else
           (let ([parsed (parse-sse-line line)])
             (if (hash? parsed)
                 (loop (cons parsed acc))
                 (loop acc)))])))
    (anthropic-parse-stream-chunks raw-events))

  (make-provider
   (lambda () "anthropic")
   (lambda () (hash 'streaming #t 'token-counting #f))
   send
   stream))
