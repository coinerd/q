#lang racket/base

;; llm/gemini.rkt — Google Gemini provider adapter
;;
;; Translates normalized model-request structs into Gemini
;; generateContent/streamGenerateContent API format, and parses
;; responses back into model-response.
;; Supports both non-streaming and streaming modes.
;;
;; HTTP calls use net/http-client from Racket stdlib.
;; SSE parsing delegates to llm/stream.rkt.

(require racket/contract
         racket/string
         (only-in "model-defaults.rkt" GEMINI-DEFAULT-MODEL GEMINI-DEFAULT-BASE-URL)
         racket/port
         racket/format
         racket/generator
         json
         net/url
         net/http-client
         "model.rkt"
         "provider.rkt"
         "stream.rkt"
         "http-helpers.rkt")

;; Provider constructor
(provide (contract-out [make-gemini-provider (-> hash? provider?)])
         ;; Request/response helpers (exported for testing)
         gemini-build-request-body
         gemini-parse-response
         gemini-parse-stream-chunks
         gemini-parse-single-event
         ;; Internal helpers for testing
         gemini-translate-tool
         gemini-translate-stop-reason
         gemini-check-http-status!
         gemini-gen-tool-id
         gemini-reset-tool-id-counter!)

;; ============================================================
;; Constants
;; ============================================================

;; model defaults in llm/model-defaults.rkt
(define GEMINI-DEFAULT-MAX-TOKENS 4096)

;; Per-request tool call ID counter (Issue #200)
;; Uses a parameter so concurrent requests don't share/collide on IDs.
;; The parameter is bound fresh in each send/stream call.
(define gemini-tool-id-counter-param (make-parameter 0))

;; Generate a unique tool call ID within the current request.
(define (gemini-gen-tool-id)
  (define next (add1 (gemini-tool-id-counter-param)))
  (gemini-tool-id-counter-param next)
  (format "gemini_~a" next))

;; Reset the parameter for a fresh request.
(define (gemini-reset-tool-id-counter!)
  (gemini-tool-id-counter-param 0))

;; ============================================================
;; Request body construction
;; ============================================================

;; Convert normalized model-request to Gemini generateContent API body.
(define (gemini-build-request-body req #:stream? [stream? #f])
  (define settings (model-request-settings req))
  (define model-name (hash-ref settings 'model GEMINI-DEFAULT-MODEL))
  (define max-tokens (hash-ref settings 'max-tokens GEMINI-DEFAULT-MAX-TOKENS))

  ;; Build contents — Gemini uses "contents" with "parts" arrays
  ;; Role mapping: "assistant" → "model", "user" → "user"
  (define raw-messages (model-request-messages req))

  ;; Build call-id → name map from assistant tool-call blocks (Issue #107)
  (define call-id->name
    (for*/fold ([acc (hash)])
               ([msg (in-list raw-messages)]
                #:when (equal? (hash-ref msg 'role "") "assistant")
                [block (in-list (let ([c (hash-ref msg 'content "")])
                                  (if (list? c)
                                      c
                                      '())))])
      (if (and (hash? block) (equal? (hash-ref block 'type #f) "tool-call"))
          (hash-set acc (hash-ref block 'id "") (hash-ref block 'name ""))
          acc)))

  (define contents
    (for/list ([msg (in-list raw-messages)])
      (define role (hash-ref msg 'role "user"))
      (define content (hash-ref msg 'content ""))
      ;; Map roles
      (define gemini-role
        (cond
          [(equal? role "assistant") "model"]
          [(equal? role "system") "user"]
          [(equal? role "tool") "user"]
          [else role]))
      ;; Build parts based on content type and role
      (define parts
        (cond
          ;; Tool role: convert to functionResponse
          [(equal? role "tool")
           (define tool-call-id
             (if (hash? content)
                 (hash-ref content 'toolCallId "")
                 ""))
           (define tool-result-content
             (if (hash? content)
                 (hash-ref content 'content "")
                 (if (string? content) content "")))
           (define tool-name (hash-ref call-id->name tool-call-id ""))
           (list (hasheq 'functionResponse
                         (hasheq 'name tool-name 'response (hasheq 'content tool-result-content))))]
          ;; Assistant with list content (tool calls + text)
          [(and (equal? role "assistant") (list? content))
           (for/list ([block (in-list content)])
             (define btype (hash-ref block 'type "text"))
             (cond
               [(equal? btype "text") (hasheq 'text (hash-ref block 'text ""))]
               [(equal? btype "tool-call")
                (hasheq
                 'functionCall
                 (hasheq 'name (hash-ref block 'name "") 'args (hash-ref block 'arguments (hasheq))))]
               [else block]))]
          ;; Simple string content: text part
          [(string? content) (list (hasheq 'text content))]
          ;; Fallback
          [else content]))
      (hasheq 'role gemini-role 'parts parts)))

  ;; Build generationConfig
  (define gen-config
    (let ([base-config (hasheq 'maxOutputTokens max-tokens)])
      (if (hash-has-key? settings 'temperature)
          (hash-set base-config 'temperature (hash-ref settings 'temperature))
          base-config)))

  ;; Base body
  (define base (hasheq 'contents contents 'generationConfig gen-config))

  ;; Add optional system prompt — goes to systemInstruction, not contents
  (define with-system
    (if (hash-has-key? settings 'system)
        (hash-set base
                  'systemInstruction
                  (hasheq 'parts (list (hasheq 'text (hash-ref settings 'system)))))
        base))

  ;; Translate tools to Gemini format if present
  (define with-tools
    (if (model-request-tools req)
        (hash-set with-system
                  'tools
                  (list (hasheq 'functionDeclarations
                                (for/list ([tool (in-list (model-request-tools req))])
                                  (gemini-translate-tool tool)))))
        with-system))

  with-tools)

;; Translate a normalized tool definition to Gemini functionDeclaration format.
;; Input: {"type":"function","function":{"name":"...","description":"...","parameters":{}}}
;; Output: {"name":"...","description":"...","parameters":{...}}
(define (gemini-translate-tool tool)
  (define fn (hash-ref tool 'function tool))
  (define name (hash-ref fn 'name "unknown"))
  (define description (hash-ref fn 'description ""))
  (define parameters (hash-ref fn 'parameters (hash)))
  (hasheq 'name name 'description description 'parameters parameters))

;; ============================================================
;; Response parsing
;; ============================================================

;; Convert Gemini generateContent API response to model-response.
(define (gemini-parse-response raw)
  (define candidates (hash-ref raw 'candidates '()))
  (define usage-raw (hash-ref raw 'usageMetadata (hasheq)))
  (define model-version (hash-ref raw 'modelVersion GEMINI-DEFAULT-MODEL))

  ;; Extract first candidate
  (define candidate
    (if (null? candidates)
        #f
        (car candidates)))
  (define content-obj
    (if candidate
        (hash-ref candidate 'content #f)
        #f))
  (define parts
    (if content-obj
        (hash-ref content-obj 'parts '())
        '()))
  (define finish-reason
    (if candidate
        (hash-ref candidate 'finishReason "STOP")
        "STOP"))

  ;; Translate stop reason
  (define stop-reason (gemini-translate-stop-reason finish-reason))

  ;; Translate usage
  (define prompt-tokens (hash-ref usage-raw 'promptTokenCount 0))
  (define candidates-tokens (hash-ref usage-raw 'candidatesTokenCount 0))
  (define total-tokens (hash-ref usage-raw 'totalTokenCount (+ prompt-tokens candidates-tokens)))
  (define usage
    (hasheq 'prompt_tokens
            prompt-tokens
            'completion_tokens
            candidates-tokens
            'total_tokens
            total-tokens))

  ;; Translate content parts
  (define content
    (for/list ([part (in-list parts)])
      (cond
        [(hash-has-key? part 'text) (hasheq 'type "text" 'text (hash-ref part 'text ""))]
        [(hash-has-key? part 'functionCall)
         (let* ([fc (hash-ref part 'functionCall)]
                [tool-id (gemini-gen-tool-id)])
           (hasheq 'type
                   "tool-call"
                   'id
                   tool-id
                   'name
                   (hash-ref fc 'name "")
                   'arguments
                   (hash-ref fc 'args (hasheq))))]
        [else part])))

  ;; Check for safety/recitation filtering — add warning when content is empty
  (define filtered-reason
    (and candidate
         (let ([fr (hash-ref candidate 'finishReason "")])
           (cond
             [(equal? fr "SAFETY") "Content filtered for safety"]
             [(equal? fr "RECITATION") "Content filtered for recitation"]
             [else #f]))))
  (define final-content
    (if (and filtered-reason (null? content))
        (list (hasheq 'type "text" 'text (format "[SYS] ⚠ ~a" filtered-reason)))
        content))

  (make-model-response final-content usage model-version stop-reason))

;; Translate Gemini finish reasons to normalized symbols.
(define (gemini-translate-stop-reason reason)
  (cond
    [(string? reason)
     (let ([r (string-trim reason)])
       (cond
         [(equal? r "STOP") 'stop]
         [(equal? r "MAX_TOKENS") 'length]
         [(equal? r "SAFETY") 'content-filtered]
         [(equal? r "RECITATION") 'content-filtered]
         [else (string->symbol r)]))]
    [(symbol? reason) reason]
    [else 'stop]))

;; ============================================================
;; Stream chunk parsing
;; ============================================================

;; Parse Gemini SSE events into canonical stream-chunk structs.
;; Gemini streaming: each SSE data line is a JSON object similar to
;; the non-streaming response, with parts accumulated across chunks.
;; Delegates to gemini-parse-single-event for each event.
(define (gemini-parse-stream-chunks raw-events)
  (define results '())
  (for ([event (in-list raw-events)])
    (set! results (append results (gemini-parse-single-event event))))
  results)

;; ============================================================
;; Per-event stream parsing (for incremental generator)
;; ============================================================

;; Parse a single Gemini SSE event into a list of stream-chunks.
(define (gemini-parse-single-event event)
  (define results '())
  (define candidates (hash-ref event 'candidates '()))
  (define usage-raw (hash-ref event 'usageMetadata #f))
  (define candidate
    (if (null? candidates)
        #f
        (car candidates)))
  (define content-obj
    (if candidate
        (hash-ref candidate 'content #f)
        #f))
  (define parts
    (if content-obj
        (hash-ref content-obj 'parts '())
        '()))
  (define finish-reason
    (if candidate
        (hash-ref candidate 'finishReason #f)
        #f))

  ;; Emit text/tool deltas from parts
  ;; Tool calls use a running index from gemini-tool-id-counter-param
  ;; so that multiple functionCalls across SSE chunks get distinct indices.
  ;; The counter starts at 0 and is incremented by gemini-gen-tool-id.
  (for ([part (in-list parts)])
    (cond
      [(hash-has-key? part 'text)
       (let* ([text (hash-ref part 'text "")])
         (when (and (string? text) (> (string-length text) 0))
           (set! results (cons (make-stream-chunk text #f #f #f) results))))]
      [(hash-has-key? part 'functionCall)
       (let* ([fc (hash-ref part 'functionCall)]
              [tc-id (gemini-gen-tool-id)]
              [tc-index (sub1 (gemini-tool-id-counter-param))]
              [tc-delta
               (hasheq
                'index
                tc-index
                'id
                tc-id
                'function
                (hasheq 'name (hash-ref fc 'name "") 'arguments (hash-ref fc 'args (hasheq))))])
         (set! results (cons (make-stream-chunk #f tc-delta #f #f) results)))]
      [else (void)]))

  ;; Emit done chunk on finish reason or usage in last event
  (cond
    [(and finish-reason (not (eq? finish-reason 'null)) (not (null? finish-reason)))
     (let* ([usage (if usage-raw
                       (hasheq 'prompt_tokens
                               (hash-ref usage-raw 'promptTokenCount 0)
                               'completion_tokens
                               (hash-ref usage-raw 'candidatesTokenCount 0)
                               'total_tokens
                               (hash-ref usage-raw 'totalTokenCount 0))
                       (hasheq))])
       (set! results (cons (make-stream-chunk #f #f usage #t) results)))]
    [(and usage-raw (not finish-reason))
     ;; Usage-only event without finish — emit usage chunk
     (let* ([prompt-tokens (hash-ref usage-raw 'promptTokenCount 0)])
       (when (> prompt-tokens 0)
         (set! results
               (cons (make-stream-chunk #f #f (hasheq 'prompt_tokens prompt-tokens) #f) results))))])

  (reverse results))

;; ============================================================
;; HTTP status check (exported for tests)
;; ============================================================

(define (gemini-check-http-status! status-line response-body)
  (define status-code (extract-status-code status-line))
  (when (http-error? status-code)
    (define error-body
      (if (bytes? response-body)
          (bytes->string/utf-8 response-body)
          response-body))
    (cond
      [(= status-code 400)
       (raise-http-error! (format "Gemini API bad request (400): ~a" error-body) status-code)]
      [(= status-code 401)
       (raise-http-error! (format "Gemini API authentication failed (401): ~a" error-body)
                          status-code)]
      [(= status-code 403)
       (raise-http-error! (format "Gemini API forbidden (403): ~a" error-body) status-code)]
      [(= status-code 429)
       (raise-http-error! (format "Gemini API rate limited (429). Please wait and try again.\n~a"
                                  error-body)
                          status-code)]
      [(>= status-code 500)
       (raise-http-error! (format "Gemini API server error (~a): ~a" status-code error-body)
                          status-code)]
      [else
       (raise-http-error! (format "Gemini API error (~a): ~a" status-code error-body) status-code)])))

;; ============================================================
;; HTTP request execution (non-streaming)
;; ============================================================

(define (gemini-do-http-request base-url api-key model body)
  (define url-str
    (string-append (string-trim base-url "/") "/v1beta/models/" model ":generateContent"))
  (define headers (list "Content-Type: application/json" (format "x-goog-api-key: ~a" api-key)))
  (make-provider-http-request url-str
                              headers
                              (jsexpr->bytes body)
                              #:status-checker gemini-check-http-status!))

;; ============================================================
;; Provider constructor
;; ============================================================

(define (make-gemini-provider config)
  (validate-api-key! "Gemini" "GEMINI_API_KEY" config)
  (define base-url (hash-ref config 'base-url GEMINI-DEFAULT-BASE-URL))
  (define api-key (hash-ref config 'api-key ""))
  (define default-model (hash-ref config 'model GEMINI-DEFAULT-MODEL))

  (define (send req)
    (define merged-req (ensure-model-setting req default-model))
    (define body (gemini-build-request-body merged-req))
    (define model-name (hash-ref (model-request-settings merged-req) 'model default-model))
    ;; Bind per-request counter (Issue #200)
    (parameterize ([gemini-tool-id-counter-param 0])
      (define raw (gemini-do-http-request base-url api-key model-name body))
      (gemini-parse-response raw)))

  ;; W10.1 (Q-19): dynamic-wind ensures response port cleanup on timeout/exception
  (define (stream req)
    (define merged-req (ensure-model-setting req default-model))
    (define body (gemini-build-request-body merged-req #:stream? #t))
    (define model-name (hash-ref (model-request-settings merged-req) 'model default-model))
    (define url-str
      (string-append (string-trim base-url "/")
                     "/v1beta/models/"
                     model-name
                     ":streamGenerateContent"
                     "?alt=sse"))
    (define uri (string->url url-str))
    (define headers (list "Content-Type: application/json" (format "x-goog-api-key: ~a" api-key)))
    (define body-bytes (jsexpr->bytes body))
    (define response-port-box (box #f))
    (dynamic-wind
     (lambda () (void))
     (lambda ()
       ;; Wrap initial HTTP request in overall timeout (SEC-11)
       (define result-vec
         (call-with-request-timeout #:cleanup (lambda ()
                                                (define rp (unbox response-port-box))
                                                (when rp
                                                  (with-handlers ([exn:fail? void])
                                                    (close-input-port rp))))
                                    (lambda ()
                                      (define-values (sl rh rp)
                                        (http-sendrecv uri 'POST #:headers headers #:data body-bytes))
                                      (set-box! response-port-box rp)
                                      (vector sl rh rp))))
       (define status-line (vector-ref result-vec 0))
       (define response-headers (vector-ref result-vec 1))
       (define response-port (vector-ref result-vec 2))
       ;; Check HTTP status first
       (define status-str
         (if (bytes? status-line)
             (bytes->string/utf-8 status-line)
             status-line))
       (define status-code
         (let ([parts (regexp-match #rx"^HTTP/[^ ]+ ([0-9]+)" status-str)])
           (if parts
               (string->number (cadr parts))
               0)))
       (when (>= status-code 400)
         (define resp-body (read-response-body/timeout response-port))
         (gemini-check-http-status! status-line resp-body))
       ;; Bind per-request counter (Issue #200)
       (parameterize ([gemini-tool-id-counter-param 0])
         ;; Incremental SSE parsing — generator yields chunks one at a time
         (define raw-port response-port)
         (generator ()
                    (let loop ([first-read? #t])
                      (define timeout-secs
                        (if first-read? http-read-timeout-default http-stream-timeout-default))
                      (define line (read-line/timeout raw-port #:timeout timeout-secs))
                      (cond
                        [(or (eq? line #f) (eof-object? line))
                         (close-input-port raw-port)
                         (yield #f)]
                        [else
                         (define parsed (parse-sse-line line))
                         (cond
                           [(eq? parsed 'done)
                            (close-input-port raw-port)
                            (yield #f)]
                           [(hash? parsed)
                            (define chunks (gemini-parse-single-event parsed))
                            (for ([ch (in-list chunks)])
                              (yield ch))
                            (loop #f)]
                           [else (loop #f)])])))))
     (lambda ()
       (define rp (unbox response-port-box))
       (when rp
         (with-handlers ([exn:fail? void])
           (close-input-port rp))))))

  (make-provider (lambda () "gemini")
                 (lambda () (hasheq 'streaming #t 'token-counting #f))
                 send
                 stream))
