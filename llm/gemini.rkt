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
         racket/port
         racket/format
         json
         net/url
         net/http-client
         "model.rkt"
         "provider.rkt"
         "stream.rkt")

(provide
 make-gemini-provider
 gemini-build-request-body
 gemini-parse-response
 gemini-parse-stream-chunks
 ;; Internal helpers for testing
 gemini-translate-tool
 gemini-translate-stop-reason
 gemini-check-http-status!
 gemini-gen-tool-id
 gemini-reset-tool-id-counter!)

;; ============================================================
;; Constants
;; ============================================================

(define GEMINI-DEFAULT-MODEL "gemini-2.5-pro")
(define GEMINI-DEFAULT-MAX-TOKENS 4096)

;; Counter-based tool call ID generator (Issue #110)
(define gemini-tool-id-counter 0)

(define (gemini-gen-tool-id)
  (set! gemini-tool-id-counter (add1 gemini-tool-id-counter))
  (format "gemini_~a" gemini-tool-id-counter))

(define (gemini-reset-tool-id-counter!)
  (set! gemini-tool-id-counter 0))
(define GEMINI-DEFAULT-BASE-URL "https://generativelanguage.googleapis.com")

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
                                  (if (list? c) c '())))])
      (if (and (hash? block)
               (equal? (hash-ref block 'type #f) "tool-call"))
          (hash-set acc
                    (hash-ref block 'id "")
                    (hash-ref block 'name ""))
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
           (list (hash 'functionResponse
                       (hash 'name tool-name
                             'response (hash 'content tool-result-content))))]
          ;; Assistant with list content (tool calls + text)
          [(and (equal? role "assistant") (list? content))
           (for/list ([block (in-list content)])
             (define btype (hash-ref block 'type "text"))
             (cond
               [(equal? btype "text")
                (hash 'text (hash-ref block 'text ""))]
               [(equal? btype "tool-call")
                (hash 'functionCall
                      (hash 'name (hash-ref block 'name "")
                            'args (hash-ref block 'arguments (hash))))]
               [else block]))]
          ;; Simple string content: text part
          [(string? content)
           (list (hash 'text content))]
          ;; Fallback
          [else content]))
      (hash 'role gemini-role
            'parts parts)))

  ;; Build generationConfig
  (define gen-config
    (let ([base-config (hash 'maxOutputTokens max-tokens)])
      (if (hash-has-key? settings 'temperature)
          (hash-set base-config 'temperature (hash-ref settings 'temperature))
          base-config)))

  ;; Base body
  (define base
    (hash 'contents contents
          'generationConfig gen-config))

  ;; Add optional system prompt — goes to systemInstruction, not contents
  (define with-system
    (if (hash-has-key? settings 'system)
        (hash-set base
                  'systemInstruction
                  (hash 'parts (list (hash 'text (hash-ref settings 'system)))))
        base))

  ;; Translate tools to Gemini format if present
  (define with-tools
    (if (model-request-tools req)
        (hash-set with-system
                  'tools
                  (list (hash
                         'functionDeclarations
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
  (hash 'name name
        'description description
        'parameters parameters))

;; ============================================================
;; Response parsing
;; ============================================================

;; Convert Gemini generateContent API response to model-response.
(define (gemini-parse-response raw)
  (define candidates (hash-ref raw 'candidates '()))
  (define usage-raw (hash-ref raw 'usageMetadata (hash)))
  (define model-version (hash-ref raw 'modelVersion GEMINI-DEFAULT-MODEL))

  ;; Extract first candidate
  (define candidate (if (null? candidates) #f (car candidates)))
  (define content-obj (if candidate (hash-ref candidate 'content #f) #f))
  (define parts (if content-obj (hash-ref content-obj 'parts '()) '()))
  (define finish-reason (if candidate (hash-ref candidate 'finishReason "STOP") "STOP"))

  ;; Translate stop reason
  (define stop-reason (gemini-translate-stop-reason finish-reason))

  ;; Translate usage
  (define prompt-tokens (hash-ref usage-raw 'promptTokenCount 0))
  (define candidates-tokens (hash-ref usage-raw 'candidatesTokenCount 0))
  (define total-tokens (hash-ref usage-raw 'totalTokenCount
                                  (+ prompt-tokens candidates-tokens)))
  (define usage
    (hash 'prompt_tokens prompt-tokens
          'completion_tokens candidates-tokens
          'total_tokens total-tokens))

  ;; Translate content parts
  (define content
    (for/list ([part (in-list parts)])
      (cond
        [(hash-has-key? part 'text)
         (hash 'type "text" 'text (hash-ref part 'text ""))]
        [(hash-has-key? part 'functionCall)
         (let* ([fc (hash-ref part 'functionCall)]
                [tool-id (gemini-gen-tool-id)])
           (hash 'type "tool-call"
                 'id tool-id
                 'name (hash-ref fc 'name "")
                 'arguments (hash-ref fc 'args (hash))))]
        [else
         part])))

  (make-model-response content usage model-version stop-reason))

;; Translate Gemini finish reasons to normalized symbols.
(define (gemini-translate-stop-reason reason)
  (cond
    [(string? reason)
     (let ([r (string-trim reason)])
       (cond
         [(equal? r "STOP") 'stop]
         [(equal? r "MAX_TOKENS") 'length]
         [(equal? r "SAFETY") 'stop]
         [(equal? r "RECITATION") 'stop]
         [else (string->symbol r)]))]
    [(symbol? reason) reason]
    [else 'stop]))

;; ============================================================
;; Stream chunk parsing
;; ============================================================

;; Parse Gemini SSE events into canonical stream-chunk structs.
;; Gemini streaming: each SSE data line is a JSON object similar to
;; the non-streaming response, with parts accumulated across chunks.
(define (gemini-parse-stream-chunks raw-events)
  (define results '())

  (for ([event (in-list raw-events)])
    (define candidates (hash-ref event 'candidates '()))
    (define usage-raw (hash-ref event 'usageMetadata #f))
    (define candidate (if (null? candidates) #f (car candidates)))
    (define content-obj (if candidate (hash-ref candidate 'content #f) #f))
    (define parts (if content-obj (hash-ref content-obj 'parts '()) '()))
    (define finish-reason (if candidate (hash-ref candidate 'finishReason #f) #f))

    ;; Emit text/tool deltas from parts
    (for ([part (in-list parts)])
      (cond
        [(hash-has-key? part 'text)
         (let* ([text (hash-ref part 'text "")])
           (when (and (string? text) (> (string-length text) 0))
             (set! results
                   (cons (stream-chunk text #f #f #f)
                         results))))]
        [(hash-has-key? part 'functionCall)
         (let* ([fc (hash-ref part 'functionCall)]
                [tc-delta (hash 'index 0
                                'id (gemini-gen-tool-id)
                                'function (hash 'name (hash-ref fc 'name "")
                                                'arguments (hash-ref fc 'args (hash))))])
           (set! results
                 (cons (stream-chunk #f tc-delta #f #f)
                       results)))]
        [else (void)]))

    ;; Emit done chunk on finish reason or usage in last event
    (cond
      [(and finish-reason
               (not (eq? finish-reason 'null))
               (not (null? finish-reason)))
       (let* ([usage (if usage-raw
                         (hash 'prompt_tokens (hash-ref usage-raw 'promptTokenCount 0)
                               'completion_tokens (hash-ref usage-raw 'candidatesTokenCount 0)
                               'total_tokens (hash-ref usage-raw 'totalTokenCount 0))
                         (hash))])
         (set! results
               (cons (stream-chunk #f #f usage #t)
                     results)))]
      [(and usage-raw (not finish-reason))
       ;; Usage-only event without finish — emit usage chunk
       (let* ([prompt-tokens (hash-ref usage-raw 'promptTokenCount 0)])
         (when (> prompt-tokens 0)
           (set! results
                 (cons (stream-chunk #f #f (hash 'prompt_tokens prompt-tokens) #f)
                       results))))]))

  (reverse results))

;; ============================================================
;; HTTP status check (exported for tests)
;; ============================================================

(define (gemini-check-http-status! status-line response-body)
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
      [(= status-code 400)
       (raise (exn:fail (format "Gemini API bad request (400): ~a"
                                error-body)
                        (current-continuation-marks)))]
      [(= status-code 401)
       (raise (exn:fail (format "Gemini API authentication failed (401): ~a"
                                error-body)
                        (current-continuation-marks)))]
      [(= status-code 403)
       (raise (exn:fail (format "Gemini API forbidden (403): ~a"
                                error-body)
                        (current-continuation-marks)))]
      [(= status-code 429)
       (raise (exn:fail (format "Gemini API rate limited (429): ~a"
                                error-body)
                        (current-continuation-marks)))]
      [(>= status-code 500)
       (raise (exn:fail (format "Gemini API server error (~a): ~a"
                                status-code
                                error-body)
                        (current-continuation-marks)))]
      [else
       (raise (exn:fail (format "Gemini API error (~a): ~a"
                                status-code
                                error-body)
                        (current-continuation-marks)))])))

;; ============================================================
;; HTTP request execution (non-streaming)
;; ============================================================

(define (gemini-do-http-request base-url api-key model body)
  (define url-str
    (string-append
     (string-trim base-url "/")
     "/v1beta/models/" model ":generateContent"))
  (define uri (string->url url-str))
  (define headers
    (list "Content-Type: application/json"
          (format "x-goog-api-key: ~a" api-key)))
  (define body-bytes (jsexpr->bytes body))
  (define-values (status-line response-headers response-port)
    (http-sendrecv uri 'POST
                   #:headers headers
                   #:data body-bytes))
  (define response-body (read-response-body response-port))
  ;; Check HTTP status
  (gemini-check-http-status! status-line response-body)
  (bytes->jsexpr response-body))

;; ============================================================
;; Provider constructor
;; ============================================================

(define (make-gemini-provider config)
  (define base-url (hash-ref config 'base-url GEMINI-DEFAULT-BASE-URL))
  (define api-key (hash-ref config 'api-key ""))
  (define default-model (hash-ref config 'model GEMINI-DEFAULT-MODEL))

  (define (send req)
    (define merged-req
      (if (hash-has-key? (model-request-settings req) 'model)
          req
          (make-model-request
           (model-request-messages req)
           (model-request-tools req)
           (hash-set (model-request-settings req) 'model default-model))))
    (define body (gemini-build-request-body merged-req))
    (define model-name (hash-ref (model-request-settings merged-req) 'model default-model))
    (define raw (gemini-do-http-request base-url api-key model-name body))
    (gemini-parse-response raw))

  (define (stream req)
    (define merged-req
      (if (hash-has-key? (model-request-settings req) 'model)
          req
          (make-model-request
           (model-request-messages req)
           (model-request-tools req)
           (hash-set (model-request-settings req) 'model default-model))))
    (define body (gemini-build-request-body merged-req #:stream? #t))
    (define model-name (hash-ref (model-request-settings merged-req) 'model default-model))
    (define url-str
      (string-append
       (string-trim base-url "/")
       "/v1beta/models/" model-name ":streamGenerateContent"
       "?alt=sse"))
    (define uri (string->url url-str))
    (define headers
      (list "Content-Type: application/json"
            (format "x-goog-api-key: ~a" api-key)))
    (define body-bytes (jsexpr->bytes body))
    (define-values (status-line response-headers response-port)
      (http-sendrecv uri 'POST
                     #:headers headers
                     #:data body-bytes))
    ;; Check HTTP status first
    (define status-str (if (bytes? status-line)
                           (bytes->string/utf-8 status-line)
                           status-line))
    (define status-code
      (let ([parts (regexp-match #rx"^HTTP/[^ ]+ ([0-9]+)" status-str)])
        (if parts (string->number (cadr parts)) 0)))
    (when (>= status-code 400)
      (define resp-body (read-response-body/timeout response-port))
      (gemini-check-http-status! status-line resp-body))
    ;; Incremental SSE parsing (Issue #108)
    (define raw-events
      (if (>= status-code 400)
          '()
          (let loop ([acc '()])
            (define line (read-line/timeout response-port))
            (cond
              [(eq? line #f) (reverse acc)]  ; timeout — return what we have
              [(eof-object? line) (reverse acc)]
              [else
               (let ([parsed (parse-sse-line line)])
                 (if (hash? parsed)
                     (loop (cons parsed acc))
                     (loop acc)))]))))
    (gemini-parse-stream-chunks raw-events))

  (make-provider
   (lambda () "gemini")
   (lambda () (hash 'streaming #t 'token-counting #f))
   send
   stream))
