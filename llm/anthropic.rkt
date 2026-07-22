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
         racket/set
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
         "provider-telemetry.rkt"
         ;; W8 v0.99.35: Pure helpers extracted from this module
         "anthropic-helpers.rkt"
         "adapters/eager-stream.rkt")

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

;; W8 v0.99.35: openai-block->anthropic extracted to anthropic-helpers.rkt

;; Convert normalized model-request to Anthropic Messages API body.
(define (anthropic-build-request-body req #:stream? [stream? #f])
  (define settings (model-request-settings req))
  (define model-name (hash-ref settings 'model ANTHROPIC-DEFAULT-MODEL))
  (define max-tokens (hash-ref settings 'max-tokens ANTHROPIC-DEFAULT-MAX-TOKENS))

  ;; Anthropic requires system instructions at the top level, never as a
  ;; role inside messages. Canonical transport may carry them as messages.
  (define raw-messages (model-request-messages req))
  (define system-text
    (string-join (for/list ([msg (in-list raw-messages)]
                            #:when (equal? (hash-ref msg 'role #f) "system"))
                   (format "~a" (hash-ref msg 'content "")))
                 "\n\n"))
  (define conversation-messages
    (filter (lambda (msg) (not (equal? (hash-ref msg 'role #f) "system"))) raw-messages))
  (define messages
    (for/list ([msg (in-list conversation-messages)])
      (define role (hash-ref msg 'role "user"))
      (define content (hash-ref msg 'content ""))
      (cond
        ;; System role: pass through unchanged
        [(equal? role "system") msg]
        ;; Tool role: convert to Anthropic user+tool_result format
        [(equal? role "tool")
         ;; OpenAI-format tool messages have:
         ;;   tool_call_id at message level, content as string
         ;; Anthropic needs user+tool_result format with tool_use_id
         (define raw-tcid (hash-ref msg 'tool_call_id #f))
         ;; Safety: never send blank/missing tool_call_id to Anthropic API
         (unless (and (string? raw-tcid) (> (string-length raw-tcid) 0))
           (log-warning "ANTHROPIC: tool message has blank/missing tool_call_id: ~v" msg))
         (define tool-call-id
           (if (and (string? raw-tcid) (> (string-length raw-tcid) 0))
               raw-tcid
               (format "tc_fallback_~a" (gensym))))
         (define tool-result-content
           (if (string? content)
               content
               (format "~a" content)))
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
        ;; Assistant with tool_calls at message level (OpenAI format)
        [(and (equal? role "assistant") (hash-ref msg 'tool_calls #f))
         (define tc-list (hash-ref msg 'tool_calls '()))
         (define text-blocks
           (if (and (string? content) (not (string=? content "")))
               (list (hasheq 'type "text" 'text content))
               '()))
         (define tool-blocks
           (for/list ([tc (in-list tc-list)])
             (define fn (hash-ref tc 'function (hasheq)))
             (hasheq 'type
                     "tool_use"
                     'id
                     (hash-ref tc 'id "")
                     'name
                     (hash-ref fn 'name "")
                     'input
                     (let ([args (hash-ref fn 'arguments "")])
                       (if (string? args)
                           (with-handlers ([exn:fail? (lambda (e) (hasheq))])
                             (string->jsexpr args))
                           args)))))
         (hasheq 'role "assistant" 'content (append text-blocks tool-blocks))]
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

  ;; Anthropic requires all tool_results from the same assistant turn to be
  ;; in a SINGLE user message. OpenAI format has separate tool messages.
  ;; Merge consecutive user messages containing tool_result blocks.
  (define merged-messages
    (let loop ([msgs messages]
               [acc '()])
      (cond
        [(null? msgs) (reverse acc)]
        [else
         (define cur (car msgs))
         (define cur-role (hash-ref cur 'role #f))
         (define cur-content (hash-ref cur 'content '()))
         ;; Check if this is a user message with tool_result blocks
         (define (has-tool-results? msg)
           (and (equal? (hash-ref msg 'role #f) "user")
                (list? (hash-ref msg 'content '()))
                (not (null? (hash-ref msg 'content '())))
                (andmap (lambda (b) (equal? (hash-ref b 'type #f) "tool_result"))
                        (hash-ref msg 'content '()))))
         (if (has-tool-results? cur)
             ;; Collect consecutive tool-result user messages
             (let gather ([remaining (cdr msgs)]
                          [blocks (hash-ref cur 'content)])
               (cond
                 [(null? remaining) (loop '() (cons (hasheq 'role "user" 'content blocks) acc))]
                 [(has-tool-results? (car remaining))
                  (gather (cdr remaining) (append blocks (hash-ref (car remaining) 'content '())))]
                 [else (loop remaining (cons (hasheq 'role "user" 'content blocks) acc))]))
             (loop (cdr msgs) (cons cur acc)))])))

  ;; Filter orphaned tool_results: context assembly may drop assistant tool_use
  ;; messages while keeping tool results. These orphaned tool_results cause
  ;; Anthropic API 400 errors. Filter them out here.
  (define filtered-messages
    (let loop ([msgs merged-messages]
               [seen-use-ids (set)]
               [acc '()])
      (if (null? msgs)
          (reverse acc)
          (let* ([m (car msgs)]
                 [m-role (hash-ref m 'role #f)]
                 [m-content (hash-ref m 'content #f)])
            (cond
              [(equal? m-role "assistant")
               (define new-ids
                 (if (list? m-content)
                     (for/fold ([s seen-use-ids]) ([block (in-list m-content)])
                       (if (equal? (hash-ref block 'type #f) "tool_use")
                           (set-add s (hash-ref block 'id #f))
                           s))
                     seen-use-ids))
               (loop (cdr msgs) new-ids (cons m acc))]
              [(and (equal? m-role "user") (list? m-content))
               ;; Filter tool_result blocks, keep only those with matching tool_use
               (define filtered-content
                 (for/list ([block (in-list m-content)])
                   (cond
                     [(equal? (hash-ref block 'type #f) "tool_result")
                      (define tuid (hash-ref block 'tool_use_id #f))
                      (cond
                        [(not (and (string? tuid) (> (string-length tuid) 0)))
                         (log-warning "ANTHROPIC: dropping tool_result with blank tool_use_id")
                         #f]
                        [(not (set-member? seen-use-ids tuid))
                         (log-warning "ANTHROPIC: dropping orphaned tool_result tuid=~a" tuid)
                         #f]
                        [else block])]
                     [else block])))
               (define kept (filter (lambda (x) x) filtered-content))
               (if (null? kept)
                   (loop (cdr msgs) seen-use-ids acc) ; drop empty user message
                   (loop (cdr msgs) seen-use-ids (cons (hash-set m 'content kept) acc)))]
              [else (loop (cdr msgs) seen-use-ids (cons m acc))])))))

  (define base
    (hasheq 'model model-name 'max_tokens max-tokens 'messages filtered-messages 'stream stream?))

  ;; Add optional temperature
  (define with-temp
    (if (hash-has-key? settings 'temperature)
        (hash-set base 'temperature (hash-ref settings 'temperature))
        base))

  ;; Explicit request settings take precedence; otherwise preserve canonical
  ;; system messages at Anthropic's native top-level boundary.
  (define effective-system
    (if (hash-has-key? settings 'system)
        (hash-ref settings 'system)
        system-text))
  (define with-system
    (if (and (string? effective-system) (not (string=? effective-system "")))
        (hash-set with-temp 'system effective-system)
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

;; W8 v0.99.35: anthropic-translate-tool extracted to anthropic-helpers.rkt

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

  ;; W8 v0.99.35: Usage translation extracted to anthropic-helpers.rkt
  (define usage (translate-anthropic-usage usage-raw))

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
                   (let ([raw-id (hash-ref block 'id #f)])
                     (if (and (string? raw-id) (> (string-length raw-id) 0))
                         raw-id
                         (format "tc_~a" (gensym))))
                   'name
                   (hash-ref block 'name "")
                   'arguments
                   (hash-ref block 'input (hasheq))))
         ;; Shadow: validate round-trip through tool-call-intent
         (validate-tool-call-intent! tc-hash "anthropic")
         tc-hash]
        [_ block])))

  (define native-id (hash-ref raw 'id #f))
  (make-model-response content
                       usage
                       model-name
                       stop-reason
                       #:provenance (response-native-identity #:adapter "anthropic"
                                                              #:native-response-id native-id
                                                              #:native-model model-name)))

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
;; Kimi eager-stream helper: uses the generic eager-stream adapter
;; from llm/adapters/eager-stream.rkt instead of inline code.
;; ============================================================

(define (kimi-eager-stream-chunks req base-url api-key provider-name default-model)
  (define merged-req (ensure-model-setting req default-model))
  (define body (anthropic-build-request-body merged-req #:stream? #f))
  (define (kimi-completion-fn _req)
    (anthropic-do-http-request base-url api-key "/v1/messages" body provider-name))
  (eager-stream kimi-completion-fn merged-req #:parse-response anthropic-parse-response))

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
          (define-values (host path-str port ssl?) (parse-provider-url url-str))
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
