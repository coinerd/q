#lang racket/base

;; llm/anthropic/sse.rkt — Anthropic HTTP transport + provider constructor
;;
;; v0.99.43 W0: Extracted from the monolithic llm/anthropic.rkt (571 lines).
;; This sub-module owns all I/O for the Anthropic provider:
;;   - HTTP request execution (non-streaming + streaming SSE)
;;   - Kimi eager-stream adapter wiring
;;   - make-anthropic-provider (the public provider constructor)
;; Request/response/SSE formatting stays pure in llm/anthropic/format.rkt.

(require racket/string
         racket/port
         json
         net/url
         net/http-client
         (only-in "../timing.rkt" log-stream-setup-timing)
         (only-in "../../util/error/error-helpers.rkt" with-logged-error)
         (only-in "../model-defaults.rkt" ANTHROPIC-DEFAULT-MODEL ANTHROPIC-DEFAULT-BASE-URL)
         (only-in "../provider.rkt" make-provider validate-api-key! ensure-model-setting)
         (only-in "../stream.rkt"
                  close-port-after-stream
                  stream-sse-events
                  call-with-request-timeout
                  effective-sse-read-timeout-for
                  read-response-body/timeout)
         (only-in "../http-helpers.rkt"
                  make-provider-http-request
                  check-provider-status!
                  parse-provider-url)
         (only-in "../model-defaults.rkt" ANTHROPIC-DEFAULT-MODEL ANTHROPIC-DEFAULT-BASE-URL)
         (only-in "../model.rkt" model-request-settings)
         (only-in "../provider.rkt" make-provider validate-api-key! ensure-model-setting)
         (only-in "format.rkt"
                  anthropic-provider-name
                  ANTHROPIC-VERSION
                  anthropic-build-request-body
                  anthropic-parse-response
                  anthropic-parse-single-event)
         (only-in "../adapters/eager-stream.rkt" eager-stream))

(provide anthropic-do-http-request
         kimi-eager-stream-chunks
         make-anthropic-provider)

;; ============================================================
;; HTTP status check (exported for tests)
;; ============================================================
;; HTTP request execution (non-streaming)
;; ============================================================

(define (anthropic-do-http-request base-url
                                   api-key
                                   path
                                   body
                                   [provider-name "anthropic"]
                                   #:model-name [model-name #f])
  (define url-str (string-append (string-trim base-url "/") path))
  (define headers
    (list* (format "x-api-key: ~a" api-key)
           (format "anthropic-version: ~a" ANTHROPIC-VERSION)
           "Content-Type: application/json"
           (if (equal? provider-name "kimi-coding")
               (list "User-Agent: KimiCLI/1.5")
               '())))
  ;; v1.00.05 W1 (#9393): honor the per-model sse-read timeout for the body
  ;; read instead of the hardcoded 120s fallback. effective-sse-read-timeout-for
  ;; returns #f when the model has no override — make-provider-http-request then
  ;; falls back to http-read-timeout-default.
  (define read-timeout (and model-name (effective-sse-read-timeout-for model-name)))
  (make-provider-http-request url-str
                              headers
                              (jsexpr->bytes body)
                              #:read-timeout read-timeout
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
  (define model-name (hash-ref (model-request-settings merged-req) 'model default-model))
  (define (kimi-completion-fn _req)
    (anthropic-do-http-request base-url
                               api-key
                               "/v1/messages"
                               body
                               provider-name
                               #:model-name model-name))
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
    (define model-name (hash-ref (model-request-settings merged-req) 'model default-model))
    (define raw
      (anthropic-do-http-request base-url
                                 api-key
                                 "/v1/messages"
                                 body
                                 provider-name
                                 #:model-name model-name))
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
          (define request-custodian (make-custodian))
          (define (cleanup-response!)
            (custodian-shutdown-all request-custodian))
          (define stream-owns-port? (box #f))
          (dynamic-wind
           (lambda () (void))
           (lambda ()
             ;; Wrap initial HTTP request in overall timeout (SEC-11)
             (define result-vec
               (call-with-request-timeout #:cleanup cleanup-response!
                                          (lambda ()
                                            (parameterize ([current-custodian request-custodian])
                                              (define-values (sl rh rp)
                                                (http-sendrecv host
                                                               path-str
                                                               #:port port
                                                               #:ssl? ssl?
                                                               #:method #"POST"
                                                               #:headers headers
                                                               #:data body-bytes))
                                              (vector sl rh rp)))))
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
             (define owned-stream
               (close-port-after-stream
                (stream-sse-events raw-port
                                   (lambda (parsed)
                                     (anthropic-parse-single-event parsed
                                                                   current-tool-id
                                                                   current-tool-name
                                                                   current-tool-index)))
                raw-port
                #:cleanup cleanup-response!))
             ;; Transfer only after finalizer registration succeeds.
             (set-box! stream-owns-port? #t)
             owned-stream)
           (lambda ()
             (unless (unbox stream-owns-port?)
               (with-logged-error "request cleanup" (cleanup-response!))))))))

  (make-provider (lambda () (anthropic-provider-name config))
                 (lambda () (hasheq 'streaming #t 'token-counting #f))
                 send
                 stream))
