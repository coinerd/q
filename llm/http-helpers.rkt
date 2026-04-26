#lang racket/base

;; llm/http-helpers.rkt — Shared HTTP utilities for LLM providers
;;
;; Common helpers for:
;;   - HTTP request execution (make-provider-http-request)
;;   - Status checking with provider-specific error messages
;;   - Parsing HTTP status lines and raising structured errors
;;
;; Used by anthropic.rkt, gemini.rkt, openai-compatible.rkt, azure-openai.rkt.

(require racket/contract
         racket/string
         racket/port
         json
         net/url
         net/http-client
         "provider-errors.rkt"
         "stream.rkt")

(provide extract-status-code
         http-error?
         raise-http-error!
         ;; Consolidated HTTP helpers (QUAL-02)
         make-provider-http-request
         check-provider-status!)

;; ============================================================
;; Contracts
;; ============================================================

(define status-line/c (or/c bytes? string?))

(define response-body/c (or/c bytes? string?))

;; ============================================================
;; Existing helpers
;; ============================================================

;; Parse "HTTP/N.N NNN ..." → integer status code.
;; Returns 0 if the pattern cannot be matched.
(define (extract-status-code status-line)
  (define status-str
    (if (bytes? status-line)
        (bytes->string/utf-8 status-line)
        status-line))
  (define m (regexp-match #rx"^HTTP/[^ ]+ ([0-9]+)" status-str))
  (if m
      (string->number (cadr m))
      0))

;; Returns #t when status-code indicates a client or server error (>= 400).
(define (http-error? status-code)
  (>= status-code 400))

;; Raise provider-error with category inferred from HTTP status code.
;; Falls back to exn:fail if no status code is provided.
(define (raise-http-error! message [status-code #f])
  (define category (classify-http-status status-code))
  (if category
      (raise (provider-error message (current-continuation-marks) status-code category))
      (raise (exn:fail message (current-continuation-marks)))))

;; ============================================================
;; Consolidated HTTP request (QUAL-02)
;; ============================================================

;; make-provider-http-request : string? (listof string?) bytes?
;;   [#:timeout exact-positive-integer?]
;;   [#:status-checker (bytes? bytes? -> void?)]
;;   -> jsexpr?
;;
;; Shared HTTP POST helper for LLM providers.
;; Handles URL parsing, http-sendrecv, body reading, status checking, and JSON parsing.
;; Providers supply their own headers and optional status-checker callback.
;;
;; Example:
;;   (make-provider-http-request
;;     "https://api.openai.com/v1/chat/completions"
;;     (list "Authorization: Bearer ..." "Content-Type: application/json")
;;     (jsexpr->bytes body)
;;     #:status-checker check-http-status!)
(define (make-provider-http-request url-str
                                    headers
                                    body-bytes
                                    #:timeout [timeout-secs #f]
                                    #:status-checker [status-checker #f])
  (define uri (string->url url-str))
  (define effective-timeout (or timeout-secs (current-http-request-timeout)))
  (call-with-request-timeout (lambda ()
                               (define-values (status-line response-headers response-port)
                                 (http-sendrecv uri 'POST #:headers headers #:data body-bytes))
                               (define response-body (read-response-body/timeout response-port))
                               (when status-checker
                                 (status-checker status-line response-body))
                               (bytes->jsexpr response-body))
                             #:timeout effective-timeout))

;; ============================================================
;; Consolidated status checker (QUAL-02)
;; ============================================================

;; check-provider-status! : string? bytes? bytes? -> void?
;;
;; Generic HTTP status checker with provider name for error messages.
;; Handles common status codes (301-303 redirects, 401, 403, 429, 5xx)
;; with provider-specific messages.
;;
;; Example:
;;   (check-provider-status! "Anthropic" status-line response-body)
(define (check-provider-status! provider-name status-line response-body)
  (define status-code (extract-status-code status-line))
  (define response-bytes
    (if (bytes? response-body)
        response-body
        (string->bytes/utf-8 response-body)))
  (define status-str
    (if (bytes? status-line)
        (bytes->string/utf-8 status-line)
        status-line))
  (cond
    ;; Redirects
    [(and (>= status-code 300) (< status-code 400))
     (raise-http-error!
      (format "~a API request redirected (~a: ~a). Check your base-url in config.json."
              provider-name
              status-code
              status-str))]
    ;; Authentication
    [(= status-code 401)
     (raise-http-error! (format "~a API authentication failed (401)" provider-name) status-code)]
    ;; Forbidden
    [(= status-code 403)
     (raise-http-error! (format "~a API forbidden (403)" provider-name) status-code)]
    ;; Rate limited
    [(= status-code 429)
     (define error-text (safe-extract-error-text response-bytes))
     (raise-http-error!
      (format "~a API rate limited (429). Please wait and try again.\n~a" provider-name error-text)
      status-code)]
    ;; Bad request
    [(= status-code 400)
     (define error-text (safe-extract-error-text response-bytes))
     (raise-http-error! (format "~a API bad request (400): ~a" provider-name error-text) status-code)]
    ;; Server errors
    [(>= status-code 500)
     (define error-text (safe-extract-error-text response-bytes))
     (raise-http-error! (format "~a API server error (~a): ~a" provider-name status-code error-text)
                        status-code)]
    ;; Other client errors
    [(http-error? status-code)
     (define error-text (safe-extract-error-text response-bytes))
     (raise-http-error! (format "~a API error (~a): ~a" provider-name status-code error-text)
                        status-code)]))

;; Safe extraction of error text from response body bytes.
;; Returns a readable string or a fallback message.
(define (safe-extract-error-text response-bytes)
  (with-handlers ([exn:fail? (lambda (_)
                               (format "<binary body ~a bytes>" (bytes-length response-bytes)))])
    (define jsexpr (bytes->jsexpr response-bytes))
    (or (extract-error-message jsexpr) (format "~a" jsexpr))))

;; Extract a readable error message from a JSON error response.
(define (extract-error-message jsexpr)
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
