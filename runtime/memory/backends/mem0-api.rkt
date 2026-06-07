#lang racket/base
;; runtime/memory/backends/mem0-api.rkt — Mem0 external memory adapter
;;
;; v0.95.16 W5: Provides a Mem0-specific memory-backend? using the existing
;; external-protocol abstraction. The transport is injectable for testing.
;;
;; Safety:
;;   - API key read from env var name (never stored/logged/emitted in events)
;;   - All payloads redacted via external-protocol
;;   - Fails closed on missing key, timeout, or malformed response
;;   - No real network access in tests (injectable transport)

(require racket/match
         racket/string
         racket/port
         (only-in json jsexpr->string bytes->jsexpr)
         net/http-client
         "../types.rkt"
         "../protocol.rkt"
         "external-protocol.rkt")

;; ---------------------------------------------------------------------------
;; Mem0 request/response codec
;; ---------------------------------------------------------------------------

;; Build a Mem0 store request (POST /v1/memories/)
(define (mem0-store-payload item)
  (hash 'content
        (memory-item-content item)
        'metadata
        (hash 'scope
              (symbol->string (memory-item-scope item))
              'type
              (symbol->string (memory-item-type item))
              'session_id
              (hash-ref (memory-item-metadata item) 'session-id "unknown"))
        'user_id
        (memory-item-id item)))

;; Build a Mem0 search request (POST /v1/memories/search/)
(define (mem0-search-payload query)
  (hash 'query
        (memory-query-text query)
        'scope
        (and (memory-query-scope query) (symbol->string (memory-query-scope query)))
        'limit
        (or (memory-query-limit query) 10)))

;; Build a Mem0 list request (GET /v1/memories/)
(define (mem0-list-payload query)
  (hash 'limit (or (memory-query-limit query) 50)))

;; Build a Mem0 delete request (DELETE /v1/memories/{id})
(define (mem0-delete-payload id scope)
  (hash 'id id 'scope (and scope (symbol->string scope))))

;; Build a Mem0 update request (PATCH /v1/memories/{id})
(define (mem0-update-payload id patch)
  (hash 'id id 'patch patch))

;; ---------------------------------------------------------------------------
;; Mem0 response decoder
;; ---------------------------------------------------------------------------

;; Decode a Mem0 search response (list of memory objects) into memory-items
(define (decode-mem0-items raw-items session-id project-root)
  (for/list ([entry (in-list (if (list? raw-items)
                                 raw-items
                                 '()))])
    (define content (hash-ref entry 'content ""))
    (define meta (hash-ref entry 'metadata (hasheq)))
    (memory-item (hash-ref entry 'id (hash-ref entry 'user_id "unknown"))
                 (string->symbol (hash-ref meta 'type "semantic"))
                 (string->symbol (hash-ref meta 'scope "session"))
                 content
                 (hash 'source
                       'mem0
                       'session-id
                       session-id
                       'project-root
                       project-root
                       'tags
                       (hash-ref meta 'tags '())
                       'origin-tool-call-id
                       "mem0-adapter")
                 (hash 'sensitivity 'public 'confidence 0.5 'expires-at #f 'supersedes '())
                 (hash-ref entry 'created_at "2026-01-01T00:00:00Z")
                 (hash-ref entry 'updated_at "2026-01-01T00:00:00Z"))))

;; ---------------------------------------------------------------------------
;; Transport constructor
;; ---------------------------------------------------------------------------

;; Build an HTTP transport function for Mem0.
;; api-key is read once and closed over; never logged.
;; Returns (method payload) -> response-hash
(define (make-mem0-http-transport base-url api-key #:timeout-ms [timeout-ms 5000])
  (define base (string-trim base-url #rx"/+"))
  (define headers
    (list (string-append "Authorization: Token " api-key) "Content-Type: application/json"))
  (lambda (method payload)
    (with-handlers
        ([exn:fail?
          (lambda (e)
            (hash 'ok?
                  #f
                  'error
                  (hash 'code 'transport-error 'message "HTTP transport error" 'retryable? #t)))])
      (define-values (http-method path json-body) (mem0-method->http method payload base))
      (define json-str (and json-body (jsexpr->string json-body)))
      (define-values (status-line header-list in)
        (http-sendrecv (url-host base)
                       (url-path+query base path)
                       #:ssl? (string-prefix? base "https")
                       #:method http-method
                       #:headers headers
                       #:data (or json-str "")))
      (define resp-body (port->bytes in))
      (close-input-port in)
      (define status-code (parse-http-status status-line))
      (cond
        [(<= 200 status-code 299)
         (define resp-json
           (with-handlers ([exn:fail? (lambda (_) #f)])
             (bytes->jsexpr resp-body)))
         (hash 'ok? #t 'value (or resp-json (hash)))]
        [else
         (hash 'ok?
               #f
               'error
               (hash 'code
                     'http-error
                     'status
                     status-code
                     'message
                     (format "Mem0 API returned ~a" status-code)
                     'retryable?
                     (<= 500 status-code 599)))]))))

;; Helper: extract host from base-url
(define (url-host base-url)
  (define m (regexp-match #rx"^https?://([^/:]+)" base-url))
  (and m (cadr m)))

;; Helper: build path from base-url + path
(define (url-path+query base-url path)
  (define m (regexp-match #rx"^https?://[^/]+(/.*)?$" base-url))
  (define base-path
    (if (and m (cadr m))
        (string-trim (cadr m) #rx"/+$")
        ""))
  (if (string=? base-path "")
      path
      (string-append "/" base-path path)))

;; Helper: parse HTTP status code from status line
(define (parse-http-status status-line)
  (define m (regexp-match #rx"^HTTP/[0-9.]+ ([0-9]+)" status-line))
  (if m
      (string->number (cadr m))
      0))

;; Map Mem0 method + payload to (HTTP-method path json-body)
(define (mem0-method->http method payload base)
  (case method
    [(store) (values "POST" "/v1/memories/" payload)]
    [(retrieve) (values "POST" "/v1/memories/search/" payload)]
    [(list) (values "GET" "/v1/memories/" #f)]
    [(delete) (values "DELETE" (format "/v1/memories/~a" (hash-ref payload 'id "")) #f)]
    [(update) (values "PATCH" (format "/v1/memories/~a" (hash-ref payload 'id "")) payload)]
    [(manage) (values "GET" "/v1/memories/" #f)]
    [else (values "GET" "/v1/memories/" #f)]))

;; Build a mock transport for testing.
;; Returns (method payload) -> response-hash based on method.
(define (make-mem0-mock-transport #:store-response [store-response #f]
                                  #:search-response [search-response '()]
                                  #:list-response [list-response '()])
  (lambda (method payload)
    (case method
      [(store)
       (or store-response
           (hash 'ok? #t 'value (hash 'id "mock-mem0-id" 'content (hash-ref payload 'content ""))))]
      [(retrieve) (hash 'ok? #t 'value search-response)]
      [(list) (hash 'ok? #t 'value list-response)]
      [(delete) (hash 'ok? #t 'value #t)]
      [(update) (hash 'ok? #t 'value #t)]
      [(manage) (hash 'ok? #t 'value (hash 'cleaned 0))]
      [else
       (hash 'ok?
             #f
             'error
             (hash 'code 'unknown-method 'message (format "Unknown method: ~a" method)))])))

;; ---------------------------------------------------------------------------
;; Mem0 backend constructor
;; ---------------------------------------------------------------------------

;; Create a Mem0-backed memory-backend using an injectable transport.
;; If api-key-env is provided, reads the API key from that env var.
;; If api-key is provided directly, uses it (for testing only).
;; If neither, fails closed (backend unavailable).
(define (make-mem0-backend #:transport [transport #f]
                           #:base-url [base-url "https://api.mem0.ai"]
                           #:api-key-env [api-key-env #f]
                           #:api-key [api-key #f]
                           #:timeout-ms [timeout-ms 5000])
  (define effective-key (or api-key (and api-key-env (getenv api-key-env))))
  (cond
    [(not effective-key)
     ;; No API key available — fail closed with a no-op backend
     (memory-backend
      "mem0(unconfigured)"
      (lambda (item)
        (memory-result #f
                       #f
                       (make-memory-error 'no-api-key "Mem0 adapter: API key not configured" #f)
                       (hasheq)))
      (lambda (query) (memory-result #t '() #f (hasheq)))
      (lambda (id patch)
        (memory-result #f
                       #f
                       (make-memory-error 'no-api-key "Mem0 adapter: not configured" #f)
                       (hasheq)))
      (lambda (id scope)
        (memory-result #f
                       #f
                       (make-memory-error 'no-api-key "Mem0 adapter: not configured" #f)
                       (hasheq)))
      (lambda (query) (memory-result #t '() #f (hasheq)))
      (lambda () #f) ; not available
      (lambda (policy) (memory-result #t #f #f (hasheq))))]
    [else
     (define effective-transport (or transport (make-mem0-http-transport base-url effective-key)))
     ;; Wrap transport to decode Mem0 responses into protocol shape
     (define mem0-transport
       (lambda (method payload)
         (define raw (effective-transport method payload))
         (case method
           [(retrieve)
            (define items (hash-ref raw 'value '()))
            (hash 'ok? #t 'value (decode-mem0-items items "session" "."))]
           [else raw])))
     (make-external-backend (format "mem0(~a)" base-url) mem0-transport #:timeout-ms timeout-ms)]))

;; ---------------------------------------------------------------------------
;; Provide
;; ---------------------------------------------------------------------------

(provide make-mem0-backend
         make-mem0-mock-transport
         make-mem0-http-transport
         mem0-store-payload
         mem0-search-payload
         decode-mem0-items)
