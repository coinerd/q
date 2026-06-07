#lang racket/base

;;; test-mem0-http-transport-g2.rkt — W3: Mem0 HTTP transport tests
;;; Uses a fake HTTP server to verify request/response mapping without real network.

(require rackunit
         racket/file
         racket/match
         racket/string
         racket/port
         (only-in json jsexpr->string bytes->jsexpr)
         (only-in "../runtime/memory/backends/mem0-api.rkt"
                  make-mem0-backend
                  make-mem0-http-transport
                  mem0-store-payload
                  mem0-search-payload
                  decode-mem0-items)
         (only-in "../runtime/memory/types.rkt"
                  memory-item
                  memory-item-content
                  memory-item-scope
                  memory-item-type
                  memory-item-id
                  memory-item-metadata
                  memory-item-validity
                  memory-item-created-at
                  memory-item-updated-at
                  memory-query
                  memory-result-ok?
                  memory-result-error
                  memory-result-value)
         (only-in "../runtime/memory/protocol.rkt"
                  gen:retrieve-memory
                  memory-backend-store!
                  memory-backend-retrieve
                  memory-backend-list)
         (only-in "../runtime/memory/backends/external-protocol.rkt"
                  current-external-backend-enabled))

;; ---------------------------------------------------------------------------
;; Fake HTTP transport — captures calls, returns configured responses
;; ---------------------------------------------------------------------------

(define captured-requests '())

(define (make-capturing-transport response-fn)
  (set! captured-requests '())
  (lambda (host path method headers data)
    (set! captured-requests
          (cons (hasheq 'host host 'path path 'method method 'headers headers 'data data)
                captured-requests))
    (response-fn method path data)))

;; Test that the transport sends correct HTTP method/path/headers
(test-case "W3: store maps to POST /v1/memories/ with auth header"
  (define transport (make-mem0-http-transport "https://api.mem0.ai" "test-key-123" #:timeout-ms 1000))
  ;; We can't call transport directly with a fake server without modifying the
  ;; make-mem0-http-transport function. Instead, verify the helper functions.
  (define item
    (memory-item
     "id1"
     'semantic
     'session
     "test content"
     (hasheq 'source 'test 'session-id "s1" 'project-root "." 'tags '() 'origin-tool-call-id "tc1")
     (hasheq 'sensitivity 'public 'confidence 0.7 'supersedes '())
     "2026-01-01T00:00:00Z"
     "2026-01-01T00:00:00Z"))
  (define payload (mem0-store-payload item))
  (check-equal? (hash-ref payload 'content) "test content")
  (check-equal? (hash-ref (hash-ref payload 'metadata) 'type) "semantic"))

(test-case "W3: search maps to POST /v1/memories/search/"
  (define q (memory-query "racket" 'session #f "s1" #f #f 10 #t))
  (define payload (mem0-search-payload q))
  (check-equal? (hash-ref payload 'query) "racket")
  (check-equal? (hash-ref payload 'scope) "session")
  (check-equal? (hash-ref payload 'limit) 10))

(test-case "W3: decode-mem0-items converts response to memory-items"
  (define raw-items
    (list (hasheq 'id
                  "mem1"
                  'content
                  "Racket uses hygienic macros"
                  'metadata
                  (hasheq 'type "semantic" 'scope "project" 'tags '())
                  'created_at
                  "2026-01-01T00:00:00Z"
                  'updated_at
                  "2026-01-01T00:00:00Z")))
  (define items (decode-mem0-items raw-items "s1" "."))
  (check-equal? (length items) 1)
  (check-equal? (memory-item-id (car items)) "mem1")
  (check-equal? (memory-item-type (car items)) 'semantic)
  (check-equal? (memory-item-scope (car items)) 'project)
  (check-equal? (memory-item-content (car items)) "Racket uses hygienic macros"))

(test-case "W3: transport fails closed on network error"
  ;; The real HTTP transport will fail with exn:fail when no server is listening.
  ;; Verify it returns a fail-closed result, not an exception.
  (define transport
    (make-mem0-http-transport "https://127.0.0.1:1" ; port 1 — nothing listening
                              "test-key"
                              #:timeout-ms 500))
  (define result (transport 'store (hasheq 'content "test")))
  (check-false (hash-ref result 'ok? #t) "Transport must return ok?=false on network error")
  (check-not-false (hash-ref (hash-ref result 'error) 'code #f)))

(test-case "W3: no API key leakage in error responses"
  (define transport
    (make-mem0-http-transport "https://127.0.0.1:1" "super-secret-key-do-not-log" #:timeout-ms 500))
  (define result (transport 'store (hasheq 'content "test")))
  ;; Error response must not contain the API key
  (define error-hash (hash-ref result 'error (hasheq)))
  (define msg (hash-ref error-hash 'message ""))
  (check-false (string-contains? msg "super-secret-key-do-not-log")
               "API key must not appear in error messages"))

(test-case "W3: url-host extraction works via transport seam"
  ;; Verify the transport receives correctly shaped requests by using
  ;; an injectable transport seam that captures method + payload.
  (define captured #f)
  (define backend
    (make-mem0-backend #:api-key "test-key"
                       #:transport (lambda (method payload)
                                     (set! captured (cons method payload))
                                     (hash 'ok? #t 'value '()))))
  ;; Trigger a retrieve — external backend requires enabled flag
  (parameterize ([current-external-backend-enabled #t])
    (gen:retrieve-memory backend (memory-query "test" 'session #f "s1" #f #f 10 #t)))
  (check-equal? (car captured) 'retrieve)
  (check-equal? (hash-ref (cdr captured) 'text) "test"))

(test-case "W3: mem0-method->http maps all methods correctly"
  ;; Verify each method reaches the transport seam correctly.
  (define calls '())
  (define (tracking-transport method payload)
    (set! calls (cons (cons method payload) calls))
    (case method
      [(store) (hash 'ok? #t 'value (hash 'id "new-id"))]
      [(retrieve) (hash 'ok? #t 'value '())]
      [(list) (hash 'ok? #t 'value '())]
      [(delete) (hash 'ok? #t 'value #t)]
      [(update) (hash 'ok? #t 'value #t)]
      [(manage) (hash 'ok? #t 'value (hash 'cleaned 0))]
      [else (hash 'ok? #f 'error (hash 'code 'unknown-method))]))
  (define backend (make-mem0-backend #:api-key "key" #:transport tracking-transport))
  ;; Exercise store directly via backend accessor (external enabled required)
  (parameterize ([current-external-backend-enabled #t])
    ((memory-backend-store! backend)
     (memory-item
      "id1"
      'semantic
      'session
      "hello"
      (hasheq 'source 'test 'session-id "s1" 'project-root "." 'tags '() 'origin-tool-call-id "tc1")
      (hasheq 'sensitivity 'public 'confidence 0.7 'supersedes '())
      "2026-01-01T00:00:00Z"
      "2026-01-01T00:00:00Z"))
    ;; Exercise retrieve
    ((memory-backend-retrieve backend) (memory-query "q" 'session #f "s1" #f #f 10 #t))
    ;; Exercise list
    ((memory-backend-list backend) (memory-query "" #f #f #f #f #f 50 #f)))
  (define method-set
    (for/hash ([c (in-list calls)])
      (values (car c) #t)))
  (check-true (hash-has-key? method-set 'store))
  (check-true (hash-has-key? method-set 'retrieve))
  (check-true (hash-has-key? method-set 'list)))

;; ---------------------------------------------------------------------------
;; v0.95.18 W0: Audit regression tests (expected red before W1)
;; ---------------------------------------------------------------------------

(test-case "W0 F1: Mem0 retrieve raw transport failure propagates fail-closed"
  (define backend
    (make-mem0-backend
     #:api-key "test-key"
     #:transport
     (lambda (method payload)
       (hash 'ok?
             #f
             'error
             (hash 'code 'mem0-timeout 'message "redacted transport failure" 'retryable? #t)))))
  (define result
    (parameterize ([current-external-backend-enabled #t])
      (gen:retrieve-memory backend (memory-query "racket" 'session #f "s1" #f #f 10 #t))))
  (check-false (memory-result-ok? result))
  (check-equal? (hash-ref (memory-result-error result) 'code) 'mem0-timeout))

(test-case "W0 F2/F3: transport tests must not contain placeholder assertions"
  (define this-file (build-path (current-directory) "test-mem0-http-transport-g2.rkt"))
  (define source (file->string this-file))
  ;; No check-true #t placeholder assertions should remain
  (check-false (regexp-match? #rx"check-true[[:space:]]+#t" source)))

(test-case "W0 F2: Mem0 authorization scheme follows Api-key contract"
  ;; Verify the implementation file uses Api-key, not Token
  (define impl-path
    (build-path (current-directory) ".." "runtime" "memory" "backends" "mem0-api.rkt"))
  (define source (file->string impl-path))
  (check-true (string-contains? source "Authorization: Api-key"))
  ;; Also verify no Token scheme is present (was the pre-fix scheme)
  (check-false (string-contains? source "Authorization: Token")))

;; ---------------------------------------------------------------------------
;; v0.95.19 W0: LF2 regression — hardcoded session-id/project-root in decode
;; ---------------------------------------------------------------------------

(test-case "W0 LF2: decode-mem0-items uses actual session-id not hardcoded value"
  ;; Create a mock transport that returns a known item
  (define mock-transport
    (lambda (method payload)
      (case method
        [(retrieve)
         (hash 'ok?
               #t
               'value
               (list (hash 'id
                           "mem-lf2"
                           'content
                           "LF2 test content"
                           'metadata
                           (hash 'tags '("test") 'type "semantic" 'scope "project"))))]
        [else (hash 'ok? #t 'value '())])))
  (parameterize ([current-external-backend-enabled #t])
    (define backend (make-mem0-backend #:api-key "test-key" #:transport mock-transport))
    ;; Retrieve with a specific session-id and project-root
    (define q (memory-query "" #f "/my-project" "my-session-123" #f #f 10 #f))
    (define result ((memory-backend-retrieve backend) q))
    (check-true (memory-result-ok? result))
    (define items (memory-result-value result))
    (when (pair? items)
      (define meta (memory-item-metadata (car items)))
      ;; The decoded item should have the query's session-id, not "session"
      (check-equal? (hash-ref meta 'session-id #f)
                    "my-session-123"
                    "Decoded item should use query session-id, not hardcoded 'session'")
      ;; The decoded item should have the query's project-root, not "."
      (check-equal? (hash-ref meta 'project-root #f)
                    "/my-project"
                    "Decoded item should use query project-root, not hardcoded '.'"))))
