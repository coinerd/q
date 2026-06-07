#lang racket/base
;;; test-memory-external-mem0-w5.rkt — W5 tests for Mem0 external adapter
(require rackunit
         racket/string
         "../runtime/memory/backends/mem0-api.rkt"
         "../runtime/memory/backends/external-protocol.rkt"
         "../runtime/memory/protocol.rkt"
         "../runtime/memory/types.rkt")

(test-case "W5: make-mem0-backend without API key fails closed"
  (define be (make-mem0-backend))
  (check-equal? (memory-backend-name be) "mem0(unconfigured)")
  (check-false ((memory-backend-available? be)))
  ;; Store should fail
  (define item
    (memory-item
     "id1"
     'semantic
     'session
     "test"
     (hasheq 'source 'test 'session-id "s" 'project-root "." 'tags '() 'origin-tool-call-id "t")
     (hasheq 'sensitivity 'public 'confidence 0.5 'expires-at #f 'supersedes '())
     "2026-01-01T00:00:00Z"
     "2026-01-01T00:00:00Z"))
  (define result (gen:store-memory! be item))
  (check-false (memory-result-ok? result)))

(test-case "W5: make-mem0-backend with mock transport constructs backend"
  (define mock (make-mem0-mock-transport))
  (parameterize ([current-external-backend-enabled #t])
    (define be (make-mem0-backend #:transport mock #:api-key "test-key"))
    (check-true (memory-backend? be))
    (check-true (string-contains? (memory-backend-name be) "mem0"))))

(test-case "W5: store request via mock transport generates expected payload"
  (define captured-payload #f)
  (define mock
    (lambda (method payload)
      (set! captured-payload payload)
      (hash 'ok? #t 'value (hash 'id "mem0-id-1" 'content "test content"))))
  (parameterize ([current-external-backend-enabled #t])
    (define be (make-mem0-backend #:transport mock #:api-key "test-key"))
    (define item
      (memory-item
       "id1"
       'semantic
       'session
       "test content for store"
       (hasheq 'source 'test 'session-id "s" 'project-root "." 'tags '() 'origin-tool-call-id "t")
       (hasheq 'sensitivity 'public 'confidence 0.5 'expires-at #f 'supersedes '())
       "2026-01-01T00:00:00Z"
       "2026-01-01T00:00:00Z"))
    (define result (gen:store-memory! be item))
    (check-true (memory-result-ok? result))
    ;; Verify captured payload has expected fields and NO API key
    (when captured-payload
      (check-false (hash-has-key? captured-payload 'api_key))
      (check-false (hash-has-key? captured-payload 'authorization)))))

(test-case "W5: retrieve maps mock response into memory-items"
  (define mock
    (make-mem0-mock-transport #:search-response (list (hash 'id
                                                            "mem0-1"
                                                            'content
                                                            "The project uses Racket"
                                                            'metadata
                                                            (hash 'type "semantic" 'scope "project")
                                                            'created_at
                                                            "2026-01-01T00:00:00Z"
                                                            'updated_at
                                                            "2026-01-01T00:00:00Z"))))
  (parameterize ([current-external-backend-enabled #t])
    (define be (make-mem0-backend #:transport mock #:api-key "test-key"))
    (define query (memory-query "Racket" #f #f #f #f #f 10 #f))
    (define result (gen:retrieve-memory be query))
    (check-true (memory-result-ok? result))
    (define items (memory-result-value result))
    (check-true (list? items))
    (check-true (> (length items) 0) "Should return at least one item")))

(test-case "W5: mock transport error maps to memory-result error"
  (define mock
    (lambda (method payload)
      (hash 'ok? #f 'error (hash 'code 'timeout 'message "Connection timed out after 5000ms"))))
  (parameterize ([current-external-backend-enabled #t])
    (define be (make-mem0-backend #:transport mock #:api-key "test-key"))
    (define item
      (memory-item
       "id1"
       'semantic
       'session
       "content"
       (hasheq 'source 'test 'session-id "s" 'project-root "." 'tags '() 'origin-tool-call-id "t")
       (hasheq 'sensitivity 'public 'confidence 0.5 'expires-at #f 'supersedes '())
       "2026-01-01T00:00:00Z"
       "2026-01-01T00:00:00Z"))
    (define result (gen:store-memory! be item))
    (check-false (memory-result-ok? result))))

(test-case "W5: mem0-store-payload does not expose API key"
  (define item
    (memory-item
     "id1"
     'semantic
     'session
     "secret content"
     (hasheq 'source 'test 'session-id "s" 'project-root "." 'tags '() 'origin-tool-call-id "t")
     (hasheq 'sensitivity 'public 'confidence 0.5 'expires-at #f 'supersedes '())
     "2026-01-01T00:00:00Z"
     "2026-01-01T00:00:00Z"))
  (define payload (mem0-store-payload item))
  (check-false (hash-has-key? payload 'api_key))
  (check-false (hash-has-key? payload 'api-key))
  (check-equal? (hash-ref payload 'content) "secret content"))

(test-case "W5: decode-mem0-items converts Mem0 response to memory-items"
  (define raw-items
    (list (hash 'id
                "m1"
                'content
                "Racket is used for the agent core"
                'metadata
                (hash 'type "semantic" 'scope "project")
                'created_at
                "2026-01-01T00:00:00Z"
                'updated_at
                "2026-06-01T00:00:00Z")))
  (define items (decode-mem0-items raw-items "sess-1" "/tmp/project"))
  (check-equal? (length items) 1)
  (define first-item (car items))
  (check-equal? (memory-item-id first-item) "m1")
  (check-equal? (memory-item-type first-item) 'semantic)
  (check-equal? (memory-item-scope first-item) 'project))

(test-case "W5: no real network access — mock transport only"
  ;; Verify mock transport never touches network
  (define network-called? #f)
  (define mock
    (lambda (method payload)
      (set! network-called? #t)
      (hash 'ok? #t 'value '())))
  (parameterize ([current-external-backend-enabled #t])
    (define be (make-mem0-backend #:transport mock #:api-key "test-key"))
    (define query (memory-query "test" #f #f #f #f #f 5 #f))
    (gen:retrieve-memory be query)
    (check-true network-called? "Mock transport was called (not HTTP)")))
