#lang racket/base
;;; test-mem0-http-transport-g2.rkt — W0 characterization for G2 Mem0 HTTP transport gap

(require rackunit
         "../runtime/memory/backends/mem0-api.rkt"
         "../runtime/memory/backends/external-protocol.rkt"
         "../runtime/memory/protocol.rkt"
         "../runtime/memory/types.rkt")

(define (test-item id content)
  (memory-item
   id
   'semantic
   'session
   content
   (hasheq 'source 'test 'session-id "s" 'project-root "." 'tags '() 'origin-tool-call-id "t")
   (hasheq 'sensitivity 'public 'confidence 0.5 'expires-at #f 'supersedes '())
   "2026-01-01T00:00:00Z"
   "2026-01-01T00:00:00Z"))

(define (error-code result)
  (hash-ref (memory-result-error result) 'code #f))

(test-case "W0 G2 characterization: real Mem0 HTTP transport is currently a not-implemented stub"
  (parameterize ([current-external-backend-enabled #t])
    (define be (make-mem0-backend #:api-key "test-key" #:base-url "http://127.0.0.1:9"))
    (define result (gen:store-memory! be (test-item "id1" "content for mem0 transport")))
    (check-false (memory-result-ok? result))
    (check-equal? (error-code result) 'not-implemented)))

(test-case "W0 G2 characterization: mock transport path still works and isolates unit tests from network"
  (parameterize ([current-external-backend-enabled #t])
    (define called? #f)
    (define be
      (make-mem0-backend #:api-key "test-key"
                         #:transport (lambda (method payload)
                                       (set! called? #t)
                                       (hash 'ok? #t 'value (hash 'id "mock-id")))))
    (define result (gen:store-memory! be (test-item "id2" "content for mock transport")))
    (check-true called?)
    (check-true (memory-result-ok? result))))
