#lang racket/base
;; tests/test-gap3-embedding-cache.rkt — GAP-3 TDD tests
;; Validates embedding cache hit/miss, batch provider, and fallback

(require rackunit
         rackunit/text-ui
         racket/vector
         (only-in "../runtime/memory/embeddings.rkt"
                  current-embedding-provider
                  current-batch-embedding-provider
                  cosine-similarity
                  cached-embed
                  clear-embedding-cache!
                  embedding-cache-stats)
         (only-in "../runtime/memory/search.rkt"
                  rank-by-relevance)
         (only-in "../runtime/memory/types.rkt"
                  memory-item memory-item-content memory-item-id))

;; Helper: simple embedding that returns a fixed vector per text
(define call-count (make-parameter 0))
(define (mock-embed text)
  (call-count (+ 1 (call-count)))
  (define h (equal-hash-code text))
  (vector (exact->inexact (modulo h 100))
          (exact->inexact (modulo (quotient h 100) 100))
          (exact->inexact (modulo (quotient h 10000) 100))))

(define (mock-batch-embed texts)
  (map mock-embed texts))

(define (make-test-item id content)
  (memory-item id
               'semantic
               'project
               content
               (hasheq 'source 'test 'session-id "test" 'project-root "." 'tags '())
               (hasheq 'sensitivity 'public 'confidence 0.8 'supersedes '())
               "2026-01-01T00:00:00Z"
               "2026-01-01T00:00:00Z"))

(define-test-suite gap-3-tests
  (test-case "GAP-3: cached-embed returns #f without provider"
    (parameterize ([current-embedding-provider #f])
      (clear-embedding-cache!)
      (check-false (cached-embed "test text"))))

  (test-case "GAP-3: cached-embed calls provider on cache miss"
    (clear-embedding-cache!)
    (parameterize ([current-embedding-provider mock-embed]
                   [call-count 0])
      (define result (cached-embed "hello world"))
      (check-not-false result "Should return an embedding vector")
      (check = (call-count) 1 "Provider should be called once")))

  (test-case "GAP-3: cached-embed returns cached result on hit"
    (clear-embedding-cache!)
    (parameterize ([current-embedding-provider mock-embed]
                   [call-count 0])
      (define r1 (cached-embed "cache hit test"))
      (check = (call-count) 1)
      (define r2 (cached-embed "cache hit test"))
      (check = (call-count) 1 "Provider should NOT be called again")
      (check-equal? r1 r2 "Cached result must equal original")))

  (test-case "GAP-3: embedding-cache-stats tracks hits and misses"
    (clear-embedding-cache!)
    (parameterize ([current-embedding-provider mock-embed])
      (cached-embed "stat miss")
      (cached-embed "stat miss")  ; hit
      (cached-embed "stat miss")  ; hit
      (define stats (embedding-cache-stats))
      (check >= (hash-ref stats 'hits) 2)
      (check >= (hash-ref stats 'misses) 1)
      (check >= (hash-ref stats 'size) 1)))

  (test-case "GAP-3: clear-embedding-cache! resets cache"
    (clear-embedding-cache!)
    (parameterize ([current-embedding-provider mock-embed]
                   [call-count 0])
      (cached-embed "to be cleared")
      (clear-embedding-cache!)
      (cached-embed "to be cleared")
      (check = (call-count) 2 "Should re-call after clear")))

  (test-case "GAP-3: rank-by-relevance uses cache (no N+1)"
    (clear-embedding-cache!)
    (parameterize ([current-embedding-provider mock-embed]
                   [call-count 0])
      (define items (list (make-test-item "i1" "item one content here")
                          (make-test-item "i2" "item two content here")))
      (define ranked (rank-by-relevance items "query"))
      ;; Query embed (1 call) + 2 item embeds (2 calls) = 3
      (check = (call-count) 3 "Should be 3 calls, not N+1 repeated")
      (check = (length ranked) 2)))

  (test-case "GAP-3: batch provider called once for all items"
    (clear-embedding-cache!)
    (define batch-calls (make-parameter 0))
    (parameterize ([current-embedding-provider mock-embed]
                   [current-batch-embedding-provider
                    (lambda (texts)
                      (batch-calls (+ 1 (batch-calls)))
                      (map mock-embed texts))]
                   [batch-calls 0])
      (define items (list (make-test-item "b1" "batch item one")
                          (make-test-item "b2" "batch item two")
                          (make-test-item "b3" "batch item three")))
      (define ranked (rank-by-relevance items "query"))
      (check >= (batch-calls) 1 "Batch provider should be called at least once")
      (check = (length ranked) 3)))

  (test-case "GAP-3: falls back to lexical on provider error"
    (clear-embedding-cache!)
    (parameterize ([current-embedding-provider
                    (lambda (text) (error "API down"))])
      (define items (list (make-test-item "e1" "error fallback test content")))
      (define ranked (rank-by-relevance items "error"))
      ;; Should not crash, falls back to lexical
      (check = (length ranked) 1))))

(run-tests gap-3-tests)
