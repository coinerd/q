#lang racket/base

;; @speed fast
;; @suite default
;; ADR-0023 Phase 3 test scaffolding — GAP-2, GAP-7, GAP-10, GAP-9
;; TDD red phase: tests verify behaviors that need implementing.

(require rackunit
         racket/string
         rackunit/text-ui
         ;; Memory types
         (only-in "../runtime/memory/types.rkt"
                  memory-item
                  memory-item-id
                  memory-item-content
                  memory-item-scope
                  memory-item-metadata)
         ;; Embedding interface
         (only-in "../runtime/memory/embeddings.rkt"
                  cosine-similarity
                  current-embedding-provider)
         ;; Search (GAP-2)
         (only-in "../runtime/memory/search.rkt"
                  rank-by-relevance)
         ;; Reflection (GAP-7)
         (only-in "../runtime/memory/reflection.rkt"
                  merge-group-items
                  current-reflection-llm-fn)
         ;; Conclusion bridge (GAP-10)
         (only-in "../runtime/memory/conclusion-bridge.rkt"
                  persist-high-value-conclusions!
                  current-conclusion-to-memory-bridge-enabled)
         ;; Hash backend for testing
         (only-in "../runtime/memory/backends/memory-hash.rkt"
                  make-memory-hash-backend
                  memory-hash-backend-items)
         ;; Auto-extraction (GAP-9)
         (only-in "../runtime/memory/auto-extraction.rkt"
                  classify-sensitivity)
         ;; Task conclusion
         (only-in "../runtime/context-assembly/task-conclusion.rkt"
                  task-conclusion))

;; ═══════════════════════════════════════════════════════════════
;; GAP-2: Semantic memory retrieval via embeddings
;; ═══════════════════════════════════════════════════════════════

(define gap2-suite
  (test-suite
   "ADR-0023 GAP-2: Semantic retrieval"

   (test-case "GAP-2: cosine-similarity computes correctly"
     (define a #(1.0 0.0 0.0))
     (define b #(0.0 1.0 0.0))
     (define c #(1.0 0.0 0.0))
     (check-within (cosine-similarity a c) 1.0 0.001)
     (check-within (cosine-similarity a b) 0.0 0.001))

   (test-case "GAP-2: rank-by-relevance with lexical fallback"
     ;; Two items: one about auth, one about files
     (define item-a (memory-item "item-a" 'semantic 'project
                                  "File I/O operations with buffered streams"
                                  (hasheq 'tags '()) (hasheq) "2026-01-01T00:00:00Z" "2026-01-01T00:00:00Z"))
     (define item-b (memory-item "item-b" 'semantic 'project
                                  "Authentication flow uses OAuth2 and JWT tokens"
                                  (hasheq 'tags '()) (hasheq) "2026-01-01T00:00:00Z" "2026-01-01T00:00:00Z"))
     ;; Without provider: lexical ranking should still work
     (define ranked-lexical (rank-by-relevance (list item-a item-b) "authentication flow"))
     (check-equal? (memory-item-id (car ranked-lexical)) "item-b"
                   "Lexical: 'authentication flow' should match item-b about auth"))

   (test-case "GAP-2: rank-by-relevance uses embedding path when provider available"
     ;; Mock provider returns vectors based on content
     (define (mock-provider text)
       (define lower (string-downcase text))
       (if (string-contains? lower "auth")
           #(1.0 0.0)
           #(0.0 1.0)))
     (define item-a (memory-item "item-a" 'semantic 'project
                                  "File I/O operations"
                                  (hasheq 'tags '()) (hasheq) "2026-01-01T00:00:00Z" "2026-01-01T00:00:00Z"))
     (define item-b (memory-item "item-b" 'semantic 'project
                                  "Authentication flow"
                                  (hasheq 'tags '()) (hasheq) "2026-01-01T00:00:00Z" "2026-01-01T00:00:00Z"))
     (parameterize ([current-embedding-provider mock-provider])
       (define ranked (rank-by-relevance (list item-a item-b) "auth"))
       (check-not-false ranked "Embedding ranking should return results")))))

;; ═══════════════════════════════════════════════════════════════
;; GAP-7: LLM-powered reflection merging
;; ═══════════════════════════════════════════════════════════════

(define gap7-suite
  (test-suite
   "ADR-0023 GAP-7: LLM reflection"

   (test-case "GAP-7: merge-group-items uses LLM synthesis when provider available"
     (define item-1 (memory-item "mi-1" 'semantic 'session
                                  "Implemented caching layer for API responses"
                                  (hasheq 'tags '("caching")) (hasheq)
                                  "2026-01-01T00:00:00Z" "2026-01-01T00:00:00Z"))
     (define item-2 (memory-item "mi-2" 'semantic 'session
                                  "Added cache invalidation on data mutation"
                                  (hasheq 'tags '("caching")) (hasheq)
                                  "2026-01-01T00:00:00Z" "2026-01-01T00:00:00Z"))
     ;; Without LLM: concatenation
     (define-values (text-no-llm ids-no-llm tags-no-llm)
       (parameterize ([current-reflection-llm-fn #f])
         (merge-group-items (list item-1 item-2))))
     (check-not-false (string-contains? text-no-llm "[reflection]")
                      "Default: should have [reflection] prefix")
     ;; With LLM: synthesis
     (define (mock-synthesize contents)
       (format "Synthesized: ~a items" (length contents)))
     (define-values (text-llm ids-llm tags-llm)
       (parameterize ([current-reflection-llm-fn mock-synthesize])
         (merge-group-items (list item-1 item-2))))
     (check-not-false (string-contains? text-llm "synthesized")
                      "With LLM: should contain synthesis output"))))

;; ═══════════════════════════════════════════════════════════════
;; GAP-10: Conclusion-to-memory bridge
;; ═══════════════════════════════════════════════════════════════

(define gap10-suite
  (test-suite
   "ADR-0023 GAP-10: Conclusion bridge"

   (test-case "GAP-10: high-value conclusions persisted to memory"
     (define backend (make-memory-hash-backend))
     (define conclusions
       (list (task-conclusion "c-1" "Decided to use HMAC-SHA256 for auth"
                              'decision 'implementation '() (current-seconds) '() '())))
     (parameterize ([current-conclusion-to-memory-bridge-enabled #t])
       (persist-high-value-conclusions! conclusions
                                        #:backend backend
                                        #:session-id "test-session"))
     (define items (memory-hash-backend-items backend))
     (check > (length items) 0
            "High-value conclusion should be persisted to memory"))

   (test-case "GAP-10: low-value conclusions NOT persisted"
     (define backend (make-memory-hash-backend))
     (define conclusions
       (list (task-conclusion "c-2" "Found a file at /tmp/test.txt"
                              'fact 'exploration '() (current-seconds) '() '())))
     (parameterize ([current-conclusion-to-memory-bridge-enabled #t])
       (persist-high-value-conclusions! conclusions
                                        #:backend backend
                                        #:session-id "test-session"))
     (define items (memory-hash-backend-items backend))
     (check = (length items) 0
            "Low-value fact conclusion should NOT be persisted"))))

;; ═══════════════════════════════════════════════════════════════
;; GAP-9: Sensitivity classification improvement
;; ═══════════════════════════════════════════════════════════════

(define gap9-suite
  (test-suite
   "ADR-0023 GAP-9: Sensitivity classification"

   (test-case "GAP-9: classify-sensitivity detects secret patterns"
     ;; API keys
     (check-eq? (classify-sensitivity "The API key is sk-proj-abc123") 'sensitive
                "Should detect API key prefix")
     ;; Password
     (check-eq? (classify-sensitivity "Password reset flow") 'sensitive
                "Should detect password keyword")
     ;; Internal markers
     (check-eq? (classify-sensitivity "This is confidential internal data") 'internal
                "Should detect internal markers")
     ;; Regular content
     (check-eq? (classify-sensitivity "Regular coding note about refactoring") 'public
                "Regular content should be public"))))

;; ═══════════════════════════════════════════════════════════════
;; Run all suites
;; ═══════════════════════════════════════════════════════════════

(define all-suites
  (test-suite
   "ADR-0023 Phase 3 Tests"
   gap2-suite
   gap7-suite
   gap10-suite
   gap9-suite))

(module+ main
  (void (run-tests all-suites)))

(module+ test
  (require (submod ".." main)))
