#lang racket/base

(require racket/list)
;; runtime/memory/embeddings.rkt — Embedding provider interface for semantic search
;; STABILITY: evolving
;;
;; GAP-2: Embedding-based retrieval alongside lexical search.
;; GAP-3: Session-scoped embedding cache to avoid N+1 API calls.
;; If current-embedding-provider is #f (default), lexical search is unchanged.

(provide current-embedding-provider
         current-embedding-dimension
         current-batch-embedding-provider
         cosine-similarity
         cached-embed
         clear-embedding-cache!
         embedding-cache-stats)

;; ---------------------------------------------------------------------------
;; Parameters
;; ---------------------------------------------------------------------------

(define current-embedding-provider (make-parameter #f))
(define current-embedding-dimension (make-parameter 1536))

;; Optional batch embedding provider: (listof string) -> (listof (or/c #f (vectorof real)))
;; When set, rank-by-relevance can compute all embeddings in one API call.
(define current-batch-embedding-provider (make-parameter #f))

;; ---------------------------------------------------------------------------
;; Cosine similarity
;; ---------------------------------------------------------------------------

(define (cosine-similarity a b)
  (define n (min (vector-length a) (vector-length b)))
  (define dot
    (for/fold ([s 0.0]) ([i (in-range n)])
      (+ s (* (vector-ref a i) (vector-ref b i)))))
  (define mag-a
    (sqrt (for/fold ([s 0.0]) ([i (in-range n)])
            (+ s (expt (vector-ref a i) 2)))))
  (define mag-b
    (sqrt (for/fold ([s 0.0]) ([i (in-range n)])
            (+ s (expt (vector-ref b i) 2)))))
  (if (or (zero? mag-a) (zero? mag-b))
      0.0
      (/ dot (* mag-a mag-b))))

;; ---------------------------------------------------------------------------
;; GAP-3: Session-scoped embedding cache
;; ---------------------------------------------------------------------------

;; Maximum cache entries (LRU eviction when exceeded)
(define max-cache-entries 500)

;; The cache: mutable hash from text-hash-code -> (vector embedding timestamp)
;; Using equal-hash-code for O(1) lookup without crypto dependency.
(define embedding-cache (make-hash))
(define cache-hits (make-parameter 0))
(define cache-misses (make-parameter 0))

;; Compute a stable cache key for a string.
(define (cache-key text)
  (equal-hash-code text))

;; Look up embedding in cache. Returns #f if not found.
(define (cache-lookup text)
  (define entry (hash-ref embedding-cache (cache-key text) #f))
  (if entry
      (begin
        (cache-hits (+ 1 (cache-hits)))
        entry)
      (begin
        (cache-misses (+ 1 (cache-misses)))
        #f)))

;; Store embedding in cache with LRU eviction.
(define (cache-store! text embedding)
  (when (> (hash-count embedding-cache) max-cache-entries)
    ;; Evict oldest 25% of entries (simple eviction, not true LRU)
    (define entries
      (sort (hash->list embedding-cache)
            (lambda (a b) (< (vector-ref (cdr a) 2) (vector-ref (cdr b) 2)))))
    (define to-evict (max 1 (quotient (length entries) 4)))
    (for ([e (in-list (take entries to-evict))])
      (hash-remove! embedding-cache (car e))))
  (hash-set! embedding-cache
             (cache-key text)
             (vector embedding (string-length text) (current-inexact-milliseconds))))

;; Get embedding for text, using cache if available.
;; Returns #f if provider is not set or provider returns #f.
(define (cached-embed text)
  (define provider (current-embedding-provider))
  (cond
    [(not provider) #f]
    [else
     (define cached (cache-lookup text))
     (cond
       [cached (vector-ref cached 0)]
       [else
        (define emb (provider text))
        (when emb
          (cache-store! text emb))
        emb])]))

;; Clear the embedding cache (call on session shutdown).
(define (clear-embedding-cache!)
  (hash-clear! embedding-cache)
  (cache-hits 0)
  (cache-misses 0))

;; Return cache statistics for diagnostics.
(define (embedding-cache-stats)
  (hasheq 'size (hash-count embedding-cache) 'hits (cache-hits) 'misses (cache-misses)))
