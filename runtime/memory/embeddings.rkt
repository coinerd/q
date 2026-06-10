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
         current-embedding-cache
         make-embedding-cache-state
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
;; GAP-3: Session-scoped embedding cache (thread-safe)
;; H5 v0.97.13: Replaced bare mutable hash with semaphore-guarded struct.
;; ---------------------------------------------------------------------------

;; Maximum cache entries (LRU eviction when exceeded)
(define max-cache-entries 500)

;; Thread-safe cache struct
(struct embedding-cache-state
        (table ; mutable hash: cache-key -> (vector embedding len timestamp)
         lock ; semaphore for thread safety
         hits ; box of exact-nonnegative-integer
         misses) ; box of exact-nonnegative-integer
  #:transparent)

(define (make-embedding-cache-state)
  (embedding-cache-state (make-hash) (make-semaphore 1) (box 0) (box 0)))

;; Module-level cache — parameterized for testability
(define current-embedding-cache (make-parameter (make-embedding-cache-state)))

(define (with-cache-lock cache thunk)
  (call-with-semaphore (embedding-cache-state-lock cache) thunk))

;; Compute a collision-resistant cache key for a string.
;; Uses length + hash-code to reduce collision probability.
(define (cache-key text)
  (cons (string-length text) (equal-hash-code text)))

;; Look up embedding in cache. Returns #f if not found.
(define (cache-lookup text)
  (define cache (current-embedding-cache))
  (define tbl (embedding-cache-state-table cache))
  (define entry (with-cache-lock cache (λ () (hash-ref tbl (cache-key text) #f))))
  (if entry
      (begin
        (with-cache-lock cache
                         (λ ()
                           (set-box! (embedding-cache-state-hits cache)
                                     (+ 1 (unbox (embedding-cache-state-hits cache))))))
        entry)
      (begin
        (with-cache-lock cache
                         (λ ()
                           (set-box! (embedding-cache-state-misses cache)
                                     (+ 1 (unbox (embedding-cache-state-misses cache))))))
        #f)))

;; Store embedding in cache with LRU eviction.
(define (cache-store! text embedding)
  (define cache (current-embedding-cache))
  (define tbl (embedding-cache-state-table cache))
  (with-cache-lock
   cache
   (λ ()
     (when (> (hash-count tbl) max-cache-entries)
       ;; Evict oldest 25% of entries (simple eviction, not true LRU)
       (define entries
         (sort (hash->list tbl) (lambda (a b) (< (vector-ref (cdr a) 2) (vector-ref (cdr b) 2)))))
       (define to-evict (max 1 (quotient (length entries) 4)))
       (for ([e (in-list (take entries to-evict))])
         (hash-remove! tbl (car e))))
     (hash-set! tbl
                (cache-key text)
                (vector embedding (string-length text) (current-inexact-milliseconds))))))

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
  (define cache (current-embedding-cache))
  (with-cache-lock cache
                   (λ ()
                     (hash-clear! (embedding-cache-state-table cache))
                     (set-box! (embedding-cache-state-hits cache) 0)
                     (set-box! (embedding-cache-state-misses cache) 0))))

;; Return cache statistics for diagnostics.
(define (embedding-cache-stats)
  (define cache (current-embedding-cache))
  (with-cache-lock cache
                   (λ ()
                     (hasheq 'size
                             (hash-count (embedding-cache-state-table cache))
                             'hits
                             (unbox (embedding-cache-state-hits cache))
                             'misses
                             (unbox (embedding-cache-state-misses cache))))))
