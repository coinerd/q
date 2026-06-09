#lang racket/base
;; runtime/memory/embeddings.rkt — Embedding provider interface for semantic search
;; STABILITY: evolving
;;
;; GAP-2: Embedding-based retrieval alongside lexical search.
;; If current-embedding-provider is #f (default), lexical search is unchanged.

(provide current-embedding-provider
         current-embedding-dimension
         cosine-similarity)

(define current-embedding-provider (make-parameter #f))
(define current-embedding-dimension (make-parameter 1536))

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
