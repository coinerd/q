#lang racket/base

;; scripts/bench-token-estimate-cache.rkt — v0.70.6 W2
;;
;; Compare cached vs uncached token estimation performance.
;; Run: racket scripts/bench-token-estimate-cache.rkt

(require racket/port
         racket/string
         (file "../util/token-estimate-cache.rkt")
         (file "../llm/token-budget.rkt"))

;; Simple estimator wrapper matching the cache signature
(define (estimator text)
  (estimate-text-tokens text))

;; Generate deterministic test text
(define (make-test-text n)
  (string-join (for/list ([i (in-range n)])
                 (format "word~a" i))
               " "))

(define texts (for/list ([i (in-range 100)])
                (make-test-text (+ 10 (modulo i 50)))))

;; Benchmark helper
(define (bench name thunk)
  (define start (current-inexact-milliseconds))
  (thunk)
  (define end (current-inexact-milliseconds))
  (printf "~a: ~a ms\n" name (real->decimal-string (- end start) 2)))

(printf "=== Token Estimate Cache Benchmark ===\n")

;; Warmup
(clear-token-estimate-cache!)
(for ([t (in-list texts)]) (cached-estimate-text-tokens estimator t))

;; Uncached (clear before each run)
(bench "Uncached (100 texts, 10 runs)"
       (lambda ()
         (for ([_ (in-range 10)])
           (clear-token-estimate-cache!)
           (for ([t (in-list texts)])
             (cached-estimate-text-tokens estimator t)))))

;; Cached (reuse cache across runs)
(clear-token-estimate-cache!)
(for ([t (in-list texts)]) (cached-estimate-text-tokens estimator t))
(bench "Cached   (100 texts, 10 runs)"
       (lambda ()
         (for ([_ (in-range 10)])
           (for ([t (in-list texts)])
             (cached-estimate-text-tokens estimator t)))))

(define stats (token-estimate-cache-stats))
(printf "Cache entries: ~a\n" (hash-ref stats 'entries))
(printf "Cache hits:    ~a\n" (hash-ref stats 'hits))
(printf "Cache misses:  ~a\n" (hash-ref stats 'misses))
