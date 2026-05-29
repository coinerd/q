#lang racket/base

;; scripts/bench-trace-logger.rkt — v0.70.4 stress benchmark
;; Compares sync vs async trace sink write throughput.
;;
;; Usage: racket scripts/bench-trace-logger.rkt [N]
;; Default N = 1000 events

(require racket/class
         racket/file
         "../runtime/trace-sink.rkt")

(define (bench-sync n)
  (define tmp (make-temporary-file "bench-sync-~a.jsonl"))
  (define sink (new file-trace-sink% [path tmp] [exists-mode 'truncate]))
  (define start (current-inexact-milliseconds))
  (for ([i (in-range n)])
    (send sink trace-write! (hasheq 'seq i 'data "benchmark-payload")))
  (send sink trace-flush!)
  (send sink trace-close!)
  (define elapsed (- (current-inexact-milliseconds) start))
  (delete-file tmp)
  elapsed)

(define (bench-async n)
  (define tmp (make-temporary-file "bench-async-~a.jsonl"))
  (define inner (new file-trace-sink% [path tmp] [exists-mode 'truncate]))
  (define sink (new async-trace-sink% [inner-sink inner] [capacity 1000]))
  (define start (current-inexact-milliseconds))
  (for ([i (in-range n)])
    (send sink trace-write! (hasheq 'seq i 'data "benchmark-payload")))
  (send sink trace-flush!)
  (send sink trace-close!)
  (define elapsed (- (current-inexact-milliseconds) start))
  (delete-file tmp)
  elapsed)

(define n (if (and (vector? (current-command-line-arguments))
                   (> (vector-length (current-command-line-arguments)) 0))
              (string->number (vector-ref (current-command-line-arguments) 0))
              1000))

(printf "Benchmark: ~a trace writes\n" n)
(define sync-ms (bench-sync n))
(define async-ms (bench-async n))
(printf "  sync:  ~a ms (~a writes/sec)\n" sync-ms (real->decimal-string (* 1000.0 (/ n sync-ms)) 1))
(printf "  async: ~a ms (~a writes/sec)\n" async-ms (real->decimal-string (* 1000.0 (/ n async-ms)) 1))
(printf "  speedup: ~ax\n" (real->decimal-string (/ sync-ms async-ms) 2))
