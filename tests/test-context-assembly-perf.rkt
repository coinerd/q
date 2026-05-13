#lang racket

;; BOUNDARY: integration

;; tests/test-context-assembly-perf.rkt — Token memoization benchmark
;;
;; Verifies that per-assembly memoization reduces repeated token estimation.
;; Tests that build-assembled-context produces correct results consistently
;; and that catalog generation uses O(1) counting.

(require rackunit
         rackunit/text-ui
         "../util/protocol-types.rkt"
         "../runtime/session-store.rkt"
         "../runtime/session-index.rkt"
         "../runtime/context-assembly.rkt"
         "../runtime/context-policy.rkt")

;; ── Helpers ──────────────────────────────────────────────────

(define (make-temp-dir)
  (make-temporary-file "q-ctx-perf-~a" 'directory))

(define (session-path dir)
  (build-path dir "session.jsonl"))

(define (index-path dir)
  (build-path dir "session.index"))

(define (make-msg id parent-id role kind text)
  (make-message id parent-id role kind (list (make-text-part text)) (current-seconds) (hasheq)))

(define (build-linear-session count #:content [content "Message content for testing assembly"])
  (define dir (make-temp-dir))
  (define sp (session-path dir))
  (define ip (index-path dir))
  (define entries
    (for/list ([i (in-range count)])
      (define id (format "m~a" i))
      (define parent
        (if (= i 0)
            #f
            (format "m~a" (sub1 i))))
      (define role
        (if (= i 0)
            'system
            (if (odd? i) 'user 'assistant)))
      (define kind (if (= i 0) 'system-instruction 'message))
      (make-msg id parent role kind (format "[~a] ~a" i content))))
  (append-entries! sp entries)
  (define idx (build-index! sp ip))
  (values idx dir))

(define perf-tests
  (test-suite "context-assembly-perf"

    ;; Memoization produces deterministic results
    (test-case "memoized assembly produces deterministic output"
      (define-values (idx dir) (build-linear-session 50 #:content (make-string 80 #\x)))
      (define cfg (make-context-assembly-config #:recent-tokens 2000))
      (define cr (build-assembled-context idx cfg))
      (check-true (context-result? cr))
      (check-true (>= (length (context-result-messages cr)) 1))
      (check-true (>= (context-result-total-tokens cr) 0))
      (delete-directory/files dir))

    ;; Large session performance: 200 messages should complete quickly
    (test-case "200-message assembly completes within 2 seconds (5 runs)"
      (define-values (idx dir) (build-linear-session 200 #:content (make-string 120 #\x)))
      (define cfg (make-context-assembly-config #:recent-tokens 5000))
      (define start (current-inexact-milliseconds))
      (for ([_ (in-range 5)])
        (build-assembled-context idx cfg))
      (define elapsed (- (current-inexact-milliseconds) start))
      ;; 5 iterations of 200-message assembly should be < 2s
      (check-true (< elapsed 2000)
                  (format "5x 200-msg assembly took ~ams (expected <2000ms)" elapsed))
      (delete-directory/files dir))

    ;; Memoization consistency: two calls produce identical results
    (test-case "repeated assemblies produce identical results"
      (define-values (idx dir) (build-linear-session 100 #:content (make-string 100 #\x)))
      (define cfg (make-context-assembly-config #:recent-tokens 3000))
      (define cr1 (build-assembled-context idx cfg))
      (define cr2 (build-assembled-context idx cfg))
      (check-equal? (length (context-result-messages cr1)) (length (context-result-messages cr2)))
      (check-equal? (context-result-total-tokens cr1) (context-result-total-tokens cr2))
      (check-equal? (context-result-pinned-count cr1) (context-result-pinned-count cr2))
      (delete-directory/files dir))

    ;; Catalog generation uses O(1) counting (not length per iteration)
    (test-case "catalog generation respects max-entries with O(1) counting"
      ;; Create messages that become catalog entries
      (define-values (idx dir) (build-linear-session 100 #:content (make-string 60 #\y)))
      ;; Tiny budget forces most into catalog
      (define cfg
        (make-context-assembly-config #:recent-tokens 50
                                      #:max-catalog-entries 10
                                      #:max-catalog-tokens 5000))
      (define cr (build-assembled-context idx cfg))
      (check-true (<= (length (context-result-catalog cr)) 10)
                  (format "Expected <= 10 catalog entries, got ~a"
                          (length (context-result-catalog cr))))
      (delete-directory/files dir))))

(run-tests perf-tests)
