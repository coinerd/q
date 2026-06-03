#lang racket/base

;; BOUNDARY: unit
;; @suite tui
;; @boundary unit
;; @speed fast
;; @mutates none
;; tests/test-tui-event-pipeline-concurrency.rkt — TUI event pipeline concurrency tests
;;
;; Tests that event publication and capture are thread-safe using
;; semaphores (not sleep-based timing).

(require rackunit
         "helpers/tui-scenarios.rkt")

;; ---------------------------------------------------------------------------
;; Synchronized capture basics
;; ---------------------------------------------------------------------------

(test-case "sync-capture starts empty"
  (define cap (make-synchronized-capture))
  (check-equal? (sync-capture-events cap) '())
  (check-equal? (sync-capture-count cap) 0))

(test-case "sync-capture-put! adds event"
  (define cap (make-synchronized-capture))
  (sync-capture-put! cap 'evt-1)
  (check-equal? (sync-capture-events cap) '(evt-1))
  (check-equal? (sync-capture-count cap) 1))

(test-case "sync-capture preserves insertion order"
  (define cap (make-synchronized-capture))
  (sync-capture-put! cap 'a)
  (sync-capture-put! cap 'b)
  (sync-capture-put! cap 'c)
  (check-equal? (sync-capture-events cap) '(a b c)))

;; ---------------------------------------------------------------------------
;; Concurrent event publication
;; ---------------------------------------------------------------------------

(test-case "concurrent put! is thread-safe"
  (define cap (make-synchronized-capture))
  (define N 100)
  (define threads
    (for/list ([i (in-range N)])
      (thread (lambda () (sync-capture-put! cap i)))))
  (for-each thread-wait threads)
  (check-equal? (sync-capture-count cap) N))

(test-case "concurrent put! and read is thread-safe"
  (define cap (make-synchronized-capture))
  (define N 50)
  (define writer
    (thread
     (lambda ()
       (for ([i (in-range N)])
         (sync-capture-put! cap i)))))
  (define reader
    (thread
     (lambda ()
       (sync-capture-wait cap N #:timeout-ms 3000))))
  (thread-wait writer)
  (thread-wait reader)
  (check-equal? (sync-capture-count cap) N))

;; ---------------------------------------------------------------------------
;; Frame snapshot capture
;; ---------------------------------------------------------------------------

(test-case "frame-snapshot construction"
  (define snap (make-frame-snapshot #:status-row "ok" #:input-row "> " #:dirty? #t))
  (check-equal? (frame-snapshot-status-row snap) "ok")
  (check-equal? (frame-snapshot-input-row snap) "> ")
  (check-equal? (frame-snapshot-transcript snap) '())
  (check-true (frame-snapshot-dirty? snap)))

(test-case "frame-snapshot default values"
  (define snap (make-frame-snapshot))
  (check-false (frame-snapshot-status-row snap))
  (check-false (frame-snapshot-input-row snap))
  (check-false (frame-snapshot-dirty? snap)))

(test-case "frame-snapshot transcript capture"
  (define snap (make-frame-snapshot #:transcript '("line 1" "line 2" "line 3")))
  (check-equal? (frame-snapshot-transcript snap) '("line 1" "line 2" "line 3")))
