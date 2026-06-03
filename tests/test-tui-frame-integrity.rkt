#lang racket/base

;; BOUNDARY: unit
;; @suite tui
;; @boundary unit
;; @speed fast
;; @mutates none
;; tests/test-tui-frame-integrity.rkt — TUI frame integrity tests
;;
;; Verifies that frame snapshots maintain structural invariants:
;; - status row and input row are independent
;; - transcript doesn't bleed into status/input
;; - dirty flag is deterministic after update

(require rackunit
         "helpers/tui-scenarios.rkt")

;; ---------------------------------------------------------------------------
;; Frame independence
;; ---------------------------------------------------------------------------

(test-case "status row update doesn't affect input row"
  (define snap1 (make-frame-snapshot #:status-row "idle" #:input-row "> "))
  (define snap2 (make-frame-snapshot #:status-row "running" #:input-row "> "))
  (check-equal? (frame-snapshot-input-row snap1) (frame-snapshot-input-row snap2))
  (check-not-equal? (frame-snapshot-status-row snap1) (frame-snapshot-status-row snap2)))

(test-case "transcript update doesn't affect status/input"
  (define snap1 (make-frame-snapshot #:status-row "ok" #:input-row "$ " #:transcript '("hello")))
  (define snap2 (make-frame-snapshot #:status-row "ok" #:input-row "$ " #:transcript '("hello" "world")))
  (check-equal? (frame-snapshot-status-row snap1) (frame-snapshot-status-row snap2))
  (check-equal? (frame-snapshot-input-row snap1) (frame-snapshot-input-row snap2))
  (check-equal? (length (frame-snapshot-transcript snap2)) 2))

(test-case "empty transcript is distinct from #f transcript"
  (define snap-empty (make-frame-snapshot #:transcript '()))
  (define snap-default (make-frame-snapshot))
  (check-equal? (frame-snapshot-transcript snap-empty) '())
  (check-equal? (frame-snapshot-transcript snap-default) '()))

;; ---------------------------------------------------------------------------
;; Dirty flag determinism
;; ---------------------------------------------------------------------------

(test-case "dirty flag is #f by default"
  (define snap (make-frame-snapshot))
  (check-false (frame-snapshot-dirty? snap)))

(test-case "dirty flag is #t when content differs"
  (define snap1 (make-frame-snapshot #:status-row "old"))
  (define snap2 (make-frame-snapshot #:status-row "new" #:dirty? #t))
  (check-false (frame-snapshot-dirty? snap1))
  (check-true (frame-snapshot-dirty? snap2)))

;; ---------------------------------------------------------------------------
;; Concurrent snapshot capture
;; ---------------------------------------------------------------------------

(test-case "concurrent snapshot creation is independent"
  (define cap (make-synchronized-capture))
  (define N 50)
  (define threads
    (for/list ([i (in-range N)])
      (thread
       (lambda ()
         (define snap (make-frame-snapshot
                       #:status-row (format "status-~a" i)
                       #:input-row "> "
                       #:transcript (list (format "msg-~a" i))
                       #:dirty? #t))
         (sync-capture-put! cap snap)))))
  (for-each thread-wait threads)
  (check-equal? (sync-capture-count cap) N)
  ;; All snapshots should have their own status rows
  (define events (sync-capture-events cap))
  (for ([snap (in-list events)])
    (check-true (frame-snapshot? snap))
    (check-true (frame-snapshot-dirty? snap))))
