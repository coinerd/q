#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: unit
;; @suite tui
;; @boundary unit
;; @speed fast
;; @mutates none
;; tests/helpers/tui-scenarios.rkt — TUI event pipeline and concurrency helpers
;;
;; Provides:
;; - Synchronized event capture for testing async event publication
;; - Channel-based interleaving for deterministic concurrency tests
;; - Frame snapshot capture for rendering integrity checks

(require racket/match
         "../../agent/event-bus.rkt")

(provide make-synchronized-capture
         sync-capture-events
         sync-capture-put!
         sync-capture-wait
         sync-capture-count
         
         make-channel-pair
         channel-pair-send
         channel-pair-receive
         
         make-frame-snapshot
         frame-snapshot?
         frame-snapshot-status-row
         frame-snapshot-input-row
         frame-snapshot-transcript
         frame-snapshot-dirty?)

;; ---------------------------------------------------------------------------
;; Synchronized event capture (thread-safe, no sleeps)
;; ---------------------------------------------------------------------------

(struct sync-capture (sema events-box) #:transparent)

(define (make-synchronized-capture)
  (sync-capture (make-semaphore 1) (box '())))

(define (sync-capture-events cap)
  (call-with-semaphore (sync-capture-sema cap)
    (lambda () (reverse (unbox (sync-capture-events-box cap))))))

(define (sync-capture-put! cap evt)
  (call-with-semaphore (sync-capture-sema cap)
    (lambda () (set-box! (sync-capture-events-box cap)
                         (cons evt (unbox (sync-capture-events-box cap)))))))

(define (sync-capture-wait cap expected-count #:timeout [timeout-ms 5000])
  ;; Spin until we have expected-count events or timeout
  (define deadline (+ (current-inexact-milliseconds) timeout-ms))
  (let loop ()
    (when (and (< (sync-capture-count cap) expected-count)
               (< (current-inexact-milliseconds) deadline))
      (sleep 0.001)
      (loop))))

(define (sync-capture-count cap)
  (call-with-semaphore (sync-capture-sema cap)
    (lambda () (length (unbox (sync-capture-events-box cap))))))

;; ---------------------------------------------------------------------------
;; Channel pair for deterministic interleaving
;; ---------------------------------------------------------------------------

(struct channel-pair (send-ch recv-ch) #:transparent)

(define (make-channel-pair)
  (channel-pair (make-channel) (make-channel)))

(define (channel-pair-send cp val)
  (channel-put (channel-pair-send-ch cp) val))

(define (channel-pair-receive cp)
  (channel-get (channel-pair-recv-ch cp)))

;; ---------------------------------------------------------------------------
;; Frame snapshot (terminal-independent)
;; ---------------------------------------------------------------------------

(struct frame-snapshot
  (status-row   ; (or/c string? #f)
   input-row    ; (or/c string? #f)
   transcript   ; (listof string?)
   dirty?)      ; boolean?
  #:transparent)

(define (make-frame-snapshot #:status-row [status-row #f]
                              #:input-row [input-row #f]
                              #:transcript [transcript '()]
                              #:dirty? [dirty? #f])
  (frame-snapshot status-row input-row transcript dirty?))
