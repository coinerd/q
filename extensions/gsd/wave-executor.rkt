#lang racket/base

;; extensions/gsd/wave-executor.rkt — Wave execution engine with error recovery
;;
;; Wave 2b of v0.21.0: Tracks wave status through execution lifecycle.
;; DD-5: Wave-level error recovery — failed waves skip, don't abort.
;;
;; Lifecycle: pending → in-progress → completed | failed | skipped
;; Failed waves do NOT block subsequent waves.

(require racket/format
         racket/string
         "plan-types.rkt")

(provide wave-status
         wave-status?
         wave-status-index
         wave-status-state
         wave-status-error-message
         wave-status-attempt-count
         wave-status-timestamp
         make-wave-executor
         wave-start!
         wave-complete!
         wave-fail!
         wave-skip!
         next-pending-wave
         wave-summary
         all-waves-done?
         ;; Exposed for testing
         wave-executor-statuses
         wave-executor-plan)

;; ============================================================
;; Wave status struct
;; ============================================================

(struct wave-status (index state error-message attempt-count timestamp) #:transparent)

;; ============================================================
;; Wave executor (mutable box wrapping plan + statuses)
;; ============================================================

(struct wave-executor (plan statuses-box) #:transparent)

(define (wave-executor-statuses exec)
  (unbox (wave-executor-statuses-box exec)))

(define (set-executor-statuses! exec statuses)
  (set-box! (wave-executor-statuses-box exec) statuses))

;; ============================================================
;; Constructor
;; ============================================================

(define (make-wave-executor plan)
  (define waves (gsd-plan-waves plan))
  (define initial-statuses
    (for/list ([w waves])
      (wave-status (gsd-wave-index w) 'pending #f 0 (current-seconds))))
  (wave-executor plan (box initial-statuses)))

;; ============================================================
;; Status transitions
;; ============================================================

(define (update-status! exec idx update-fn)
  (define statuses (wave-executor-statuses exec))
  (define new-statuses
    (for/list ([s statuses])
      (if (= (wave-status-index s) idx)
          (update-fn s)
          s)))
  (set-executor-statuses! exec new-statuses))

(define (wave-start! exec idx)
  (update-status!
   exec
   idx
   (lambda (s)
     (wave-status idx 'in-progress #f (add1 (wave-status-attempt-count s)) (current-seconds)))))

(define (wave-complete! exec idx)
  (update-status! exec
                  idx
                  (lambda (s)
                    (wave-status idx 'completed #f (wave-status-attempt-count s) (current-seconds)))))

(define (wave-fail! exec idx error-message)
  (update-status!
   exec
   idx
   (lambda (s)
     (wave-status idx 'failed error-message (wave-status-attempt-count s) (current-seconds)))))

(define (wave-skip! exec idx)
  (update-status! exec
                  idx
                  (lambda (s)
                    (wave-status idx 'skipped #f (wave-status-attempt-count s) (current-seconds)))))

;; ============================================================
;; Queries
;; ============================================================

(define (next-pending-wave exec)
  (define statuses (wave-executor-statuses exec))
  (define pending (filter (lambda (s) (eq? (wave-status-state s) 'pending)) statuses))
  (if (null? pending)
      #f
      (wave-status-index (car pending))))

(define (all-waves-done? exec)
  (define statuses (wave-executor-statuses exec))
  (for/and ([s statuses])
    (and (memq (wave-status-state s) '(completed failed skipped)) #t)))

(define (wave-summary exec)
  (define statuses (wave-executor-statuses exec))
  (define by-state
    (for/fold ([acc (hasheq)]) ([s statuses])
      (hash-update acc (wave-status-state s) add1 0)))
  (define n-completed (hash-ref by-state 'completed 0))
  (define n-failed (hash-ref by-state 'failed 0))
  (define n-skipped (hash-ref by-state 'skipped 0))
  (define n-pending (hash-ref by-state 'pending 0))
  (define n-in-progress (hash-ref by-state 'in-progress 0))
  (define total (length statuses))
  (define parts
    (filter values
            (list (format "Total: ~a waves" total)
                  (if (> n-completed 0)
                      (format "✅ Completed: ~a" n-completed)
                      #f)
                  (if (> n-failed 0)
                      (format "❌ Failed: ~a" n-failed)
                      #f)
                  (if (> n-skipped 0)
                      (format "⏭  Skipped: ~a" n-skipped)
                      #f)
                  (if (> n-pending 0)
                      (format "⏳ Pending: ~a" n-pending)
                      #f)
                  (if (> n-in-progress 0)
                      (format "🔄 In Progress: ~a" n-in-progress)
                      #f))))
  (string-join parts "\n"))
