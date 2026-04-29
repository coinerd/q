#lang racket/base

;; extensions/gsd/wave-executor.rkt — Wave execution engine with error recovery
;; STABILITY: evolving
;;
;; Wave 2b of v0.21.0: Tracks wave status through execution lifecycle.
;; DD-5: Wave-level error recovery — failed waves skip, don't abort.
;;
;; Lifecycle: pending → in-progress → completed | failed | skipped
;; Failed waves do NOT block subsequent waves.

(require racket/format
         racket/string
         racket/file
         racket/port
         "plan-types.rkt"
         "../gsd/wave-docs.rkt")

(provide wave-status
         wave-status?
         wave-status-index
         wave-status-state
         wave-status-error-message
         wave-status-attempt-count
         wave-status-timestamp
         make-wave-executor
         load-plan-from-index
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

;; ============================================================
;; Load plan from disk (PLAN.md index + wave docs)

;; ============================================================
;; Load plan from disk (PLAN.md index + wave docs)
;; ============================================================

(define (load-plan-from-index base-dir)
  (define plan-path (build-path base-dir ".planning" "PLAN.md"))
  (if (not (file-exists? plan-path))
      #f
      (let* ([text (call-with-input-file plan-path port->string)]
             [entries (parse-plan-index text)])
        (if (null? entries)
            #f
            (let* ([title (extract-plan-title text)]
                   [waves (for/list ([e entries])
                            (define idx (wave-index-entry-idx e))
                            (define slug (wave-index-entry-slug e))
                            (define wave-data (read-wave-doc base-dir idx slug))
                            (define wave-content
                              (if wave-data
                                  (hash-ref wave-data 'content)
                                  ""))
                            (gsd-wave idx
                                      (wave-index-entry-title e)
                                      (string->wave-status-from-entry e)
                                      wave-content
                                      (extract-files-from-content wave-content)
                                      '()
                                      (extract-verify-from-content wave-content)
                                      ""))])
              (gsd-plan waves '() '() '()))))))

(define (extract-plan-title text)
  (define lines (string-split text "\n"))
  (for/first ([line lines]
              #:when (string-prefix? line "# Plan:"))
    (string-trim (substring line 7))))

(define (string->wave-status-from-entry e)
  (define s (wave-index-entry-status e))
  (cond
    [(string=? s "DONE") 'completed]
    [(string=? s "FAILED") 'failed]
    [(string=? s "DEFERRED") 'skipped]
    [(string=? s "In-Progress") 'in-progress]
    [else 'pending]))

(define (extract-files-from-content content)
  (hash-ref (parse-wave-content content) 'files '()))

(define (extract-verify-from-content content)
  (hash-ref (parse-wave-content content) 'verify ""))
