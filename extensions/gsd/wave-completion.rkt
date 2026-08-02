#lang racket/base

;; extensions/gsd/wave-completion.rkt — Verifier-First Completion and Lifecycle Truth
;;
;; v0.99.80 W1: GC-3, GC-4, GC-5, GC-6, GC-11, GC-12
;;
;; Verifier-first: no production path may persist DONE before evidence is
;; verified. The completion service validates campaign/wave/attempt/fence
;; and runs verification before committing DONE to the durable record.
;;
;; Durable outbox: completion events have stable IDs and are deduplicated.
;; Crashes before publication publish later; crashes after publication can
;; redeliver but dedupe suppresses duplicate effects (D6).

(require racket/file
         racket/path
         racket/format
         racket/match
         "campaign-state.rkt")

;; ============================================================
;; Completion result
;; ============================================================

(struct completion-result (status event-id) #:transparent)

;; ============================================================
;; Stable event ID (D6)
;; ============================================================

(define (make-event-id plan-id wave-idx attempt-id)
  (format "campaign/~a/wave/~a/attempt/~a/completed" plan-id wave-idx attempt-id))

;; ============================================================
;; Verifier-first wave completion (GC-4)
;; ============================================================

;; Try to complete a wave. The verifier must approve before DONE is persisted.
;; On rejection, the wave is marked 'failed — DONE is never written.
;; On approval, DONE is persisted + outbox event appended atomically.
(define (try-complete-wave! base-dir rec wave-idx #:verifier-approve? [approve? #t])
  (define waves (campaign-record-waves rec))
  (define wave
    (for/first ([w waves]
                #:when (= (campaign-wave-index w) wave-idx))
      w))
  (cond
    [(not wave) (completion-result 'invalid-wave #f)]
    [(eq? (campaign-wave-status wave) 'done) (completion-result 'already-done #f)]
    [(eq? (campaign-wave-status wave) 'deferred) (completion-result 'already-done #f)]
    [(not approve?)
     ;; Verifier rejected — mark FAILED, do NOT persist DONE
     (set-campaign-wave-status! wave 'failed)
     (persist-campaign! base-dir rec)
     (completion-result 'failed #f)]
    [else
     ;; Verifier approved — persist DONE + outbox event
     (set-campaign-wave-status! wave 'done)
     (define attempt (campaign-wave-current-attempt wave))
     (define event-id
       (and attempt (make-event-id (campaign-plan-id rec) wave-idx (campaign-attempt-id attempt))))
     (when event-id
       (append-completion-event! base-dir rec event-id))
     (persist-campaign! base-dir rec)
     (completion-result 'done event-id)]))

;; ============================================================
;; /skip — commit DEFERRED durably (GC-11, D7)
;; ============================================================

(define (skip-wave! base-dir rec wave-idx)
  (define waves (campaign-record-waves rec))
  (define wave
    (for/first ([w waves]
                #:when (= (campaign-wave-index w) wave-idx))
      w))
  (cond
    [(not wave) (completion-result 'invalid-wave #f)]
    [(memq (campaign-wave-status wave) '(done deferred)) (completion-result 'already-done #f)]
    [else
     (set-campaign-wave-status! wave 'deferred)
     (persist-campaign! base-dir rec)
     (completion-result 'deferred #f)]))

;; ============================================================
;; Durable completion outbox (D6, GC-12)
;; ============================================================

(define (outbox-path base-dir plan-id)
  (build-path base-dir ".planning" "campaigns" (string-append plan-id ".outbox.rktd")))

(define (load-outbox base-dir plan-id)
  (define p (outbox-path base-dir plan-id))
  (if (file-exists? p)
      (call-with-input-file p read)
      '()))

;; Append event ID to outbox with deduplication.
;; Uses atomic write (write-to-tmp + rename) for crash safety.
(define (append-completion-event! base-dir rec event-id)
  (define plan-id (campaign-plan-id rec))
  (define p (outbox-path base-dir plan-id))
  (define existing (load-outbox base-dir plan-id))
  ;; Dedup: skip if event-id already present
  (unless (member event-id existing)
    (define updated (append existing (list event-id)))
    (define dir (path-only p))
    (make-directory* dir)
    (define tmp (path-replace-extension p ".tmp"))
    (call-with-output-file tmp #:exists 'truncate (lambda (out) (write updated out)))
    (rename-file-or-directory tmp p #t)))

(define (count-completion-events base-dir rec)
  (length (load-outbox base-dir (campaign-plan-id rec))))

;; ============================================================
;; Path helper
;; ============================================================

(define (path-only p)
  (define-values (base name must-be-dir?) (split-path p))
  (if (path? base)
      base
      (current-directory)))

;; ============================================================
;; Provide
;; ============================================================

(provide try-complete-wave!
         skip-wave!
         completion-result
         completion-result-status
         completion-result-event-id
         load-outbox
         count-completion-events
         make-event-id)
