#lang racket/base

;; extensions/gsd/go-orchestrator.rkt — Single-Wave Campaign Coordinator
;;
;; v0.99.80 W2: GC-1, GC-6, GC-7, GC-9, GC-13
;;
;; D1 — Coordinator loop is OUTSIDE run-prompt!:
;;   acquire campaign lease
;;   while earliest actionable wave exists:
;;       persist IN-PROGRESS + fresh attempt/fence
;;       result := run-wave!(single-wave-prompt)
;;       wait for run-wave! return
;;       if cancelled/error/timeout: persist INTERRUPTED/FAILED; stop
;;       persist VERIFYING
;;       verification := verify current attempt evidence
;;       if rejected: persist FAILED; stop
;;       commit DONE + completion outbox record
;;       deliver completion event idempotently
;;   release campaign lease

(require racket/format
         racket/file
         racket/match
         "campaign-state.rkt"
         "wave-completion.rkt")

;; ============================================================
;; Lease (D5: process-safe OS advisory lock)
;; ============================================================

(struct campaign-lease (path port owner-pid owner-session) #:mutable)

(define (lease-path base-dir plan-id)
  (build-path base-dir ".planning" "campaigns" (string-append plan-id ".lock")))

(define (acquire-lease base-dir plan-id #:session-id [session-id "unknown"])
  (define p (lease-path base-dir plan-id))
  (define-values (dir _ __) (split-path p))
  (make-directory* dir)
  (with-handlers ([exn:fail:filesystem? (lambda (_) #f)])
    (define port (open-output-file p #:exists 'error))
    (write (hasheq 'pid (current-seconds) 'session session-id 'acquired (current-seconds)) port)
    (flush-output port)
    (campaign-lease p port (current-seconds) session-id)))

(define (release-lease! lease)
  (when (and lease (campaign-lease? lease))
    (with-handlers ([exn:fail? void])
      (close-output-port (campaign-lease-port lease))
      (delete-file (campaign-lease-path lease)))))

;; ============================================================
;; Coordinator result
;; ============================================================

(struct campaign-result (status completed-waves message) #:transparent)

;; ============================================================
;; Wave runner abstraction (injectable for testing)
;; ============================================================

(define default-runner (lambda (wave-idx) 'ok))
(define default-verifier (lambda (wave-idx) #t))

;; ============================================================
;; Find wave helper
;; ============================================================

(define (find-wave rec wave-idx)
  (for/first ([w (campaign-record-waves rec)]
              #:when (= (campaign-wave-index w) wave-idx))
    w))

;; ============================================================
;; Single-wave campaign coordinator (D1)
;; ============================================================

(define (run-campaign-wave base-dir
                           rec
                           wave-idx
                           #:runner [runner default-runner]
                           #:verifier [verifier default-verifier]
                           #:fence-token [fence 1])
  ;; Step 1: begin attempt (D5 fencing, one-active-wave invariant)
  (begin-attempt! rec wave-idx fence)
  (persist-campaign! base-dir rec)

  ;; Step 2: run the wave (one prompt, one wave)
  (define run-result (runner wave-idx))

  ;; Step 3: handle runner outcome
  (case run-result
    [(ok)
     (define result
       (try-complete-wave! base-dir rec wave-idx #:verifier-approve? (verifier wave-idx)))
     (case (completion-result-status result)
       [(done) (campaign-result 'wave-done (list wave-idx) "wave completed")]
       [(failed) (campaign-result 'wave-failed '() "verifier rejected")]
       [else (campaign-result 'wave-failed '() "unexpected completion state")])]
    [(error)
     (define w (find-wave rec wave-idx))
     (when w
       (set-campaign-wave-status! w 'failed))
     (persist-campaign! base-dir rec)
     (campaign-result 'wave-failed '() "runner error")]
    [(cancelled)
     (define w (find-wave rec wave-idx))
     (when w
       (set-campaign-wave-status! w 'interrupted))
     (persist-campaign! base-dir rec)
     (campaign-result 'wave-cancelled '() "runner cancelled")]
    [else (campaign-result 'wave-failed '() "unknown runner result")]))

;; ============================================================
;; Full campaign execution (loop one wave at a time)
;; ============================================================

(define (run-campaign! base-dir
                       rec
                       #:runner [runner default-runner]
                       #:verifier [verifier default-verifier])
  (define plan-id (campaign-plan-id rec))
  (define lease (acquire-lease base-dir plan-id))
  (if (not lease)
      (campaign-result 'busy '() "campaign lease held by another process")
      (dynamic-wind
       void
       (lambda ()
         (let loop ([completed '()]
                    [fence 1])
           (define next-idx (select-next-actionable-wave rec))
           (cond
             [(not next-idx)
              (campaign-result 'campaign-complete (reverse completed) "all waves done or deferred")]
             [else
              (define result
                (run-campaign-wave base-dir
                                   rec
                                   next-idx
                                   #:runner runner
                                   #:verifier verifier
                                   #:fence-token fence))
              (case (campaign-result-status result)
                [(wave-done) (loop (cons next-idx completed) (add1 fence))]
                [(wave-failed wave-cancelled)
                 (campaign-result (campaign-result-status result)
                                  (reverse completed)
                                  (campaign-result-message result))]
                [else
                 (campaign-result 'error (reverse completed) "unexpected coordinator state")])])))
       (lambda () (release-lease! lease)))))

;; ============================================================
;; /go N assertion (D8)
;; ============================================================

(define (assert-go-n rec n)
  (define next (select-next-actionable-wave rec))
  (and next (= n next)))

;; ============================================================
;; Provide
;; ============================================================

(provide campaign-lease
         campaign-lease?
         acquire-lease
         release-lease!
         campaign-result
         campaign-result-status
         campaign-result-completed-waves
         campaign-result-message
         run-campaign-wave
         run-campaign!
         assert-go-n)
