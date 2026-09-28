#lang racket/base

;; tests/helpers/honest-delivery-fixture.rkt — TEST-ONLY operator stand-in for
;; the BUG-0077 honest completion state machine (awaiting-delivery → DONE).
;;
;; A verifier-approved attempt parks durably in 'awaiting-delivery with a
;; pending handoff. In production only an exact authenticated controller
;; readback plus the attempt-bound Verify journal finalizes DONE. These
;; helpers model exactly that operator step for tests whose subject REQUIRES
;; durable completion. They are NOT production proof, must never be seeded
;; into rejection/failure/refusal tests, and never substitute for the
;; controller readback outside a test process.
;;
;; STABILITY: internal test-support module (tests/helpers/* is excluded from
;; test discovery).

(require racket/list
         (only-in "../../extensions/gsd/campaign-state.rkt"
                  campaign-plan-id
                  campaign-fence-token
                  campaign-record-waves
                  campaign-wave-index
                  campaign-wave-status
                  campaign-wave-current-attempt
                  campaign-wave-delivery-branch
                  campaign-attempt-id
                  campaign-attempt-fence-token
                  set-campaign-wave-delivery-branch!
                  set-campaign-wave-delivery-head-sha!)
         (only-in "../../extensions/gsd/campaign-repository.rkt"
                  load-campaign-record
                  persist-campaign!)
         (only-in "../../extensions/gsd/delivery-journal.rkt"
                  record-delivery-receipt!
                  update-delivery-journal!)
         (only-in "../../extensions/gsd/delivery-handoff.rkt"
                  persist-delivery-handoff!
                  reconcile-delivered-handoff!)
         (only-in "../../extensions/gsd/go-orchestrator.rkt"
                  run-campaign!
                  campaign-result
                  campaign-result-status
                  campaign-result-message
                  campaign-result-completed-waves))

(provide awaiting-test-merge-sha
         test-delivered-reader
         seed-awaiting-delivery!
         seed-all-awaiting!
         call-with-delivery-rounds!
         finalize-awaiting!)

;; Fixed 40-hex stand-in for the authenticated protected merge SHA.
(define (awaiting-test-merge-sha)
  (make-string 40 #\c))

;; Delivered-shaped reader for tests that inject the controller readback.
(define (test-delivered-reader)
  (lambda (_base plan idx)
    (hasheq 'status "delivered" 'plan-id plan 'wave idx 'merge-sha (awaiting-test-merge-sha))))

;; Complete the operator-side protected delivery for ONE wave: bind an
;; attempt-fenced Verify receipt, mirror the branch/head provenance into the
;; durable wave, persist the pending handoff, reconcile it to delivered with
;; the merge SHA, and advance the coordinator journal to terminal stage.
;; Works for VERIFYING and AWAITING-DELIVERY waves (both carry the attempt).
;; Returns the merge SHA, or #f when the wave has no current attempt.
(define (seed-awaiting-delivery! dir
                                 plan-id
                                 idx
                                 #:merge-sha [merge-sha (awaiting-test-merge-sha)]
                                 #:branch [branch "campaign/test"])
  (define rec (load-campaign-record dir plan-id))
  (define wave
    (for/first ([w (in-list (campaign-record-waves rec))]
                #:when (= (campaign-wave-index w) idx))
      w))
  (define attempt (and wave (campaign-wave-current-attempt wave)))
  (cond
    [(or (not rec) (not wave) (not attempt)) #f]
    [else
     (define head (make-string 40 #\a))
     (record-delivery-receipt! dir
                               plan-id
                               idx
                               (hasheq 'repo
                                       "git@github.com:example/q.git"
                                       'branch
                                       branch
                                       'origin
                                       "https://github.com/example/q.git"
                                       'evidence
                                       "docs/reports/gsd-wave-evidence/fixture.rktd"
                                       'head
                                       head
                                       'tree
                                       (make-string 40 #\b)
                                       'attempt-id
                                       (campaign-attempt-id attempt)
                                       'attempt-fence
                                       (campaign-attempt-fence-token attempt)
                                       'verified-at
                                       42))
     (set-campaign-wave-delivery-branch! wave branch)
     (set-campaign-wave-delivery-head-sha! wave head)
     (persist-campaign! dir rec)
     (persist-delivery-handoff! dir plan-id wave "test precondition")
     (reconcile-delivered-handoff! dir plan-id idx merge-sha)
     (update-delivery-journal! dir plan-id idx (hasheq 'stage "delivered"))
     merge-sha]))

;; Seed every currently-awaiting wave. Returns the sorted seeded indices
;; (empty when no wave is awaiting — no synthetic progress).
(define (seed-all-awaiting! dir plan-id)
  (define rec (load-campaign-record dir plan-id))
  (define indices
    (sort (for/list ([w (in-list (campaign-record-waves rec))]
                     #:when (eq? (campaign-wave-status w) 'awaiting-delivery))
            (campaign-wave-index w))
          <))
  (for ([idx (in-list indices)])
    (seed-awaiting-delivery! dir plan-id idx))
  indices)

;; Drive a /go-shaped thunk the way an operator would across delivery
;; boundaries: each invocation stops honestly at awaiting delivery; the
;; operator completes the protected merge; the next invocation finalizes
;; WITHOUT rerunning. Union completed waves across rounds so the returned
;; campaign-complete result reports every completed wave exactly once.
(define (call-with-delivery-rounds! dir plan-id go [max-rounds 8])
  (let loop ([rounds max-rounds]
             [completed '()])
    (define result (go))
    (define all (remove-duplicates (append completed (campaign-result-completed-waves result))))
    (cond
      [(and (> rounds 1)
            (eq? (campaign-result-status result) 'wave-blocked)
            (pair? (seed-all-awaiting! dir plan-id)))
       (loop (sub1 rounds) all)]
      [(eq? (campaign-result-status result) 'campaign-complete)
       (campaign-result 'campaign-complete all (campaign-result-message result))]
      [else result])))

;; Finalize awaiting delivery for tests that only need the DONE surface:
;; run-campaign! with a runner that MUST never be invoked (a verified
;; implementation is never rerun) and the delivered-shaped reader.
(define (finalize-awaiting! dir rec #:delivery-reader [reader (test-delivered-reader)])
  (define plan-id (campaign-plan-id rec))
  (seed-all-awaiting! dir plan-id)
  (call-with-delivery-rounds!
   dir
   plan-id
   (lambda ()
     (run-campaign! dir
                    rec
                    #:runner (lambda (_)
                               (error 'finalize-awaiting!
                                      "verified implementation reran during finalization"))
                    #:verifier (lambda (_) #t)
                    #:delivery-reader reader))))
