#lang racket/base

;; extensions/gsd/delivery-finalize.rkt — receipt-authoritative delivery
;; finalization and attempt provenance (BUG-0077).
;;
;; Verifier approval is implementation truth, not delivery truth. This module
;; owns the two halves of the honest completion state machine that used to
;; live inside the orchestrator (extracted to keep go-orchestrator.rkt under
;; its size pin):
;;
;;   record-attempt-delivery-provenance! — bind the verified attempt's REAL
;;     branch/head (attempt-bound Verify receipt, else the committed worktree
;;     branch/head) before the attempt parks. Never a guess, never a checkout
;;     HEAD, never an empty string (the v1.00.32 incident shape).
;;
;;   finalize-delivered-wave! — turn authenticated delivered proof into the
;;     durable DONE commit through try-complete-wave!. DONE requires the exact
;;     attempt, a real 40-hex merge SHA, a delivered handoff carrying that
;;     SHA, and a terminal, attempt-bound Verify journal. Anything else is a
;;     resumable pause, never a completion and never a failure.
;;
;; delivery-journal-advanced? — the coordinator progress test: a coordinator
;; may only re-enter the loop when its durable journal actually moved.

(require racket/list
         racket/string
         (only-in "campaign-state.rkt"
                  campaign-plan-id
                  campaign-record-waves
                  campaign-wave-index
                  campaign-wave-current-attempt
                  campaign-attempt-id
                  campaign-attempt-fence-token
                  campaign-record-cancellation
                  campaign-wave?
                  campaign-wave-attempt-context
                  set-campaign-wave-attempt-context!)
         (only-in "campaign-repository.rkt" load-campaign-record persist-campaign!)
         (only-in "delivery-journal.rkt" load-delivery-journal)
         (only-in "wave-completion.rkt"
                  try-complete-wave!
                  completion-result-status
                  completion-result-event-id)
         (only-in "attempt-artifacts.rkt"
                  record-wave-delivery!
                  wave-worktree-head-sha
                  warn-zero-commit-delivery-branch!
                  artifact-merge-status/local
                  mark-attempt-artifact-terminal!)
         (only-in "delivery-verifier.rkt" branch-delivery-context-ref)
         (only-in "delivery-coordinator.rkt"
                  delivery-outcome?
                  delivery-outcome-kind
                  delivery-outcome-message)
         (only-in "campaign-result.rkt" campaign-result)
         (only-in "delivery-handoff.rkt" delivery-readback)
         (only-in "reconciliation-checkpoint.rkt"
                  run-tracker-reconciliation!
                  reconciliation-checkpoint-result-status
                  reconciliation-checkpoint-result-actions)
         (only-in "tracker-production-wiring.rkt" resolve-live-tracker-reconciler))

(provide record-attempt-delivery-provenance!
         finalize-delivered-wave!
         delivery-finalization-done?
         delivery-finalization-paused?
         delivery-finalization-stale?
         delivery-finalization-failed?
         delivery-finalization-merge-sha
         delivery-journal-advanced?
         delivery-journal-snapshot
         finalize-checkpoint-decision
         coordinator-checkpoint-decision
         run-delivery-finalization
         run-coordinator-checkpoint
         park-approved-attempt-result!)

;; ============================================================
;; Attempt delivery provenance
;; ============================================================

;; Record the verified attempt's delivery branch/head. The attempt-bound
;; Verify receipt is authoritative; the committed worktree branch/head is the
;; honest fallback for the isolated path (it is what will be merged). Returns
;; the recorded (branch . head) or #f when no real provenance exists.
(define (record-attempt-delivery-provenance! base-dir
                                             plan-id
                                             wave-idx
                                             attempt-id
                                             attempt-fence
                                             #:worktree [worktree #f]
                                             #:delivery-context [delivery-context #f])
  (define journal
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (load-delivery-journal base-dir plan-id wave-idx)))
  (define receipt (and (hash? journal) (hash-ref journal 'receipt #f)))
  (define worktree-branch
    (and worktree delivery-context (branch-delivery-context-ref delivery-context 'branch)))
  (define worktree-head (and worktree (wave-worktree-head-sha worktree)))
  (cond
    [(and (hash? receipt)
          (equal? (hash-ref receipt 'attempt-id #f) attempt-id)
          (equal? (hash-ref receipt 'attempt-fence #f) attempt-fence)
          (string? (hash-ref receipt 'branch #f))
          (regexp-match? #px"^[0-9a-f]{40}$" (or (hash-ref receipt 'head #f) ""))
          (not (member (hash-ref receipt 'branch) '("main" "master"))))
     (record-wave-delivery! base-dir
                            plan-id
                            wave-idx
                            (hash-ref receipt 'branch)
                            (hash-ref receipt 'head))
     (cons (hash-ref receipt 'branch) (hash-ref receipt 'head))]
    [(and (string? worktree-branch)
          (positive? (string-length worktree-branch))
          (not (member worktree-branch '("main" "master")))
          (string? worktree-head)
          (regexp-match? #px"^[0-9a-f]{40}$" worktree-head))
     (record-wave-delivery! base-dir plan-id wave-idx worktree-branch worktree-head)
     (cons worktree-branch worktree-head)]
    [else #f]))

;; ============================================================
;; Finalization
;; ============================================================

;; Finalize one delivered wave from its parked 'awaiting-delivery attempt.
;; merge-sha is the memoized authenticated controller readback result for THIS
;; campaign/wave; #f (or any non-40-hex value) can never complete.
(define (finalize-delivered-wave! base-dir rec wave-idx merge-sha #:release-check [release-check #f])
  (define attempt
    (for/first ([w (in-list (campaign-record-waves rec))]
                #:when (= (campaign-wave-index w) wave-idx))
      (campaign-wave-current-attempt w)))
  (and attempt
       (try-complete-wave! base-dir
                           rec
                           wave-idx
                           #:verifier-approve? #t
                           #:expected-attempt-id (campaign-attempt-id attempt)
                           #:expected-fence-token (campaign-attempt-fence-token attempt)
                           #:delivered-merge-sha merge-sha
                           #:release-check release-check)))

(define (delivery-finalization-done? result)
  (and result (eq? (completion-result-status result) 'done)))

(define (delivery-finalization-failed? result)
  (and result (eq? (completion-result-status result) 'failed)))

(define (delivery-finalization-stale? result)
  (and result (memq (completion-result-status result) '(cancelled stale-attempt invalid-state))))

;; A refused finalization is a clean, resumable pause — NOT a wave failure.
(define (delivery-finalization-paused? result)
  (and result
       (not (delivery-finalization-done? result))
       (not (delivery-finalization-failed? result))
       (not (delivery-finalization-stale? result))))

(define (delivery-finalization-merge-sha result)
  (and result (completion-result-event-id result)))

;; ============================================================
;; Checkpoint decisions (the loop cases stay thin in the orchestrator)
;; ============================================================

;; Finalize one delivered wave and classify the outcome for the loop:
;;   'done   — durable DONE + completion event committed
;;   'stale  — cancellation / stale fence / invalid state; ignore
;;   'failed — the release gate refused; the wave is failed
;;   'paused — provenance did not authorize DONE; stay resumable
(define (finalize-checkpoint-decision base-dir rec wave-idx merge-sha release-check)
  (define result
    (finalize-delivered-wave! base-dir rec wave-idx merge-sha #:release-check release-check))
  (cond
    [(delivery-finalization-done? result) 'done]
    [(delivery-finalization-stale? result) 'stale]
    [(delivery-finalization-failed? result) 'failed]
    [else 'paused]))

;; Classify one coordinator step:
;;   'advance   — durable journal moved; re-enter the loop
;;   'cancelled — campaign cancellation landed during the effect
;;   'blocked   — typed stop, or progress claimed without a durable transition
(define (coordinator-checkpoint-decision outcome-kind before after cancelled)
  (cond
    [cancelled 'cancelled]
    [(eq? outcome-kind 'ok) (if (delivery-journal-advanced? before after) 'advance 'blocked)]
    [else 'blocked]))

;; ============================================================
;; Coordinator progress
;; ============================================================

;; The coordinator's typed 'ok authorizes a loop re-entry only when its
;; durable journal actually advanced. A coordinator that reports progress
;; without a durable transition must not spin the campaign forever.
(define (delivery-journal-advanced? before after)
  (and (hash? after) (not (equal? before after))))

;; Durable journal snapshot for the coordinator progress test (#f when absent
;; or unreadable — a missing journal is never a durable transition).
(define (delivery-journal-snapshot base-dir plan-id wave-idx)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (load-delivery-journal base-dir plan-id wave-idx)))

;; ============================================================
;; Checkpoint drivers (loop bookkeeping stays in the orchestrator)
;; ============================================================

;; Finalize the parked attempt for wave-idx from the orchestrator's checkpoint.
;; on-delivered performs the durable DONE bookkeeping (mirror, notify, loop);
;; every non-delivered outcome is returned as a campaign result here so the
;; orchestrator's case body stays three lines.
(define (run-delivery-finalization base-dir
                                   plan-id
                                   rec
                                   wave-idx
                                   merge-sha
                                   completed
                                   #:release-check [release-check #f]
                                   #:on-delivered [on-delivered (lambda () #f)]
                                   #:tracker-reconciler [tracker-reconciler #f]
                                   #:delivery-reader [delivery-reader delivery-readback])
  (case (finalize-checkpoint-decision base-dir rec wave-idx merge-sha release-check)
    [(done)
     ;; v1.00.33 W0 (BUG-0074): the wave is provably `done` NOW, so the
     ;; tracker may be reconciled from its receipt. Deliberately BEFORE
     ;; on-delivered and deliberately non-fatal: reconciliation is an output
     ;; of delivery, so its refusal can never move this wave off `done`.
     (define tracker-result
       (run-tracker-reconciliation! base-dir
                                    plan-id
                                    wave-idx
                                    (or tracker-reconciler
                                        (resolve-live-tracker-reconciler base-dir plan-id wave-idx))
                                    #:delivery-reader delivery-reader))
     ;; Do not conflate wave delivery and tracker delivery. This warning makes
     ;; a blocked or partially applied tracker pass visible to the operator;
     ;; the wave remains DONE and the exact actions can be inspected separately.
     (unless (eq? (reconciliation-checkpoint-result-status tracker-result) 'reconciled)
       (log-warning
        "tracker reconciliation ~a for plan ~a wave ~a (~a completed action(s)); wave remains delivered"
        (reconciliation-checkpoint-result-status tracker-result)
        plan-id
        wave-idx
        (length (reconciliation-checkpoint-result-actions tracker-result))))
     (on-delivered)]
    [(stale)
     (campaign-result 'wave-cancelled (reverse completed) "stale delivery completion ignored")]
    [(failed) (campaign-result 'wave-failed (reverse completed) "release verification failed")]
    [else
     (campaign-result 'wave-blocked
                      (reverse completed)
                      "delivery provenance changed during finalization")]))

;; Run one delivery-coordinator step and classify it. A typed 'ok re-enters the
;; loop ONLY when the durable delivery journal actually moved; a coordinator
;; that claims progress without a durable transition is a clean, resumable
;; block (never an infinite spin, never a false DONE).
(define (run-coordinator-checkpoint dir
                                    plan-id
                                    extra
                                    message
                                    delivery-coordinator
                                    #:on-cancel [on-cancel (lambda (_) #f)]
                                    #:on-advance [on-advance (lambda () #f)]
                                    #:on-blocked [on-blocked (lambda (_) #f)])
  (if (and extra (campaign-record-cancellation (load-campaign-record dir plan-id)))
      (on-cancel message)
      (let* ([idx (and (campaign-wave? extra) (campaign-wave-index extra))]
             [before (and idx (delivery-journal-snapshot dir plan-id idx))]
             [outcome (delivery-coordinator dir plan-id idx)]
             [after (and idx (delivery-journal-snapshot dir plan-id idx))]
             [kind (and outcome (delivery-outcome-kind outcome))]
             [decision (coordinator-checkpoint-decision
                        kind
                        before
                        after
                        (and extra
                             (campaign-record-cancellation (load-campaign-record dir plan-id))))])
        (case decision
          [(cancelled) (on-cancel "campaign cancellation requested")]
          [(advance) (on-advance)]
          [else
           (on-blocked
            (cond
              [(eq? kind 'delivered)
               "terminal stage lacks authenticated readback; no delivery proof advanced"]
              [(and outcome (delivery-outcome? outcome)) (delivery-outcome-message outcome)]
              [(and before (not after))
               "coordinator reported progress without a durable delivery transition"]
              [else message]))]))))

;; Park a verifier-approved attempt whose protected delivery is still pending
;; (BUG-0077). Verify approval is durable, but it is NOT delivery: the SAME
;; attempt stays resumable, the delivery branch survives coordinator cleanup
;; (it is the pending merge evidence), the durable prior-attempt context
;; clears for the next wave, and the attempt artifact goes terminal 'success
;; with the locally determinable merge status. No DONE event, no completion
;; outbox, no attempt rerun.
(define (park-approved-attempt-result! base-dir
                                       plan-id
                                       wave-idx
                                       attempt-id
                                       #:delivery-context [delivery-context #f]
                                       #:worktree [worktree #f]
                                       #:keep-branch-box [keep-branch-box #f]
                                       #:refresh [refresh (lambda () #f)])
  (when delivery-context
    (warn-zero-commit-delivery-branch! delivery-context))
  ;; Delivery approved: the release wrapper must KEEP the branch (it is the
  ;; pending delivery evidence).
  (when (and worktree keep-branch-box)
    (set-box! keep-branch-box #t))
  ;; BUG-0024 W3: success clears the durable prior-attempt context so the
  ;; next wave starts from zero context.
  (let ([done-rec (refresh)])
    (define done-wave
      (and done-rec
           (for/first ([w (in-list (campaign-record-waves done-rec))]
                       #:when (= (campaign-wave-index w) wave-idx))
             w)))
    (when (and done-wave (positive? (string-length (campaign-wave-attempt-context done-wave))))
      (set-campaign-wave-attempt-context! done-wave "")
      (persist-campaign! base-dir done-rec)))
  ;; v1.00.21 W5 (BUG-0029 action 1): terminal 'success + locally-determinable
  ;; merge status for the delivered branch — the ledger owns it until
  ;; merged/reclaimed.
  (mark-attempt-artifact-terminal!
   base-dir
   plan-id
   wave-idx
   attempt-id
   'success
   #:merge-status
   (and delivery-context
        (artifact-merge-status/local (branch-delivery-context-ref delivery-context 'repo-root)
                                     (branch-delivery-context-ref delivery-context 'branch))))
  (campaign-result 'wave-awaiting-delivery
                   '()
                   "verified attempt awaiting authenticated protected delivery"))
