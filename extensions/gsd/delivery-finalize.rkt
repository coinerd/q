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
         (only-in "review-checkpoint.rkt" resolve-review-checkpoint! pending-review-rejection-data)
         (only-in "author-checkpoint.rkt"
                  resolve-author-checkpoint!
                  resolve-supplemental-author-checkpoint!
                  author-repair-pending?)
         (only-in "supplemental-evidence-controller.rkt"
                  supplemental-resumable?
                  supplemental-author-pending?
                  supplemental-pointer-retained?
                  supplemental-entry-outcome
                  supplemental-checkpoint-step!)
         (only-in "delivery-journal.rkt"
                  journal-evidence-author-repair
                  update-delivery-journal!) ; BUG-0084: durable review-infra retry counter
         (only-in "author-session-recovery.rkt"
                  author-publication-recovery
                  recover-lost-author-session!)
         (only-in "campaign-state.rkt"
                  campaign-plan-id
                  campaign-fence-token
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
         (only-in "delivery-journal.rkt" load-delivery-journal record-verification-context!)
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
                  delivery-outcome
                  delivery-outcome?
                  delivery-outcome-kind
                  delivery-outcome-message
                  delivery-outcome-data
                  review-rejected-message)
         (only-in "campaign-result.rkt"
                  campaign-result
                  campaign-result?
                  campaign-result-status
                  campaign-result-message
                  campaign-result-with-findings)
         ;; W3: the ONE pure campaign decision + the authenticated delivery
         ;; readback adapter that feeds it.
         "campaign-decision.rkt"
         (only-in "delivery-handoff.rkt"
                  delivery-readback
                  campaign-delivery-view-for-pass
                  pending-delivery-message)
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
         current-gsd-review-infra-retry-limit
         journal-review-infra-count
         run-delivery-finalization
         run-coordinator-checkpoint
         (struct-out campaign-outcome)
         campaign-lane-view-for-wave
         gather-campaign-views
         run-campaign-resume!
         run-campaign-block!
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
                                             #:delivery-context [delivery-context #f]
                                             #:verification-context [verification-context #f])
  (define journal
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (load-delivery-journal base-dir plan-id wave-idx)))
  (define receipt (and (hash? journal) (hash-ref journal 'receipt #f)))
  (define worktree-branch
    (and worktree delivery-context (branch-delivery-context-ref delivery-context 'branch)))
  (define worktree-head (and worktree (wave-worktree-head-sha worktree)))
  (define provenance
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
  ;; Directive item 2 (v1.00.33 audit): when the verify pipeline supplies the
  ;; verification context, bind it durably beside the receipt it explains —
  ;; the audited defect was exactly this context being propagated nowhere.
  ;; Side effect only: the documented return contract above is preserved.
  (when verification-context
    (record-verification-context! base-dir plan-id wave-idx verification-context))
  provenance)

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
    ;; BUG-0084: a review-infra retry advances only through its durable
    ;; counter bump — the same no-spin law as 'ok.
    [(eq? outcome-kind 'review-infra-retry)
     (if (delivery-journal-advanced? before after) 'advance 'blocked)]
    [else 'blocked]))

;; BUG-0084: bounded review-lane relaunch. A typed 'review-infra outcome
;; (reviewer transport failure, stale session, malformed output, missing
;; verdict) used to be a terminal wave-block — a dead end whenever the ONLY
;; missing evidence was the review itself. The relaunch budget is a SEPARATE
;; durable counter in the delivery journal (never an author/rejection
;; allowance); each retry IS the durable journal transition, so the existing
;; no-spin guard (advance requires durable progress) stays intact. Any other
;; advancing outcome resets the counter so every review ROUND gets a fresh
;; budget. Beyond the limit the campaign stops blocked with the honest
;; reason instead of spinning.
(define current-gsd-review-infra-retry-limit (make-parameter 2))
(define (journal-review-infra-count journal)
  (or (and journal (hash-ref journal 'review-infra-count #f)) 0))
(define (bump-review-infra-count! dir plan-id idx)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define j (load-delivery-journal dir plan-id idx))
    (and j
         (begin
           (update-delivery-journal! dir
                                     plan-id
                                     idx
                                     (hasheq 'review-infra-count
                                             (add1 (journal-review-infra-count j))))
           (add1 (journal-review-infra-count j))))))
(define (clear-review-infra-count! dir plan-id idx)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define j (load-delivery-journal dir plan-id idx))
    (when (and j (positive? (journal-review-infra-count j)))
      (update-delivery-journal! dir plan-id idx (hasheq 'review-infra-count 0)))))
(define (honest-blocked-reason reason)
  ;; BUG-0084: a blank blocked reason is never a durable failure reason.
  (if (and (string? reason) (positive? (string-length (string-trim reason))))
      reason
      "wave blocked without an actionable reason; inspect the review/validation reports"))

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

;; ============================================================
;; W3: lane facts, the resume effect interpreter, and the block
;; effect (secondary/idempotent) that precedes a typed denial
;; ============================================================

;; Durable lane facts for ONE wave. Reads only: the pure decision
;; (campaign-decision.rkt) chooses the route from these facts. Probe failures
;; are fail-closed to "no such lane" — every probe below already treats a
;; malformed/absent artifact as absent, so an exception here is an unexpected
;; read failure, never permission.
(define (campaign-lane-view-for-wave dir plan-id wave-index)
  (define (safe thunk)
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (thunk)))
  (define journal (and wave-index (delivery-journal-snapshot dir plan-id wave-index)))
  (campaign-lane-view
   wave-index
   journal
   (and wave-index (safe (lambda () (supplemental-author-pending? dir plan-id wave-index))))
   (and wave-index (safe (lambda () (supplemental-resumable? dir plan-id wave-index))))
   (and wave-index (safe (lambda () (supplemental-pointer-retained? dir plan-id wave-index))))
   (and wave-index (safe (lambda () (author-repair-pending? dir plan-id wave-index))))
   (and journal
        (let ([marker (journal-evidence-author-repair journal)])
          (and marker (equal? (hash-ref marker 'state #f) "running"))))
   (and journal (pending-review-rejection-data journal))))

;; The outcome of performing ONE decided resume/block route.
;; kind ∈ advance cancelled blocked. `typed` carries a decision-shaped denial
;; unchanged (the retained-supplemental entry seam returns the protected
;; operation's own campaign result); the campaign loop promotes it so a
;; secondary seal never erases the primary cause.
(struct campaign-outcome (kind reason typed) #:transparent)

;; BUG-0088 C1: an authenticated active supplemental operation past its launch
;; boundary resumes HERE, between wave passes, under the held campaign lease —
;; WITHOUT beginning a new attempt (never re-reserves). One advance per
;; checkpoint visit; the private CAS journal is the durable progress proof,
;; so the existing no-spin law stays intact through 'advance only when the
;; protected operation actually moved.
(define (supplemental-checkpoint-visit! dir plan-id idx)
  (define record (load-campaign-record dir plan-id))
  (if (and record (campaign-record-cancellation record))
      (campaign-outcome 'cancelled "campaign cancellation requested" #f)
      (let-values ([(verdict detail) (supplemental-checkpoint-step! dir plan-id idx)])
        (case verdict
          [(advance) (campaign-outcome 'advance #f #f)]
          [else (campaign-outcome 'blocked detail #f)]))))

;; Typed terminal payload for a BLOCKED review rejection: the wave-blocked
;; result carries the reviewer's findings as structured diagnostics, so no
;; consumer ever re-parses the operator message to recover them. #f when the
;; outcome carried no typed payload (legacy coordinator) or no findings.
(define (review-rejection-blocked-result reason outcome*)
  (define data (and (delivery-outcome? outcome*) (delivery-outcome-data outcome*)))
  (define findings (and (hash? data) (hash-ref data 'findings #f)))
  (and findings
       (campaign-result-with-findings
        (campaign-result 'wave-blocked '() (honest-blocked-reason reason))
        findings)))

;; Classify a lane step after its effect: the durable journal must actually
;; have moved; a coordinator that claims progress without a durable transition
;; must not spin, and the author/review checkpoints keep their existing
;; resolution attempts. Identical classification to the pre-W3 code, now
;; reached from every route instead of one else-branch.
(define (classify-lane-effect! dir
                               plan-id
                               idx
                               before
                               after
                               outcome*
                               cancelled?
                               message
                               author-session-live?)
  (define kind (and outcome* (delivery-outcome-kind outcome*)))
  (case (coordinator-checkpoint-decision kind before after cancelled?)
    [(cancelled) (campaign-outcome 'cancelled "campaign cancellation requested" #f)]
    [(advance)
     ;; BUG-0084: any genuine advance ends the current review round — its infra
     ;; budget is refreshed for the next round.
     (unless (eq? kind 'review-infra-retry)
       (clear-review-infra-count! dir plan-id idx))
     (campaign-outcome 'advance #f #f)]
    [else
     (define resolved
       (cond
         [(and idx (author-repair-pending? dir plan-id idx))
          ;; BUG-0086: a dead running author may retain an unchanged snapshot or
          ;; finish its committed scoped successor through real owned
          ;; Verify/publication. A refusal is actionable and must NOT be
          ;; swallowed by the unlaunched-permission resolver.
          (or (recover-lost-author-session! dir plan-id idx #:session-live? author-session-live?)
              (resolve-author-checkpoint! dir plan-id idx))]
         [(and (eq? kind 'review-rejected) idx)
          ;; GSD simplification W5: the typed rejection payload authorizes the
          ;; checkpoint (legacy coordinators that only produced the historic
          ;; message still work through the dated reader inside it).
          (resolve-review-checkpoint! dir
                                      plan-id
                                      idx
                                      (or (delivery-outcome-data outcome*)
                                          (delivery-outcome-message outcome*)))]
         [else #f]))
     (if (eq? resolved #t)
         (campaign-outcome 'advance #f #f)
         (let ([blocked-reason
                (cond
                  [(eq? kind 'delivered)
                   "terminal stage lacks authenticated readback; no delivery proof advanced"]
                  [resolved (honest-blocked-reason resolved)]
                  [(and outcome* (delivery-outcome? outcome*))
                   (honest-blocked-reason (delivery-outcome-message outcome*))]
                  [(and before (not after))
                   "coordinator reported progress without a durable delivery transition"]
                  [else message])])
           ;; The typed payload (when present) carries the SAME reason plus the
           ;; reviewer's findings, so no consumer re-parses the message.
           (campaign-outcome 'blocked
                             blocked-reason
                             (review-rejection-blocked-result blocked-reason outcome*))))]))

;; Perform ONE decided coordinator/author/review route. The route itself was
;; chosen by decide-campaign-action / decide-lane-action; this function only
;; executes the effect and classifies its durable result.
(define (run-lane-effect! dir plan-id idx route message delivery-coordinator author-session-live?)
  (define before (and idx (delivery-journal-snapshot dir plan-id idx)))
  ;; Durable cancellation wins before any lane effect (pre-W3 contract, which
  ;; reported the caller's message rather than a generic one).
  (define rec (load-campaign-record dir plan-id))
  (cond
    [(and rec (campaign-record-cancellation rec)) (campaign-outcome 'cancelled message #f)]
    [else
     (define author-replay (and idx (author-repair-pending? dir plan-id idx)))
     (define recovery
       (and (eq? route 'author-publication-recovery)
            idx
            author-replay
            (author-publication-recovery dir plan-id idx)))
     (define replay (and idx (pending-review-rejection-data before)))
     (define outcome
       (cond
         [(eq? route 'author-permission)
          (cons 'evidence-author-repair "durable author permission pending")]
         [(eq? route 'author-publication-recovery)
          (cond
            [(not recovery) (cons 'evidence-author-repair "durable author permission pending")]
            [else
             ;; Publication-only crash recovery: the confirmed successor
             ;; publication and its owned proof replay supersession; no author
             ;; phase, reviewer or ladder effect is redispatched.
             (with-handlers ([exn:fail? (lambda (e)
                                          (cons 'evidence-author-repair
                                                (format "author publication recovery refused: ~a"
                                                        (exn-message e))))])
               (cons 'ok (hash-ref (recovery) 'message)))])]
         [(eq? route 'review-replay)
          (delivery-outcome 'review-rejected
                            (or (and replay (review-rejected-message replay)) "")
                            replay)]
         [(procedure? delivery-coordinator) (delivery-coordinator dir plan-id idx)]
         [else (cons 'blocked "no delivery coordinator is wired for this campaign")]))
     ;; BUG-0084: a typed review-infra outcome consumes one unit of the SEPARATE
     ;; durable relaunch budget before the decision. The counter bump IS a
     ;; durable journal transition, so advancing on it never violates the
     ;; no-spin guard; past the limit the honest bounded reason stops the wave.
     (define outcome*
       (if (and idx (delivery-outcome? outcome) (eq? (delivery-outcome-kind outcome) 'review-infra))
           (let ([count (bump-review-infra-count! dir plan-id idx)]
                 [limit (current-gsd-review-infra-retry-limit)])
             (cond
               [(and count (<= count limit))
                (cons 'review-infra-retry (delivery-outcome-message outcome))]
               [count
                (cons 'blocked
                      (format "review lane failed ~a times (limit ~a); operator review required: ~a"
                              count
                              limit
                              (delivery-outcome-message outcome)))]
               [else outcome]))
           outcome))
     (define after (and idx (delivery-journal-snapshot dir plan-id idx)))
     (classify-lane-effect! dir
                            plan-id
                            idx
                            before
                            after
                            outcome*
                            (and rec
                                 (campaign-record-cancellation (load-campaign-record dir plan-id)))
                            message
                            author-session-live?)]))

;; Execute the ONE decided resume route. Returns a campaign-outcome; the caller
;; owns the loop continuation and the campaign result. The route was chosen from
;; lane facts by the pure decision; the effect re-reads the durable facts it must
;; classify (author marker, journal snapshot), exactly as the pre-W3 checkpoint
;; did after its effect.
(define (run-campaign-resume! dir
                              plan-id
                              decision
                              message
                              delivery-coordinator
                              #:author-session-live? [author-session-live? (lambda () #t)])
  (define idx (campaign-decision-wave-index decision))
  (case (campaign-decision-route decision)
    [(supplemental-author)
     (define resolved (resolve-supplemental-author-checkpoint! dir plan-id idx))
     (if (eq? resolved #t)
         (campaign-outcome 'advance #f #f)
         (campaign-outcome 'blocked resolved #f))]
    [(supplemental-checkpoint) (supplemental-checkpoint-visit! dir plan-id idx)]
    [(coordinator-step author-publication-recovery author-permission review-replay)
     (run-lane-effect! dir
                       plan-id
                       idx
                       (campaign-decision-route decision)
                       message
                       delivery-coordinator
                       author-session-live?)]
    [else (campaign-outcome 'blocked message #f)]))

;; Execute the ONE decided BLOCK. A retained supplemental pointer is a block
;; whose secondary, best-effort, idempotent entry seal must run BEFORE the typed
;; denial; the seal can never launch ordinary work and its failure never erases
;; the primary cause.
(define (run-campaign-block! dir plan-id decision)
  (define primary (campaign-decision-message decision))
  (case (campaign-decision-route decision)
    [(supplemental-retained)
     (define denial
       (with-handlers ([exn:fail? (lambda (e)
                                    (log-warning "supplemental retained seal refused: ~a"
                                                 (exn-message e))
                                    #f)])
         (supplemental-entry-outcome dir plan-id (campaign-decision-wave-index decision))))
     (cond
       [(and denial (campaign-result? denial))
        (campaign-outcome 'blocked (campaign-result-message denial) denial)]
       [else (campaign-outcome 'blocked primary #f)])]
    [else (campaign-outcome 'blocked primary #f)]))

;; Build the ONE decision input for a campaign pass: authenticated delivery
;; facts, the live durable record, lane facts for the delivery-blocked wave, and
;; the runtime capability. The pending message is built here (this adapter owns
;; the handoff ledger write) so a block decision always carries its operator
;; text. The pure decision then chooses exactly one action.
(define (gather-campaign-views base-dir
                               plan-id
                               current
                               completed
                               delivery-reader
                               verified
                               delivery-coordinator)
  (define-values (delivery stale? live pending-message)
    (campaign-delivery-view-for-pass base-dir plan-id current completed delivery-reader verified))
  (define pending (campaign-delivery-view-pending delivery))
  (define lane (and pending (campaign-lane-view-for-wave base-dir plan-id (car pending))))
  (campaign-views live
                  completed
                  stale?
                  delivery
                  lane
                  pending-message
                  (procedure? delivery-coordinator)))

;; Perform one decided resume route and classify its durable outcome. The route
;; was chosen by the pure decision; this wrapper exists for the focused
;; checkpoints that call one lane step directly, and keeps their historic
;; on-cancel / on-advance / on-blocked continuation contract.
(define (run-coordinator-checkpoint dir
                                    plan-id
                                    extra
                                    message
                                    delivery-coordinator
                                    #:on-cancel [on-cancel (lambda (_) #f)]
                                    #:on-advance [on-advance (lambda () #f)]
                                    #:on-blocked [on-blocked (lambda (_) #f)]
                                    ;; BUG-0085: liveness of the launched
                                    ;; evidence-author session. The default is
                                    ;; LIVE (conservative: never reclaim on a
                                    ;; guess); the orchestrator passes the
                                    ;; ownership-contract verdict — checkpoints
                                    ;; run strictly between wave passes and the
                                    ;; owned registry reaps its jobs on pass end
                                    ;; and termination, so #f is a proven dead
                                    ;; session, never an assumption.
                                    #:author-session-live? [author-session-live? (lambda () #t)])
  (define idx (and (campaign-wave? extra) (campaign-wave-index extra)))
  (define rec (load-campaign-record dir plan-id))
  (cond
    ;; Durable cancellation wins before any lane effect.
    [(and (campaign-wave? extra) (campaign-record-cancellation rec)) (on-cancel message)]
    [else
     (define lane (campaign-lane-view-for-wave dir plan-id idx))
     (define decision (decide-lane-action lane idx message (procedure? delivery-coordinator)))
     (define outcome
       (case (campaign-decision-kind decision)
         [(resume)
          (run-campaign-resume! dir
                                plan-id
                                decision
                                message
                                delivery-coordinator
                                #:author-session-live? author-session-live?)]
         [else (run-campaign-block! dir plan-id decision)]))
     (case (campaign-outcome-kind outcome)
       [(advance) (on-advance)]
       [(cancelled) (on-cancel (campaign-outcome-reason outcome))]
       [else (on-blocked (or (campaign-outcome-typed outcome) (campaign-outcome-reason outcome)))])]))

;; W3 removed the pre-W3 `coordinator-checkpoint-result` and
;; `wave-blocked-checkpoint-result!` route functions. They each reconstructed
;; the next action from a status symbol; the campaign loop (go-orchestrator.rkt)
;; is now the ONE interpreter of decide-campaign-action, and the focused
;; checkpoints call run-coordinator-checkpoint directly.

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
