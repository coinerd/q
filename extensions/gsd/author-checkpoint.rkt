#lang racket/base
;; Durable author permission -> ordinary successor attempt, without rejection.
;; BUG-0088 C1: the legacy evidence-author checkpoint and the governed
;; supplemental task are DISJOINT lanes. While an authenticated supplemental
;; operation is live for the same wave, the legacy resolver and its retained
;; worktree refuse: no old-author route may hijack the supplemental flow, and
;; the supplemental task context owns the retained checkout instead.
(require racket/path
         "campaign-state.rkt"
         "campaign-repository.rkt"
         "delivery-journal.rkt"
         "delivery-receipt.rkt"
         (only-in "supplemental-evidence-authority.rkt" call-with-supplemental-authority-lock)
         (only-in "supplemental-evidence-controller.rkt"
                  supplemental-author-pending?
                  supplemental-approved-entry-context
                  supplemental-approved-entry-parent-allocation
                  supplemental-operation-live?
                  supplemental-pointer-retained?
                  supplemental-verify-route!)
         (only-in "wave-executor.rkt" wave-worktree))
(provide author-repair-pending?
         author-repair-running?
         resolve-author-checkpoint!
         resolve-supplemental-author-checkpoint!
         retained-author-worktree
         ;; Combined-lane predicates for the orchestrator's thin call sites:
         ;; the legacy durable author permission OR an authenticated active
         ;; supplemental operation selects the owned lane. The done-case route
         ;; continues the supplemental flow after the author attempt returns.
         author-lane-pending?
         author-live-supplemental?
         supplemental-checkout-retained?
         supplemental-done-route!)
(define (author-lane-pending? root rec wave-idx)
  (define plan (campaign-plan-id rec))
  ;; A retained pointer also owns terminal/revoked retries. It selects the
  ;; guarded lane, never permission to dispatch a spent task.
  (or (supplemental-pointer-retained? root plan wave-idx)
      (author-repair-pending? root plan wave-idx)))
(define (author-live-supplemental? root rec wave-idx)
  (supplemental-operation-live? root (campaign-plan-id rec) wave-idx))
(define (supplemental-checkout-retained? root rec wave-idx)
  (supplemental-pointer-retained? root (campaign-plan-id rec) wave-idx))
(define (supplemental-done-route! root rec wave-idx expected-id fence)
  ;; Done-case fall-through route: #f unless an authenticated supplemental
  ;; operation is live; otherwise continue the protected flow and park at
  ;; typed pauses. Consumed by the orchestrator's `[... => values]` clause.
  (and (supplemental-operation-live? root (campaign-plan-id rec) wave-idx)
       (supplemental-verify-route! root (campaign-plan-id rec) wave-idx expected-id fence)))
(define (author-repair-pending? root plan wave)
  (evidence-author-repair-pending? (load-delivery-journal root plan wave)))
(define (author-repair-running? root plan wave)
  (define journal (load-delivery-journal root plan wave))
  (define marker (journal-evidence-author-repair journal))
  (and marker (evidence-author-repair-pending? journal) (equal? (hash-ref marker 'state) "running")))
;; Full campaign entry calls this BEFORE the legacy coordinator, under the
;; campaign lease. The authority lock serializes approval revocation with the
;; status-only commit. No marker, receipt, task, approval or allocation changes.
;;
;; PARKED PARENT ALLOCATION (preserved candidates). The ordinary entry is
;; parked on the exact attempt/fence of the immutable E_old receipt context.
;; A preserved entry is deliberately parked on the fixed third-generation
;; terminal allocation (attempt-6/fence-9), which is DISTINCT from E_old
;; (attempt-2/fence-5). The additive controller accessor authenticates the
;; same live child T/pointer and returns that fully validated terminal
;; allocation for preserved tasks only, so the parked attempt is compared to
;; the allocation that actually owns it. Ordinary entries keep the exact
;; context attempt/fence comparison; every cancellation, pause, status,
;; coordinator-fence and spent-operation guard below is unchanged and is
;; re-read immediately before the pending status commit.
(define (resolve-supplemental-author-checkpoint! root plan wave)
  (with-handlers ([exn:fail? (lambda (e) (exn-message e))])
    (call-with-supplemental-authority-lock
     (lambda ()
       (define record (load-campaign-record root plan))
       (define w
         (and record
              (for/first ([w (in-list (campaign-record-waves record))]
                          #:when (= wave (campaign-wave-index w)))
                w)))
       (define attempt (and w (campaign-wave-current-attempt w)))
       (define context (supplemental-approved-entry-context root plan wave))
       (define parent-allocation (supplemental-approved-entry-parent-allocation root plan wave))
       (define expected
         (or parent-allocation
             (hasheq 'attempt-id
                     (hash-ref context 'attempt-id)
                     'attempt-fence
                     (hash-ref context 'attempt-fence)
                     'coordinator-fence
                     (hash-ref context 'attempt-fence))))
       (unless (and record
                    w
                    attempt
                    (not (campaign-record-cancellation record))
                    (not (campaign-record-budget-pause record))
                    (if parent-allocation
                        (= (campaign-fence-token record) (hash-ref expected 'coordinator-fence))
                        (>= (campaign-fence-token record) (hash-ref expected 'coordinator-fence)))
                    (eq? (campaign-wave-status w) 'awaiting-delivery)
                    (equal? (campaign-attempt-id attempt) (hash-ref expected 'attempt-id))
                    (equal? (campaign-attempt-fence-token attempt)
                            (hash-ref expected 'attempt-fence)))
         (error 'author-checkpoint "stale, cancelled or paused supplemental checkpoint"))
       ;; Re-read the authenticated helper, context and campaign after
       ;; validation, immediately before persisting.
       (unless (and (equal? context (supplemental-approved-entry-context root plan wave))
                    (equal? parent-allocation
                            (supplemental-approved-entry-parent-allocation root plan wave))
                    (equal? record (load-campaign-record root plan)))
         (error 'author-checkpoint "supplemental checkpoint changed before record transition"))
       (set-campaign-wave-status! w 'pending)
       (persist-campaign! root record)
       #t))))

(define (resolve-author-checkpoint! root plan wave)
  (with-handlers ([exn:fail? (lambda (e) (exn-message e))])
    (define journal (load-delivery-journal root plan wave))
    (define marker (journal-evidence-author-repair journal))
    (unless (and (evidence-author-repair-pending? journal)
                 (equal? (hash-ref marker 'state) "pending"))
      (error 'author-checkpoint "no unlaunched evidence author permission; ambiguous launches pause"))
    (when (supplemental-operation-live? root plan wave)
      (error
       'author-checkpoint
       "an authenticated supplemental operation is live; the legacy author checkpoint stays disjoint"))
    (define record (load-campaign-record root plan))
    (define w
      (and record
           (for/first ([w (in-list (campaign-record-waves record))]
                       #:when (= wave (campaign-wave-index w)))
             w)))
    (define attempt (and w (campaign-wave-current-attempt w)))
    (define context (hash-ref marker 'context))
    (unless (and record
                 w
                 attempt
                 (not (campaign-record-cancellation record))
                 (not (campaign-record-budget-pause record))
                 (>= (campaign-fence-token record) (hash-ref context 'attempt-fence))
                 (memq (campaign-wave-status w) '(awaiting-delivery pending))
                 (equal? (campaign-attempt-id attempt) (hash-ref context 'attempt-id))
                 (equal? (campaign-attempt-fence-token attempt) (hash-ref context 'attempt-fence)))
      (error 'author-checkpoint "stale, cancelled or paused evidence author checkpoint"))
    (unless (equal? journal (load-delivery-journal root plan wave))
      (error 'author-checkpoint "author permission changed before record transition"))
    ;; No failed attempt, findings, rejection counter, approval or completion
    ;; is invented. The prior verified receipt remains immutable until a real
    ;; successor full Verify supersedes it.
    (set-campaign-wave-status! w 'pending)
    (set-campaign-wave-attempt-context!
     w
     "Evidence author permission: retain prior implementation; author compatible regression inside frozen FILES; then full Verify and fresh review.")
    (persist-campaign! root record)
    #t))
(define (retained-author-worktree root rec wave)
  (define plan (campaign-plan-id rec))
  (define journal (load-delivery-journal root plan wave))
  (and (evidence-author-repair-pending? journal)
       (begin
         (when (supplemental-operation-live? root plan wave)
           (error 'author-checkpoint
                  "an authenticated supplemental operation owns the retained checkout"))
         (let* ([marker (journal-evidence-author-repair journal)]
                [receipt (hash-ref journal 'receipt)]
                [context (hash-ref marker 'context)]
                [cwd (string->path (hash-ref context 'repo))]
                [source (committed-delivery-snapshot cwd)])
           (unless (and (equal? (hash-ref marker 'state) "pending")
                        source
                        (for/and ([k '(repo branch origin head tree)])
                          (equal? (hash-ref source k) (hash-ref receipt k))))
             (error 'author-checkpoint
                    "exact retained author checkout unavailable; no fallback or rewind"))
           (wave-worktree (string->path (hash-ref receipt 'repo))
                          cwd
                          (hash-ref receipt 'branch)
                          "origin/main"
                          (build-path root ".planning"))))))
