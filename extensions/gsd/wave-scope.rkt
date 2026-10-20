#lang racket/base
;; wave-scope.rkt — the per-attempt execution scope decision.
;;
;; Extracted from go-orchestrator.rkt (BUG-0042 maintenance contract:
;; extract a module instead of growing the orchestrator). Owns ONE
;; question: does this attempt run in the shared checkout or inside a
;; retained isolated worktree, and which checkout is retained when one
;; must be? Rules preserved verbatim from the orchestrator:
;;   - shared checkout only when NOT owned-lane, isolation not enabled,
;;     and no pending review retry;
;;   - owned author phases and review-retry attempts NEVER fall back to
;;     the shared checkout (a worktree failure raises instead of
;;     abandoning retained source);
;;   - ordinary isolation failures degrade to the shared checkout with
;;     a loud warning (campaign liveness over isolation purity);
;;   - evidence-author and review-retry lanes retain their exact prior
;;     checkouts; fresh attempts build a campaign worktree off the
;;     durable spare branches.
(require (only-in "campaign-state.rkt" campaign-plan-id)
         (only-in "wave-executor.rkt"
                  worktree-isolation-enabled?
                  make-wave-worktree!
                  call-with-retained-wave-worktree!)
         (only-in "review-worktree.rkt" review-retry-pending? retained-review-retry-worktree)
         (only-in "author-checkpoint.rkt" retained-author-worktree)
         (only-in "supplemental-evidence-controller.rkt"
                  retained-supplemental-worktree
                  supplemental-author-pending?)
         (only-in "attempt-artifacts.rkt" record-attempt-artifact!)
         (only-in "delivery-publication.rkt" durable-spare-branches))
(provide call-with-wave-scope)

;; owned-phases? : the owned-red-v1 / evidence-author lane marker.
;; author-pending? : a durable evidence-author permission is pending.
;; supplemental-pending? : an authenticated active private supplemental
;;   operation is pending; its task context pins the SAME physical checkout.
;; wt-box : the caller-owned worktree box (release stays with the caller).
(define (call-with-wave-scope base-dir
                              active
                              wave-idx
                              isolate?
                              owned-phases?
                              author-pending?
                              expected-id
                              wt-box
                              run-thunk
                              #:supplemental-pending? [supplemental-arg #f])
  ;; A pending supplemental task selects the same retention class as the
  ;; legacy author lane: its task context pins the SAME physical checkout.
  (define supplemental-pending?
    (or supplemental-arg (supplemental-author-pending? base-dir (campaign-plan-id active) wave-idx)))
  (if (and (not owned-phases?)
           (not supplemental-pending?)
           (not (worktree-isolation-enabled? #:isolate? isolate?))
           (not (review-retry-pending? base-dir (campaign-plan-id active) wave-idx)))
      (run-thunk)
      (call-with-retained-wave-worktree!
       wt-box
       (lambda ()
         (with-handlers
             ([exn:fail?
               (lambda (e)
                 (when (or owned-phases?
                           supplemental-pending?
                           (review-retry-pending? base-dir (campaign-plan-id active) wave-idx))
                   (raise e)) ; never abandon rejected/supplemental source for the shared checkout
                 (log-warning
                  "wave ~a: worktree isolation unavailable (~a); running in shared checkout"
                  wave-idx
                  (exn-message e))
                 #f)])
           (define acquired
             ;; BUG-0088 disjoint lanes: a live supplemental task owns the
             ;; retained checkout; the legacy author worktree never hijacks it.
             (or (and supplemental-pending? (retained-supplemental-worktree base-dir active wave-idx))
                 (and author-pending?
                      (not supplemental-pending?)
                      (retained-author-worktree base-dir active wave-idx))
                 (retained-review-retry-worktree base-dir active wave-idx)
                 (make-wave-worktree! base-dir
                                      #:campaign-id (campaign-plan-id active)
                                      #:wave-index wave-idx
                                      #:spare-branches (durable-spare-branches base-dir active))))
           (when (and (or owned-phases? supplemental-pending?) (not acquired))
             (error 'go-author-phase "owned author phases require an isolated source checkout"))
           acquired))
       (lambda (wt)
         (record-attempt-artifact! base-dir (campaign-plan-id active) wave-idx expected-id wt))
       run-thunk)))
