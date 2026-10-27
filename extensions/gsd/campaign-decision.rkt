#lang racket/base

;; extensions/gsd/campaign-decision.rkt — ONE pure /go campaign decision (W3).
;;
;; Before W3 the next /go action was reconstructed independently in
;; delivery-handoff.rkt (`ready-for-run-checkpoint`), delivery-finalize.rkt
;; (`run-coordinator-checkpoint`) and go-orchestrator.rkt (its `case` on those
;; results). This module is the single routing authority: it consumes explicit
;; views and returns exactly one action.
;;
;;   complete — every wave is done/deferred and delivery-proven
;;   launch   — begin the next actionable wave (implementation effect)
;;   resume   — a durable lane effect may advance the campaign; `route` names
;;              the ONE adapter effect to perform (coordinator step,
;;              supplemental author/checkpoint lane, evidence-author recovery,
;;              durable author permission, review replay)
;;   finalize — an authenticated delivered proof commits the parked wave DONE
;;   block    — no route may advance; `route` may name a secondary/idempotent
;;              sealing effect that must run BEFORE the typed denial
;;   cancel   — durable cancellation, or durable state moved during the
;;              authenticated readback
;;
;; Purity: no I/O, no parameters, no subprocesses, no journal writes. The
;; caller gathers the views (durable record, authenticated delivery facts, lane
;; facts, runtime capability) and interprets the decision. Routing NEVER
;; inspects a human message and never matches text; only typed kinds, statuses,
;; wave indices and the route symbol choose behaviour.

(require "campaign-state.rkt")

;; ============================================================
;; Input views
;; ============================================================

;; Authenticated delivery facts for one pass, produced by the delivery-readback
;; adapter (delivery-handoff.rkt). `pending` is the first durable wave whose
;; authenticated proof is not (yet) binding, as (cons wave-index reason);
;; `awaiting` maps a wave index to its memoized full 40-hex merge SHA.
(struct campaign-delivery-view (pending awaiting) #:transparent)

;; Durable lane facts for ONE wave, produced by the supplemental/author/review
;; adapters (delivery-finalize.rkt). All fields are disk-observed facts; none
;; is a decision. `review-rejection` is the TYPED review-rejection payload (the
;; journal marker hash) or #f — never a human message (W5).
(struct campaign-lane-view
        (wave-index journal-before
                    supplemental-author-pending?
                    supplemental-resumable?
                    supplemental-pointer-retained?
                    author-repair-pending?
                    author-repair-running?
                    review-rejection)
  #:transparent)

;; The complete decision input. `record` is the authoritative durable campaign
;; record; `stale?` is #t when durable state moved during the authenticated
;; readback; `lane` is #f when no lane was observed for this pass;
;; `pending-message` is the operator text for a delivery block (already built by
;; the handoff adapter, which owns the ledger write); `coordinator-available?` is
;; the runtime capability fact (a delivery coordinator is wired).
(struct campaign-views (record completed stale? delivery lane pending-message coordinator-available?)
  #:transparent)

;; ============================================================
;; Output decision
;; ============================================================

;; kind  ∈ complete launch resume finalize block cancel
;; route ∈ none
;;         coordinator-step
;;         supplemental-author | supplemental-checkpoint | supplemental-retained
;;         author-publication-recovery | author-permission | review-replay
;; code   is the stable machine code (never parsed for control flow);
;; merge-sha is the authenticated proof for a `finalize` decision.
(struct campaign-decision (kind wave-index route message code merge-sha) #:transparent)

(define (make-decision kind wave-index route message code #:merge-sha [merge-sha #f])
  (campaign-decision kind wave-index route message code merge-sha))

;; Stable machine codes. The route→code mapping is total so every decision
;; carries one code; no consumer has to derive a code from text.
(define (route-code route)
  (case route
    [(none) 'campaign-action]
    [(coordinator-step) 'coordinator-step]
    [(supplemental-author) 'supplemental-author-pending]
    [(supplemental-checkpoint) 'supplemental-resume]
    [(supplemental-retained) 'supplemental-retained]
    [(author-publication-recovery) 'evidence-author-recovery]
    [(author-permission) 'evidence-author-permission]
    [(review-replay) 'review-replay]
    [else (error 'campaign-decision "unknown campaign route: ~a" route)]))

;; ============================================================
;; Lane routing (the one route table)
;; ============================================================

;; Supplemental lanes come first: an approved/unlaunched T and an operation past
;; its launch boundary own the wave. A retained pointer that is neither
;; launchable nor resumable is a BLOCK whose secondary seal must run first — it
;; never falls through to ordinary coordinator/author work. Only then may the
;; evidence-author / review / coordinator lanes advance.
;;
;; A wave with no observed lane can still reach the coordinator when one is
;; wired (the pre-W3 default), and is otherwise a plain block.
(define (lane-route lane coordinator-available?)
  (cond
    [(not lane) (and coordinator-available? 'coordinator-step)]
    [(campaign-lane-view-supplemental-author-pending? lane) 'supplemental-author]
    [(campaign-lane-view-supplemental-resumable? lane) 'supplemental-checkpoint]
    [(campaign-lane-view-supplemental-pointer-retained? lane) 'supplemental-retained]
    [(not coordinator-available?) #f]
    [(and (campaign-lane-view-author-repair-pending? lane)
          (campaign-lane-view-author-repair-running? lane))
     'author-publication-recovery]
    [(campaign-lane-view-author-repair-pending? lane) 'author-permission]
    [(campaign-lane-view-review-rejection lane) 'review-replay]
    [else 'coordinator-step]))

;; Decide the lane action for one wave. `message` is the fallback operator text
;; for a block (often the pending-delivery text); the resume executor may
;; replace it with a more specific effect reason.
(define (decide-lane-action lane wave-index message coordinator-available?)
  (define route (lane-route lane coordinator-available?))
  (case route
    [(supplemental-retained)
     (make-decision 'block
                    wave-index
                    route
                    (or message "retained supplemental task is not launchable")
                    (route-code route))]
    [else
     (if route
         (make-decision 'resume wave-index route message (route-code route))
         (make-decision 'block
                        wave-index
                        'none
                        (or message "campaign is blocked without an actionable route")
                        'delivery-pending))]))

;; ============================================================
;; Campaign decision
;; ============================================================

;; The first awaiting-delivery wave (record order) that carries an
;; authenticated merge SHA, as (cons wave-index sha), or #f.
(define (awaiting-finalizable rec delivery)
  (for/first ([w (in-list (campaign-record-waves rec))]
              #:when (eq? (campaign-wave-status w) 'awaiting-delivery))
    (define sha (hash-ref (campaign-delivery-view-awaiting delivery) (campaign-wave-index w) #f))
    (and sha (cons (campaign-wave-index w) sha))))

;; ONE decision per loop iteration. Precedence is durable truth first:
;;   1. durable state moved during the authenticated readback  → cancel
;;   2. durable cancellation                                   → cancel
;;   3. a wave lacks binding delivery proof                    → lane resume/block
;;   4. an awaiting-delivery wave has a binding proof          → finalize
;;   5. an actionable wave exists                              → launch
;;   6. nothing is actionable                                  → complete
(define (decide-campaign-action views)
  (define rec (campaign-views-record views))
  (define delivery (campaign-views-delivery views))
  (cond
    [(campaign-views-stale? views)
     (make-decision 'cancel
                    #f
                    'none
                    "campaign changed during delivery readback; stale result ignored, resume /go"
                    'campaign-stale-readback)]
    [(campaign-record-cancellation rec)
     (make-decision 'cancel #f 'none "campaign cancellation requested" 'campaign-cancelled)]
    [(campaign-delivery-view-pending delivery)
     =>
     (lambda (pending)
       (decide-lane-action (campaign-views-lane views)
                           (car pending)
                           (campaign-views-pending-message views)
                           (campaign-views-coordinator-available? views)))]
    [(awaiting-finalizable rec delivery)
     =>
     (lambda (awaiting)
       (make-decision 'finalize
                      (car awaiting)
                      'none
                      "authenticated delivery ready for finalization"
                      'delivery-ready
                      #:merge-sha (cdr awaiting)))]
    [(select-next-actionable-wave rec)
     =>
     (lambda (idx) (make-decision 'launch idx 'none (format "launch wave ~a" idx) 'wave-launch))]
    [else (make-decision 'complete #f 'none "all waves done or deferred" 'campaign-complete)]))

;; Operator-facing next action for a decision. Descriptive only: neither the
;; interpreter nor any adapter reads this text to choose behaviour.
(define (campaign-decision-next-action decision)
  (case (campaign-decision-kind decision)
    [(complete) "none - the campaign is complete"]
    [(launch) "none - the wave is executing"]
    [(finalize) "none - delivery finalization is committing DONE"]
    [(cancel)
     "inspect the durable campaign record; re-run /go only after the stop reason is resolved"]
    [(resume)
     (case (campaign-decision-route decision)
       [(supplemental-author)
        "complete the approved supplemental author lane (owner action), then re-run /go"]
       [(supplemental-checkpoint) "complete the protected supplemental operation, then re-run /go"]
       [(author-publication-recovery) "none - durable publication recovery is advancing"]
       [(author-permission) "complete the owner evidence-author action, then re-run /go"]
       [(review-replay) "resolve the recorded review rejection (owner procedure), then re-run /go"]
       [else "none - the coordinator step is advancing"])]
    [(block)
     (case (campaign-decision-route decision)
       [(supplemental-retained)
        "owner must close or approve the retained supplemental task; ordinary work is refused"]
       [else "inspect the blocking artifact named in the detail; never delete campaign state"])]
    [else #f]))

(provide (struct-out campaign-delivery-view)
         (struct-out campaign-lane-view)
         (struct-out campaign-views)
         (struct-out campaign-decision)
         decide-campaign-action
         decide-lane-action
         lane-route
         route-code
         campaign-decision-next-action)
