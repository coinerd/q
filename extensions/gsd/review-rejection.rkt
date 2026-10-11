#lang racket/base
;; extensions/gsd/review-rejection.rkt — automatic independent review,
;; slice A state port (BUG_PLAN automatic-review).
;;
;; One coherent, guarded transition when a genuine independent reviewer
;; returns an explicit head-bound REQUEST_CHANGES against a PRE-MERGE
;; delivery receipt:
;;
;;   resolve-review-rejection!
;;     1. re-verifies the LIVE campaign record the caller holds (plan,
;;        wave, fence, current attempt identity, no cancellation, no
;;        durable budget pause, wave awaiting delivery review);
;;     2. persists the durable review-rejection authorization in the
;;        delivery journal (exact rejected receipt identity + findings +
;;        the bounded persistent retry accounting; default ceiling 1,
;;        fail-closed when exhausted);
;;     3. marks the rejected attempt superseded (artifact ledger terminal
;;        status + durable failure reason) and moves the wave to the
;;        ORDINARY failed retry status.
;;
;; It NEVER generates the next attempt id or fence: the fresh fenced
;; attempt is created by the ordinary retry machinery (begin-attempt!),
;; so at-least-once semantics, fence monotonicity and attempt accounting
;; stay exactly where they already live. Infrastructure failures, missing
;; artifacts and stale outputs are not rejections and must never call this.
;;
;; Idempotency/restart law: the journal write happens BEFORE the record
;; transition, and replaying the identical rejection after a restart is a
;; no-op (the budget is consumed once); the record-side stamps are stable,
;; so re-deriving them is harmless. The companion cross-attempt receipt
;; supersession lives in delivery-journal.rkt
;; (reconcile-review-rejected-receipt!) and is authorized ONLY by the
;; exact marker written here plus a live record guard.

(require racket/format
         racket/string
         (only-in "campaign-state.rkt"
                  campaign-record?
                  campaign-plan-id
                  campaign-fence-token
                  campaign-record-cancellation
                  campaign-record-budget-pause
                  campaign-record-waves
                  campaign-wave-index
                  campaign-wave-status
                  campaign-wave-current-attempt
                  campaign-attempt-id
                  campaign-attempt-fence-token
                  set-campaign-wave-status!
                  stamp-wave-failure!
                  wave-artifact-ledger
                  campaign-artifact-entry-attempt-id
                  set-campaign-artifact-entry-terminal-status!)
         (only-in "delivery-journal.rkt"
                  load-delivery-journal
                  record-review-rejection!
                  journal-review-rejection
                  journal-review-rejection-count
                  review-rejection-matches-receipt?)
         (only-in "policy.rkt" current-gsd-wave-review-rejection-retries))

(provide resolve-review-rejection!
         review-rejection-resolution
         review-rejection-resolution?
         review-rejection-resolution-status
         review-rejection-resolution-reason
         review-rejection-resolution-marker
         review-rejection-resolution-findings
         review-rejection-resolution-consumed-retries
         review-rejection-resolution-retry-budget
         review-rejection-resolution-record
         review-rejection-refused?
         review-rejection-live-authorized?
         normalize-review-findings)

;; status           — 'resolved | 'refused
;; reason           — #f, or the typed refusal explanation
;; marker           — the durable review-rejection marker hash, or #f
;; findings         — the normalized findings list ('() when unavailable)
;; consumed-retries — the persistent rejection-retry count after the call
;; retry-budget     — the ceiling the call ran under
;; record           — the (possibly transitioned) campaign record; the
;;                    CALLER persists it after a 'resolved result
(struct review-rejection-resolution
        (status reason marker findings consumed-retries retry-budget record)
  #:transparent)

(define (review-rejection-refused? r)
  (and (review-rejection-resolution? r) (eq? (review-rejection-resolution-status r) 'refused)))

;; Findings normalization: a single non-empty string becomes a one-element
;; list; a non-empty list of non-empty strings is kept verbatim; anything
;; else (empty, blank, non-strings) is #f — blank findings can never
;; authorize a durable rejection.
(define (normalize-review-findings v)
  (cond
    [(and (string? v) (positive? (string-length (string-trim v)))) (list v)]
    [(and (list? v) (pair? v) (andmap (lambda (f) (and (string? f) (positive? (string-length f)))) v))
     v]
    [else #f]))

(define (wave-for record wave-idx)
  (for/first ([w (in-list (campaign-record-waves record))]
              #:when (equal? (campaign-wave-index w) wave-idx))
    w))

;; The LIVE record guard for cross-attempt supersession (and for this
;; port's own pre-checks): the record is a campaign record for the exact
;; plan, not durably cancelled, not under a budget pause, and its CURRENT
;; attempt for the wave is exactly (attempt-id, fence). The runtime closes
;; over a fresh (load-campaign-record) call so takeover, cancellation and
;; budget pauses that land mid-flight refuse at the write boundary.
(define (review-rejection-live-authorized? record plan wave attempt-id fence)
  (and (campaign-record? record)
       (equal? (campaign-plan-id record) plan)
       (not (campaign-record-cancellation record))
       (not (campaign-record-budget-pause record))
       (let ([w (and record (wave-for record wave))])
         (and w
              (let ([a (campaign-wave-current-attempt w)])
                (and a
                     (equal? (campaign-attempt-id a) attempt-id)
                     (equal? (campaign-attempt-fence-token a) fence)))))))

;; resolve-review-rejection! : the state port used by the runtime
;; integration. REFUSALS are typed results with ZERO effects (the journal
;; is never written); a 'resolved result means the journal marker is
;; durable and the record was transitioned in place (caller persists).
(define (resolve-review-rejection!
         base
         plan
         wave
         record
         #:findings findings
         #:expected-attempt-id attempt-id
         #:expected-fence fence
         #:max-rejection-retries [max-rejection-retries (current-gsd-wave-review-rejection-retries)])
  (define normalized (normalize-review-findings findings))
  (define (refuse reason)
    (define journal
      (with-handlers ([exn:fail? (lambda (_) #f)])
        (load-delivery-journal base plan wave)))
    (review-rejection-resolution 'refused
                                 reason
                                 (and journal (journal-review-rejection journal))
                                 (or normalized '())
                                 (and journal (journal-review-rejection-count journal))
                                 max-rejection-retries
                                 record))
  (cond
    [(not (campaign-record? record)) (refuse "campaign record is missing")]
    [(not (equal? (campaign-plan-id record) plan)) (refuse "campaign record is for a different plan")]
    [(not (exact-nonnegative-integer? wave)) (refuse "wave index is invalid")]
    [(not (and (string? attempt-id) (positive? (string-length attempt-id))))
     (refuse "expected attempt identity is missing")]
    [(not (exact-nonnegative-integer? fence)) (refuse "expected fence is invalid")]
    [(not (exact-nonnegative-integer? max-rejection-retries)) (refuse "retry budget is invalid")]
    [(campaign-record-cancellation record) (refuse "campaign is durably cancelled")]
    [(campaign-record-budget-pause record) (refuse "campaign budget pause is in force")]
    [(not (equal? (campaign-fence-token record) fence))
     (refuse (format "stale coordinator fence: expected ~a, record holds ~a"
                     fence
                     (campaign-fence-token record)))]
    [else
     (define w (wave-for record wave))
     (cond
       [(not w) (refuse (format "campaign record carries no wave ~a" wave))]
       [else
        (define attempt (campaign-wave-current-attempt w))
        (define journal (load-delivery-journal base plan wave))
        (define marker (and journal (journal-review-rejection journal)))
        ;; Restart idempotency: when the exact marker for THIS attempt is
        ;; already durable, the record transition may be re-derived on a
        ;; reloaded (already failed) wave; without it, only a wave still
        ;; awaiting its delivery review may be rejected.
        (define replay?
          (and journal
               marker
               (not (hash-ref marker 'superseded-by-attempt #f))
               (review-rejection-matches-receipt? marker (hash-ref journal 'receipt #f))
               (equal? (hash-ref marker 'attempt-id #f) attempt-id)))
        (cond
          [(and (not replay?) (not (member (campaign-wave-status w) '(done awaiting-delivery))))
           (refuse (format "wave ~a is not awaiting delivery review (status ~a)"
                           wave
                           (campaign-wave-status w)))]
          [(not attempt) (refuse "the rejected attempt is not the wave's live attempt")]
          [(not (and (equal? (campaign-attempt-id attempt) attempt-id)
                     (equal? (campaign-attempt-fence-token attempt) fence)))
           (refuse (format "live attempt is not the rejected attempt (~a)"
                           (campaign-attempt-id attempt)))]
          [(not normalized) (refuse "findings are missing or malformed")]
          [(not journal) (refuse "no durable delivery journal to reject")]
          [else
           (with-handlers ([exn:fail? (lambda (e) (refuse (exn-message e)))])
             (define saved-journal
               (record-review-rejection! base
                                         plan
                                         wave
                                         #:findings normalized
                                         #:expected-attempt-id attempt-id
                                         #:expected-fence fence
                                         #:max-rejection-retries max-rejection-retries))
             ;; Durable authorization first; record transition second (the
             ;; caller persists the record — a crash in between re-derives
             ;; the transition from the durable marker on the next pass).
             (stamp-wave-failure! w
                                  (format "independent review rejected attempt ~a: ~a"
                                          attempt-id
                                          (string-join normalized "; ")))
             (for ([entry (in-list (wave-artifact-ledger w))]
                   #:when (equal? (campaign-artifact-entry-attempt-id entry) attempt-id))
               (set-campaign-artifact-entry-terminal-status! entry 'superseded))
             (set-campaign-wave-status! w 'failed)
             (review-rejection-resolution 'resolved
                                          #f
                                          (journal-review-rejection saved-journal)
                                          normalized
                                          (journal-review-rejection-count saved-journal)
                                          max-rejection-retries
                                          record))])])]))
