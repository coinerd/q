#lang racket/base

;; extensions/gsd/supplemental-disposition-decision.rkt — pure disposition gate (CL4 #9981)
;;
;; The single decision core for "may this exact parent wave be dispositioned?".
;; It consumes already-authenticated immutable facts supplied by the adapter
;; (extensions/gsd/supplemental-evidence-disposition.rkt) and returns one typed
;; decision. Invariants enforced by tests/test-gsd-supplemental-disposition-decision-purity.rkt:
;;
;;   * transitive import closure is {racket/base} — no storage, authority, lease,
;;     journal, receipt, proof or git module may ever appear here;
;;   * no observation of repository state, no clocks, no subprocesses, no ports;
;;   * authorization stays in the adapter: an `allow` never authenticates
;;     anything, it only says the supplied facts are mutually consistent.
;;
;; Refusal order is the contract. Stage E runs in two phases because the source/
;; context reads must NOT happen when lineage facts already refuse:
;;
;;   phase E1–E4  decide-disposition-lineage   request + amendment + terminal + campaign
;;   phase E5–E6  decide-disposition-evidence  context + source equality
;;
;; Messages are owned here so the adapter raises exactly one copy of each text.

(provide disposition-request-view
         disposition-amendment-view
         disposition-terminal-view
         disposition-campaign-view
         disposition-context-view
         disposition-source-view
         disposition-decision
         disposition-decision?
         disposition-decision-status
         disposition-decision-code
         disposition-decision-message
         decide-disposition-lineage
         decide-disposition-evidence)

;; ---------------------------------------------------------------- views --

;; One proposal/install request as the adapter received it. `expected-pointer` is
;; the canonical policy pointer the adapter derived from the fixed amendment, so
;; this core never needs to import the (effectful) policy module.
(struct disposition-request-view (plan-id wave pointer expected-pointer) #:transparent)

;; The fixed amendment generation the request must belong to.
(struct disposition-amendment-view
        (plan-id wave
                 snapshot-sha256
                 parent-proposal
                 parent-approval
                 terminal-body-sha256
                 terminal-record-sha256
                 allocation)
  #:transparent)

;; Authenticated parent inspection results (the adapter computes the record digest
;; and the late-terminal admission; this core only compares them).
(struct disposition-terminal-view
        (proposal terminal-sha256 terminal-record-digest allocation late-terminal?)
  #:transparent)

;; Durable campaign facts for the wave/attempt the amendment binds.
(struct disposition-campaign-view
        (record-found? wave-found?
                       attempt-found?
                       status
                       cancelled?
                       budget-paused?
                       attempt-id
                       attempt-fence
                       coordinator-fence)
  #:transparent)

;; Evidence context equality (expected is the authenticated live/preserved context).
(struct disposition-context-view (plan-id wave snapshot-sha256 expected actual) #:transparent)

;; Retained source and receipt-owned proof equality. Flags are computed by the
;; adapter because they depend on preserved-mode expectations.
(struct disposition-source-view
        (source-found? proof-found? execution-cwd context-repo refs-match? head-match? tree-match?)
  #:transparent)

(struct disposition-decision (status code message) #:transparent)

;; --------------------------------------------------------- refusal texts --

(define msg-outside-amendment "outside the fixed disposition amendment")
(define msg-terminal-drift "parent terminal or digest namespace changed")
(define msg-wrong-allocation "wrong failed parent allocation")
(define msg-campaign-state
  (string-append "parent must own current failed (or exact fixed late awaiting-delivery) "
                 "uncancelled unpaused attempt and fences"))
(define msg-context-changed "parent context, frozen snapshot or receipt changed")
(define msg-source-changed "retained clean source or authenticated owned receipt proof changed")

(define (deny code message)
  (disposition-decision 'refuse code message))
(define (allow)
  (disposition-decision 'allow 'eligible #f))

;; ------------------------------------------------- phase E1–E4: lineage --

(define (decide-disposition-lineage request amendment terminal campaign)
  (define plan-id (disposition-request-view-plan-id request))
  (define wave (disposition-request-view-wave request))
  (define allocation (disposition-amendment-view-allocation amendment))
  (cond
    [(not (and (equal? plan-id (disposition-amendment-view-plan-id amendment))
               (equal? wave (disposition-amendment-view-wave amendment))
               (equal? (disposition-request-view-pointer request)
                       (disposition-request-view-expected-pointer request))))
     (deny 'outside-amendment msg-outside-amendment)]
    [(not (and (equal? (disposition-terminal-view-proposal terminal)
                       (disposition-amendment-view-parent-proposal amendment))
               (equal? (disposition-terminal-view-terminal-sha256 terminal)
                       (disposition-amendment-view-terminal-body-sha256 amendment))
               (equal? (disposition-terminal-view-terminal-record-digest terminal)
                       (disposition-amendment-view-terminal-record-sha256 amendment))))
     (deny 'terminal-drift msg-terminal-drift)]
    [(not (equal? (disposition-terminal-view-allocation terminal) allocation))
     (deny 'wrong-allocation msg-wrong-allocation)]
    [else (decide-campaign-state campaign allocation terminal)]))

(define (decide-campaign-state campaign allocation terminal)
  (define late? (disposition-terminal-view-late-terminal? terminal))
  (define status (disposition-campaign-view-status campaign))
  (cond
    [(not (and (disposition-campaign-view-record-found? campaign)
               (disposition-campaign-view-wave-found? campaign)
               (disposition-campaign-view-attempt-found? campaign)
               (or (eq? status 'failed) (and late? (eq? status 'awaiting-delivery)))
               (not (disposition-campaign-view-cancelled? campaign))
               (not (disposition-campaign-view-budget-paused? campaign))
               (equal? (disposition-campaign-view-attempt-id campaign)
                       (hash-ref allocation 'attempt-id))
               (equal? (disposition-campaign-view-attempt-fence campaign)
                       (hash-ref allocation 'attempt-fence))
               (equal? (disposition-campaign-view-coordinator-fence campaign)
                       (hash-ref allocation 'coordinator-fence))))
     (deny 'campaign-state msg-campaign-state)]
    [else (allow)]))

;; ------------------------------------------------ phase E5–E6: evidence --

(define (decide-disposition-evidence amendment context source)
  (cond
    [(not (and (equal? (disposition-context-view-plan-id context)
                       (disposition-amendment-view-plan-id amendment))
               (equal? (disposition-context-view-wave context)
                       (disposition-amendment-view-wave amendment))
               (equal? (disposition-context-view-snapshot-sha256 context)
                       (disposition-amendment-view-snapshot-sha256 amendment))
               (equal? (disposition-context-view-actual context)
                       (disposition-context-view-expected context))))
     (deny 'context-changed msg-context-changed)]
    [(not (and (disposition-source-view-source-found? source)
               (disposition-source-view-proof-found? source)
               (equal? (disposition-source-view-execution-cwd source)
                       (disposition-source-view-context-repo source))
               (disposition-source-view-refs-match? source)
               (disposition-source-view-head-match? source)
               (disposition-source-view-tree-match? source)))
     (deny 'source-changed msg-source-changed)]
    [else (allow)]))
