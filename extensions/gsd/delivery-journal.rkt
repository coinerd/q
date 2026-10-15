#lang racket/base
;; Coordinator-owned delivery state. Not campaign DONE/attempt state and NOT
;; delivery proof. JSON permits the trusted GitHub step adapter to consume the
;; same immutable Verify receipt. Callers hold the campaign lease for writes.
(require json
         racket/file
         racket/list
         racket/path
         "review-proof.rkt"
         "author-scope.rkt"
         "evidence-repair-format.rkt")
(provide journal-evidence-author-repair
         evidence-author-repair-pending?
         save!
         review-rejection-allowed-stages
         journal-review-rejection
         review-rejection-matches-receipt?
         context-receipt?
         literal-files?
         text?
         hex?
         delivery-journal-path
         load-delivery-journal
         record-delivery-receipt!
         update-delivery-journal!
         valid-delivery-receipt?
         valid-receipt-history?
         refresh-owned-proof-receipt!
         reconcile-repair-tail-receipt!
         repair-tail-allowed-stages
         reconcile-merged-tip-receipt!
         merged-tip-allowed-stages
         valid-review-rejection?
         valid-review-rejection-history?
         review-rejection-allowed-stages
         review-rejection-matches-receipt?
         journal-review-rejection
         journal-review-rejection-count
         record-review-rejection!
         reconcile-review-rejected-receipt!
         valid-verification-context?
         record-verification-context!
         delivery-stages
         remote-pending-path
         load-remote-pending
         record-remote-pending!
         clear-remote-pending!
         remote-pending-blocker)
(define delivery-stages
  '("context-ready" "implementation-review"
                    "implementation-pr"
                    "implementation-ci"
                    "implementation-merged"
                    "binding-prepared"
                    "binding-review"
                    "binding-pr"
                    "binding-ci"
                    "binding-merged"
                    "governance"
                    "sync"
                    "delivered"
                    "awaiting-approval"
                    "retryable"
                    "blocked"))
(define (hex? s n)
  (and (string? s) (= (string-length s) n) (regexp-match? #px"^[0-9a-f]+$" s)))
(define (text? s)
  (and (string? s) (positive? (string-length s))))
(define (valid-delivery-receipt? r)
  (and (hash? r)
       (andmap (lambda (k) (text? (hash-ref r k #f))) '(repo branch origin evidence))
       (hex? (hash-ref r 'head #f) 40)
       (hex? (hash-ref r 'tree #f) 40)
       (exact-nonnegative-integer? (hash-ref r 'verified-at #f))
       (or (not (hash-has-key? r 'owned-proof))
           (proof-reference-matches-receipt? (hash-ref r 'owned-proof) r))))
;; v1.00.33 W0 repair-tail protocol (audit §21.2): the append-only history
;; of PRIOR immutable receipts preserved by reconcile-repair-tail-receipt!.
;; Malformed history never loads — a journal that cannot prove its own past
;; is refused, not repaired.
(define (valid-receipt-history? h)
  (and (list? h) (andmap valid-delivery-receipt? h)))
;; Automatic independent review (BUG_PLAN automatic-review slice A): the
;; durable review-rejection marker. A genuine independent REQUEST_CHANGES
;; verdict against a PRE-MERGE receipt is recorded exactly once per
;; rejected attempt; the marker names the exact rejected receipt identity
;; (attempt-id, attempt-fence, branch, head), the findings, the journal
;; stage at rejection, and — after a successful cross-attempt supersession
;; — the attempt that superseded it. The marker is the ONLY authorization
;; for reconcile-review-rejected-receipt!; it can never be written through
;; update-delivery-journal! and a malformed marker refuses at load.
(define review-rejection-allowed-stages
  '("context-ready" "implementation-review" "implementation-pr" "implementation-ci"))
(define (valid-review-findings? v)
  (and (list? v) (pair? v) (andmap (lambda (f) (and (string? f) (positive? (string-length f)))) v)))
(define (valid-review-rejection? m)
  (and (hash? m)
       (text? (hash-ref m 'attempt-id #f))
       (exact-nonnegative-integer? (hash-ref m 'attempt-fence #f))
       (text? (hash-ref m 'branch #f))
       (hex? (hash-ref m 'head #f) 40)
       (valid-review-findings? (hash-ref m 'findings #f))
       (member (hash-ref m 'stage #f) review-rejection-allowed-stages)
       (exact-nonnegative-integer? (hash-ref m 'recorded-at #f))
       (let ([by (hash-ref m 'superseded-by-attempt #f)]
             [at (hash-ref m 'superseded-at #f)])
         (or (and (not by) (not at)) (and (text? by) (exact-nonnegative-integer? at))))))
(define (valid-review-rejection-history? h)
  (and (list? h) (andmap valid-review-rejection? h)))
;; The current marker, or #f (absent or — impossibly, given load
;; validation — malformed).
(define (journal-review-rejection journal)
  (define m (and (hash? journal) (hash-ref journal 'review-rejection #f)))
  (and (valid-review-rejection? m) m))
;; The persistent rejection-retry accounting consumed so far (0 default).
(define (journal-review-rejection-count journal)
  (define n (and (hash? journal) (hash-ref journal 'review-rejection-count #f)))
  (if (exact-nonnegative-integer? n) n 0))
;; EXACT authorization test: the marker must name the given receipt's
;; attempt-id, attempt-fence, branch and head verbatim. Anything less would
;; let a stale marker authorize supersession of a different receipt.
(define (review-rejection-matches-receipt? m r)
  (and (valid-review-rejection? m)
       (hash? r)
       (equal? (hash-ref m 'attempt-id #f) (hash-ref r 'attempt-id #f))
       (equal? (hash-ref m 'attempt-fence #f) (hash-ref r 'attempt-fence #f))
       (equal? (hash-ref m 'branch #f) (hash-ref r 'branch #f))
       (equal? (hash-ref m 'head #f) (hash-ref r 'head #f))))
;; Verification context (v1.00.33 audit, directive item 2): the durable slot
;; for the context the verifier actually ran under — repo-root, base, and the
;; snapshot refs (merge-sha / PR head / binding branch). Without it a journal
;; binds only branch/head, and a later ref mismatch is forensics instead of
;; arithmetic. Optional at load (older journals lack it); malformed slots fail
;; closed; written once via record-verification-context!.
(define (valid-verification-context? vc)
  (and (hash? vc)
       (hex? (hash-ref vc 'base #f) 40)
       (hex? (hash-ref vc 'merge-sha #f) 40)
       (hex? (hash-ref vc 'pr-head #f) 40)
       (text? (hash-ref vc 'branch #f))
       (text? (hash-ref vc 'repo-root #f))
       (exact-nonnegative-integer? (hash-ref vc 'verified-at #f))
       (let ([refs (hash-ref vc 'snapshot-refs #f)])
         (and (list? refs) (pair? refs) (andmap text? refs)))))
(define (delivery-journal-path root plan wave)
  (unless (and (hex? plan 64) (exact-nonnegative-integer? wave))
    (error 'delivery-journal "invalid campaign/wave identity"))
  ;; Include ancestors of root, not just descendants: a redirected .planning
  ;; or checkout cannot place an authorization receipt outside its namespace.
  (define path
    (build-path (path->complete-path root)
                ".planning"
                "campaigns"
                plan
                (format "coordinator-w~a.json" wave)))
  (for/fold ([part (build-path "/")]) ([piece (in-list (cdr (explode-path path)))])
    (define next (build-path part piece))
    (when (link-exists? next)
      (error 'delivery-journal "symlinked journal path refused"))
    next)
  path)
(define (load-delivery-journal root plan wave)
  (define path (delivery-journal-path root plan wave))
  (and (file-exists? path)
       (let ([data (call-with-input-file path
                                         (lambda (in)
                                           (define v (read-json in))
                                           (unless (eof-object? (read-json in))
                                             (error 'delivery-journal "multiple journal values"))
                                           v))])
         (unless (and (hash? data)
                      (equal? (hash-ref data 'schema-version #f) 1)
                      (equal? (hash-ref data 'plan-id #f) plan)
                      (equal? (hash-ref data 'wave #f) wave)
                      (member (hash-ref data 'stage #f) delivery-stages)
                      (valid-delivery-receipt? (hash-ref data 'receipt #f))
                      (or (not (hash-has-key? data 'evidence-author-repair))
                          (author-marker-matches-journal? (hash-ref data 'evidence-author-repair)
                                                          data))
                      (let ([h (hash-ref data 'receipt-history #f)])
                        (or (not h) (valid-receipt-history? h)))
                      (let ([m (hash-ref data 'review-rejection #f)])
                        (or (not m) (valid-review-rejection? m)))
                      (let ([rh (hash-ref data 'review-rejection-history #f)])
                        (or (not rh) (valid-review-rejection-history? rh)))
                      (let ([rc (hash-ref data 'review-rejection-count #f)])
                        (or (not rc) (exact-nonnegative-integer? rc)))
                      (let ([vc (hash-ref data 'verification-context #f)])
                        (or (not vc) (valid-verification-context? vc))))
           (error 'delivery-journal "invalid journal; refusing to overwrite"))
         data)))
(define (save! root plan wave data)
  (define path (delivery-journal-path root plan wave))
  (make-parent-directory* path)
  (call-with-atomic-output-file path
                                (lambda (out _)
                                  (write-json data out)
                                  (newline out)))
  data)
(define (record-delivery-receipt! root plan wave receipt)
  (unless (valid-delivery-receipt? receipt)
    (error 'delivery-journal "invalid Verify receipt"))
  (validate-receipt-owned-proof! receipt root plan wave)
  (define old (load-delivery-journal root plan wave))
  (cond
    [old
     (unless (equal? (hash-ref old 'receipt) receipt)
       (error 'delivery-journal "verified provenance changed; explicit reconciliation required"))
     old]
    [else
     (save! root
            plan
            wave
            (hasheq 'schema-version
                    1
                    'plan-id
                    plan
                    'wave
                    wave
                    'stage
                    "context-ready"
                    'receipt
                    receipt
                    'model-calls
                    0
                    'tokens
                    0
                    'cost
                    0
                    'usage-missing
                    #f))]))
(define (update-delivery-journal! root plan wave fields)
  (unless (and (hash? fields)
               (not (ormap (lambda (k) (hash-has-key? fields k))
                           '(receipt plan-id
                                     wave
                                     schema-version
                                     receipt-history
                                     evidence-author-repair
                                     review-rejection
                                     review-rejection-history
                                     review-rejection-count)))
               (or (not (hash-has-key? fields 'stage))
                   (member (hash-ref fields 'stage) delivery-stages)))
    (error 'delivery-journal "invalid journal update"))
  (define old (load-delivery-journal root plan wave))
  (unless old
    (error 'delivery-journal "missing verified provenance"))
  (save! root
         plan
         wave
         (for/fold ([data old]) ([(k v) (in-hash fields)])
           (hash-set data k v))))

;; v1.00.33 W0 repair-tail protocol (audit §21.2): the explicit, atomic,
;; fenced SAME-ATTEMPT receipt reconciliation transition.
;;
;; A parked attempt whose delivery branch gained repair commits past the
;; recorded receipt head needs a SUPPORTED path to a new receipt — but never
;; a silent overwrite and never a marker clear on a mismatched receipt. The
;; transition requires, and records atomically in ONE save:
;;   * the SAME plan/wave (journal identity);
;;   * the SAME attempt-id and fence on the old receipt, the new receipt and
;;     the caller's expected binding (the live attempt resolved by the
;;     production wrapper — never inferred);
;;   * the SAME non-protected delivery branch;
;;   * a strictly-DESCENDANT new head (a repair tail, proven by the
;;     caller-supplied ancestry predicate — real git in production);
;;   * a journal stage strictly BEFORE implementation-merged (the merge
;;     provenance is not yet anchored).
;; Effect: the prior receipt is appended to the immutable `receipt-history`
;; (load-validated, update-refused) and the new receipt becomes the current
;; one. `save!` is atomic, so a crash leaves either the old or the new
;; journal, never a mixture; a retry at the already-reconciled head is the
;; caller's same-head idempotent path, and a repeat of this primitive at an
;; equal head refuses (not a repair tail).
(define repair-tail-allowed-stages
  '("context-ready" "implementation-review" "implementation-pr" "implementation-ci"))

;; Explicit same-source proof upgrade. The opaque authorization can only be
;; minted by a fresh owned terminal capture, never by reading a JSON file.
;; Caller holds the campaign lease; guard must check live attempt/cancellation
;; and source identity. Exact old receipt is a compare-and-swap precondition.
(define proof-refresh-lock (make-semaphore 1))
(define (refresh-owned-proof-receipt! root
                                      plan
                                      wave
                                      new-receipt
                                      #:old-receipt expected-old
                                      #:authorization authorization
                                      #:guard guard)
  (call-with-semaphore proof-refresh-lock
                       (lambda ()
                         (refresh-owned-proof-receipt/core! root
                                                            plan
                                                            wave
                                                            new-receipt
                                                            expected-old
                                                            authorization
                                                            guard))))
(define (refresh-owned-proof-receipt/core! root
                                           plan
                                           wave
                                           new-receipt
                                           expected-old
                                           authorization
                                           guard)
  (unless (and (valid-delivery-receipt? new-receipt)
               (procedure? guard)
               (procedure-arity-includes? guard 0))
    (error 'delivery-journal "invalid proof refresh/mandatory live guard"))
  (define old (load-delivery-journal root plan wave))
  (unless (and old
               (equal? (hash-ref old 'receipt) expected-old)
               (member (hash-ref old 'stage) repair-tail-allowed-stages)
               (not (journal-review-rejection old)))
    (error 'delivery-journal "stale, rejected or postmerge proof refresh"))
  (unless (for/and ([k '(repo branch origin head tree attempt-id attempt-fence)])
            (equal? (hash-ref expected-old k #f) (hash-ref new-receipt k #f)))
    (error 'delivery-journal "proof refresh source/attempt changed"))
  (validate-receipt-owned-proof! new-receipt root plan wave)
  (define ref (hash-ref new-receipt 'owned-proof #f))
  (unless (and (proof-reference-matches-receipt? ref new-receipt)
               (equal? (hash-ref (hash-ref ref 'binding) 'plan-id) plan)
               (equal? (hash-ref (hash-ref ref 'binding) 'wave) wave)
               (guard))
    (error 'delivery-journal "proof refresh lacks exact live owned proof"))
  (cond
    [(equal? ref (hash-ref expected-old 'owned-proof #f)) old]
    [else
     (unless (consume-owned-proof-refresh! authorization ref)
       (error 'delivery-journal "proof refresh requires fresh owner authorization"))
     (unless (and (equal? old (load-delivery-journal root plan wave)) (guard))
       (error 'delivery-journal "proof refresh raced or live guard refused"))
     (save! root
            plan
            wave
            (hash-set* old
                       'receipt
                       new-receipt
                       'receipt-history
                       (append (hash-ref old 'receipt-history '()) (list expected-old))
                       'stage
                       "context-ready"))]))

;; BUG-0083: the post-merge analogue. A delivery branch whose implementation
;; PR was lawfully merged at an evidence-only drift tip PAST the recorded
;; receipt head (the resolve/merge drift tolerance — production shape of
;; campaign 79a69b42 W0) needs the receipt to advance to that tip (or a later
;; republication head) before the binding chain can authenticate it; the
;; binding contract requires receipt head == merged PR head. This transition
;; runs ONLY at stage implementation-merged (the merge identity is anchored;
;; pre-merge stages keep using the repair-tail transition; binding/delivered
;; stages stay terminal), reuses every identity/descendant/history guarantee,
;; and additionally demands a caller-supplied evidence-only tail predicate —
;; post-merge the tail over the receipt must be provably evidence-only
;; (real git in the CLI boundary), never asserted on trust.
(define merged-tip-allowed-stages '("implementation-merged"))

;; Shared fail-closed core of the two same-attempt receipt reconciliations.
;; LABEL names the transition in refusals; TAIL-EVIDENCE-ONLY? is an
;; additional mandatory predicate over (old-head new-head) — the repair-tail
;; wrapper passes a trivially-true one to preserve its historical contract.
(define (reconcile-receipt-tail! root
                                 plan
                                 wave
                                 new-receipt
                                 expected-attempt-id
                                 fence
                                 head-ancestor?
                                 tail-evidence-only?
                                 allowed
                                 label)
  (define (refuse! reason)
    (error 'delivery-journal (format "~a reconciliation refused: ~a" label reason)))
  (unless (valid-delivery-receipt? new-receipt)
    (refuse! "new receipt is invalid"))
  (validate-receipt-owned-proof! new-receipt root plan wave)
  (unless (and (string? expected-attempt-id) (positive? (string-length expected-attempt-id)))
    (refuse! "expected attempt identity is missing"))
  (unless (exact-nonnegative-integer? fence)
    (refuse! "expected fence is invalid"))
  (unless (and (procedure? head-ancestor?) (procedure-arity-includes? head-ancestor? 2))
    (refuse! "ancestry predicate is missing"))
  (unless (and (procedure? tail-evidence-only?) (procedure-arity-includes? tail-evidence-only? 2))
    (refuse! "evidence-only tail predicate is missing"))
  (define old (load-delivery-journal root plan wave))
  (define old-receipt (and old (hash-ref old 'receipt #f)))
  (unless old
    (refuse! "no durable journal to reconcile"))
  (unless (hash? old-receipt)
    (refuse! "journal carries no receipt"))
  (unless (equal? (hash-ref old-receipt 'attempt-id #f) expected-attempt-id)
    (refuse! "attempt identity changed"))
  (unless (equal? (hash-ref old-receipt 'attempt-fence #f) fence)
    (refuse! "attempt fence changed"))
  (unless (equal? (hash-ref new-receipt 'attempt-id #f) expected-attempt-id)
    (refuse! "new receipt is bound to a different attempt"))
  (unless (equal? (hash-ref new-receipt 'attempt-fence #f) fence)
    (refuse! "new receipt carries a different fence"))
  (define old-branch (hash-ref old-receipt 'branch #f))
  (define new-branch (hash-ref new-receipt 'branch #f))
  (unless (and (string? old-branch)
               (equal? old-branch new-branch)
               (not (member new-branch '("main" "master"))))
    (refuse! "delivery branch identity changed"))
  (define old-head-sha (hash-ref old-receipt 'head #f))
  (define new-head-sha (hash-ref new-receipt 'head #f))
  (unless (and (hex? old-head-sha 40) (hex? new-head-sha 40))
    (refuse! "receipt heads are malformed"))
  (when (string=? old-head-sha new-head-sha)
    (refuse! "heads are equal; not a repair tail"))
  ;; An execution proof certifies its own tree, not a later merge/tail tree.
  ;; A caller must supply a newly owned proof for the new head or omit it.
  (when (and (hash-has-key? new-receipt 'owned-proof)
             (equal? (hash-ref new-receipt 'owned-proof) (hash-ref old-receipt 'owned-proof #f)))
    (refuse! "execution proof cannot be carried to another head"))
  (define stage (hash-ref old 'stage #f))
  (unless (member stage allowed)
    (refuse! (format "stage ~a is terminal for ~a reconciliation" stage label)))
  (unless (head-ancestor? old-head-sha new-head-sha)
    (refuse! "new head is not a descendant of the receipt head"))
  (unless (tail-evidence-only? old-head-sha new-head-sha)
    (refuse! "tail over the receipt head is not evidence-only"))
  (define history (hash-ref old 'receipt-history '()))
  (unless (valid-receipt-history? history)
    (refuse! "existing receipt history is malformed"))
  (save! root
         plan
         wave
         (hash-set* old 'receipt new-receipt 'receipt-history (append history (list old-receipt))))
  new-receipt)

(define (reconcile-repair-tail-receipt! root
                                        plan
                                        wave
                                        new-receipt
                                        #:expected-attempt-id attempt-id
                                        #:expected-fence fence
                                        #:head-ancestor? head-ancestor?
                                        #:allowed-stages [allowed repair-tail-allowed-stages])
  (reconcile-receipt-tail! root
                           plan
                           wave
                           new-receipt
                           attempt-id
                           fence
                           head-ancestor?
                           (lambda (_old _new) #t)
                           allowed
                           'repair-tail))

(define (reconcile-merged-tip-receipt! root
                                       plan
                                       wave
                                       new-receipt
                                       #:expected-attempt-id attempt-id
                                       #:expected-fence fence
                                       #:head-ancestor? head-ancestor?
                                       #:tail-evidence-only? tail-evidence-only?
                                       #:allowed-stages [allowed merged-tip-allowed-stages])
  (reconcile-receipt-tail! root
                           plan
                           wave
                           new-receipt
                           attempt-id
                           fence
                           head-ancestor?
                           tail-evidence-only?
                           allowed
                           'merged-tip))

;; Automatic independent review (slice A) part 1: record the durable
;; explicit rejection authorization for the CURRENT pre-merge receipt.
;; Requires, and records atomically in ONE save:
;;   * an existing journal whose receipt is bound to the EXACT expected
;;     attempt-id and fence (the live rejected attempt — never inferred);
;;   * a journal stage strictly BEFORE implementation-merged (postmerge
;;     rejections repair evidence review instead — fail closed here);
;;   * genuine structured findings (non-empty list of non-empty strings;
;;     missing-output/infra failures are NOT rejections and never reach here).
;; Effects, in the same atomic save:
;;   * the `review-rejection` marker naming the exact rejected receipt
;;     identity + findings + stage (the only key that can later authorize a
;;     cross-attempt supersession);
;;   * `review-rejection-count` incremented by exactly one (the persistent
;;     bounded retry accounting — default ceiling 1, enforced BEFORE the
;;     write; exhausted budgets refuse with zero effects);
;;   * a prior superseded marker is demoted into the append-only
;;     `review-rejection-history` (immutable, load-validated).
;; Idempotency/restart law: replaying the IDENTICAL rejection (same receipt
;; identity + same findings) returns the journal unchanged — the budget is
;; never double-consumed across restarts. A DIFFERENT rejection against the
;; same receipt is a reconciliation event and refuses.
(define (record-review-rejection! root
                                  plan
                                  wave
                                  #:findings findings
                                  #:expected-attempt-id attempt-id
                                  #:expected-fence fence
                                  #:max-rejection-retries max-rejection-retries)
  (define (refuse! reason)
    (error 'delivery-journal (format "review rejection refused: ~a" reason)))
  (unless (valid-review-findings? findings)
    (refuse! "findings are missing or malformed"))
  (unless (and (string? attempt-id) (positive? (string-length attempt-id)))
    (refuse! "expected attempt identity is missing"))
  (unless (exact-nonnegative-integer? fence)
    (refuse! "expected fence is invalid"))
  (unless (exact-nonnegative-integer? max-rejection-retries)
    (refuse! "retry budget is invalid"))
  (define old (load-delivery-journal root plan wave))
  (define old-receipt (and old (hash-ref old 'receipt #f)))
  (unless old
    (refuse! "no durable journal to record the rejection against"))
  (unless (hash? old-receipt)
    (refuse! "journal carries no receipt"))
  (unless (equal? (hash-ref old-receipt 'attempt-id #f) attempt-id)
    (refuse! "attempt identity changed"))
  (unless (equal? (hash-ref old-receipt 'attempt-fence #f) fence)
    (refuse! "attempt fence changed"))
  (define stage (hash-ref old 'stage #f))
  (unless (member stage review-rejection-allowed-stages)
    (refuse! (format "stage ~a is terminal for review rejection" stage)))
  (define prior (hash-ref old 'review-rejection #f))
  (cond
    [(and (valid-review-rejection? prior)
          (review-rejection-matches-receipt? prior old-receipt)
          (not (hash-ref prior 'superseded-by-attempt #f))
          (equal? (hash-ref prior 'findings #f) findings))
     ;; identical replay (restart): nothing written, budget not re-consumed
     old]
    [(and (valid-review-rejection? prior) (not (hash-ref prior 'superseded-by-attempt #f)))
     (refuse! "a different rejection is already recorded for this receipt")]
    [else
     (define count (journal-review-rejection-count old))
     (when (>= count max-rejection-retries)
       (refuse!
        (format "review-rejection retry budget exhausted (~a/~a)" count max-rejection-retries)))
     (define history (hash-ref old 'review-rejection-history '()))
     (unless (valid-review-rejection-history? history)
       (refuse! "existing rejection history is malformed"))
     (save! root
            plan
            wave
            (hash-set* old
                       'review-rejection
                       (hasheq 'schema-version
                               1
                               'attempt-id
                               attempt-id
                               'attempt-fence
                               fence
                               'branch
                               (hash-ref old-receipt 'branch)
                               'head
                               (hash-ref old-receipt 'head)
                               'findings
                               findings
                               'stage
                               stage
                               'recorded-at
                               (current-seconds))
                       'review-rejection-history
                       (if (valid-review-rejection? prior)
                           (append history (list prior))
                           history)
                       'review-rejection-count
                       (add1 count)))
     (load-delivery-journal root plan wave)]))

;; Automatic independent review (slice A) part 2: the EXPLICIT cross-attempt
;; receipt supersession. Authorized ONLY by a prior EXACT durable rejection
;; marker (the marker must name the current receipt's attempt-id, fence,
;; branch and head verbatim — never a stale or different receipt) plus a
;; caller-supplied LIVE record guard (procedure of no arguments, re-run
;; immediately before the write; in production it proves the campaign record
;; is uncancelled, within budget and its CURRENT attempt is exactly the new
;; receipt's attempt — see review-rejection.rkt). Additional fail-closed
;; guards:
;;   * the journal stage is strictly BEFORE implementation-merged (a merged
;;     implementation cannot be undone; postmerge transitions refuse);
;;   * the new receipt is attempt-bound to a DIFFERENT attempt (same-attempt
;;     drift stays on the repair-tail protocol);
;;   * the new receipt keeps the SAME non-protected delivery branch
;;     (stale/different branch identities refuse);
;;   * the existing receipt history is well-formed.
;; Effect (ONE atomic save): the rejected receipt is appended to the
;; immutable `receipt-history` (generation +1 exactly once), the new VERIFIED
;; receipt becomes current, the stage RESETS to context-ready (the ladder
;; re-runs preflight → review on the new receipt) and the marker records
;; `superseded-by-attempt`/`superseded-at`. The old receipt and its frozen
;; evidence trio are never deleted or rewritten. Replaying the transition at
;; the already-superseded identity is idempotent (no second history append,
;; no stage churn); any other replay refuses.
(define (reconcile-review-rejected-receipt! root
                                            plan
                                            wave
                                            new-receipt
                                            #:guard guard
                                            #:allowed-stages
                                            [allowed review-rejection-allowed-stages])
  (define (refuse! reason)
    (error 'delivery-journal (format "review-rejected receipt supersession refused: ~a" reason)))
  (unless (valid-delivery-receipt? new-receipt)
    (refuse! "new receipt is invalid"))
  (validate-receipt-owned-proof! new-receipt root plan wave)
  (unless (text? (hash-ref new-receipt 'attempt-id #f))
    (refuse! "new receipt is not attempt-bound"))
  (unless (exact-nonnegative-integer? (hash-ref new-receipt 'attempt-fence #f))
    (refuse! "new receipt fence is invalid"))
  (unless (and (procedure? guard) (procedure-arity-includes? guard 0))
    (refuse! "live record guard is missing"))
  (define old (load-delivery-journal root plan wave))
  (define old-receipt (and old (hash-ref old 'receipt #f)))
  (unless old
    (refuse! "no durable journal to reconcile"))
  (unless (hash? old-receipt)
    (refuse! "journal carries no receipt"))
  (define marker (hash-ref old 'review-rejection #f))
  (unless (valid-review-rejection? marker)
    (refuse! "no durable review rejection authorizes the current receipt"))
  (cond
    [(and (equal? (hash-ref old-receipt 'attempt-id #f) (hash-ref new-receipt 'attempt-id #f))
          (equal? (hash-ref old-receipt 'attempt-fence #f) (hash-ref new-receipt 'attempt-fence #f))
          (equal? (hash-ref old-receipt 'head #f) (hash-ref new-receipt 'head #f))
          (equal? (hash-ref old-receipt 'branch #f) (hash-ref new-receipt 'branch #f))
          (equal? (hash-ref marker 'superseded-by-attempt #f) (hash-ref new-receipt 'attempt-id #f)))
     ;; already superseded by exactly this attempt: idempotent no-op
     old]
    [(hash-ref marker 'superseded-by-attempt #f)
     (refuse! "the recorded rejection is already superseded")]
    [else
     (unless (review-rejection-matches-receipt? marker old-receipt)
       (refuse! "no durable review rejection authorizes the current receipt"))
     (when (equal? (hash-ref old-receipt 'attempt-id #f) (hash-ref new-receipt 'attempt-id #f))
       (refuse! "same-attempt drift belongs to the repair-tail reconciliation"))
     (define old-branch (hash-ref old-receipt 'branch #f))
     (define new-branch (hash-ref new-receipt 'branch #f))
     (unless (and (string? old-branch)
                  (equal? old-branch new-branch)
                  (not (member new-branch '("main" "master"))))
       (refuse! "delivery branch identity changed"))
     (define stage (hash-ref old 'stage #f))
     (unless (member stage allowed)
       (refuse! (format "stage ~a is terminal for supersession" stage)))
     (define history (hash-ref old 'receipt-history '()))
     (unless (valid-receipt-history? history)
       (refuse! "existing receipt history is malformed"))
     (unless (guard)
       (refuse! "live record guard refused"))
     (save! root
            plan
            wave
            (hash-set* old
                       'receipt
                       new-receipt
                       'receipt-history
                       (append history (list old-receipt))
                       'stage
                       "context-ready"
                       'review-rejection
                       (hash-set* marker
                                  'superseded-by-attempt
                                  (hash-ref new-receipt 'attempt-id)
                                  'superseded-at
                                  (current-seconds))))
     (load-delivery-journal root plan wave)]))

;; Independent evidence authoring has a permanent, separate one-per-wave slot.
(define (literal-files? fs)
  (and (list? fs)
       (pair? fs)
       (= (length fs) (length (remove-duplicates fs)))
       (andmap (lambda (f)
                 (and (text? f)
                      (regexp-match? #px"^[A-Za-z0-9_.-]+(?:/[A-Za-z0-9_.-]+)*$" f)
                      (not (regexp-match? #px"(?:^|/)[.][.]?(?:/|$)" f))))
               fs)))
(define (context-receipt? c r plan wave generation)
  (and (valid-evidence-context? c)
       (valid-delivery-receipt? r)
       (equal? (hash-ref c 'plan-id) plan)
       (equal? (hash-ref c 'wave) wave)
       (equal? (hash-ref c 'receipt-generation) generation)
       (equal? (hash-ref c 'receipt-sha256) (canonical-evidence-digest r))
       (valid-owned-proof-reference? (hash-ref r 'owned-proof #f))
       (for/and ([k '(attempt-id attempt-fence branch origin head tree)])
         (equal? (hash-ref c k) (hash-ref r k #f)))
       (equal? (hash-ref c 'green-proof-sha256)
               (hash-ref (hash-ref r 'owned-proof (hasheq)) 'sha256 #f))))
(define (valid-author-marker? m plan wave)
  (and (hash? m)
       (equal? (hash-ref m 'schema-version #f) 1)
       (member (hash-ref m 'state #f) '("pending" "running" "completed"))
       (literal-files? (hash-ref m 'frozen-files #f))
       (let ([c (hash-ref m 'context #f)]
             [r (hash-ref m 'old-receipt #f)])
         (and (valid-evidence-context? c)
              (context-receipt? c r plan wave (hash-ref c 'receipt-generation))
              (or (equal? (hash-ref m 'state) "pending")
                  (and (text? (hash-ref m 'successor-attempt #f))
                       (not (equal? (hash-ref m 'successor-attempt) (hash-ref c 'attempt-id)))
                       (exact-nonnegative-integer? (hash-ref m 'successor-fence #f))
                       (>= (hash-ref m 'successor-fence) (hash-ref c 'attempt-fence))))))
       (or (not (equal? (hash-ref m 'state) "completed"))
           (hex? (hash-ref m 'successor-receipt-sha256 #f) 64))))
(define (author-marker-matches-journal? m j)
  (and (valid-author-marker? m (hash-ref j 'plan-id #f) (hash-ref j 'wave #f))
       (let* ([history (hash-ref j 'receipt-history '())]
              [receipts (append history (list (hash-ref j 'receipt)))]
              [generation (hash-ref (hash-ref m 'context) 'receipt-generation)])
         (and (< generation (length receipts))
              (equal? (list-ref receipts generation) (hash-ref m 'old-receipt))
              (if (equal? (hash-ref m 'state) "completed")
                  (and (< (add1 generation) (length receipts))
                       (let ([successor (list-ref receipts (add1 generation))])
                         (and (equal? (hash-ref successor 'attempt-id #f)
                                      (hash-ref m 'successor-attempt))
                              (equal? (hash-ref successor 'attempt-fence #f)
                                      (hash-ref m 'successor-fence))
                              (equal? (canonical-evidence-digest successor)
                                      (hash-ref m 'successor-receipt-sha256)))))
                  (= generation (length history)))))))
(define (journal-evidence-author-repair j)
  (define m (and (hash? j) (hash-ref j 'evidence-author-repair #f)))
  (and m (author-marker-matches-journal? m j) m))
(define (evidence-author-repair-pending? j)
  (define m (journal-evidence-author-repair j))
  (and m
       (member (hash-ref m 'state) '("pending" "running"))
       (equal? (hash-ref m 'old-receipt) (hash-ref j 'receipt #f))
       #t))


;; Write-once verification context. Requires an existing verified journal
;; (the context only means something against a recorded receipt); identical
;; re-records are idempotent; a differing value is a reconciliation event,
;; never a silent overwrite — same rule as the receipt itself.
(define (record-verification-context! root plan wave vc)
  (unless (valid-verification-context? vc)
    (error 'delivery-journal "invalid verification context"))
  (define old (load-delivery-journal root plan wave))
  (unless old
    (error 'delivery-journal "missing verified provenance"))
  (define prior (hash-ref old 'verification-context #f))
  (cond
    [(not prior) (save! root plan wave (hash-set old 'verification-context vc))]
    [(equal? prior vc) old]
    [else
     (error 'delivery-journal "verification context changed; explicit reconciliation required")]))

;; ============================================================
;; Remote-backing marker (v1.00.31 W3, register F5)
;; ============================================================
;; The journal's load validation REQUIRES a verified receipt, so an
;; unpublished branch can never park its state in the journal itself.
;; The typed remote-pending marker is a SIBLING record: schema-1, exact
;; campaign/wave identity, the unpushed branch/head and the reason. Its
;; presence is a typed ladder-entry refusal naming branch and head; it is
;; cleared automatically the moment the receipt is recorded (or already
;; durable), so "push, re-verify, deliver" is the only way forward.

(define (remote-pending-path root plan wave)
  (unless (and (hex? plan 64) (exact-nonnegative-integer? wave))
    (error 'delivery-journal "invalid campaign/wave identity"))
  (build-path (path->complete-path root)
              ".planning"
              "campaigns"
              plan
              (format "coordinator-w~a.remote-pending.json" wave)))

(define (load-remote-pending root plan wave)
  (define path (remote-pending-path root plan wave))
  (and (file-exists? path)
       (with-handlers ([exn:fail? (lambda (_) #f)])
         (define data (call-with-input-file path read-json))
         (and (hash? data)
              (equal? (hash-ref data 'schema-version #f) 1)
              (equal? (hash-ref data 'plan-id #f) plan)
              (equal? (hash-ref data 'wave #f) wave)
              (text? (hash-ref data 'branch #f))
              (hex? (hash-ref data 'head #f) 40)
              (string? (hash-ref data 'reason #f))
              data))))

(define (record-remote-pending! root plan wave branch head reason)
  (unless (and (text? branch) (hex? head 40) (string? reason) (positive? (string-length reason)))
    (error 'delivery-journal "invalid remote-pending marker"))
  (define path (remote-pending-path root plan wave))
  (when (link-exists? path)
    (error 'delivery-journal "symlinked remote-pending marker refused"))
  (make-parent-directory* path)
  (call-with-atomic-output-file path
                                (lambda (out _)
                                  (write-json (hasheq 'schema-version
                                                      1
                                                      'plan-id
                                                      plan
                                                      'wave
                                                      wave
                                                      'branch
                                                      branch
                                                      'head
                                                      head
                                                      'at
                                                      (current-seconds)
                                                      'reason
                                                      reason)
                                              out)
                                  (newline out))))

(define (clear-remote-pending! root plan wave)
  (define path (remote-pending-path root plan wave))
  (when (file-exists? path)
    (delete-file path)))

;; The typed ladder-entry gate: (remote-pending-blocker root plan wave)
;; returns 'branch-not-published exactly when the marker exists, else #f.
;; Callers that carry the marker hash can name branch and head in the
;; operator-facing refusal (see delivery-coordinator).
(define (remote-pending-blocker root plan wave)
  (and (load-remote-pending root plan wave) 'branch-not-published))
