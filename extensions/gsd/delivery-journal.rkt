#lang racket/base
;; Coordinator-owned delivery state. Not campaign DONE/attempt state and NOT
;; delivery proof. JSON permits the trusted GitHub step adapter to consume the
;; same immutable Verify receipt. Callers hold the campaign lease for writes.
(require json
         racket/file
         racket/list
         racket/path)
(provide delivery-journal-path
         load-delivery-journal
         record-delivery-receipt!
         update-delivery-journal!
         valid-delivery-receipt?
         valid-receipt-history?
         reconcile-repair-tail-receipt!
         repair-tail-allowed-stages
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
       (exact-nonnegative-integer? (hash-ref r 'verified-at #f))))
;; v1.00.33 W0 repair-tail protocol (audit §21.2): the append-only history
;; of PRIOR immutable receipts preserved by reconcile-repair-tail-receipt!.
;; Malformed history never loads — a journal that cannot prove its own past
;; is refused, not repaired.
(define (valid-receipt-history? h)
  (and (list? h) (andmap valid-delivery-receipt? h)))
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
                      (let ([h (hash-ref data 'receipt-history #f)])
                        (or (not h) (valid-receipt-history? h)))
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
                           '(receipt plan-id wave schema-version receipt-history)))
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

(define (reconcile-repair-tail-receipt! root
                                        plan
                                        wave
                                        new-receipt
                                        #:expected-attempt-id attempt-id
                                        #:expected-fence fence
                                        #:head-ancestor? head-ancestor?
                                        #:allowed-stages [allowed repair-tail-allowed-stages])
  (define (refuse! reason)
    (error 'delivery-journal (format "repair-tail reconciliation refused: ~a" reason)))
  (unless (valid-delivery-receipt? new-receipt)
    (refuse! "new receipt is invalid"))
  (unless (and (string? attempt-id) (positive? (string-length attempt-id)))
    (refuse! "expected attempt identity is missing"))
  (unless (exact-nonnegative-integer? fence)
    (refuse! "expected fence is invalid"))
  (unless (and (procedure? head-ancestor?) (procedure-arity-includes? head-ancestor? 2))
    (refuse! "ancestry predicate is missing"))
  (define old (load-delivery-journal root plan wave))
  (define old-receipt (and old (hash-ref old 'receipt #f)))
  (unless old
    (refuse! "no durable journal to reconcile"))
  (unless (hash? old-receipt)
    (refuse! "journal carries no receipt"))
  (unless (equal? (hash-ref old-receipt 'attempt-id #f) attempt-id)
    (refuse! "attempt identity changed"))
  (unless (equal? (hash-ref old-receipt 'attempt-fence #f) fence)
    (refuse! "attempt fence changed"))
  (unless (equal? (hash-ref new-receipt 'attempt-id #f) attempt-id)
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
  (define stage (hash-ref old 'stage #f))
  (unless (member stage allowed)
    (refuse! (format "stage ~a is terminal for repair-tail reconciliation" stage)))
  (unless (head-ancestor? old-head-sha new-head-sha)
    (refuse! "new head is not a descendant of the receipt head"))
  (define history (hash-ref old 'receipt-history '()))
  (unless (valid-receipt-history? history)
    (refuse! "existing receipt history is malformed"))
  (save! root
         plan
         wave
         (hash-set* old 'receipt new-receipt 'receipt-history (append history (list old-receipt))))
  new-receipt)

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
