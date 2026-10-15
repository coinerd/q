#lang racket/base
;; Capture committed provenance on BOTH isolated and shared-checkout Verify.
;; A missing receipt does not rewrite an existing Verify verdict, but autonomous
;; publication subsequently refuses it. No inference from arbitrary current HEAD.
(require racket/path
         racket/string
         "delivery-journal.rkt"
         "delivery-verifier.rkt"
         "review-proof.rkt"
         "campaign-state.rkt"
         (only-in "review-rejection.rkt" review-rejection-live-authorized?)
         (only-in "plan-context-builder.rkt" find-git-root-dir)
         "../../sandbox/subprocess.rkt"
         (only-in "../../util/json/checksum.rkt" sha256-string)
         (only-in "../../util/credential-redaction.rkt" redact-credential-data))
(provide committed-delivery-snapshot
         current-gsd-remote-published
         current-wave-for-attempt
         durable-receipt-head
         default-remote-published?
         verify-with-delivery-receipt
         verify-campaign-delivery
         certify-publication-receipt!
         recover-legacy-delivery-receipt!
         delivery-receipt-blocker)
;; Register F5 (v1.00.31 W3): a Verify verdict does not publish a receipt
;; until the local head is fetchable at origin. The default check is a real
;; ls-remote against the configured origin; tests inject a pure procedure.
(define (default-remote-published? repo branch head)
  (define out (git repo "ls-remote" "origin" (string-append "refs/heads/" branch)))
  (and (string? out) (string-contains? out head)))

;; Injectable seam for tests and host embeddings: the parameter is read at
;; the verify call sites, so a campaign thread picks up the value current at
;; execution time without threading keywords through the request struct.
(define current-gsd-remote-published (make-parameter default-remote-published?))
;; Shared pure attempt fence: the same identity test guards the implementation
;; result and the Verify receipt, avoiding a second weaker completion predicate.
(define (current-wave-for-attempt rec wave-idx fence attempt-id)
  (define wave
    (and rec
         (for/first ([w (in-list (campaign-record-waves rec))]
                     #:when (= (campaign-wave-index w) wave-idx))
           w)))
  (define attempt (and wave (campaign-wave-current-attempt wave)))
  (and rec
       wave
       attempt
       (= (campaign-fence-token rec) fence)
       (= (campaign-attempt-fence-token attempt) fence)
       (equal? (campaign-attempt-id attempt) attempt-id)
       wave))
(define (git root . args)
  (define r
    (run-subprocess "git"
                    #:args (append (list "-C" (path->string (path->complete-path root))) args)
                    #:directory root
                    #:timeout 15))
  (and (not (subprocess-result-timed-out? r))
       (not (subprocess-result-truncated? r))
       (equal? (subprocess-result-exit-code r) 0)
       (string-trim (subprocess-result-stdout r))))
(define (sha? s)
  (and (string? s) (regexp-match? #px"^[0-9a-f]{40}$" s)))
(define (identity root branch head)
  (identity/facts (git root "rev-parse" "--path-format=absolute" "--git-common-dir")
                  branch
                  head
                  (git root "rev-parse" (string-append head "^{tree}"))
                  (git root "config" "--get" "remote.origin.url")))
(define (identity/facts common branch head tree origin)
  ;; Reject credential-bearing or rewritten arbitrary origins before persistence.
  (and
   (sha? head)
   (sha? tree)
   origin
   common
   (equal? (path->string (file-name-from-path common)) ".git")
   (regexp-match? #px"^(?:https://github\\.com/|git@github\\.com:)[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$"
                  origin)
   (hasheq 'repo
           (path->string (simplify-path (path-only common)))
           'branch
           branch
           'head
           head
           'tree
           tree
           'origin
           origin)))
(define (committed-delivery-snapshot cwd)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define root (find-git-root-dir cwd))
    (and
     root
     (equal? (git root "status" "--porcelain=v1" "--untracked-files=all") "")
     (let* ([branch (git root "symbolic-ref" "--quiet" "--short" "HEAD")]
            [refs
             (git root "rev-parse" "--path-format=absolute" "--git-common-dir" "HEAD" "HEAD^{tree}")]
            [parts (and refs (string-split refs "\n"))])
       (and branch
            (not (member branch '("main" "master")))
            parts
            (= (length parts) 3)
            (identity/facts (car parts)
                            branch
                            (cadr parts)
                            (caddr parts)
                            (git root "config" "--get" "remote.origin.url")))))))
;; v1.00.33 W0 (audit §21.2): the REAL repair-tail ancestry predicate — the
;; recorded receipt head must be a strict ancestor of the verified head in
;; the snapshot's own repository. Any git/timeout failure is a refusal
;; (fail-closed), never an assumed ancestry.
(define (default-repair-tail-ancestor? repo old-head new-head)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (and (string? repo)
         (sha? old-head)
         (sha? new-head)
         (not (string=? old-head new-head))
         (and (git repo "merge-base" "--is-ancestor" old-head new-head) #t))))

;; BUG-0079 REVIEW-2 item 7: the IMMUTABLE binding identity of a publication
;; receipt — repo/branch/head/tree/origin/attempt-id/attempt-fence plus the
;; evidence DIGEST. verified-at timestamps and evidence formatting are
;; deliberately excluded: an identical replay under a fresh timestamp is
;; idempotent and preserves the durable receipt verbatim, while any
;; differing immutable binding refuses.
(define receipt-immutable-binding-keys
  '(repo branch head tree origin attempt-id attempt-fence owned-proof))

(define (same-immutable-receipt-binding? a b)
  (and (hash? a)
       (hash? b)
       (for/and ([key (in-list receipt-immutable-binding-keys)])
         (equal? (hash-ref a key #f) (hash-ref b key #f)))
       (equal? (sha256-string (format "~a" (hash-ref a 'evidence #f)))
               (sha256-string (format "~a" (hash-ref b 'evidence #f))))))

;; Certify a receipt from a previously approved publication intent.  This is
;; intentionally narrower than Verify: it never invokes a verifier or fabricates
;; evidence, and it reuses the same journal/repair-tail guards as the normal
;; receipt path against the exact persisted receipt candidate.
;; BUG-0079 REVIEW-2 item 2: the MANDATORY guard callback runs immediately
;; before every durable write and marker clear, so the caller's stable
;; active-fence+digest capture is re-verified at the exact write boundary.
(define (certify-publication-receipt! base
                                      plan
                                      wave
                                      receipt
                                      #:expected-attempt-id attempt-id
                                      #:expected-fence fence
                                      #:guard guard
                                      #:repair-tail-ancestor?
                                      [repair-tail-ancestor? default-repair-tail-ancestor?])
  (unless (and (procedure? guard) (procedure-arity-includes? guard 0))
    (error 'delivery-receipt "publication certification requires a transition guard"))
  (unless (valid-delivery-receipt? receipt)
    (error 'delivery-receipt "publication receipt candidate is invalid"))
  (validate-receipt-owned-proof! receipt base plan wave)
  (unless (and (equal? (hash-ref receipt 'attempt-id #f) attempt-id)
               (equal? (hash-ref receipt 'attempt-fence #f) fence))
    (error 'delivery-receipt "publication receipt identity changed"))
  (define old (load-delivery-journal base plan wave))
  (define old-receipt (and old (hash-ref old 'receipt #f)))
  (cond
    [(and old (hash? old-receipt) (same-immutable-receipt-binding? old-receipt receipt))
     ;; Identical immutable binding: the existing durable receipt/journal is
     ;; preserved verbatim (no timestamp-driven overwrite); only the typed
     ;; marker clear remains, guarded.
     (guard)
     (clear-remote-pending! base plan wave)
     old]
    [(and old
          (hash? old-receipt)
          (equal? (hash-ref old-receipt 'branch #f) (hash-ref receipt 'branch #f))
          (equal? (hash-ref old-receipt 'attempt-id #f) attempt-id)
          (equal? (hash-ref old-receipt 'attempt-fence #f) fence)
          (repair-tail-ancestor? (hash-ref receipt 'repo #f)
                                 (hash-ref old-receipt 'head #f)
                                 (hash-ref receipt 'head #f)))
     (guard)
     (reconcile-repair-tail-receipt!
      base
      plan
      wave
      receipt
      #:expected-attempt-id attempt-id
      #:expected-fence fence
      #:head-ancestor? (lambda (old-head new-head)
                         (repair-tail-ancestor? (hash-ref receipt 'repo #f) old-head new-head)))
     (guard)
     (clear-remote-pending! base plan wave)
     (load-delivery-journal base plan wave)]
    [(and old (evidence-author-repair-pending? old))
     (guard)
     (reconcile-evidence-author-receipt!
      base
      plan
      wave
      receipt
      #:guard guard
      #:head-ancestor? (lambda (old-head new-head)
                         (repair-tail-ancestor? (hash-ref receipt 'repo #f) old-head new-head)))
     (guard)
     (clear-remote-pending! base plan wave)
     (load-delivery-journal base plan wave)]
    [old (error 'delivery-receipt "publication receipt does not match durable journal identity")]
    [else
     (guard)
     (record-delivery-receipt! base plan wave receipt)
     (guard)
     (clear-remote-pending! base plan wave)
     (load-delivery-journal base plan wave)]))

(define (proof-attempt-active? rec wave)
  (define w
    (for/first ([w (in-list (campaign-record-waves rec))]
                #:when (= (campaign-wave-index w) wave))
      w))
  (and w
       (memq (campaign-wave-status w) '(in-progress running verifying done awaiting-delivery))
       (not (campaign-record-budget-pause rec))))
(define (verify-with-delivery-receipt
         base
         plan
         wave
         cwd
         thunk
         #:approved? approved?
         #:evidence evidence
         #:snapshot [snapshot committed-delivery-snapshot]
         #:attempt [attempt #f]
         #:current-record [current-record #f]
         #:remote-published [remote-published (current-gsd-remote-published)]
         #:coordinator-fence [coordinator-fence #f]
         #:publish-approved-head [publish-approved-head #f]
         ;; v1.00.33 W0 repair-tail protocol (audit
         ;; §21.2): injectable repair-tail ancestry
         ;; predicate. The default is the REAL git
         ;; ancestry check (merge-base
         ;; --is-ancestor) in the snapshot's
         ;; repository; tests inject a pure predicate.
         #:repair-tail-ancestor? [repair-tail-ancestor? default-repair-tail-ancestor?]
         #:refresh-owned-proof? [refresh-owned-proof? #f])
  (define before (snapshot cwd))
  (define expected-old (and refresh-owned-proof? (load-delivery-journal base plan wave)))
  (when refresh-owned-proof?
    (unless (and expected-old
                 before
                 attempt
                 (procedure? current-record)
                 (member (hash-ref expected-old 'stage) repair-tail-allowed-stages)
                 (let ([rec (current-record)])
                   (and (campaign-record? rec)
                        (equal? (campaign-plan-id rec) plan)
                        (not (campaign-record-cancellation rec))
                        (proof-attempt-active? rec wave)
                        (current-wave-for-attempt rec
                                                  wave
                                                  (campaign-attempt-fence-token attempt)
                                                  (campaign-attempt-id attempt)))))
      (error 'delivery-receipt
             "explicit proof refresh requires a live premerge attempt and old receipt")))
  (define-values (result capture)
    (call-with-owned-proof-capture base
                                   plan
                                   wave
                                   before
                                   (and attempt (campaign-attempt-id attempt))
                                   (and attempt (campaign-attempt-fence-token attempt))
                                   thunk
                                   #:cwd cwd))
  (define proof (and capture (owned-proof-capture-reference capture)))
  (define after (snapshot cwd))
  (when (and refresh-owned-proof? (not (and proof (approved? result) before (equal? before after))))
    (error 'delivery-receipt
           "explicit proof refresh failed to obtain unchanged successful owned execution"))
  (when (and (approved? result) before (equal? before after))
    (with-handlers
        ([exn:fail?
          (lambda (e)
            (when refresh-owned-proof?
              (raise e))
            (log-warning
             "delivery receipt could not be recorded; delivery will require provenance reconciliation"))])
      (define old (load-delivery-journal base plan wave))
      (define old-receipt (and old (hash-ref old 'receipt #f)))
      ;; Automatic independent review (slice A): the durable review-rejection
      ;; marker for THIS receipt, if any. It authorizes the cross-attempt
      ;; supersession below; it never weakens any same-attempt path.
      (define old-rejection (and old (hash? old-receipt) (journal-review-rejection old)))
      (define old-author
        (and old (evidence-author-repair-pending? old) (journal-evidence-author-repair old)))
      ;; BUG-0079 REVIEW-2 item 8: publication requires GENUINE non-empty
      ;; successful Verify evidence. A bare boolean, empty list, or blank
      ;; text approval is bool-only and can never trigger the publication
      ;; side effect — it records the typed marker instead.
      (define (genuine-delivery-evidence? v)
        (cond
          [(pair? v) #t]
          [(string? v) (positive? (string-length (string-trim v)))]
          [else #f]))
      ;; BUG-0079 REVIEW-2 item 2: the publisher is an external effect; the
      ;; approval verdict, the delivery snapshot, and the current campaign
      ;; record guard are REPEATED after it returns, immediately before the
      ;; durable receipt write (not only at module return).
      (define (post-publication-guard-ok?)
        (and (approved? result)
             (equal? before (snapshot cwd))
             (or (not (procedure? current-record))
                 (let ([rec (current-record)])
                   (and (campaign-record? rec)
                        (equal? (campaign-plan-id rec) plan)
                        (not (campaign-record-cancellation rec)))))))
      (define (record-post-publication-pending! reason)
        (record-remote-pending! base
                                plan
                                wave
                                (hash-ref before 'branch)
                                (hash-ref before 'head)
                                reason)
        #f)
      (define (publish-approved-head-or-record-pending! reason-prefix)
        (with-handlers ([exn:fail?
                         (lambda (e)
                           (define reason
                             (format "~a; publication refused: ~a" reason-prefix (exn-message e)))
                           (record-remote-pending! base
                                                   plan
                                                   wave
                                                   (hash-ref before 'branch)
                                                   (hash-ref before 'head)
                                                   reason)
                           #f)])
          (cond
            [attempt
             (cond
               [(and (procedure? publish-approved-head)
                     (delivery-verification? result)
                     (delivery-verification-approved? result)
                     (genuine-delivery-evidence? (delivery-verification-evidence result))
                     (procedure? current-record))
                (define args
                  (list base
                        plan
                        wave
                        before
                        (campaign-attempt-id attempt)
                        (campaign-attempt-fence-token attempt)
                        coordinator-fence
                        (format "~a" (redact-credential-data (delivery-verification-evidence result)))
                        current-record))
                (define-values (_required accepted) (procedure-keywords publish-approved-head))
                (if (or (not accepted) (memq '#:owned-proof accepted))
                    (keyword-apply publish-approved-head '(#:owned-proof) (list proof) args)
                    (apply publish-approved-head args))]
               [(and (procedure? publish-approved-head) (not (delivery-verification? result)))
                (record-remote-pending!
                 base
                 plan
                 wave
                 (hash-ref before 'branch)
                 (hash-ref before 'head)
                 (format
                  "~a; publication refused: approved-head publication requires genuine delivery-verification evidence"
                  reason-prefix))
                #f]
               [(and (procedure? publish-approved-head)
                     (delivery-verification? result)
                     (not (genuine-delivery-evidence? (delivery-verification-evidence result))))
                (record-remote-pending!
                 base
                 plan
                 wave
                 (hash-ref before 'branch)
                 (hash-ref before 'head)
                 (format
                  "~a; publication refused: approved-head publication requires non-empty successful Verify evidence"
                  reason-prefix))
                #f]
               [else
                (record-remote-pending!
                 base
                 plan
                 wave
                 (hash-ref before 'branch)
                 (hash-ref before 'head)
                 (format "~a; publication refused: approved-head publisher unavailable"
                         reason-prefix))
                #f])]
            [else
             (record-remote-pending!
              base
              plan
              wave
              (hash-ref before 'branch)
              (hash-ref before 'head)
              (format "~a; publication refused: approved Verify is not attempt-bound" reason-prefix))
             #f])))
      (define (live-proof-guard?)
        (and proof
             attempt
             (procedure? current-record)
             (approved? result)
             (equal? before (committed-delivery-snapshot cwd))
             (let ([rec (current-record)])
               (and (campaign-record? rec)
                    (equal? (campaign-plan-id rec) plan)
                    (not (campaign-record-cancellation rec))
                    (proof-attempt-active? rec wave)
                    (current-wave-for-attempt rec
                                              wave
                                              (campaign-attempt-fence-token attempt)
                                              (campaign-attempt-id attempt))))
             (equal? (hash-ref (hash-ref proof 'binding) 'snapshot-sha256)
                     (hash-ref (frozen-verify-binding base plan wave) 'snapshot-sha256))))
      (define (new-receipt-for-head!)
        (define text (format "~a" (redact-credential-data (evidence result))))
        (define receipt
          (hash-set* (if attempt
                         (hash-set* before
                                    'attempt-id
                                    (campaign-attempt-id attempt)
                                    'attempt-fence
                                    (campaign-attempt-fence-token attempt))
                         before)
                     'verified-at
                     (current-seconds)
                     'evidence
                     (substring text 0 (min 8192 (string-length text)))))
        (if (and proof (live-proof-guard?))
            (hash-set receipt 'owned-proof proof)
            receipt))
      (cond
        [refresh-owned-proof?
         (unless (and expected-old
                      old
                      (equal? expected-old old)
                      (live-proof-guard?)
                      (remote-published (hash-ref before 'repo)
                                        (hash-ref before 'branch)
                                        (hash-ref before 'head)))
           (error 'delivery-receipt
                  "explicit proof refresh requires unchanged old journal and live owned Verify"))
         (refresh-owned-proof-receipt! base
                                       plan
                                       wave
                                       (new-receipt-for-head!)
                                       #:old-receipt (hash-ref expected-old 'receipt)
                                       #:authorization capture
                                       #:guard live-proof-guard?)
         (clear-remote-pending! base plan wave)]
        ;; Automatic independent review (slice A): the SUPPORTED cross-attempt
        ;; receipt supersession. The journal holds the receipt of an attempt
        ;; that an independent reviewer explicitly rejected (durable marker
        ;; naming that exact receipt), and the LIVE attempt is a DIFFERENT
        ;; fenced attempt on the same delivery branch whose genuine approved
        ;; Verify produced this snapshot. The transition itself runs in the
        ;; journal (reconcile-review-rejected-receipt!) and re-runs the live
        ;; record guard at the write boundary; the old receipt and its frozen
        ;; evidence trio are preserved as immutable history (generation +1
        ;; exactly once) and the stage resets to context-ready so preflight →
        ;; review re-run on the new receipt. Without the marker this entire
        ;; clause is inert and the fail-closed branch below applies unchanged.
        [(and old
              attempt
              (hash? old-receipt)
              (or (and old-rejection
                       (review-rejection-matches-receipt? old-rejection old-receipt)
                       (not (hash-ref old-rejection 'superseded-by-attempt #f)))
                  (and old-author
                       (equal? (hash-ref old-author 'state) "running")
                       (equal? (hash-ref old-author 'successor-attempt) (campaign-attempt-id attempt))
                       (equal? (hash-ref old-author 'successor-fence)
                               (campaign-attempt-fence-token attempt))))
              (not (equal? (hash-ref old-receipt 'attempt-id #f) (campaign-attempt-id attempt)))
              (equal? (hash-ref old-receipt 'branch #f) (hash-ref before 'branch #f)))
         (define (live-supersession-guard-ok?)
           (and (procedure? current-record)
                (approved? result)
                (equal? before (snapshot cwd))
                (if old-author
                    (let ([rec (current-record)])
                      (and rec
                           (not (campaign-record-budget-pause rec))
                           (current-wave-for-attempt rec
                                                     wave
                                                     (campaign-attempt-fence-token attempt)
                                                     (campaign-attempt-id attempt))))
                    (review-rejection-live-authorized? (current-record)
                                                       plan
                                                       wave
                                                       (campaign-attempt-id attempt)
                                                       (campaign-attempt-fence-token attempt)))))
         (define (supersede-rejected!)
           (if old-author
               (reconcile-evidence-author-receipt!
                base
                plan
                wave
                (new-receipt-for-head!)
                #:guard live-supersession-guard-ok?
                #:head-ancestor?
                (lambda (old-head new-head)
                  (default-repair-tail-ancestor? (hash-ref before 'repo #f) old-head new-head)))
               (reconcile-review-rejected-receipt! base
                                                   plan
                                                   wave
                                                   (new-receipt-for-head!)
                                                   #:guard live-supersession-guard-ok?))
           (clear-remote-pending! base plan wave)
           (log-info "~a receipt superseded by attempt ~a at head ~a"
                     (if old-author "evidence-author" "review-rejected")
                     (campaign-attempt-id attempt)
                     (hash-ref before 'head)))
         (cond
           [(remote-published (hash-ref before 'repo)
                              (hash-ref before 'branch)
                              (hash-ref before 'head))
            (supersede-rejected!)]
           [else
            (define reason
              (format "cross-attempt head ~a is not published on origin; receipt not superseded"
                      (hash-ref before 'head)))
            (define published? (publish-approved-head-or-record-pending! reason))
            (cond
              [(and published? (post-publication-guard-ok?))
               (with-handlers ([exn:fail?
                                (lambda (e)
                                  (record-post-publication-pending!
                                   (format "~a; supersession refused: ~a" reason (exn-message e)))
                                  (log-warning "~a supersession refused: ~a"
                                               (if old-author "evidence-author" "review-rejected")
                                               (exn-message e)))])
                 (supersede-rejected!))]
              [published?
               (record-post-publication-pending!
                (string-append
                 reason
                 "; supersession guard refused: approval, snapshot or campaign record drifted after the publisher effect"))
               (log-warning "~a supersession withheld: guard refused"
                            (if old-author "evidence-author" "review-rejected"))]
              [else (log-warning "delivery receipt withheld: ~a; publication refused" reason)])])]
        ;; v1.00.33 W0 (audit §20 blocker 2): a durable receipt resolves any
        ;; stale marker ONLY at the SAME head+branch (idempotent re-verify).
        ;; A receipt at ANY other head never does — the demonstrated defect
        ;; was exactly this idempotence being unconditional.
        [(and old
              (hash? old-receipt)
              (equal? (hash-ref old-receipt 'head #f) (hash-ref before 'head #f))
              (equal? (hash-ref old-receipt 'branch #f) (hash-ref before 'branch #f)))
         (clear-remote-pending! base plan wave)]
        ;; v1.00.33 W0: the supported same-attempt repair-tail transition. The
        ;; delivery branch advanced past the recorded receipt head while the
        ;; SAME attempt is live: reconcile through the explicit atomic fenced
        ;; transition (old receipt preserved as history), never a silent
        ;; overwrite, and clear the marker only AFTER the new receipt for the
        ;; verified head is durable.
        [(and old
              attempt
              (hash? old-receipt)
              (equal? (hash-ref old-receipt 'branch #f) (hash-ref before 'branch #f))
              (equal? (hash-ref old-receipt 'attempt-id #f) (campaign-attempt-id attempt))
              (equal? (hash-ref old-receipt 'attempt-fence #f) (campaign-attempt-fence-token attempt))
              (repair-tail-ancestor? (hash-ref before 'repo #f)
                                     (hash-ref old-receipt 'head #f)
                                     (hash-ref before 'head #f)))
         (cond
           [(remote-published (hash-ref before 'repo)
                              (hash-ref before 'branch)
                              (hash-ref before 'head))
            (reconcile-repair-tail-receipt!
             base
             plan
             wave
             (new-receipt-for-head!)
             #:expected-attempt-id (campaign-attempt-id attempt)
             #:expected-fence (campaign-attempt-fence-token attempt)
             #:head-ancestor? (lambda (old-head new-head)
                                (repair-tail-ancestor? (hash-ref before 'repo #f) old-head new-head)))
            (clear-remote-pending! base plan wave)
            (log-info "repair-tail receipt reconciled to head ~a" (hash-ref before 'head))]
           [else
            (define reason
              (format "repair-tail head ~a is not published on origin; receipt not reconciled"
                      (hash-ref before 'head)))
            (define published? (publish-approved-head-or-record-pending! reason))
            (cond
              [(and published? (not (post-publication-guard-ok?)))
               (record-post-publication-pending!
                (string-append
                 reason
                 "; publication guard refused: approval, snapshot or campaign record drifted after the publisher effect"))
               (log-warning "repair-tail receipt withheld: post-publication guard refused")]
              [published?
               (reconcile-repair-tail-receipt!
                base
                plan
                wave
                (new-receipt-for-head!)
                #:expected-attempt-id (campaign-attempt-id attempt)
                #:expected-fence (campaign-attempt-fence-token attempt)
                #:head-ancestor?
                (lambda (old-head new-head)
                  (repair-tail-ancestor? (hash-ref before 'repo #f) old-head new-head)))
               (clear-remote-pending! base plan wave)
               (log-info "repair-tail receipt reconciled after publishing head ~a"
                         (hash-ref before 'head))]
              [else (log-warning "delivery receipt withheld: ~a; publication refused" reason)])])]
        ;; Any OTHER existing receipt (head mismatch without same-attempt
        ;; repair-tail eligibility) fails closed: the old receipt stays
        ;; untouched, the marker is NEVER cleared by a mismatched receipt,
        ;; and the typed reason demands explicit reconciliation.
        [old
         (log-warning
          "delivery receipt head mismatch; reconciliation required: old ~a new ~a on branch ~a"
          (hash-ref old-receipt 'head #f)
          (hash-ref before 'head #f)
          (hash-ref before 'branch #f))]
        [(remote-published (hash-ref before 'repo) (hash-ref before 'branch) (hash-ref before 'head))
         (record-delivery-receipt! base plan wave (new-receipt-for-head!))
         (clear-remote-pending! base plan wave)]
        ;; Register F5: an unpublished branch records the typed remote-pending
        ;; marker instead of a receipt. The state is NOT verified and blocks
        ;; ladder entry with 'branch-not-published naming branch and head.
        [else
         (define reason
           (format "branch ~a head ~a is not published on origin; receipt not verified"
                   (hash-ref before 'branch)
                   (hash-ref before 'head)))
         (define published? (publish-approved-head-or-record-pending! reason))
         (cond
           [(and published? (not (post-publication-guard-ok?)))
            (record-post-publication-pending!
             (string-append
              reason
              "; publication guard refused: approval, snapshot or campaign record drifted after the publisher effect"))
            (log-warning "delivery receipt withheld: post-publication guard refused")]
           [published?
            (record-delivery-receipt! base plan wave (new-receipt-for-head!))
            (clear-remote-pending! base plan wave)
            (log-info "delivery receipt recorded after publishing approved head ~a"
                      (hash-ref before 'head))]
           [else (log-warning "delivery receipt withheld: ~a; publication refused" reason)])])))
  result)
;; Thin orchestration seam: context and verdict interpretation stay beside
;; receipt capture. `current-record` rejects stale attempts before persistence.
(define (verify-campaign-delivery base
                                  plan
                                  wave
                                  cwd
                                  context
                                  verifier
                                  current-record
                                  #:remote-published [remote-published (current-gsd-remote-published)]
                                  #:publish-approved-head [publish-approved-head #f]
                                  #:refresh-owned-proof? [refresh-owned-proof? #f])
  (define initial (current-record))
  (define attempt
    (and initial
         (for/first ([w (in-list (campaign-record-waves initial))]
                     #:when (= (campaign-wave-index w) wave))
           (campaign-wave-current-attempt w))))
  (verify-with-delivery-receipt
   base
   plan
   wave
   cwd
   (lambda ()
     (parameterize ([current-gsd-delivery-branch-context context])
       (verifier wave)))
   #:attempt attempt
   #:remote-published remote-published
   #:coordinator-fence (and initial (campaign-fence-token initial))
   #:current-record current-record
   #:publish-approved-head publish-approved-head
   #:refresh-owned-proof? refresh-owned-proof?
   #:approved? (lambda (v)
                 (define current (current-record))
                 (and current
                      attempt
                      (equal? (campaign-plan-id current) plan)
                      (not (campaign-record-cancellation current))
                      (current-wave-for-attempt current
                                                wave
                                                (campaign-attempt-fence-token attempt)
                                                (campaign-attempt-id attempt))
                      (if (delivery-verification? v)
                          (delivery-verification-approved? v)
                          v)))
   #:evidence (lambda (v)
                (if (delivery-verification? v)
                    (delivery-verification-evidence v)
                    "verifier approved"))))
;; Pure eligibility prerequisite, NOT approval or delivery proof. The caller
;; must load journal and record from disk while holding the lease and repeat
;; this check before each effect. Git identity/review/checks belong to the
;; protected action adapter. A resumed coordinator has a new fence; the DONE
;; implementation attempt retains its own historical fence.
(define (delivery-receipt-blocker journal record plan wave expected-fence)
  (define receipt (and (hash? journal) (hash-ref journal 'receipt #f)))
  (define w
    (and (campaign-record? record)
         (for/first ([w (in-list (campaign-record-waves record))]
                     #:when (equal? (campaign-wave-index w) wave))
           w)))
  (define attempt (and w (campaign-wave-current-attempt w)))
  (cond
    [(not (and (hash? journal)
               (equal? (hash-ref journal 'schema-version #f) 1)
               (equal? (hash-ref journal 'plan-id #f) plan)
               (equal? (hash-ref journal 'wave #f) wave)
               (valid-delivery-receipt? receipt)))
     'missing-provenance]
    [(not (and (campaign-record? record) (equal? (campaign-plan-id record) plan) w))
     'campaign-mismatch]
    [(campaign-record-cancellation record) 'cancelled]
    [(not (and (exact-nonnegative-integer? expected-fence)
               (equal? (campaign-fence-token record) expected-fence)))
     'stale-coordinator]
    [(not (memq (campaign-wave-status w) '(done awaiting-delivery))) 'implementation-not-done]
    [(not (and attempt
               (string? (hash-ref receipt 'attempt-id #f))
               (exact-nonnegative-integer? (hash-ref receipt 'attempt-fence #f))
               (equal? (hash-ref receipt 'attempt-id) (campaign-attempt-id attempt))
               (equal? (hash-ref receipt 'attempt-fence) (campaign-attempt-fence-token attempt))))
     'unbound-attempt]
    [(member (hash-ref receipt 'branch) '("main" "master")) 'protected-branch]
    [(or (and (not (equal? (campaign-wave-delivery-branch w) ""))
              (not (equal? (campaign-wave-delivery-branch w) (hash-ref receipt 'branch))))
         (and (not (equal? (campaign-wave-delivery-head-sha w) ""))
              (not (equal? (campaign-wave-delivery-head-sha w) (hash-ref receipt 'head)))))
     'stale-provenance]
    [else #f]))

;; The durable receipt head (register F3): the verified branch head recorded
;; by verify-with-delivery-receipt for this wave, or #f when no verified
;; receipt exists. Evidence and review heads must equal it before delivery.
(define (durable-receipt-head base plan wave)
  (define journal (load-delivery-journal base plan wave))
  (define receipt (and (hash? journal) (hash-ref journal 'receipt #f)))
  (define head (and (hash? receipt) (hash-ref receipt 'head #f)))
  (and (sha? head) head))

(define (recover-legacy-delivery-receipt! base plan wave branch head cwd)
  (and
   (string? branch)
   (not (string=? branch ""))
   (sha? head)
   (with-handlers ([exn:fail? (lambda (_) #f)])
     (define root (find-git-root-dir cwd))
     (and
      root
      (equal? (git root "rev-parse" (string-append "refs/heads/" branch)) head)
      (let ([snapshot (identity root branch head)])
        (and
         snapshot
         (record-delivery-receipt!
          base
          plan
          wave
          (hash-set*
           snapshot
           'verified-at
           (current-seconds)
           'evidence
           "Recovered from coordinator's durable DONE branch/head; original Verify log not retained"))))))))
