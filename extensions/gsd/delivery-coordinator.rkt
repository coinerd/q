#lang racket/base
;; STABILITY: internal
;;
;; extensions/gsd/delivery-coordinator.rkt — B2a deterministic journal-driven
;; delivery stage machine (BUG: coordinator-delivery-execution-gap).
;;
;; The coordinator owns authenticated delivery execution INSIDE /go: it drives
;; ONE protected controller effect per invocation and persists each successful
;; stage transition to the immutable delivery journal. Every effect:
;;   - reloads journal + durable campaign record (fail-closed on corruption),
;;   - re-runs delivery-receipt-blocker (the ONLY eligibility prerequisite),
;;   - rejects cancellation / stale fence / takeover before and after the call,
;;   - records separate delivery usage (never touches implementation attempts),
;;   - advances the journal by exactly one stage on success.
;; The controller seam is injectable for deterministic tests: a procedure
;; (base-dir plan wave target-stage) -> delivery-effect-result. The default
;; production seam shells scripts/gsd-delivery.py through the same credential
;; boundary as delivery-handoff (B2b wiring). Model completion, journal
;; stages, and Verify receipts are NEVER delivery proof: only a controller
;; effect result carrying authenticated merge evidence does. The outer
;; run-campaign! loop re-enters after each step, so restarts resume at the
;; exact persisted stage without duplicating external effects.

(require racket/base
         racket/string
         "campaign-state.rkt"
         "campaign-repository.rkt"
         "delivery-receipt.rkt"
         (only-in "delivery-journal.rkt"
                  delivery-stages
                  load-delivery-journal
                  update-delivery-journal!
                  load-remote-pending
                  record-remote-pending!
                  clear-remote-pending!
                  remote-pending-blocker))

(provide delivery-effect-result
         delivery-effect-result?
         delivery-effect-result-kind
         delivery-effect-result-data
         delivery-outcome
         delivery-outcome?
         delivery-outcome-kind
         delivery-outcome-message
         run-delivery-coordinator!
         default-delivery-controller
         binding-dispatch-stage-action
         default-delivery-coordinator
         default-delivery-controller-interpret
         parse-delivery-stop)

;; Journal stage order excluding the typed stops
;; (awaiting-approval / retryable / blocked are outcomes, not linear stages).
;; context-ready is the initial stage recorded with the Verify receipt.
(define linear-delivery-stages
  (list "context-ready"
        "implementation-review"
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
        "delivered"))

;; A controller effect result: (cons kind data) where kind ∈
;; 'ok | 'awaiting-review | 'retryable | 'blocked. data is a hash carrying
;; 'stage (the requested target) plus optional 'reason and delivery usage
;; fields ('model-calls 'tokens 'cost).
(define (delivery-effect-result kind [data #hasheq()])
  (cons kind data))

(define (delivery-effect-result? v)
  (and (pair? v) (memq (car v) '(ok awaiting-review retryable blocked))))

(define (delivery-effect-result-kind r)
  (car r))
(define (delivery-effect-result-data r)
  (cdr r))

(define (delivery-outcome kind [message ""])
  (cons kind
        (if (string? message)
            message
            (format "~a" message))))

(define (delivery-outcome? o)
  (and (pair? o) (symbol? (car o)) (string? (cdr o))))

(define (delivery-outcome-kind o)
  (car o))
(define (delivery-outcome-message o)
  (cdr o))

(define (indexof lst v)
  (let loop ([i 0]
             [rest lst])
    (cond
      [(null? rest) #f]
      [(equal? (car rest) v) i]
      [else (loop (add1 i) (cdr rest))])))

(define (full-sha-string? value)
  (and (string? value) (regexp-match? #px"^[0-9a-f]{40}$" value)))

;; Next linear stage after `current` (a member of linear-delivery-stages).
(define (next-stage current)
  (define idx (indexof linear-delivery-stages current))
  (and idx (list-ref linear-delivery-stages (add1 idx))))

;; Durable eligibility re-check against an EXPLICIT expected fence. The fence
;; is captured once at step start: a takeover (fence bump) or cancellation
;; that lands while the controller runs must reject the continuation. The
;; receipt's attempt-fence is intentionally historical and never used here.
(define (durable-blocker dir plan wave expected-fence)
  ;; Register F5: an existing remote-pending marker (the Verify receipt was
  ;; withheld because the branch was never pushed) refuses before anything
  ;; else, with the typed reason naming branch and head.
  (or (remote-pending-blocker dir plan wave)
      (with-handlers ([exn:fail? (lambda (_) 'invalid-journal)])
        (define journal (load-delivery-journal dir plan wave))
        (define record (load-campaign-record dir plan))
        (and (not (and record (campaign-record-cancellation record)))
             (delivery-receipt-blocker journal record plan wave expected-fence)))))

;; The operator-facing blocked message; enriched with the marker's branch and
;; head whenever the typed branch-not-published reason fired.
(define (blocked-message dir plan wave reason)
  (define marker (and (eq? reason 'branch-not-published) (load-remote-pending dir plan wave)))
  (if marker
      (format
       "delivery blocked: branch-not-published: branch ~a head ~a is not published on origin; push the verified head and re-verify"
       (hash-ref marker 'branch "?")
       (hash-ref marker 'head "?"))
      (format "delivery blocked: ~a" reason)))

;; Stamp separate delivery usage onto the journal, additive-only. The
;; forbidden update fields (receipt plan-id wave schema-version) are never
;; passed; model-calls/tokens/cost accumulate across effects.
(define (record-delivery-usage! dir plan wave data)
  (define journal (load-delivery-journal dir plan wave))
  (when journal
    (define (acc key default)
      (define v (hash-ref data key #f))
      (and (real? v) (+ (hash-ref journal key default) v)))
    (define fields
      (for/hash ([(k _) (in-hash data)]
                 #:when (memq k '(model-calls tokens cost)))
        (values k (acc k 0))))
    (when (positive? (hash-count fields))
      (update-delivery-journal! dir plan wave fields))))

;; Exactly one bounded controller effect. Returns a delivery-outcome:
;;   'delivered       — already at the terminal stage (nothing to do)
;;   'ok              — one effect ran; the journal advanced one stage
;;   'awaiting-review — typed stop from the controller, journal untouched
;;   'retryable       — typed stop (e.g. CI pending), journal untouched
;;   'blocked         — eligibility/cancellation/takeover refusal
(define (run-delivery-coordinator! dir plan wave #:controller [controller #f])
  (define (blocked message)
    (delivery-outcome 'blocked message))
  (cond
    [(not (and (procedure? controller) (procedure-arity-includes? controller 4)))
     (blocked "delivery coordinator has no controller seam")]
    [else
     ;; Capture the fence ONCE at step start; the post-effect check below
     ;; must observe the SAME durable fence, plan and cancellation state.
     (define start-record (load-campaign-record dir plan))
     (define start-fence (and start-record (campaign-fence-token start-record)))
     (define (still-current?)
       (define now (load-campaign-record dir plan))
       (and now
            (equal? (campaign-plan-id now) plan)
            (equal? (campaign-fence-token now) start-fence)
            (not (campaign-record-cancellation now))))
     (define block-reason (durable-blocker dir plan wave start-fence))
     (cond
       [block-reason (blocked (blocked-message dir plan wave block-reason))]
       [else
        (define journal (load-delivery-journal dir plan wave))
        (define current (and journal (hash-ref journal 'stage #f)))
        (cond
          [(equal? current "delivered") (delivery-outcome 'delivered "already delivered")]
          [(not current) (blocked "journal stage missing")]
          [else
           (define target (next-stage current))
           (cond
             [(not target) (blocked (format "unknown journal stage: ~a" current))]
             ;; Register F5 preflight (v1.00.31 W3): before the FIRST ladder
             ;; action, the handoff seam verifies branch published, remote tip
             ;; equal to the verified head (evidence-only drift allowed) and
             ;; the required policy object present. Typed refusal; the journal
             ;; stays at context-ready so the next invocation re-preflights.
             [(and (equal? current "context-ready") (equal? target "implementation-review"))
              ;; Register F7 (v1.00.31 W5): the artifact provenance gate runs
              ;; before the F5 remote preflight — a wave with stale/drifting
              ;; artifacts cannot be delivered.
              (define provenance (artifact-provenance-gate dir plan wave))
              (cond
                [(eq? (delivery-effect-result-kind provenance) 'blocked)
                 (blocked (format "artifact-provenance: ~a"
                                  (hash-ref (delivery-effect-result-data provenance)
                                            'reason
                                            "declared artifacts failed the provenance gate")))]
                [else
                 (define preflight (controller dir plan wave "delivery-preflight"))
                 (cond
                   [(not (delivery-effect-result? preflight))
                    (blocked "controller returned a malformed effect result")]
                   [(eq? (delivery-effect-result-kind preflight) 'ok)
                    (define result (controller dir plan wave target))
                    (cond
                      [(not (delivery-effect-result? result))
                       (blocked "controller returned a malformed effect result")]
                      [else
                       (define data (delivery-effect-result-data result))
                       (case (delivery-effect-result-kind result)
                         [(ok)
                          (if (and (still-current?) (not (durable-blocker dir plan wave start-fence)))
                              (begin
                                (record-delivery-usage! dir plan wave data)
                                (update-delivery-journal! dir plan wave (hasheq 'stage target))
                                (delivery-outcome 'ok target))
                              (blocked "stale continuation: campaign changed during the effect"))]
                         [(awaiting-review)
                          (delivery-outcome
                           'awaiting-review
                           (hash-ref data 'reason "awaiting genuine independent review"))]
                         [(retryable)
                          (delivery-outcome 'retryable
                                            (hash-ref data 'reason "retryable delivery failure"))]
                         [else
                          (delivery-outcome
                           'blocked
                           (hash-ref data 'reason "controller refused the effect"))])])]
                   [else
                    (define reason
                      (hash-ref (delivery-effect-result-data preflight)
                                'reason
                                "delivery preflight refused"))
                    (case (delivery-effect-result-kind preflight)
                      [(awaiting-review) (delivery-outcome 'awaiting-review reason)]
                      [(retryable) (delivery-outcome 'retryable reason)]
                      [else (blocked (format "delivery preflight: ~a" reason))])])])]
             [else
              (define result (controller dir plan wave target))
              (cond
                [(not (delivery-effect-result? result))
                 (blocked "controller returned a malformed effect result")]
                [else
                 (define data (delivery-effect-result-data result))
                 (case (delivery-effect-result-kind result)
                   [(ok)
                    ;; Post-effect durable re-check against the CAPTURED
                    ;; fence: a cancellation or takeover that landed while
                    ;; the controller ran must reject the continuation, and
                    ;; the blocker must stay green (an already-merged
                    ;; idempotent retry is safe — the journal advances only
                    ;; after this passes).
                    (if (and (still-current?) (not (durable-blocker dir plan wave start-fence)))
                        (begin
                          (record-delivery-usage! dir plan wave data)
                          (update-delivery-journal! dir plan wave (hasheq 'stage target))
                          (delivery-outcome 'ok target))
                        (blocked "stale continuation: campaign changed during the effect"))]
                   [(awaiting-review)
                    (delivery-outcome 'awaiting-review
                                      (hash-ref data 'reason "awaiting genuine independent review"))]
                   [(retryable)
                    (delivery-outcome 'retryable
                                      (hash-ref data 'reason "retryable delivery failure"))]
                   [else
                    (delivery-outcome
                     'blocked
                     (hash-ref data 'reason "controller refused the effect"))])])])])])]))

;; ============================================================
;; B2b: default production controller seam
;; ============================================================
;;
;; The wave-blocked checkpoint in run-campaign! accepts an injectable
;; `delivery-coordinator` procedure (base-dir plan wave-index) ->
;; delivery-outcome. The PRODUCTION default wraps the journal-driven stage
;; machine around a controller that shells scripts/gsd-delivery.py through
;; the SAME credential boundary as delivery-readback (controller-environment
;; re-adds GH_TOKEN/GITHUB_TOKEN to an otherwise scrubbed environment; every
;; operator-visible reason is redacted). One bounded effect per invocation;
;; absent credentials or an action the controller cannot safely take are
;; typed stops, never fabricated success.

(require (only-in "delivery-handoff.rkt" controller-environment redact-delivery-text)
         racket/runtime-path
         json
         (only-in "../../sandbox/subprocess.rkt"
                  run-subprocess
                  subprocess-result-stdout
                  subprocess-result-stderr
                  subprocess-result-exit-code
                  subprocess-result-timed-out?
                  subprocess-result-truncated?))

(define-runtime-path default-delivery-controller-script "../../scripts/gsd-delivery.py")
(define-runtime-path artifact-provenance-lint-script
                     "../../scripts/ci/verify-artifact-provenance.rkt")

;; Register F7 (v1.00.31 W5): the artifact provenance gate. Declared wave
;; artifacts must be bound (SHA256SUMS), canonical, and internally consistent;
;; recorded heads must descend from the verified wave tip. Drift is a typed
;; blocked stop BEFORE any ladder action; the journal stays put so the next
;; invocation re-runs the gate. Strictness requires a current wave: the
;; version tag comes from the durable receipt branch (campaign/vX.Y.Z-wN); a
;; receipt without a version tag runs the lint in historical mode only.
(define (artifact-provenance-gate base-dir plan wave)
  (with-handlers
      ([exn:fail?
        (lambda (e)
          (delivery-effect-result
           'blocked
           (hasheq 'stage "delivery-preflight" 'reason (redact-delivery-text (exn-message e)))))])
    (define branch (default-delivery-receipt-branch base-dir plan wave))
    (define version-tag
      (let ([m (regexp-match #px"v[0-9]+\\.[0-9]+\\.[0-9]+" branch)]) (and m (car m))))
    (define repo
      (if (or (directory-exists? (build-path base-dir ".git"))
              (file-exists? (build-path base-dir ".git")))
          base-dir
          (build-path base-dir "q")))
    (define args
      (list (path->string artifact-provenance-lint-script)
            "--root"
            (path->string (path->complete-path repo))))
    (define args*
      (if version-tag
          (append args
                  (list "--current-wave"
                        (format "~a-w~a" version-tag wave)
                        "--wave-tip"
                        (default-delivery-receipt-head base-dir plan wave)))
          args))
    (define result
      (run-subprocess "racket"
                      #:args args*
                      #:directory base-dir
                      #:environment (controller-environment)
                      #:timeout 240
                      #:process-group? #t))
    (if (zero? (subprocess-result-exit-code result))
        (delivery-effect-result 'ok (hasheq 'stage "delivery-preflight"))
        (delivery-effect-result 'blocked
                                (hasheq 'stage
                                        "delivery-preflight"
                                        'reason
                                        (format "artifact provenance gate: ~a"
                                                (redact-delivery-text (subprocess-result-stdout
                                                                       result))))))))

;; Typed-stop reason preservation: gsd-delivery.py reports typed stops as
;; exit 2 with {"status":"delivery-pending","reason":…} on STDOUT while
;; stderr stays empty. Reading only stderr discarded every controller reason;
;; this parser surfaces the preserved, redacted reason and returns #f for
;; anything else (garbage, other statuses, empty output).
(define (parse-delivery-stop stdout target-stage)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define data (string->jsexpr stdout))
    (and (hash? data)
         (equal? (hash-ref data 'status #f) "delivery-pending")
         (delivery-effect-result
          'blocked
          (hasheq 'stage
                  target-stage
                  'reason
                  (redact-delivery-text (format "~a" (hash-ref data 'reason "delivery pending"))))))))

(define (default-delivery-controller base-dir plan wave target-stage)
  ;; The controller shell runs in the checkout identified by base-dir (or its
  ;; q/ child), exactly like delivery-readback. Tokens are added for THIS
  ;; process only; the reason is redacted on every failure path.
  (define repo
    (if (or (directory-exists? (build-path base-dir ".git"))
            (file-exists? (build-path base-dir ".git")))
        base-dir
        (build-path base-dir "q")))
  (define (run-controller . args)
    (with-handlers
        ([exn:fail? (lambda (e)
                      (delivery-effect-result
                       'blocked
                       (hasheq 'stage target-stage 'reason (redact-delivery-text (exn-message e)))))])
      (define result
        (run-subprocess "python3"
                        #:args (append (list (path->string default-delivery-controller-script))
                                       args
                                       (list "--repo"
                                             (path->string (path->complete-path repo))
                                             "--plan"
                                             plan
                                             "--wave"
                                             (number->string wave)))
                        #:directory base-dir
                        #:environment (controller-environment)
                        #:timeout 240
                        #:process-group? #t))
      (if (or (subprocess-result-timed-out? result)
              (subprocess-result-truncated? result)
              (not (zero? (subprocess-result-exit-code result))))
          ;; gsd-delivery.py reports typed stops as exit 2 with the reason on
          ;; STDOUT ({"status":"delivery-pending","reason":…}); stderr stays
          ;; empty. Surface the preserved, redacted reason instead of
          ;; discarding it with the blank stderr.
          (or (parse-delivery-stop (subprocess-result-stdout result) target-stage)
              (delivery-effect-result 'blocked
                                      (hasheq 'stage
                                              target-stage
                                              'reason
                                              (redact-delivery-text (subprocess-result-stderr
                                                                     result)))))
          (default-delivery-controller-interpret
           target-stage
           (string->jsexpr (subprocess-result-stdout result))))))
  ;; Map the journal's linear delivery targets to gsd-delivery.py actions.
  (case target-stage
    [("delivery-preflight")
     ;; Register F5: the read-only preflight action at the handoff seam —
     ;; branch published, remote tip at the verified head (evidence-only
     ;; drift allowed), required-check policy object present. It mutates
     ;; nothing; a pending result is a typed stop with the remedy.
     (run-controller "preflight"
                     "--expected-head"
                     (default-delivery-receipt-head base-dir plan wave)
                     "--expected-branch"
                     (default-delivery-receipt-branch base-dir plan wave))]
    [("implementation-review")
     ;; Durable review validation at the receipt identity: the frozen
     ;; binding-named evidence trio plus the schema-2 review artifact are
     ;; validated by the unchanged strict gate at the receipt head (never
     ;; the working tree). The review itself is produced by a genuine
     ;; independent non-author reviewer outside the controller; its absence
     ;; is a typed awaiting-review, never fabricated progress.
     (run-controller "review"
                     "--expected-head"
                     (default-delivery-receipt-head base-dir plan wave)
                     "--expected-branch"
                     (default-delivery-receipt-branch base-dir plan wave))]
    [("implementation-pr")
     ;; Resolve the durable branch identity first. A missing open PR is the
     ;; only blocked result that may proceed to deterministic creation; API,
     ;; fetch, or identity failures remain typed stops and never get retried
     ;; as a create attempt.
     (define pr-branch (default-delivery-receipt-branch base-dir plan wave))
     (define pr-receipt-head (default-delivery-receipt-head base-dir plan wave))
     (define resolved-pr
       (run-controller "resolve-pr" "--expected-branch" pr-branch "--expected-head" pr-receipt-head))
     (define resolved-pr-data (delivery-effect-result-data resolved-pr))
     (define resolved-pr-status (and (hash? resolved-pr-data) (hash-ref resolved-pr-data 'status #f)))
     (define resolved-pr-number (and (hash? resolved-pr-data) (hash-ref resolved-pr-data 'pr #f)))
     (define resolved-pr-head (and (hash? resolved-pr-data) (hash-ref resolved-pr-data 'head #f)))
     (cond
       [(and (eq? (delivery-effect-result-kind resolved-pr) 'ok)
             (equal? resolved-pr-status "resolved")
             (exact-nonnegative-integer? resolved-pr-number)
             (full-sha-string? resolved-pr-head))
        resolved-pr]
       [(and (eq? (delivery-effect-result-kind resolved-pr) 'ok)
             (equal? resolved-pr-status "resolved"))
        (delivery-effect-result
         'blocked
         (hasheq 'stage
                 target-stage
                 'reason
                 (format "controller resolved no usable PR identity for branch ~a" pr-branch)))]
       [(and (eq? (delivery-effect-result-kind resolved-pr) 'blocked)
             (equal? resolved-pr-status "none"))
        (run-controller "open-pr"
                        "--expected-branch"
                        pr-branch
                        "--expected-head"
                        (default-delivery-receipt-head base-dir plan wave))]
       [else resolved-pr])]
    [("implementation-ci")
     ;; CI is evaluated only after an exact open PR has been resolved from the
     ;; receipt branch. The PR number is never guessed or persisted.
     (define ci-branch (default-delivery-receipt-branch base-dir plan wave))
     (define ci-receipt-head (default-delivery-receipt-head base-dir plan wave))
     (define resolved-ci-pr
       (run-controller "resolve-pr" "--expected-branch" ci-branch "--expected-head" ci-receipt-head))
     (define resolved-ci-data (delivery-effect-result-data resolved-ci-pr))
     (define resolved-ci-status (and (hash? resolved-ci-data) (hash-ref resolved-ci-data 'status #f)))
     (define ci-pr-number (and (hash? resolved-ci-data) (hash-ref resolved-ci-data 'pr #f)))
     (define ci-pr-head (and (hash? resolved-ci-data) (hash-ref resolved-ci-data 'head #f)))
     (cond
       [(and (eq? (delivery-effect-result-kind resolved-ci-pr) 'ok)
             (equal? resolved-ci-status "resolved")
             (exact-nonnegative-integer? ci-pr-number)
             (full-sha-string? ci-pr-head))
        (run-controller "pr-ci"
                        "--pr"
                        (number->string ci-pr-number)
                        "--expected-branch"
                        ci-branch
                        "--expected-head"
                        (default-delivery-receipt-head base-dir plan wave))]
       [(and (eq? (delivery-effect-result-kind resolved-ci-pr) 'ok)
             (equal? resolved-ci-status "resolved"))
        (delivery-effect-result
         'blocked
         (hasheq 'stage
                 target-stage
                 'reason
                 (format "controller resolved no usable PR identity for branch ~a" ci-branch)))]
       [else resolved-ci-pr])]
    [("implementation-merged")
     ;; A protected merge needs (a) the PR identity resolved from the durable
     ;; receipt branch (never guessed), (b) the exact verified PR head and
     ;; branch, and (c) the frozen schema-2 evidence path. Resolve validates
     ;; that the PR tip descends from the receipt through evidence-only commits;
     ;; merge then enforces its exact-head contract at that actual PR tip.
     (define receipt-branch (default-delivery-receipt-branch base-dir plan wave))
     (define receipt-head (default-delivery-receipt-head base-dir plan wave))
     (define evidence (default-delivery-evidence-path plan wave))
     (define resolve
       (run-controller "resolve-pr"
                       "--expected-branch"
                       receipt-branch
                       "--expected-head"
                       receipt-head))
     (define resolve-data (delivery-effect-result-data resolve))
     ;; string->jsexpr hashes carry SYMBOL keys; a string key here never
     ;; matched and made the protected merge unreachable in production.
     (define pr-number (and (hash? resolve-data) (hash-ref resolve-data 'pr #f)))
     (define pr-head (and (hash? resolve-data) (hash-ref resolve-data 'head #f)))
     (cond
       ;; None/ambiguous PR, or shell failure: typed stop with the controller
       ;; reason — never invent a PR identity.
       [(not (eq? (delivery-effect-result-kind resolve) 'ok)) resolve]
       [(not (and (exact-nonnegative-integer? pr-number) (full-sha-string? pr-head)))
        (delivery-effect-result
         'blocked
         (hasheq 'stage
                 target-stage
                 'reason
                 (format "controller resolved no usable PR identity for branch ~a" receipt-branch)))]
       [else
        (run-controller "merge"
                        "--pr"
                        (number->string pr-number)
                        "--expected-head"
                        pr-head
                        "--expected-branch"
                        receipt-branch
                        "--evidence"
                        evidence)])]
    [("binding-prepared")
     ;; Prepare the durable, hash-named binding staging output from the
     ;; implementation receipt. The Python action self-resolves the merged
     ;; implementation PR from the receipt branch when --pr is omitted.
     (define receipt-branch (default-delivery-receipt-branch base-dir plan wave))
     (run-controller "prepare"
                     "--evidence"
                     (default-delivery-evidence-path plan wave)
                     "--output"
                     (default-binding-staging-path base-dir plan wave)
                     "--campaign-root"
                     (path->string base-dir)
                     "--expected-branch"
                     receipt-branch)]
    [("binding-review")
     ;; Validate the staged draft and its rebind guard. This is evidence
     ;; validation only; it never manufactures an approval.
     (run-controller "binding-review"
                     "--output"
                     (default-binding-staging-path base-dir plan wave)
                     "--campaign-root"
                     (path->string base-dir)
                     "--expected-branch"
                     (default-delivery-receipt-branch base-dir plan wave))]
    [("binding-pr")
     ;; Publish the validated staging trio on the deterministic fresh-main
     ;; binding branch, resolving an existing PR before creating a new one.
     (run-controller "binding-pr"
                     "--output"
                     (default-binding-staging-path base-dir plan wave)
                     "--campaign-root"
                     (path->string base-dir))]
    [("binding-ci")
     ;; Resolve the exact binding PR by branch, then check its exact fetched
     ;; head. Binding commits are fresh-main publication commits, so the
     ;; implementation receipt ancestry check is intentionally not applied.
     (binding-dispatch-stage-action run-controller target-stage plan wave "binding-ci")]
    [("binding-merged")
     ;; Reuse the protected merge workflow verbatim, with the binding path as
     ;; its evidence source and the exact resolved binding PR head as input.
     (binding-dispatch-stage-action run-controller target-stage plan wave "binding-merge")]
    [("governance")
     (run-controller "governance" "--expected-branch" (default-binding-branch plan wave))]
    [("sync")
     (run-controller "sync" "--expected-branch" (current-git-delivery-branch base-dir plan wave))]
    [else
     ;; Stages the controller shell cannot yet act on deterministically are
     ;; typed stops with an actionable reason — never fabricated progress.
     (delivery-effect-result 'blocked
                             (hasheq 'stage
                                     target-stage
                                     'reason
                                     (format "controller action for stage ~a not implemented; "
                                             target-stage)))]))

(define (default-delivery-controller-interpret target-stage data)
  (define status (and (hash? data) (hash-ref data 'status #f)))
  (cond
    [(equal? status "merged") (delivery-effect-result 'ok (hasheq 'stage target-stage))]
    ;; Preserve the payload for already-merged: the binding dispatch stage
    ;; actions consume the interpreted data and must see the idempotent
    ;; resume identity (status/pr/head) to route verbatim instead of
    ;; advancing on a stripped ok.
    [(equal? status "already-merged") (delivery-effect-result 'ok data)]
    [(equal? status "resolved") (delivery-effect-result 'ok data)]
    ;; implementation-review: the strict gate validated the durable review
    ;; at the receipt head; carry the binding evidence (reviewed-sha) forward.
    [(equal? status "reviewed") (delivery-effect-result 'ok data)]
    [(equal? status "opened") (delivery-effect-result 'ok data)]
    [(equal? status "exists") (delivery-effect-result 'ok data)]
    [(equal? status "already-published") (delivery-effect-result 'ok data)]
    [(equal? status "green") (delivery-effect-result 'ok data)]
    [(equal? status "pending-review") (delivery-effect-result 'ok data)]
    [(equal? status "governed") (delivery-effect-result 'ok data)]
    ;; W3 register F5: the preflight action's ready verdict — the handoff
    ;; seam cleared, the ladder action may run.
    [(equal? status "ready") (delivery-effect-result 'ok data)]
    [(equal? status "awaiting-review")
     (delivery-effect-result 'awaiting-review
                             (hasheq 'stage
                                     target-stage
                                     'reason
                                     (hash-ref data 'reason "awaiting genuine independent review")))]
    [(equal? status "synchronized") (delivery-effect-result 'ok (hasheq 'stage target-stage))]
    [(equal? status "delivered") (delivery-effect-result 'ok (hasheq 'stage target-stage))]
    [(equal? status "none")
     (delivery-effect-result
      'blocked
      (hasheq 'stage
              target-stage
              'status
              "none"
              'branch
              (hash-ref data 'branch "?")
              'reason
              (format "~a: no open pull request for branch ~a; open it for genuine independent review"
                      target-stage
                      (hash-ref data 'branch "?"))))]
    [else
     (delivery-effect-result 'blocked
                             (hasheq 'stage
                                     target-stage
                                     'reason
                                     (or (and (hash? data) (hash-ref data 'reason #f))
                                         "controller returned an unexpected status")))]))

(define (current-git-delivery-branch base-dir plan wave)
  ;; The durable wave record owns the delivery branch; the journal receipt is
  ;; the fallback when the record is mid-persist. Never guessed from cwd.
  (define record (load-campaign-record base-dir plan))
  (define w
    (and record
         (for/first ([x (in-list (campaign-record-waves record))]
                     #:when (= (campaign-wave-index x) wave))
           x)))
  (define from-wave (and w (campaign-wave-delivery-branch w)))
  (define journal (load-delivery-journal base-dir plan wave))
  (define from-receipt (and journal (hash-ref (hash-ref journal 'receipt #f) 'branch #f)))
  (cond
    [(and (string? from-wave) (not (equal? from-wave ""))) from-wave]
    [(string? from-receipt) from-receipt]
    [else
     (raise-argument-error 'current-git-delivery-branch "campaign wave with delivery branch" w)]))

;; Exact verified identity for the protected merge: the durable receipt is the
;; only authority (never a model claim or a fresh checkout HEAD).
(define (default-delivery-receipt-branch base-dir plan wave)
  (define journal (load-delivery-journal base-dir plan wave))
  (define branch (and journal (hash-ref (hash-ref journal 'receipt #f) 'branch #f)))
  (unless (and (string? branch) (positive? (string-length branch)))
    (raise-argument-error 'default-delivery-receipt-branch "receipt with branch" journal))
  branch)

(define (default-delivery-receipt-head base-dir plan wave)
  (define journal (load-delivery-journal base-dir plan wave))
  (define head (and journal (hash-ref (hash-ref journal 'receipt #f) 'head #f)))
  (unless (and (string? head) (= (string-length head) 40))
    (raise-argument-error 'default-delivery-receipt-head "receipt with 40-hex head" journal))
  head)

;; The frozen schema-2 evidence path is derivable from campaign+wave identity
;; (the same path gsd-delivery.py's binding_path computes) — never guessed
;; from a checkout.
(define (default-delivery-evidence-path plan wave)
  (format "docs/reports/gsd-wave-evidence/~a-w~a.rktd" plan wave))

;; Binding publication uses a deterministic branch and durable campaign-local
;; staging directory. These derivations mirror gsd-delivery.py and are never
;; taken from a mutable checkout HEAD.
(define (default-binding-branch plan wave)
  (string-append "binding/" (substring plan 0 (min 12 (string-length plan))) (format "-w~a" wave)))

(define (default-binding-staging-path base-dir plan wave)
  (path->string (build-path base-dir ".planning" "campaigns" plan (format "binding-w~a" wave))))

;; Binding CI/merge dispatch: resolve the deterministic binding PR by branch,
;; then act on the typed status. `run` is the controller seam (injectable for
;; tests; the production caller passes run-controller). Statuses are JSON
;; strings — string->jsexpr never produces symbols, so comparing against a
;; quoted symbol list never matches and would silently skip the CI/merge
;; action (fabricated progress). "already-merged" returns the resolve result
;; verbatim (idempotent resume); a resolved identity without a usable number
;; or head fails closed as a typed blocked stop.
(define (binding-dispatch-stage-action run target-stage plan wave action-name)
  (define binding-branch (default-binding-branch plan wave))
  ;; The run-controller seam appends --repo/--plan/--wave itself; per-action
  ;; flags must never duplicate them (argparse refuses unknown flags like
  ;; --plan-id and would turn every dispatch into a usage-error stop).
  (define binding-resolved (run "binding-resolve-pr" "--expected-branch" binding-branch))
  (define binding-resolved-data (delivery-effect-result-data binding-resolved))
  (define binding-resolved-status
    (and (hash? binding-resolved-data) (hash-ref binding-resolved-data 'status #f)))
  (define binding-pr-number
    (and (hash? binding-resolved-data) (hash-ref binding-resolved-data 'pr #f)))
  (define binding-pr-head
    (and (hash? binding-resolved-data) (hash-ref binding-resolved-data 'head #f)))
  (cond
    [(and (eq? (delivery-effect-result-kind binding-resolved) 'ok)
          (member binding-resolved-status '("resolved" "already-merged"))
          (exact-positive-integer? binding-pr-number)
          (full-sha-string? binding-pr-head))
     (if (equal? binding-resolved-status "already-merged")
         binding-resolved
         (if (equal? action-name "binding-ci")
             (run action-name
                  "--pr"
                  (number->string binding-pr-number)
                  "--expected-branch"
                  binding-branch
                  "--expected-head"
                  binding-pr-head)
             (run action-name
                  "--pr"
                  (number->string binding-pr-number)
                  "--expected-head"
                  binding-pr-head
                  "--expected-branch"
                  binding-branch
                  "--evidence"
                  (default-delivery-evidence-path plan wave))))]
    [(and (eq? (delivery-effect-result-kind binding-resolved) 'ok)
          (member binding-resolved-status '("resolved" "already-merged")))
     (delivery-effect-result
      'blocked
      (hasheq 'stage
              target-stage
              'reason
              (format "controller resolved no usable binding PR identity for branch ~a"
                      binding-branch)))]
    [else binding-resolved]))

(define (default-delivery-coordinator base-dir plan wave-index)
  (run-delivery-coordinator! base-dir plan wave-index #:controller default-delivery-controller))
