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
                  update-delivery-journal!))

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
         default-delivery-coordinator
         default-delivery-controller-interpret)

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

;; Next linear stage after `current` (a member of linear-delivery-stages).
(define (next-stage current)
  (define idx (indexof linear-delivery-stages current))
  (and idx (list-ref linear-delivery-stages (add1 idx))))

;; Durable eligibility re-check against an EXPLICIT expected fence. The fence
;; is captured once at step start: a takeover (fence bump) or cancellation
;; that lands while the controller runs must reject the continuation. The
;; receipt's attempt-fence is intentionally historical and never used here.
(define (durable-blocker dir plan wave expected-fence)
  (with-handlers ([exn:fail? (lambda (_) 'invalid-journal)])
    (define journal (load-delivery-journal dir plan wave))
    (define record (load-campaign-record dir plan))
    (and (not (and record (campaign-record-cancellation record)))
         (delivery-receipt-blocker journal record plan wave expected-fence))))

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
       [block-reason (blocked (format "delivery blocked: ~a" block-reason))]
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
          (delivery-effect-result 'blocked
                                  (hasheq 'stage
                                          target-stage
                                          'reason
                                          (redact-delivery-text (subprocess-result-stderr result))))
          (default-delivery-controller-interpret
           target-stage
           (string->jsexpr (subprocess-result-stdout result))))))
  ;; Map the journal's linear delivery targets to gsd-delivery.py actions.
  (case target-stage
    [("implementation-merged")
     ;; A protected merge needs (a) the PR identity resolved from the durable
     ;; receipt branch (never guessed), (b) the exact verified head and branch
     ;; from the receipt, and (c) the frozen schema-2 evidence path. The PR
     ;; lookup is the same fail-closed resolve-before-create the controller
     ;; already uses; zero/multiple open PRs become a typed stop.
     (define receipt-branch (default-delivery-receipt-branch base-dir plan wave))
     (define receipt-head (default-delivery-receipt-head base-dir plan wave))
     (define evidence (default-delivery-evidence-path plan wave))
     (define resolve (run-controller "resolve-pr" "--expected-branch" receipt-branch))
     (define pr-number (hash-ref (delivery-effect-result-data resolve) "pr" #f))
     (cond
       ;; None/ambiguous PR, or shell failure: typed stop with the controller
       ;; reason — never invent a PR identity.
       [(not (eq? (delivery-effect-result-kind resolve) 'ok)) resolve]
       [(not (exact-nonnegative-integer? pr-number))
        (delivery-effect-result 'blocked
                                (hasheq 'stage
                                        target-stage
                                        'reason
                                        (format "controller resolved no usable PR for branch ~a"
                                                receipt-branch)))]
       [else
        (run-controller "merge"
                        "--pr"
                        (number->string pr-number)
                        "--expected-head"
                        receipt-head
                        "--expected-branch"
                        receipt-branch
                        "--evidence"
                        evidence)])]
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
    [(equal? status "already-merged") (delivery-effect-result 'ok (hasheq 'stage target-stage))]
    [(equal? status "resolved") (delivery-effect-result 'ok data)]
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
      (hasheq
       'stage
       target-stage
       'reason
       (format "no open implementation PR for branch ~a; open it for genuine independent review"
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

(define (default-delivery-coordinator base-dir plan wave-index)
  (run-delivery-coordinator! base-dir plan wave-index #:controller default-delivery-controller))
