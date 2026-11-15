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
;; production seam is NATIVE (scripts/gsd-delivery-*.rkt) through the same
;; credential boundary as delivery-handoff (B2b wiring); no delivery stage
;; spawns python3 any more (Phase B final deletion). Model completion, journal
;; stages, and Verify receipts are NEVER delivery proof: only a controller
;; effect result carrying authenticated merge evidence does. The outer
;; run-campaign! loop re-enters after each step, so restarts resume at the
;; exact persisted stage without duplicating external effects.

(require racket/base
         racket/path
         racket/string
         "campaign-state.rkt"
         "campaign-repository.rkt"
         "delivery-receipt.rkt"
         "evidence-repair-controller.rkt"
         "evidence-author-repair.rkt"
         (only-in "planning-paths.rkt"
                  wave-evidence-report-rel-path
                  wave-evidence-report-rel-prefix
                  wave-review-report-rel-prefix
                  wave-validation-report-rel-prefix)
         (only-in "delivery-publication.rkt" replay-approved-publication! load-approved-publication)
         (only-in "delivery-journal.rkt"
                  delivery-stages
                  load-delivery-journal
                  update-delivery-journal!
                  load-remote-pending
                  record-remote-pending!
                  clear-remote-pending!
                  remote-pending-blocker)
         ;; BUG_PLAN automatic-review slice B: the automatic independent
         ;; review dispatch port. Imported for the stage wiring ONLY — the
         ;; hook itself (current-gsd-auto-review-runner) stays a plain
         ;; review-dispatch export so the runtime (slice C) wires the real
         ;; fresh-session factory without any runtime import reaching core.
         (only-in "review-dispatch.rkt"
                  current-gsd-auto-review-runner
                  review-runner-installed?
                  auto-review-dispatch!)
         ;; R76 (BUG-0076): shared layout-aware delivery-root resolution.
         (only-in "git-root-layout.rkt" delivery-root-or-raise git-root-marker?))

(provide delivery-effect-result
         delivery-effect-result?
         delivery-effect-result-kind
         delivery-effect-result-data
         delivery-outcome
         delivery-outcome?
         delivery-outcome-kind
         delivery-outcome-message
         delivery-outcome-data
         run-delivery-coordinator!
         default-delivery-controller
         binding-generation
         default-binding-branch
         default-binding-staging-path
         binding-dispatch-stage-action
         default-delivery-coordinator
         default-delivery-controller-interpret
         run-native-resolve-pr
         run-native-pr-ci
         run-native-review
         run-native-open-pr
         run-native-status
         run-native-governance
         run-native-merge
         run-native-prepare
         run-native-binding-review
         run-native-binding-pr
         run-native-binding-action
         default-active-delivery-evidence-path
         default-artifact-provenance-root
         resolve-coordinator-repo
         review-rejected-message)

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
;; 'ok | 'awaiting-review | 'retryable | 'blocked | 'review-rejected |
;; 'review-infra. data is a hash carrying 'stage (the requested target) plus
;; optional 'reason and delivery usage fields ('model-calls 'tokens 'cost).
;; 'review-rejected (BUG_PLAN automatic-review slice B) is ONLY ever produced
;; by an explicit head-bound REQUEST_CHANGES from the fresh reviewer port —
;; carrying the exact receipt head/branch/attempt/fence + findings — and
;; 'review-infra covers reviewer transport/timeout/stale/malformed output;
;; neither can masquerade as the other, and neither advances the journal.
(define (delivery-effect-result kind [data #hasheq()])
  (cons kind data))

(define (delivery-effect-result? v)
  (and (pair? v) (memq (car v) '(ok awaiting-review retryable blocked review-rejected review-infra))))

(define (delivery-effect-result-kind r)
  (car r))
(define (delivery-effect-result-data r)
  (cdr r))

(define (delivery-outcome kind [message ""] [data #f])
  ;; Legacy shape (kind . message) when there is no typed payload; a
  ;; three-element list (kind message data) when the effect produced typed
  ;; facts. Every consumer uses the accessors, so both shapes coexist.
  (define text
    (if (string? message)
        message
        (format "~a" message)))
  (if data
      (list kind text data)
      (cons kind text)))

(define (delivery-outcome? o)
  (and (pair? o)
       (symbol? (car o))
       (let ([rest (cdr o)]) (or (string? rest) (and (pair? rest) (string? (car rest)))))))

(define (delivery-outcome-kind o)
  (car o))

;; The operator-facing message (unchanged for legacy and typed outcomes).
(define (delivery-outcome-message o)
  (let ([rest (cdr o)])
    (if (string? rest)
        rest
        (car rest))))

;; The typed payload carried by an outcome (a hash for review rejections), or
;; #f for a legacy value. Retry authorization reads THIS, never the message.
(define (delivery-outcome-data o)
  (let ([rest (cdr o)]) (and (pair? rest) (cadr rest))))

;; Operator-facing message for a typed review rejection: the exact receipt
;; identity plus every finding, so a blocked wave surfaces the genuine
;; review verdict (never an opaque strict-gate failure).
;;
;; GSD simplification W5: this is a DISPLAY rendering of the typed rejection
;; payload. Internal retry authorization reads the typed hash that travels
;; alongside it on the delivery outcome (delivery-outcome-data); the text is
;; never parsed back into authorization.
(define (review-rejected-message data)
  ;; Lossless checkpoint envelope: findings may themselves contain delimiters
  ;; or newlines.
  (string-append "review-rejected: " (jsexpr->string data)))

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

;; Pure durable eligibility re-check against an EXPLICIT expected fence. The
;; fence is captured once at step start: a takeover (fence bump) or cancellation
;; that lands while the controller runs must reject the continuation. The
;; receipt's attempt-fence is intentionally historical and never used here.
;; BUG-0079 REVIEW-2 item 4: this is a PURE prerequisite again — the
;; side-effectful publication-only replay moved to ONE explicit initial
;; run-delivery-coordinator! transition (publication-replay-blocked!) and is
;; never re-run by these per-effect eligibility checks.
(define (durable-blocker dir plan wave expected-fence)
  (or (remote-pending-blocker dir plan wave)
      (with-handlers ([exn:fail? (lambda (_) 'invalid-journal)])
        (define journal (load-delivery-journal dir plan wave))
        (define record (load-campaign-record dir plan))
        (and (not (and record (campaign-record-cancellation record)))
             (delivery-receipt-blocker journal record plan wave expected-fence)))))

;; BUG-0079 REVIEW-2 item 4: the ONE explicit initial publication-only replay
;; transition. It runs only when publication is GENUINELY pending — a typed
;; remote-pending marker, or a missing journal receipt together with a durable
;; approved publication intent (the crash window between a confirmed
;; publication readback and receipt certification) — and never after the
;; journal reached the delivered stage. A failed replay surfaces as the
;; branch-not-published blocker; it is never swallowed into ladder admission.
;; Returns #f when the ladder may proceed, or the operator-facing blocked
;; message string.
(define (publication-replay-blocked! dir plan wave expected-fence)
  (define (read-current)
    (define rec (load-campaign-record dir plan))
    (and rec (equal? (campaign-fence-token rec) expected-fence) rec))
  (define journal
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (load-delivery-journal dir plan wave)))
  (define delivered? (and journal (equal? (hash-ref journal 'stage #f) "delivered")))
  (define pending?
    (and (not delivered?)
         (or (load-remote-pending dir plan wave)
             ;; missing receipt + a loadable bound intent. A publication file
             ;; that cannot even load also counts as pending so the replay
             ;; surfaces its typed refusal instead of a silent pass-through.
             (and (not (and journal (hash-ref journal 'receipt #f)))
                  (with-handlers ([exn:fail? (lambda (_) #t)])
                    (and (load-approved-publication dir plan wave) #t))))))
  (and
   pending?
   (let ()
     (define failure
       (with-handlers ([exn:fail? (lambda (e) e)])
         (replay-approved-publication! dir plan wave read-current)
         #f))
     (cond
       [(exn? failure)
        (format "delivery blocked: branch-not-published: approved-head publication replay failed: ~a"
                (exn-message failure))]
       [(load-remote-pending dir plan wave)
        =>
        (lambda (marker)
          (format
           "delivery blocked: branch-not-published: branch ~a head ~a is not published on origin; push the verified head and re-verify"
           (hash-ref marker 'branch "?")
           (hash-ref marker 'head "?")))]
       [else #f]))))

;; The operator-facing blocked message; enriched with the marker's branch and
;; head whenever the typed branch-not-published reason fired.
(define (blocked-message dir plan wave reason)
  (cond
    [(and (eq? reason 'branch-not-published) (load-remote-pending dir plan wave))
     =>
     (lambda (marker)
       (format
        "delivery blocked: branch-not-published: branch ~a head ~a is not published on origin; push the verified head and re-verify"
        (hash-ref marker 'branch "?")
        (hash-ref marker 'head "?")))]
    ;; A pre-composed operator-facing message (publication replay refusal)
    ;; passes through verbatim.
    [(string? reason) reason]
    [else (format "delivery blocked: ~a" reason)]))

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
     ;; BUG-0079 REVIEW-2 item 4: ONE explicit initial publication-only replay
     ;; transition, evaluated exactly once per invocation BEFORE any ladder
     ;; effect and before the pure eligibility blocker. Its message (if any)
     ;; becomes the blocked outcome verbatim.
     (define publication-message (publication-replay-blocked! dir plan wave start-fence))
     (define block-reason (or publication-message (durable-blocker dir plan wave start-fence)))
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
                         [(review-rejected)
                          (delivery-outcome 'review-rejected (review-rejected-message data) data)]
                         [(review-infra)
                          (delivery-outcome 'review-infra
                                            (hash-ref data 'reason "review infrastructure failure"))]
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
                      [(review-rejected)
                       (define preflight-data (delivery-effect-result-data preflight))
                       (delivery-outcome 'review-rejected
                                         (review-rejected-message preflight-data)
                                         preflight-data)]
                      [(review-infra) (delivery-outcome 'review-infra reason)]
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
                   [(review-rejected)
                    (delivery-outcome 'review-rejected (review-rejected-message data) data)]
                   [(review-infra)
                    (delivery-outcome 'review-infra
                                      (hash-ref data 'reason "review infrastructure failure"))]
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
;; machine around the NATIVE controller modules (scripts/gsd-delivery-*.rkt)
;; through the SAME credential boundary as delivery-readback
;; (controller-environment re-adds GH_TOKEN/GITHUB_TOKEN to an otherwise
;; scrubbed environment; every operator-visible reason is redacted).
;;
;; Phase B final deletion: EVERY action a stage can dispatch is native, so no
;; delivery stage spawns python3 and scripts/gsd-delivery.py is gone. An action
;; name nobody ported is now a typed blocked stop (never a shell-out).
;; One bounded effect per invocation; absent credentials or an action the
;; controller cannot safely take are typed stops, never fabricated success.

(require (only-in "delivery-handoff.rkt" controller-environment redact-delivery-text)
         (only-in "../../scripts/gsd-binding-data.rkt" active-paths)
         ;; Phase B slice S1: the READ-ONLY `preflight` action is native; slice
         ;; S2 adds the `sync` stage, and slice S5 the read-only ladder actions
         ;; (resolve-pr / pr-ci / review).
         (only-in "../../scripts/gsd-delivery-preflight.rkt" delivery-preflight!)
         (only-in "../../scripts/gsd-delivery-sync.rkt" delivery-sync!)
         ;; Phase B slice S5: the READ-ONLY ladder actions (resolve-pr, pr-ci,
         ;; review) are native too.
         (only-in "../../scripts/gsd-delivery-pr.rkt"
                  delivery-resolve-pr!
                  delivery-pr-ci!
                  delivery-review!)
         ;; Phase B slice S6: the `open-pr` mutation (write seam) plus the
         ;; read-only `status`/`governance` readback actions are native.
         (only-in "../../scripts/gsd-delivery-write.rkt" delivery-open-pr!)
         (only-in "../../scripts/gsd-delivery-status.rkt" delivery-status! delivery-governance!)
         ;; Phase B slice S9: the LAST MUTATING actions are native too — the
         ;; protected `merge` and `prepare` (scripts/gsd-delivery-merge.rkt) and
         ;; the whole `binding-*` ladder (scripts/gsd-delivery-binding.rkt). No
         ;; delivery stage spawns python3 any more, and there is no Python
         ;; fallback left: an unknown action name is a typed blocked stop.
         (only-in "../../scripts/gsd-delivery-merge.rkt" delivery-merge! delivery-prepare!)
         (only-in "../../scripts/gsd-delivery-binding.rkt"
                  delivery-binding-review!
                  delivery-binding-pr!
                  delivery-binding-ci!
                  delivery-binding-merged!
                  delivery-binding-resolve-pr!
                  delivery-binding-action!)
         racket/runtime-path
         json
         (only-in "../../sandbox/subprocess.rkt"
                  run-subprocess
                  subprocess-result-stdout
                  subprocess-result-stderr
                  subprocess-result-exit-code
                  subprocess-result-timed-out?
                  subprocess-result-truncated?))

(define-runtime-path artifact-provenance-lint-script
                     "../../scripts/ci/verify-artifact-provenance.rkt")

;; v1.00.33 W1 (BUG: scoped artifact checkout root). The git seam every
;; receipt-bound checkout selection runs through.
;; LAW: (repo-dir args) -> (values exit-code stdout-string) where repo-dir is
;; the directory git runs with -C in. The production default shells the same
;; `git` binary through the same credential boundary as the delivery seam;
;; tests script deterministic replies through it. A git that cannot even run
;; (missing binary, spawn failure) reports a nonzero exit, never a success.
(define (shell-git repo-dir args)
  (with-handlers ([exn:fail? (lambda (_) (values 1 ""))])
    (define result
      (run-subprocess "git"
                      #:args (append (list "-C"
                                           (if (string? repo-dir)
                                               repo-dir
                                               (path->string repo-dir)))
                                     args)
                      #:environment (controller-environment)
                      #:timeout 120
                      #:process-group? #t))
    (values (subprocess-result-exit-code result) (subprocess-result-stdout result))))

;; Parse `git worktree list --porcelain` into (list path-string head-string
;; branch-string-or-#f) records. A "detached" worktree carries #f for its
;; branch; unrecognized lines are skipped, never guessed. The base checkout
;; is the first record git prints; linked worktrees follow.
(define (parse-worktree-porcelain text)
  (define (emit path head branch acc)
    (if (and path head)
        (cons (list path head branch) acc)
        acc))
  (let loop ([lines (string-split text "\n")]
             [path #f]
             [head #f]
             [branch #f]
             [acc '()])
    (cond
      [(null? lines) (reverse (emit path head branch acc))]
      [else
       (define line (string-trim (car lines) "\r"))
       (cond
         [(string-prefix? line "worktree ")
          (loop (cdr lines)
                (substring line (string-length "worktree "))
                #f
                #f
                (emit path head branch acc))]
         [(string-prefix? line "HEAD ")
          (loop (cdr lines) path (substring line (string-length "HEAD ")) branch acc)]
         [(string-prefix? line "branch refs/heads/")
          (loop (cdr lines) path head (substring line (string-length "branch refs/heads/")) acc)]
         [(string-prefix? line "detached") (loop (cdr lines) path head #f acc)]
         [else (loop (cdr lines) path head branch acc)])])))

;; v1.00.33 W1 (BUG: scoped artifact checkout root): select the checkout the
;; artifact provenance gate is allowed to inspect, or #f (fail closed).
;; LAW: only a checkout that is EXACTLY the durable delivery receipt
;; qualifies — a worktree whose checked-out branch IS the receipt branch,
;; whose HEAD IS the verified 40-hex receipt head, and whose
;; `status --porcelain` is empty (no tracked modification, no planted
;; untracked artifact). The idle base checkout on main, a stale head, a
;; wrong branch, a detached HEAD, or a dirty/untracked worktree never
;; qualifies: artifact bytes that merely exist somewhere are never accepted.
;; Automatic-review slice B amendment: the automatic reviewer's evidence
;; trio is committed AFTER the receipt head (evidence-only, same law as the
;; controller's require_receipt_tip), so a checkout that is a CLEAN
;; receipt-branch descendant whose receipt-head..HEAD diff touches only
;; excluded evidence directories ALSO qualifies. Non-evidence drift after
;; the receipt is still refused outright (no source drift tolerated).
;; Only marker-less synthetic fixtures/plain source trees keep the legacy
;; root on Git enumeration failure. A Git marker with an unavailable or
;; corrupt repository MUST fail closed rather than inspecting idle main.
(define excluded-evidence-prefixes
  (list (wave-evidence-report-rel-prefix)
        (wave-review-report-rel-prefix)
        (wave-validation-report-rel-prefix)))

;; The receipt-tip law for a checkout HEAD: exactly the receipt head, or a
;; descendant over a strictly evidence-only tail (same ancestry +
;; excluded-diff rule the trusted controller enforces on the branch tip).
(define (receipt-lawful-checkout-head? run-git worktree receipt-head head)
  (or (equal? head receipt-head)
      (and (string? head)
           (full-sha-string? head)
           (let-values ([(anc-code _out)
                         (run-git worktree (list "merge-base" "--is-ancestor" receipt-head head))])
             (and (zero? anc-code)
                  (let-values ([(dcode dout) (run-git worktree
                                                      (list "diff" "--name-only" receipt-head head))])
                    (and (zero? dcode)
                         (andmap (lambda (line)
                                   (define path (string-trim line))
                                   (or (zero? (string-length path))
                                       (ormap (lambda (prefix) (string-prefix? path prefix))
                                              excluded-evidence-prefixes)))
                                 (string-split dout "\n")))))))))

;; R76 (BUG-0076): the ONE repository-identity selector shared by the
;; artifact-provenance root and the delivery controller seams (previously two
;; independent copies of the name-based base-dir-or-q selector). Layout-aware
;; resolution: a decoy outer repository beside an independent nested q, and
;; any broken .git marker, raise the typed git-root refusal (root+reason);
;; every seam maps that to its own typed stop — never a silent selection.
;; ONLY when no repository exists anywhere does the legacy naming survive
;; (base-dir itself for plain synthetic source trees, otherwise the legacy
;; child q/ root), preserving the marker-less fixture law: on Git
;; enumeration failure such unscoped roots keep flowing to the callers.
(define (resolve-coordinator-repo base-dir)
  (or (delivery-root-or-raise base-dir)
      (if (git-root-marker? base-dir)
          #f
          (build-path base-dir "q"))))

(define (default-artifact-provenance-root base-dir plan wave #:run-git [run-git shell-git])
  (define repo (resolve-coordinator-repo base-dir))
  (define-values (code listing) (run-git repo '("worktree" "list" "--porcelain")))
  (if (not (zero? code))
      (and (not (or (directory-exists? (build-path repo ".git"))
                    (file-exists? (build-path repo ".git"))))
           repo)
      (let ()
        (define receipt-branch (default-delivery-receipt-branch base-dir plan wave))
        (define receipt-head (default-delivery-receipt-head base-dir plan wave))
        (define (clean-exact-receipt-checkout? entry)
          (and
           (equal? (list-ref entry 2) receipt-branch)
           (receipt-lawful-checkout-head? run-git (list-ref entry 0) receipt-head (list-ref entry 1))
           (let-values ([(scode out) (run-git (list-ref entry 0) '("status" "--porcelain"))])
             (and (zero? scode) (equal? (string-trim out) "")))))
        (for/first ([entry (in-list (parse-worktree-porcelain listing))]
                    #:when (clean-exact-receipt-checkout? entry))
          (string->path (list-ref entry 0))))))

;; Register F7 (v1.00.31 W5): the artifact provenance gate. Declared wave
;; artifacts must be bound (SHA256SUMS), canonical, and internally consistent;
;; recorded heads must descend from the verified wave tip. Drift is a typed
;; blocked stop BEFORE any ladder action; the journal stays put so the next
;; invocation re-runs the gate. Strictness requires a current wave. The
;; version tag is taken from the durable receipt branch when it carries one
;; (campaign/vX.Y.Z-wN), and otherwise from the frozen campaign manifest title
;; ("v1.00.31 ..."), which is independent of the branch naming convention —
;; the executor's own branches are campaign/<hash8>/w<N>, so relying on the
;; branch alone silently degraded the gate to historical (advisory) mode.
;; v1.00.33 W1: the lint runs ONLY on the root selected by
;; default-artifact-provenance-root — the clean worktree exactly bound to
;; the durable receipt — and refuses (typed blocked stop, journal stays
;; put) when no such checkout exists, never falling back to the idle base.
(define (plan-title-version-tag base-dir plan)
  (with-handlers ([exn:fail? (lambda (_e) #f)])
    (define rec (load-campaign-record base-dir plan))
    (define title (campaign-manifest-title (campaign-record-manifest rec)))
    (define m (and (string? title) (regexp-match #px"v?[0-9]+\\.[0-9]+\\.[0-9]+" title)))
    (and m (car m))))

(define (artifact-provenance-gate base-dir plan wave)
  (with-handlers
      ([exn:fail?
        (lambda (e)
          (delivery-effect-result
           'blocked
           (hasheq 'stage "delivery-preflight" 'reason (redact-delivery-text (exn-message e)))))])
    (define branch (default-delivery-receipt-branch base-dir plan wave))
    (define head (default-delivery-receipt-head base-dir plan wave))
    (define version-tag
      (or (let ([m (regexp-match #px"v[0-9]+\\.[0-9]+\\.[0-9]+" branch)]) (and m (car m)))
          (plan-title-version-tag base-dir plan)))
    (define root (default-artifact-provenance-root base-dir plan wave))
    (cond
      [(not root)
       (delivery-effect-result
        'blocked
        (hasheq
         'stage
         "delivery-preflight"
         'reason
         (format
          "no clean receipt-bound checkout: no worktree on branch ~a at the verified head ~a is clean; refusing to lint any other checkout"
          branch
          head)))]
      [else
       (define lint-args
         (list (path->string artifact-provenance-lint-script)
               "--root"
               (path->string (path->complete-path root))))
       (define args*
         (if version-tag
             (append lint-args
                     (list "--current-wave" (format "~a-w~a" version-tag wave) "--wave-tip" head))
             lint-args))
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
                                                                          result))))))])))

;; ============================================================
;; Phase B slices S5/S6: native delivery actions
;; ============================================================
;;
;; `resolve-pr`, `pr-ci` and `review` were the remaining READ-ONLY actions
;; still served by the Python controller. They are now the native
;; scripts/gsd-delivery-pr.rkt actions, dispatched with exactly the arguments
;; the replaced dispatch carried: the controller dispatch appended
;; `--repo <resolved>/--plan/--wave`, so the native call receives plan/wave
;; explicitly and the repository resolved the same way run-native-sync does
;; (`path->complete-path` of the resolved coordinator repo). Everything else
;; is IDENTICAL to the replaced dispatch:
;;   * the action's JSON-shaped result goes through
;;     default-delivery-controller-interpret, so "resolved"/"none"/"green"/
;;     "reviewed"/"awaiting-review" keep mapping to the same typed effect the
;;     action's stdout produced;
;;   * a typed refusal is converted through the same with-handlers shape the
;;     preflight/sync seams use — a redacted blocked stop carrying the
;;     actionable reason, which for a native refusal IS the exact reason
;;     string the action's {"status":"delivery-pending","reason":…} payload
;;     carried;
;;   * no stage ever spawns python3.
;;
;; Phase B slice S6 repeats the same discipline for the `open-pr` mutation
;; (scripts/gsd-delivery-write.rkt) and the read-only `status`/`governance`
;; readback actions (scripts/gsd-delivery-status.rkt): same repository value,
;; same `--plan/--wave` identity, same campaign root / expected branch the
;; replaced dispatch carried, the same interpreter (so "opened"/"governed"/
;; "delivered" keep their typed effect), and the same redacted blocked stop on
;; refusal.
;;
;; Phase B slice S9 closed the ladder: the protected `merge`, the binding
;; `prepare` and every `binding-*` action are native (scripts/
;; gsd-delivery-merge.rkt and scripts/gsd-delivery-binding.rkt). Phase B final
;; deletion removed the last Python fallback, so EVERY action a delivery stage
;; can dispatch is native and an unknown/legacy name is a typed blocked stop:
;;
;;   action               native implementation
;;   preflight            delivery-preflight!
;;   sync                 delivery-sync!
;;   resolve-pr           delivery-resolve-pr!
;;   pr-ci                delivery-pr-ci!
;;   review               delivery-review!
;;   open-pr              delivery-open-pr!
;;   status               delivery-status!
;;   governance           delivery-governance!
;;   merge                delivery-merge!
;;   prepare              delivery-prepare!
;;   binding-review       delivery-binding-review!
;;   binding-pr           delivery-binding-pr!
;;   binding-resolve-pr   delivery-binding-resolve-pr!
;;   binding-ci           delivery-binding-ci!
;;   binding-merge        delivery-binding-merged!
;;   (any other name)     typed blocked stop — nothing was published
;;
;; These helpers are provided ONLY so the stage wiring is testable directly: the
;; `implementation-review` stage reaches its dispatch only after the durable
;; evidence-readiness gate, whose ready state needs a full owned-proof
;; fixture. That end-to-end path stays covered by the existing production
;; review fixtures; the wiring itself is covered here.
(define (native-stage-blocked target-stage message)
  (delivery-effect-result 'blocked
                          (hasheq 'stage target-stage 'reason (redact-delivery-text message))))

;; The ONE repository value every native action receives: the shared
;; layout-aware coordinator root (R76) — the exact root the replaced dispatch
;; passed as `--repo` (`path->complete-path` of the same value).
(define (native-delivery-repo base-dir)
  (path->complete-path (resolve-coordinator-repo base-dir)))

(define (run-native-resolve-pr base-dir plan wave target-stage branch expected-head)
  (with-handlers ([exn:fail? (lambda (e) (native-stage-blocked target-stage (exn-message e)))])
    (default-delivery-controller-interpret target-stage
                                           (delivery-resolve-pr! (native-delivery-repo base-dir)
                                                                 plan
                                                                 wave
                                                                 branch
                                                                 #:expected-head expected-head))))

(define (run-native-pr-ci base-dir plan wave target-stage pr-number branch expected-head)
  (with-handlers ([exn:fail? (lambda (e) (native-stage-blocked target-stage (exn-message e)))])
    (default-delivery-controller-interpret target-stage
                                           (delivery-pr-ci! (native-delivery-repo base-dir)
                                                            pr-number
                                                            branch
                                                            #:expected-head expected-head))))

(define (run-native-review base-dir plan wave target-stage expected-head expected-branch generation)
  (with-handlers ([exn:fail? (lambda (e) (native-stage-blocked target-stage (exn-message e)))])
    (default-delivery-controller-interpret
     target-stage
     (delivery-review! (native-delivery-repo base-dir)
                       plan
                       wave
                       expected-head
                       expected-branch
                       ;; Same campaign root / generation the Python
                       ;; invocation passed via --campaign-root/--generation.
                       ;; delivery-review! takes the root as a PATH (the Python
                       ;; CLI argument was text), so pass it unchanged rather
                       ;; than stringifying it.
                       #:campaign-root base-dir
                       #:generation generation))))

;; open-pr: the SAME `--expected-branch`/`--expected-head` the Python action
;; received. The mutation keeps its EXACT order (resolve-before-create, forced
;; single-branch refresh, receipt-tip pin, then create) inside the native
;; action, and its "opened" payload goes through the same interpreter.
(define (run-native-open-pr base-dir plan wave target-stage expected-branch expected-head)
  (with-handlers ([exn:fail? (lambda (e) (native-stage-blocked target-stage (exn-message e)))])
    (default-delivery-controller-interpret target-stage
                                           (delivery-open-pr! (native-delivery-repo base-dir)
                                                              plan
                                                              wave
                                                              expected-branch
                                                              #:expected-head expected-head))))

;; status: the delivered readback. The campaign root travels UNCHANGED (the
;; Python invocation passed `--campaign-root <base-dir>`); the native action
;; takes it as a path, so it is handed the same value rather than stringified.
(define (run-native-status base-dir plan wave target-stage)
  (with-handlers ([exn:fail? (lambda (e) (native-stage-blocked target-stage (exn-message e)))])
    (default-delivery-controller-interpret
     target-stage
     (delivery-status! (native-delivery-repo base-dir) plan wave #:campaign-root base-dir))))

;; governance: the read-only protected-publication check, with the same
;; deterministic binding branch (generation-aware) the Python action received.
(define (run-native-governance base-dir plan wave target-stage expected-branch)
  (with-handlers ([exn:fail? (lambda (e) (native-stage-blocked target-stage (exn-message e)))])
    (default-delivery-controller-interpret
     target-stage
     (delivery-governance! (native-delivery-repo base-dir) plan wave expected-branch))))

;; ============================================================
;; Phase B slice S9: merge / prepare / binding-* native actions
;; ============================================================
;;
;; Each helper receives EXACTLY the values the replaced Python dispatch call
;; built, in the same order, so the native action sees the same identity the
;; Python action saw (`run-legacy-python-controller` appends
;; `--repo/--plan/--wave`; the helpers pass the resolved repository and
;; plan/wave explicitly):
;;
;;   merge      --pr / --expected-head / --expected-branch / --evidence
;;   prepare    --evidence / --output / --campaign-root / --expected-branch /
;;              --expected-head (no --pr: prepare self-resolves the merged PR)
;;   binding-review  --output / --campaign-root / --expected-branch
;;   binding-pr      --output / --campaign-root
;;
;; The JSON-shaped result goes through the SAME interpreter as every other
;; action, and a typed refusal becomes the same redacted blocked stop, so the
;; journal text/status is unchanged in both directions.

;; merge: the protected squash gate for the resolved implementation PR head.
(define (run-native-merge base-dir
                          plan
                          wave
                          target-stage
                          pr-number
                          expected-head
                          expected-branch
                          evidence)
  (with-handlers ([exn:fail? (lambda (e) (native-stage-blocked target-stage (exn-message e)))])
    (default-delivery-controller-interpret target-stage
                                           (delivery-merge! (native-delivery-repo base-dir)
                                                            plan
                                                            wave
                                                            pr-number
                                                            expected-head
                                                            expected-branch
                                                            evidence))))

;; prepare: `--campaign-root` travels UNCHANGED (the Python invocation passed
;; `--campaign-root <base-dir>` and the native action takes it as a path), and
;; `#f` for the number keeps the action's own merged-PR self-resolution.
(define (run-native-prepare base-dir
                            plan
                            wave
                            target-stage
                            evidence
                            output
                            expected-branch
                            expected-head)
  (with-handlers ([exn:fail? (lambda (e) (native-stage-blocked target-stage (exn-message e)))])
    (default-delivery-controller-interpret target-stage
                                           (delivery-prepare! (native-delivery-repo base-dir)
                                                              plan
                                                              wave
                                                              #f
                                                              evidence
                                                              base-dir
                                                              output
                                                              #:expected-branch expected-branch
                                                              #:expected-head expected-head))))

;; binding-review: validate the durable staged trio (never manufacture an
;; approval) at the deterministic staging directory.
(define (run-native-binding-review base-dir plan wave target-stage output expected-branch)
  (with-handlers ([exn:fail? (lambda (e) (native-stage-blocked target-stage (exn-message e)))])
    (default-delivery-controller-interpret target-stage
                                           (delivery-binding-review! (native-delivery-repo base-dir)
                                                                     plan
                                                                     wave
                                                                     output
                                                                     #:campaign-root base-dir
                                                                     #:expected-branch
                                                                     expected-branch))))

;; binding-pr: publish the validated staging trio on the deterministic
;; fresh-main binding branch (resolve-before-create inside the action).
(define (run-native-binding-pr base-dir plan wave target-stage output)
  (with-handlers ([exn:fail? (lambda (e) (native-stage-blocked target-stage (exn-message e)))])
    (default-delivery-controller-interpret target-stage
                                           (delivery-binding-pr! (native-delivery-repo base-dir)
                                                                 plan
                                                                 wave
                                                                 output
                                                                 #:campaign-root base-dir))))

;; The native action surfaces of the binding CI/merge dispatch seam. The
;; dispatch (binding-dispatch-stage-action) builds the per-action flags exactly
;; as the deleted CLI did, so it is served by the native CLI surface that
;; carries the SAME flag contract and the SAME per-action requirement text.
(define binding-dispatch-action-surface
  (hash "binding-resolve-pr"
        delivery-binding-action!
        "binding-ci"
        delivery-binding-action!
        "binding-merge"
        delivery-binding-action!))

;; The `run` seam binding-dispatch-stage-action needs: LAW
;;   (action-name flag ...) -> delivery-effect-result
;; It appends `--repo/--plan/--wave` in the SAME position the CLI dispatch did,
;; so the per-action flags the dispatch built are untouched, and interprets the
;; native JSON result through the same interpreter. A name outside the surface
;; table is UNKNOWN/legacy: there is no shell-out left (the Python controller is
;; deleted), so it is a typed blocked stop that published nothing — never a
;; fabricated success and never a subprocess.
(define (run-native-binding-action base-dir plan wave target-stage action-name . flags)
  (define action
    (if (symbol? action-name)
        (symbol->string action-name)
        action-name))
  (define surface (and (string? action) (hash-ref binding-dispatch-action-surface action #f)))
  (cond
    [surface
     (with-handlers ([exn:fail? (lambda (e) (native-stage-blocked target-stage (exn-message e)))])
       (default-delivery-controller-interpret
        target-stage
        (surface (append (list action)
                         flags
                         (list "--repo"
                               (path->string (native-delivery-repo base-dir))
                               "--plan"
                               plan
                               "--wave"
                               (number->string wave))))))]
    [else
     ;; Unknown/legacy action: the native controller implements a CLOSED set of
     ;; actions, so this is a typed refusal naming the action. Nothing was
     ;; published, no credential was used and no interpreter was spawned.
     (native-stage-blocked
      target-stage
      (format (string-append "delivery action ~a is not implemented by the native controller; "
                             "nothing was published")
              (or action (format "~s" action-name))))]))

;; The seam closure the binding dispatch stages are handed.
(define (native-binding-stage-run base-dir plan wave target-stage)
  (lambda (action . flags)
    (apply run-native-binding-action base-dir plan wave target-stage action flags)))

;; ============================================================
;; The default stage dispatch (every action is native)
;; ============================================================
;;
;; Phase B final deletion: scripts/gsd-delivery.py and this module's last
;; caller of it are gone. There is no Python fallback and no `python3` spawn on
;; any delivery path — the closed action set above IS the controller.
(define (default-delivery-controller base-dir plan wave target-stage)
  (define (run-native-preflight)
    ;; Phase B slice S1: the read-only preflight stage is native Racket. It
    ;; keeps the effect contract of the replaced Python dispatch: the same
    ;; receipt-bound inputs, the same "ready" -> ok interpretation, and every
    ;; typed refusal becomes a redacted blocked stop with the exact remedy.
    (with-handlers
        ([exn:fail? (lambda (e)
                      (delivery-effect-result
                       'blocked
                       (hasheq 'stage target-stage 'reason (redact-delivery-text (exn-message e)))))])
      (define repo (resolve-coordinator-repo base-dir))
      (default-delivery-controller-interpret
       target-stage
       (delivery-preflight! repo
                            plan
                            wave
                            (default-delivery-receipt-head base-dir plan wave)
                            (default-delivery-receipt-branch base-dir plan wave)))))
  (define (run-native-sync)
    ;; Phase B slice S2: the `sync` stage is native Racket with the Python
    ;; action's exact contract (clean tree, expected branch or main, non-forced
    ;; refresh, fast-forward only, never switching branches) over the shared
    ;; budget/credential/redaction seam. "synchronized" -> ok through the same
    ;; interpreter; every typed refusal is a redacted blocked stop.
    (with-handlers
        ([exn:fail? (lambda (e)
                      (delivery-effect-result
                       'blocked
                       (hasheq 'stage target-stage 'reason (redact-delivery-text (exn-message e)))))])
      (define repo (resolve-coordinator-repo base-dir))
      (default-delivery-controller-interpret
       target-stage
       (delivery-sync! (path->complete-path repo) (current-git-delivery-branch base-dir plan wave)))))
  ;; Map the journal's linear delivery targets to their controller actions.
  ;; EVERY stage below is native (python3-free) since Phase B slice S9:
  ;; delivery-preflight, sync, the read-only ladder actions resolve-pr / pr-ci /
  ;; review, the `open-pr` mutation, the status/governance readbacks, the
  ;; protected `merge`, `prepare`, and every `binding-*` action. Since the Phase
  ;; B final deletion no delivery stage reaches a Python controller at all — the
  ;; closed action set below is the whole controller.
  (case target-stage
    ;; Register F5: the read-only preflight action at the handoff seam —
    ;; branch published, remote tip at the verified head (evidence-only
    ;; drift allowed), required-check policy object present. It mutates
    ;; nothing; a typed refusal is a blocked stop with the remedy. Native
    ;; since Phase B slice S1 (this stage never spawns python3).
    [("delivery-preflight") (run-native-preflight)]
    [("implementation-review")
     ;; Installed production runner: prepare -> fresh independent review ->
     ;; trusted strict validation/publication. Absent ports retain manual
     ;; validation; a typed dispatch refusal never falls back to approval.
     (define record (load-campaign-record base-dir plan))
     (define fence (and record (campaign-fence-token record)))
     (define (repair-live?)
       (define now (load-campaign-record base-dir plan))
       (and now
            record
            (equal? (campaign-fence-token now) fence)
            (not (campaign-record-cancellation now))
            (not (durable-blocker base-dir plan wave fence))))
     (define repair-readiness (resume-evidence-repair! base-dir plan wave #:guard repair-live?))
     (define readiness
       (if (equal? (hash-ref repair-readiness 'code #f) "evidence-repair-required")
           ;; A separate durable allowance, never evidence-only permission or
           ;; an invented rejection. The finalizer schedules an ordinary fresh
           ;; attempt from this receipt-pinned source, not a new checkout.
           (authorize-evidence-author-repair! base-dir plan wave #:guard repair-live?)
           repair-readiness))
     (if (not (equal? (hash-ref readiness 'status #f) "ready"))
         (delivery-effect-result
          'blocked
          (hash-set readiness 'reason (hash-ref readiness 'code "evidence-repair-required")))
         (or (and (review-runner-installed?)
                  (auto-review-dispatch! base-dir
                                         plan
                                         wave
                                         "implementation-review"
                                         #:generation (binding-generation base-dir plan wave)))
             ;; Native since Phase B slice S5: the Python `review` action's
             ;; exact receipt-bound inputs (head, branch, campaign root and
             ;; binding generation).
             (run-native-review base-dir
                                plan
                                wave
                                target-stage
                                (default-delivery-receipt-head base-dir plan wave)
                                (default-delivery-receipt-branch base-dir plan wave)
                                (binding-generation base-dir plan wave))))]
    [("implementation-pr")
     ;; Resolve the durable branch identity first. A missing open PR is the
     ;; only blocked result that may proceed to deterministic creation; API,
     ;; fetch, or identity failures remain typed stops and never get retried
     ;; as a create attempt.
     (define pr-branch (default-delivery-receipt-branch base-dir plan wave))
     (define pr-receipt-head (default-delivery-receipt-head base-dir plan wave))
     ;; Native since Phase B slice S5 (the read-only resolve never spawns
     ;; python3); a typed refusal keeps the Python reason text.
     (define resolved-pr
       (run-native-resolve-pr base-dir plan wave target-stage pr-branch pr-receipt-head))
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
        ;; Native since Phase B slice S6: the same --expected-branch /
        ;; --expected-head the Python action received. Creating a pull request
        ;; is the ONE mutating step of this stage; it never spawns python3.
        (run-native-open-pr base-dir
                            plan
                            wave
                            target-stage
                            pr-branch
                            (default-delivery-receipt-head base-dir plan wave))]
       [else resolved-pr])]
    [("implementation-ci")
     ;; CI is evaluated only after an exact open PR has been resolved from the
     ;; receipt branch. The PR number is never guessed or persisted.
     (define ci-branch (default-delivery-receipt-branch base-dir plan wave))
     (define ci-receipt-head (default-delivery-receipt-head base-dir plan wave))
     (define resolved-ci-pr
       (run-native-resolve-pr base-dir plan wave target-stage ci-branch ci-receipt-head))
     (define resolved-ci-data (delivery-effect-result-data resolved-ci-pr))
     (define resolved-ci-status (and (hash? resolved-ci-data) (hash-ref resolved-ci-data 'status #f)))
     (define ci-pr-number (and (hash? resolved-ci-data) (hash-ref resolved-ci-data 'pr #f)))
     (define ci-pr-head (and (hash? resolved-ci-data) (hash-ref resolved-ci-data 'head #f)))
     (cond
       [(and (eq? (delivery-effect-result-kind resolved-ci-pr) 'ok)
             (equal? resolved-ci-status "resolved")
             (exact-nonnegative-integer? ci-pr-number)
             (full-sha-string? ci-pr-head))
        ;; Native since Phase B slice S5: same --pr/--expected-branch/
        ;; --expected-head the Python action received.
        (run-native-pr-ci base-dir
                          plan
                          wave
                          target-stage
                          ci-pr-number
                          ci-branch
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
     (define evidence (default-active-delivery-evidence-path base-dir plan wave))
     (define resolve
       ;; Native since Phase B slice S5 (read-only resolve, no python3).
       (run-native-resolve-pr base-dir plan wave target-stage receipt-branch receipt-head))
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
        ;; Native since Phase B slice S9: the SAME `merge`/`--pr`/
        ;; `--expected-head`/`--expected-branch`/`--evidence` the Python
        ;; invocation carried (the protected squash PUT is the ONE mutation).
        (run-native-merge base-dir
                          plan
                          wave
                          target-stage
                          pr-number
                          pr-head
                          receipt-branch
                          evidence)])]
    [("binding-prepared")
     ;; Prepare the durable, hash-named binding staging output from the
     ;; implementation receipt. The Python action self-resolves the merged
     ;; implementation PR from the receipt branch when --pr is omitted; the
     ;; receipt head disambiguates branches carrying multiple merged PRs
     ;; (lawful across repair generations).
     (define receipt-branch (default-delivery-receipt-branch base-dir plan wave))
     ;; Native since Phase B slice S9: the SAME `--evidence`/`--output`/
     ;; `--campaign-root`/`--expected-branch`/`--expected-head` the Python
     ;; invocation carried (no `--pr`: prepare self-resolves the merged PR).
     (run-native-prepare base-dir
                         plan
                         wave
                         target-stage
                         (default-active-delivery-evidence-path base-dir plan wave)
                         (default-binding-staging-path base-dir plan wave)
                         receipt-branch
                         (default-delivery-receipt-head base-dir plan wave))]
    [("binding-review")
     ;; Post-merge staged binding review: validation only, plus (slice B)
     ;; automatic finalization of a still-pending staged trio through the
     ;; fresh reviewer port. The controller never manufactures an approval.
     (or (and (review-runner-installed?)
              (auto-review-dispatch! base-dir
                                     plan
                                     wave
                                     "binding-review"
                                     #:generation (binding-generation base-dir plan wave)
                                     #:staging-root
                                     (default-binding-staging-path base-dir plan wave)))
         ;; Native since Phase B slice S9: the SAME `--output`/
         ;; `--campaign-root`/`--expected-branch` the Python invocation carried.
         (run-native-binding-review base-dir
                                    plan
                                    wave
                                    target-stage
                                    (default-binding-staging-path base-dir plan wave)
                                    (default-delivery-receipt-branch base-dir plan wave)))]
    [("binding-pr")
     ;; Publish the validated staging trio on the deterministic fresh-main
     ;; binding branch, resolving an existing PR before creating a new one.
     ;; Native since Phase B slice S9 (same `--output`/`--campaign-root`).
     (run-native-binding-pr base-dir
                            plan
                            wave
                            target-stage
                            (default-binding-staging-path base-dir plan wave))]
    [("binding-ci")
     ;; Resolve the exact binding PR by branch, then check its exact fetched
     ;; head. Binding commits are fresh-main publication commits, so the
     ;; implementation receipt ancestry check is intentionally not applied.
     ;; Native since Phase B slice S9: the dispatch keeps building the SAME
     ;; per-action flags; its `run` seam now serves them from the native action.
     (binding-dispatch-stage-action (native-binding-stage-run base-dir plan wave target-stage)
                                    target-stage
                                    plan
                                    wave
                                    "binding-ci"
                                    #:base-dir base-dir)]
    [("binding-merged")
     ;; Reuse the protected merge workflow verbatim, with the binding path as
     ;; its evidence source and the exact resolved binding PR head as input.
     (binding-dispatch-stage-action (native-binding-stage-run base-dir plan wave target-stage)
                                    target-stage
                                    plan
                                    wave
                                    "binding-merge"
                                    #:base-dir base-dir)]
    [("governance")
     ;; Native since Phase B slice S6: the same generation-aware deterministic
     ;; binding branch the Python invocation passed via --expected-branch.
     (run-native-governance base-dir
                            plan
                            wave
                            target-stage
                            (default-binding-branch plan wave #:base-dir base-dir))]
    ;; The campaign working checkout synchronizes with the published bytes
    ;; (fast-forward-only, never switching branches): either the wave branch
    ;; itself or the shared main checkout the controller runs from. Native
    ;; since Phase B slice S2 (this stage never spawns python3).
    [("sync") (run-native-sync)]
    ;; Terminal stage: the authenticated delivered readback (controller
    ;; `status`) is the ONLY effect that completes the ladder — the same
    ;; receipt-bound proof the full-loop checkpoint finalizes DONE from.
    ;; default-delivery-controller-interpret already maps its "delivered"
    ;; status to an ok effect; this wiring makes the terminal stage
    ;; reachable instead of an unimplemented stop. Native since Phase B slice
    ;; S6 (the same `--campaign-root <base-dir>` the Python invocation passed).
    [("delivered") (run-native-status base-dir plan wave target-stage)]
    [else
     ;; Stages with no controller action at all are typed stops with an
     ;; actionable reason — never fabricated progress.
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
    [(equal? status "already-staged") (delivery-effect-result 'ok data)]
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
    ;; Automatic independent review (slice B): an explicit head-bound
    ;; REQUEST_CHANGES surfaces as its own typed effect (the runtime
    ;; integration checkpoints a fresh attempt from it); reviewer
    ;; infrastructure failures never masquerade as a rejection or approval.
    [(equal? status "review-rejected") (delivery-effect-result 'review-rejected data)]
    [(equal? status "review-infra") (delivery-effect-result 'review-infra data)]
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
;; (the same path scripts/gsd-delivery-pr.rkt's binding-path computes) — never
;; guessed from a checkout.
(define (default-delivery-evidence-path plan wave #:base-dir [base-dir #f])
  (define generation
    (if base-dir
        (binding-generation base-dir plan wave)
        0))
  (wave-evidence-report-rel-path plan wave generation))

(define (q-prefixed-path->repo-relative base-dir path)
  (if (and (string? path)
           (string-prefix? path "q/")
           (or (directory-exists? (build-path base-dir "q"))
               (equal? (path->string (file-name-from-path (path->complete-path base-dir))) "q")))
      (substring path 2)
      path))

(define (default-active-delivery-evidence-path base-dir plan wave)
  ;; Active source paths come from the immutable campaign snapshot when it is
  ;; available. If a local synthetic test fixture has no snapshot yet, fall back
  ;; to the deterministic hash path rather than fabricating a coordinator-level
  ;; stop; the native status action still refuses without the authoritative
  ;; journal, so nothing is proven by the fallback.
  (with-handlers ([exn:fail? (lambda (_e)
                               (default-delivery-evidence-path plan wave #:base-dir base-dir))])
    (define generation (binding-generation base-dir plan wave))
    (q-prefixed-path->repo-relative base-dir
                                    (hash-ref (active-paths base-dir plan wave generation)
                                              'evidence))))

;; Binding publication uses a deterministic branch and durable campaign-local
;; staging directory. These derivations mirror scripts/gsd-delivery-pr.rkt's
;; binding-path / scripts/gsd-delivery-status.rkt's binding-branch and are never
;; taken from a mutable checkout HEAD.
;; v1.00.33 W0 (audit §21.4): republication generation = the number of PRIOR
;; immutable receipts in the wave's journal (each repair-tail reconciliation
;; appends one). Generation n >= 1 publishes under an -r<n> branch/staging
;; directory because governance admits exactly one merged PR per binding
;; branch; load-delivery-journal refuses a malformed history, so a journal
;; that cannot prove its past never reaches publication under a guessed name.
(define (binding-generation base-dir plan wave)
  (define journal (load-delivery-journal base-dir plan wave))
  (if (not journal)
      0
      (length (hash-ref journal 'receipt-history '()))))

(define (default-binding-branch plan wave #:base-dir [base-dir #f])
  (define generation
    (if base-dir
        (binding-generation base-dir plan wave)
        0))
  (define base
    (string-append "binding/" (substring plan 0 (min 12 (string-length plan))) (format "-w~a" wave)))
  (if (= generation 0)
      base
      (format "~a-r~a" base generation)))

(define (default-binding-staging-path base-dir plan wave)
  (define generation (binding-generation base-dir plan wave))
  (define suffix
    (if (= generation 0)
        ""
        (format "-r~a" generation)))
  (path->string
   (build-path base-dir ".planning" "campaigns" plan (format "binding-w~a~a" wave suffix))))

;; Binding CI/merge dispatch: resolve the deterministic binding PR by branch,
;; then act on the typed status. `run` is the controller seam (injectable for
;; tests; the production caller passes native-binding-stage-run, which serves
;; the binding actions from native Racket and refuses an unknown/legacy action
;; name as a typed blocked stop). Statuses are JSON
;; strings — string->jsexpr never produces symbols, so comparing against a
;; quoted symbol list never matches and would silently skip the CI/merge
;; action (fabricated progress). "already-merged" returns the resolve result
;; verbatim (idempotent resume); a resolved identity without a usable number
;; or head fails closed as a typed blocked stop.
(define (binding-dispatch-stage-action run target-stage plan wave action-name #:base-dir base-dir)
  (define binding-branch (default-binding-branch plan wave #:base-dir base-dir))
  ;; The run seam (native-binding-stage-run) appends --repo/--plan/--wave
  ;; itself; per-action flags must never duplicate them (argparse refuses
  ;; unknown flags like --plan-id and would turn every dispatch into a usage
  ;; error stop).
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
                  (default-delivery-evidence-path plan wave #:base-dir base-dir))))]
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
