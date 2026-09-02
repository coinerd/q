#lang racket/base

;; extensions/gsd/go-orchestrator.rkt — Single-Wave Campaign Coordinator
;;
;; v0.99.80 W2: GC-1, GC-6, GC-7, GC-9, GC-13
;;
;; D1 — Coordinator loop is OUTSIDE run-prompt!:
;;   acquire campaign lease
;;   while earliest actionable wave exists:
;;       persist IN-PROGRESS + fresh attempt/fence
;;       result := run-wave!(single-wave-prompt)
;;       wait for run-wave! return
;;       if cancelled/error/timeout: persist INTERRUPTED/FAILED; stop
;;       persist VERIFYING
;;       verification := verify current attempt evidence
;;       if rejected: persist FAILED; stop
;;       commit DONE + completion outbox record
;;       deliver completion event idempotently
;;   release campaign lease

(require racket/format
         racket/file
         racket/match
         racket/string
         racket/system
         (only-in "../../util/version.rkt" q-version)
         "campaign-state.rkt"
         "campaign-repository.rkt"
         "wave-completion.rkt"
         "wave-runner-port.rkt"
         (only-in "wave-docs.rkt" wave-slug plan-slug-map)
         (only-in "wave-status.rkt" STATUS-DONE STATUS-FAILED)
         "projection-effects.rkt"
         "../../util/loop-result.rkt"
         (only-in "system-adapters.rkt" run-wave-with-timeout)
         (only-in "plan-context-builder.rkt" current-git-root)
         (only-in "policy.rkt"
                  current-gsd-wave-timeout-seconds
                  current-gsd-wave-timeout-retries
                  current-gsd-wave-no-change-retries
                  current-gsd-wave-failure-context
                  current-gsd-max-consecutive-tool-calls
                  current-gsd-campaign-infra-retries
                  current-gsd-campaign-infra-retry-delay
                  current-gsd-release-check)
         (only-in "prompts.rkt"
                  wave-failure-context-block
                  wave-attempt-context-block
                  executor-reanchor-prompt)
         (only-in "events.rkt" emit-gsd-event!)
         ;; v1.00.22 W6 (BUG-0040): terminal-transition notification
         ;; surface (tmux/desktop/webhook sinks; best-effort, never
         ;; affects the campaign).
         (only-in "notify.rkt"
                  current-gsd-notify-sinks
                  gsd-notify-sinks-from-settings
                  make-gsd-notification
                  notify-terminal-transition!
                  notify-terminal-transition*!)
         (only-in "wave-executor.rkt"
                  stall-limit?
                  make-stall-watchdog
                  stall-watchdog-observe!
                  stall-watchdog-snapshot
                  ;; v1.00.17 W6 (#9512a): wave worktree isolation
                  worktree-isolation-enabled?
                  ;; v1.00.19 W2 (BUG-0028 S1): gsd.worktree-isolation
                  ;; settings wiring + start banner.
                  resolve-worktree-isolation
                  apply-worktree-isolation-setting!
                  worktree-isolation-banner
                  find-repo-root
                  wave-worktree-path
                  wave-worktree-branch
                  wave-worktree-repo-root
                  wave-worktree-base-ref
                  make-wave-worktree!
                  cleanup-wave-worktree!
                  release-wave-worktree!
                  worktree-hash8
                  default-run-git
                  git-result
                  git-result-code
                  git-result-stdout
                  git-result-stderr
                  reclaim-orphaned-worktrees!
                  ;; v1.00.20 W4 (BUG-0030): mid-wave checkpoint contract
                  checkpoint-contract-lines
                  commit-wave-checkpoint!
                  checkpoint-commit-message?
                  CHECKPOINT-COMMIT-PREFIX
                  wave-checkpoint-commit-message
                  ;; v1.00.21 W5 (BUG-0029): attempt-artifact ledger
                  inherited-artifacts-block
                  current-gsd-wave-inherited-artifacts)
         (only-in "wave-docs.rkt" wave-slug plan-slug-map read-wave-doc)
         (only-in "../../runtime/iteration/step-executor.rkt" current-post-tool-result-hook)
         (only-in "../../agent/state.rkt" current-empty-response-nudge)
         (only-in "delivery-verifier.rkt"
                  delivery-verification?
                  delivery-verification-approved?
                  delivery-verification-message
                  ;; v1.00.17 W7 (#9512b): branch-based delivery
                  current-gsd-delivery-branch-context
                  make-branch-delivery-context
                  branch-delivery-context-ref)
         (only-in "../../util/iteration/decision.rkt" current-max-consecutive-tool-calls)
         (only-in "../../runtime/provider-retry.rkt"
                  current-provider-retry-max-retries
                  current-provider-retry-stall-max-consecutive
                  current-provider-retry-ceiling-secs)
         racket/os
         ;; v1.00.22 W7 (BUG-0042): extracted pure seams. This module keeps
         ;; orchestration + persistence calls only; stall messages and
         ;; classification live in stall-policy.rkt, infra-retry budget /
         ;; backoff / prior-attempt-context joining in infra-retry-policy.rkt,
         ;; and the /go freshness guard + build identity in freshness.rkt.
         ;; All previously-public names are re-provided below (compat shim).
         "stall-policy.rkt"
         "infra-retry-policy.rkt"
         "freshness.rkt"
         ;; v1.00.22 W7 (BUG-0042) continued: budget accounting + settings
         ;; load in campaign-budgets.rkt; branch-delivery bookkeeping +
         ;; attempt-artifact ledger + leftovers report in
         ;; attempt-artifacts.rkt. Same compat-shim re-provide below.
         "campaign-budgets.rkt"
         "attempt-artifacts.rkt")

;; ============================================================
;; Lease (D5: process-safe OS advisory lock)
;; ============================================================

(struct campaign-lease (path port owner-pid owner-session) #:mutable)

(define (lease-path base-dir plan-id)
  (build-path base-dir ".planning" "campaigns" (string-append plan-id ".lock")))

(define (acquire-lease base-dir plan-id #:session-id [session-id "unknown"])
  (define p (lease-path base-dir plan-id))
  (define-values (dir _ __) (split-path p))
  (make-directory* dir)
  (with-handlers ([exn:fail:filesystem? (lambda (_) #f)])
    ;; The lock file may survive a crash, but the OS advisory lock cannot.
    ;; Reopening the stable path therefore recovers safely after process exit.
    (define port (open-output-file p #:exists 'can-update))
    (if (port-try-file-lock? port 'exclusive)
        (begin
          ;; S2a (#9358): the file may hold a LONGER lease from a previous
          ;; owner (e.g. "01M0645J64E772Q0ZFNVGGEKK0"). open-output-file
          ;; 'can-update does NOT truncate, so file-position 0 + write left
          ;; a stale tail after a shorter write — corrupting the lease
          ;; (observed: `…VGGEKK0") (pid …)`). Truncate to zero before write.
          ;; D4 (#9351): record the owning session id AND pid so a stale
          ;; lock file names its holder (incident 81f9be4b: "unknown").
          ;; S2a (#9358): never write an empty owner — a re-dispatch path
          ;; passed "" (observed in attempt-5), defeating D4 diagnostics.
          (let ([owner
                 (if (and (string? session-id) (not (string=? session-id ""))) session-id "unknown")])
            (file-truncate port 0)
            (file-position port 0)
            (write (hasheq 'owner owner 'pid (getpid) 'acquired (current-seconds)) port)
            (flush-output port)
            (campaign-lease p port (current-seconds) owner)))
        (begin
          (close-output-port port)
          #f))))

(define (release-lease! lease)
  (when (and lease (campaign-lease? lease))
    (with-handlers ([exn:fail? void])
      (port-file-unlock (campaign-lease-port lease))
      (close-output-port (campaign-lease-port lease)))))

;; ============================================================
;; Coordinator result
;; ============================================================

(struct campaign-result (status completed-waves message) #:transparent)

;; ============================================================
;; Wave runner abstraction (injectable for testing)
;; ============================================================

;; Missing execution or verification authority must never invent DONE.
(define default-runner
  (lambda (wave-idx) (wave-execution-outcome 'failed "no wave runner configured")))
(define default-verifier (lambda (wave-idx) #f))

;; A caller that owns a wave-specific cancellation handle may bind it here.
;; The default is deliberately a no-op: a campaign must never terminate the
;; process-global gateway worker, which may be serving unrelated sessions.
(define current-gsd-wave-cancel! (make-parameter void))

;; Normalize a runner value to a gsd-wave-runner-port. Legacy plain functions
;; returning symbols ('ok/'error/'cancelled) are wrapped and coerced at the
;; boundary so the coordinator switch only ever sees structured outcomes.
(define (coerce-runner runner)
  (cond
    [(gsd-wave-runner-port? runner) runner]
    [else (make-wave-runner-port (lambda (idx) (coerce-run-result (runner idx))))]))

;; A campaign request is the interface-safe execution boundary for /go.  It
;; carries durable campaign identity plus callbacks that build one wave prompt
;; and verify one completed attempt; interfaces supply only the prompt runner.
;; timeout-sec: per-campaign override for the wave budget (current-gsd-wave-
;; timeout-seconds), resolved at /go time from the --wave-timeout=SECONDS flag,
;; then ~/.q/config.json wave-timeout-seconds, then the parameter default.
;; #f → use the current parameter value at execution time. Carried on the
;; request (not the parameter) because the campaign runs in a separate thread.
(struct campaign-request (base-dir record prompt-for-wave verifier timeout-sec allow-stale?)
  #:transparent
  #:constructor-name make-campaign-request/6)

(define (make-campaign-request base-dir
                               record
                               prompt-for-wave
                               verifier
                               #:timeout-sec [timeout-sec #f]
                               #:allow-stale? [allow-stale? #f])
  (make-campaign-request/6 base-dir record prompt-for-wave verifier timeout-sec allow-stale?))

(define (execute-campaign-request! request
                                   run-prompt
                                   #:lease-owner [lease-owner "unknown"]
                                   #:allow-stale? [allow-stale-arg #f])
  (define base-dir (campaign-request-base-dir request))
  (define record (campaign-request-record request))
  (define plan-id (campaign-plan-id record))
  ;; v1.00.19 W3 (BUG-0031): version-freshness guard at /go entry. A stale
  ;; build is refused loudly ("restart required (running X, checkout Y)")
  ;; unless the operator passed allow-stale through the command parser;
  ;; the override is recorded in the campaign record. Offline mode warns
  ;; but never blocks.
  (define allow-stale? (or allow-stale-arg (campaign-request-allow-stale? request)))
  ;; v1.00.22 W5 (BUG-0039): durable budget-pause resume gate at /go
  ;; entry. A campaign paused by a crossed ceiling stays refused with
  ;; the named reason while the CURRENT ceilings still cross; a raised
  ;; (or removed) ceiling clears the pause durably and execution
  ;; continues with the refreshed record — nothing dropped, nothing
  ;; re-counted (totals live in the durable record).
  (define-values (budget-proceed? budget-refreshed-record budget-refusal)
    (resume-after-budget-pause! base-dir plan-id record))
  (cond
    [(not budget-proceed?) (campaign-result 'error '() budget-refusal)]
    [budget-refreshed-record (set! record budget-refreshed-record)]
    [else (void)])
  (define freshness ((current-gsd-freshness-check) base-dir))
  (cond
    [(and (freshness-stale? freshness) (not allow-stale?))
     (campaign-result 'error '() (freshness-refusal-message freshness))]
    [else
     (when (freshness-offline-warning freshness)
       (log-warning "~a" (freshness-offline-warning freshness)))
     (when (and allow-stale? (freshness-stale? freshness))
       (set-campaign-record-stale-override! record #t)
       (log-warning
        "gsd freshness: stale override accepted — recording stale-override: true (running ~a, checkout ~a)"
        (freshness-running-version freshness)
        (or (freshness-checkout-version freshness) "?")))
     ;; Every campaign record identifies the exact build that produced it.
     (stamp-campaign-build-identity! record base-dir)
     (execute-stamped-campaign-request! request
                                        run-prompt
                                        record
                                        freshness
                                        #:lease-owner lease-owner)]))

(define (execute-stamped-campaign-request! request
                                           run-prompt
                                           record
                                           freshness
                                           #:lease-owner [lease-owner "unknown"])
  (define base-dir (campaign-request-base-dir request))
  (define plan-id (campaign-plan-id record))
  ;; v1.00.03: per-campaign wave budget. Resolved at /go time (flag > config
  ;; > parameter) and carried on the request because the campaign runs in a
  ;; separate thread where a parameterize at /go time would not apply.
  (define effective-wave-timeout-secs
    (or (campaign-request-timeout-sec request) (current-gsd-wave-timeout-seconds)))
  ;; Pending-tool cancellation surface: the executor port's cancel-requested?
  ;; reflects the durable campaign cancellation flag so a long-running tool
  ;; loop can abort mid-wave instead of completing after /cancel.
  (define (durable-cancellation-requested?)
    (define observed (load-campaign-record base-dir plan-id))
    (and observed (campaign-record-cancellation observed)))
  ;; D8 (#9357): campaign-aware provider retry. A single transient SSE read
  ;; timeout (120 s) must not burn an implementation wave that may have made
  ;; 30+ tool-call minutes of progress. Scale the interactive retry knobs to
  ;; wave budget: more retries, more consecutive-stall tolerance, and a
  ;; cumulative ceiling proportional to the wave timeout (capped at 900 s).
  (parameterize ([current-max-consecutive-tool-calls (current-gsd-max-consecutive-tool-calls)]
                 [current-provider-retry-max-retries 5]
                 [current-provider-retry-stall-max-consecutive 4]
                 [current-provider-retry-ceiling-secs
                  (let ([budget effective-wave-timeout-secs])
                    (min 900 (max 60 (quotient (inexact->exact (floor budget)) 2))))]
                 ;; v1.00.22 W5 (BUG-0039): in-process usage observation
                 ;; transport. The runner lambda below (same dynamic extent)
                 ;; stores the raw loop-result's 'usage datum in this box
                 ;; BEFORE prompt-run-result->outcome strips it; the parent
                 ;; retry-loop drains it after each attempt and stamps the
                 ;; durable record.
                 [current-campaign-usage-observation (box #f)])
    (run-campaign!
     base-dir
     record
     #:lease-owner lease-owner
     #:runner (make-wave-runner-port
               (lambda (wave-idx)
                 ;; BUG-0037 W1: a stall kill is retryable infrastructure —
                 ;; distinct clause BEFORE the generic exn:fail? fallback.
                 (with-handlers ([gsd-stall-exn?
                                  (lambda (e)
                                    (log-error "campaign runner stall-killed: ~a" (exn-message e))
                                    (wave-execution-outcome 'infra-failed (exn-message e)))]
                                 [exn:fail? (lambda (e)
                                              (log-error "campaign runner failed: ~a" (exn-message e))
                                              (wave-execution-outcome 'failed (exn-message e)))])
                   (define returned-values
                     (call-with-values
                      (lambda () (run-prompt ((campaign-request-prompt-for-wave request) wave-idx)))
                      list))
                   ;; Runtime/session runners return either a single
                   ;; loop-result or (values updated-session result).
                   (define run-result
                     (if (= (length returned-values) 2)
                         (cadr returned-values)
                         (and (pair? returned-values) (car returned-values))))
                   ;; v1.00.22 W5 (BUG-0039): observe the raw loop-result's
                   ;; usage BEFORE prompt-run-result->outcome strips it.
                   (record-usage-observation! wave-idx run-result)
                   (prompt-run-result->outcome run-result)))
               #:cancel! (current-gsd-wave-cancel!)
               #:cancel-requested? durable-cancellation-requested?)
     #:verifier (campaign-request-verifier request)
     #:timeout-sec effective-wave-timeout-secs)))

;; Hook payloads cross a Typed Racket Any boundary that intentionally rejects
;; higher-order values. Keep callbacks process-local and send only an opaque
;; token through TUI/GUI/SDK hook payloads.
(define campaign-request-registry (make-hash))
(define campaign-request-registry-lock (make-semaphore 1))

(define (register-campaign-request! request)
  (define token
    (format "~a-~a-~a"
            (campaign-plan-id (campaign-request-record request))
            (current-inexact-milliseconds)
            (random 1000000000)))
  (call-with-semaphore campaign-request-registry-lock
                       (lambda () (hash-set! campaign-request-registry token request)))
  token)

(define (lookup-campaign-request token)
  (call-with-semaphore campaign-request-registry-lock
                       (lambda () (hash-ref campaign-request-registry token #f))))

(define (execute-campaign-token! token run-prompt #:lease-owner [lease-owner "unknown"])
  (define request (lookup-campaign-request token))
  (if request
      (dynamic-wind
       void
       (lambda () (execute-campaign-request! request run-prompt #:lease-owner lease-owner))
       (lambda ()
         (call-with-semaphore campaign-request-registry-lock
                              (lambda () (hash-remove! campaign-request-registry token)))))
      (campaign-result 'error '() "campaign request token is missing or expired")))

;; ============================================================
;; Mutation-stall watchdog injection (v1.00.18 W5 — #9513)
;;
;; v1.00.16 W3 attempt-2: 92 read-only tool calls, zero edits, ~40 min and
;; ~$12 burned before delivery verification noticed. The busy-watchdog
;; fires on wall-clock, not usefulness. The accounting lives in
;; wave-executor.rkt (pure); THIS module owns the mid-session steering
;; injection (soft limit) and the honest attempt termination (hard limit).
;; ============================================================

;; BUG-0043 (W2): a terminal wave-execution-outcome with kind != 'done must
;; surface as a typed [SYS] [ERROR] transcript event — NOT as conversation/
;; message-surface text. The TUI reducer (core-handlers.rkt) renders the
;; payload's kind + message verbatim on the error surface. Best-effort: a bus
;; failure must never break the campaign control flow.
(define (emit-wave-outcome-error! wave-idx kind message)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning "gsd: outcome-error event emission failed: ~a"
                                            (exn-message e)))])
    (emit-gsd-event! 'gsd.wave.outcome-error
                     (hasheq 'wave wave-idx 'kind kind 'level "error" 'message (or message "")))))

(define (runner-outcome-failure-reason outcome result)
  (define msg (wave-execution-outcome-message result))
  (if (and (string? msg) (positive? (string-length (string-trim msg))))
      msg
      (format "wave execution ended '~a' without a runner message" outcome)))

;; ============================================================
;; Build identity & version-freshness guard (v1.00.19 W3 — BUG-0031)
;;
;; A long-running TUI executes modules loaded at process start. After a
;; release lands, /go campaigns silently run on OLD code until the operator
;; remembers to restart. Detection is exact and network-free: the running
;; process holds the OLD (q-version) value in memory while the checkout's
;; util/version.rkt on disk already has the new one — compare the two.
;; origin/main is a secondary, best-effort signal; when it cannot be
;; resolved (offline) the guard proceeds with a warning instead of
;; blocking. Every campaign record also gains build identity fields
;; (build-version / main-head-sha) so evidence is attributable to the
;; exact build that produced it.
;; ============================================================

;; ============================================================
;; Single-wave campaign coordinator (D1)
;; ============================================================

(define (current-wave-for-attempt rec wave-idx fence attempt-id)
  (define wave (and rec (find-wave rec wave-idx)))
  (define attempt (and wave (campaign-wave-current-attempt wave)))
  (and rec
       wave
       attempt
       (= (campaign-fence-token rec) fence)
       (= (campaign-attempt-fence-token attempt) fence)
       (equal? (campaign-attempt-id attempt) attempt-id)
       wave))

(define (run-campaign-wave base-dir
                           rec
                           wave-idx
                           #:runner [runner default-runner]
                           #:verifier [verifier default-verifier]
                           #:meta-fix-predicate [meta-fix-predicate (lambda (_) #f)]
                           #:fence-token [requested-fence #f]
                           #:timeout-sec [timeout-sec #f]
                           #:timeout-retries [timeout-retries (current-gsd-wave-timeout-retries)]
                           #:no-change-retries
                           [no-change-retries (current-gsd-wave-no-change-retries)]
                           ;; v1.00.18 W5 (#9513): mutation-stall watchdog.
                           ;; v1.00.21 W1 (BUG-0044): keywords are now
                           ;; overrides on top of the gsd.stall.* settings
                           ;; keys; 'unset = not given (defer to settings,
                           ;; then to the 8/15/30/300 defaults). #f disables
                           ;; a limit.
                           #:stall-soft-limit [stall-soft-limit 'unset]
                           #:stall-hard-limit [stall-hard-limit 'unset]
                           #:stall-window [stall-window 'unset]
                           #:stall-backstop [stall-backstop 'unset]
                           ;; v1.00.17 W6 (#9512a): wave worktree isolation
                           ;; (gsd.worktree-isolation; #t forces isolation,
                           ;; #f forces it off — overrides the flag for tests).
                           ;; v1.00.19 W2 (BUG-0028 S1): 'auto = honor the
                           ;; gsd.worktree-isolation project-settings key.
                           ;; Explicit #t/#f overrides the key; key absent
                           ;; falls back to current-gsd-worktree-isolation
                           ;; (default OFF). Precedence documented at
                           ;; resolve-worktree-isolation (wave-executor.rkt).
                           #:isolate? [isolate-arg 'auto])
  ;; BUG-0028 S1 (v1.00.19 W2): composition-root settings wiring — the
  ;; gsd.worktree-isolation key drives isolation from here on.
  ;; isolate-arg is the explicit caller choice; 'auto defers to settings.
  (define isolate?
    (apply-worktree-isolation-setting! (load-project-settings-silently base-dir)
                                       #:isolate? isolate-arg))
  ;; v1.00.21 W1 (BUG-0044): stall-threshold composition root. The
  ;; settings layer is the source when a gsd.stall.* key is present;
  ;; keyword overrides win; absent both → the 8/15/30/300 defaults.
  ;; v1.00.22 W7 (BUG-0042): the composition math lives in stall-policy
  ;; (resolve-effective-stall-thresholds); each accessor there already
  ;; warns-and-defaults on invalid values, so a typo'd settings file
  ;; can never crash a campaign mid-wave.
  (define-values (effective-stall-soft-limit
                  effective-stall-hard-limit
                  effective-stall-window
                  effective-stall-backstop)
    (resolve-effective-stall-thresholds (load-project-settings-silently base-dir)
                                        #:soft stall-soft-limit
                                        #:hard stall-hard-limit
                                        #:window stall-window
                                        #:backstop stall-backstop))
  ;; Startup visibility (BUG-0044 action 3): operators see the EFFECTIVE
  ;; thresholds in the wave log without reading source.
  (log-info "wave ~a: effective stall thresholds soft=~a hard=~a window=~a backstop=~a"
            (campaign-plan-id rec)
            effective-stall-soft-limit
            effective-stall-hard-limit
            effective-stall-window
            effective-stall-backstop)
  ;; Reload before beginning so an old request token cannot overwrite a newer
  ;; completion, cancellation, or fence after waiting for the process lock.
  (define active (or (load-campaign-record base-dir (campaign-plan-id rec)) rec))
  (define fence (or requested-fence (add1 (campaign-fence-token active))))
  (define initial-wave (find-wave active wave-idx))
  (cond
    [(campaign-record-cancellation active)
     (campaign-result 'wave-cancelled '() "campaign cancellation requested")]
    [(or (not initial-wave)
         (memq (campaign-wave-status initial-wave) '(done deferred))
         (<= fence (campaign-fence-token active)))
     (campaign-result 'wave-cancelled '() "stale campaign request ignored")]
    [else
     ;; #9515 (v1.00.17 W3): run-once is re-entered at most
     ;; #:no-change-retries times when the delivery verifier rejects with
     ;; "no wave target files changed". Each re-entry re-runs the
     ;; begin-attempt!/fence protocol (fresh attempt id, same fence),
     ;; mirroring the timeout-retry shape, with a failure-context block
     ;; parameterized so the executor prompt carries the verbatim verifier
     ;; verdict. Bounded + at-least-once: a crash mid-retry leaves the
     ;; durable wave status 'pending (re-attemptable), never lost.
     ;; BUG-0024 W3: campaign-level infra-retry state, closed over by run-once*
     ;; across recursive re-attempts. car = automatic retries left (initial
     ;; current-gsd-campaign-infra-retries, default 3); cdr = accumulated failure
     ;; descriptors ((attempt unix-timestamp) ...) for the aggregated stop
     ;; message once the bound is exhausted.
     (define infra-retry-state (box (cons (current-gsd-campaign-infra-retries) '())))

     (define (run-once* no-change-retries-left wt-box keep-branch-box attempt-id-box)
       (set-campaign-fence-token! active fence)
       (begin-attempt! active wave-idx fence)
       (persist-campaign! base-dir active)
       (define started-attempt (campaign-wave-current-attempt (find-wave active wave-idx)))
       (define expected-id (campaign-attempt-id started-attempt))
       (set-box! attempt-id-box expected-id)
       ;; v1.00.21 W5 (BUG-0029 action 2): the PRIOR ARTIFACTS block is
       ;; built from the ledger BEFORE this attempt adds its own entry —
       ;; exactly the prior attempts' branches/worktrees, with terminal
       ;; and merge status, distilled to ~1 KB by inherited-artifacts-block
       ;; (same bounded-block shape as the BUG-0024 context).
       (define inherited-artifact-text
         (inherited-artifacts-block (wave-artifact-ledger (find-wave active wave-idx))))
       ;; BUG-0030 (action 4): before the attempt starts, warn LOUDLY about
       ;; uncommitted .rkt changes in the main checkout OUTSIDE the active
       ;; attempt lease/worktree — exactly what polluted PR #9529's metrics
       ;; twice ("expected 158320, found 158324"). Pure warning: never
       ;; auto-commits, never auto-discards, never blocks. Only meaningful
       ;; when the attempt runs in an isolated worktree (in shared-checkout
       ;; mode the attempt's own edits legitimately live there and are
       ;; covered by the infra-stop dirty capture instead).
       (when (worktree-isolation-enabled? #:isolate? isolate?)
         (warn-outside-lease-dirty-state! (find-repo-root base-dir)))
       (define (observe)
         (load-campaign-record base-dir (campaign-plan-id active)))
       (define (mirror-status! status #:failure-reason [failure-reason #f])
         (define caller-wave (find-wave rec wave-idx))
         (when caller-wave
           (when failure-reason
             (stamp-wave-failure! caller-wave failure-reason))
           (set-campaign-wave-status! caller-wave status)))
       (define (persist-current-status! status #:failure-reason [failure-reason #f])
         (define observed (observe))
         (define observed-wave (current-wave-for-attempt observed wave-idx fence expected-id))
         (and observed-wave
              (begin
                (when failure-reason
                  (stamp-wave-failure! observed-wave failure-reason))
                (set-campaign-wave-status! observed-wave status)
                (persist-campaign! base-dir observed)
                (mirror-status! status #:failure-reason failure-reason)
                observed)))
       (define (interrupt-current! message #:failure-reason [failure-reason #f])
         (persist-current-status! 'interrupted #:failure-reason failure-reason)
         (campaign-result 'wave-cancelled '() message))
       ;; Executor port boundary (W3 #9234): ONE structured terminal outcome per
       ;; invocation. Legacy symbol runners coerce; an optional deadline wraps
       ;; the port with run-wave-with-timeout so a hung tool yields
       ;; 'timed-out (persisted as interrupted) instead of blocking forever.
       (define runner-port (coerce-runner runner))
       (define run-one
         (if timeout-sec
             (lambda (idx) (run-wave-with-timeout runner-port timeout-sec idx))
             (gsd-wave-runner-port-run runner-port)))
       ;; v1.00.18 W5 (#9513): wrap run-one with the mutation-stall
       ;; watchdog. Both limits #f → inert (no wrapper at all). The
       ;; watchdog counts executor tool calls since the last file
       ;; mutation via the post-tool-result hook (thread-inherited by
       ;; the timeout worker); soft limit injects W2's re-anchor steering
       ;; ("begin the first edit now") once per session, hard limit
       ;; terminates the attempt 'failed with an explicit stall cause.
       (define run-one/watchdog
         (if (and (not effective-stall-soft-limit) (not effective-stall-hard-limit))
             run-one
             (wrap-run-one-with-stall-watchdog
              run-one
              (make-stall-watchdog #:soft-limit effective-stall-soft-limit
                                   #:hard-limit effective-stall-hard-limit
                                   #:window effective-stall-window
                                   #:backstop effective-stall-backstop)
              base-dir
              active
              wave-idx
              effective-stall-soft-limit
              effective-stall-hard-limit)))
       (define prior-failure-reason
         (let ([w (find-wave rec wave-idx)])
           (and w
                (let ([reason (wave-failure-reason w)])
                  (and (positive? (string-length (string-trim reason))) reason)))))
       (define durable-failure-context
         (and
          prior-failure-reason
          (string-append
           "\n\n=== PREVIOUS ATTEMPT TERMINAL REASON — ADAPT BEFORE RETRY ===\n"
           prior-failure-reason
           "\n\nDo not repeat the failed approach. Use this durable reason to choose the next concrete action.")))
       ;; BUG-0017 follow-up: retry a wave whose run exceeds the per-wave budget
       ;; (timed-out) with a FRESH session (each run-one invocation re-enters the
       ;; runner port, which the TUI/GUI factory maps to a new session). The
       ;; attempt is NOT consumed by retries — only final exhaustion persists
       ;; interrupted (at-least-once). Mirrors the LLM provider-retry ceiling
       ;; (current-provider-retry-max-retries = 5).
       (define (run-with-timeout-retry retries-left)
         ;; v1.00.21 W5 (BUG-0029 action 2): the PRIOR ARTIFACTS block rides
         ;; the same prompt plumbing as current-gsd-wave-failure-context —
         ;; the prompt builder runs inside this parameterize extent, so
         ;; successor executors see prior attempts' artifacts without
         ;; rediscovering them.
         (parameterize ([current-gsd-wave-inherited-artifacts inherited-artifact-text]
                        [current-gsd-wave-failure-context (or (current-gsd-wave-failure-context)
                                                              durable-failure-context)])
           (let retry-loop ([retries-left retries-left])
             (define result (coerce-run-result (run-one/watchdog wave-idx)))
             ;; v1.00.22 W5 (BUG-0039): ATTEMPT BOUNDARY — drain the
             ;; in-process usage observation (runner lambda stored it
             ;; before outcome conversion) and stamp it durably; nothing
             ;; observed stamps 'usage-missing, never zeros.
             (stamp-observed-usage! base-dir
                                    (campaign-plan-id active)
                                    wave-idx
                                    (take-usage-observation!))
             ;; Ceiling check (never mid-tool-call): crossing persists a
             ;; durable pause with the named reason; the loop's pause
             ;; clause below stops dispatch.
             (pause-campaign-if-over-budget! base-dir (campaign-plan-id active))
             (if (and (eq? (wave-execution-outcome-kind result) 'timed-out) (> retries-left 0))
                 (begin
                   (log-info "wave ~a timed out; retrying (~a retries left)"
                             wave-idx
                             (sub1 retries-left))
                   (retry-loop (sub1 retries-left)))
                 result))))
       ;; v1.00.17 W6 (#9512a): wave worktree isolation. When enabled the
       ;; executor session runs inside its own git worktree + campaign
       ;; branch: current-directory is parameterized to the worktree path
       ;; so sessions spawned by the host runner factory inherit it as
       ;; their cwd, while base-dir — and therefore .planning/ — stays the
       ;; REAL project root (canonical campaign state is shared, never
       ;; per-worktree; executor prompts already receive base-dir
       ;; separately). Worktree creation failure falls back to the shared
       ;; checkout with a logged warning (campaign liveness over isolation
       ;; purity). W7 (#9512b): the worktree is NO LONGER cleaned up here —
       ;; its lifecycle now spans verification and the completion decision
       ;; (branch = delivery evidence); run-once releases it on exit.
       ;; Design record:
       ;; q/docs/reports/GSD-WORKTREE-ISOLATION-v1.00.17.md
       (define (run-isolated run-thunk)
         (if (not (worktree-isolation-enabled? #:isolate? isolate?))
             (run-thunk)
             (let ([wt
                    (with-handlers
                        ([exn:fail?
                          (lambda (e)
                            (log-warning
                             "wave ~a: worktree isolation unavailable (~a); running in shared checkout"
                             wave-idx
                             (exn-message e))
                            #f)])
                      (make-wave-worktree! base-dir
                                           #:campaign-id (campaign-plan-id active)
                                           #:wave-index wave-idx))])
               (cond
                 [(not wt) (run-thunk)]
                 [else
                  (set-box! wt-box wt)
                  ;; v1.00.21 W5 (BUG-0029 action 1): the worktree+branch
                  ;; pair is a DURABLE artifact — it enters the wave's
                  ;; artifact ledger AT CREATION, owned by the record from
                  ;; its first breath until terminal update / teardown /
                  ;; reclaim.
                  (record-attempt-artifact! base-dir
                                            (campaign-plan-id active)
                                            wave-idx
                                            expected-id
                                            wt)
                  (parameterize ([current-directory (wave-worktree-path wt)])
                    (run-thunk))]))))
       ;; W7 (#9512b): the worktree outlives the run thunk — its branch
       ;; carries the delivery evidence through verification and the
       ;; completion decision. run-once* therefore receives the boxes
       ;; from run-campaign-wave, which owns the release dynamic-wind
       ;; (below): the worktree directory is removed on EVERY exit path;
       ;; the branch is kept only when delivery was approved
       ;; (release-wave-worktree!), deleted otherwise
       ;; (cleanup-wave-worktree!). The previous attempt released here —
       ;; before verification — which made wt always #f at the delivery
       ;; check and silently disabled branch-based verification.
       (define run-result (run-isolated (lambda () (run-with-timeout-retry timeout-retries))))
       (define outcome (wave-execution-outcome-kind run-result))
       (define after-run (observe))
       (cond
         [(and after-run (campaign-record-budget-pause after-run))
          ;; v1.00.22 W5 (BUG-0039): a budget ceiling tripped at the last
          ;; attempt boundary (persisted with its named reason). Stop the
          ;; loop; the pause is durable and resumable — raising the
          ;; ceiling and re-running /go clears it at the entry gate.
          (notify-terminal-transition*!
           (campaign-plan-id active)
           wave-idx
           'budget-pause
           #:reason (campaign-budget-pause-message (campaign-record-budget-pause after-run))
           #:spend (let ([pause (campaign-record-budget-pause after-run)])
                     (define observed (and pause (campaign-budget-pause-observed pause)))
                     (and (pair? observed) (number? (car observed)) (car observed))))
          (interrupt-current! (campaign-budget-pause-message
                               (campaign-record-budget-pause after-run)))]
         [(and after-run (campaign-record-cancellation after-run))
          (mark-attempt-artifact-terminal! base-dir
                                           (campaign-plan-id active)
                                           wave-idx
                                           expected-id
                                           'cancelled)
          (notify-terminal-transition*! (campaign-plan-id active)
                                        wave-idx
                                        'campaign-cancelled
                                        #:reason "campaign cancellation requested")
          (interrupt-current! "campaign cancellation requested")]
         [(not (current-wave-for-attempt after-run wave-idx fence expected-id))
          (campaign-result 'wave-cancelled '() "stale runner result ignored")]
         [else
          (case outcome
            [(done)
             (cond
               [(meta-fix-predicate run-result)
                ;; Meta-fix: reset wave status to pending, don't consume attempt
                (log-info "meta-fix detected for wave ~a -- resetting to pending" wave-idx)
                (define meta-wave (current-wave-for-attempt after-run wave-idx fence expected-id))
                (when meta-wave
                  (set-campaign-wave-status! meta-wave 'pending)
                  (persist-campaign! base-dir after-run)
                  (mirror-status! 'pending))
                (mark-attempt-artifact-terminal! base-dir
                                                 (campaign-plan-id active)
                                                 wave-idx
                                                 expected-id
                                                 'cancelled)
                (campaign-result 'meta-fix (list wave-idx) "meta-fix wave reset")]
               [else
                (define verifying (persist-current-status! 'verifying))
                (if (not verifying)
                    (campaign-result 'wave-cancelled '() "stale runner result ignored")
                    ;; v1.00.17 W7 (#9512b): branch-based delivery. In the
                    ;; isolated path, changes are only DELIVERED when they
                    ;; are COMMITTED to the wave branch. Auto-commit any
                    ;; uncommitted worktree changes with the deterministic
                    ;; campaign message, then verify against the COMMITTED
                    ;; branch diff (base = origin/main at attempt start):
                    ;; worktree dirt can never fake delivery, and an empty
                    ;; diff means the honest no-change verdict below.
                    (let* ([wt (unbox wt-box)]
                           [_ (when wt
                                (let ([w (find-wave active wave-idx)])
                                  (commit-wave-worktree! wt
                                                         (campaign-plan-id active)
                                                         wave-idx
                                                         (if w
                                                             (campaign-wave-title w)
                                                             (format "wave-~a" wave-idx)))))]
                           [delivery-ctx (and wt (wave-worktree-delivery-context wt))]
                           [_w
                            (when (and wt (not delivery-ctx))
                              (log-warning
                               "wave ~a: could not resolve branch delivery context; falling back to legacy working-tree check"
                               wave-idx))]
                           [verifier-result
                            (with-handlers ([exn:fail? (lambda (_) #f)])
                              (if delivery-ctx
                                  (parameterize ([current-gsd-delivery-branch-context delivery-ctx])
                                    (verifier wave-idx))
                                  (verifier wave-idx)))]
                           [approved? (cond
                                        [(delivery-verification? verifier-result)
                                         (delivery-verification-approved? verifier-result)]
                                        [else (and verifier-result #t)])]
                           [verifier-message (if (delivery-verification? verifier-result)
                                                 (delivery-verification-message verifier-result)
                                                 "")]
                           [after-verifier (observe)])
                      (cond
                        [(and after-verifier (campaign-record-cancellation after-verifier))
                         (interrupt-current! "campaign cancelled during verification")]
                        [(not (current-wave-for-attempt after-verifier wave-idx fence expected-id))
                         (campaign-result 'wave-cancelled '() "stale verifier result ignored")]
                        [else
                         (define result
                           (try-complete-wave!
                            base-dir
                            after-verifier
                            wave-idx
                            #:verifier-approve? approved?
                            #:verifier-message verifier-message
                            #:expected-attempt-id expected-id
                            #:expected-fence-token fence
                            ;; BUG-0051 (W6): release waves are
                            ;; gated on the configured external
                            ;; release check (policy parameter),
                            ;; returning #f for non-release waves.
                            #:release-check (and (current-gsd-release-check)
                                                 (lambda () ((current-gsd-release-check) wave-idx)))))
                         (define completion-status (completion-result-status result))
                         (when (memq completion-status '(done failed))
                           (mirror-status! completion-status))
                         (case completion-status
                           [(done)
                            ;; W7: record delivery provenance (branch + head
                            ;; SHA) in the durable campaign record; the
                            ;; merge/PR itself stays with the operator or
                            ;; wave-finish flow (no silent auto-merge in
                            ;; v1.00.17).
                            (when delivery-ctx
                              (record-wave-delivery! base-dir
                                                     (campaign-plan-id active)
                                                     wave-idx
                                                     (branch-delivery-context-ref delivery-ctx
                                                                                  'branch)
                                                     (wave-worktree-head-sha wt))
                              ;; BUG-0030 (action 2): verification is
                              ;; files/targets-based, NEVER commit-count
                              ;; based — a delivered branch may legitimately
                              ;; carry N mid-wave checkpoints plus the final
                              ;; state. A DONE branch with ZERO commits is
                              ;; nonsensical, so WARN (never fail).
                              (warn-zero-commit-delivery-branch! delivery-ctx))
                            ;; Delivery approved: the release wrapper must
                            ;; KEEP the branch (it is the merge evidence).
                            (when wt
                              (set-box! keep-branch-box #t))
                            ;; BUG-0024 W3: success clears the durable
                            ;; prior-attempt context so the next wave starts
                            ;; from zero context.
                            (let ([done-rec (observe)])
                              (define done-wave
                                (and done-rec
                                     (for/first ([w (campaign-record-waves done-rec)]
                                                 #:when (= (campaign-wave-index w) wave-idx))
                                       w)))
                              (when (and done-wave
                                         (positive? (string-length (campaign-wave-attempt-context
                                                                    done-wave))))
                                (set-campaign-wave-attempt-context! done-wave "")
                                (persist-campaign! base-dir done-rec)))
                            ;; v1.00.21 W5 (BUG-0029 action 1): terminal
                            ;; 'success + locally-determinable merge status
                            ;; for the delivered branch — the ledger owns it
                            ;; until merged/reclaimed.
                            (mark-attempt-artifact-terminal!
                             base-dir
                             (campaign-plan-id active)
                             wave-idx
                             expected-id
                             'success
                             #:merge-status
                             (and delivery-ctx
                                  (artifact-merge-status/local
                                   (branch-delivery-context-ref delivery-ctx 'repo-root)
                                   (branch-delivery-context-ref delivery-ctx 'branch))))
                            (notify-terminal-transition*! (campaign-plan-id active)
                                                          wave-idx
                                                          'wave-done
                                                          #:reason "wave completed")
                            (campaign-result 'wave-done (list wave-idx) "wave completed")]
                           [(failed)
                            (cond
                              [(and (> no-change-retries-left 0)
                                    (no-change-rejection? verifier-message))
                               ;; #9515: exactly-once-bounded failure-context
                               ;; retry. Reset the durable status to pending
                               ;; so the re-run begins cleanly and a crash
                               ;; leaves the wave re-attemptable
                               ;; (at-least-once). W7: release the rejected
                               ;; attempt's worktree AND branch before the
                               ;; retry — the re-run builds a fresh branch
                               ;; off origin/main (the rejected attempt's
                               ;; diff was empty by definition of the
                               ;; verdict).
                               (log-info
                                "wave ~a made zero target edits; retrying with failure context (~a retries left)"
                                wave-idx
                                (sub1 no-change-retries-left))
                               (when (and wt (not (unbox keep-branch-box)))
                                 (cleanup-wave-worktree! wt))
                               ;; v1.00.21 W5 (BUG-0029): the rejected
                               ;; attempt is terminal; its branch was just
                               ;; deleted by cleanup-wave-worktree!.
                               (mark-attempt-artifact-terminal! base-dir
                                                                (campaign-plan-id active)
                                                                wave-idx
                                                                expected-id
                                                                'failure
                                                                #:merge-status 'deleted)
                               (set-box! wt-box #f)
                               (define retry-rec (observe))
                               (define retry-wave
                                 (current-wave-for-attempt retry-rec wave-idx fence expected-id))
                               (if (not retry-wave)
                                   (campaign-result 'wave-cancelled '() "stale completion ignored")
                                   (begin
                                     (set-campaign-wave-status! retry-wave 'pending)
                                     (persist-campaign! base-dir retry-rec)
                                     (mirror-status! 'pending)
                                     (parameterize ([current-gsd-wave-failure-context
                                                     (wave-failure-context-block
                                                      verifier-message
                                                      (no-change-target-files verifier-message))])
                                       (run-once* (sub1 no-change-retries-left)
                                                  wt-box
                                                  keep-branch-box
                                                  attempt-id-box))))]
                              [else
                               (mark-attempt-artifact-terminal!
                                base-dir
                                (campaign-plan-id active)
                                wave-idx
                                expected-id
                                'failure
                                #:merge-status
                                (and wt
                                     (artifact-merge-status/local (wave-worktree-repo-root wt)
                                                                  (wave-worktree-branch wt))))
                               (notify-terminal-transition*! (campaign-plan-id active)
                                                             wave-idx
                                                             'wave-failed
                                                             #:reason
                                                             (if (string=? verifier-message "")
                                                                 "verifier rejected"
                                                                 verifier-message))
                               (campaign-result 'wave-failed
                                                '()
                                                (if (string=? verifier-message "")
                                                    "verifier rejected"
                                                    verifier-message))])]
                           [(stale-attempt invalid-state)
                            (campaign-result 'wave-cancelled '() "stale completion ignored")]
                           [else
                            (notify-terminal-transition*! (campaign-plan-id active)
                                                          wave-idx
                                                          'wave-failed
                                                          #:reason "unexpected completion state")
                            (campaign-result 'wave-failed '() "unexpected completion state")])])))])]
            [(failed)
             ;; Persist and report the runner's own failure reason.
             (define failure-reason (runner-outcome-failure-reason outcome run-result))
             ;; BUG-0043 (W2): route the failure text to the typed error
             ;; transcript surface; the conversation copy is gone.
             (emit-wave-outcome-error! wave-idx outcome (wave-execution-outcome-message run-result))
             (mark-attempt-artifact-terminal!
              base-dir
              (campaign-plan-id active)
              wave-idx
              expected-id
              'failure
              #:merge-status (let ([w (unbox wt-box)])
                               (and w
                                    (artifact-merge-status/local (wave-worktree-repo-root w)
                                                                 (wave-worktree-branch w)))))
             (if (persist-current-status! 'failed #:failure-reason failure-reason)
                 (begin
                   (apply-wave-status-projections! base-dir
                                                   wave-idx
                                                   STATUS-FAILED
                                                   (lambda (idx) (wave-slug base-dir idx)))
                   (notify-terminal-transition*!
                    (campaign-plan-id active)
                    wave-idx
                    (wave-failure-notification-kind (wave-execution-outcome-message run-result))
                    #:reason failure-reason)
                   (campaign-result 'wave-failed '() failure-reason))
                 (campaign-result 'wave-cancelled '() "stale runner result ignored"))]
            ;; D8 (#9357) + BUG-0024 W3: transient provider/network/SSE
            ;; failure — do NOT consume the attempt. Roll back the
            ;; begin-attempt! increment, reset the wave to pending, and
            ;; AUTOMATICALLY re-attempt the same wave with exponential
            ;; backoff (30s/60s/120s), bounded by
            ;; current-gsd-campaign-infra-retries (default 3). Each retry
            ;; emits gsd.campaign.infra-retry and carries a durable
            ;; prior-attempt context. Only when the bound is exhausted does
            ;; the campaign stop with an aggregated message listing all
            ;; failure timestamps — no manual /retry needed for transient
            ;; outages, no hot-looping on a sick provider.
            [(infra-failed)
             (define infra-wave (current-wave-for-attempt after-run wave-idx fence expected-id))
             (define failed-attempt (and infra-wave (campaign-wave-attempt-count infra-wave)))
             ;; BUG-0030 (action 3): the attempt died mid-wave — capture its
             ;; worktree's uncommitted state (dirty-sha / diff-stat /
             ;; edited-files) NOW, before anything can clean it, and join it
             ;; to the PRIOR ATTEMPT CONTEXT block (same carrier, ~2 KB cap)
             ;; so the automatic re-attempt resumes from recorded, recoverable
             ;; progress instead of re-deriving context from zero.
             (define dying-wt (unbox wt-box))
             (define dirty-capture
               (and dying-wt
                    (worktree-isolation-enabled? #:isolate? isolate?)
                    (capture-worktree-dirty-state dying-wt)))
             (when dirty-capture
               (log-info
                "gsd: wave ~a infra-stopped with dirty worktree state — captured ~a file(s), dirty-sha ~a"
                wave-idx
                (length (hash-ref dirty-capture 'edited-files '()))
                (or (hash-ref dirty-capture 'dirty-sha #f) "none")))
             ;; v1.00.21 W5 (BUG-0029): the dying attempt is TERMINAL
             ;; ('interrupted) even though the WAVE stays re-attemptable —
             ;; its ledger entry must not dangle open forever.
             (mark-attempt-artifact-terminal!
              base-dir
              (campaign-plan-id active)
              wave-idx
              expected-id
              'interrupted
              #:merge-status (and dying-wt
                                  (artifact-merge-status/local (wave-worktree-repo-root dying-wt)
                                                               (wave-worktree-branch dying-wt))))
             (when infra-wave
               (define (roll-back-wave! w)
                 (set-campaign-wave-status! w 'pending)
                 (set-campaign-wave-attempt-count! w (max 0 (sub1 (campaign-wave-attempt-count w))))
                 (set-campaign-wave-current-attempt! w #f)
                 ;; W3: durable prior-attempt context so the automatic
                 ;; re-attempt resumes instead of re-exploring from zero.
                 ;; W4 (BUG-0030): the dirty-state capture joins it.
                 (set-campaign-wave-attempt-context!
                  w
                  (append-dirty-capture-to-context
                   (build-wave-attempt-context wave-idx
                                               (or failed-attempt 0)
                                               (wave-execution-outcome-message run-result))
                   dirty-capture)))
               ;; Roll back BOTH views: the disk-truth `after-run` copy (which
               ;; is persisted) and the in-memory `active` wave — the
               ;; recursive run-once* re-entry re-begins the attempt on
               ;; `active`, so without this the re-attempt would start from
               ;; the stale, un-rolled-back attempt count.
               (roll-back-wave! infra-wave)
               (define active-wave (find-wave active wave-idx))
               (when active-wave
                 (roll-back-wave! active-wave))
               (persist-campaign! base-dir after-run)
               (mirror-status! 'pending))
             (define infra-retries-left (infra-retry-consume! infra-retry-state failed-attempt))
             ;; retries-left is the budget AFTER this failure; a bound of N
             ;; permits N automatic re-attempts, so re-enter while the budget
             ;; is non-negative (0 = one last retry; -1 = exhausted).
             (if (>= infra-retries-left 0)
                 (let* ([this-retry (- (current-gsd-campaign-infra-retries) infra-retries-left)]
                        [delay-secs (infra-retry-backoff-secs infra-retries-left)])
                   (log-info
                    "gsd: wave ~a infra failure — automatic retry ~a/~a in ~as (attempt not consumed)"
                    wave-idx
                    this-retry
                    (current-gsd-campaign-infra-retries)
                    delay-secs)
                   (emit-infra-retry-event! wave-idx (or failed-attempt 0) delay-secs)
                   (when (> delay-secs 0)
                     (sleep delay-secs))
                   ;; Re-enter run-once* for the SAME wave with the same
                   ;; attempt budget. The prior-attempt context block rides
                   ;; the existing current-gsd-wave-failure-context prompt
                   ;; plumbing — no parallel state.
                   (parameterize ([current-gsd-wave-failure-context
                                   (wave-attempt-context-block
                                    (and infra-wave (campaign-wave-attempt-context infra-wave)))])
                     (run-once* no-change-retries-left wt-box keep-branch-box attempt-id-box)))
                 ;; Bound exhausted: fail closed with an aggregated message
                 ;; listing every failure timestamp (attempt not consumed —
                 ;; the durable wave stays pending and re-attemptable).
                 ;; BUG-0043 (W2): the terminal infra-failure text rides the
                 ;; typed error transcript event.
                 (begin
                   (emit-wave-outcome-error! wave-idx
                                             'infra-failed
                                             (wave-execution-outcome-message run-result))
                   (campaign-result 'wave-cancelled
                                    '()
                                    (infra-retry-exhausted-message infra-retry-state))))]
            [(cancelled interrupted)
             (mark-attempt-artifact-terminal! base-dir
                                              (campaign-plan-id active)
                                              wave-idx
                                              expected-id
                                              'interrupted)
             (interrupt-current! (wave-execution-outcome-message run-result)
                                 #:failure-reason (runner-outcome-failure-reason outcome run-result))]
            ;; A hung tool that exceeded its deadline: persist INTERRUPTED per
            ;; D1 (cancelled/error/timeout stop the campaign) and never emit a
            ;; completion — the durable record says interrupted, so a restart
            ;; re-attempts the wave (at-least-once, exactly-once event).
            [(timed-out)
             ;; All retries exhausted. Persist INTERRUPTED per D1 (cancelled/
             ;; error/timeout stop the campaign) and never emit a completion —
             ;; the durable record says interrupted, so a restart re-attempts
             ;; the wave (at-least-once, exactly-once event).
             ;; BUG-0043 (W2): the stall/timeout text rides the typed error
             ;; transcript event, not the conversation surface.
             (define timeout-message
               (if (> timeout-retries 0)
                   (format "~a after ~a retries"
                           (wave-execution-outcome-message run-result)
                           timeout-retries)
                   (wave-execution-outcome-message run-result)))
             (emit-wave-outcome-error! wave-idx
                                       'timed-out
                                       (wave-execution-outcome-message run-result))
             (mark-attempt-artifact-terminal! base-dir
                                              (campaign-plan-id active)
                                              wave-idx
                                              expected-id
                                              'interrupted)
             (interrupt-current! timeout-message #:failure-reason timeout-message)]
            [else
             ;; BUG-0043 (W2): unknown terminal outcome — same typed error
             ;; transcript routing as the named failure branches.
             ;; Name an unknown terminal outcome durably.
             (define failure-reason (runner-outcome-failure-reason outcome run-result))
             (emit-wave-outcome-error! wave-idx outcome (wave-execution-outcome-message run-result))
             (mark-attempt-artifact-terminal!
              base-dir
              (campaign-plan-id active)
              wave-idx
              expected-id
              'failure
              #:merge-status (let ([w (unbox wt-box)])
                               (and w
                                    (artifact-merge-status/local (wave-worktree-repo-root w)
                                                                 (wave-worktree-branch w)))))
             (if (persist-current-status! 'failed #:failure-reason failure-reason)
                 (begin
                   (apply-wave-status-projections! base-dir
                                                   wave-idx
                                                   STATUS-FAILED
                                                   (lambda (idx) (wave-slug base-dir idx)))
                   (notify-terminal-transition*!
                    (campaign-plan-id active)
                    wave-idx
                    (wave-failure-notification-kind (wave-execution-outcome-message run-result))
                    #:reason failure-reason)
                   (campaign-result 'wave-failed '() failure-reason))
                 (campaign-result 'wave-cancelled '() "stale runner result ignored"))])]))
     ;; W7 (#9512b): run-once* receives fresh boxes; the worktree outlives
     ;; the run (its branch carries delivery evidence through verification
     ;; and the completion decision). This dynamic-wind owns the terminal
     ;; release on EVERY exit path — normal return, cancellation, or
     ;; exception: the worktree directory is always removed; the branch is
     ;; KEPT only when delivery was approved (release-wave-worktree! keeps
     ;; it as durable merge evidence), deleted otherwise
     ;; (cleanup-wave-worktree!). The no-change retry inside run-once*
     ;; already releases its own rejected worktree and resets wt-box, so
     ;; this fires exactly once for the final attempt's worktree (or never,
     ;; when isolation is off / creation fell back to the shared checkout).
     (let ([wt-box (box #f)]
           [keep-branch-box (box #f)]
           ;; v1.00.21 W5 (BUG-0029): ledger id of the CURRENT attempt —
           ;; maintained across recursive re-entries so the dynamic-wind
           ;; postlude can mark the final attempt's teardown outcome.
           [attempt-id-box (box #f)])
       ;; BUG-0024 W3: each campaign-wave run starts with a FRESH infra-retry
       ;; budget so (a) a parameterized current-gsd-campaign-infra-retries
       ;; is honored (the module-level box captured its value at module load)
       ;; and (b) back-to-back runs (tests, /go restarts) never inherit a
       ;; partially-consumed budget.
       (set-box! infra-retry-state (cons (current-gsd-campaign-infra-retries) '()))
       (dynamic-wind
        void
        (lambda () (run-once* no-change-retries wt-box keep-branch-box attempt-id-box))
        (lambda ()
          (define wt (unbox wt-box))
          (when wt
            (with-handlers ([exn:fail? (lambda (e)
                                         (log-warning "wave ~a: worktree release failed: ~a"
                                                      wave-idx
                                                      (exn-message e)))])
              (if (unbox keep-branch-box)
                  (release-wave-worktree! wt)
                  (cleanup-wave-worktree! wt)))))))]))

(require (only-in "../../runtime/settings.rkt" load-settings)
         (only-in "../../runtime/settings-query.rkt"
                  gsd-stall-soft-limit
                  gsd-stall-hard-limit
                  gsd-stall-window
                  gsd-stall-backstop
                  ;; v1.00.21 W1 (BUG-0044): canonical defaults for the
                  ;; settings-absent fallback (8/15/30/300).
                  STALL-SOFT-LIMIT-DEFAULT
                  STALL-HARD-LIMIT-DEFAULT
                  STALL-REPETITION-WINDOW-DEFAULT
                  STALL-BACKSTOP-LIMIT-DEFAULT
                  ;; v1.00.22 W5 (BUG-0039): campaign budget ceilings
                  ;; gsd.campaign.max-cost / gsd.campaign.max-tokens.
                  gsd-campaign-max-cost
                  gsd-campaign-max-tokens))

;; ============================================================
;; Full campaign execution (loop one wave at a time)
;; ============================================================

(define (run-campaign! base-dir
                       rec
                       #:runner [runner default-runner]
                       #:verifier [verifier default-verifier]
                       #:meta-fix-predicate [meta-fix-predicate (lambda (_) #f)]
                       #:timeout-sec [timeout-sec #f]
                       #:lease-owner [lease-owner "unknown"]
                       ;; v1.00.19 W2 (BUG-0028 S1): 'auto = honor the
                       ;; gsd.worktree-isolation project-settings key (see
                       ;; resolve-worktree-isolation in wave-executor.rkt).
                       #:isolate? [isolate-arg 'auto])
  ;; Resolve ONCE at campaign start so every downstream reader (including
  ;; the pre-wave isolation log) sees the effective flag, settings included.
  (define project-settings (load-project-settings-silently base-dir))
  (define isolate? (apply-worktree-isolation-setting! project-settings #:isolate? isolate-arg))
  (define plan-id (campaign-plan-id rec))
  ;; v1.00.22 W6 (BUG-0040): resolve the notification sinks ONCE from
  ;; project settings (gsd.notify.* keys; silent default outside tmux
  ;; with no keys — then the fan-out below is a no-op). Wave-level
  ;; sites inside run-campaign-wave read the parameterized list; the
  ;; campaign-level helper closes over the resolved list.
  (define notify-sinks
    (with-handlers ([exn:fail? (lambda (e)
                                 (log-warning "gsd-notify: sink resolution failed: ~a"
                                              (exn-message e))
                                 '())])
      (gsd-notify-sinks-from-settings project-settings)))
  (define (notify-campaign-terminal! kind wave-idx reason)
    (with-handlers ([exn:fail? void])
      (notify-terminal-transition! notify-sinks
                                   (make-gsd-notification plan-id wave-idx kind #:reason reason))))
  ;; D4 (#9351): pass the owning session id so the lease file names its
  ;; holder instead of the opaque "unknown" observed in incident 81f9be4b.
  (define lease (acquire-lease base-dir plan-id #:session-id lease-owner))
  (if (not lease)
      (campaign-result 'busy '() "campaign lease held by another process")
      (dynamic-wind
       void
       (lambda ()
         ;; A request may have waited behind another process. Reload only after
         ;; owning the lease, then carry durable state between waves.
         (define authoritative (or (load-campaign-record base-dir plan-id) rec))
         ;; v0.99.89 W2: repair stale projections left by a crash between the
         ;; durable commit and the projection apply (golden-trace oracle
         ;; finding #2). The durable record is the source of truth; reconcile
         ;; re-derives PLAN.md / wave docs / STATE.md from it. Never blocks
         ;; the campaign — a reconcile failure only logs.
         (with-handlers ([exn:fail? (lambda (e)
                                      (log-warning "projection reconcile failed: ~a"
                                                   (exn-message e)))])
           (reconcile-projections-from-waves! base-dir
                                              (for/list ([w (campaign-record-waves authoritative)])
                                                (cons (campaign-wave-index w)
                                                      (campaign-wave-status w)))
                                              (plan-slug-map base-dir)))
         ;; v0.99.90 W2 (#9233): the completion outbox is a DERIVED ledger —
         ;; a crash between the durable commit and the outbox append would
         ;; otherwise lose the event. Rebuild missing events from the durable
         ;; 'done waves (dedup-safe; never invents events for non-done waves).
         (with-handlers ([exn:fail? (lambda (e)
                                      (log-warning "completion outbox reconcile failed: ~a"
                                                   (exn-message e)))])
           (reconcile-completion-outbox! base-dir authoritative))
         ;; v1.00.17 W6 (#9512a): campaign start reclaims orphaned wave
         ;; worktrees left behind by crashed attempts (best-effort, logged;
         ;; unrelated worktrees and other campaigns' worktrees untouched).
         (when isolate?
           (define wt-repo (find-repo-root base-dir))
           (when wt-repo
             (reclaim-orphaned-worktrees! wt-repo #:campaign-id plan-id)))
         (define final-result
           (let loop ([current authoritative]
                      [completed '()])
             (define next-idx (select-next-actionable-wave current))
             (cond
               [(campaign-record-cancellation current)
                (notify-terminal-transition*! (campaign-plan-id current)
                                              #f
                                              'campaign-cancelled
                                              #:reason "campaign cancellation requested")
                (campaign-result 'wave-cancelled
                                 (reverse completed)
                                 "campaign cancellation requested")]
               [(not next-idx)
                (notify-terminal-transition*! (campaign-plan-id current)
                                              #f
                                              'campaign-complete
                                              #:reason "all waves done or deferred")
                (campaign-result 'campaign-complete (reverse completed) "all waves done or deferred")]
               [else
                (define result
                  ;; v1.00.22 W6 (BUG-0040): wave-level terminal sites
                  ;; (done/failed/stall/budget-pause) inside
                  ;; run-campaign-wave emit through these sinks.
                  (parameterize ([current-gsd-notify-sinks notify-sinks])
                    (run-campaign-wave base-dir
                                       current
                                       next-idx
                                       #:runner runner
                                       #:verifier verifier
                                       #:meta-fix-predicate meta-fix-predicate
                                       #:fence-token (add1 (campaign-fence-token current))
                                       #:timeout-sec timeout-sec
                                       #:timeout-retries (current-gsd-wave-timeout-retries)
                                       #:isolate? isolate?)))
                (define observed (load-campaign-record base-dir plan-id))
                (mirror-durable-statuses! rec observed)
                (case (campaign-result-status result)
                  [(wave-done)
                   (define refreshed (load-campaign-record base-dir plan-id))
                   (if refreshed
                       (loop refreshed (cons next-idx completed))
                       (campaign-result 'error (reverse completed) "campaign record disappeared"))]
                  [(meta-fix)
                   ;; Meta-fix: retry the same wave, attempt not consumed
                   (define refreshed (load-campaign-record base-dir plan-id))
                   (if refreshed
                       (loop refreshed completed)
                       (campaign-result 'error (reverse completed) "campaign record disappeared"))]
                  [(wave-failed wave-cancelled)
                   ;; The runner timeout/cancellation boundary owns only its wave
                   ;; thread. Do not stop the process-global gateway worker: it
                   ;; may be serving an unrelated interactive or SDK session.
                   (campaign-result (campaign-result-status result)
                                    (reverse completed)
                                    (campaign-result-message result))]
                  [else
                   (campaign-result 'error (reverse completed) "unexpected coordinator state")])])))
         ;; v1.00.21 W5 (BUG-0029 action 3): the campaign ended (success OR
         ;; terminal failure) — report non-delivery leftover artifacts and
         ;; offer operator-approved reclaim. NEVER auto-deletes.
         (report-campaign-artifact-leftovers! base-dir plan-id #:repo-root (find-repo-root base-dir))
         final-result)
       (lambda () (release-lease! lease)))))

;; ============================================================
;; /go N assertion (D8)
;; ============================================================

(define (assert-go-n rec n)
  (define next (select-next-actionable-wave rec))
  (and next (= n next)))

;; ============================================================
;; Git Root Resolution (F-7)
;; Uses `current-git-root` parameter from plan-context-builder for W1 cwd migration.
(define (find-git-root start-dir)
  (define start-path
    (path->complete-path (if (path? start-dir)
                             start-dir
                             (string->path start-dir))))
  (define (has-git? dir)
    (define git-marker (build-path dir ".git"))
    (or (directory-exists? git-marker) (file-exists? git-marker)))
  (define q-sub (build-path start-path "q"))
  (cond
    [(has-git? start-path) start-path]
    [(and (directory-exists? q-sub) (has-git? q-sub)) q-sub]
    [else
     ;; Walk up from start-path first (handles nested dirs in temp tests)
     (define walked (find-git-root-walking-up start-path has-git?))
     (if walked
         walked
         ;; Last resort: use current-git-root parameter if set and valid
         (let ([param-root (current-git-root)])
           (if (and param-root (has-git? param-root)) param-root #f)))]))

(define (find-git-root-walking-up start-path has-git?)
  (let loop ([dir start-path])
    (cond
      [(has-git? dir) dir]
      [else
       (define-values (parent _sub _dir?) (split-path dir))
       (if (and parent (path? parent) (not (equal? parent dir)))
           (loop parent)
           #f)])))

(define (git-available? base-dir)
  (define git (find-executable-path "git"))
  (define (inside-work-tree? dir)
    (and git
         dir
         (directory-exists? dir)
         (let ([stdout (open-output-string)]
               [stderr (open-output-string)])
           (with-handlers ([exn:fail? (lambda (_) #f)])
             (define exit-code
               (parameterize ([current-output-port stdout]
                              [current-error-port stderr])
                 (system*/exit-code git "-C" dir "rev-parse" "--is-inside-work-tree")))
             (and (zero? exit-code) (string=? (string-trim (get-output-string stdout)) "true"))))))
  ;; Validate from the requested base directory. Preserve the supported
  ;; two-tier checkout layout by trying its q/ child explicitly, but never
  ;; trust a .git marker or an unrelated current-git-root fallback.
  (and base-dir (or (inside-work-tree? base-dir) (inside-work-tree? (build-path base-dir "q"))) #t))
;; ============================================================
;; Provide
;; ============================================================

(provide campaign-lease
         find-git-root
         git-available?
         campaign-lease?
         acquire-lease
         release-lease!
         campaign-result
         campaign-result-status
         campaign-result-completed-waves
         campaign-result-message
         run-campaign-wave
         run-campaign!
         prompt-run-result->outcome
         infra-failure?
         assert-go-n
         campaign-request
         campaign-request?
         make-campaign-request
         campaign-request-base-dir
         campaign-request-record
         campaign-request-prompt-for-wave
         campaign-request-verifier
         campaign-request-timeout-sec
         execute-campaign-request!
         current-gsd-wave-cancel!
         register-campaign-request!
         lookup-campaign-request
         no-change-rejection?
         no-change-target-files
         build-wave-attempt-context
         execute-campaign-token!
         ;; v1.00.19 W3 (BUG-0031): version-freshness guard + build identity
         campaign-freshness
         campaign-freshness?
         freshness-running-version
         freshness-checkout-version
         freshness-origin-head
         freshness-behind-origin?
         freshness-offline?
         check-campaign-freshness
         read-checkout-build-version
         resolve-origin-main-head
         checkout-behind-origin-main?
         freshness-stale?
         freshness-refusal-message
         freshness-offline-warning
         stamp-campaign-build-identity!
         current-gsd-freshness-check
         campaign-request-allow-stale?
         ;; v1.00.22 W7 (BUG-0042): re-provide the extracted seams so
         ;; existing importers of go-orchestrator keep working unchanged.
         current-campaign-usage-observation
         loop-result->usage-datum
         record-usage-observation!
         take-usage-observation!
         stamp-observed-usage!
         resolve-campaign-budget
         pause-campaign-if-over-budget!
         resume-after-budget-pause!
         load-project-settings-silently
         wave-worktree-head-sha
         wave-worktree-delivery-context
         record-wave-delivery!
         git-out->string
         git-ok?
         artifact-merge-status/local
         record-attempt-artifact!
         mark-attempt-artifact-terminal!
         record-attempt-teardown!
         LEFTOVERS-REPORT-MAX-ENTRIES
         report-campaign-artifact-leftovers!
         record-delivered-branches
         mirror-durable-statuses!
         ;; v1.00.18 W5 (#9513): mutation-stall watchdog
         gsd-stall-exn
         gsd-stall-exn?
         make-gsd-stall-exn
         current-gsd-stall-steerer
         stall-steering-message
         stall-hard-failure-message
         stall-cause-message?
         resolve-effective-stall-thresholds
         wave-failure-notification-kind
         wave-doc-target-files
         wrap-run-one-with-stall-watchdog
         ;; v1.00.17 W7 (#9512b): branch-based delivery bookkeeping
         ;; (exposed for testing; the commit step itself runs pre-approval)
         wave-delivery-commit-message
         commit-wave-worktree!
         wave-worktree-base-commit
         ;; v1.00.20 W4 (BUG-0030): mid-wave dirty-state hand-off + guard
         capture-worktree-dirty-state
         append-dirty-capture-to-context
         outside-lease-dirty-rkt-files
         outside-lease-dirty-warning
         warn-outside-lease-dirty-state!
         wave-branch-commit-count
         warn-zero-commit-delivery-branch!
         take-up-to)
