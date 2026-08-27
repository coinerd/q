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
                  current-gsd-campaign-infra-retry-delay)
         (only-in "prompts.rkt"
                  wave-failure-context-block
                  wave-attempt-context-block
                  executor-reanchor-prompt)
         (only-in "events.rkt" emit-gsd-event!)
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
         racket/os)

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

;; D8 (#9357): classify a loop-result as an infrastructure failure (provider/
;; network/SSE timeout) rather than a genuine agent/code failure. Positive
;; signals: classify-error domain network/provider, a retry-exhausted marker
;; (the provider retry machinery only retries transient LLM failures), or
;; stream/SSE/read-timeout messages. A single transient provider stall must
;; NOT consume a campaign attempt (attempt-4: 30 tools done, then one
;; 120 s SSE read timeout → wave-failed).
(define (infra-failure? result)
  (and (loop-result? result)
       (eq? (loop-result-termination-reason result) 'error)
       (let ([meta (loop-result-metadata result)])
         (and (hash? meta)
              (let* ([err-type (hash-ref meta 'errorType #f)]
                     [domain (and (pair? err-type) (car err-type))]
                     [retries (hash-ref meta 'retries-attempted #f)]
                     [msg (let ([e (hash-ref meta 'error #f)]) (if (string? e) e ""))])
                (or (memq domain '(network provider))
                    (and retries (positive? retries))
                    (regexp-match?
                     #rx"read timeout|SSE|stream|circuit|temporarily unavailable|network|connection"
                     msg)))))))

(define (prompt-run-result->outcome result)
  (cond
    [(loop-result? result)
     (define termination (loop-result-termination-reason result))
     (define metadata (loop-result-metadata result))
     (define tool-loop-limit? (hash-ref metadata 'toolLoopLimit #f))
     (define completion-reason (hash-ref metadata 'reason #f))
     (define shutdown-reason? (equal? completion-reason "graceful-shutdown"))
     (cond
       [tool-loop-limit? (wave-execution-outcome 'failed "tool loop limit reached")]
       [(and (eq? termination 'completed) (not completion-reason)) (wave-execution-outcome 'done "")]
       [(or shutdown-reason? (member termination '(cancelled force-shutdown shutdown)))
        (wave-execution-outcome 'cancelled "")]
       [(eq? termination 'completed)
        (wave-execution-outcome 'failed (format "completion blocked: ~a" completion-reason))]
       [(eq? termination 'tool-calls-pending)
        (wave-execution-outcome 'failed "tool calls remain pending")]
       [(eq? termination 'empty-response)
        (wave-execution-outcome 'failed "model returned an empty response")]
       [(let ([e (hash-ref metadata 'error #f)]) (stall-cause-message? e))
        ;; BUG-0037 W1: watchdog kill → retryable infrastructure, bounded
        ;; auto-resume with prior-attempt context (never a manual /retry).
        (wave-execution-outcome 'infra-failed
                                (format "~a attempt preserved for automatic re-attempt"
                                        (hash-ref metadata 'error "")))]
       [(infra-failure? result)
        ;; D8 (#9357): transient provider/network/SSE failure — distinct
        ;; outcome so run-campaign-wave can preserve the attempt.
        (wave-execution-outcome 'infra-failed
                                "provider/network failure — wave preserved (attempt not consumed)")]
       [else (wave-execution-outcome 'failed (format "termination reason: ~a" termination))])]
    [(eq? result 'completed) (wave-execution-outcome 'done "")]
    [(eq? result 'ok) (wave-execution-outcome 'done "")]
    [(eq? result 'cancelled) (wave-execution-outcome 'cancelled "")]
    [else (wave-execution-outcome 'failed (format "unknown runner result: ~s" result))]))

;; ============================================================
;; v1.00.22 W5 (BUG-0039): campaign cost/token accounting + ceilings.
;;
;; Provider usage metadata rides loop-result's 'usage field but is
;; STRIPPED at the wave-runner-port boundary (wave-execution-outcome is
;; exactly (kind message)). The in-process default-runner path observes
;; the raw loop-result inside the runner lambda — parameterized over a
;; box in execute-campaign-request!'s extent — and the parent retry-loop
;; stamps it onto the durable record at ATTEMPT boundaries (never
;; mid-tool-call). Honest accounting: absent metadata is recorded
;; distinctly as 'usage-missing, never fake zeros (campaign-state's
;; stamp-wave-usage!). Ceilings gsd.campaign.max-cost /
;; gsd.campaign.max-tokens cross → durable pause with a named reason;
;; raising the ceiling + /go resume clears it and continues cleanly.
;; ============================================================

(define current-campaign-usage-observation
  (make-parameter #f)) ; box of (cons wave-index usage-datum|#f) | #f

;; Extract an honest usage datum from a raw loop-result, or #f when the
;; provider reported nothing. Tolerant of any metadata shape — junk is
;; treated as absent (usage-missing), never coerced to zeros.
(define (loop-result->usage-datum result)
  (and (loop-result? result)
       (let ([u (hash-ref (loop-result-metadata result) 'usage #f)])
         (and (hash? u)
              (let ()
                (define (num key)
                  (define v (hash-ref u key #f))
                  (cond
                    [(and (real? v) (not (negative? v))) v]
                    [(string? v)
                     (define n (string->number v))
                     (and (real? n) (not (negative? n)) n)]
                    [else #f]))
                (define in (num 'prompt_tokens))
                (define out (num 'completion_tokens))
                (define tot (or (num 'total_tokens) (and (or in out) (+ (or in 0) (or out 0)))))
                (define cost (num 'cost))
                (and (or in out tot cost)
                     (usage-datum in out tot cost (and (hash-ref u 'estimated? #f) #t))))))))

;; Runner-lambda side: observe BEFORE outcome conversion strips metadata.
(define (record-usage-observation! wave-index run-result)
  (define b (current-campaign-usage-observation))
  (when (box? b)
    (set-box! b (cons wave-index (loop-result->usage-datum run-result)))))

;; Parent side: drain + reset after an attempt (next attempt starts clean).
(define (take-usage-observation!)
  (define b (current-campaign-usage-observation))
  (and (box? b)
       (let ([v (unbox b)])
         (set-box! b #f)
         v)))

;; Durable per-attempt stamp: load fresh → stamp (datum, or usage-missing
;; when nothing was observed) → persist. Best-effort at attempt
;; boundaries; a stamp failure only logs (campaign liveness over
;; telemetry purity). Idempotent across restarts: stamping a second
;; observation REPLACES the attempt's numbers and re-accumulates the
;; wave totals from the per-attempt fields — no double counting.
(define (stamp-observed-usage! base-dir plan-id wave-index observation)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning "campaign usage stamp failed: ~a" (exn-message e)))])
    (define rec (load-campaign-record base-dir plan-id))
    (when rec
      (stamp-wave-usage! rec wave-index (and observation (cdr observation)))
      (persist-campaign! base-dir rec))))

;; Resolve the campaign ceilings from project/user settings.
;; (cons max-cost max-tokens) — each side #f when unset/invalid.
(define (resolve-campaign-budget base-dir)
  (define s (load-project-settings-silently base-dir))
  (define (pos-real v)
    (and (real? v) (positive? v) v))
  (define (pos-int v)
    (and (real? v) (>= (floor v) 1) (inexact->exact (floor v))))
  (cons (pos-real (gsd-campaign-max-cost s)) (pos-int (gsd-campaign-max-tokens s))))

;; Durable ceiling check at an attempt boundary. Returns the pause
;; message string when the campaign is now paused, #f otherwise
;; (within budget, ceilings unset, or the pause could not persist —
;; checked again at the next boundary in the latter case).
(define (pause-campaign-if-over-budget! base-dir plan-id)
  (define budget (resolve-campaign-budget base-dir))
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning "campaign budget check failed: ~a" (exn-message e))
                               #f)])
    (define rec (load-campaign-record base-dir plan-id))
    (cond
      [(not rec) #f]
      ;; Already paused (e.g. resumed with the ceiling untouched):
      ;; surface the existing named reason.
      [(campaign-record-budget-pause rec)
       =>
       campaign-budget-pause-message]
      [else
       (define pause (budget-pause-violation? rec (car budget) (cdr budget)))
       (and pause
            (begin
              (pause-campaign-for-budget! rec pause)
              (persist-campaign! base-dir rec)
              (log-info "campaign ~a paused by budget ceiling (~a)"
                        plan-id
                        (campaign-budget-pause-kind pause))
              (campaign-budget-pause-message pause)))])))

;; Durable resume gate for run-campaign!'s loop: a paused campaign stays
;; paused while the CURRENT ceiling is still crossed; a raised (or
;; removed) ceiling clears the pause and returns the cleared record so
;; the loop continues cleanly (nothing dropped, nothing re-counted).
;; Returns (values proceed? refreshed-record-or-#f reason-or-#f).
(define (resume-after-budget-pause! base-dir plan-id rec)
  (define pause (and rec (campaign-record-budget-pause rec)))
  (cond
    [(not pause) (values #t #f #f)]
    [(budget-pause-still-violated? pause
                                   (car (resolve-campaign-budget base-dir))
                                   (cdr (resolve-campaign-budget base-dir)))
     (values #f #f (campaign-budget-pause-message pause))]
    [else
     (with-handlers ([exn:fail? (lambda (e)
                                  (log-warning "budget-pause clear failed: ~a" (exn-message e))
                                  (values #f #f (campaign-budget-pause-message pause)))])
       (define fresh (load-campaign-record base-dir plan-id))
       (cond
         [(not fresh) (values #f #f "campaign record disappeared")]
         [(not (campaign-record-budget-pause fresh)) (values #t fresh #f)]
         [else
          (clear-budget-pause! fresh)
          (persist-campaign! base-dir fresh)
          (log-info "campaign ~a budget pause cleared (ceiling raised); resuming" plan-id)
          (values #t fresh #f)]))]))

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

;; Hard-limit exception. An exn:fail subtype so the production runner
;; boundary's existing exn:fail? handler (execute-campaign-request!)
;; converts it into (wave-execution-outcome 'failed <stall message>) with
;; the explicit cause intact; run-campaign-wave installs its own
;; gsd-stall-exn? guard so plain/test runners see the same conversion.
(struct gsd-stall-exn exn:fail () #:transparent)
(define (make-gsd-stall-exn message)
  (gsd-stall-exn message (current-continuation-marks)))

;; Soft-limit steering message. Reuses W2's re-anchor constructor
;; (executor-reanchor-prompt) so the executor role context travels with
;; the steering — the steered session cannot reinterpret itself as an
;; interactive assistant. Pure constructor; no I/O.
(define (stall-steering-message calls-since-mutation wave-id campaign-id task-line target-files)
  (string-append
   (executor-reanchor-prompt wave-id
                             campaign-id
                             task-line
                             "(no edit has been made yet — this session has only read/explored)")
   "\n\n"
   "[MUTATION-STALL WATCHDOG — SOFT LIMIT REACHED]\n"
   (format "You have made ~a calls without any edit. Wave targets: ~a. "
           calls-since-mutation
           (if (null? target-files)
               "(none recorded)"
               (string-join target-files ", ")))
   "Begin the first edit now."))

;; Hard-limit failure cause. BUG-0037 W1 reclassification: a stall death
;; during an attempt with zero file mutations maps to the INFRA-RETRY path
;; ('infra-failed outcome), not straight to campaign stop — the bounded
;; automatic re-attempt carries prior-attempt context instead of forcing a
;; manual /retry. Wording keeps D8 (#9357)'s infra vocabulary out so
;; infra-failure? itself does not double-match; classification happens via
;; the explicit gsd-stall-exn? handlers.
(define (stall-hard-failure-message calls-since-mutation
                                    limit
                                    target-files
                                    [stall-tool #f]
                                    [recent-tools '()])
  (define targets-desc
    (if (null? target-files)
        "(none recorded)"
        (string-join target-files ", ")))
  (define tools-desc
    (if (null? recent-tools)
        "(none recorded)"
        (string-join (map (lambda (t) (format "~a" t)) recent-tools) ", ")))
  (format (string-append "mutation-stall watchdog: attempt terminated after ~a mutation-free "
                         "calls (limit ~a)~a. Target files: ~a. Recent tools: ~a. "
                         "The attempt will be re-attempted automatically with its prior "
                         "context preserved — resume implementation from recorded state.")
          calls-since-mutation
          limit
          (if stall-tool
              (format " — repeating '~a'" stall-tool)
              "")
          targets-desc
          tools-desc))

;; Steering injection hook. Default implementation logs the steering and
;; arms the thread's empty-response re-anchor (W2's plumbing: the same
;; channel command-handlers parameterizes at session start) with the
;; steering message, so the next reasoning-only turn re-anchors the
;; executor to "begin the first edit now". Bindable for tests and for
;; future direct-injection adapters.
(define current-gsd-stall-steerer
  (make-parameter (lambda (message)
                    (log-info "gsd mutation-stall watchdog: steering executor (~a chars)"
                              (string-length (if (string? message) message "")))
                    ;; W2 plumbing: the re-anchor nudge is re-sent on the next empty
                    ;; visible-output turn — exactly the failure mode of v1.00.16 W3
                    ;; attempt-2 (long reasoning turns, no edits).
                    (current-empty-response-nudge message)
                    (void))))

;; Read a wave's declared target files from its wave doc (best effort).
;; The campaign record carries no file list; the wave doc does. Any
;; failure degrades to '() — steering without target names still orders
;; the first edit.
;; Wave-doc "File:" declaration lines: `- File: <path>` with optional
;; [exists]/[MISSING] and role annotations after the path. Paths contain
;; no spaces, so capture the first token.
(define wave-file-line-rx #rx"^[-*] *File: *([^ \t\n]+)")

(define (wave-doc-target-files base-dir wave-idx)
  (with-handlers ([exn:fail? (lambda (_) '())])
    (define slug (wave-slug base-dir wave-idx))
    (and slug
         (string? slug)
         (let ([doc (read-wave-doc base-dir wave-idx slug)])
           (and (hash? doc)
                (let ([content (hash-ref doc 'content "")])
                  (and (string? content)
                       (for/list ([line (in-list (string-split content "\n"))]
                                  #:when (regexp-match? wave-file-line-rx line))
                         (cadr (regexp-match wave-file-line-rx line))))))))))

;; Wrap a run-one function with stall observation. Chained onto the
;; existing post-tool-result hook (memory extraction keeps working) and
;; thread-inherited by the run-wave-with-timeout worker, so the parameter
;; IS visible in the live executor session. Returns a function idx →
;; outcome; a hard-stall raise is converted HERE for runners without
;; their own exn handler (the production path converts at the runner
;; boundary with the same message).
(define (wrap-run-one-with-stall-watchdog run-one-fn
                                          watchdog
                                          base-dir
                                          rec
                                          wave-idx
                                          soft-limit
                                          hard-limit)
  (if (not watchdog)
      run-one-fn
      (let* ([target-files (or (wave-doc-target-files base-dir wave-idx) '())]
             [wave (find-wave rec wave-idx)]
             [task-line (if wave
                            (format "W~a: ~a" wave-idx (campaign-wave-title wave))
                            (format "W~a" wave-idx))]
             [campaign-id (campaign-plan-id rec)]
             [prev-hook (current-post-tool-result-hook)])
        (lambda (idx)
          ;; BUG-0037 W1: a watchdog kill is RETRYABLE infrastructure —
          ;; map to 'infra-failed so run-once*'s bounded auto-retry picks
          ;; the attempt back up with prior-attempt context instead of
          ;; halting the campaign on 'wave-failed.
          (with-handlers ([gsd-stall-exn? (lambda (e)
                                            (wave-execution-outcome 'infra-failed (exn-message e)))])
            (parameterize
                ([current-post-tool-result-hook
                  (lambda (msgs sid root)
                    (prev-hook msgs sid root)
                    ;; BUG-0037 W1: records MUST carry 'arguments — the v2
                    ;; signature is tool name + normalized arguments hash,
                    ;; so a read of file A and a read of file B are
                    ;; DIFFERENT signatures. Without arguments every read
                    ;; collapses to one signature and any three reads trip
                    ;; the repetition limit.
                    (define records
                      (for/list ([m (in-list (if (list? msgs)
                                                 msgs
                                                 '()))]
                                 #:when (and (hash? m) (hash-ref m 'name #f)))
                        (hasheq 'name (hash-ref m 'name #f) 'arguments (hash-ref m 'arguments #f))))
                    (define event (stall-watchdog-observe! watchdog records))
                    (case event
                      [(soft-stall)
                       (define snap (stall-watchdog-snapshot watchdog))
                       (log-info "gsd: wave ~a soft stall (~a calls, no mutation) — steering"
                                 wave-idx
                                 (hash-ref snap 'calls-since-mutation))
                       ((current-gsd-stall-steerer)
                        (stall-steering-message (hash-ref snap 'calls-since-mutation)
                                                (format "W~a" wave-idx)
                                                campaign-id
                                                task-line
                                                target-files))]
                      [(hard-stall)
                       (define snap (stall-watchdog-snapshot watchdog))
                       (log-error
                        "gsd: wave ~a hard stall (~a calls, no mutation, reason ~a) — failing attempt"
                        wave-idx
                        (hash-ref snap 'calls-since-mutation)
                        (hash-ref snap 'stall-reason 'unknown))
                       (raise (make-gsd-stall-exn (stall-hard-failure-message
                                                   (hash-ref snap 'calls-since-mutation)
                                                   (or hard-limit 0)
                                                   target-files
                                                   (hash-ref snap 'stall-tool #f)
                                                   (hash-ref snap 'recent-tools '()))))]
                      [else (void)]))])
              (run-one-fn idx)))))))

;; ============================================================
;; No-change rejection retry (v1.00.17 W3 — #9515)
;; ============================================================

;; Prefix of the delivery-verifier message emitted when a wave finished but
;; ZERO declared target files changed (delivery-verifier.rkt: "no wave target
;; files changed: f1, f2, ..."). Only this verdict gets the bounded
;; failure-context retry: a plain "verifier rejected" (empty message) or any
;; other rejection still fails the wave on the first attempt.
;; BUG-0037 W1 follow-up (live campaign evidence, v1.00.20 W2 attempt 1):
;; the executor session's tool loop catches the gsd-stall-exn INSIDE the
;; worker and converts it to a loop-result 'error termination carrying the
;; stall message — so the gsd-stall-exn? handlers at the runner boundary
;; never fire and the death classified as a plain 'failed. Recognize the
;; canonical prefix here and route it to the retryable infra-failed path.
(define stall-cause-prefix "mutation-stall watchdog:")

(define (stall-cause-message? msg)
  (and (string? msg)
       (>= (string-length msg) (string-length stall-cause-prefix))
       (string-prefix? msg stall-cause-prefix)))

(define no-change-rejection-prefix "no wave target files changed")

(define (no-change-rejection? verifier-message)
  (and (string? verifier-message)
       (>= (string-length verifier-message) (string-length no-change-rejection-prefix))
       (string-prefix? verifier-message no-change-rejection-prefix)))

;; ============================================================
;; Campaign-level infra-failure retry (v1.00.18 BUG-0024 W3)
;; ============================================================

;; Hard cap on the durable prior-attempt context (~2 KB per the wave spec).
(define attempt-context-max-chars 2048)

;; Build the ≤2 KB prior-attempt context captured into the campaign record's
;; attempt-context field when an executor session dies on an infra failure.
;; The next (automatic) attempt of the SAME wave gets this prepended to its
;; prompt, so it resumes from recorded context instead of re-exploring from
;; zero (~12 duplicated tool calls observed per restart in the v1.00.17
;; campaign).
(define (build-wave-attempt-context wave-idx attempt error-message)
  (define raw
    (format (string-append "Prior attempt ~a of wave W~a ended in an INFRASTRUCTURE failure "
                           "(provider/network), not a logic failure. Work already done lives on "
                           "the attempt branch — check git status / git diff there before writing "
                           "anything.\nLast executor error: ~a\n"
                           "Resume from that state; do NOT restart exploration from zero.")
            attempt
            wave-idx
            error-message))
  (substring raw 0 (min (string-length raw) attempt-context-max-chars)))

;; ============================================================
;; Mid-wave dirty-state hand-off + drift guard (v1.00.20 W4 — BUG-0030)
;;
;; Complements the mid-wave CHECKPOINT contract (wave-executor.rkt):
;; checkpoints make progress durable, but an attempt can still die with
;; uncommitted residue in its worktree. When it does, the orchestrator
;; captures that dirty state into the campaign record — joining the
;; PRIOR ATTEMPT CONTEXT block above (same carrier, same ~2 KB cap) —
;; so the retry resumes from a recorded, recoverable snapshot instead
;; of re-deriving context. The coordinator also guards the OTHER
;; failure mode: uncommitted .rkt drift in the main checkout OUTSIDE
;; the active attempt lease/worktree (exactly what polluted PR #9529's
;; metrics twice with "expected 158320, found 158324").
;; ============================================================

;; One porcelain line ("XY <path>" or "XY <old> -> <new>") → the path the
;; change lands on (new name for renames, quotes stripped). Pure.
(define (porcelain-file-path line)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define rest (substring line 3))
    (define parts (string-split rest " -> "))
    (define raw
      (string-trim (if (= (length parts) 2)
                       (cadr parts)
                       (car parts))))
    (define unquoted
      (if (and (>= (string-length raw) 2) (string-prefix? raw "\"") (string-suffix? raw "\""))
          (substring raw 1 (sub1 (string-length raw)))
          raw))
    (and (non-empty-string? unquoted) unquoted)))

;; Capture the dirty state of a dying attempt's worktree as pure data:
;;   dirty-sha    — `git stash create` output: the SHA of a REAL commit
;;                  object holding the uncommitted tracked changes
;;                  ("dirty-sha-if-committed"; recoverable via that SHA
;;                  without moving HEAD or touching the index), or #f
;;                  when clean / untracked-only (stash create ignores
;;                  untracked files — the file list still names them)
;;   diff-stat    — `git diff --stat` vs HEAD ("" when clean)
;;   edited-files — files with uncommitted changes, tracked + untracked
;;                  (git-relative)
;; Never raises: any git failure degrades to the "clean" shape.
(define (capture-worktree-dirty-state wt)
  (define clean (hasheq 'dirty-sha #f 'diff-stat "" 'edited-files '()))
  (with-handlers ([exn:fail? (lambda (_) clean)])
    (define dir (wave-worktree-path wt))
    (define r-status (default-run-git dir (list "status" "--porcelain")))
    (define dirty-lines
      (if (zero? (git-result-code r-status))
          (filter non-empty-string? (string-split (git-result-stdout r-status) "\n"))
          '()))
    (cond
      [(null? dirty-lines) clean]
      [else
       (define edited-files
         (filter values
                 (for/list ([line (in-list dirty-lines)])
                   (porcelain-file-path line))))
       (define r-diff (default-run-git dir (list "diff" "--stat")))
       (define diff-stat
         (if (zero? (git-result-code r-diff))
             (string-trim (git-result-stdout r-diff))
             ""))
       (define r-stash (default-run-git dir (list "stash" "create")))
       (define dirty-sha
         (let ([sha (and (zero? (git-result-code r-stash))
                         (string-trim (git-result-stdout r-stash)))])
           (and (non-empty-string? sha) sha)))
       (hasheq 'dirty-sha dirty-sha 'diff-stat diff-stat 'edited-files edited-files)])))

;; Take at most n elements of lst (racket/base-only helper).
(define (take-up-to lst n)
  (for/list ([x (in-list lst)]
             [i (in-naturals)]
             #:break (>= i n))
    x))

;; Append the captured dirty state to a base attempt-context (the BUG-0024
;; PRIOR ATTEMPT CONTEXT block), hard-capped at attempt-context-max-chars.
;; Pure. A clean capture appends nothing — no noise for clean restarts.
(define (append-dirty-capture-to-context base-context capture)
  (define dirty-sha (and capture (hash-ref capture 'dirty-sha #f)))
  (define diff-stat (and capture (hash-ref capture 'diff-stat "")))
  (define edited-files (and capture (hash-ref capture 'edited-files '())))
  (define has-dirt?
    (or dirty-sha
        (and (string? diff-stat) (non-empty-string? diff-stat))
        (and (list? edited-files) (pair? edited-files))))
  (define raw
    (if (not has-dirt?)
        base-context
        (string-append
         base-context
         "\n"
         "Dirty state captured at infra-stop (BUG-0030):\n"
         (format "- dirty-sha-if-committed: ~a\n"
                 (or dirty-sha "none (clean or untracked-only residue)"))
         (format "- diff-summary-stat: ~a\n"
                 (string-join (take-up-to (string-split (or diff-stat "") "\n") 3) " | "))
         (format "- edited-files: ~a\n" (string-join (take-up-to (or edited-files '()) 12) ", ")))))
  (substring raw 0 (min (string-length raw) attempt-context-max-chars)))

;; Uncommitted .rkt files in `repo-root`, excluding an explicit exempt
;; list (the dying/resumed attempt's own recorded files) — pure
;; detection, never raises, never mutates.
(define (outside-lease-dirty-rkt-files repo-root #:exempt [exempt '()])
  (with-handlers ([exn:fail? (lambda (_) '())])
    (and
     repo-root
     (let ([r (default-run-git repo-root (list "status" "--porcelain"))])
       (if (not (zero? (git-result-code r)))
           '()
           (filter
            values
            (for/list ([line (in-list (filter non-empty-string?
                                              ;; BUG-0030: do NOT trim the whole stdout —
                                              ;; porcelain's first line is " M file"; a global
                                              ;; trim eats that leading space and shifts
                                              ;; every column by one ("racked.rkt").
                                              (string-split (git-result-stdout r) "\n" #:trim? #f)))])
              (define f (porcelain-file-path line))
              (and f (non-empty-string? f) (string-suffix? f ".rkt") (not (member f exempt)) f))))))))

;; Coordinator-side drift guard (BUG-0030 action 4): BEFORE an attempt
;; starts, detect uncommitted .rkt changes in the main checkout OUTSIDE
;; the active attempt lease/worktree and build the LOUD warning naming
;; them. This is exactly what would have caught both PR #9529
;; metrics-drift incidents. Pure: it NEVER auto-commits, auto-discards,
;; or blocks the attempt — call sites log it loudly and continue.
(define (outside-lease-dirty-warning repo-root
                                     #:worktree-path [worktree-path #f]
                                     #:exempt [exempt '()])
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (and repo-root
         (let ([wt-prefix (and worktree-path
                               (if (string? worktree-path)
                                   worktree-path
                                   (path->string worktree-path)))])
           (define files
             (filter (lambda (f) (not (and wt-prefix (string-prefix? f wt-prefix))))
                     (outside-lease-dirty-rkt-files repo-root #:exempt exempt)))
           (and (pair? files)
                (format (string-append
                         "UNCOMMITTED DRIFT (BUG-0030): ~a uncommitted .rkt change(s) in the main "
                         "checkout OUTSIDE the active attempt lease/worktree: ~a. These will NOT be "
                         "auto-committed or auto-discarded — review them before they pollute metrics "
                         "or successor attempts (cf. PR #9529 metrics-drift incidents).")
                        (length files)
                        (string-join (take-up-to files 15) ", ")))))))

;; Effectful half of the guard: log the warning loudly, return it (tests
;; observe the string; operations observe the log).
(define (warn-outside-lease-dirty-state! repo-root
                                         #:worktree-path [worktree-path #f]
                                         #:exempt [exempt '()])
  (define warning
    (outside-lease-dirty-warning repo-root #:worktree-path worktree-path #:exempt exempt))
  (when warning
    (log-warning "gsd: ~a" warning))
  warning)

;; Count commits on `branch` since `base-commit` (repo-root-relative);
;; #f when git fails (never raises).
(define (wave-branch-commit-count repo-root base-commit branch)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (and repo-root
         base-commit
         branch
         (let ([r (default-run-git repo-root
                                   (list "rev-list" "--count" (format "~a..~a" base-commit branch)))])
           (and (zero? (git-result-code r)) (string->number (string-trim (git-result-stdout r))))))))

;; BUG-0030 action 2 (tolerance half): delivery verification checks
;; FILES/TARGETS, never commit count — a delivered branch may carry N
;; checkpoint commits plus the final state. A DONE wave with ZERO commits
;; on its delivery branch is nonsensical (an empty diff cannot pass), so
;; warn — never fail.
(define (warn-zero-commit-delivery-branch! delivery-ctx)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (and
     delivery-ctx
     (let* ([repo (branch-delivery-context-ref delivery-ctx 'repo-root)]
            [base (branch-delivery-context-ref delivery-ctx 'base-commit)]
            [branch (branch-delivery-context-ref delivery-ctx 'branch)]
            [n (and repo base branch (wave-branch-commit-count repo base branch))])
       (and
        (equal? n 0)
        (log-warning
         "gsd: DONE wave delivered on ~a with ZERO commits since ~a — expected at least the delivery commit"
         branch
         base)
        #t)))))

;; Best-effort observability: every automatic retry emits
;; gsd.campaign.infra-retry (payload: wave idx, attempt, delay seconds).
;; A bus failure must never break the retry loop itself.
(define (emit-infra-retry-event! wave-idx attempt delay-secs)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning "gsd: infra-retry event emission failed: ~a"
                                            (exn-message e)))])
    (emit-gsd-event! 'gsd.campaign.infra-retry
                     (hasheq 'wave wave-idx 'attempt attempt 'delay delay-secs))))

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

;; "no wave target files changed: f1, f2" → '("f1" "f2"). The verifier
;; comma-space-joins the declared targets into its message; recover the list
;; so the retry prompt can name the files explicitly.
(define (no-change-target-files verifier-message)
  (define body
    (substring verifier-message
               (min (string-length verifier-message)
                    (add1 (string-length no-change-rejection-prefix)))))
  (filter non-empty-string? (map string-trim (string-split body ","))))

;; ============================================================
;; Find wave helper
;; ============================================================

(define (find-wave rec wave-idx)
  (for/first ([w (campaign-record-waves rec)]
              #:when (= (campaign-wave-index w) wave-idx))
    w))

;; ============================================================
;; v1.00.17 W7 (#9512b): branch-based delivery bookkeeping
;;
;; With worktree isolation ON, delivery = COMMITTED diff on the wave branch
;; (campaign/<hash8>/w<N>) against its base (origin/main at attempt start).
;; Uncommitted worktree dirt never satisfies delivery. Before verification
;; the coordinator auto-commits remaining worktree changes with a
;; deterministic message; after approval it records branch + head SHA in the
;; campaign record. Merge/PR stays OUTSIDE the coordinator (no silent
;; auto-merge in v1.00.17).
;; ============================================================

;; Deterministic commit message: feat(<campaign-hash8>/w<N>): <wave title>.
;; Same (campaign, wave, title) → same message, every attempt.
(define (wave-delivery-commit-message campaign-id wave-idx wave-title)
  (format "feat(~a/w~a): ~a" (worktree-hash8 campaign-id) wave-idx wave-title))

;; Commit any uncommitted changes in the wave worktree (git add -A + commit
;; with a hermetic identity so no global git config is required). Best-effort
;; and logged, never raises: a failed commit simply leaves the changes
;; uncommitted, and the branch-diff verifier then rejects with the honest
;; "no wave target files changed" verdict. "nothing to commit" is a no-op
;; success (empty diff at this point means the verifier's no-change path
;; fires — now meaning what it says).
(define (commit-wave-worktree! wt campaign-id wave-idx wave-title)
  (define dir (wave-worktree-path wt))
  (define dir-str
    (if (string? dir)
        dir
        (path->string dir)))
  (define msg (wave-delivery-commit-message campaign-id wave-idx wave-title))
  ;; Locale-independent clean-tree detection: `git status --porcelain` is
  ;; machine-readable in every locale (the human-facing "nothing to commit"
  ;; hint is NOT — it is translated, e.g. "nichts zum Commit vorgemerkt").
  (define r-status (default-run-git dir-str (list "status" "--porcelain")))
  (define clean-tree?
    (and (zero? (git-result-code r-status)) (string=? (string-trim (git-result-stdout r-status)) "")))
  (cond
    [clean-tree?
     (log-info "commit-wave-worktree!: nothing to commit in ~a" dir-str)
     #t]
    [else
     (define r-add (default-run-git dir-str (list "add" "-A")))
     (unless (zero? (git-result-code r-add))
       (log-warning "commit-wave-worktree!: git add -A failed in ~a: ~a"
                    dir-str
                    (string-trim (git-result-stderr r-add))))
     (define r-commit
       (default-run-git dir-str
                        (list "-c"
                              "user.name=gsd-coordinator"
                              "-c"
                              "user.email=coordinator@gsd.local"
                              "commit"
                              "-m"
                              msg)))
     (cond
       [(zero? (git-result-code r-commit)) #t]
       ;; Exit 1 with a "nothing to commit" hint (English locale) — clean
       ;; tree, nothing to deliver. Non-English locales are caught by the
       ;; porcelain check above.
       [(regexp-match? #rx"nothing to commit" (git-result-stdout r-commit))
        (log-info "commit-wave-worktree!: nothing to commit in ~a" dir-str)
        #t]
       [else
        (log-warning "commit-wave-worktree!: git commit failed in ~a: ~a"
                     dir-str
                     (string-trim (git-result-stderr r-commit)))
        #f])]))

;; Resolve the base commit SHA for the branch diff (the ref the worktree was
;; created from, captured at attempt start). #f when resolution fails —
;; callers treat that as "no branch context available" and fall back to the
;; legacy working-tree check.
(define (wave-worktree-base-commit wt)
  (define r
    (default-run-git (wave-worktree-repo-root wt) (list "rev-parse" (wave-worktree-base-ref wt))))
  (and (zero? (git-result-code r))
       (non-empty-string? (git-result-stdout r))
       (string-trim (git-result-stdout r))))

;; Head SHA of the wave branch (recorded at approval time).
(define (wave-worktree-head-sha wt)
  (define r
    (default-run-git (wave-worktree-repo-root wt) (list "rev-parse" (wave-worktree-branch wt))))
  (and (zero? (git-result-code r))
       (non-empty-string? (git-result-stdout r))
       (string-trim (git-result-stdout r))))

;; The branch-context the delivery verifier reads via
;; current-gsd-delivery-branch-context, or #f when any ingredient cannot be
;; resolved (then verification stays legacy).
(define (wave-worktree-delivery-context wt)
  (define base-commit (wave-worktree-base-commit wt))
  (and base-commit
       (make-branch-delivery-context #:repo-root (wave-worktree-repo-root wt)
                                     #:branch (wave-worktree-branch wt)
                                     #:base-commit base-commit
                                     #:worktree-path (wave-worktree-path wt))))

;; Record delivery provenance in the durable campaign record and persist.
;; Observes a FRESH record (the completion may have already mutated the
;; durable state), so the delivery fields can never resurrect a stale wave.
(define (record-wave-delivery! base-dir plan-id wave-idx branch head-sha)
  (define rec (load-campaign-record base-dir plan-id))
  (define wave (and rec (find-wave rec wave-idx)))
  (cond
    [(not wave)
     (log-warning "record-wave-delivery!: wave ~a not found in campaign ~a" wave-idx plan-id)]
    [(not head-sha) (log-warning "record-wave-delivery!: could not resolve head SHA for ~a" branch)]
    [else
     (set-campaign-wave-delivery-branch! wave branch)
     (set-campaign-wave-delivery-head-sha! wave head-sha)
     (persist-campaign! base-dir rec)
     (log-info "record-wave-delivery!: wave ~a delivered on ~a @ ~a" wave-idx branch head-sha)]))

;; ============================================================
;; v1.00.21 W5 (BUG-0029): attempt-artifact ledger + reclaim.
;; Failed/killed attempts used to leave durable artifacts (delivery
;; branches, per-attempt worktrees) that were never reconciled, so
;; successor attempts burned context on git archaeology. Every attempt
;; that creates a worktree now gets a ledger entry AT CREATION; terminal
;; transitions and teardown update it; campaign end reports leftovers
;; with an operator-approved reclaim offer. Nothing is deleted
;; automatically — ever.
;; ============================================================

;; Normalize a git helper result to its stdout string (default-run-git
;; returns a plain string in the simple paths and a git-result struct in
;; others; both shapes occur across call sites).
(define (git-out->string out)
  (cond
    [(string? out) out]
    [(bytes? out) (bytes->string/utf-8 out #:error-char #\?)]
    [else
     (with-handlers ([exn:fail? (lambda (_) (format "~a" out))])
       (git-result-stdout out))]))

;; #t when the git helper result indicates exit success (strings — which
;; only occur on the raising-free happy paths — count as success).
(define (git-ok? out)
  (with-handlers ([exn:fail? (lambda (_) #t)])
    (<= (git-result-code out) 0)))

;; Locally-determinable merge status of a terminal attempt branch:
;;   'deleted  branch no longer exists (teardown removed it)
;;   'merged   branch tip is an ancestor of origin/main
;;   'unmerged branch exists but is not on origin/main yet
;;   'unknown  git could not answer (never a failure path)
(define (artifact-merge-status/local repo-root branch)
  (define repo
    (if (string? repo-root)
        repo-root
        (path->string repo-root)))
  (with-handlers ([exn:fail? (lambda (_) 'unknown)])
    (define sha
      (with-handlers ([exn:fail? (lambda (_) "")])
        (string-trim
         (git-out->string
          (default-run-git repo (list "rev-parse" "--verify" (string-append branch "^{commit}")))))))
    (cond
      [(not (non-empty-string? sha)) 'deleted]
      [(let ([merged (with-handlers ([exn:fail? (lambda (_) #f)])
                       (default-run-git repo
                                        (list "merge-base" "--is-ancestor" branch "origin/main")))])
         (and merged (git-ok? merged)))
       'merged]
      [else 'unmerged])))

;; Append a creation entry for a freshly created attempt worktree: the
;; worktree+branch pair is a durable artifact and is owned by the record
;; from its first breath. Best-effort: bookkeeping never kills a campaign.
(define (record-attempt-artifact! base-dir plan-id wave-idx attempt-id wt)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning "record-attempt-artifact!: ~a" (exn-message e)))])
    (define rec (load-campaign-record base-dir plan-id))
    (define wave (and rec (find-wave rec wave-idx)))
    (cond
      [(not wave)
       (log-warning "record-attempt-artifact!: wave ~a not found in campaign ~a" wave-idx plan-id)]
      [else
       (define wt-path (wave-worktree-path wt))
       (define wt-path-str
         (if (string? wt-path)
             wt-path
             (path->string wt-path)))
       (define base-sha
         (with-handlers ([exn:fail? (lambda (_) #f)])
           (string-trim (git-out->string (default-run-git (wave-worktree-repo-root wt)
                                                          (list "rev-parse"
                                                                (wave-worktree-base-ref wt)))))))
       (set-campaign-wave-artifact-ledger!
        wave
        (append (wave-artifact-ledger wave)
                (list (make-campaign-artifact-entry attempt-id
                                                    (wave-worktree-branch wt)
                                                    wt-path-str
                                                    (if (non-empty-string? base-sha) base-sha "")))))
       (persist-campaign! base-dir rec)
       (log-info "gsd: ledger — wave ~a attempt ~a created branch ~a (base ~a)"
                 wave-idx
                 (substring attempt-id 0 (min 10 (string-length attempt-id)))
                 (wave-worktree-branch wt)
                 (if (non-empty-string? base-sha) base-sha "?"))])))

;; Mark the ledger entry of a finished attempt terminal. No-op when the
;; attempt never created an artifact (shared-checkout fallback): an
;; attempt that owns nothing has nothing to mark.
(define (mark-attempt-artifact-terminal! base-dir
                                         plan-id
                                         wave-idx
                                         attempt-id
                                         terminal-status
                                         #:merge-status [merge-status #f])
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning "mark-attempt-artifact-terminal!: ~a" (exn-message e)))])
    (define rec (load-campaign-record base-dir plan-id))
    (define wave (and rec (find-wave rec wave-idx)))
    (when (and wave attempt-id)
      (for ([entry (in-list (wave-artifact-ledger wave))])
        (when (string=? (campaign-artifact-entry-attempt-id entry) attempt-id)
          (set-campaign-artifact-entry-terminal-status! entry terminal-status)
          (when merge-status
            (set-campaign-artifact-entry-merge-status! entry merge-status))
          (persist-campaign! base-dir rec)
          (log-info "gsd: ledger — wave ~a attempt ~a terminal:~a~a"
                    wave-idx
                    (substring attempt-id 0 (min 10 (string-length attempt-id)))
                    terminal-status
                    (if merge-status
                        (format " merge:~a" merge-status)
                        "")))))))

;; Record the best-effort teardown outcome of the final attempt's
;; worktree (action 4): teardown failures are logged INTO the ledger,
;; never silently skipped.
(define (record-attempt-teardown! base-dir plan-id wave-idx attempt-id wt removed?)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning "record-attempt-teardown!: ~a" (exn-message e)))])
    (define rec (load-campaign-record base-dir plan-id))
    (define wave (and rec (find-wave rec wave-idx)))
    (when (and wave attempt-id)
      (for ([entry (in-list (wave-artifact-ledger wave))])
        (when (string=? (campaign-artifact-entry-attempt-id entry) attempt-id)
          (set-campaign-artifact-entry-teardown-status! entry (if removed? 'removed 'left-on-disk))
          (persist-campaign! base-dir rec)
          (log-info "gsd: ledger — wave ~a attempt ~a teardown:~a"
                    wave-idx
                    (substring attempt-id 0 (min 10 (string-length attempt-id)))
                    (if removed? 'removed 'left-on-disk)))))))

;; v1.00.21 W5 (BUG-0029 action 3): end-of-campaign reclaim report.
;; Enumerates NON-DELIVERY leftovers across the campaign — ledger entries
;; whose worktree directory still exists on disk or whose branch still
;; exists while the wave recorded no delivery on that branch — and prints
;; an operator-visible summary with an explicit reclaim offer. NEVER
;; deletes anything: reclamation is operator-approved only (/go gc or the
;; printed command list).
(define LEFTOVERS-REPORT-MAX-ENTRIES 20)
(define (report-campaign-artifact-leftovers! base-dir plan-id #:repo-root repo-root)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning "gsd: leftovers report failed: ~a" (exn-message e)))])
    (define rec (load-campaign-record base-dir plan-id))
    (define repo
      (and repo-root
           (if (string? repo-root)
               repo-root
               (path->string repo-root))))
    (define leftovers
      (for*/list ([wave (in-list (and rec (campaign-record-waves rec)))]
                  [entry (in-list (wave-artifact-ledger wave))]
                  #:when (not (and (string? (campaign-wave-delivery-branch wave))
                                   (non-empty-string? (campaign-wave-delivery-branch wave))
                                   (string=? (campaign-wave-delivery-branch wave)
                                             (campaign-artifact-entry-branch entry)))))
        (list wave entry)))
    (define live
      (for/list ([we (in-list leftovers)])
        (define entry (cadr we))
        (define dir-left?
          (and (non-empty-string? (campaign-artifact-entry-worktree-path entry))
               (directory-exists? (campaign-artifact-entry-worktree-path entry))))
        (define branch-live?
          (and repo
               (not (eq? 'deleted
                         (artifact-merge-status/local repo (campaign-artifact-entry-branch entry))))))
        (and (or dir-left? branch-live?) (list (car we) entry dir-left? branch-live?))))
    (define rows
      (for/list ([r (in-list live)]
                 #:when r)
        r))
    (cond
      [(null? rows)
       (log-info "gsd: campaign ~a — no leftover attempt artifacts (all reclaimed or delivered)"
                 plan-id)]
      [else
       (define shown
         (for/list ([r (in-list rows)]
                    [i (in-naturals)]
                    #:when (< i LEFTOVERS-REPORT-MAX-ENTRIES))
           r))
       (define commands
         (string-join (for/list ([r (in-list shown)])
                        (define entry (list-ref r 1))
                        (string-append (if (list-ref r 2)
                                           (format "  git -C ~a worktree remove --force ~a\n"
                                                   repo
                                                   (campaign-artifact-entry-worktree-path entry))
                                           "")
                                       (if (list-ref r 3)
                                           (format "  git -C ~a branch -D ~a"
                                                   repo
                                                   (campaign-artifact-entry-branch entry))
                                           "")))
                      "\n"))
       (define summary
         (string-append (format "\n=== GSD CAMPAIGN ~a: LEFTOVER ATTEMPT ARTIFACTS (~a) ===\n"
                                plan-id
                                (length rows))
                        "These artifacts were NOT delivered and are still on disk/in git.\n"
                        "NOTHING has been deleted. To reclaim (operator-approved ONLY), run:\n"
                        "  /go gc\n"
                        "or manually:\n"
                        commands
                        (if (> (length rows) (length shown))
                            (format "\n  (+~a more — see the campaign record ledger)\n"
                                    (- (length rows) (length shown)))
                            "\n")
                        "=== END LEFTOVER ATTEMPT ARTIFACTS ===\n"))
       (log-warning
        "gsd: ~a leftover attempt artifact(s) after campaign ~a — reclaim offer printed (NO auto-delete)"
        (length rows)
        plan-id)
       (displayln summary)])))

;; Delivered branches of a record — the spare-list handed to campaign-start
;; reclaim so crash recovery never destroys durable merge evidence (W7).
(define (record-delivered-branches rec)
  (for/list ([w (in-list (campaign-record-waves rec))]
             #:when (non-empty-string? (campaign-wave-delivery-branch w)))
    (campaign-wave-delivery-branch w)))

(define (mirror-durable-statuses! target durable)
  (when durable
    (for ([durable-wave (campaign-record-waves durable)])
      (define target-wave (find-wave target (campaign-wave-index durable-wave)))
      (when target-wave
        (set-campaign-wave-status! target-wave (campaign-wave-status durable-wave))))))

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

;; Pure status value. running-version  : the RUNNING process's (q-version)
;; checkout-version    : freshly-read util/version.rkt version, or #f when
;;                       no repo checkout is resolvable (then nothing can
;;                       diverge and the run proceeds — legacy behavior)
;; origin-head         : origin/main HEAD SHA at /go time, or #f (offline /
;;                       no such ref / outside a work tree — never fatal)
;; behind-origin?      : #t when the checkout HEAD is a strict ancestor of
;;                       origin/main (checkout is out of date)
;; offline?            : #t when origin/main could not be resolved → the
;;                       operator is warned but NEVER blocked
(struct campaign-freshness (running-version checkout-version origin-head behind-origin? offline?)
  #:transparent)

;; Short aliases used by the guard, the /go entry path, and the tests.
(define freshness-running-version campaign-freshness-running-version)
(define freshness-checkout-version campaign-freshness-checkout-version)
(define freshness-origin-head campaign-freshness-origin-head)
(define freshness-behind-origin? campaign-freshness-behind-origin?)
(define freshness-offline? campaign-freshness-offline?)

;; util/version.rkt is `(define q-version "1.00.19")`. Read FRESH from disk —
;; the module binding in this process is exactly the thing that may be stale.
(define FRESHNESS-VERSION-RX #rx"q-version[ \t]*\"([^\"]+)\"")

(define (read-checkout-build-version repo-root)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (and repo-root
         (let ([version-file (build-path repo-root "util" "version.rkt")])
           (and (file-exists? version-file)
                (let ([m (regexp-match FRESHNESS-VERSION-RX
                                       (file->string version-file #:mode 'text))])
                  (and m (cadr m))))))))

(define (resolve-origin-main-head repo-root)
  (and repo-root
       (let ([r (default-run-git repo-root '("rev-parse" "origin/main"))])
         (and (zero? (git-result-code r))
              (non-empty-string? (git-result-stdout r))
              (string-trim (git-result-stdout r))))))

;; Strictly-behind = HEAD is an ancestor of origin/main AND differs from it.
;; A campaign branch with its own commits, or a checkout equal to main, is
;; NOT behind — the guard never blocks legitimate forward work.
(define (checkout-behind-origin-main? repo-root origin-head)
  (and repo-root
       origin-head
       (let* ([head-r (default-run-git repo-root '("rev-parse" "HEAD"))]
              [head (and (zero? (git-result-code head-r))
                         (non-empty-string? (git-result-stdout head-r))
                         (string-trim (git-result-stdout head-r)))])
         (and head
              (not (string=? head origin-head))
              (zero? (git-result-code
                      (default-run-git repo-root
                                       (list "merge-base" "--is-ancestor" head origin-head))))))))

;; The authoritative /go-time check. Pure w.r.t. injected ingredients so
;; tests can simulate a stale build without mutating the real checkout.
(define (check-campaign-freshness base-dir
                                  #:running-version [running-version q-version]
                                  #:repo-root [repo-root (find-repo-root base-dir)])
  (define checkout-version (read-checkout-build-version repo-root))
  (define origin-head (resolve-origin-main-head repo-root))
  (campaign-freshness running-version
                      checkout-version
                      origin-head
                      (checkout-behind-origin-main? repo-root origin-head)
                      (not origin-head)))

;; Stale = running version ≠ checkout version (authoritative), or the
;; checkout itself is behind origin/main. Unknown checkout (#f) or offline
;; origin NEVER counts as stale — the guard fails open there.
(define (freshness-stale? f)
  (and (campaign-freshness? f)
       (or (and (freshness-checkout-version f)
                (not (string=? (freshness-running-version f) (freshness-checkout-version f))))
           (freshness-behind-origin? f))))

(define (freshness-refusal-message f)
  (cond
    [(and (freshness-checkout-version f)
          (not (string=? (freshness-running-version f) (freshness-checkout-version f))))
     (format
      (string-append
       "/go refused — restart required (running ~a, checkout ~a): the running q "
       "process predates the checked-out build. Exit and restart q, then /go "
       "again. To override anyway: /go <plan> allow-stale (records stale-override: true in the campaign record).")
      (freshness-running-version f)
      (freshness-checkout-version f))]
    [(freshness-behind-origin? f)
     (format
      (string-append "/go refused — update required: checkout HEAD is behind origin/main (~a). "
                     "Run git pull and restart q, then /go again. To override anyway: "
                     "/go <plan> allow-stale (records stale-override: true in the campaign record).")
      (freshness-origin-head f))]
    [else "/go refused — running build is stale."]))

;; Offline operators are warned, never blocked (BUG-0031 action 4).
(define (freshness-offline-warning f)
  (and
   (freshness-offline? f)
   (format
    "gsd freshness: origin/main unreachable — continuing with checkout-only version comparison (running ~a)."
    (freshness-running-version f))))

;; Stamp build identity onto a campaign record (idempotent re-stamp is fine —
;; the running build IS the identity). stale-override is owned by the guard
;; decision and is never cleared here.
(define (stamp-campaign-build-identity! rec base-dir)
  (define repo-root (find-repo-root base-dir))
  (set-campaign-record-build-version! rec q-version)
  (set-campaign-record-main-head-sha! rec (resolve-origin-main-head repo-root))
  rec)

;; Injection point for tests: replace the /go-entry check without touching
;; the real checkout or network.
(define current-gsd-freshness-check (make-parameter check-campaign-freshness))

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
  ;; Each accessor already warns-and-defaults on invalid values, so a
  ;; typo'd settings file can never crash a campaign mid-wave.
  (define effective-stall-soft-limit
    (if (eq? stall-soft-limit 'unset)
        (or (gsd-stall-soft-limit (load-project-settings-silently base-dir)) STALL-SOFT-LIMIT-DEFAULT)
        stall-soft-limit))
  (define effective-stall-hard-limit
    (if (eq? stall-hard-limit 'unset)
        (or (gsd-stall-hard-limit (load-project-settings-silently base-dir)) STALL-HARD-LIMIT-DEFAULT)
        stall-hard-limit))
  (define effective-stall-window
    (if (eq? stall-window 'unset)
        (or (gsd-stall-window (load-project-settings-silently base-dir))
            STALL-REPETITION-WINDOW-DEFAULT)
        stall-window))
  (define effective-stall-backstop
    (if (eq? stall-backstop 'unset)
        (or (gsd-stall-backstop (load-project-settings-silently base-dir))
            STALL-BACKSTOP-LIMIT-DEFAULT)
        stall-backstop))
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
       (define (mirror-status! status)
         (define caller-wave (find-wave rec wave-idx))
         (when caller-wave
           (set-campaign-wave-status! caller-wave status)))
       (define (persist-current-status! status)
         (define observed (observe))
         (define observed-wave (current-wave-for-attempt observed wave-idx fence expected-id))
         (and observed-wave
              (begin
                (set-campaign-wave-status! observed-wave status)
                (persist-campaign! base-dir observed)
                (mirror-status! status)
                observed)))
       (define (interrupt-current! message)
         (persist-current-status! 'interrupted)
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
         (parameterize ([current-gsd-wave-inherited-artifacts inherited-artifact-text])
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
          (interrupt-current! (campaign-budget-pause-message
                               (campaign-record-budget-pause after-run)))]
         [(and after-run (campaign-record-cancellation after-run))
          (mark-attempt-artifact-terminal! base-dir
                                           (campaign-plan-id active)
                                           wave-idx
                                           expected-id
                                           'cancelled)
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
                           (try-complete-wave! base-dir
                                               after-verifier
                                               wave-idx
                                               #:verifier-approve? approved?
                                               #:verifier-message verifier-message
                                               #:expected-attempt-id expected-id
                                               #:expected-fence-token fence))
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
                               (campaign-result 'wave-failed
                                                '()
                                                (if (string=? verifier-message "")
                                                    "verifier rejected"
                                                    verifier-message))])]
                           [(stale-attempt invalid-state)
                            (campaign-result 'wave-cancelled '() "stale completion ignored")]
                           [else (campaign-result 'wave-failed '() "unexpected completion state")])])))])]
            [(failed)
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
             (if (persist-current-status! 'failed)
                 (begin
                   (apply-wave-status-projections! base-dir
                                                   wave-idx
                                                   STATUS-FAILED
                                                   (lambda (idx) (wave-slug base-dir idx)))
                   (campaign-result 'wave-failed '() "runner error"))
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
             (set-box! infra-retry-state
                       (cons (sub1 (car (unbox infra-retry-state)))
                             (append (cdr (unbox infra-retry-state))
                                     (list (list (or failed-attempt 0) (current-seconds))))))
             (define infra-retries-left (car (unbox infra-retry-state)))
             ;; retries-left is the budget AFTER this failure; a bound of N
             ;; permits N automatic re-attempts, so re-enter while the budget
             ;; is non-negative (0 = one last retry; -1 = exhausted).
             (if (>= infra-retries-left 0)
                 (let* ([this-retry (- (current-gsd-campaign-infra-retries) infra-retries-left)]
                        [delay-secs ((current-gsd-campaign-infra-retry-delay) this-retry)])
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
                   (campaign-result
                    'wave-cancelled
                    '()
                    (format (string-append
                             "provider/network failure persisted after ~a automatic retries "
                             "(attempt not consumed); re-run /go when the provider is healthy. "
                             "Failures: ~a")
                            (current-gsd-campaign-infra-retries)
                            (string-join (for/list ([f (cdr (unbox infra-retry-state))])
                                           (format "attempt ~a at ~a" (car f) (cadr f)))
                                         "; ")))))]
            [(cancelled interrupted)
             (mark-attempt-artifact-terminal! base-dir
                                              (campaign-plan-id active)
                                              wave-idx
                                              expected-id
                                              'interrupted)
             (interrupt-current! (wave-execution-outcome-message run-result))]
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
             (emit-wave-outcome-error! wave-idx
                                       'timed-out
                                       (wave-execution-outcome-message run-result))
             (mark-attempt-artifact-terminal! base-dir
                                              (campaign-plan-id active)
                                              wave-idx
                                              expected-id
                                              'interrupted)
             (interrupt-current! (if (> timeout-retries 0)
                                     (format "~a after ~a retries"
                                             (wave-execution-outcome-message run-result)
                                             timeout-retries)
                                     (wave-execution-outcome-message run-result)))]
            [else
             ;; BUG-0043 (W2): unknown terminal outcome — same typed error
             ;; transcript routing as the named failure branches.
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
             (if (persist-current-status! 'failed)
                 (begin
                   (apply-wave-status-projections! base-dir
                                                   wave-idx
                                                   STATUS-FAILED
                                                   (lambda (idx) (wave-slug base-dir idx)))
                   (campaign-result 'wave-failed '() "unknown runner outcome"))
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

;; BUG-0028 S1 (v1.00.19 W2): best-effort project-settings load for the
;; gsd.worktree-isolation wiring at the composition root. NEVER raises —
;; settings unavailable means the key is absent, which resolves to the
;; current-gsd-worktree-isolation default (OFF).
(define (load-project-settings-silently base-dir)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (load-settings base-dir)))

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
  (define isolate?
    (apply-worktree-isolation-setting! (load-project-settings-silently base-dir)
                                       #:isolate? isolate-arg))
  (define plan-id (campaign-plan-id rec))
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
                (campaign-result 'wave-cancelled
                                 (reverse completed)
                                 "campaign cancellation requested")]
               [(not next-idx)
                (campaign-result 'campaign-complete (reverse completed) "all waves done or deferred")]
               [else
                (define result
                  (run-campaign-wave base-dir
                                     current
                                     next-idx
                                     #:runner runner
                                     #:verifier verifier
                                     #:meta-fix-predicate meta-fix-predicate
                                     #:fence-token (add1 (campaign-fence-token current))
                                     #:timeout-sec timeout-sec
                                     #:timeout-retries (current-gsd-wave-timeout-retries)
                                     #:isolate? isolate?))
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
         ;; v1.00.18 W5 (#9513): mutation-stall watchdog
         gsd-stall-exn
         gsd-stall-exn?
         make-gsd-stall-exn
         current-gsd-stall-steerer
         stall-steering-message
         stall-hard-failure-message
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
