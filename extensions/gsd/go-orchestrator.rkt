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
                  current-gsd-max-consecutive-tool-calls)
         (only-in "prompts.rkt" wave-failure-context-block executor-reanchor-prompt)
         (only-in "wave-executor.rkt"
                  STALL-SOFT-LIMIT-DEFAULT
                  STALL-HARD-LIMIT-DEFAULT
                  stall-limit?
                  make-stall-watchdog
                  stall-watchdog-observe!
                  stall-watchdog-snapshot
                  ;; v1.00.17 W6 (#9512a): wave worktree isolation
                  worktree-isolation-enabled?
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
                  reclaim-orphaned-worktrees!)
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
(struct campaign-request (base-dir record prompt-for-wave verifier timeout-sec)
  #:transparent
  #:constructor-name make-campaign-request/5)

(define (make-campaign-request base-dir
                               record
                               prompt-for-wave
                               verifier
                               #:timeout-sec [timeout-sec #f])
  (make-campaign-request/5 base-dir record prompt-for-wave verifier timeout-sec))

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

(define (execute-campaign-request! request run-prompt #:lease-owner [lease-owner "unknown"])
  (define base-dir (campaign-request-base-dir request))
  (define record (campaign-request-record request))
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
                    (min 900 (max 60 (quotient (inexact->exact (floor budget)) 2))))])
    (run-campaign!
     base-dir
     record
     #:lease-owner lease-owner
     #:runner (make-wave-runner-port
               (lambda (wave-idx)
                 (with-handlers ([exn:fail? (lambda (e)
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

;; Hard-limit failure cause. Wording deliberately avoids the
;; infra-failure? vocabulary (network/connection/stream/...) so D8 (#9357)
;; classifies it as a genuine attempt failure, not a transient provider
;; error: an exploring-only attempt must consume its failure honestly.
(define (stall-hard-failure-message calls-since-mutation limit target-files)
  (format
   "mutation-stall watchdog: ~a tool calls without any file mutation exceeded the hard limit (~a). Target files: ~a. Attempt terminated for exploration-only behavior — an implementation wave must edit its target files."
   calls-since-mutation
   limit
   (if (null? target-files)
       "(none recorded)"
       (string-join target-files ", "))))

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
          (with-handlers ([gsd-stall-exn? (lambda (e)
                                            (wave-execution-outcome 'failed (exn-message e)))])
            (parameterize
                ([current-post-tool-result-hook
                  (lambda (msgs sid root)
                    (prev-hook msgs sid root)
                    (define records
                      (for/list ([m (in-list (if (list? msgs)
                                                 msgs
                                                 '()))]
                                 #:when (and (hash? m) (hash-ref m 'name #f)))
                        (hasheq 'name (hash-ref m 'name #f))))
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
                       (log-error "gsd: wave ~a hard stall (~a calls, no mutation) — failing attempt"
                                  wave-idx
                                  (hash-ref snap 'calls-since-mutation))
                       (raise (make-gsd-stall-exn (stall-hard-failure-message
                                                   (hash-ref snap 'calls-since-mutation)
                                                   (or hard-limit 0)
                                                   target-files)))]
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
(define no-change-rejection-prefix "no wave target files changed")

(define (no-change-rejection? verifier-message)
  (and (string? verifier-message)
       (>= (string-length verifier-message) (string-length no-change-rejection-prefix))
       (string-prefix? verifier-message no-change-rejection-prefix)))

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
                           ;; #f disables each limit.
                           #:stall-soft-limit [stall-soft-limit STALL-SOFT-LIMIT-DEFAULT]
                           #:stall-hard-limit [stall-hard-limit STALL-HARD-LIMIT-DEFAULT]
                           ;; v1.00.17 W6 (#9512a): wave worktree isolation
                           ;; (gsd.worktree-isolation; #t forces isolation,
                           ;; #f forces it off — overrides the flag for tests).
                           #:isolate? [isolate? (worktree-isolation-enabled?)])
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
     (define (run-once* no-change-retries-left wt-box keep-branch-box)
       (set-campaign-fence-token! active fence)
       (begin-attempt! active wave-idx fence)
       (persist-campaign! base-dir active)
       (define started-attempt (campaign-wave-current-attempt (find-wave active wave-idx)))
       (define expected-id (campaign-attempt-id started-attempt))
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
         (if (and (not stall-soft-limit) (not stall-hard-limit))
             run-one
             (wrap-run-one-with-stall-watchdog run-one
                                               (make-stall-watchdog #:soft-limit stall-soft-limit
                                                                    #:hard-limit stall-hard-limit)
                                               base-dir
                                               active
                                               wave-idx
                                               stall-soft-limit
                                               stall-hard-limit)))
       ;; BUG-0017 follow-up: retry a wave whose run exceeds the per-wave budget
       ;; (timed-out) with a FRESH session (each run-one invocation re-enters the
       ;; runner port, which the TUI/GUI factory maps to a new session). The
       ;; attempt is NOT consumed by retries — only final exhaustion persists
       ;; interrupted (at-least-once). Mirrors the LLM provider-retry ceiling
       ;; (current-provider-retry-max-retries = 5).
       (define (run-with-timeout-retry retries-left)
         (define result (coerce-run-result (run-one/watchdog wave-idx)))
         (if (and (eq? (wave-execution-outcome-kind result) 'timed-out) (> retries-left 0))
             (begin
               (log-info "wave ~a timed out; retrying (~a retries left)" wave-idx (sub1 retries-left))
               (run-with-timeout-retry (sub1 retries-left)))
             result))
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
         [(and after-run (campaign-record-cancellation after-run))
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
                                                     (wave-worktree-head-sha wt)))
                            ;; Delivery approved: the release wrapper must
                            ;; KEEP the branch (it is the merge evidence).
                            (when wt
                              (set-box! keep-branch-box #t))
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
                                                  keep-branch-box))))]
                              [else
                               (campaign-result 'wave-failed
                                                '()
                                                (if (string=? verifier-message "")
                                                    "verifier rejected"
                                                    verifier-message))])]
                           [(stale-attempt invalid-state)
                            (campaign-result 'wave-cancelled '() "stale completion ignored")]
                           [else (campaign-result 'wave-failed '() "unexpected completion state")])])))])]
            [(failed)
             (if (persist-current-status! 'failed)
                 (begin
                   (apply-wave-status-projections! base-dir
                                                   wave-idx
                                                   STATUS-FAILED
                                                   (lambda (idx) (wave-slug base-dir idx)))
                   (campaign-result 'wave-failed '() "runner error"))
                 (campaign-result 'wave-cancelled '() "stale runner result ignored"))]
            ;; D8 (#9357): transient provider/network/SSE failure — do NOT
            ;; consume the attempt. Roll back the begin-attempt! increment,
            ;; reset the wave to pending, and stop the campaign so the user
            ;; re-runs /go when the provider is healthy. Avoids both attempt
            ;; churn (attempt-4: 30 tools done, one 120 s SSE read timeout
            ;; → wave-failed) and hot-looping on a sick provider.
            [(infra-failed)
             (define infra-wave (current-wave-for-attempt after-run wave-idx fence expected-id))
             (when infra-wave
               (set-campaign-wave-status! infra-wave 'pending)
               (set-campaign-wave-attempt-count!
                infra-wave
                (max 0 (sub1 (campaign-wave-attempt-count infra-wave))))
               (set-campaign-wave-current-attempt! infra-wave #f)
               (persist-campaign! base-dir after-run)
               (mirror-status! 'pending))
             (campaign-result
              'wave-cancelled
              '()
              (format
               "provider/network failure: ~a — wave preserved (attempt not consumed); re-run /go"
               (wave-execution-outcome-message run-result)))]
            [(cancelled interrupted) (interrupt-current! (wave-execution-outcome-message run-result))]
            ;; A hung tool that exceeded its deadline: persist INTERRUPTED per
            ;; D1 (cancelled/error/timeout stop the campaign) and never emit a
            ;; completion — the durable record says interrupted, so a restart
            ;; re-attempts the wave (at-least-once, exactly-once event).
            [(timed-out)
             ;; All retries exhausted. Persist INTERRUPTED per D1 (cancelled/
             ;; error/timeout stop the campaign) and never emit a completion —
             ;; the durable record says interrupted, so a restart re-attempts
             ;; the wave (at-least-once, exactly-once event).
             (interrupt-current! (if (> timeout-retries 0)
                                     (format "~a after ~a retries"
                                             (wave-execution-outcome-message run-result)
                                             timeout-retries)
                                     (wave-execution-outcome-message run-result)))]
            [else
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
           [keep-branch-box (box #f)])
       (dynamic-wind
        void
        (lambda () (run-once* no-change-retries wt-box keep-branch-box))
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
                       #:isolate? [isolate? (worktree-isolation-enabled?)])
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
         (let loop ([current authoritative]
                    [completed '()])
           (define next-idx (select-next-actionable-wave current))
           (cond
             [(campaign-record-cancellation current)
              (campaign-result 'wave-cancelled (reverse completed) "campaign cancellation requested")]
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
         wave-worktree-base-commit)
