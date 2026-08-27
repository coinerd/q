#lang racket/base

;; infra-retry-policy.rkt — GSD infra-retry budget/backoff seam
;; (BUG-0042, v1.00.22 W7).
;;
;; Extracted VERBATIM from go-orchestrator.rkt (behavior-preserving
;; decomposition): infra-failure classification of loop results,
;; raw-loop-result -> wave-execution-outcome conversion, prior-attempt
;; context joining with dirty-state capture, and the campaign infra-retry
;; telemetry events. go-orchestrator re-provides these names for
;; compatibility with existing importers; new code should import this
;; module directly.

(require racket/string
         "../../util/loop-result.rkt"
         (only-in "wave-runner-port.rkt" wave-execution-outcome)
         (only-in "wave-executor.rkt"
                  default-run-git
                  git-result-code
                  git-result-stdout
                  wave-worktree-path)
         (only-in "events.rkt" emit-gsd-event!)
         (only-in "stall-policy.rkt" stall-cause-message?)
         (only-in "policy.rkt"
                  current-gsd-campaign-infra-retries
                  current-gsd-campaign-infra-retry-delay))

(provide infra-failure?
         prompt-run-result->outcome
         attempt-context-max-chars
         build-wave-attempt-context
         porcelain-file-path
         capture-worktree-dirty-state
         take-up-to
         append-dirty-capture-to-context
         outside-lease-dirty-rkt-files
         outside-lease-dirty-warning
         warn-outside-lease-dirty-state!
         emit-infra-retry-event!)

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

;; Best-effort observability: every automatic retry emits
;; gsd.campaign.infra-retry (payload: wave idx, attempt, delay seconds).
;; A bus failure must never break the retry loop itself.
(define (emit-infra-retry-event! wave-idx attempt delay-secs)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning "gsd: infra-retry event emission failed: ~a"
                                            (exn-message e)))])
    (emit-gsd-event! 'gsd.campaign.infra-retry
                     (hasheq 'wave wave-idx 'attempt attempt 'delay delay-secs))))

;; =============================================================
;; v1.00.22 W7 (BUG-0042): budget/backoff/exhaustion-message seams,
;; extracted verbatim from go-orchestrator's infra-retry arm.
;; =============================================================

(provide infra-retry-consume!
         infra-retry-backoff-secs
         infra-retry-exhausted-message)

;; Consume one retry slot and record the failing attempt + timestamp.
;; Returns the budget AFTER this failure: a bound of N permits N
;; automatic re-attempts, so re-enter while the budget is
;; non-negative (0 = one last retry; -1 = exhausted).
(define (infra-retry-consume! state failed-attempt)
  (set-box! state
            (cons (sub1 (car (unbox state)))
                  (append (cdr (unbox state)) (list (list (or failed-attempt 0) (current-seconds))))))
  (car (unbox state)))

;; Backoff delay for the retry that follows a state with this many
;; retries left (this-retry = 1 on the first automatic re-attempt).
(define (infra-retry-backoff-secs retries-left)
  ((current-gsd-campaign-infra-retry-delay) (- (current-gsd-campaign-infra-retries) retries-left)))

;; Terminal message once the bound is exhausted — verbatim format from
;; go-orchestrator (attempt not consumed; the durable wave stays
;; pending and re-attemptable, so we only re-run /go when healthy).
(define (infra-retry-exhausted-message state)
  (format (string-append "provider/network failure persisted after ~a automatic retries "
                         "(attempt not consumed); re-run /go when the provider is healthy. "
                         "Failures: ~a")
          (current-gsd-campaign-infra-retries)
          (string-join (for/list ([f (cdr (unbox state))])
                         (format "attempt ~a at ~a" (car f) (cadr f)))
                       "; ")))
