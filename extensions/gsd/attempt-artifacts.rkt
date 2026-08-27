#lang racket/base

;; attempt-artifacts.rkt — GSD branch-delivery bookkeeping + attempt-
;; artifact ledger + reclaim report (BUG-0042, v1.00.22 W7).
;;
;; Extracted VERBATIM from go-orchestrator.rkt (behavior-preserving
;; decomposition). Owns:
;;   - wave-branch commit counting + zero-commit delivery warning
;;   - the deterministic wave delivery commit (worktree isolation ON)
;;   - branch context / base-commit / head-sha resolution
;;   - durable delivery provenance recording
;;   - the v1.00.21 W5 (BUG-0029) attempt-artifact ledger: creation
;;     entries, terminal transitions, teardown outcomes, end-of-campaign
;;     leftovers report (operator-approved reclaim, never auto-delete)
;;   - delivered-branches spare list + durable-status mirroring
;; go-orchestrator re-provides these names for compatibility with
;; existing importers; new code should import this module directly.

(require racket/base
         racket/string
         "campaign-state.rkt"
         (only-in "campaign-repository.rkt" load-campaign-record persist-campaign!)
         (only-in "wave-executor.rkt"
                  default-run-git
                  git-result-code
                  git-result-stdout
                  git-result-stderr
                  wave-worktree-path
                  wave-worktree-branch
                  wave-worktree-repo-root
                  wave-worktree-base-ref
                  worktree-hash8)
         (only-in "delivery-verifier.rkt" make-branch-delivery-context branch-delivery-context-ref)
         (only-in "stall-policy.rkt" find-wave))

(provide wave-branch-commit-count
         warn-zero-commit-delivery-branch!
         wave-delivery-commit-message
         commit-wave-worktree!
         wave-worktree-base-commit
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
         mirror-durable-statuses!)

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
