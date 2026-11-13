#lang racket/base

;; Phase B slice S3 — native GitHub API read layer of the delivery controller.
;;
;; Scope law: this module is the READ layer only (`api`, `checks`,
;; `trusted_check`, `protection`, `validate_merge`, `fetch_head` of
;; scripts/gsd-delivery.py). It performs no mutation, is NOT yet wired into
;; extensions/gsd/delivery-coordinator.rkt, and the Python controller stays the
;; production path until an explicit later slice says otherwise.
;;
;; Every subprocess goes through the shared bounded seam
;; (scripts/gsd-delivery-git.rkt): ONE total process budget, credential
;; isolation via controller-environment, process-group cleanup and the typed
;; exn:fail:delivery-action refusal. Raw gh/git stderr is never surfaced — the
;; seam only ever raises its typed cause. `gh` is invoked as an ARGUMENT
;; VECTOR (no shell), so a route containing `?`/`&` is one literal argument.
;;
;; ONE-BUDGET LAW (B2): a single delivery-api-session owns a single
;; (budget-seconds, start-ms) pair, and BOTH the `gh` runner and every repo
;; `git -C <repo> ...` command (fetch-head) are built from that one bounded
;; runner. A gh read and a fetch-head therefore draw down the SAME decreasing
;; budget, exactly like Python's process-wide remaining_budget(); a second
;; runner would silently grant a fresh budget and is never created. The
;; documented production entry point is
;;
;;     (define session (make-delivery-api-session repo))
;;     ((delivery-api-session-api-call session) slug route)
;;     ((delivery-api-session-fetch-head session) number)
;;
;; The module-level api-call / checks / trusted-check / protection / fetch-head
;; functions are compatibility shims over ONE lazily created default session
;; (created without a repository, so a repo is passed to fetch-head per call).
;; They are kept for the read-layer tests and for incremental callers of this
;; unwired slice; a real action must build one session, never mix both.
;;
;; Budgets are MONOTONIC: clear-api-cache! clears memos only. It never
;; recreates a runner, never re-reads GSD_DELIVERY_BUDGET_SEC and never resets
;; a start time, so a spent budget stays spent. Because a mutation can only be
;; proven from a FRESH read, a post-mutation proof MUST pass `#:refresh?` (the
;; Racket mirror of Python's `_refresh=True`); clearing the memo and re-reading
;; is not a refresh substitute and is not needed for that purpose.
;;
;; Parity law (the Python functions are the contract; message strings are
;; byte-identical so the coordinator's reason chain does not change):
;;
;;   api(slug, route, paginate=False, _refresh=False)
;;     `gh api --hostname github.com repos/<slug>/<route>`, plus
;;     `--paginate --slurp` when requested, 60 s requested timeout. Malformed
;;     JSON and a non-object/non-list payload both refuse with
;;     "malformed GitHub response for <route>". Results are memoized per
;;     (slug, route, paginate) so a repeated read spawns gh exactly once;
;;     `#:refresh?` BYPASSES the memo (and stores the fresh payload), which is
;;     what a post-mutation proof must use.
;;
;;   checks(slug, sha)
;;     Manual in-process pagination (gh 2.45 has no `--paginate --slurp`), so
;;     every gh version returns the same fail-closed result: pages of
;;     commits/<sha>/check-runs?per_page=100&page=N until a page is empty or the
;;     accumulated run count reaches total_count. Routes are built with
;;     `format` (never shell-concatenated), so `?`/`&` stay literal inside one
;;     argv element. Refusals: "invalid check result" (no check_runs list),
;;     "invalid check run entry", "malformed check-total",
;;     "check pagination incomplete". Memoized per (slug, sha); a total_count of
;;     0 with an empty page returns '() after exactly one request.
;;
;;   trusted_check(slug, sha, name, event, branch=None)
;;     Exactly one trusted Actions check per name: the highest-id matching run
;;     must be completed/success on <sha> by app 15368 (the Actions app), its
;;     details_url must be an authenticated Actions run URL for THIS repository,
;;     and the run itself must be completed/success on <sha>, on
;;     .github/workflows/ci.yml, owned by <slug>, with the requested event (and
;;     branch when given). The details_url pattern uses `regexp-quote` on the
;;     slug, so a slug with regexp metacharacters (`coinerd/q.b`) matches only
;;     literally, and both anchors are end-of-input (`$` in Racket is strictly
;;     end-of-input, unlike Python's pre-final-newline `$`), so a hostile
;;     prefix or suffix never authenticates. Refusals: "missing check: <name>",
;;     "pending, failed, or untrusted check: <name>",
;;     "check has no authenticated Actions run: <name>",
;;     "check workflow/ref/event mismatch: <name>".
;;
;;   protection(slug, expected_names)
;;     The default branch must enforce admins, require strict/up-to-date
;;     checks, forbid force pushes and deletions, and require a non-empty
;;     native check context list that covers the publication-time snapshot.
;;     Refusals: "invalid expected check names",
;;     "malformed branch protection response",
;;     "default branch protection is absent or weakened",
;;     "default branch protection requires configured native checks",
;;     "branch protection does not require expected checks: <name, name>".
;;
;;   validate_merge(pr, commit, slug, merge, head)
;;     The implementation PR must be authoritatively merged, its
;;     merge_commit_sha/head SHA must equal the verified full SHAs, base must be
;;     main of the SAME repository on both sides, and the merge commit must have
;;     exactly one parent (squash provenance). Refusals:
;;     "malformed PR/commit response", "implementation PR is not merged",
;;     "implementation merge SHA mismatch", "implementation head SHA mismatch",
;;     "implementation repository/default branch mismatch",
;;     "delivery must have single-parent squash provenance".
;;
;;   fetch_head(repo, number)
;;     Fetch refs/pull/<n>/head through the session's bounded runner and assert
;;     what was actually fetched: a non-positive/non-integer number refuses
;;     before any command ("invalid implementation PR number") and a non-40-hex
;;     FETCH_HEAD refuses with "malformed fetched PR head".

(require json
         racket/string
         "gsd-delivery-git.rkt")

(provide make-delivery-api-session
         delivery-api-session?
         delivery-api-session-budget
         delivery-api-session-start-ms
         delivery-api-session-api-call
         delivery-api-session-checks
         delivery-api-session-trusted-check
         delivery-api-session-protection
         delivery-api-session-fetch-head
         ;; compatibility shims (one lazily created default session)
         api-call
         clear-api-cache!
         default-session-deadline-ms
         checks
         trusted-check
         protection
         pr-is-merged?
         validate-merge
         fetch-head
         full-sha?
         actions-app-id)

;; ============================================================
;; Constants (mirror scripts/gsd-delivery.py)
;; ============================================================

;; GitHub's own Actions app id: only runs dispatched by GitHub Actions are
;; trusted to have executed the repository's committed workflow.
(define actions-app-id 15368)

;; Python `api` requests a 60 s timeout; the shared runner clamps it to the
;; remaining total process budget.
(define api-request-timeout 60.0)

;; ============================================================
;; Small JSON helpers
;; ============================================================

;; Python full_sha: isinstance(value, str) and re.fullmatch('[0-9a-f]{40}').
;; Pregexp (not #rx/POSIX): the interval quantifier {40} must be a real
;; quantifier, otherwise a 40-hex SHA is never recognized. `$` is
;; end-of-input in Racket, so a trailing newline is refused (Python's `$`
;; would allow one) — a fail-closed difference only.
(define full-sha-rx #px"^[0-9a-f]{40}$")

(define (full-sha? value)
  (and (string? value) (regexp-match? full-sha-rx value)))

;; Python dig: any non-dict along the path yields None (here #f).
(define (dig value key . keys)
  (for/fold ([current (and (hash? value) (hash-ref value key #f))]) ([next (in-list keys)])
    (if (hash? current)
        (hash-ref current next #f)
        #f)))

;; Python's `is None` distinguishes a JSON null from an absent key; Racket's
;; jsexpr reader keeps null as the symbol 'null, so both are "absent" here.
(define (absent? value)
  (or (not value) (eq? value 'null)))

(define (json-string-or-empty value)
  (if (string? value) value ""))

(define (malformed-response! route)
  (refuse! (string-append "malformed GitHub response for " route)))

;; ============================================================
;; Session: ONE budget shared by the gh runner and repo git commands
;; ============================================================

(struct delivery-api-session
        (repo budget
              start-ms
              api-cache
              checks-cache
              api-call
              checks
              trusted-check
              protection
              fetch-head)
  #:transparent)

;; Every session created in this process, so clear-api-cache! can drop every
;; memo without ever touching a runner or a budget.
(define sessions (box '()))

(define (register-session! session)
  (set-box! sessions (cons session (unbox sessions))))

;; Build the ONE session an action uses. `#:budget`/`#:start-ms` are a seam for
;; budget tests (and for callers that already spent wall-clock budget); the
;; defaults are the enforced GSD_DELIVERY_BUDGET_SEC setting and "now".
(define (make-delivery-api-session repo #:budget [budget #f] #:start-ms [start-ms #f])
  (define total-budget (or budget (budget-seconds)))
  (define start (or start-ms (current-inexact-monotonic-milliseconds)))
  ;; The single bounded runner of this session. Both closures below call it, so
  ;; every gh read AND every repo git command draws down the same budget.
  (define run (make-bounded-runner total-budget start))
  (define api-cache (make-hash))
  (define checks-cache (make-hash))

  ;; Repo-git built from the SHARED runner (`git -C <repo> ...`), never from a
  ;; second make-repo-git-command: a second runner would grant a fresh budget.
  (define (repo-git repo*)
    (define repo-string
      (if (path? repo*)
          (path->string repo*)
          repo*))
    (lambda (args) (run "git" (append (list "-C" repo-string) args) command-timeout-budget)))

  ;; A memoized, read-only GitHub REST GET through `gh api`. Returns the jsexpr
  ;; hash/list; raises the typed refusal for any unreadable response.
  (define (api slug route #:paginate? [paginate? #f] #:refresh? [refresh? #f])
    (define key (list slug route paginate?))
    (cond
      [(and (not refresh?) (hash-has-key? api-cache key)) (hash-ref api-cache key)]
      [else
       (define args
         (append (list "api" "--hostname" "github.com" (string-append "repos/" slug "/" route))
                 (if paginate?
                     (list "--paginate" "--slurp")
                     '())))
       (define text (run "gh" args api-request-timeout))
       (define data
         (with-handlers ([exn:fail? (lambda (_error) (malformed-response! route))])
           (string->jsexpr text)))
       ;; A bare number/string/null is not an API object graph; refuse instead
       ;; of letting a typed accessor later read nonsense.
       (unless (or (hash? data) (list? data))
         (malformed-response! route))
       ;; A refresh always STORES the fresh payload, so the next unrefreshed
       ;; read sees the post-mutation state instead of the stale entry.
       (hash-set! api-cache key data)
       data]))

  ;; Every check run recorded for a commit, paged manually so no gh version can
  ;; silently truncate the list. Memoized per (slug, sha).
  (define (checks slug sha)
    (define key (list slug sha))
    (cond
      [(hash-has-key? checks-cache key) (hash-ref checks-cache key)]
      [else
       (define-values (result total)
         (let loop ([page 1]
                    [acc '()])
           ;; Built with `format` so the query string stays inside ONE argv
           ;; element; gh is spawned as a vector, never through a shell.
           (define route (format "commits/~a/check-runs?per_page=100&page=~a" sha page))
           (define data (api slug route))
           (unless (and (hash? data) (list? (hash-ref data 'check_runs #f)))
             (refuse! "invalid check result"))
           (define runs (hash-ref data 'check_runs))
           (unless (for/and ([run (in-list runs)])
                     (hash? run))
             (refuse! "invalid check run entry"))
           ;; exact-integer? is STRICTER than Python's isinstance(total, int),
           ;; which also accepts JSON true/false; Racket reads those as #t/#f,
           ;; so a boolean total_count fails closed here instead of paginating
           ;; on nonsense.
           (define page-total (hash-ref data 'total_count #f))
           (unless (and (exact-integer? page-total) (>= page-total 0))
             (refuse! "malformed check-total"))
           (define accumulated (append acc runs))
           (if (or (null? runs) (>= (length accumulated) page-total))
               (values accumulated page-total)
               (loop (add1 page) accumulated))))
       ;; A short page is only acceptable when the accumulated runs already
       ;; cover the advertised total; otherwise the read is incomplete and
       ;; fails closed.
       (unless (>= (length result) total)
         (refuse! "check pagination incomplete"))
       (hash-set! checks-cache key result)
       result]))

  ;; The single authoritative Actions check for `name` on `sha`, proven by both
  ;; the check run AND the workflow run it points at.
  (define (trusted-check slug sha name event #:branch [branch #f])
    (define candidates
      (for/list ([check (in-list (checks slug sha))]
                 #:when (equal? (hash-ref check 'name #f) name))
        check))
    (unless (pair? candidates)
      (refuse! (string-append "missing check: " name)))
    (define check (highest-id candidates))
    (unless (and (equal? (hash-ref check 'status #f) "completed")
                 (equal? (hash-ref check 'conclusion #f) "success")
                 (equal? (hash-ref check 'head_sha #f) sha)
                 (equal? (dig check 'app 'id) actions-app-id))
      (refuse! (string-append "pending, failed, or untrusted check: " name)))
    ;; The details_url must name an Actions run of THIS repository: a look-alike
    ;; URL (another repo, another host, a non-run path) is never authenticated.
    ;; regexp-quote makes a slug with metacharacters (`coinerd/q.b`) match only
    ;; literally, and `^`/`$` are end-of-input anchors, so a hostile prefix
    ;; ("https://evil/x/https://github.com/...") or suffix (".../job/123/extra",
    ;; ".../job/123?x=1", a trailing newline) never matches.
    (define details (json-string-or-empty (hash-ref check 'details_url #f)))
    (define match
      (regexp-match (string-append "^https://github\\.com/"
                                   (regexp-quote slug)
                                   "/actions/runs/([0-9]+)/job/[0-9]+$")
                    details))
    (unless match
      (refuse! (string-append "check has no authenticated Actions run: " name)))
    (define run-data (api slug (string-append "actions/runs/" (cadr match))))
    (unless (and (hash? run-data)
                 (equal? (hash-ref run-data 'status #f) "completed")
                 (equal? (hash-ref run-data 'conclusion #f) "success")
                 (equal? (hash-ref run-data 'head_sha #f) sha)
                 (equal? (hash-ref run-data 'event #f) event)
                 (equal? (dig run-data 'repository 'full_name) slug)
                 (equal? (hash-ref run-data 'path #f) ".github/workflows/ci.yml")
                 (or (not branch) (equal? (hash-ref run-data 'head_branch #f) branch)))
      (refuse! (string-append "check workflow/ref/event mismatch: " name)))
    (void))

  ;; The default branch protection must actually require native checks, be
  ;; admin-enforced, strict and push/delete-locked, and cover the expected name
  ;; snapshot (newer additive names are allowed; an empty list fails closed).
  (define (protection slug expected-names)
    (unless (and (list? expected-names) (pair? expected-names))
      (refuse! "invalid expected check names"))
    (define policy (api slug "branches/main/protection"))
    (unless (hash? policy)
      (refuse! "malformed branch protection response"))
    (define contexts (dig policy 'required_status_checks 'contexts))
    (unless (and (equal? (dig policy 'enforce_admins 'enabled) #t)
                 (equal? (dig policy 'required_status_checks 'strict) #t)
                 (equal? (dig policy 'allow_force_pushes 'enabled) #f)
                 (equal? (dig policy 'allow_deletions 'enabled) #f))
      (refuse! "default branch protection is absent or weakened"))
    (unless (and (list? contexts)
                 (pair? contexts)
                 (for/and ([item (in-list contexts)])
                   (string? item)))
      (refuse! "default branch protection requires configured native checks"))
    (define missing
      (for/list ([name (in-list expected-names)]
                 #:unless (member name contexts))
        name))
    (unless (null? missing)
      (refuse! (string-append "branch protection does not require expected checks: "
                              (string-join missing ", "))))
    policy)

  ;; Fetch the exact PR head ref through the shared repo-git command and assert
  ;; what was actually fetched (a 40-hex commit, not a tag or a branch name).
  ;; `repo*` lets the repository-less default session serve the compatibility
  ;; fetch-head; it is still one command of THIS session's budget.
  (define (fetch-head number [repo* repo])
    (unless (and (exact-integer? number) (> number 0))
      (refuse! "invalid implementation PR number"))
    (define target (or repo* repo))
    (unless target
      (error 'make-delivery-api-session
             "fetch-head needs a repository: create the session with one, or pass it per call"))
    (define git (repo-git target))
    (git (list "fetch" "--no-tags" "origin" (format "refs/pull/~a/head" number)))
    (define fetched (string-trim (git (list "rev-parse" "FETCH_HEAD"))))
    (unless (full-sha? fetched)
      (refuse! "malformed fetched PR head"))
    fetched)

  (define session
    (delivery-api-session repo
                          total-budget
                          start
                          api-cache
                          checks-cache
                          api
                          checks
                          trusted-check
                          protection
                          fetch-head))
  (register-session! session)
  session)

;; Drop every memoized read in every session. Budgets are MONOTONIC: no runner
;; is recreated, no budget is re-read, no start time is reset — a spent budget
;; stays spent (only `#:refresh?` may bypass a memo, and it never restores one).
(define (clear-api-cache!)
  (for ([session (in-list (unbox sessions))])
    (hash-clear! (delivery-api-session-api-cache session))
    (hash-clear! (delivery-api-session-checks-cache session)))
  (void))

;; ============================================================
;; Compatibility shims over ONE lazily created default session
;; ============================================================

;; The runner is created lazily so `budget-seconds` is read AFTER the ambient
;; environment fixture is installed (the same order the native sync entry
;; uses), and only ONCE per process (a budget is never refreshed). Created
;; without a repository: fetch-head receives the repo per call.
(define default-session-box (box #f))

(define (default-session)
  (or (unbox default-session-box)
      (let ([session (make-delivery-api-session #f)])
        (set-box! default-session-box session)
        session)))

;; Read-only peek at the default session's budget deadline (start-ms plus the
;; budget in milliseconds), or #f while no compatibility call has created the
;; session yet. This is a PURE accessor: it never creates a session, so a test
;; can prove clear-api-cache! neither drops nor recreates the default session.
(define (default-session-deadline-ms)
  (define session (unbox default-session-box))
  (and session
       (+ (delivery-api-session-start-ms session)
          (* 1000.0 (delivery-api-session-budget session)))))

(define (api-call slug route #:paginate? [paginate? #f] #:refresh? [refresh? #f])
  ((delivery-api-session-api-call (default-session)) slug
                                                     route
                                                     #:paginate? paginate?
                                                     #:refresh? refresh?))

(define (checks slug sha)
  ((delivery-api-session-checks (default-session)) slug sha))

(define (trusted-check slug sha name event #:branch [branch #f])
  ((delivery-api-session-trusted-check (default-session)) slug sha name event #:branch branch))

(define (protection slug expected-names)
  ((delivery-api-session-protection (default-session)) slug expected-names))

(define (fetch-head repo number)
  ((delivery-api-session-fetch-head (default-session)) number repo))

;; ============================================================
;; pr_is_merged / validate_merge
;; ============================================================

;; Authoritative merge state: `merged: true`, or a closed PR carrying a real
;; merged_at (the commit-association response can report merged: null).
(define (pr-is-merged? pr)
  (and (hash? pr)
       (or (equal? (hash-ref pr 'merged #f) #t)
           (and (equal? (hash-ref pr 'state #f) "closed")
                (not (absent? (hash-ref pr 'merged_at #f)))))))

;; The verified head must be exactly what the default branch now contains, via
;; a single-parent (squash) merge commit in the SAME repository.
(define (validate-merge pr commit slug merge head)
  (unless (and (hash? pr) (hash? commit))
    (refuse! "malformed PR/commit response"))
  (unless (pr-is-merged? pr)
    (refuse! "implementation PR is not merged"))
  (unless (and (equal? (dig pr 'merge_commit_sha) merge) (full-sha? merge))
    (refuse! "implementation merge SHA mismatch"))
  (unless (and (equal? (dig pr 'head 'sha) head) (full-sha? head))
    (refuse! "implementation head SHA mismatch"))
  (unless (and (equal? (dig pr 'base 'ref) "main")
               (equal? (dig pr 'base 'repo 'full_name) slug)
               (equal? (dig pr 'head 'repo 'full_name) slug))
    (refuse! "implementation repository/default branch mismatch"))
  (define parents (hash-ref commit 'parents #f))
  (unless (and (equal? (hash-ref commit 'sha #f) merge) (list? parents) (= 1 (length parents)))
    (refuse! "delivery must have single-parent squash provenance"))
  (void))

;; ============================================================
;; Check-run id ordering
;; ============================================================

;; Python max(candidates, key=...): the FIRST maximal element wins. Python
;; compares raw `c.get('id', 0)` values and would raise TypeError on mixed
;; types; this port is STRICTER — any non-number id counts as 0, so ordering
;; stays total instead of crashing (fail closed, never a crash).
(define (check-run-id check)
  (define value (hash-ref check 'id #f))
  (if (real? value) value 0))

(define (highest-id candidates)
  (let loop ([rest (cdr candidates)]
             [best (car candidates)])
    (cond
      [(null? rest) best]
      [else
       (define candidate (car rest))
       (loop (cdr rest) (if (> (check-run-id candidate) (check-run-id best)) candidate best))])))
