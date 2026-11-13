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
;; Parity law (the Python functions are the contract; message strings are
;; byte-identical so the coordinator's reason chain does not change):
;;
;;   api(slug, route, paginate=False)
;;     `gh api --hostname github.com repos/<slug>/<route>`, plus
;;     `--paginate --slurp` when requested, 60 s requested timeout. Malformed
;;     JSON and a non-object/non-list payload both refuse with
;;     "malformed GitHub response for <route>". Results are memoized per
;;     (slug, route, paginate) so a repeated read spawns gh exactly once;
;;     clear-api-cache! drops the memo (tests and a future post-mutation
;;     refresh path).
;;
;;   checks(slug, sha)
;;     Manual in-process pagination (gh 2.45 has no `--paginate --slurp`), so
;;     every gh version returns the same fail-closed result: pages of
;;     commits/<sha>/check-runs?per_page=100&page=N until a page is empty or the
;;     accumulated run count reaches total_count. Refusals: "invalid check
;;     result" (no check_runs list), "invalid check run entry",
;;     "malformed check-total", "check pagination incomplete". Memoized per
;;     (slug, sha).
;;
;;   trusted_check(slug, sha, name, event, branch=None)
;;     Exactly one trusted Actions check per name: the highest-id matching run
;;     must be completed/success on <sha> by app 15368 (the Actions app), its
;;     details_url must be an authenticated Actions run URL for THIS repository,
;;     and the run itself must be completed/success on <sha>, on
;;     .github/workflows/ci.yml, owned by <slug>, with the requested event (and
;;     branch when given). Refusals: "missing check: <name>",
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
;;     Fetch refs/pull/<n>/head and assert what was actually fetched: a
;;     non-positive/non-integer number refuses before any command
;;     ("invalid implementation PR number") and a non-40-hex FETCH_HEAD refuses
;;     with "malformed fetched PR head".

(require json
         racket/string
         "gsd-delivery-git.rkt")

(provide api-call
         clear-api-cache!
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
;; quantifier, otherwise a 40-hex SHA is never recognized.
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

;; ============================================================
;; Bounded gh runner (shared seam; one total budget per process)
;; ============================================================

;; The runner is created lazily so `budget-seconds` is read AFTER the ambient
;; environment fixture is installed (the same order the native sync entry
;; uses). One runner = one decreasing process budget, matching Python's
;; process-wide remaining_budget().
(define runner-box (box #f))

(define (gh-runner)
  (or (unbox runner-box)
      (let ([runner (make-bounded-runner (budget-seconds) (current-inexact-monotonic-milliseconds))])
        (set-box! runner-box runner)
        runner)))

;; ============================================================
;; api(slug, route, paginate=False)
;; ============================================================

(define api-cache (make-hash))
(define checks-cache (make-hash))

;; Drop every memoized read (and the lazily created budget) so a test or a
;; post-mutation caller never reuses a pre-mutation response.
(define (clear-api-cache!)
  (hash-clear! api-cache)
  (hash-clear! checks-cache)
  (set-box! runner-box #f)
  (void))

(define (malformed-response! route)
  (refuse! (string-append "malformed GitHub response for " route)))

;; A memoized, read-only GitHub REST GET through `gh api`. Returns the jsexpr
;; hash/list; raises the typed refusal for any unreadable response.
(define (api-call slug route #:paginate? [paginate? #f])
  (define key (list slug route paginate?))
  (cond
    [(hash-has-key? api-cache key) (hash-ref api-cache key)]
    [else
     (define args
       (append (list "api" "--hostname" "github.com" (string-append "repos/" slug "/" route))
               (if paginate?
                   (list "--paginate" "--slurp")
                   '())))
     (define text ((gh-runner) "gh" args api-request-timeout))
     (define data
       (with-handlers ([exn:fail? (lambda (_error) (malformed-response! route))])
         (string->jsexpr text)))
     ;; A bare number/string/null is not an API object graph; refuse instead of
     ;; letting a typed accessor later read nonsense.
     (unless (or (hash? data) (list? data))
       (malformed-response! route))
     (hash-set! api-cache key data)
     data]))

;; ============================================================
;; checks(slug, sha)
;; ============================================================

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
         (define route (format "commits/~a/check-runs?per_page=100&page=~a" sha page))
         (define data (api-call slug route))
         (unless (and (hash? data) (list? (hash-ref data 'check_runs #f)))
           (refuse! "invalid check result"))
         (define runs (hash-ref data 'check_runs))
         (unless (for/and ([run (in-list runs)])
                   (hash? run))
           (refuse! "invalid check run entry"))
         (define page-total (hash-ref data 'total_count #f))
         (unless (and (exact-integer? page-total) (>= page-total 0))
           (refuse! "malformed check-total"))
         (define accumulated (append acc runs))
         (if (or (null? runs) (>= (length accumulated) page-total))
             (values accumulated page-total)
             (loop (add1 page) accumulated))))
     ;; A short page is only acceptable when the accumulated runs already cover
     ;; the advertised total; otherwise the read is incomplete and fails closed.
     (unless (>= (length result) total)
       (refuse! "check pagination incomplete"))
     (hash-set! checks-cache key result)
     result]))

;; Python max(candidates, key=...): the FIRST maximal element wins, and a
;; missing/non-numeric id counts as 0.
(define (check-run-id check)
  (define value (hash-ref check 'id #f))
  (if (number? value) value 0))

(define (highest-id candidates)
  (let loop ([rest (cdr candidates)]
             [best (car candidates)])
    (cond
      [(null? rest) best]
      [else
       (define candidate (car rest))
       (loop (cdr rest) (if (> (check-run-id candidate) (check-run-id best)) candidate best))])))

;; ============================================================
;; trusted-check(slug, sha, name, event, #:branch)
;; ============================================================

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
  (define details (json-string-or-empty (hash-ref check 'details_url #f)))
  (define match
    (regexp-match
     (string-append "^https://github\\.com/" (regexp-quote slug) "/actions/runs/([0-9]+)/job/[0-9]+$")
     details))
  (unless match
    (refuse! (string-append "check has no authenticated Actions run: " name)))
  (define run (api-call slug (string-append "actions/runs/" (cadr match))))
  (unless (and (hash? run)
               (equal? (hash-ref run 'status #f) "completed")
               (equal? (hash-ref run 'conclusion #f) "success")
               (equal? (hash-ref run 'head_sha #f) sha)
               (equal? (hash-ref run 'event #f) event)
               (equal? (dig run 'repository 'full_name) slug)
               (equal? (hash-ref run 'path #f) ".github/workflows/ci.yml")
               (or (not branch) (equal? (hash-ref run 'head_branch #f) branch)))
    (refuse! (string-append "check workflow/ref/event mismatch: " name)))
  (void))

;; ============================================================
;; protection(slug, expected_names)
;; ============================================================

;; The default branch protection must actually require native checks, be
;; admin-enforced, strict and push/delete-locked, and cover the expected name
;; snapshot (newer additive names are allowed; an empty list fails closed).
(define (protection slug expected-names)
  (unless (and (list? expected-names) (pair? expected-names))
    (refuse! "invalid expected check names"))
  (define policy (api-call slug "branches/main/protection"))
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
;; fetch_head(repo, number)
;; ============================================================

;; Fetch the exact PR head ref through the shared repo-git seam and assert what
;; was actually fetched (a 40-hex commit, not a tag or a branch name).
(define (fetch-head repo number)
  (unless (and (exact-integer? number) (> number 0))
    (refuse! "invalid implementation PR number"))
  (define git (make-repo-git-command repo))
  (git (list "fetch" "--no-tags" "origin" (format "refs/pull/~a/head" number)))
  (define fetched (string-trim (git (list "rev-parse" "FETCH_HEAD"))))
  (unless (full-sha? fetched)
    (refuse! "malformed fetched PR head"))
  fetched)
