#lang racket/base

;; Phase B slice S4 — native port of the READ-ONLY delivery ladder actions
;; `resolve-pr`, `pr-ci` and `review` of the deleted Python delivery controller,
;; together with the helpers those actions share (resolve_pr, resolve_merged_pr,
;; resolve_existing_pr, resolve_prs_for_branch, branch_owner,
;; validate_pr_identity, require_receipt_tip, policy_names, blob_or_none,
;; refresh, repository, EXCLUDED_PREFIXES, full_sha).
;;
;; Scope law: this module is the READ-ONLY ladder only. It mutates nothing; it
;; IS dispatched by extensions/gsd/delivery-coordinator.rkt, which is the
;; production path since the Phase B final deletion removed the Python
;; controller. It reuses the S3 read layer
;; (scripts/gsd-delivery-api.rkt: make-delivery-api-session, api-call,
;; checks/trusted-check/protection, validate-merge, fetch-head) and the shared
;; bounded seam (scripts/gsd-delivery-git.rkt).
;;
;; ONE-BUDGET LAW (B2), inherited: an action builds ONE delivery-api-session and
;; passes it through every helper. The session owns one (budget-seconds,
;; start-ms) pair; this module's `git -C <repo> ...` command AND its
;; `racket scripts/...` helper command are built from that SAME pair via
;; make-bounded-runner, so a gh read, a fetch-head and a policy/tip read all
;; draw down ONE decreasing budget exactly like Python's process-wide
;; remaining_budget(). A second make-repo-git-command would silently grant a
;; fresh budget and is never created here.
;;
;; Parity law (the Python functions are the contract; refusal strings are
;; byte-identical so the coordinator's reason chain does not change):
;;
;;   resolve_pr(repo, plan, wave, branch, expected_head=None) -> hash
;;     Refresh origin/main, resolve at most one OPEN PR for the durable branch
;;     (GitHub head filter `owner:branch`, NOT `owner/repo:branch`), and — only
;;     when a durable expected_head is supplied and no open PR exists — accept
;;     one CLOSED MERGED PR for the branch (lost-merge-response resume).
;;     Zero candidates -> {'status':'none',...}; one candidate ->
;;     {'status':'resolved',...}. With expected_head the resolved PR must be
;;     open (or closed on the merged-resume path), carry a full 40-hex head on
;;     the expected branch, be a same-repository main-base PR, match the
;;     actually-fetched refs/pull/<n>/head, and satisfy the receipt-tip rule:
;;     the receipt head must be an ANCESTOR of the PR head with
;;     excluded-evidence-only drift, so the merged-resume head may be a
;;     post-receipt evidence-only descendant and is NOT required to be the
;;     receipt head itself.
;;
;;   pr_ci(repo, number, expected_branch, expected_head=None) -> hash
;;     Refresh, read origin/main (a malformed head refuses before any GitHub
;;     read), re-read the PR WITHOUT the memo (`_refresh=True`), then require an
;;     open same-repo main-base PR (a closed merged PR is tolerated ONLY as the
;;     explicit receipt-bound resume), a full head SHA on the expected branch,
;;     the actually-fetched PR ref equal to that head, the receipt-tip rule, the
;;     required-check policy snapshot at origin/main, branch protection that
;;     requires those names, and one trusted Actions check per name.
;;
;;   review(repo, plan, wave, expected_head, expected_branch,
;;          campaign_root=None, generation=None) -> hash
;;     Pure git-object readback plus the unchanged strict gate: fetch the wave
;;     branch, require the receipt head to be an ancestor of the fetched tip
;;     with evidence-only drift, require the binding-named evidence trio at the
;;     tip, then `validate_trio` (materialized blobs + excluded-path digest +
;;     scripts/gsd-wave-gate.rkt). `awaiting-review` when the review artifact is
;;     absent at the receipt identity; `reviewed` only after the reviewed-sha
;;     receipt-binding clause passes. That clause is the documented WEAKER
;;     form: a reviewed-sha that is not the receipt head is accepted when it is
;;     an ancestor of the receipt head whose commits are excluded-evidence-only
;;     (any other reviewed-sha is refused), never when it is unrelated.
;;
;;   Helper parity (each helper below is exported and independently exercised):
;;
;;   repository(repo) -> 'owner/name'
;;     Reads the CONFIGURED remote.origin.url (an insteadOf rewrite is only
;;     transport and never redefines identity) and refuses any URL that is not
;;     https://github.com/... or git@github.com:... (no arbitrary API host).
;;
;;   blob_or_none(repo, ref, path) -> text | None
;;     `git show <ref>:<path>`; an absent path is repository state (None), but
;;     an INFRASTRUCTURE refusal (spent budget, broken exec boundary) is
;;     re-raised instead of being reported as an absent blob.
;;
;;   pr_ci(repo, number, expected_branch, expected_head=None) -> hash
;;     With expected_head the open-PR contract is loosened ONLY for the
;;     receipt-bound merged resume: a CLOSED MERGED PR whose head is exactly
;;     expected_head. A closed-but-NOT-merged PR is never a resume, and the
;;     receipt-tip rule still applies to the resume head.
;;
;;   resolve_merged_pr(slug, branch, expected_head=None) -> pr | None
;;     The all-state query is filtered to closed AND MERGED PRs whose head ref
;;     is the durable branch, so a closed-but-unmerged PR is never a resume
;;     candidate. At most one candidate survives: an exact expected-head match
;;     narrows the set first, and an absent match keeps every merged candidate
;;     and therefore fails closed on two (no guessing among historical PRs).
;;
;;   require_receipt_tip(repo, expected_head, tip, action) -> void
;;     An ABSENT (None) expected head is a no-op — the rule is receipt-bound.
;;     Otherwise the receipt head must be an ancestor of the tip and every
;;     commit between them must touch only excluded evidence directories.
;;
;; Refusal discipline: every refusal is the typed exn:fail:delivery-action from
;; the shared seam. Pure-helper refusals carry the exact Python reason string;
;; infrastructure faults (budget/timeout/exec boundary) keep their own typed
;; cause through `infrastructure-refusal?` and are never re-labelled as
;; repository state.
;;
;; Documented deviations (all fail closed, never open):
;;   * git stdout is decoded UTF-8 (errors replaced) before hashing, so the
;;     review content digest of a NON-UTF-8 binary diff can differ from
;;     Python's raw-byte hash; the strict gate recomputes the digest from the
;;     materialized root, so the only effect is a typed refusal.
;;   * `blob_or_none` re-raises INFRASTRUCTURE refusals instead of swallowing
;;     them: a spent budget or a broken exec boundary is never mistaken for
;;     "the blob is absent".
;;   * a malformed helper subprocess payload (binding datum, journal, snapshot)
;;     is a typed refusal with the documented reason instead of a raw Python
;;     traceback surfaced through main()'s blanket except.
;;   * resolve_prs_for_branch returns a JSON-null candidate as the symbol
;;     'null, which is TRUTHY in Racket (Python's None is falsy). So a
;;     `[null]` pulls payload is STRICTER here: it fails closed with 'resolved
;;     pull request has no usable number' instead of Python's `none` /
;;     merged-resume fallback. Nothing is ever resolved from a null candidate.
;;   * the session's api and checks memos are per-session and `checks` has NO
;;     refresh bypass (only `api` takes `#:refresh?`), so one context must
;;     serve exactly ONE action; reusing a context would serve the
;;     pre-mutation check snapshot to a second action.

(require json
         "../util/shell-quote.rkt"
         racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         file/sha1
         openssl
         "gsd-delivery-git.rkt"
         "gsd-delivery-api.rkt")

(provide delivery-resolve-pr!
         delivery-pr-ci!
         delivery-review!
         delivery-read-ladder-main
         ;; shared helpers (exported for parity tests and later slices)
         make-delivery-pr-context
         delivery-pr-context?
         delivery-pr-context-repo
         delivery-pr-context-session
         delivery-pr-context-git
         delivery-pr-context-api
         delivery-pr-context-run
         repository
         refresh
         branch-owner
         resolve-prs-for-branch
         resolve-existing-pr
         resolve-merged-pr
         validate-pr-identity
         require-receipt-tip
         policy-names
         blob-or-none
         binding-path
         artifact-path
         excluded-prefixes
         ;; S12 (auto-review) reuses these exact seams instead of duplicating
         ;; digest computation and datum materialization.
         content-digest
         materialize
         read-binding-datum
         ;; The strict-gate trio validator: Python keeps ONE `validate_trio` and
         ;; so does this port (the read ladder, the S12 review publication and
         ;; the status readback all share it). Its `#:owned-authority` argument
         ;; is the only way an owned-proof audit trio is authenticated.
         validate-trio)

;; ============================================================
;; Constants (mirror the deleted Python controller; recorded in the goldens fixture)
;; ============================================================

(define-runtime-path this-source "gsd-delivery-pr.rkt")
(define scripts-dir (path-only this-source))
(define binding-data-script (build-path scripts-dir "gsd-binding-data.rkt"))
(define wave-gate-script (build-path scripts-dir "gsd-wave-gate.rkt"))

(define policy-relative-path "scripts/required-pr-checks.policy")

;; Python EXCLUDED_PREFIXES: every evidence directory the ladder tolerates as
;; post-receipt drift. EXCLUDES are the same directories as git pathspec magic.
(define excluded-dirs '("evidence" "reviews" "validation"))
(define excluded-prefixes
  (for/list ([dir (in-list excluded-dirs)])
    (string-append "docs/reports/gsd-wave-" dir "/")))
(define exclude-pathspecs
  (for/list ([dir (in-list excluded-dirs)])
    (format ":(exclude)docs/reports/gsd-wave-~a/**" dir)))

(define wave-branch-prefix "refs/heads/")
(define helper-command-timeout 60.0)
(define campaign-id-rx #px"^[0-9a-f]{64}$")

;; Python's `re.fullmatch(r'(?:https://github\.com/|git@github\.com:)…')`: the
;; configured origin URL (never the insteadOf-rewritten transport URL) must name
;; a GitHub repository. `$` is end-of-input in Racket.
(define origin-url-rx
  #px"^(?:https://github\\.com/|git@github\\.com:)([\\w.-]+/[\\w.-]+?)(?:\\.git)?$")

;; ============================================================
;; Small helpers (same semantics as the S3 read layer)
;; ============================================================

;; Python `require(ok, reason)`.
(define (ensure! ok message)
  (unless ok
    (refuse! message)))

;; Python dig: any non-hash along the path yields None (#f).
(define (dig value key . keys)
  (for/fold ([current (and (hash? value) (hash-ref value key #f))]) ([next (in-list keys)])
    (if (hash? current)
        (hash-ref current next #f)
        #f)))

(define (nonblank-string? value)
  (and (string? value) (not (string=? (string-trim value) ""))))

(define (short-sha value)
  (if (string? value)
      (substring value 0 (min 12 (string-length value)))
      (format "~a" value)))

;; Python `x is None` for a JSON null that Racket read as the symbol 'null.
(define (absent? value)
  (or (not value) (eq? value 'null)))

(define (json-null value)
  (if (absent? value) 'null value))

(define (sha256-hex-of-text text)
  (bytes->hex-string (sha256-bytes (open-input-bytes (string->bytes/utf-8 text)))))

;; ============================================================
;; Action context: ONE session, ONE budget, passed through
;; ============================================================

;; The context is the ONE budgeted handle an action passes to every helper:
;; `session` owns the gh runner (api/checks/trusted-check/protection/fetch-head)
;; and `git` + `run` are built from the SAME (budget, start-ms) pair, so no
;; helper can start a second budget.
(struct delivery-pr-context (repo session api fetch-head git run) #:transparent)

(define (make-delivery-pr-context repo #:budget [budget #f] #:start-ms [start-ms #f])
  (define session (make-delivery-api-session repo #:budget budget #:start-ms start-ms))
  ;; Built from the session's OWN pair: identical remaining-budget arithmetic.
  (define run
    (make-bounded-runner (delivery-api-session-budget session)
                         (delivery-api-session-start-ms session)))
  (define repo-string
    (if (path? repo)
        (path->string repo)
        repo))
  (define (git args)
    (run "git" (append (list "-C" repo-string) args) command-timeout-budget))
  (delivery-pr-context repo
                       session
                       (delivery-api-session-api-call session)
                       (delivery-api-session-fetch-head session)
                       git
                       run))

;; ============================================================
;; repository / refresh / origin-main
;; ============================================================

;; The CONFIGURED origin URL (url.<x>.insteadOf rewrites are transport
;; conveniences and never redefine repository identity).
(define (repository git)
  (define origin (string-trim (git (list "config" "--get" "remote.origin.url"))))
  (define match (regexp-match origin-url-rx origin))
  (unless match
    (refuse! "origin must identify a GitHub repository (no arbitrary API host)"))
  (cadr match))

;; Non-forced refresh of the tracking ref, exactly like Python refresh().
(define (refresh git)
  (git (list "fetch" "--no-tags" "origin" "refs/heads/main:refs/remotes/origin/main")))

(define (origin-main git)
  (define main (string-trim (git (list "rev-parse" "refs/remotes/origin/main"))))
  (unless (full-sha? main)
    (refuse! "malformed origin/main head"))
  main)

;; ============================================================
;; PR resolution (resolve_prs_for_branch / existing / merged)
;; ============================================================

;; Owner login of an owner/repo slug. GitHub's pulls head filter is
;; `owner:branch`; the owner/repo form matches nothing (F6).
(define (branch-owner slug)
  (unless (string? slug)
    (refuse! (format "malformed repository slug: ~a" slug)))
  (define parts (string-split slug "/"))
  (unless (and (= 2 (length parts)) (not (string=? (car parts) "")) (not (string=? (cadr parts) "")))
    (refuse! (string-append "malformed repository slug: " slug)))
  (car parts))

(define (pull-head-filter slug branch)
  (format "~a:~a" (branch-owner slug) branch))

;; At most one candidate may exist for a branch at the requested state; the
;; caller fails closed on two instead of silently reusing one.
(define (resolve-prs-for-branch api slug branch state)
  (ensure! (nonblank-string? branch) "invalid branch identity")
  (define candidates
    (api slug (format "pulls?state=~a&head=~a" state (pull-head-filter slug branch))))
  (ensure! (list? candidates) "malformed pull-request response")
  (when (> (length candidates) 1)
    (refuse! (string-append "multiple pull requests already target branch " branch)))
  (and (pair? candidates) (car candidates)))

(define (resolve-existing-pr api slug branch)
  (resolve-prs-for-branch api slug branch "open"))

;; Idempotent merged resume: one CLOSED MERGED PR for the branch, optionally
;; narrowed to exactly the durable expected head. Never guesses among
;; historical PRs.
(define (resolve-merged-pr api slug branch expected-head)
  (ensure! (or (absent? expected-head) (full-sha? expected-head))
           "merged resolution requires a full expected-head SHA")
  (define candidates (api slug (format "pulls?state=all&head=~a" (pull-head-filter slug branch))))
  (ensure! (list? candidates) "malformed pull-request response")
  (define merged
    (for/list ([pr (in-list candidates)]
               #:when (and (hash? pr)
                           (equal? (hash-ref pr 'state #f) "closed")
                           (pr-is-merged? pr)
                           (equal? (dig pr 'head 'ref) branch)))
      pr))
  (define narrowed
    (if (absent? expected-head)
        merged
        (let ([exact (for/list ([pr (in-list merged)]
                                #:when (equal? (dig pr 'head 'sha) expected-head))
                       pr)])
          ;; No exact candidate retains the fail-closed uniqueness rule.
          (if (pair? exact) exact merged))))
  (when (> (length narrowed) 1)
    (refuse! (string-append "multiple merged pull requests already target branch " branch)))
  (and (pair? narrowed) (car narrowed)))

;; Same-repo branch identity: main base, both sides in THIS repository.
(define (validate-pr-identity pr slug)
  (ensure! (hash? pr) "malformed PR response")
  (unless (and (equal? (dig pr 'base 'ref) "main")
               (equal? (dig pr 'base 'repo 'full_name) slug)
               (equal? (dig pr 'head 'repo 'full_name) slug))
    (refuse! "implementation PR must be a same-repository main-base pull request")))

;; ============================================================
;; Receipt tip (ancestry + excluded-evidence-only drift)
;; ============================================================

;; Pin a branch/PR tip to the durable receipt identity. After the receipt head
;; only excluded evidence directories may change; any source drift refuses.
(define (require-receipt-tip git expected-head tip [action "delivery"])
  (cond
    [(absent? expected-head) (void)]
    [else
     (ensure! (full-sha? expected-head) "expected-head must be a full SHA")
     (define common (string-trim (git (list "merge-base" expected-head tip))))
     (unless (equal? common expected-head)
       (refuse!
        (format
         "receipt head ~a is not an ancestor of tip ~a; the wave branch may have been rewritten; re-verify before ~a"
         (short-sha expected-head)
         (short-sha tip)
         action)))
     (define drifted
       (git (list "diff" "--name-only" "--no-renames" (format "~a..~a" expected-head tip) "--")))
     (define foreign
       (for/list ([line (in-list (string-split drifted "\n"))]
                  #:when (let ([path (string-trim line)])
                           (and (not (string=? path ""))
                                (not (for/or ([prefix (in-list excluded-prefixes)])
                                       (string-prefix? path prefix))))))
         (string-trim line)))
     (unless (null? foreign)
       (refuse!
        (format
         "receipt head ~a is verified but later wave-branch commits touch non-evidence paths: ~a; re-verify before ~a"
         (short-sha expected-head)
         (string-join (take foreign (min 3 (length foreign))) ", ")
         action)))]))

;; ============================================================
;; Blobs and the required-check policy
;; ============================================================

;; The blob text at ref:path, or #f when the path is absent. Infrastructure
;; refusals (budget/exec boundary) are re-raised: an absent blob is repository
;; state, a spent budget is not.
(define (blob-or-none git ref path)
  (with-handlers ([exn:fail:delivery-action? (lambda (error)
                                               (if (infrastructure-refusal? (exn-message error))
                                                   (raise error)
                                                   #f))])
    (git (list "show" (format "~a:~a" ref path)))))

;; Parse the policy datum with Racket (never by shelling to python): one
;; self-contained datum, coerced through the same write-json round trip the
;; Python reader used, so `("ci" "lint")` yields the JSON string list.
(define (policy-datum names-text ref)
  (define (malformed)
    (refuse! (string-append "malformed required-check policy at " ref)))
  (with-handlers ([exn:fail? (lambda (_error) (malformed))])
    (define datum
      (parameterize ([read-accept-reader #f]
                     [read-accept-lang #f]
                     [read-accept-graph #f])
        (define in (open-input-string names-text))
        (define first (read in))
        (when (eof-object? first)
          (malformed))
        (unless (eof-object? (read in))
          (malformed))
        first))
    (string->jsexpr (jsexpr-string datum))))

;; Expected required-check name snapshot at a ref. Each name must be a STRING
;; that is non-blank after trimming and the list must be non-empty and unique —
;; but the returned names are NOT trimmed: trimming is only the non-blankness
;; test, so `("ci" "  ci  ")` is a duplicate-free-but-inconsistent policy that
;; is still compared verbatim against branch protection.
(define (policy-names git ref)
  (define data (blob-or-none git ref policy-relative-path))
  (unless data
    (refuse! (string-append "required-check policy is absent at " ref)))
  (define names (policy-datum data ref))
  (unless (and (list? names)
               (pair? names)
               (for/and ([item (in-list names)])
                 (and (string? item) (not (string=? (string-trim item) "")))))
    (refuse! (string-append "malformed required-check policy at " ref)))
  (unless (= (length names) (length (remove-duplicates names)))
    (refuse! (string-append "malformed required-check policy at " ref)))
  names)

;; ============================================================
;; Path identities (binding path / artifact path)
;; ============================================================

(define (posix-parts value)
  (string-split value "/"))

(define (valid-artifact-name? name)
  (and (not (string=? name "")) (not (string=? name ".rktd")) (string-suffix? name ".rktd")))

(define (artifact-path value directory)
  (ensure! (string? value) "missing artifact path")
  (define parts (posix-parts value))
  (define prefix (string-append "docs/reports/" directory "/"))
  (unless (and (not (string-prefix? value "/"))
               (not (member ".." parts))
               (string-prefix? value prefix)
               (valid-artifact-name? (last parts)))
    (refuse! "invalid artifact path"))
  value)

(define (binding-path plan wave [generation 0])
  (ensure! (and (string? plan) (regexp-match? campaign-id-rx plan)) "invalid campaign id")
  (ensure! (and (exact-integer? wave) (>= wave 0)) "invalid wave index")
  (ensure! (and (exact-integer? generation) (>= generation 0)) "invalid binding generation")
  (format "docs/reports/gsd-wave-evidence/~a-w~a~a.rktd"
          plan
          wave
          (if (= generation 0)
              ""
              (format "-r~a" generation))))

;; ============================================================
;; Materialization, digest, binding datum and the strict gate
;; ============================================================

(define (materialize git ref relative root)
  (define listing (string-trim (git (list "ls-tree" ref "--" relative))))
  (define match (regexp-match #px"^([^ ]+)" listing))
  (unless (and match (equal? (cadr match) "100644"))
    (refuse! (string-append "artifact is absent, executable, or a symlink: " relative)))
  (define target (build-path root relative))
  (make-directory* (path-only target))
  (call-with-output-file target
                         #:exists 'truncate
                         (lambda (out)
                           (write-string (git (list "show" (format "~a:~a" ref relative))) out)))
  target)

;; Python digest(): sha256 over `git diff --binary base...head -- . <excludes>`.
(define (content-digest git base head)
  (sha256-hex-of-text (git (append (list "diff" "--binary" (format "~a...~a" base head) "--" ".")
                                   exclude-pathspecs))))

;; One binding datum through the trusted reader (the same module Python spawns).
(define (read-binding-datum run path)
  (define (malformed)
    (refuse! (string-append "malformed binding datum: " (path->string path))))
  (define text
    (with-handlers ([exn:fail? (lambda (_error) (malformed))])
      (run "racket"
           (list (path->string binding-data-script) "read" (path->string path))
           helper-command-timeout)))
  (with-handlers ([exn:fail? (lambda (_error) (malformed))])
    (string->jsexpr text)))

(define (read-binding-json run args refusal)
  (define text
    (with-handlers ([exn:fail? (lambda (_error) (refuse! refusal))])
      (run "racket" args helper-command-timeout)))
  (with-handlers ([exn:fail? (lambda (_error) (refuse! refusal))])
    (string->jsexpr text)))

;; Every portable owned-proof file a strict-gate root must carry for an owned
;; audit trio: the green manifest + raw log, the linked RED manifest + raw log
;; (unless it is the baseline-probe relation, whose probe tree is addressed by
;; `publication-files`) and every declared publication file.
(define (portable-owned-proof-paths authority)
  (unless (hash? authority)
    (refuse! "malformed owned proof authority"))
  (define bundle (hash-ref authority 'bundle))
  (define red (hash-ref authority 'red #f))
  (define refs
    (append (list (hash-ref authority 'reference) (hash-ref authority 'log-reference))
            (if (and (hash? red)
                     (not (equal? (hash-ref red 'relation #f) "baseline-with-test-overlay-v1")))
                (list (hash-ref red 'reference) (hash-ref red 'log-reference))
                '())))
  (define files
    (append (for/list ([ref (in-list refs)])
              (string-append bundle "/" (hash-ref ref 'path)))
            (hash-ref authority 'publication-files '())))
  (for ([relative (in-list files)])
    (ensure! (and (not (absolute-path? (string->path relative)))
                  (not (member ".." (string-split relative "/")))
                  (string-prefix? relative (string-append bundle "/")))
             "unsafe portable proof path"))
  (remove-duplicates files string=?))

;; Materialize the schema-2 trio at `ref` and run the unchanged strict gate on
;; it; returns (values evidence review-datum trio-paths).
;; Python `validate_trio(repo, ref, relative, base, owned_proof_authority=None)`:
;; an owned audit trio is validated against the SAME portable bundle it
;; publishes, materialized from the committed blobs under the gate root, with
;; the trusted authority payload supplied out of band (never selected from
;; model output). Manual-compatibility trios pass no authority at all.
(define (validate-trio ctx ref relative base #:owned-authority [owned-authority #f])
  (define git (delivery-pr-context-git ctx))
  (define run (delivery-pr-context-run ctx))
  (define root (make-temporary-file "q-delivery-validate-~a" 'directory))
  (define repo (delivery-pr-context-repo ctx))
  (dynamic-wind
   void
   (lambda ()
     (define evidence (read-binding-datum run (materialize git ref relative root)))
     (unless (and (hash? evidence) (equal? (hash-ref evidence 'schema-version #f) 2))
       (refuse! "delivery requires a schema-2 hash trio"))
     (define review-relative
       (artifact-path (hash-ref evidence 'review-artifact #f) "gsd-wave-reviews"))
     (define validation-relative
       (artifact-path (hash-ref evidence 'validation-artifact #f) "gsd-wave-validation"))
     (for ([path (in-list (list review-relative validation-relative policy-relative-path))])
       (materialize git ref path root))
     (define owned-payload
       (and owned-authority
            (let ()
              ;; The portable bundle lives beside the trio in the same commit;
              ;; the gate opens only these addressed files.
              (for ([path (in-list (portable-owned-proof-paths owned-authority))])
                (materialize git ref path root))
              ;; The committed authority payload lets push CI (and the staged
              ;; binding gate) validate the audit trio with the exact trusted
              ;; authority the controller used.
              (define bundle-root (build-path root (hash-ref owned-authority 'bundle)))
              (make-directory* bundle-root)
              (call-with-output-file (build-path bundle-root "authority.json")
                                     #:exists 'truncate
                                     (lambda (out)
                                       (write-string (jsexpr->string owned-authority) out)))
              (define payload (build-path root "owned-proof-authority.json"))
              (call-with-output-file payload
                                     #:exists 'truncate
                                     (lambda (out)
                                       (write-string (jsexpr->string owned-authority) out)))
              payload)))
     (define actual (content-digest git base ref))
     ;; KNOWN LIMITATION: the gate's evidence probe resolves commits with real
     ;; git from this temporary root, which has no object store, so a trio whose
     ;; evidence carries an implementation commit is refused here (the owned
     ;; production repair path; manual-compatibility trios do not need it).
     ;; The fix is to materialize into a detached worktree of the repo; that was
     ;; tried and it changed the working tree the status proof inspects, so it
     ;; needs a dedicated design (read-only git dir injection) rather than a
     ;; quick patch. Tracked as the one known-red delivery case.
     (define gate-args
       (append (list (path->string wave-gate-script)
                     (path->string (build-path root relative))
                     "--content-digest"
                     actual
                     "--root"
                     (path->string root)
                     "--policy"
                     (path->string (build-path root policy-relative-path)))
               (if owned-payload
                   (list "--owned-proof-authority"
                         (path->string owned-payload)
                         "--owned-source-repo"
                         (path->string
                          (simplify-path
                           (path->complete-path (delivery-pr-context-repo ctx)))))
                   '())))
     (run "racket" gate-args helper-command-timeout)
     (values evidence
             (read-binding-datum run (build-path root review-relative))
             (list relative review-relative validation-relative)))
   (lambda () (delete-directory/files root))))

;; ============================================================
;; Campaign journal / snapshot (review --campaign-root)
;; ============================================================

(define (absolute-path value)
  (path->complete-path (if (path? value)
                           value
                           (string->path value))))

(define (parents-of p)
  (let loop ([current p]
             [acc '()])
    (define parent (path-only current))
    (cond
      [(not parent) (reverse acc)]
      [(equal? parent current) (reverse acc)]
      [else (loop parent (cons parent acc))])))

(define (exists-any? p)
  (or (file-exists? p) (directory-exists? p) (link-exists? p)))

(define (campaign-journal-root repo campaign-root)
  (cond
    [campaign-root
     (define root (absolute-path campaign-root))
     (when (for/or ([p (in-list (cons root (parents-of root)))])
             (link-exists? p))
       (refuse! "symlinked campaign root refused"))
     (simplify-path root)]
    [else
     (define repo-path (simplify-path (absolute-path repo)))
     (define name (file-name-from-path repo-path))
     (define parent (path-only repo-path))
     (cond
       [(and (equal? name (string->path "q"))
             parent
             (directory-exists? (build-path parent ".planning")))
        (when (exists-any? (build-path repo-path ".planning"))
          (refuse! "ambiguous campaign roots; explicit --campaign-root required"))
        parent]
       [else repo-path])]))

(define (campaign-journal ctx repo plan wave campaign-root)
  (define root (campaign-journal-root repo campaign-root))
  (define data
    (read-binding-json (delivery-pr-context-run ctx)
                       (list (path->string binding-data-script)
                             "journal"
                             (path->string root)
                             plan
                             (number->string wave))
                       "invalid coordinator journal response"))
  (ensure! (or (eq? data #f) (hash? data)) "invalid coordinator journal response")
  data)

(define (journal-generation ctx repo plan wave campaign-root)
  (ensure! (and (string? plan) (regexp-match? campaign-id-rx plan)) "invalid campaign id")
  (ensure! (and (exact-integer? wave) (>= wave 0)) "invalid wave index")
  (define data (campaign-journal ctx repo plan wave campaign-root))
  (cond
    [(eq? data #f) 0]
    [else
     (define history (hash-ref data 'receipt-history '()))
     (ensure! (list? history) "invalid coordinator journal response")
     (length history)]))

(define (snapshot-facts ctx campaign-root plan wave)
  (unless (and campaign-root (directory-exists? campaign-root) (not (link-exists? campaign-root)))
    (refuse! "prepare requires --campaign-root with the campaign working tree"))
  (binding-path plan wave)
  (define facts
    (read-binding-json (delivery-pr-context-run ctx)
                       (list (path->string binding-data-script)
                             "snapshot"
                             (path->string campaign-root)
                             plan
                             (number->string wave))
                       "malformed campaign snapshot facts"))
  (unless (and (hash? facts)
               (equal? (hash-ref facts 'status #f) "ok")
               (equal? (hash-ref facts 'plan-id #f) plan)
               (equal? (hash-ref facts 'wave #f) wave))
    (refuse! "campaign snapshot does not verify for this plan/wave"))
  facts)

(define declared-dirs
  (hasheq 'evidence "gsd-wave-evidence" 'review "gsd-wave-reviews" 'validation "gsd-wave-validation"))

(define (declared-outputs facts repo)
  (define declared (and (hash? facts) (hash-ref facts 'declared #f)))
  (cond
    [(not (hash? declared)) #f]
    [else
     (define groups
       (for/hash ([kind (in-list '(evidence review validation))])
         (define raw (hash-ref declared kind #f))
         (values kind
                 (cond
                   [(absent? raw) '()]
                   [(list? raw) raw]
                   [else
                    (refuse! (string-append "malformed declared wave output for "
                                            (symbol->string kind)))]))))
     (cond
       [(for/and ([kind (in-list '(evidence review validation))])
          (null? (hash-ref groups kind)))
        #f]
       [else
        (unless (for/and ([kind (in-list '(evidence review validation))])
                  (= 1 (length (hash-ref groups kind))))
          (refuse!
           "frozen wave document must declare exactly one evidence, review and validation output; filename guessing is refused"))
        (define repo-name (path->string (file-name-from-path (simplify-path (absolute-path repo)))))
        (for/fold ([result (hash)]) ([kind (in-list '(evidence review validation))])
          (define path (car (hash-ref groups kind)))
          (ensure! (nonblank-string? path) "malformed declared wave output")
          (define normalized
            (cond
              [(string-prefix? path "q/")
               (unless (equal? repo-name "q")
                 (refuse! "declared output uses a q/ prefix but the resolved repo is not q"))
               (substring path 2)]
              [else path]))
          (hash-set result kind (artifact-path normalized (hash-ref declared-dirs kind))))])]))

(define (repair-generation-path relative generation)
  (ensure! (and (exact-integer? generation) (>= generation 0)) "invalid binding generation")
  (cond
    [(= generation 0) relative]
    [else
     (define name (last (posix-parts relative)))
     (unless (and (string-suffix? name ".rktd") (not (string=? name ".rktd")))
       (refuse! "multiple/conflicting declared output path"))
     (string-append (substring relative 0 (- (string-length relative) 5))
                    (format "-r~a.rktd" generation))]))

(define (active-declared-outputs facts repo plan wave generation)
  (define declared (declared-outputs facts repo))
  (cond
    [(not declared)
     (define suffix
       (if (= generation 0)
           ""
           (format "-r~a" generation)))
     (hasheq 'evidence
             (binding-path plan wave generation)
             'review
             (format "docs/reports/gsd-wave-reviews/~a-w~a~a.rktd" plan wave suffix)
             'validation
             (format "docs/reports/gsd-wave-validation/~a-w~a~a.rktd" plan wave suffix))]
    [(= generation 0) declared]
    [else
     (for/hash ([(kind path) (in-hash declared)])
       (values kind (repair-generation-path path generation)))]))

;; ============================================================
;; resolve-pr
;; ============================================================

(define (delivery-resolve-pr! repo
                              plan
                              wave
                              branch
                              #:expected-head [expected-head #f]
                              #:context [context #f])
  (define ctx (or context (make-delivery-pr-context repo)))
  (define git (delivery-pr-context-git ctx))
  (define api (delivery-pr-context-api ctx))
  (define slug (repository git))
  (refresh git)
  (define existing (resolve-existing-pr api slug branch))
  (define-values (pr merged-resume?)
    (cond
      [existing (values existing #f)]
      [(absent? expected-head) (values #f #f)]
      [else
       (define resumed (resolve-merged-pr api slug branch expected-head))
       (values resumed (and resumed #t))]))
  (cond
    [(not pr) (hasheq 'status "none" 'plan-id (json-null plan) 'wave (json-null wave) 'branch branch)]
    [else
     (define number (and (hash? pr) (hash-ref pr 'number #f)))
     (unless (and (exact-integer? number) (> number 0))
       (refuse! "resolved pull request has no usable number"))
     (define head (and (hash? pr) (dig pr 'head 'sha)))
     (unless (absent? expected-head)
       (ensure! (hash? pr) "malformed pull-request response")
       (unless (equal? (hash-ref pr 'state #f) (if merged-resume? "closed" "open"))
         (refuse! "resolved pull request has an unexpected state"))
       (unless (full-sha? head)
         (refuse! "resolved pull request has no full head SHA"))
       (unless (equal? (dig pr 'head 'ref) branch)
         (refuse! "resolved pull request branch does not match expected branch"))
       (validate-pr-identity pr slug)
       (define fetched ((delivery-pr-context-fetch-head ctx) number))
       (unless (equal? fetched head)
         (refuse! "resolved pull-request head does not match fetched ref"))
       (require-receipt-tip git expected-head head "merge"))
     (hasheq 'status
             "resolved"
             'pr
             number
             'plan-id
             (json-null plan)
             'wave
             (json-null wave)
             'branch
             branch
             'head
             (json-null head))]))

;; ============================================================
;; pr-ci
;; ============================================================

(define (delivery-pr-ci! repo
                         number
                         expected-branch
                         #:expected-head [expected-head #f]
                         #:context [context #f])
  (unless (and (exact-integer? number) (> number 0))
    (refuse! "pr-ci requires --pr"))
  (unless (nonblank-string? expected-branch)
    (refuse! "pr-ci requires --expected-branch"))
  (define ctx (or context (make-delivery-pr-context repo)))
  (define git (delivery-pr-context-git ctx))
  (define api (delivery-pr-context-api ctx))
  (define slug (repository git))
  (refresh git)
  (define main (origin-main git))
  ;; A post-mutation-style proof: the PR is re-read WITHOUT the memo.
  (define pr (api slug (format "pulls/~a" number) #:refresh? #t))
  (ensure! (hash? pr) "malformed pull-request response")
  (define head (dig pr 'head 'sha))
  ;; The open-PR contract is loosened ONLY by the explicit receipt-bound
  ;; merged-resume identity (a closed MERGED PR at exactly the receipt head).
  (define merged-resume?
    (and (not (absent? expected-head))
         (equal? (hash-ref pr 'state #f) "closed")
         (pr-is-merged? pr)
         (equal? head expected-head)))
  (unless (or (equal? (hash-ref pr 'state #f) "open") merged-resume?)
    (refuse! "pull request is not open"))
  (validate-pr-identity pr slug)
  (unless (full-sha? head)
    (refuse! "pull request has no full head SHA"))
  (unless (equal? (dig pr 'head 'ref) expected-branch)
    (refuse! "pull request branch does not match expected branch"))
  (define fetched ((delivery-pr-context-fetch-head ctx) number))
  (unless (equal? fetched head)
    (refuse! "pull-request head does not match fetched ref"))
  (require-receipt-tip git expected-head head "delivery")
  (define names (policy-names git main))
  ((delivery-api-session-protection (delivery-pr-context-session ctx)) slug names)
  (for ([name (in-list names)])
    ((delivery-api-session-trusted-check (delivery-pr-context-session ctx)) slug
                                                                            head
                                                                            name
                                                                            "pull_request"
                                                                            #:branch expected-branch))
  (hasheq 'status "green" 'pr number 'branch expected-branch 'head head))

;; ============================================================
;; review
;; ============================================================

;; --campaign-root arrives as TEXT from this module's own CLI (read-flag-value
;; yields strings) and as a PATH from the coordinator. Normalize ONCE at the
;; action boundary: every downstream helper (journal/snapshot subprocess argv)
;; then needs `path->string`, which raises a contract error for a raw string.
(define (campaign-root-path value)
  (cond
    [(not value) #f]
    [(path? value) value]
    [(string? value)
     (when (string=? (string-trim value) "")
       (refuse! "invalid --campaign-root"))
     (string->path value)]
    [else (refuse! (format "invalid --campaign-root: ~a" value))]))

(define (review-relative-path ctx repo plan wave campaign-root generation)
  (cond
    [campaign-root
     (define actual-generation (journal-generation ctx repo plan wave campaign-root))
     (when generation
       (unless (and (exact-integer? generation) (>= generation 0) (= generation actual-generation))
         (refuse! "review binding generation does not match the authoritative journal")))
     (define facts (snapshot-facts ctx campaign-root plan wave))
     (hash-ref (active-declared-outputs facts repo plan wave actual-generation) 'evidence)]
    [else
     (unless (or (not generation) (equal? generation 0))
       (refuse! "review --generation requires --campaign-root for authoritative resolution"))
     (binding-path plan wave)]))

(define (delivery-review! repo
                          plan
                          wave
                          expected-head
                          expected-branch
                          #:campaign-root [campaign-root #f]
                          #:generation [generation #f]
                          #:context [context #f])
  (unless (full-sha? expected-head)
    (refuse! "review requires --expected-head (the durable receipt head)"))
  (unless (nonblank-string? expected-branch)
    (refuse! "review requires --expected-branch (the durable receipt branch)"))
  (define ctx (or context (make-delivery-pr-context repo)))
  (define git (delivery-pr-context-git ctx))
  (define run (delivery-pr-context-run ctx))
  ;; Accept a string campaign root (this module's CLI) and a path (coordinator).
  (define campaign-dir (campaign-root-path campaign-root))
  (define relative (review-relative-path ctx repo plan wave campaign-dir generation))
  (refresh git)
  (define main (origin-main git))
  ;; Materialize the receipt head's objects: fetch the wave branch tip and
  ;; require the receipt head to be on it (a rewritten branch refuses here).
  (git (list "fetch" "--no-tags" "origin" (string-append wave-branch-prefix expected-branch)))
  (define tip (string-trim (git (list "rev-parse" "FETCH_HEAD"))))
  (unless (full-sha? tip)
    (refuse! "malformed fetched receipt branch tip"))
  (require-receipt-tip git expected-head tip "review")
  (unless (blob-or-none git tip relative)
    (refuse!
     (format
      "binding trio ~a is absent on branch ~a (tip ~a); a genuine independent reviewer must commit it after Verify"
      relative
      expected-branch
      (short-sha tip))))
  (define evidence-root (make-temporary-file "q-delivery-review-~a" 'directory))
  (define review-relative
    (dynamic-wind
     void
     (lambda ()
       (define evidence (read-binding-datum run (materialize git tip relative evidence-root)))
       (unless (and (hash? evidence) (equal? (hash-ref evidence 'schema-version #f) 2))
         (refuse! "review requires a schema-2 evidence record at the branch tip"))
       (unless (equal? (hash-ref evidence 'wave #f) (format "W~a" wave))
         (refuse! (format "evidence record belongs to another wave (expected W~a)" wave)))
       (define declared-plan (hash-ref evidence 'plan-id #f))
       (unless (or (absent? declared-plan) (equal? declared-plan plan))
         (refuse! "evidence record explicitly names another campaign"))
       (artifact-path (hash-ref evidence 'review-artifact #f) "gsd-wave-reviews"))
     (lambda () (delete-directory/files evidence-root))))
  (cond
    [(not (blob-or-none git tip review-relative))
     (hasheq
      'status
      "awaiting-review"
      'plan-id
      (json-null plan)
      'wave
      (json-null wave)
      'head
      expected-head
      'review-artifact
      review-relative
      'reason
      "review artifact absent at the receipt identity; a genuine independent non-author reviewer must produce it")]
    [else
     (validate-trio ctx tip relative main)
     (define bind-root (make-temporary-file "q-delivery-review-bind-~a" 'directory))
     (define review-datum
       (dynamic-wind void
                     (lambda ()
                       (read-binding-datum run (materialize git tip review-relative bind-root)))
                     (lambda () (delete-directory/files bind-root))))
     (define reviewed (and (hash? review-datum) (hash-ref review-datum 'reviewed-sha #f)))
     (unless (full-sha? reviewed)
       (refuse! "review record carries no reviewed-sha"))
     (unless (equal? reviewed expected-head)
       (require-receipt-tip git reviewed expected-head "review"))
     (hasheq 'status
             "reviewed"
             'plan-id
             (json-null plan)
             'wave
             (json-null wave)
             'head
             expected-head
             'reviewed-sha
             reviewed
             'review-artifact
             review-relative)]))

;; ============================================================
;; CLI surface (same JSON shapes as the Python actions)
;; ============================================================

(define (flag-integer flags name)
  (define raw (hash-ref flags name #f))
  (and raw
       (let ([value (string->number raw)])
         (if (and value (exact-integer? value))
             value
             (refuse! (format "invalid integer for ~a: ~a" name raw))))))

(define (read-ladder-action! argv)
  (define action (and (pair? argv) (car argv)))
  (define flags
    (read-flag-value (if (pair? argv)
                         (cdr argv)
                         '())))
  (define repo (hash-ref flags "--repo" #f))
  (define plan (hash-ref flags "--plan" #f))
  (define wave (flag-integer flags "--wave"))
  (define expected-branch (hash-ref flags "--expected-branch" #f))
  (define expected-head (hash-ref flags "--expected-head" #f))
  (unless repo
    (refuse! (format "~a requires --repo" (if action action ""))))
  (cond
    [(equal? action "resolve-pr")
     (unless expected-branch
       (refuse! "resolve-pr requires --expected-branch"))
     (delivery-resolve-pr! repo plan wave expected-branch #:expected-head expected-head)]
    [(equal? action "pr-ci")
     (define number (flag-integer flags "--pr"))
     (unless (and number expected-branch)
       (refuse! "pr-ci requires --pr and --expected-branch"))
     (delivery-pr-ci! repo number expected-branch #:expected-head expected-head)]
    [(equal? action "review")
     (unless (and expected-head expected-branch)
       (refuse! "review requires --expected-head and --expected-branch"))
     (delivery-review! repo
                       plan
                       wave
                       expected-head
                       expected-branch
                       #:campaign-root (hash-ref flags "--campaign-root" #f)
                       #:generation (flag-integer flags "--generation"))]
    [else
     (refuse!
      (format "unsupported action ~a; this module implements resolve-pr, pr-ci and review only"
              (or action "")))]))

;; Success -> JSON on stdout, exit 0. Refusal -> delivery-pending JSON, exit 2
;; (the exact shape delivery-coordinator.rkt's typed-stop parser expects).
(define (delivery-read-ladder-main argv)
  (with-handlers ([exn:fail? (lambda (error)
                               (printf "~a\n" (delivery-pending-json (exn-message error)))
                               (exit 2))])
    (printf "~a\n" (jsexpr-string (read-ladder-action! argv)))
    (exit 0)))

(module+ main
  (delivery-read-ladder-main (vector->list (current-command-line-arguments))))
