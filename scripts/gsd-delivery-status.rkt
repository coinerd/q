#lang racket/base

;; Phase B slice S6 — native port of the delivery-proof `status` and the
;; read-only `governance` actions of scripts/gsd-delivery.py.
;;
;; Scope law: this module implements exactly two actions and mutates nothing.
;; It is NOT wired into extensions/gsd/delivery-coordinator.rkt, it does not
;; edit scripts/gsd-delivery.py, and the Python controller stays the production
;; path until an explicit later slice says otherwise. It reuses the S3 read
;; layer (scripts/gsd-delivery-api.rkt: make-delivery-api-session,
;; validate-merge, fetch-head, protection, trusted-check), the S4 read-only
;; ladder helpers (scripts/gsd-delivery-pr.rkt: make-delivery-pr-context,
;; repository, refresh, binding-path, artifact-path, resolve-merged-pr,
;; excluded-prefixes, full-sha?) and the shared bounded seam
;; (scripts/gsd-delivery-git.rkt). None of those modules is modified.
;;
;; ONE-BUDGET LAW (B2), inherited: an action builds ONE delivery-pr-context,
;; which owns ONE (budget-seconds, start-ms) pair and builds the gh runner, the
;; repo-git command, the `racket` helper command and fetch-head from that SAME
;; pair. Every gh read, every git read and every helper subprocess therefore
;; draws down ONE decreasing budget exactly like Python's process-wide
;; remaining_budget(); a second runner would silently grant a fresh budget and
;; is never created here.
;;
;; STALENESS AUDIT (the status proof is a security gate). Python's `status` and
;; `governance` never pass `_refresh=True` to `api()`, so this port must not
;; either — a `#:refresh?` here would spawn a duplicate gh request and could
;; disagree with the Python ladder. The only freshness guarantees those actions
;; rely on are (a) `refresh(repo)` — a fresh `git fetch` of
;; refs/heads/main:refs/remotes/origin/main, re-executed here, and (b)
;; `fetch_head(repo, n)` — a fresh `git fetch` of refs/pull/<n>/head, whose
;; FETCH_HEAD is asserted against the binding head. Every gh read is memoized
;; per session exactly like Python's per-process `_API_CACHE`/`_CHECKS_CACHE`,
;; so ONE context serves exactly ONE action: reusing a context across two
;; actions would serve a pre-action check snapshot to the second one.
;;
;; Parity law (the Python functions are the contract; refusal strings are
;; byte-identical so the coordinator's reason chain does not change):
;;
;;   status(repo, plan, wave, campaign_root) -> hash
;;     Order is load-bearing and preserved: campaign journal root, binding
;;     generation (append-only receipt-history length), the authoritative
;;     journal + attempt-bound Verify receipt, the binding path, the repository
;;     slug, a fresh origin/main, the publication commit for the binding path,
;;     a single-parent publication, the unchanged strict trio gate, the
;;     campaign/wave/merge identity, full implementation SHAs, receipt
;;     head/branch authentication, implementation-sha == merge-sha, the
;;     implementation PR identity, `changed_evidence == [binding]` (governance
;;     green-skip rule-out), the merged same-repo publication PR, the fetched
;;     PR/merge identity, the implementation head fetched from refs/pull/N/head,
;;     merge-tree == PR-head-tree, the required-check name snapshot from the
;;     binding plus branch protection and one trusted Actions check per name at
;;     that head, ONE trusted governance check on the publication push to main,
;;     the trio bytes unchanged in publication/main/local, and finally
;;     merge -> publication -> HEAD ancestry. Payload keys are exactly
;;     {'status':'delivered','merge-sha','publication-sha','plan-id','wave',
;;      'delivery-head-sha','delivery-branch','binding-generation',
;;      'attempt-id','attempt-fence'}.
;;
;;   governance(repo, plan, wave, expected_branch) -> hash
;;     Read-only publication verification: the branch must be the deterministic
;;     binding branch (base or -r<n> republication), origin/main must be a full
;;     SHA, exactly one CLOSED MERGED same-repo main-base PR must exist for it
;;     with complete identity and a single-parent squash merge commit that is an
;;     ancestor of origin/main, and the merge commit must carry ONE trusted
;;     gsd-governance Actions check from a `push` run on `main`. It never
;;     mutates, never merges and never writes a journal.
;;
;; Documented deviations (all fail closed, never open):
;;   * `validate_trio` is NOT exported by scripts/gsd-delivery-pr.rkt (that
;;     module is read-only in this slice, and Racket cannot reach an unexported
;;     binding), so the trio proof is mirrored here: the same materialization,
;;     the same excluded-path content digest and the SAME unchanged strict gate
;;     subprocess (scripts/gsd-wave-gate.rkt, argv identical to the read
;;     ladder's). No gate semantics are re-implemented.
;;   * a helper (`racket scripts/gsd-binding-data.rkt`, the strict gate) that
;;     fails raises the seam's typed refusal verbatim, exactly like Python's
;;     `command()`; only an unreadable/JSON-broken payload becomes the
;;     documented malformed-payload refusal. (The read ladder converts seam
;;     refusals too; the Python contract is what is ported here.)
;;   * git blobs are read as UTF-8 text (pr.rkt parity) and the local artifact
;;     is compared as bytes re-encoded from that text, so a NON-UTF-8 committed
;;     artifact can only ever produce a typed refusal, never a false proof.
;;   * `campaign_journal_root`'s repo-derived (no --campaign-root) branch is
;;     ported for parity, but the CLI refuses without --campaign-root exactly
;;     like the Python `main()`: 'status requires --campaign-root'.
;;   * JSON booleans/floats: `exact-integer?` is stricter than Python's
;;     `isinstance(x, int)`, so a `true`/`1.0` PR number or attempt-fence fails
;;     closed here instead of being coerced.

(require json
         racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         file/sha1
         openssl
         "gsd-delivery-git.rkt"
         "gsd-delivery-api.rkt"
         "gsd-delivery-pr.rkt")

(provide delivery-status!
         delivery-governance!
         delivery-status-action!
         delivery-status-main
         ;; helpers (exported for parity tests and later slices)
         binding-branch
         campaign-journal-root
         campaign-journal
         journal-generation
         changed-evidence
         tree-of
         publication-pr
         validate-trio
         governance-check-name)

;; ============================================================
;; Constants (mirror scripts/gsd-delivery.py)
;; ============================================================

(define-runtime-path this-source "gsd-delivery-status.rkt")
(define scripts-dir (path-only this-source))
(define binding-data-script (build-path scripts-dir "gsd-binding-data.rkt"))
(define wave-gate-script (build-path scripts-dir "gsd-wave-gate.rkt"))

(define policy-relative-path "scripts/required-pr-checks.policy")

;; The mandatory governance check the protected publication push must carry.
(define governance-check-name "gsd-governance")

;; Protected branch names a receipt or publication may never use as a wave
;; branch (mirrors Python's `branch not in ('main', 'master')`).
(define protected-branch-names '("main" "master"))

;; Python reads the binding evidence out of docs/reports/gsd-wave-evidence/.
(define evidence-directory "docs/reports/gsd-wave-evidence/")

(define campaign-id-rx #px"^[0-9a-f]{64}$")
(define helper-command-timeout 60.0)

;; Python EXCLUDES: the same excluded directories as git pathspec magic. The
;; prefixes come from the read ladder (its own export), so widening one side
;; without the other is impossible.
(define exclude-pathspecs
  (for/list ([prefix (in-list excluded-prefixes)])
    (string-append ":(exclude)" prefix "**")))

;; ============================================================
;; Small helpers (same semantics as the S3/S4/S5 modules)
;; ============================================================

;; Python `require(ok, reason)`.
(define (ensure! ok message)
  (unless ok
    (refuse! message)))

(define (nonblank-string? value)
  (and (string? value) (not (string=? (string-trim value) ""))))

;; Python dig: any non-hash along the path yields None (#f).
(define (dig value key . keys)
  (for/fold ([current (and (hash? value) (hash-ref value key #f))]) ([next (in-list keys)])
    (if (hash? current)
        (hash-ref current next #f)
        #f)))

;; Python `x is None` for a JSON null that Racket read as the symbol 'null.
(define (absent? value)
  (or (not value) (eq? value 'null)))

(define (json-null value)
  (if (absent? value) 'null value))

(define (as-path value)
  (cond
    [(path? value) value]
    [else (string->path value)]))

(define (sha256-hex-of-text text)
  (bytes->hex-string (sha256-bytes (string->bytes/utf-8 text))))

;; ============================================================
;; Campaign journal root / journal / generation
;; ============================================================

(define (path-and-parents p)
  ;; Every ancestor of `p`, nearest first and including `p` itself (Python's
  ;; `(root, *root.parents)`), so a symlinked ancestor cannot host a journal.
  (let loop ([current p]
             [acc '()])
    (cond
      [(not current) (reverse acc)]
      [(equal? current (path-only current)) (reverse (cons current acc))]
      [else (loop (path-only current) (cons current acc))])))

;; Python campaign_journal_root: an explicit campaign root must be a real
;; (never symlinked, in any component) directory; without one the repository
;; directory is the root, and a `q` checkout inside a campaign tree is
;; ambiguous unless it carries no .planning of its own.
(define (campaign-journal-root repo campaign-root)
  (cond
    [(not (absent? campaign-root))
     (define root (simplify-path (path->complete-path (as-path campaign-root))))
     (when (for/or ([p (in-list (path-and-parents root))])
             (link-exists? p))
       (refuse! "symlinked campaign root refused"))
     root]
    [else
     (define repo-path
       (simplify-path (path->complete-path (as-path (or repo (refuse! "status requires --repo"))))))
     (define parent (path-only repo-path))
     (cond
       [(and (equal? (file-name-from-path repo-path) (string->path "q"))
             parent
             (directory-exists? (build-path parent ".planning")))
        (define local (build-path repo-path ".planning"))
        (when (or (file-exists? local) (directory-exists? local) (link-exists? local))
          (refuse! "ambiguous campaign roots; explicit --campaign-root required"))
        parent]
       [else repo-path])]))

;; The trusted journal loader (schema, receipt/history and symlink validation
;; all live in gsd-binding-data.rkt, the same module Python spawns). A missing
;; journal is JSON `false` -> #f; a MALFORMED journal makes the helper exit
;; non-zero, so the seam's typed failure propagates exactly like Python's
;; `command()`; only an unparseable payload becomes the documented refusal.
(define (campaign-journal run root plan wave)
  (define text
    (run "racket"
         (list (path->string binding-data-script)
               "journal"
               (path->string root)
               plan
               (number->string wave))
         helper-command-timeout))
  (define data
    (with-handlers ([exn:fail? (lambda (_error) (refuse! "invalid coordinator journal response"))])
      (string->jsexpr text)))
  (ensure! (or (eq? data #f) (hash? data)) "invalid coordinator journal response")
  data)

;; Repair-tail republication generation: the append-only receipt-history length
;; (0 for a missing journal). A malformed history fails closed instead of
;; guessing a branch that governance would later reject.
(define (journal-generation run root plan wave)
  (binding-path plan wave)
  (ensure! (and (exact-integer? wave) (>= wave 0)) "invalid wave index")
  (define data (campaign-journal run root plan wave))
  (cond
    [(eq? data #f) 0]
    [else
     (define history (hash-ref data 'receipt-history '()))
     (ensure! (list? history) "invalid coordinator journal response")
     (length history)]))

;; ============================================================
;; Publication / tree / changed-evidence reads
;; ============================================================

;; Python's `malformed origin/main head` guard on the freshly fetched tracking
;; ref (the read ladder's own accessor is private to that module).
(define (origin-main git)
  (define main (string-trim (git (list "rev-parse" "refs/remotes/origin/main"))))
  (unless (full-sha? main)
    (refuse! "malformed origin/main head"))
  main)

(define (tree-of git ref)
  (define tree (string-trim (git (list "rev-parse" (format "~a^{tree}" ref)))))
  (unless (full-sha? tree)
    (refuse! (string-append "malformed tree for " ref)))
  tree)

;; Every path the publication commit changed under the evidence directory.
(define (changed-evidence git publication)
  (define out
    (git (list "diff-tree"
               "--no-commit-id"
               "--name-only"
               "--no-renames"
               "-r"
               publication
               "--"
               evidence-directory)))
  (for/list ([line (in-list (string-split out "\n"))]
             #:when (not (string=? (string-trim line) "")))
    (string-trim line)))

;; Exactly one merged same-repo main-base PR whose merge commit IS the
;; publication. The commit-association endpoint reports `merged: null` for a
;; genuine squash merge, so an otherwise complete identity falls back to the
;; authoritative pulls/<n> endpoint instead of being refused.
(define (publication-pr api slug publication)
  (define prs (api slug (format "commits/~a/pulls" publication)))
  (unless (and (list? prs) (pair? prs))
    (refuse! "binding publication is not associated with a PR"))
  (define same
    (for/list ([pr (in-list prs)]
               #:when (and (hash? pr)
                           (equal? (dig pr 'base 'ref) "main")
                           (equal? (dig pr 'base 'repo 'full_name) slug)
                           (equal? (dig pr 'head 'repo 'full_name) slug)
                           (equal? (hash-ref pr 'merge_commit_sha #f) publication)))
      pr))
  (unless (= 1 (length same))
    (refuse! "binding publication must be exactly one merged same-repo PR"))
  (define pr (car same))
  (define merged
    (or (equal? (hash-ref pr 'merged #f) #t)
        (and (equal? (hash-ref pr 'state #f) "closed") (not (absent? (hash-ref pr 'merged_at #f))))))
  (define proven-merged
    (cond
      [merged #t]
      [else
       (define number (hash-ref pr 'number #f))
       (and (exact-integer? number) (pr-is-merged? (api slug (format "pulls/~a" number))))]))
  (unless proven-merged
    (refuse! "binding publication must be exactly one merged same-repo PR"))
  pr)

;; ============================================================
;; The unchanged strict gate over materialized committed blobs
;; ============================================================

;; `materialize`, `content-digest`, `read-binding-datum` and `validate-trio`
;; live in the read ladder (`gsd-delivery-pr.rkt`) and are imported above:
;; Python keeps ONE `validate_trio`, and so does this port (a second copy here
;; would silently diverge — the read ladder's copy is also the one that can
;; authenticate a portable owned-proof bundle). `validate-trio` is re-exported
;; below for the parity tests that pinned the status ladder's readback.

;; ============================================================
;; status
;; ============================================================


;; ============================================================
;; status
;; ============================================================

;; A receipt is only usable when it is attempt-bound and names a non-protected
;; branch; a bare head/branch pair is not a Verify receipt.
(define (attempt-bound-receipt? receipt)
  (and (hash? receipt)
       (full-sha? (hash-ref receipt 'head #f))
       (nonblank-string? (hash-ref receipt 'branch #f))
       (not (member (hash-ref receipt 'branch #f) protected-branch-names))
       (nonblank-string? (hash-ref receipt 'attempt-id #f))
       (exact-nonnegative-integer? (hash-ref receipt 'attempt-fence #f))))

;; The authoritative coordinator journal receipt for this plan/wave, or #f when
;; no campaign root was supplied (the CLI refuses that for status).
(define (journal-receipt run root plan wave campaign-root)
  (cond
    [(absent? campaign-root) #f]
    [else
     (define journal-path
       (build-path root ".planning" "campaigns" plan (format "coordinator-w~a.json" wave)))
     ;; A symlink is refused even when it resolves to a regular file.
     (unless (and (file-exists? journal-path) (not (link-exists? journal-path)))
       (refuse! "delivery proof requires the authoritative coordinator journal"))
     (define data (campaign-journal run root plan wave))
     (ensure! (hash? data) "delivery proof requires a valid coordinator journal")
     (unless (and (equal? (hash-ref data 'plan-id #f) plan) (equal? (hash-ref data 'wave #f) wave))
       (refuse! "delivery journal campaign/wave mismatch"))
     (define receipt (hash-ref data 'receipt #f))
     (unless (attempt-bound-receipt? receipt)
       (refuse! "delivery proof requires an attempt-bound Verify receipt"))
     receipt]))

(define (delivery-status! repo plan wave #:campaign-root [campaign-root #f] #:context [context #f])
  (define root (campaign-journal-root repo campaign-root))
  (define ctx (or context (make-delivery-pr-context repo)))
  (define git (delivery-pr-context-git ctx))
  (define api (delivery-pr-context-api ctx))
  (define session (delivery-pr-context-session ctx))
  (define run (delivery-pr-context-run ctx))
  (define generation (journal-generation run root plan wave))
  (define receipt (journal-receipt run root plan wave campaign-root))
  (define relative (binding-path plan wave generation))
  (define slug (repository git))
  (refresh git)
  (define main (origin-main git))
  (define publication (string-trim (git (list "log" "-1" "--format=%H" main "--" relative))))
  (unless (nonblank-string? publication)
    (refuse! (format "W~a verified; delivery pending: protected campaign binding is absent" wave)))
  (unless (full-sha? publication)
    (refuse! "malformed publication SHA"))
  (define parents
    (for/list ([token (in-list (string-split
                                (string-trim (git (list "rev-list" "--parents" "-n" "1" publication)))
                                " "))]
               #:unless (string=? token ""))
      token))
  (unless (and (= 2 (length parents)) (equal? (car parents) publication))
    (refuse! "binding publication is not a single-parent protected commit"))
  (define-values (evidence _review paths) (validate-trio ctx publication relative (cadr parents)))
  (unless (and (equal? (hash-ref evidence 'plan-id #f) plan)
               (equal? (hash-ref evidence 'wave #f) (format "W~a" wave))
               (equal? (hash-ref evidence 'merge-method #f) "squash"))
    (refuse! "campaign/wave/merge identity mismatch"))
  (define merge (hash-ref evidence 'merge-sha #f))
  (define head (hash-ref evidence 'delivery-head-sha #f))
  (define number (hash-ref evidence 'delivery-pr #f))
  (define wave-branch (hash-ref evidence 'wave-branch #f))
  (unless (and (full-sha? merge) (full-sha? head))
    (refuse! "binding must carry full implementation SHAs"))
  (when receipt
    (unless (and (equal? head (hash-ref receipt 'head #f))
                 (equal? wave-branch (hash-ref receipt 'branch #f)))
      (refuse! "published binding does not authenticate the current receipt head/branch")))
  (unless (equal? (hash-ref evidence 'implementation-sha #f) merge)
    (refuse! "binding implementation-sha differs from merge-sha (squash semantics violated)"))
  (unless (and (exact-integer? number) (> number 0))
    (refuse! "missing implementation PR identity"))
  (unless (nonblank-string? wave-branch)
    (refuse! "missing implementation wave branch"))
  (unless (equal? (changed-evidence git publication) (list relative))
    (refuse! (string-append "publication must change exactly the current binding evidence "
                            "(possible governance green-skip refused)")))
  (publication-pr api slug publication)
  (define pr (api slug (format "pulls/~a" number)))
  (validate-merge pr (api slug (format "commits/~a" merge)) slug merge head)
  ;; The session's own fetch-head (the context accessor is not exported): the
  ;; SAME bounded runner and the SAME budget.
  (define fetched ((delivery-api-session-fetch-head session) number))
  (unless (equal? fetched head)
    (refuse! "fetched implementation head does not match binding delivery-head-sha"))
  (unless (equal? (tree-of git merge) (tree-of git fetched))
    (refuse! "implementation merge tree differs from PR head tree (not a clean squash)"))
  (define names (hash-ref evidence 'required-checks #f))
  (unless (and (list? names)
               (pair? names)
               (for/and ([name (in-list names)])
                 (nonblank-string? name)))
    (refuse! "binding required-checks are malformed"))
  ((delivery-api-session-protection session) slug names)
  (for ([name (in-list names)])
    ((delivery-api-session-trusted-check session) slug head name "pull_request" #:branch wave-branch))
  ;; The governance run is bound to the publication PUSH to main, not to the
  ;; implementation PR and not to any later main tip.
  ((delivery-api-session-trusted-check session) slug
                                                publication
                                                governance-check-name
                                                "push"
                                                #:branch "main")
  (for ([path (in-list paths)])
    (define published (git (list "show" (format "~a:~a" publication path))))
    (unless (equal? (git (list "show" (format "~a:~a" main path))) published)
      (refuse! "binding trio changed since governance publication"))
    (unless (equal? (git (list "show" (format "HEAD:~a" path))) published)
      (refuse! "delivery sync pending: current checkout does not contain published trio"))
    (define local (build-path repo path))
    (unless (and (file-exists? local)
                 (not (link-exists? local))
                 (equal? (file->bytes local) (string->bytes/utf-8 published)))
      (refuse! "delivery sync pending: local artifact differs from committed proof")))
  ;; Squash provenance: the merge is on the protected main, and the publication
  ;; is part of the checkout being read back.
  (git (list "merge-base" "--is-ancestor" merge publication))
  (git (list "merge-base" "--is-ancestor" publication "HEAD"))
  (hasheq 'status
          "delivered"
          'merge-sha
          merge
          'publication-sha
          publication
          'plan-id
          plan
          'wave
          wave
          'delivery-head-sha
          head
          'delivery-branch
          wave-branch
          'binding-generation
          generation
          'attempt-id
          (if receipt
              (json-null (hash-ref receipt 'attempt-id #f))
              'null)
          'attempt-fence
          (if receipt
              (json-null (hash-ref receipt 'attempt-fence #f))
              'null)))

;; ============================================================
;; governance (read-only)
;; ============================================================

;; Python binding_branch: the deterministic binding branch name, with the
;; -r<n> republication family for repair generations.
(define (binding-branch plan wave [generation 0])
  (binding-path plan wave)
  (ensure! (and (exact-integer? wave) (>= wave 0)) "invalid wave index")
  (ensure! (and (exact-integer? generation) (>= generation 0)) "invalid binding generation")
  (define base (format "binding/~a-w~a" (substring plan 0 (min 12 (string-length plan))) wave))
  (cond
    [(= generation 0) base]
    [else (format "~a-r~a" base generation)]))

(define (binding-branch-identity? value base)
  (or (equal? value base)
      (and (regexp-match? (pregexp (string-append "^" (regexp-quote base) "-r[1-9][0-9]*$")) value)
           #t)))

(define (delivery-governance! repo plan wave expected-branch #:context [context #f])
  (unless (nonblank-string? expected-branch)
    (refuse! "governance requires --expected-branch"))
  (define base (binding-branch plan wave))
  (unless (binding-branch-identity? expected-branch base)
    (refuse! "governance branch does not match the deterministic binding branch"))
  (define ctx (or context (make-delivery-pr-context repo)))
  (define git (delivery-pr-context-git ctx))
  (define api (delivery-pr-context-api ctx))
  (define session (delivery-pr-context-session ctx))
  (define slug (repository git))
  (refresh git)
  (origin-main git)
  (define pr (resolve-merged-pr api slug expected-branch #f))
  (unless (hash? pr)
    (refuse! (string-append "governance cannot self-resolve exactly one merged binding PR for branch "
                            expected-branch)))
  (define number (hash-ref pr 'number #f))
  (define merge (dig pr 'merge_commit_sha))
  (define head (dig pr 'head 'sha))
  (unless (and (exact-integer? number) (> number 0) (full-sha? merge) (full-sha? head))
    (refuse! "merged binding PR has incomplete identity"))
  (unless (equal? (dig pr 'head 'ref) expected-branch)
    (refuse! "merged binding PR branch does not match expected branch"))
  (validate-merge pr (api slug (format "commits/~a" merge)) slug merge head)
  (git (list "merge-base" "--is-ancestor" merge "refs/remotes/origin/main"))
  ((delivery-api-session-trusted-check session) slug
                                                merge
                                                governance-check-name
                                                "push"
                                                #:branch "main")
  (hasheq 'status "governed" 'pr number 'branch expected-branch 'publication-sha merge))

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

(define (delivery-status-action! argv)
  (define action (and (pair? argv) (car argv)))
  (define flags
    (read-flag-value (if (pair? argv)
                         (cdr argv)
                         '())))
  (define repo (hash-ref flags "--repo" #f))
  (unless repo
    (refuse! (format "~a requires --repo" (or action ""))))
  (define plan (hash-ref flags "--plan" #f))
  (define wave (flag-integer flags "--wave"))
  (define campaign-root (hash-ref flags "--campaign-root" #f))
  (define expected-branch (hash-ref flags "--expected-branch" #f))
  (cond
    [(equal? action "status")
     (unless campaign-root
       (refuse! "status requires --campaign-root"))
     (delivery-status! repo plan wave #:campaign-root campaign-root)]
    [(equal? action "governance")
     (unless expected-branch
       (refuse! "governance requires --expected-branch"))
     (delivery-governance! repo plan wave expected-branch)]
    [else
     (refuse! (format "unsupported action ~a; this module implements status and governance only"
                      (or action "")))]))

;; Success -> JSON on stdout, exit 0. Refusal -> delivery-pending JSON, exit 2
;; (the exact shape delivery-coordinator.rkt's typed-stop parser expects).
(define (delivery-status-main argv)
  (with-handlers ([exn:fail? (lambda (error)
                               (printf "~a\n" (delivery-pending-json (exn-message error)))
                               (exit 2))])
    (printf "~a\n" (jsexpr-string (delivery-status-action! argv)))
    (exit 0)))

(module+ main
  (delivery-status-main (vector->list (current-command-line-arguments))))
