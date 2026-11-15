#lang racket/base

;; Phase B slice S7 — native port of the MUTATING delivery ladder actions
;; `merge` (plus its `merge_authorization` / `reviewed_head_approval` proofs)
;; and `prepare` of the deleted Python delivery controller.
;;
;; Scope law: this module implements exactly those two actions. Both are
;; dispatched by extensions/gsd/delivery-coordinator.rkt, which is the
;; production path since the Phase B final deletion removed the Python
;; controller; the
;; S3/S4/S5/S6 modules it reuses (gsd-delivery-api.rkt, gsd-delivery-git.rkt,
;; gsd-delivery-pr.rkt, gsd-delivery-write.rkt, gsd-delivery-status.rkt) are
;; READ-ONLY here. The one mutation this slice performs — and the ONLY one it is
;; allowed to perform — is Python's `gh_put(slug, "pulls/<n>/merge",
;; {"merge_method":"squash"})` through the S5 write seam.
;;
;; ONE-BUDGET LAW (B2), inherited: an action builds ONE delivery-pr-context
;; (make-delivery-pr-context), which owns ONE (budget-seconds, start-ms) pair and
;; builds the gh runner, the repo-git command, the `racket` helper command and
;; fetch-head from that SAME pair. Every gh read, every git read/fetch and every
;; helper subprocess therefore draws down ONE decreasing budget exactly like
;; Python's process-wide remaining_budget(); a second runner would silently grant
;; a fresh budget and is never created here. Credential isolation comes from the
;; shared seam (controller-environment), every subprocess is bounded, and raw
;; child output is never surfaced.
;;
;; STALENESS LAW: `merge` re-reads the implementation PR with `#:refresh? #t`
;; AFTER the PUT, because a stage-1 read must never satisfy a post-mutation proof
;; (Python: `api(slug, f'pulls/{number}', _refresh=True)`). Every read Python
;; performs through its memoized `api()` stays memoized here (same route, same
;; per-process cache shape), so an interleaving cannot disagree with the Python
;; ladder. ONE context serves exactly ONE action: the session's `checks` memo has
;; no refresh bypass, so reusing a context across two actions is never done.
;;
;; NON-IDEMPOTENT-PUT LAW: the merge PUT is attempted EXACTLY ONCE. There is no
;; retry loop, no retry parameter and no "retry once on transport error" path,
;; because a repeated merge PUT can 405/409 or be re-evaluated against a moved PR.
;; A lost/ambiguous response refuses with the seam's typed cause; the ONLY
;; recovery is a later explicit run, which re-reads the PR and takes the
;; idempotent `already-merged` path after re-proving every pre-merge gate (a
;; merge that happened outside this gate is refused, never accepted).
;;
;; Parity law (the Python functions are the contract; refusal strings are
;; byte-identical so the coordinator's reason chain does not change):
;;
;;   merge(repo, plan, wave, number, expected_head, expected_branch, source)
;;     1. repository slug, `refresh` of origin/main, full origin/main head;
;;     2. `api pulls/<n>` (memoized, like Python's stage-1 read) and
;;        "malformed PR response";
;;     3. validate_pr_identity (same-repo, main-base);
;;     4. the PR head must equal the verified expected head (and be a full SHA):
;;        "implementation PR head does not match the verified expected head";
;;     5. if the PR is ALREADY merged: validate_merge against its merge commit,
;;        then the SAME pre-merge proofs (fetched refs/pull/<n>/head, strict trio
;;        at the exact head, non-blank PR author, operator authorization,
;;        non-author APPROVED review bound to the exact head, protection and
;;        every required trusted check) and the idempotent payload — the PUT is
;;        never repeated;
;;     6. otherwise the PR must be OPEN ("implementation PR is neither open nor
;;        already merged") and based on FRESH origin/main
;;        ("implementation PR is not based on fresh origin/main; reanchor and
;;        re-verify");
;;     7. fetched refs/pull/<n>/head == the expected head;
;;     8. validate_trio at the exact head, the non-blank PR author, then the
;;        amended approval contract (merge_authorization + reviewed_head_
;;        approval);
;;     9. policy_names at origin/main, branch protection requiring them, and one
;;        trusted Actions `pull_request` check per name on that head;
;;    10. the protected squash PUT, then "merged is true AND a full SHA" or
;;        "GitHub did not confirm the protected squash merge";
;;    11. a FRESH re-read of the PR (`#:refresh? #t`) plus validate_merge against
;;        the reported merge commit;
;;    12. the exact payload {'status','merge-sha','plan-id','wave','head','pr'}.
;;
;;   prepare(repo, plan, wave, number, relative, campaign-root, output,
;;           expected_branch, expected_head)
;;     Journal root/generation -> the deterministic binding path -> repository
;;     slug -> fresh origin/main -> self-resolve exactly one merged PR for the
;;     durable branch (when --pr was omitted) -> same-repo main-base identity ->
;;     merged requirement -> validate_merge -> fetched PR head -> merge tree == PR
;;     head tree -> the F4 evidence-record purity probe (gsd-evidence-bind.rkt
;;     record-commit) -> required-check policy + protection -> the merge on
;;     origin/main -> frozen snapshot facts + declared/active outputs -> the exact
;;     source-path rule -> the materialized source trio identity (schema, wave,
;;     plan, review/validation artifact, issue, milestone) -> a refusal to rebind
;;     an already-published binding for a different implementation -> the strict
;;     trio gate -> the attempt-bound receipt -> the binding draft request -> the
;;     deterministic staging write (`gsd-binding-data.rkt prepare`) or the
;;     idempotent `already-staged` resume. It NEVER mutates GitHub.
;;
;; Documented deviations (all fail closed, never open):
;;   * `validate_trio`, `campaign_journal`, `journal_generation`, `tree_of` and
;;     `binding_branch` live in the read-only S4/S6 modules; the S6 module exports
;;     them, so they are REUSED (same subprocess argv, same refusal strings). The
;;     trio gate is therefore never re-implemented. `snapshot_facts`,
;;     `declared_outputs`, `active_declared_outputs`, `draft_request` and
;;     `require_binding_receipt` are unexported in S4, so they are mirrored here
;;     with identical argv and identical refusal strings.
;;   * Python's `%r` formatting (merge_authorization's wave, reviewed_head_
;;     approval's verdict) is reproduced by a small Python-repr shim for strings,
;;     numbers and booleans; an exotic JSON value falls back to Racket's `~s`
;;     (a diagnostic-only difference, never a policy difference).
;;   * Python's `str.casefold()` is `string-foldcase` (Unicode full case
;;     folding), so the non-author comparison keeps Python's class of collisions.
;;   * a helper subprocess that fails keeps the seam's typed cause; only an
;;     unreadable helper payload becomes the documented malformed-payload
;;     refusal (Python surfaces the raw JSONDecodeError/OSError text instead).
;;   * `read_datum` failures while inspecting an ALREADY PUBLISHED binding are
;;     treated as "not a publication" (Python catches Pending and sets
;;     bound=None), so a malformed published binding can never block a
;;     pre-binding draft; the strict gate still guards the eventual publication.
;;     A malformed STAGED draft keeps its typed "malformed binding datum"
;;     refusal, exactly like Python's uncaught read_datum.
;;   * JSON integers: `exact-integer?` is stricter than Python's
;;     `isinstance(x, int)` (a JSON true/1.0 is refused here, coerced there).
;;   * the `--output` path is made absolute with `path->complete-path` +
;;     `simplify-path` instead of Python's `Path.resolve()`, so a staging path
;;     that itself contains a symlinked COMPONENT is reported verbatim rather
;;     than resolved. The determinism check on that path is the staged-draft
;;     identity check above, so only the echoed `output` string can differ.
;;   * `prepare` returns the stage payload only after the trusted writer
;;     (`gsd-binding-data.rkt prepare`) exits zero; Python deletes the transient
;;     `request.json` unconditionally afterwards, while this port deletes it only
;;     when it still exists (a no-op difference).

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
         "gsd-delivery-pr.rkt"
         "gsd-delivery-write.rkt"
         "gsd-delivery-status.rkt")

(provide delivery-merge!
         delivery-prepare!
         ;; the amended approval contract (exported for parity tests)
         merge-authorization
         reviewed-head-approval
         ;; prepare helpers (exported for parity tests and later slices)
         snapshot-facts
         declared-group
         declared-outputs
         active-declared-outputs
         draft-request
         require-binding-receipt
         record-commit-purity
         prepare-next-message
         ;; CLI surface (same JSON shapes as the Python actions)
         delivery-merge-action!
         delivery-merge-main)

;; ============================================================
;; Constants (mirror the deleted Python controller; recorded in the goldens fixture)
;; ============================================================

(define-runtime-path this-source "gsd-delivery-merge.rkt")
(define scripts-dir (path-only this-source))
(define binding-data-script (build-path scripts-dir "gsd-binding-data.rkt"))
(define evidence-bind-script (build-path scripts-dir "gsd-evidence-bind.rkt"))

(define declared-kinds '(evidence review validation))

(define declared-dirs
  (hasheq 'evidence "gsd-wave-evidence" 'review "gsd-wave-reviews" 'validation "gsd-wave-validation"))

(define protected-branch-names '("main" "master"))

;; Python's helper subprocesses all pass `timeout=60`; the shared bounded runner
;; clamps it to the remaining total process budget.
(define helper-command-timeout 60.0)

;; Python's EXCLUDES: the same excluded directories as git pathspec magic. The
;; prefixes come from the read ladder's own export, so one side cannot widen
;; without the other.
(define exclude-pathspecs
  (for/list ([prefix (in-list excluded-prefixes)])
    (string-append ":(exclude)" prefix "**")))

;; ============================================================
;; Small helpers (same semantics as the S3/S4/S5/S6 modules)
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

;; Python's `%s` of a JSON value: None prints as "None".
(define (python-text value)
  (cond
    [(absent? value) "None"]
    [(string? value) value]
    [(eq? value #t) "True"]
    [(eq? value #f) "False"]
    [(number? value) (format "~a" value)]
    [else (format "~s" value)]))

;; Python's `%r` for the value classes the two approval refusals can carry
;; (strings and JSON scalars). Diagnostic only: it never decides a policy.
(define (python-repr value)
  (cond
    [(string? value) (format "'~a'" (string-replace (string-replace value "\\" "\\\\") "'" "\\'"))]
    [(eq? value #t) "True"]
    [(number? value) (format "~a" value)]
    [(absent? value) "None"]
    [else (format "~s" value)]))

;; The freshly fetched default-branch head (the S4 accessor is private there).
(define (origin-main git)
  (define main (string-trim (git (list "rev-parse" "refs/remotes/origin/main"))))
  (unless (full-sha? main)
    (refuse! "malformed origin/main head"))
  main)

;; Python digest(): sha256 over `git diff --binary base...head -- . <excludes>`.
(define (content-digest git base head)
  (sha256-hex-of-text (git (append (list "diff" "--binary" (format "~a...~a" base head) "--" ".")
                                   exclude-pathspecs))))

;; ============================================================
;; merge_authorization / reviewed_head_approval (the amended contract)
;; ============================================================

;; Recorded operator authorization from the evidence record's
;; `merge-authorization` object: it must name the operator, THIS wave, the exact
;; authorized head and the authorized action, and cite its source. The authorized
;; head is the evidence record's own `implementation-sha`; the excluded-path
;; digest from the PR base to that receipt must equal the digest to the merge tip
;; (and the recorded content-digest), so the merge delivers exactly the
;; authorized content and nothing else.
(define (merge-authorization evidence wave head base git)
  (define auth (and (hash? evidence) (hash-ref evidence 'merge-authorization #f)))
  (unless (hash? auth)
    (refuse! "no-operator-authorization: evidence record carries no merge-authorization object"))
  (for ([field (in-list '("operator" "wave" "head" "action" "source"))])
    (unless (nonblank-string? (hash-ref auth (string->symbol field) #f))
      (refuse! (string-append "no-operator-authorization: merge-authorization."
                              field
                              " is missing or empty"))))
  (unless (equal? (hash-ref auth 'wave #f) wave)
    (refuse! (format "no-operator-authorization: merge-authorization names wave ~a, not ~a"
                     (python-repr (hash-ref auth 'wave #f))
                     (python-repr wave))))
  (define receipt (hash-ref evidence 'implementation-sha #f))
  (unless (full-sha? receipt)
    (refuse! "no-operator-authorization: evidence record has no implementation head to authorize"))
  (unless (equal? (hash-ref auth 'head #f) receipt)
    (refuse!
     (format (string-append "no-operator-authorization: merge-authorization authorizes head ~a, "
                            "not the verified implementation head ~a")
             (python-text (hash-ref auth 'head #f))
             (python-text receipt))))
  (define recorded (hash-ref evidence 'content-digest #f))
  (unless (and (equal? (content-digest git base receipt) recorded)
               (equal? (content-digest git base head) recorded))
    (refuse!
     (string-append
      "no-operator-authorization: the merge tip no longer carries the authorized implementation content")))
  auth)

;; The APPROVED independent non-author review artifact bound to the exact
;; verified implementation head (F3 head binding, replacing the unsatisfiable
;; second-account GitHub review requirement without weakening anything else).
(define (reviewed-head-approval review evidence author)
  (unless (hash? review)
    (refuse! "no-review-artifact: the review artifact is missing or malformed"))
  (unless (nonblank-string? author)
    (refuse! (string-append "no-review-artifact: PR author identity is missing or malformed; "
                            "the non-author property is unverifiable")))
  (define verdict (hash-ref review 'verdict #f))
  (unless (equal? verdict "APPROVED")
    (refuse! (format "no-review-artifact: review verdict is ~a, not APPROVED" (python-repr verdict))))
  (define reviewer (hash-ref review 'reviewer #f))
  (unless (nonblank-string? reviewer)
    (refuse! "no-review-artifact: review has no reviewer identity"))
  (unless (not (equal? (string-foldcase (string-trim reviewer))
                       (string-foldcase (string-trim author))))
    (refuse! "no-review-artifact: review has no non-author reviewer identity"))
  (define receipt (and (hash? evidence) (hash-ref evidence 'implementation-sha #f)))
  (define reviewed (hash-ref review 'reviewed-sha #f))
  (unless (equal? reviewed receipt)
    (refuse! (format (string-append "head-binding-mismatch: review reviewed-sha ~a does not bind "
                                    "the verified implementation head ~a")
                     (python-text reviewed)
                     (python-text receipt))))
  review)

;; ============================================================
;; merge
;; ============================================================

;; The proofs that must run BEFORE any mutation is reachable, in Python's exact
;; order: the actually-fetched PR ref, the unchanged strict trio at that exact
;; head, the non-blank author, the operator authorization, the approved
;; non-author review, then the policy snapshot + protection + trusted checks.
(define (require-merge-proofs! ctx pr number slug wave head source main expected-branch)
  (define git (delivery-pr-context-git ctx))
  (define session (delivery-pr-context-session ctx))
  (define base (dig pr 'base 'sha))
  ;; Python fetches refs/pull/<number>/head with the CLI number, never with the
  ;; number the PR payload claims.
  (define fetched ((delivery-api-session-fetch-head session) number))
  (unless (equal? fetched head)
    (refuse! "fetched implementation head does not match expected head"))
  (define-values (evidence review _paths) (validate-trio ctx head source base))
  (define author (or (dig pr 'user 'login) ""))
  (unless (nonblank-string? author)
    (refuse! "malformed PR author identity; refusing the non-author review comparison"))
  (merge-authorization evidence (format "W~a" wave) head base git)
  (reviewed-head-approval review evidence author)
  (define names (policy-names git main))
  ((delivery-api-session-protection session) slug names)
  (for ([name (in-list names)])
    ((delivery-api-session-trusted-check session) slug
                                                  head
                                                  name
                                                  "pull_request"
                                                  #:branch expected-branch))
  void)

;; Deterministic protected squash merge of the implementation PR. Every gate runs
;; before any mutation; `expected-head` is the validated PR tip, never model
;; output. Returns the exact Python payload.
(define (delivery-merge! repo
                         plan
                         wave
                         number
                         expected-head
                         expected-branch
                         source
                         #:context [context #f])
  (define ctx (or context (make-delivery-pr-context repo)))
  (define git (delivery-pr-context-git ctx))
  (define api (delivery-pr-context-api ctx))
  (define slug (repository git))
  (refresh git)
  (define main (origin-main git))
  ;; Python's stage-1 read is memoized; the post-mutation proof below is not.
  (define pr (api slug (format "pulls/~a" number)))
  (ensure! (hash? pr) "malformed PR response")
  (validate-pr-identity pr slug)
  (define head (dig pr 'head 'sha))
  (unless (and (equal? head expected-head) (full-sha? expected-head))
    (refuse! "implementation PR head does not match the verified expected head"))
  (cond
    [(pr-is-merged? pr)
     ;; Idempotent resume after a lost merge response: the mutation already
     ;; happened so it is NOT repeated — but every pre-merge proof still runs at
     ;; the PR head, so a merge performed outside this gate is refused.
     (define merge-sha (dig pr 'merge_commit_sha))
     (validate-merge pr (api slug (format "commits/~a" merge-sha)) slug merge-sha head)
     (require-merge-proofs! ctx pr number slug wave head source main expected-branch)
     (hasheq 'status
             "already-merged"
             'merge-sha
             (json-null merge-sha)
             'plan-id
             (json-null plan)
             'wave
             wave
             'head
             head
             'pr
             number)]
    [else
     (unless (equal? (hash-ref pr 'state #f) "open")
       (refuse! "implementation PR is neither open nor already merged"))
     (unless (equal? (dig pr 'base 'sha) main)
       (refuse! "implementation PR is not based on fresh origin/main; reanchor and re-verify"))
     (require-merge-proofs! ctx pr number slug wave head source main expected-branch)
     ;; The ONE mutation: the same argv the Python gh_put builds, executed once by
     ;; the context's own bounded runner.
     (define result
       (gh-put ctx slug (format "pulls/~a/merge" number) (list (cons "merge_method" "squash"))))
     (define merge-sha (hash-ref result 'sha #f))
     (unless (and (equal? (hash-ref result 'merged #f) #t) (full-sha? merge-sha))
       (refuse! "GitHub did not confirm the protected squash merge"))
     ;; Re-read WITHOUT the memo: a stage-1 read must never satisfy a
     ;; post-mutation proof.
     (define merged-pr (api slug (format "pulls/~a" number) #:refresh? #t))
     (validate-merge merged-pr (api slug (format "commits/~a" merge-sha)) slug merge-sha head)
     (hasheq 'status
             "merged"
             'merge-sha
             merge-sha
             'plan-id
             (json-null plan)
             'wave
             wave
             'head
             head
             'pr
             number)]))

;; ============================================================
;; prepare helpers: snapshot facts, declared outputs, source materialization
;; ============================================================

;; Verify the immutable campaign plan snapshot through the trusted Racket module
;; (full hash validation lives there, exactly like Python's snapshot_facts).
(define (snapshot-facts run campaign-root plan wave)
  (define root (and campaign-root (as-path campaign-root)))
  (unless (and root (directory-exists? root) (not (link-exists? root)))
    (refuse! "prepare requires --campaign-root with the campaign working tree"))
  (binding-path plan wave)
  (define text
    (run "racket"
         (list (path->string binding-data-script)
               "snapshot"
               (path->string root)
               plan
               (number->string wave))
         helper-command-timeout))
  (define facts
    (with-handlers ([exn:fail? (lambda (_error) (refuse! "malformed campaign snapshot facts"))])
      (string->jsexpr text)))
  (unless (and (hash? facts)
               (equal? (hash-ref facts 'status #f) "ok")
               (equal? (hash-ref facts 'plan-id #f) plan)
               (equal? (hash-ref facts 'wave #f) wave))
    (refuse! "campaign snapshot does not verify for this plan/wave"))
  facts)

;; One declared kind's frozen output list (Python's per-kind normalization).
(define (declared-group declared kind)
  (define raw (hash-ref declared kind #f))
  (cond
    [(absent? raw) '()]
    [(list? raw) raw]
    [else (refuse! (string-append "malformed declared wave output for " (symbol->string kind)))]))

(define (declared-path kind path repo-name)
  (ensure! (nonblank-string? path) "malformed declared wave output")
  (define normalized
    (cond
      [(string-prefix? path "q/")
       (unless (equal? repo-name "q")
         (refuse! "declared output uses a q/ prefix but the resolved repo is not q"))
       (substring path 2)]
      [else path]))
  (artifact-path normalized (hash-ref declared-dirs kind)))

;; Frozen declared trio outputs, or #f when the wave declares none. A leading
;; `q/` prefix is normalized only when the resolved repository directory itself
;; is named q; anything else fails closed.
(define (declared-outputs facts repo)
  (define declared (and (hash? facts) (hash-ref facts 'declared #f)))
  (cond
    [(not (hash? declared)) #f]
    [else
     (define groups
       (for/hash ([kind (in-list declared-kinds)])
         (values kind (declared-group declared kind))))
     (define counts
       (for/list ([kind (in-list declared-kinds)])
         (length (hash-ref groups kind))))
     (cond
       [(andmap zero? counts) #f]
       [(not (andmap (lambda (count) (= count 1)) counts))
        (refuse! (string-append "frozen wave document must declare exactly one evidence, review and "
                                "validation output; filename guessing is refused"))]
       [else
        (define repo-name
          (path->string (file-name-from-path (simplify-path (path->complete-path (as-path repo))))))
        (for/hash ([kind (in-list declared-kinds)])
          (values kind (declared-path kind (car (hash-ref groups kind)) repo-name)))])]))

;; Python repair_generation_path: the -r<n> republication path family.
(define (repair-generation-path relative generation)
  (ensure! (and (exact-integer? generation) (>= generation 0)) "invalid binding generation")
  (cond
    [(= generation 0) relative]
    [else
     (define name (last (string-split relative "/")))
     (unless (and (string-suffix? name ".rktd") (not (string=? name ".rktd")))
       (refuse! "multiple/conflicting declared output path"))
     (string-append (substring relative 0 (- (string-length relative) 5))
                    (format "-r~a.rktd" generation))]))

;; The active output paths for this generation: the declared trio (repaired for
;; repair generations) or the deterministic hash-named family.
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

;; `git ls-tree` must report a plain regular file, then the blob is written into
;; the materialized root (Python materialize()).
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
                           (display (git (list "show" (format "~a:~a" ref relative))) out)))
  target)

;; One committed .rktd datum through the trusted reader (the module Python
;; spawns). A helper failure keeps the seam's typed cause; only an unreadable
;; datum becomes the documented malformed-datum refusal.
(define (read-binding-datum run path)
  (define text
    (run "racket"
         (list (path->string binding-data-script) "read" (path->string path))
         helper-command-timeout))
  (with-handlers ([exn:fail? (lambda (_error)
                               (refuse! (string-append "malformed binding datum: "
                                                       (path->string path))))])
    (string->jsexpr text)))

;; The F4 purity probe: the newest base..head commit touching the evidence record
;; directory must have touched ONLY record paths. The shared Racket tool decides
;; the verdict; any tool failure keeps the seam's typed cause, and a decided
;; refusal carries the tool's own typed code.
(define (record-commit-purity run repo base head)
  (define verdict
    (run "racket"
         (list (path->string evidence-bind-script)
               "record-commit"
               "--repo"
               (path->string (as-path repo))
               "--base"
               base
               "--head"
               head)
         helper-command-timeout))
  (define decided (string-trim verdict))
  (ensure! (string-prefix? decided "pure") decided)
  decided)

;; Python require_binding_receipt: a journal-bearing campaign is pinned to the
;; current attempt-bound receipt head/branch before any authoring.
(define (require-binding-receipt data head branch)
  (cond
    [(or (eq? data #f) (not (hash? data))) (void)]
    [else
     (define receipt (hash-ref data 'receipt #f))
     (unless (and (hash? receipt)
                  (full-sha? (hash-ref receipt 'head #f))
                  (equal? head (hash-ref receipt 'head #f))
                  (equal? branch (hash-ref receipt 'branch #f))
                  (not (member branch protected-branch-names))
                  (nonblank-string? (hash-ref receipt 'attempt-id #f))
                  (exact-integer? (hash-ref receipt 'attempt-fence #f))
                  (>= (hash-ref receipt 'attempt-fence #f) 0))
       (refuse! "binding must match the current attempt-bound receipt head/branch"))]))

;; Python draft_request: the exact staged binding request object.
(define (draft-request source plan wave pr branch required-checks [generation 0])
  (binding-path plan wave)
  (unless (equal? (hash-ref source 'wave #f) (format "W~a" wave))
    (refuse! "source trio belongs to another wave"))
  (unless (and (list? required-checks)
               (pair? required-checks)
               (for/and ([name (in-list required-checks)])
                 (nonblank-string? name)))
    (refuse! "binding draft requires the exact required-check policy snapshot"))
  (hasheq 'plan-id
          plan
          'wave
          (format "W~a" wave)
          'milestone
          (json-null (hash-ref source 'milestone #f))
          'issue
          (json-null (hash-ref source 'issue #f))
          'status
          "pending-review"
          'merge-sha
          (hash-ref pr 'merge_commit_sha)
          'delivery-pr
          (hash-ref pr 'number)
          'delivery-head-sha
          (dig pr 'head 'sha)
          'wave-branch
          (dig pr 'head 'ref)
          'merged-at
          (json-null (hash-ref pr 'merged_at #f))
          'branch
          branch
          'required-pr-checks
          required-checks
          'binding-generation
          generation))

;; The exact `next` guidance Python returns from a fresh staging.
(define (prepare-next-message)
  (string-append "Copy trio into a dedicated fresh-main worktree; complete genuine "
                 "binding review and gates; publish through protected squash PR; wait "
                 "for main governance; sync and resume /go."))

;; ============================================================
;; prepare
;; ============================================================

;; An already PUBLISHED binding may only be redrafted for the identical verified
;; implementation; a different one is refused outright. A pre-binding trio (no
;; merge identity) is not a publication and is superseded by the reviewed binding
;; PR. A malformed published binding is treated as "not a publication" (Python
;; catches Pending), never as an error that blocks a draft.
(define (require-rebind-identity ctx binding merge head)
  (define git (delivery-pr-context-git ctx))
  (define run (delivery-pr-context-run ctx))
  (define existing (blob-or-none git "refs/remotes/origin/main" binding))
  (when existing
    (define root (make-temporary-file "q-delivery-rebind-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (define path (build-path root "binding.rktd"))
       (call-with-output-file path #:exists 'truncate (lambda (out) (display existing out)))
       (define bound
         (with-handlers ([exn:fail:delivery-action? (lambda (_error) #f)])
           (read-binding-datum run path)))
       (define published
         (and (hash? bound)
              (for/and ([key (in-list '(merge-sha delivery-head-sha delivery-pr implementation-sha))])
                (not (absent? (hash-ref bound key #f))))))
       (unless (or (not published)
                   (and (equal? (hash-ref bound 'merge-sha #f) merge)
                        (equal? (hash-ref bound 'delivery-head-sha #f) head)
                        (equal? (hash-ref bound 'implementation-sha #f)
                                (hash-ref bound 'merge-sha #f))))
         (refuse!
          (string-append
           "origin/main already binds this wave to a different implementation; refusing rebind"))))
     (lambda () (delete-directory/files root))))
  void)

;; The idempotent staging resume: a staging directory that already holds the
;; binding draft for THIS exact identity is already-prepared; a draft for any
;; OTHER identity refuses (staging is never overwritten). A malformed staged
;; datum keeps read_datum's typed refusal, exactly like Python.
(define (require-staged-identity run output binding merge head number branch wave-branch)
  (define staged (build-path (as-path output) binding))
  (unless (and (file-exists? staged) (not (link-exists? staged)))
    (refuse! "binding staging output exists without the staged binding draft"))
  (define existing (read-binding-datum run staged))
  (unless (hash? existing)
    (refuse! "existing staged binding draft is malformed"))
  (unless (and (equal? (hash-ref existing 'merge-sha #f) merge)
               (equal? (hash-ref existing 'delivery-head-sha #f) head)
               (equal? (hash-ref existing 'delivery-pr #f) number)
               (equal? (hash-ref existing 'branch #f) branch)
               (equal? (hash-ref existing 'wave-branch #f) wave-branch))
    (refuse! (string-append "existing binding staging binds a different delivery "
                            "identity; refusing to reuse it for this preparation")))
  (hasheq 'status
          "already-staged"
          'output
          (path->string (as-path output))
          'binding
          binding
          'branch
          branch
          'merge-sha
          merge
          'delivery-head-sha
          head
          'implementation-sha
          merge))

;; Prepare the binding evidence/paths for a later binding PR. Never mutates
;; GitHub; the only writes are the deterministic local staging directory.
(define (delivery-prepare! repo
                           plan
                           wave
                           number
                           relative
                           campaign-root
                           output
                           #:expected-branch [expected-branch #f]
                           #:expected-head [expected-head #f]
                           #:context [context #f])
  (define ctx (or context (make-delivery-pr-context repo)))
  (define git (delivery-pr-context-git ctx))
  (define api (delivery-pr-context-api ctx))
  (define session (delivery-pr-context-session ctx))
  (define run (delivery-pr-context-run ctx))
  ;; The journal root and generation are resolved from the SAME campaign root the
  ;; frozen snapshot is read from, exactly like Python's campaign_journal_root.
  (define journal-root (campaign-journal-root repo campaign-root))
  (define generation (journal-generation run journal-root plan wave))
  (define binding (binding-path plan wave generation))
  (define branch (binding-branch plan wave generation))
  (define slug (repository git))
  (refresh git)
  (define main (origin-main git))
  ;; The PR identity: given, or self-resolved from exactly one merged PR for the
  ;; durable branch. The resolved number REBINDS the argument, exactly like
  ;; Python's `number = pr.get('number')`; every later use (fetch_head, the
  ;; closed-state check, draft request, staged-identity comparison, payloads)
  ;; reads the rebound value, so self-resolution is reachable and consistent.
  (define pr
    (cond
      [(not number)
       (unless (nonblank-string? expected-branch)
         (refuse! "prepare --pr requires --expected-branch for durable PR self-resolution"))
       (define resolved (resolve-merged-pr api slug expected-branch expected-head))
       (unless (hash? resolved)
         (refuse! (string-append
                   "prepare cannot self-resolve exactly one merged implementation PR for branch "
                   expected-branch)))
       resolved]
      [else (api slug (format "pulls/~a" number))]))
  (ensure! (hash? pr) "malformed PR response")
  (validate-pr-identity pr slug)
  (define merge (dig pr 'merge_commit_sha))
  (define head (dig pr 'head 'sha))
  (unless (pr-is-merged? pr)
    (refuse! "implementation PR must be independently reviewed and merged first"))
  (unless number
    (unless (equal? (hash-ref pr 'state #f) "closed")
      (refuse! "self-resolved implementation PR is not closed")))
  ;; Python rebinds `number` from the (self-)resolved PR before fetch_head.
  (define resolved-number (or number (hash-ref pr 'number #f)))
  (ensure! (and (exact-integer? resolved-number) (positive? resolved-number))
           "resolved pull request has no usable number")
  (set! number resolved-number)
  (validate-merge pr (api slug (format "commits/~a" merge)) slug merge head)
  (define fetched ((delivery-api-session-fetch-head session) number))
  (unless (equal? fetched head)
    (refuse! "fetched implementation head does not match PR head SHA"))
  (unless (equal? (tree-of git merge) (tree-of git fetched))
    (refuse! "implementation merge tree differs from fetched PR head tree"))
  (record-commit-purity run repo (dig pr 'base 'sha) head)
  (define names (policy-names git main))
  ((delivery-api-session-protection session) slug names)
  (git (list "merge-base" "--is-ancestor" merge "refs/remotes/origin/main"))
  ;; Frozen provenance: the immutable snapshot must verify for this plan/wave and
  ;; the source trio must be exactly the wave's declared output.
  (define facts (snapshot-facts run campaign-root plan wave))
  (define declared (declared-outputs facts repo))
  (define active (active-declared-outputs facts repo plan wave generation))
  (artifact-path relative "gsd-wave-evidence")
  (cond
    [declared
     (when (and (>= generation 1) (equal? relative (hash-ref declared 'evidence)))
       (refuse! (string-append "generation-0 source refused for active repair generation; use "
                               (hash-ref active 'evidence))))
     (ensure! (equal? relative (hash-ref active 'evidence))
              (if (= generation 0)
                  "source evidence is not the frozen wave output for this plan/wave"
                  "source evidence is not the active frozen wave output for this plan/wave"))]
    [else
     (ensure! (equal? relative binding)
              "wave declares no outputs; only the exact campaign-hash source is accepted")])
  (define source
    (let ([root (make-temporary-file "q-delivery-source-~a" 'directory)])
      (dynamic-wind void
                    (lambda () (read-binding-datum run (materialize git head relative root)))
                    (lambda () (delete-directory/files root)))))
  (unless (and (hash? source) (equal? (hash-ref source 'schema-version #f) 2))
    (refuse! "source must be a schema-2 hash trio"))
  (unless (equal? (hash-ref source 'wave #f) (format "W~a" wave))
    (refuse! "source trio belongs to another wave"))
  (unless (absent? (hash-ref source 'plan-id #f))
    (unless (equal? (hash-ref source 'plan-id #f) plan)
      (refuse! "source trio explicitly names another campaign; refusing")))
  (when declared
    (unless (equal? (hash-ref source 'review-artifact #f) (hash-ref active 'review))
      (refuse! (if (= generation 0)
                   "source review artifact is not the frozen wave output"
                   "source review artifact is not the active frozen wave output")))
    (unless (equal? (hash-ref source 'validation-artifact #f) (hash-ref active 'validation))
      (refuse! (if (= generation 0)
                   "source validation artifact is not the frozen wave output"
                   "source validation artifact is not the active frozen wave output"))))
  (when (hash-ref facts 'issue #f)
    (unless (equal? (hash-ref source 'issue #f) (hash-ref facts 'issue))
      (refuse! "source issue differs from frozen plan metadata")))
  (when (hash-ref facts 'milestone #f)
    (unless (equal? (hash-ref source 'milestone #f) (hash-ref facts 'milestone))
      (refuse! "source milestone differs from frozen plan metadata")))
  (require-rebind-identity ctx binding merge head)
  (validate-trio ctx head relative (dig pr 'base 'sha))
  (require-binding-receipt (campaign-journal run journal-root plan wave) head (dig pr 'head 'ref))
  (define request (draft-request source plan wave pr branch names generation))
  (define output-path (simplify-path (path->complete-path (as-path output))))
  (cond
    [(or (file-exists? output-path) (directory-exists? output-path))
     (require-staged-identity run output-path binding merge head number branch (dig pr 'head 'ref))]
    [else
     (make-directory* output-path)
     (define payload (build-path output-path "request.json"))
     (call-with-output-file payload #:exists 'truncate (lambda (out) (write-json request out)))
     (run "racket"
          (list (path->string binding-data-script)
                "prepare"
                (path->string payload)
                (path->string output-path))
          helper-command-timeout)
     (when (file-exists? payload)
       (delete-file payload))
     (hasheq 'status
             "pending-review"
             'output
             (path->string output-path)
             'binding
             binding
             'source
             relative
             'next
             (prepare-next-message))]))

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

;; Python main(): the flag requirements are part of the action contract, so they
;; run BEFORE any session is created.
(define (delivery-merge-action! argv)
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
  (define number (flag-integer flags "--pr"))
  (define expected-head (hash-ref flags "--expected-head" #f))
  (define expected-branch (hash-ref flags "--expected-branch" #f))
  (define evidence (hash-ref flags "--evidence" #f))
  (cond
    [(member action (list "merge" "binding-merge"))
     (unless (and number expected-head expected-branch evidence)
       (refuse!
        (if (equal? action "merge")
            "merge requires --pr, --expected-head, --expected-branch and --evidence"
            "binding-merge requires --pr, --expected-branch, --expected-head and --evidence")))
     (delivery-merge! repo plan wave number expected-head expected-branch evidence)]
    [(equal? action "prepare")
     (unless (and evidence (hash-ref flags "--output" #f) (hash-ref flags "--campaign-root" #f))
       (refuse! "prepare requires --evidence, --output and --campaign-root"))
     (when (not number)
       (unless expected-branch
         (refuse! "prepare without --pr requires --expected-branch")))
     (delivery-prepare! repo
                        plan
                        wave
                        number
                        evidence
                        (hash-ref flags "--campaign-root" #f)
                        (hash-ref flags "--output" #f)
                        #:expected-branch expected-branch
                        #:expected-head expected-head)]
    [else
     (refuse! (format "unsupported action ~a; this module implements merge and prepare only"
                      (or action "")))]))

;; Success -> JSON on stdout, exit 0. Refusal -> delivery-pending JSON, exit 2
;; (the exact shape delivery-coordinator.rkt's typed-stop parser expects).
(define (delivery-merge-main argv)
  (with-handlers ([exn:fail? (lambda (error)
                               (printf "~a\n" (delivery-pending-json (exn-message error)))
                               (exit 2))])
    (printf "~a\n" (jsexpr-string (delivery-merge-action! argv)))
    (exit 0)))

(module+ main
  (delivery-merge-main (vector->list (current-command-line-arguments))))
