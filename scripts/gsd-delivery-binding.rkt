#lang racket/base

;; Phase B slice S8 — native port of the BINDING ladder actions of
;; scripts/gsd-delivery.py:
;;
;;   binding-review   -> read_staged_trio  (the deterministic staged-trio reader)
;;   binding-pr       -> binding_publish   (Python: `binding_pr = binding_publish`)
;;   binding-ci       -> binding_ci
;;   binding-merged   -> binding_merge     (= merge, unchanged protected gate)
;;
;; Scope law: this module implements exactly those four actions. It is NOT wired
;; into extensions/gsd/delivery-coordinator.rkt (the coordinator still shells to
;; the Python controller), it does not edit scripts/gsd-delivery.py, and every
;; module it reuses (S3 api, S4 read ladder, S5 write seam, S6 status, S7 merge)
;; is READ-ONLY here. The ONE mutation this slice performs is Python's
;; `gh_post(slug, "pulls", {title,body,head,base})` through the S5 write seam,
;; plus the non-force push of the deterministic binding branch — exactly the two
;; writes binding_publish performs.
;;
;; ONE-BUDGET LAW (inherited from S3–S7): an action builds ONE
;; delivery-pr-context (make-delivery-pr-context), which owns ONE
;; (budget-seconds, start-ms) pair and builds the gh runner, the repo git
;; command, the `racket` helper command and fetch-head from that SAME pair.
;; binding-pr creates the context FIRST and hands it to its own binding-review
;; step, so the composite action still spends ONE budget instead of two.
;;
;; STALENESS LAW: every read Python performs with `_refresh=True` is performed
;; here with `#:refresh? #t`:
;;   * binding-pr re-reads the PR it created/resolved (`pulls/<n>`),
;;   * binding-ci re-reads the PR under test,
;;   * binding-merged inherits merge's post-PUT re-read.
;; The memoized reads Python performs through plain `api()` (PR resolution, the
;; journal-backed policy snapshot) stay memoized here.
;;
;; WRITE-ONCE LAW: the PR creation POST is attempted EXACTLY once. There is no
;; retry path; resolve-before-create (resolve_existing_pr, then
;; resolve_merged_pr) runs BEFORE the POST, so a lost creation response is
;; recovered by a later action that RESOLVES the durable PR instead of
;; duplicating it. A repeated run of binding-pr on an already-published branch
;; returns `already-published` with no POST at all.
;;
;; Parity law (Python is the contract; refusal strings are byte-identical so the
;; coordinator's reason chain does not change):
;;
;;   read_staged_trio(repo, plan, wave, output, campaign_root, expected_branch)
;;     journal generation -> the deterministic staging path (per-component
;;     symlink refusal) -> `binding output must be a deterministic campaign
;;     staging directory for generation <n>` -> `awaiting-review` when the
;;     directory is absent -> `not a regular directory` -> the three staged
;;     `.rktd` artifacts (`trio is incomplete or unsafe`) -> evidence schema-2,
;;     plan/wave, milestone, issue -> the attempt-bound receipt head/branch ->
;;     full implementation SHAs -> implementation-sha == merge-sha -> squash
;;     publication -> a positive delivery PR -> merge provenance -> the binding
;;     branch identity -> the durable implementation branch -> the required-check
;;     snapshot -> the artifact paths -> the review/validation datums -> the
;;     current policy snapshot at origin/main -> the origin/main rebind refusal
;;     -> the placeholder contract (`pending-review`) or the finalized contract
;;     plus the UNCHANGED strict wave gate (`reviewed`).
;;
;;   binding_review = read_staged_trio with the campaign root (or the repo root).
;;
;;   binding_publish(repo, plan, wave, output, campaign_root)
;;     binding_review -> `binding staging is not ready to publish` -> slug ->
;;     fresh origin/main -> journal generation -> binding branch ->
;;     an EXISTING branch is validated first (merged publication ->
;;     `already-published`, else fresh-main single-parent + datum/byte equality
;;     with the staged trio), otherwise a detached worktree on origin/main
;;     builds the evidence-only publication commit -> ONE non-force push ->
;;     re-fetch + tip equality -> resolve-before-create (OPEN PR first, then a
;;     merged PR -> `already-published`) -> ONE gh POST with Python's exact
;;     title/body argv -> refreshed PR re-read -> open + same-repo identity +
;;     branch/head equality.
;;
;;   binding_ci(repo, number, expected_branch, expected_head)
;;     --pr/--expected-branch/--expected-head -> slug -> refresh -> origin/main
;;     -> refreshed PR read -> open -> same-repo identity -> head equality ->
;;     branch identity -> fetched refs/pull/<n>/head equality -> policy snapshot
;;     -> protection -> one trusted `pull_request` check per name -> green.
;;
;;   binding_merge = merge: the payload, the refusal ladder, the single squash
;;     PUT and the post-PUT re-read all come from delivery-merge! (S7).
;;
;; Reuse vs local implementation (everything unexported upstream is mirrored
;; here with the same argv and the same refusal strings):
;;   * REUSED: repository/refresh/branch-owner/resolve-prs-for-branch/
;;     resolve-existing-pr/resolve-merged-pr/validate-pr-identity/
;;     require-receipt-tip/policy-names/blob-or-none/binding-path/artifact-path/
;;     excluded-prefixes (S4), validate-trio/changed-evidence/tree-of/
;;     publication-pr/binding-branch/campaign-journal/campaign-journal-root/
;;     journal-generation/governance-check-name (S6), gh-put/gh-post/
;;     mutation-argv (S5), delivery-merge!/require-binding-receipt/draft-request
;;     (S7), full-sha?/checks/trusted-check/protection/fetch-head/validate-merge
;;     (S3).
;;   * LOCAL (unexported upstream): read_staged_trio, binding_staging_path,
;;     require_binding_output, _binding_bundle_files, the rebind inspection of
;;     the origin/main binding, the finalized-triple strict-gate invocation with
;;     its owned-proof mirroring, and the detached-worktree publication commit.
;;
;; Documented deviations (all fail closed, never open):
;;   * Python's `Path.resolve()` is `path->complete-path` + `simplify-path`, the
;;     same convention S7 adopted: a staging path containing a symlinked
;;     COMPONENT is reported verbatim instead of being resolved. Every symlink
;;     guard Python performs (staging path components, the output path itself,
;;     the staged files) is still enforced, so only an echoed path string can
;;     differ.
;;   * `python: cwd=gate_root` is not set for the strict binding gate: the
;;     bounded runner has no cwd parameter, and gsd-wave-gate.rkt takes `--root`
;;     explicitly (its cwd default is never used).
;;   * Python's `except Pending` around the pre-existing binding branch probe is
;;     narrowed to non-infrastructure refusals (same as blob-or-none): a spent
;;     budget or a runner fault is re-raised instead of being read as "the
;;     branch does not exist".
;;   * Python's `SENTINEL_TEXT.fullmatch`/`SENTINEL_COMPOUND.search` anchors are
;;     Racket `^`/`$` (end of input, no pre-trailing-newline match); a reviewer
;;     identity whose only difference is a trailing newline is refused here and
;;     accepted there, in the fail-closed direction.
;;   * `read_datum` failures on the OWNED origin/main binding are re-labelled
;;     `origin/main binding is malformed; refusing rebind` exactly like Python
;;     (which catches Pending around its read_datum).
;;   * a helper subprocess failure keeps the seam's typed cause; only an
;;     unreadable helper payload becomes the documented malformed-datum refusal
;;     (Python surfaces the JSONDecodeError text instead).
;;   * the coordinator journal is read ONCE per staged-trio proof and both the
;;     publication generation and the attempt-bound receipt are derived from
;;     that ONE read. Python reads it twice (journal_generation, then
;;     require_binding_receipt) through the same trusted loader, so every
;;     refusal is identical; the port simply drops the redundant subprocess and
;;     the (previously racy) chance that the two reads disagree.

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
         "gsd-delivery-status.rkt"
         "gsd-delivery-merge.rkt")

(provide delivery-binding-review!
         delivery-binding-pr!
         delivery-binding-ci!
         delivery-binding-merged!
         ;; deterministic helpers (exported so a test can pin the exact contract)
         binding-staging-path
         require-binding-output
         read-staged-trio
         binding-publish-title
         binding-publish-body
         staged-bundle-files
         empty-sha
         ;; CLI surface (same JSON shapes as the Python actions)
         delivery-binding-action!
         delivery-binding-main)

;; ============================================================
;; Constants (mirror scripts/gsd-delivery.py)
;; ============================================================

(define-runtime-path this-source "gsd-delivery-binding.rkt")
(define scripts-dir (path-only this-source))
(define binding-data-script (build-path scripts-dir "gsd-binding-data.rkt"))
(define wave-gate-script (build-path scripts-dir "gsd-wave-gate.rkt"))

(define policy-relative-path "scripts/required-pr-checks.policy")
(define stage-directory "binding-w")

;; Python EMPTY_SHA = hashlib.sha256(b'').hexdigest(): the publication commit
;; only changes excluded evidence directories, so its changed-content digest is
;; the digest of an empty diff.
(define empty-sha (bytes->hex-string (sha256-bytes #"")))

;; Python's helper subprocesses all pass `timeout=60`; the bounded runner clamps
;; it to the remaining total process budget.
(define helper-command-timeout 60.0)

;; Register F12 sentinels: a finalized binding review must name a reviewer that
;; existed. SENTINEL_TEXT is the whole-string placeholder; SENTINEL_COMPOUND is
;; the boundary-anchored compound placeholder (e.g. "PENDING-INDEPENDENT-REVIEW")
;; that is not a whole-string match but still encodes placeholder semantics.
(define sentinel-text-rx
  #px"^(?i:pending|todo|tbd|tbc|tba|placeholder|place-holder|fixme|xxx|n/a|na)[:.;!,?]*$")
(define sentinel-compound-rx
  #px"(?i:(?:^|[^a-z])(?:pending|todo|tbd|tbc|tba|placeholder|place-holder|fixme|xxx)(?:[^a-z]|$))")

;; ============================================================
;; Small helpers (same semantics as the S3–S7 modules)
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

;; Python `Path(x).resolve()` as this port spells it (see the deviation note).
(define (absolute value)
  (simplify-path (path->complete-path (as-path value))))

(define (exists-any? p)
  (or (file-exists? p) (directory-exists? p) (link-exists? p)))

;; The freshly fetched default-branch head (the S4 accessor is private there).
(define (origin-main git)
  (define main (string-trim (git (list "rev-parse" "refs/remotes/origin/main"))))
  (unless (full-sha? main)
    (refuse! "malformed origin/main head"))
  main)

;; A git read whose ABSENCE is repository state: a non-infrastructure refusal
;; yields #f (Python's `except Pending`), a spent budget or runner fault is
;; re-raised so it can never be mistaken for "the ref does not exist".
(define (git-or-none git args)
  (with-handlers ([exn:fail:delivery-action? (lambda (error)
                                               (if (infrastructure-refusal? (exn-message error))
                                                   (raise error)
                                                   #f))])
    (git args)))

;; The commit's own SHA followed by its parents (Python's
;; `git rev-list --parents -n 1 <rev>` split on whitespace).
(define (commit-parents git rev)
  (define out (git (list "rev-list" "--parents" "-n" "1" rev)))
  (for/list ([token (in-list (string-split (string-trim out) " "))]
             #:unless (string=? token ""))
    token))

;; One `.rktd` datum through the trusted reader (the module Python spawns). A
;; helper failure keeps the seam's typed cause; only an unreadable datum becomes
;; the documented malformed-datum refusal.
(define (read-binding-datum run path)
  (define text
    (run "racket"
         (list (path->string binding-data-script) "read" (path->string path))
         helper-command-timeout))
  (with-handlers ([exn:fail? (lambda (_error)
                               (refuse! (string-append "malformed binding datum: "
                                                       (path->string path))))])
    (string->jsexpr text)))

;; The same reader over an in-memory blob (a committed binding datum).
(define (read-datum-bytes run bytes)
  (define root (make-temporary-file "q-delivery-binding-datum-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define path (build-path root "binding.rktd"))
     (call-with-output-file path #:exists 'truncate (lambda (out) (write-string bytes out)))
     (read-binding-datum run path))
   (lambda () (delete-directory/files root #:must-exist? #f))))

;; Every regular file of an audit binding's owned-proof bundle, keyed by its
;; repo-relative path (Python `_binding_bundle_files`). A staged bundle that
;; escapes its own directory is refused; nothing is ever written here.
(define (staged-bundle-files output)
  (define parent (build-path (as-path output) "docs/reports/gsd-wave-validation"))
  (define proofs (build-path parent "owned-proofs"))
  (cond
    [(and (directory-exists? proofs) (not (link-exists? proofs)))
     (for/fold ([files (hash)]) ([payload (in-list (sort (for/list ([p (in-directory proofs)]) p)
                                                        path<?))])
       (cond
         [(and (file-exists? payload) (not (link-exists? payload)))
          (define relative (find-relative-path parent payload))
          (when (or (absolute-path? relative) (member 'up (explode-path relative)))
            (refuse! (string-append "unsafe staged owned-proof bundle path: "
                                    (path->string payload))))
          (hash-set files
                    (path->string (build-path "docs"
                                              "reports"
                                              "gsd-wave-validation"
                                              relative))
                    payload)]
         [else files]))]
    [else (hash)]))

;; ============================================================
;; The deterministic staging directory (binding_staging_path)
;; ============================================================

;; The ONE directory a binding draft for this plan/wave/generation may be staged
;; in. Every existing path component is probed for a symlink (a later symlink
;; must never redirect the durable staging directory), and the campaign root
;; itself must be a real directory.
(define (binding-staging-path campaign-root plan wave [generation 0])
  (binding-path plan wave)
  (ensure! (and (exact-integer? generation) (>= generation 0)) "invalid binding generation")
  (define root (as-path campaign-root))
  (unless (and (directory-exists? root) (not (link-exists? root)))
    (refuse! "binding staging requires a real campaign root"))
  (define staging
    (if (= generation 0)
        (format "~a~a" stage-directory wave)
        (format "~a~a-r~a" stage-directory wave generation)))
  (for/fold ([current root]) ([part (in-list (list ".planning" "campaigns" plan staging))])
    (define next (build-path current part))
    (when (link-exists? next)
      (refuse! "binding staging path contains a symlink"))
    next)
  (simplify-path (build-path (absolute root) ".planning" "campaigns" plan staging)))

;; `--output` must BE that directory (not a symlink, not another generation).
(define (require-binding-output output campaign-root plan wave [generation 0])
  (define expected (binding-staging-path campaign-root plan wave generation))
  (define raw (as-path output))
  (when (link-exists? raw)
    (refuse! "binding output path must not be a symlink"))
  (define actual (absolute raw))
  (unless (equal? actual expected)
    (refuse! (format "binding output must be the deterministic campaign staging directory ~a"
                     expected)))
  expected)

;; ============================================================
;; read_staged_trio (binding-review)
;; ============================================================

;; The three staged artifact paths for one generation: every staged trio uses
;; the SAME base name in the three evidence directories.
(define (staged-artifact-paths staging plan wave generation)
  (define artifact (last (string-split (binding-path plan wave generation) "/")))
  (for/hash ([kind (in-list '("evidence" "reviews" "validation"))])
    (values kind (build-path staging "docs/reports" (string-append "gsd-wave-" kind) artifact))))

;; A staged directory that already holds a FINALIZED trio is validated through
;; the unchanged strict wave gate. The gate reads the trio from its own throwaway
;; root (never from the staging tree), mirrors an audit owned-proof bundle when
;; one is staged, and uses the policy blob from origin/main — exactly Python's
;; `q-delivery-binding-gate-` block.
(define (run-binding-gate! ctx staging plan wave generation git main)
  (define run (delivery-pr-context-run ctx))
  (define paths (staged-artifact-paths staging plan wave generation))
  (define artifact (last (string-split (binding-path plan wave generation) "/")))
  (define gate-root (make-temporary-file "q-delivery-binding-gate-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (for ([kind (in-list '("evidence" "reviews" "validation"))])
       (define source (hash-ref paths kind))
       (define target
         (build-path gate-root "docs/reports" (string-append "gsd-wave-" kind) artifact))
       (make-directory* (path-only target))
       (call-with-output-file target
                              #:exists
                              'truncate
                              (lambda (out) (write-bytes (file->bytes source) out))))
     (define proofs-src
       (build-path staging "docs/reports/gsd-wave-validation/owned-proofs"))
     (when (and (directory-exists? proofs-src) (not (link-exists? proofs-src)))
       (for ([payload (in-list (for/list ([p (in-directory proofs-src)]) p))])
         (when (and (file-exists? payload) (not (link-exists? payload)))
           (define relative (find-relative-path proofs-src payload))
           (define target
             (build-path gate-root
                         "docs/reports/gsd-wave-validation/owned-proofs"
                         relative))
           (make-directory* (path-only target))
           (call-with-output-file target
                                  #:exists
                                  'truncate
                                  (lambda (out) (write-bytes (file->bytes payload) out))))))
     (define policy (blob-or-none git main policy-relative-path))
     (unless policy
       (refuse! "binding gate policy is unavailable"))
     (define policy-path (build-path gate-root policy-relative-path))
     (make-directory* (path-only policy-path))
     (call-with-output-file policy-path
                            #:exists
                            'truncate
                            (lambda (out) (write-string policy out)))
     (define out
       (run "racket"
            (list (path->string wave-gate-script)
                  (path->string (build-path gate-root "docs/reports/gsd-wave-evidence" artifact))
                  "--content-digest"
                  empty-sha
                  "--root"
                  (path->string gate-root)
                  "--policy"
                  (path->string policy-path))
            helper-command-timeout))
     (unless (string-contains? out "GSD wave evidence PASS")
       (refuse! "finalized binding trio failed the strict wave gate")))
   (lambda () (delete-directory/files gate-root #:must-exist? #f))))

;; Python reads the coordinator journal TWICE per staged-trio proof
;; (`journal_generation`, then `require_binding_receipt`). Both reads go through
;; the SAME trusted loader and therefore produce the same refusals, so this port
;; performs ONE read and derives both facts from it: the redundant subprocess is
;; gone and the (previously racy) second read cannot disagree with the first.
(define (journal-generation-of journal)
  (cond
    [(eq? journal #f) 0]
    [else
     (define history (hash-ref journal 'receipt-history '()))
     (ensure! (list? history) "invalid coordinator journal response")
     (length history)]))

;; The deterministic staged-trio reader. Never manufactures a review: it proves
;; that a durable draft exists and, for a finalized draft, that it passes the
;; unchanged strict gate; the `reviewed` status therefore always means "a
;; genuine reviewer finalized this staged trio".
(define (read-staged-trio repo
                          plan
                          wave
                          output
                          campaign-root
                          expected-branch
                          #:context [context #f])
  (define ctx (or context (make-delivery-pr-context repo)))
  (define git (delivery-pr-context-git ctx))
  (define run (delivery-pr-context-run ctx))
  (define journal-root (campaign-journal-root repo campaign-root))
  (define journal (campaign-journal run journal-root plan wave))
  (define generation (journal-generation-of journal))
  ;; A staging directory may belong to ANY deterministic generation; the journal
  ;; names where a NEW draft would be staged, and the strict gates below do all
  ;; the semantic work either way.
  (define expected
    (with-handlers ([exn:fail:delivery-action? (lambda (_error) #f)])
      (require-binding-output output campaign-root plan wave generation)))
  (unless expected
    (refuse!
     (format "binding output must be a deterministic campaign staging directory for generation ~a"
             generation)))
  (cond
    [(not (exists-any? expected))
     (hasheq 'status
             "awaiting-review"
             'output
             (path->string expected)
             'branch
             (binding-branch plan wave generation))]
    [else
     (unless (and (directory-exists? expected) (not (link-exists? expected)))
       (refuse! "binding staging output is not a regular directory"))
     (define paths (staged-artifact-paths expected plan wave generation))
     (for ([path (in-list (for/list ([kind (in-list '("evidence" "reviews" "validation"))])
                            (hash-ref paths kind)))])
       (unless (and (file-exists? path) (not (link-exists? path)))
         (refuse! (string-append "binding staging trio is incomplete or unsafe: "
                                 (path->string (file-name-from-path path))))))
     (define evidence (read-binding-datum run (hash-ref paths "evidence")))
     (define review (read-binding-datum run (hash-ref paths "reviews")))
     (define validation (read-binding-datum run (hash-ref paths "validation")))
     (unless (and (hash? evidence) (equal? (hash-ref evidence 'schema-version #f) 2))
       (refuse! "staged binding must contain a schema-2 evidence datum"))
     (unless (and (equal? (hash-ref evidence 'plan-id #f) plan)
                  (equal? (hash-ref evidence 'wave #f) (format "W~a" wave)))
       (refuse! "staged binding plan/wave identity mismatch"))
     (define milestone (hash-ref evidence 'milestone #f))
     (unless (and (exact-integer? milestone) (> milestone 0))
       (refuse! "staged binding milestone is invalid"))
     (define issue (hash-ref evidence 'issue #f))
     (unless (and (exact-integer? issue) (> issue 0))
       (refuse! "staged binding issue is invalid"))
     (define merge (hash-ref evidence 'merge-sha #f))
     (define head (hash-ref evidence 'delivery-head-sha #f))
     (require-binding-receipt journal head (hash-ref evidence 'wave-branch #f))
     (unless (and (full-sha? merge) (full-sha? head))
       (refuse! "staged binding must carry full implementation SHAs"))
     (unless (equal? (hash-ref evidence 'implementation-sha #f) merge)
       (refuse! "staged binding implementation-sha differs from merge-sha"))
     (unless (equal? (hash-ref evidence 'merge-method #f) "squash")
       (refuse! "staged binding must use squash publication"))
     (define delivery-pr (hash-ref evidence 'delivery-pr #f))
     (unless (and (exact-integer? delivery-pr) (> delivery-pr 0))
       (refuse! "staged binding has no implementation PR identity"))
     (unless (nonblank-string? (hash-ref evidence 'merged-at #f))
       (refuse! "staged binding has no merge provenance"))
     (define branch (binding-branch plan wave generation))
     (unless (equal? (hash-ref evidence 'branch #f) branch)
       (refuse! "staged binding branch identity mismatch"))
     (when (not (absent? expected-branch))
       (unless (equal? (hash-ref evidence 'wave-branch #f) expected-branch)
         (refuse! "staged binding implementation branch does not match the durable receipt")))
     (define snapshot (hash-ref evidence 'required-pr-checks #f))
     (unless (and (list? snapshot) (pair? snapshot))
       (refuse! "staged binding has no required-check policy snapshot"))
     (define artifact (last (string-split (binding-path plan wave generation) "/")))
     (define expected-review (format "docs/reports/gsd-wave-reviews/~a" artifact))
     (define expected-validation (format "docs/reports/gsd-wave-validation/~a" artifact))
     (unless (and (equal? (hash-ref evidence 'review-artifact #f) expected-review)
                  (equal? (hash-ref evidence 'validation-artifact #f) expected-validation))
       (refuse! "staged binding artifact paths do not match plan/wave identity"))
     (unless (and (hash? review) (hash? validation))
       (refuse! "staged binding review/validation artifacts are malformed"))
     (refresh git)
     (define main (origin-main git))
     (unless (equal? snapshot (policy-names git main))
       (refuse! "staged binding required-check snapshot differs from the current policy"))
     (define binding (binding-path plan wave generation))
     (define published (blob-or-none git main binding))
     (when published
       (define bound
         (with-handlers ([exn:fail:delivery-action?
                          (lambda (_error)
                            (refuse! "origin/main binding is malformed; refusing rebind"))])
           (read-datum-bytes run published)))
       (define is-publication
         (and (hash? bound)
              (for/and ([key (in-list '(merge-sha delivery-head-sha delivery-pr implementation-sha))])
                (not (absent? (hash-ref bound key #f))))))
       (unless (and (hash? bound)
                    (or (not is-publication) (full-sha? (hash-ref bound 'merge-sha #f))))
         (refuse! "origin/main binding is malformed; refusing rebind"))
       (unless (or (not is-publication)
                   (and (equal? (hash-ref bound 'merge-sha #f) merge)
                        (equal? (hash-ref bound 'delivery-head-sha #f) head)
                        (equal? (hash-ref bound 'implementation-sha #f) merge)))
         (refuse!
          (string-append
           "origin/main already binds this wave to a different implementation; refusing rebind"))))
     (cond
       [(equal? (hash-ref evidence 'status #f) "pending-review")
        ;; The draft is deliberately not a second approval channel: its honest
        ;; placeholders are validated as data, and publication stays blocked
        ;; until a genuine independent reviewer finalizes the staged trio.
        (unless (and (equal? (hash-ref review 'verdict #f) "PENDING")
                     (equal? (hash-ref review 'reviewed-sha #f) merge)
                     (equal? (hash-ref review 'content-digest #f) "PENDING"))
          (refuse! "staged binding review placeholders do not match the pending draft contract"))
        (unless (and (equal? (hash-ref validation 'status #f) "pending")
                     (equal? (hash-ref validation 'implementation-sha #f) merge)
                     (equal? (hash-ref validation 'content-digest #f) "PENDING")
                     (equal? (hash-ref validation 'review-artifact #f) expected-review))
          (refuse! "staged binding validation placeholders do not match the pending draft contract"))
        (for ([field (in-list '("red-first" "focused-tests" "format-compile" "lint" "fast"))])
          (unless (hash? (hash-ref validation (string->symbol field) #f))
            (refuse! (string-append "staged binding validation gate evidence is malformed: "
                                    field))))
        (hasheq 'status
                "pending-review"
                'output
                (path->string expected)
                'binding
                binding
                'branch
                branch
                'merge-sha
                merge
                'delivery-head-sha
                head
                'implementation-sha
                merge)]
       [else
        (unless (equal? (hash-ref evidence 'status #f) "ready-for-merge")
          (refuse! "staged trio status must be pending-review or ready-for-merge"))
        (unless (equal? (hash-ref evidence 'content-digest #f) empty-sha)
          (refuse! "finalized binding evidence must use the excluded-diff digest"))
        (define reviewer (hash-ref review 'reviewer #f))
        (unless (and (nonblank-string? reviewer)
                     (not (regexp-match? sentinel-text-rx (string-trim reviewer)))
                     (not (regexp-match? sentinel-compound-rx (string-trim reviewer))))
          (refuse! "finalized binding review has no genuine reviewer identity"))
        (unless (and (equal? (hash-ref review 'verdict #f) "APPROVED")
                     (equal? (hash-ref review 'reviewed-sha #f) merge)
                     (equal? (hash-ref review 'content-digest #f) empty-sha))
          (refuse! "finalized binding review is not bound to the publication evidence"))
        (unless (and (equal? (hash-ref validation 'status #f) "current")
                     (equal? (hash-ref validation 'implementation-sha #f) merge)
                     (equal? (hash-ref validation 'content-digest #f) empty-sha)
                     (equal? (hash-ref validation 'branch #f) branch)
                     (equal? (hash-ref validation 'review-artifact #f) expected-review)
                     (equal? (hash-ref validation 'planning-sync #f) "current"))
          (refuse! "finalized binding validation is not bound to the publication evidence"))
        (run-binding-gate! ctx expected plan wave generation git main)
        (hasheq 'status
                "reviewed"
                'output
                (path->string expected)
                'binding
                binding
                'branch
                branch
                'merge-sha
                merge
                'delivery-head-sha
                head
                'implementation-sha
                merge)])]))

;; ============================================================
;; binding-review
;; ============================================================

;; Validate a durable staged binding draft without manufacturing review.
(define (delivery-binding-review! repo
                                  plan
                                  wave
                                  output
                                  #:campaign-root [campaign-root #f]
                                  #:expected-branch [expected-branch #f]
                                  #:context [context #f])
  (binding-path plan wave)
  (define root (if campaign-root campaign-root (as-path repo)))
  (read-staged-trio repo plan wave output root expected-branch #:context context))

;; ============================================================
;; binding-pr (binding_publish)
;; ============================================================

;; Python's exact publication title and body (byte-identical, newline included).
(define (binding-publish-title plan wave branch)
  (format "W~a binding publication: ~a" wave branch))

(define (binding-publish-body plan wave commit branch binding)
  (string-append (format "Plan: ~a\n" plan)
                 (format "Wave: W~a\n" wave)
                 (format "Head: ~a\n" commit)
                 (format "Branch: ~a\n" branch)
                 (format "Binding evidence: ~a\n" binding)
                 (string-append "Review and approval: genuine independent human review "
                                "required at the exact binding head.\n")))

;; An existing binding branch may be REUSED only when it is a fresh-main
;; single-parent commit whose three binding artifacts are datum-equal to the
;; staged trio (and whose audit bundle is byte-equal). The strict digest gate
;; still binds the eventual publication content.
(define (require-existing-binding! ctx output tip plan wave generation)
  (define git (delivery-pr-context-git ctx))
  (define run (delivery-pr-context-run ctx))
  (define paths (staged-artifact-paths (as-path output) plan wave generation))
  (for ([kind (in-list '("evidence" "reviews" "validation"))])
    (define relative
      (path->string
       (find-relative-path
        (as-path output)
        (hash-ref paths kind))))
    (define blob (blob-or-none git tip relative))
    (unless blob
      (refuse! (string-append "existing binding branch is missing " relative)))
    (unless (equal? (read-datum-bytes run blob)
                    (read-binding-datum run (hash-ref paths kind)))
      (refuse! "existing binding branch does not match the staged trio")))
  (for ([(relative source) (in-hash (staged-bundle-files output))])
    (define blob (blob-or-none git tip relative))
    (unless blob
      (refuse! (string-append "existing binding branch is missing the audit bundle: "
                              relative)))
    (unless (equal? (string->bytes/utf-8 blob) (file->bytes source))
      (refuse! (string-append "existing binding branch audit bundle differs from staging: "
                              relative))))
  tip)

;; Build the evidence-only publication commit in a detached worktree on
;; origin/main and return its SHA. The worktree is always removed.
(define (create-binding-commit! ctx output plan wave generation main)
  (define git (delivery-pr-context-git ctx))
  (define run (delivery-pr-context-run ctx))
  (define paths (staged-artifact-paths (as-path output) plan wave generation))
  (define worktree (make-temporary-file "q-delivery-binding-~a" 'directory))
  (define (worktree-git args)
    (run "git" (append (list "-C" (path->string worktree)) args) command-timeout-budget))
  (dynamic-wind
   void
   (lambda ()
     (git (list "worktree" "add" "--detach" (path->string worktree) main))
     (for ([kind (in-list '("evidence" "reviews" "validation"))])
       (define source (hash-ref paths kind))
       (unless (and (file-exists? source) (not (link-exists? source)))
         (refuse! "staged binding trio is not a regular file"))
       (define target
         (build-path worktree "docs/reports" (string-append "gsd-wave-" kind)
                     (path->string (file-name-from-path source))))
       (make-directory* (path-only target))
       (call-with-output-file target
                              #:exists
                              'truncate
                              (lambda (out) (write-bytes (file->bytes source) out))))
     (for ([(relative source) (in-hash (staged-bundle-files output))])
       (define target (build-path worktree relative))
       (when (link-exists? target)
         (refuse! (string-append "staged audit bundle path is unsafe: " relative)))
       (make-directory* (path-only target))
       (call-with-output-file target
                              #:exists
                              'truncate
                              (lambda (out) (write-bytes (file->bytes source) out))))
     (worktree-git (list "add" "-A"))
     (worktree-git (list "commit"
                         "--no-gpg-sign"
                         "-m"
                         (format "W~a binding draft: ~a-w~a" wave plan wave)))
     (define commit (string-trim (worktree-git (list "rev-parse" "HEAD"))))
     (define parents (commit-parents worktree-git "HEAD"))
     (unless (and (= 2 (length parents))
                  (equal? (car parents) commit)
                  (equal? (cadr parents) main))
       (refuse! "binding publication commit is not based on fresh origin/main"))
     commit)
   (lambda ()
     (with-handlers ([exn:fail:delivery-action? void])
       (git (list "worktree" "remove" "--force" (path->string worktree)))))))

;; Publish a validated binding draft on a fresh-main binding branch, then
;; resolve-before-create its PR.
(define (delivery-binding-pr! repo
                              plan
                              wave
                              output
                              #:campaign-root [campaign-root #f]
                              #:context [context #f])
  (define ctx (or context (make-delivery-pr-context repo)))
  ;; ONE context, ONE budget: the review step spends the SAME budget.
  (define staged
    (delivery-binding-review! repo plan wave output #:campaign-root campaign-root #:context ctx))
  (unless (equal? (hash-ref staged 'status #f) "reviewed")
    (refuse! "binding staging is not ready to publish"))
  (define git (delivery-pr-context-git ctx))
  (define api (delivery-pr-context-api ctx))
  (define run (delivery-pr-context-run ctx))
  (define slug (repository git))
  (refresh git)
  (define main (origin-main git))
  (define journal-root (campaign-journal-root repo campaign-root))
  (define generation (journal-generation run journal-root plan wave))
  (define branch (binding-branch plan wave generation))
  (define remote-ref (string-append "refs/remotes/origin/" branch))
  (define refspec (string-append "refs/heads/" branch))
  (define binding (binding-path plan wave generation))
  (define remote-tip (git-or-none git (list "rev-parse" remote-ref)))
  ;; Either the final `already-published` payload (hash) or the commit to push
  ;; (string). An existing branch NEVER republishes without proving identity.
  (define outcome
    (cond
      [remote-tip
       (define tip (string-trim remote-tip))
       (unless (full-sha? tip)
         (refuse! "malformed existing binding branch tip"))
       ;; Already-published resume: the publication completed, so a lawfully
       ;; advanced protected main is not a republish trigger.
       (define merged-publication (resolve-merged-pr api slug branch #f))
       (cond
         [merged-publication
          (hasheq 'status
                  "already-published"
                  'pr
                  (json-null (hash-ref merged-publication 'number #f))
                  'branch
                  branch
                  'head
                  tip
                  'merge-sha
                  (json-null (dig merged-publication 'merge_commit_sha)))]
         [else
          (define parents (commit-parents git remote-ref))
          (unless (and (= 2 (length parents)) (equal? (cadr parents) main))
            (refuse! "existing binding branch is not based on fresh origin/main"))
          (require-existing-binding! ctx output tip plan wave generation)
          tip])]
      [else (create-binding-commit! ctx output plan wave generation main)]))
  (cond
    [(hash? outcome) outcome]
    [else
     (define commit outcome)
     ;; The ONE push, then the re-read that proves the ref landed there.
     (git (list "push" "--no-tags" "origin" (format "~a:~a" commit refspec)))
     (git (list "fetch" "--no-tags" "origin" (format "~a:~a" refspec remote-ref)))
     (define tip (string-trim (git (list "rev-parse" remote-ref))))
     (unless (equal? tip commit)
       (refuse! "binding branch did not publish at the verified commit"))
     ;; Resolve-before-create: an OPEN PR wins, then a merged PR, and only then
     ;; the ONE creation POST.
     (define existing (resolve-existing-pr api slug branch))
     (define merged
       (and (not existing) (resolve-merged-pr api slug branch #f)))
     (cond
       [merged
        (hasheq 'status
                "already-published"
                'pr
                (json-null (hash-ref merged 'number #f))
                'branch
                branch
                'head
                commit)]
       [else
        (define-values (number status pr)
          (if existing
              (let ([resolved (hash-ref existing 'number #f)])
                (unless (and (exact-integer? resolved) (> resolved 0))
                  (refuse! "resolved binding pull request has no usable number"))
                (values resolved
                        "exists"
                        (api slug (format "pulls/~a" resolved) #:refresh? #t)))
              (let ([created (gh-post ctx
                                      slug
                                      "pulls"
                                      (list (cons "title" (binding-publish-title plan wave branch))
                                            (cons "body"
                                                  (binding-publish-body plan wave commit branch binding))
                                            (cons "head" branch)
                                            (cons "base" "main")))])
                (define created-number (hash-ref created 'number #f))
                (unless (and (exact-integer? created-number) (> created-number 0))
                  (refuse!
                   "GitHub pull-request creation response has no usable number"))
                (values created-number
                        "opened"
                        (api slug (format "pulls/~a" created-number) #:refresh? #t)))))
        (unless (and (hash? pr) (equal? (hash-ref pr 'state #f) "open"))
          (refuse! "binding pull request is not open"))
        (validate-pr-identity pr slug)
        (unless (and (equal? (dig pr 'head 'ref) branch)
                     (equal? (dig pr 'head 'sha) commit))
          (refuse! "binding pull request does not match the verified publication branch/head"))
        (hasheq 'status status 'pr number 'branch branch 'head commit)])]))

;; ============================================================
;; binding-ci
;; ============================================================

;; Check a binding PR at its exact head without implementation ancestry rules.
(define (delivery-binding-ci! repo
                              number
                              expected-branch
                              expected-head
                              #:context [context #f])
  (unless (and (exact-integer? number) (> number 0))
    (refuse! "binding-ci requires --pr"))
  (unless (nonblank-string? expected-branch)
    (refuse! "binding-ci requires --expected-branch"))
  (unless (full-sha? expected-head)
    (refuse! "binding-ci requires --expected-head"))
  (define ctx (or context (make-delivery-pr-context repo)))
  (define git (delivery-pr-context-git ctx))
  (define api (delivery-pr-context-api ctx))
  (define session (delivery-pr-context-session ctx))
  (define slug (repository git))
  (refresh git)
  (define main (origin-main git))
  ;; A fresh read: a memoized stage-1 read must never satisfy this proof.
  (define pr (api slug (format "pulls/~a" number) #:refresh? #t))
  (unless (and (hash? pr) (equal? (hash-ref pr 'state #f) "open"))
    (refuse! "binding pull request is not open"))
  (validate-pr-identity pr slug)
  (define head (dig pr 'head 'sha))
  (unless (and (full-sha? head) (equal? head expected-head))
    (refuse! "binding pull request head does not match the verified expected head"))
  (unless (equal? (dig pr 'head 'ref) expected-branch)
    (refuse! "binding pull request branch does not match expected branch"))
  ;; Python fetches refs/pull/<number>/head with the CLI number.
  (define fetched ((delivery-api-session-fetch-head session) number))
  (unless (equal? fetched head)
    (refuse! "binding pull-request head does not match fetched ref"))
  (define names (policy-names git main))
  ((delivery-api-session-protection session) slug names)
  (for ([name (in-list names)])
    ((delivery-api-session-trusted-check session) slug
                                                  head
                                                  name
                                                  "pull_request"
                                                  #:branch expected-branch))
  (hasheq 'status "green" 'pr number 'branch expected-branch 'head head))

;; ============================================================
;; binding-merged
;; ============================================================

;; Run the unchanged protected merge gate for finalized binding evidence. The
;; whole action — payload, refusal ladder, the single squash PUT and the
;; post-PUT re-read — is delivery-merge!'s (S7); binding evidence differs only
;; in which trio the reviewer finalized.
(define (delivery-binding-merged! repo
                                  plan
                                  wave
                                  number
                                  expected-head
                                  expected-branch
                                  source
                                  #:context [context #f])
  (delivery-merge! repo
                   plan
                   wave
                   number
                   expected-head
                   expected-branch
                   source
                   #:context context))

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

(define (delivery-binding-action! argv)
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
  (define expected-branch (hash-ref flags "--expected-branch" #f))
  (define expected-head (hash-ref flags "--expected-head" #f))
  (define evidence (hash-ref flags "--evidence" #f))
  (define output (hash-ref flags "--output" #f))
  (define campaign-root (hash-ref flags "--campaign-root" #f))
  (cond
    [(equal? action "binding-review")
     (unless (and output campaign-root)
       (refuse! "binding-review requires --output and --campaign-root"))
     (delivery-binding-review! repo
                               plan
                               wave
                               output
                               #:campaign-root campaign-root
                               #:expected-branch expected-branch)]
    [(equal? action "binding-pr")
     (unless (and output campaign-root)
       (refuse! "binding-pr requires --output and --campaign-root"))
     (delivery-binding-pr! repo plan wave output #:campaign-root campaign-root)]
    [(equal? action "binding-ci")
     (unless (and number expected-branch expected-head)
       (refuse! "binding-ci requires --pr, --expected-branch and --expected-head"))
     (delivery-binding-ci! repo number expected-branch expected-head)]
    [(equal? action "binding-merge")
     (unless (and number expected-branch expected-head evidence)
       (refuse! "binding-merge requires --pr, --expected-branch, --expected-head and --evidence"))
     (delivery-binding-merged! repo plan wave number expected-head expected-branch evidence)]
    [else
     (refuse!
      (format
       "unsupported action ~a; this module implements binding-review, binding-pr, binding-ci and binding-merge only"
       (or action "")))]))

;; Success -> JSON on stdout, exit 0. Refusal -> delivery-pending JSON, exit 2
;; (the exact shape delivery-coordinator.rkt's typed-stop parser expects).
(define (delivery-binding-main argv)
  (with-handlers ([exn:fail? (lambda (error)
                               (printf "~a\n" (delivery-pending-json (exn-message error)))
                               (exit 2))])
    (printf "~a\n" (jsexpr-string (delivery-binding-action! argv)))
    (exit 0)))

(module+ main
  (delivery-binding-main (vector->list (current-command-line-arguments))))
