#lang racket/base

;; Phase B slice S11 — native port of the `publish-head` action of the deleted
;; Python delivery controller.
;;
;; Scope law: this module replaces that ONE action — the
;; one implementation-head publication producer the delivery publication owner
;; (extensions/gsd/delivery-publication.rkt, `default-approved-head-publisher`)
;; runs today. Nothing else of the Python controller is reproduced here: the
;; `prepare`/`auto-review`/`_finalize_implementation` ladder (single-parent
;; protected binding commit construction, evidence trio binding, review
;; staging) lives in the separately ported merge/binding slices and is NOT part
;; of this action. `publish_head` performs no evidence binding, writes no local
;; file and updates no journal/status record — the coordinator-owned
;; publication intent/confirmation around it stays exactly where it is
;; (delivery-publication.rkt). This module therefore has ZERO local writes: its
;; only durable effect is the ONE remote ref update, and its only readback is a
;; fresh `git ls-remote` of that exact ref.
;;
;; Byte-identical parity with the Python action is the contract — the check
;; ORDER below is the Python order, so the first failing proof is always the
;; same one:
;;
;;   1. a nonblank --expected-branch is mandatory;
;;   2. main/master are protected and refuse with the branch named;
;;   3. `git check-ref-format --branch <branch>` must normalize to the branch
;;      itself and must not be a raw `refs/...` ref (catches shorthand such as
;;      `@{-1}` that git would expand);
;;   4. when plan OR wave is supplied, BOTH must be exact (64-hex campaign id,
;;      non-negative integer wave) and the branch must be exactly
;;      `campaign/<plan[0:8]>/w<wave>`;
;;   5. the expected head must be a full 40-hex SHA;
;;   6. the CONFIGURED origin URL must be present, and — when the approved
;;      receipt carried one — must equal the expected origin exactly;
;;   7. the approved head object must resolve to itself, the local branch ref
;;      must still point at it, and (when supplied) the approved tree must
;;      match;
;;   8. the remote readback parses at most ONE line, with exactly
;;      `<full-sha> <refs/heads/<branch>>`; empty means unpublished;
;;   9. already-published: re-verify origin/local-ref/tree before returning
;;      idempotent success (`during the already-published readback` — the
;;      readback is itself an external-effect window);
;;  10. an existing remote tip may fast-forward only when it is an ancestor of
;;      the approved head; divergence refuses and NOTHING is pushed;
;;  11. ONE non-force `git push --no-tags origin <head>:<refs/heads/branch>` —
;;      never retried, never forced;
;;  12. re-verify origin/local-ref/tree (`during publication`), re-read the
;;      remote ref and require the exact approved head, then re-verify the
;;      guards AGAIN (`after the final readback`).
;;
;;   success -> {status: "published" | "already-published", branch, head}.
;;
;; The bounded-git runner, the typed exn:fail:delivery-action refusal, the ONE
;; total process budget per action, credential isolation and process-group
;; cleanup are the SHARED seam in scripts/gsd-delivery-git.rkt; `full-sha?` is
;; the shared S3 predicate in scripts/gsd-delivery-api.rkt. This action performs
;; no GitHub API read (the remote proof is `git ls-remote`), so it builds no
;; gh/API session — the one budget is `make-repo-git-command`, exactly like the
;; S2 sync port. Raw git stderr is never surfaced; only the seam's typed cause.
;;
;; Documented deviations (all fail closed, never open):
;;   * a spent budget / broken exec boundary during the `merge-base
;;     --is-ancestor` probe is re-raised with its own typed cause instead of
;;     being relabelled "remote branch diverged…"; only a genuine command
;;     failure (the real divergence signal, exit 1 with no typed cause)
;;     becomes the divergence refusal;
;;   * a `--wave` flag that argparse's `type=int` would reject is refused with
;;     "publish-head requires exact wave index" instead of argparse usage text;
;;   * the CLI refuses a missing `--repo` with the module's own
;;     "publish-head requires --repo" (argparse would print usage), and the
;;     in-process action refuses a non-path `repo` with the same reason
;;     (Python's argparse already guaranteed a Path).
;;
;; Idempotence and the lost-response window: the action never retries its
;; write. A push whose response is lost leaves the refusal to the caller; the
;; ONLY recovery is a FRESH invocation, which re-reads the remote ref and
;; returns "already-published" without a second push.

(require racket/string
         "gsd-delivery-git.rkt"
         (only-in "gsd-delivery-api.rkt" full-sha?))

(provide delivery-publish-head!
         delivery-publish-head-json
         delivery-publish-head-main)

;; ============================================================
;; Constants (mirror the deleted Python controller; recorded in the goldens fixture)
;; ============================================================

(define protected-branches '("main" "master"))
(define plan-id-rx #px"^[0-9a-f]{64}$")

;; ============================================================
;; Small helpers (same semantics as the sibling native actions)
;; ============================================================

(define (nonblank-string? value)
  (and (string? value) (not (string=? (string-trim value) ""))))

;; Python `.strip()` on a subprocess answer.
(define (trimmed value)
  (string-trim value))

;; ============================================================
;; publish_head
;; ============================================================

;; The publication action. Returns the success hash or raises
;; exn:fail:delivery-action carrying the typed reason (byte-identical to the
;; Python `Pending` reason wherever the Python action raises one).
(define (delivery-publish-head! repo
                                expected-branch
                                expected-head
                                #:expected-origin [expected-origin #f]
                                #:expected-tree [expected-tree #f]
                                #:plan [plan #f]
                                #:wave [wave #f])
  (unless (path-string? repo)
    (refuse! "publish-head requires --repo"))
  ;; 1-2. nonblank branch; main/master are protected.
  (unless (nonblank-string? expected-branch)
    (refuse! "publish-head requires --expected-branch"))
  (when (member expected-branch protected-branches)
    (refuse! (string-append "publish-head refuses protected branch " expected-branch)))
  ;; ONE budget for the whole action (the shared bounded-git seam).
  (define git (make-repo-git-command repo))
  ;; 3. Branch names are path components under refs/heads, never raw refs,
  ;; tags, main/master aliases, or shell-ish shorthand.
  (define normalized-branch (trimmed (git (list "check-ref-format" "--branch" expected-branch))))
  (unless (and (equal? normalized-branch expected-branch)
               (not (string-prefix? expected-branch "refs/")))
    (refuse! (string-append "publish-head refuses malformed branch " expected-branch)))
  ;; 4. Production calls bind the branch to campaign/<plan8>/w<wave>; the
  ;; binding is checked whenever EITHER half is supplied (Python's
  ;; `plan is not None or wave is not None`).
  (when (or plan wave)
    (unless (and (string? plan) (regexp-match? plan-id-rx plan))
      (refuse! "publish-head requires exact campaign plan id"))
    (unless (and (exact-integer? wave) (>= wave 0))
      (refuse! "publish-head requires exact wave index"))
    (unless (equal? expected-branch (format "campaign/~a/w~a" (substring plan 0 8) wave))
      (refuse! "publish-head branch is not bound to campaign/wave")))
  ;; 5. The expected head is always a full SHA (never a short/ambiguous ref).
  (unless (and (string? expected-head) (full-sha? expected-head))
    (refuse! "publish-head requires --expected-head"))
  ;; 6. Repository identity: the CONFIGURED origin URL (url.<x>.insteadOf
  ;; rewrites are transport only and never redefine identity).
  (define origin (trimmed (git (list "config" "--get" "remote.origin.url"))))
  (unless (nonblank-string? origin)
    (refuse! "publish-head requires configured origin"))
  (when expected-origin
    (unless (equal? origin expected-origin)
      (refuse! "origin drifted before publication")))
  ;; 7. The approved object must still be exactly what the receipt approved.
  (define local (trimmed (git (list "rev-parse" (string-append expected-head "^{commit}")))))
  (unless (equal? local expected-head)
    (refuse! "approved head object is missing or ambiguous"))
  (define local-ref
    (trimmed (git (list "rev-parse" (string-append "refs/heads/" expected-branch)))))
  (unless (equal? local-ref expected-head)
    (refuse! "local branch ref drifted before publication"))
  (define tree (trimmed (git (list "rev-parse" (string-append expected-head "^{tree}")))))
  (when expected-tree
    (unless (equal? tree expected-tree)
      (refuse! "approved tree drifted before publication")))
  (define remote-ref (string-append "refs/heads/" expected-branch))
  ;; 8. ONE exact parse of the remote readback. An empty answer means the
  ;; branch is unpublished; anything but a single well-formed line refuses.
  (define (read-remote)
    (define out (git (list "ls-remote" "--heads" "origin" remote-ref)))
    (define lines
      (for/list ([line (in-list (string-split out "\n"))]
                 #:when (not (string=? (trimmed line) "")))
        (trimmed line)))
    (unless (<= (length lines) 1)
      (refuse! (string-append "ambiguous remote readback for " remote-ref)))
    (cond
      [(null? lines) #f]
      [else
       (define parts (string-split (car lines)))
       (unless (and (= (length parts) 2)
                    (equal? (cadr parts) remote-ref)
                    (full-sha? (car parts)))
         (refuse! (string-append "malformed remote readback for " remote-ref)))
       (car parts)]))
  ;; 9/12. The ls-remote readback is itself an external-effect window: the
  ;; origin/local-ref/tree guards run again before EVERY return — including
  ;; the already-published one — and again after the final readback.
  (define (verify-local-unchanged! moment)
    (unless (equal? (trimmed (git (list "config" "--get" "remote.origin.url"))) origin)
      (refuse! (string-append "origin drifted " moment)))
    (unless (equal? (trimmed (git (list "rev-parse" (string-append "refs/heads/" expected-branch))))
                    expected-head)
      (refuse! (string-append "local branch ref drifted " moment)))
    (when expected-tree
      (unless (equal? (trimmed (git (list "rev-parse" (string-append expected-head "^{tree}"))))
                      tree)
        (refuse! (string-append "approved tree drifted " moment)))))
  (define before (read-remote))
  (cond
    [(equal? before expected-head)
     (verify-local-unchanged! "during the already-published readback")
     (hasheq 'status "already-published" 'branch expected-branch 'head expected-head)]
    [else
     ;; 10. A remote ancestor may fast-forward; divergence refuses with no push.
     (when before
       (with-handlers ([exn:fail:delivery-action?
                        (lambda (error)
                          (if (infrastructure-refusal? (exn-message error))
                              (raise error)
                              (refuse! "remote branch diverged from approved head; refusing publication")))])
         (git (list "merge-base" "--is-ancestor" before expected-head))))
     ;; 11. EXACTLY ONE non-force publication attempt. Never retried.
     (git (list "push" "--no-tags" "origin" (string-append expected-head ":" remote-ref)))
     (verify-local-unchanged! "during publication")
     (define after (read-remote))
     (unless (equal? after expected-head)
       (refuse! "remote readback did not confirm exact approved head after publication"))
     (verify-local-unchanged! "after the final readback")
     (hasheq 'status "published" 'branch expected-branch 'head expected-head)]))

;; ============================================================
;; JSON / CLI surface (same shape as the Python action)
;; ============================================================

(define (delivery-publish-head-json result)
  (jsexpr-string result))

;; Exact Python main() contract for this action: argparse requires an action and
;; --repo and the action then requires --expected-head/--expected-branch/
;; --expected-origin/--expected-tree. Success -> JSON on stdout, exit 0.
;; Refusal -> {"status":"delivery-pending","reason":...} on stdout, exit 2.
(define (delivery-publish-head-main argv)
  (with-handlers ([exn:fail? (lambda (error)
                               (printf "~a\n" (delivery-pending-json (exn-message error)))
                               (exit 2))])
    (define action (and (pair? argv) (car argv)))
    (define flags
      (read-flag-value (if (pair? argv)
                           (cdr argv)
                           '())))
    (define (flag name)
      (hash-ref flags name #f))
    (unless (equal? action "publish-head")
      (refuse!
       (format "unsupported action ~a; this module implements the publish-head action only"
               (or action ""))))
    (unless (flag "--repo")
      (refuse! "publish-head requires --repo"))
    ;; Python's main() requires all four identity flags by TRUTHINESS (`and
    ;; args.expected_*`), so an empty value refuses with the combined reason.
    (define (present? value)
      (and (string? value) (positive? (string-length value))))
    (unless (and (present? (flag "--expected-head"))
                 (present? (flag "--expected-branch"))
                 (present? (flag "--expected-origin"))
                 (present? (flag "--expected-tree")))
      (refuse!
       "publish-head requires --expected-head, --expected-branch, --expected-origin and --expected-tree"))
    ;; argparse's `type=int`: a supplied value must be an exact integer.
    (define raw-wave (flag "--wave"))
    (define wave
      (cond
        [(not raw-wave) #f]
        [else
         (define parsed (string->number raw-wave))
         (unless (and parsed (exact-integer? parsed))
           (refuse! "publish-head requires exact wave index"))
         parsed]))
    (printf "~a\n"
            (delivery-publish-head-json
             (delivery-publish-head! (flag "--repo")
                                     (flag "--expected-branch")
                                     (flag "--expected-head")
                                     #:expected-origin (flag "--expected-origin")
                                     #:expected-tree (flag "--expected-tree")
                                     #:plan (flag "--plan")
                                     #:wave wave)))
    (exit 0)))

(module+ main
  (delivery-publish-head-main (vector->list (current-command-line-arguments))))
