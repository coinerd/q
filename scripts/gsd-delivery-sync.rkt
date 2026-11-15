#lang racket/base

;; Phase B slice S2 — native port of the `sync` action of the deleted Python
;; delivery controller.
;;
;; Scope law: this module implements the `sync` action ONLY; every other action
;; lives in its own native module, and extensions/gsd/delivery-coordinator.rkt
;; dispatches all of them. No delivery stage spawns python3 since the Phase B
;; final deletion removed the Python controller.
;;
;; Parity with the Python action is the contract — sync(repo, expected_branch):
;;   1. a nonblank --expected-branch is mandatory (else
;;      "sync requires --expected-branch");
;;   2. `git symbolic-ref --quiet --short HEAD` yields the current branch; a
;;      genuine command failure (detached HEAD) refuses with
;;      "detached HEAD; refusing synchronization". INFRASTRUCTURE refusals from
;;      the shared seam (budget-exhausted, delivery-timeout, command-not-found,
;;      command-unavailable, process-group-unavailable,
;;      group-cleanup-unconfirmed, output-truncated) are re-raised UNCHANGED —
;;      a spent budget or a broken exec boundary is never reported as a
;;      repository state (the reason chain stays attributable);
;;   3. only the expected branch or the shared `main` checkout may synchronize;
;;      anything else refuses with the unrelated-branch reason and is NEVER
;;      switched;
;;   4. `git status --porcelain --untracked-files=all` must be empty (else
;;      "dirty checkout; refusing synchronization") — there is no in-tree
;;      scratch exemption;
;;   5. refresh() — a NON-forced fetch of
;;      refs/heads/main:refs/remotes/origin/main, so a rewritten origin/main is
;;      refused as non-fast-forward instead of overwriting the tracking ref;
;;   6. `git merge --ff-only origin/main`. A diverged checkout fails here with
;;      the typed classifier summary for the merge failure, and a failed fetch
;;      can never be followed by a merge (the refusal is raised first);
;;   7. success -> hash {status "synchronized", head <HEAD sha>, branch <the
;;      branch sync ran on>}.
;;
;; The bounded-git runner, the typed failure classification, the internal
;; process budget, the credential isolation and the exit-2 CLI surface are the
;; SHARED seam in scripts/gsd-delivery-git.rkt (extracted from the S1
;; preflight port), so both native actions behave identically on
;; budget/timeout/cleanup/redaction faults. Raw git stderr is never surfaced.
;;
;; Trusted-helper caveat: git and its configured helpers are trusted. The
;; bounded runner is a BUDGET and CLEANUP boundary (credential-scrubbed
;; environment, one total process budget, group cleanup when the host supports
;; it) — it is not a sandbox, and it can only refuse with a typed cause for the
;; faults it can observe.

(require racket/string
         "gsd-delivery-git.rkt")

(provide delivery-sync!
         delivery-sync-json
         delivery-sync-main)

;; The fast-forward-only, clean-tree, expected-branch synchronization. Returns
;; the success hash or raises exn:fail:delivery-action with the typed reason.
(define (delivery-sync! repo expected-branch)
  (unless (and (string? expected-branch) (not (string=? (string-trim expected-branch) "")))
    (refuse! "sync requires --expected-branch"))
  (unless (path-string? repo)
    (refuse! "sync requires --repo"))
  (define git (make-repo-git-command repo))
  ;; 1. Detached HEAD is a refusal, never an implicit branch selection. Only a
  ;; genuine symbolic-ref failure means detached HEAD: an infrastructure
  ;; refusal from the shared seam keeps its own cause.
  (define current
    (with-handlers ([exn:fail:delivery-action?
                     (lambda (error)
                       (if (infrastructure-refusal? (exn-message error))
                           (raise error)
                           (refuse! "detached HEAD; refusing synchronization")))])
      (string-trim (git (list "symbolic-ref" "--quiet" "--short" "HEAD")))))
  ;; 2. The shared main checkout may carry the published bytes too; any other
  ;; branch is unrelated and refuses. The fast-forward below never switches.
  (unless (or (equal? current expected-branch) (equal? current "main"))
    (refuse!
     (format
      "synchronization refused: HEAD is '~a', expected '~a' (unrelated branch changes not accepted)"
      current
      expected-branch)))
  ;; 3. Any dirt — tracked, staged or untracked — blocks synchronization.
  (unless (string=? (string-trim (git (list "status" "--porcelain" "--untracked-files=all"))) "")
    (refuse! "dirty checkout; refusing synchronization"))
  ;; 4. refresh(): non-forced, so a divergent origin/main raises here.
  (git (list "fetch" "--no-tags" "origin" "refs/heads/main:refs/remotes/origin/main"))
  ;; 5. Fast-forward only; a stale/dirty tree cannot be merged implicitly.
  (git (list "merge" "--ff-only" "origin/main"))
  (hasheq 'status "synchronized" 'head (string-trim (git (list "rev-parse" "HEAD"))) 'branch current))

;; ============================================================
;; JSON / CLI surface (same shape as the Python action)
;; ============================================================

(define (delivery-sync-json result)
  (jsexpr-string result))

;; Exact Python main() contract for this action: argparse requires an action
;; and --repo; sync then requires --expected-branch. Success -> JSON on stdout,
;; exit 0. Refusal -> JSON on stdout, exit 2.
(define (delivery-sync-main argv)
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
    (unless (equal? action "sync")
      (refuse! (format "unsupported action ~a; this module implements the sync action only"
                       (or action ""))))
    (unless (flag "--repo")
      (refuse! "sync requires --repo"))
    (printf "~a\n" (delivery-sync-json (delivery-sync! (flag "--repo") (flag "--expected-branch"))))
    (exit 0)))

(module+ main
  (delivery-sync-main (vector->list (current-command-line-arguments))))
